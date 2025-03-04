
# load in once in R_scripts/init_session.R
#ctp_df <- read_consensus_template()

global_settings_default <- 
  list(
    localizer = 
      purrr::map(.x = mri_planes, .f = ~ list(col = "red", lwd = 1.5)) %>% 
      purrr::set_names(nm = mri_planes), 
    mri_slider = list(ticks = TRUE)
  )


vars_brain_ann <- c("hemisphere", "ann_macro", "ann_dk_adj", "ann_dt_adj", "wm_tract")

ann_var_names <- 
  c("hemisphere" = "Hemisphere", 
    "ann_macro" = "Macroanatomical", 
    "ann_dk_adj" = "Desikan-Kiliany", 
    "ann_dt_adj" = "Destrieux", 
    "wm_tract" = "White Matter Tract")

ann_var_names_rev <- purrr::set_names(x = names(ann_var_names), nm = unname(ann_var_names))

# nested named list of named vectors for shinyWidgets::pickerInput()!
shiny_picker_choices <- 
  purrr::map(
    .x = vars_brain_ann, 
    .f = function(ann_var){
      
      vals <- 
        stringr::str_subset(
          string = sort(unique(ctp_df[[ann_var]])), 
          pattern = "^wmt_|^wma_", 
          negate = TRUE
        )
      
      if(ann_var == "hemisphere"){
        
        pretty <- stringr::str_to_title(string = vals)
        
      } else if(ann_var == "ann_macro"){
        
        pretty <- 
          stringr::str_replace(string = vals, pattern = "_", replacement = " ") %>% 
          stringr::str_to_title()
        
      } else if(ann_var == "ann_dk_adj"){
        
        pretty <- 
          stringr::str_replace_all(string = vals, pattern = "_|-", replacement = " ") %>% 
          stringr::str_to_title(string = .) %>% 
          stringr::str_replace_all(string = ., pattern = "Cc", replacement = "C.Callosum")
        
      } else if(ann_var == "ann_dt_adj"){
        
        pretty <- 
          stringr::str_replace_all(string = vals, pattern = "G_", replacement = "Gyr._") %>% 
          stringr::str_replace_all(string = ., pattern = "S_", replacement = "Sulc._") %>% 
          stringr::str_replace_all(string = ., pattern = "_|-", replacement = " ") %>% 
          stringr::str_to_title(string = .) %>% 
          stringr::str_replace_all(string = ., pattern = "Cc", replacement = "C.Callosum")
        
      } else if(ann_var == "wm_tract"){
        
        vals <- vals[vals != "none"]
        pretty <- stringr::str_to_title(string = vals)
        
      }
      
      names(vals) <- pretty
      
      if(ann_var %in% c("ann_dk_adj", "ann_dt_adj")){
        
        count_df <- 
          dplyr::filter(ctp_df, !!rlang::sym(ann_var) %in% c(vals)) %>% 
          dplyr::group_by(ann_macro, !!rlang::sym(ann_var)) %>% 
          dplyr::summarise(count = dplyr::n(), .groups = "drop") %>% 
          dplyr::group_by(!!rlang::sym(ann_var)) %>% 
          dplyr::slice_max(order_by = count, with_ties = FALSE)
        
        macro_labels <- sort(unique(count_df$ann_macro))
        
        out <- 
          purrr::map(
            .x = macro_labels, 
            .f = function(ml){
              
              labels <- 
                dplyr::filter(count_df, ann_macro == {{ml}}) %>% 
                dplyr::pull(!!rlang::sym(ann_var)) %>% 
                sort()
              
              vals[vals %in% labels]
              
            }
          ) 
        
        names(out) <- 
          stringr::str_replace(string = macro_labels, pattern = "_", replacement = " ") %>% 
          stringr::str_to_title()
          
        
      } else {
        
        out <- vals
        
      }
      
      return(out)
      
    }
  ) %>% 
  purrr::set_names(nm = vars_brain_ann)


# a named list of named character values: pretty label -  label
ann_var_labels <- 
  purrr::imap(
    .x = shiny_picker_choices, 
    .f = function(sc, ann_var){
      
      if(is.list(sc)){
        
        sc <- purrr::flatten_chr(sc)
        
      }
      
      if(ann_var != "hemisphere"){
        
        if(ann_var %in% c("ann_dk_adj", "ann_dt_adj")){
          
          sc <- c(sc, shiny_picker_choices$wm_tract)
          
        }
        
        
        add <- 
          purrr::set_names(
            x = paste0("wma_", unname(sc)), 
            nm = paste0("WMA ", names(sc))
          )
        

      } else {
        
        add <- NULL
        
      }
      
      purrr::set_names(nm = names(c(sc, add)), x = unname(c(sc, add)))
      
    }
  )

ann_var_labels_rev <- 
  purrr::map(
    .x = ann_var_labels, 
    .f = function(labels){
      
      purrr::set_names(nm = names(labels), x = unname(labels))
      
    }
  )


tissue_selection_opts <- 
  list(
    select = 
      c(
        "Destrieux" = "ann_dt_adj",
        "Desikan-Kiliany" = "ann_dk_adj", 
        "Hemisphere" = "hemisphere",
        "Macroscopical" = "ann_macro"
      ), 
    numeric = 
      c(
        "Sagittal" = "sag",
        "Axial" = "axi", 
        "Coronar" = "cor"
      ), 
    logical = 
      c(
        "Cortex" = "is_cortex",
        "White-Matter" = "is_wm", 
        "White-Matter-Tract" = "is_tract"
      ) 
  )

tissue_selection_opts_reversed <- 
  lapply(tissue_selection_opts, function(vec) {
    setNames(names(vec), vec)  
  })


css_styles <- 
  list(
    CB_action_button = ".CB-action-btn {
                        background-color: rgba(200, 200,  200, 0.2);
                        color: black;
                        border: 1px solid #ccc;
                        border-radius: 5px;
                        padding: 8px 16px;
                        font-weight: bold;
                        transition: all 0.2s ease-in-out;
                        width: 50%;  /* Make the button fill 50% of the column */
                        display: block;
                        margin: 0 auto; /* Center it */",
    CB_box = 
             "background-color: white; 
              box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1); 
              display: flex; 
              flex-direction: column; 
              padding: 10px; 
              border: 1px solid #ccc; 
              max-width: 100%; 
              flex-wrap: wrap;"
  )


# rules: 
# 1. first value of choices must correspond to the missing (not-yet-assigned) value
# 2. all values of choices must be of the same class
# 3. length of $choices must be equal to length of $colors
score_set_up = list(
  label = "Resection Risk", 
  choices = c("Missing" = 0,
              "Low" = 1, 
              "Low-Medium" = 2, 
              "Medium" = 3, 
              "Medium-High" = 4,
              "High" = 5), 
  colors = c("lightgrey", "#4CAF50", "#8BC34A", "#FFC107", "#FF9800", "#F44336")
)


mri_slider_labels <- list(
  sag = c("Right", "Left"),        # Sagittal: Moves left - right
  axi = c("Superior", "Inferior"), # Axial: Moves top - bottom
  cor = c("Anterior", "Posterior") # Coronal: Moves front - back
)


allowedCols <- c("#D4AF37", "#EEDD82", "#ADFF2F", "#a1c935", "#556B2F",
                  "#40E0D0","forestgreen" , "#4682B4", "#6495ED", "#0F52BA", 
                  "#483D8B", "#9467bd", "#663399", "#8A2BE2", "#BA55D3", "#6b2d5c")

