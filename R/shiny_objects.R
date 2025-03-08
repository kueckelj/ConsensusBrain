
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

