moduleVoxelSelectionUI <- function(id,
                                   title = "Brain Tissue Selection",
                                   label_applyVS = "Apply",
                                   picker_size = 15){
  
  ns <- shiny::NS(id)
  
  spc <- shiny_picker_choices
  
  picker_options <- 
    shinyWidgets::pickerOptions(
      actionsBox = TRUE,
      container = "body", 
      dropupAuto = TRUE, 
      liveSearch = TRUE, 
      size = picker_size
    )
  
  shiny::tagList(
    shiny::div(
      style = css_styles$CB_box,
      #style = "background-color: white; padding: 10px; border: 1px solid black; border-radius: 5px",
      shiny::h4(title, style = "margin-bottom: 10px;"),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          align = "left",
          shinyWidgets::pickerInput(
            inputId = ns("macro"), 
            label = "Macroanatomical:",
            choices = 
              list(
                "Telencephalon" = c(spc$ann_macro[stringr::str_detect(names(spc$ann_macro), pattern = "Lobe$")]),
                "Other" = c("Brainstem", "Cerebellum")
              ), 
            selected = NULL, 
            multiple = TRUE, 
            options = picker_options
          )
        ), 
        shiny::column(
          width = 4, 
          align = "left", 
          shinyWidgets::pickerInput(
            inputId = ns("parc_atlas"), 
            label = "Parcellation Atlas:", 
            choices = c("Desikan-Kiliany" = "ann_dk_adj", "Destrieux" = "ann_dt_adj"), 
            selected = "ann_dk_adj"
          )
        ), 
        shiny::column(
          width = 4, 
          align = "left", 
          shiny::uiOutput(ns("ann_parc"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4, 
          align = "left", 
          shinyWidgets::pickerInput(
            inputId = ns("wm_tracts"), 
            label = "White Matter Tracts:",
            choices = spc$wm_tract, 
            selected = NULL, 
            multiple = TRUE,
            options = picker_options
          )
        ),
        shiny::column(
          width = 4,
          align = "left",
          shinyWidgets::pickerInput(
            inputId = ns("subcortical"), 
            label = "Subcortical:",
            choices = spc$ann_dk_adj$Subcortical, 
            selected = NULL, 
            multiple = TRUE, 
            options = picker_options
          )
        ), 
        shiny::column(
          width = 4, 
          align = "left", 
          shinyWidgets::pickerInput(
            inputId = ns("corpus_callosum"), 
            label = "Corpus Callosum:",
            choices = spc$ann_dk_adj[["Corpus Callosum"]], 
            selected = NULL, 
            multiple = TRUE, 
            options = picker_options
          )
        )
      ), 
      shiny::fluidRow(
        shiny::column(
          width = 6, 
          align = "left", 
          shinyWidgets::radioGroupButtons(
            inputId = ns("wm_handling"),
            label = "White Matter:",
            choices = 
              c("All" = "all",
                "Plus Associated" = "associated_plus",
                "Only Associated" = "associated_only",
                "None" = "none"),
            justified = FALSE,
            selected = "none",
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          )
        ), 
        shiny::column(
          width = 3, 
          align = "left", 
          shinyWidgets::checkboxGroupButtons(
            inputId = ns("hemisphere"),
            label = "Hemisphere:",
            choices = c("Left" = "left", "Right" = "right"),
            justified = TRUE,
            selected = c("left", "right"),
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          )
        ) 
      ), # to there
      
      # ----- Safety Margin
      shiny::uiOutput(outputId = ns("refinement_options")),
      # ----- Apply
      shiny::h4("Apply Selection", style = "margin-bottom: 10px;"),
      shiny::fluidRow(
        #style = "margin-top: -25px;",
        shiny::column(
          width = 3, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("applyVS"),
            label = label_applyVS, 
            width = "100%",
            class = "CB-action-btn"
          )
          
        ), 
        shiny::column(
          width = 3, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("resetVS"),
            label = "Reset",
            width = "100%",
            class = "CB-action-btn"
          )
        ), 
        shiny::column(
          width = 3, 
          align = "center", 
          shiny::actionButton(ns("test"), "Test")
        )
      )
    ), 
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(css_styles$CB_action_button))
    )
  )
  
}

moduleVoxelSelectionServer <- function(id,
                                       voxel_df_input,
                                       size_picker = 15, 
                                       trigger_reset = function(){ NULL }) {
  
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      
      # ----- debugging
      
      shiny::observeEvent(input$test, {
        
        print(voxel_subset())
        
      })
      
      # ----- steady values
      ns <- session$ns
      
      spc <- shiny_picker_choices
      
      picker_options <- 
        shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          container = "body", 
          dropupAuto = TRUE, 
          liveSearch = TRUE, 
          size = size_picker
        )
      
      # ----- reactive values
      voxel_df <- shiny::reactiveVal(data.frame())
      voxel_subset <- shiny::reactiveVal(data.frame())
      
      selected_cc <- shiny::reactive({ input$corpus_callosum })
      selected_parc <- shiny::reactive({ input$ann_parc })
      selected_subcortical <- shiny::reactive({ input$subcortical })
      selected_wmts <- shiny::reactive({ input$wm_tracts })
      
      # ----- reactive (events)
      
      # ----- renderUI
      output$ann_parc <- shiny::renderUI({
        
        shiny::req(input$parc_atlas) 
        
        if (input$parc_atlas == "ann_dk_adj") {
          
          shinyWidgets::pickerInput(
            inputId = ns("ann_parc"), 
            label = "Desikan-Kiliany Atlas:",
            choices = spc$ann_dk_adj[stringr::str_detect(names(spc$ann_dk_adj), pattern = "Lobe$")], 
            selected = NULL, 
            multiple = TRUE, 
            options = picker_options
          )
          
        } else if (input$parc_atlas == "ann_dt_adj") {
          
          shinyWidgets::pickerInput(
            inputId = ns("ann_parc"), 
            label = "Destrieux Atlas:",
            choices = spc$ann_dt_adj[stringr::str_detect(names(spc$ann_dt_adj), pattern = "Lobe$")], 
            selected = NULL, 
            multiple = TRUE,
            options = picker_options
          )
          
        }
        
      })
      
      # ----- observe events
      
      shiny::observeEvent(voxel_df_input(), {
        
        if(!identical(x = voxel_df(), y = voxel_df_input())){
          
          voxel_df({ voxel_df_input() })
          
        }
        
      })
      
      # make input$parc_atlas reactive to input$macro
      oe <- shiny::observeEvent(list(input$macro, input$parc_atlas), {
        
        shiny::req(input$parc_atlas)  
        
        macro_areas <- names(spc$ann_macro[spc$ann_macro %in% input$macro])
        
        selected_parc_new <- 
          purrr::map(.x = spc[[input$parc_atlas]][macro_areas], .f = unname) %>% 
          purrr::flatten_chr()
        
        shiny::isolate({
          shinyWidgets::updatePickerInput(
            session = session,    
            inputId = "ann_parc",  
            selected = selected_parc_new
          )
          
        })
        
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      # make input$macro backwards reactive to input$ann_parc 
      # ??? without retriggering input$macro ??? -> diffuse for now with NULL
      shiny::observeEvent(NULL, {
        
        selected <- 
          purrr::keep(
            .x = spc[[shiny::isolate(input$parc_atlas)]], 
            .p = ~ any(.x %in% input$ann_parc)
          ) %>%
          names() %>% 
          tolower() %>% 
          stringr::str_replace(pattern = " ", replacement = "_")
        
        shinyWidgets::updatePickerInput(
          session = session, 
          inputId = "macro",
          selected = selected
        )
        
      })
      
      # VOXELSELECTION! 
      shiny::observeEvent(input$applyVS, {
        
        # 1. Pre-filter by hemisphere
        filt_vox <- dplyr::filter(shiny::isolate({voxel_df()}), hemisphere %in% c(input$hemisphere, "midline"))
        
        # 2. Select relevant labels
        labels_keep <- c(selected_parc(), selected_subcortical(), selected_cc(), paste0("wmt_", selected_wmts()))
        
        if ("Brainstem" %in% input$macro) {
          labels_keep <- c(labels_keep, "Brain-Stem")
        }
        
        if ("Cerebellum" %in% input$macro) {
          labels_keep <- c(labels_keep, "Cerebellum-Cortex")
        }
        
        ann_var <- input$parc_atlas
        
        if(input$wm_handling == "associated_plus"){
          
          labels_keep <- c(
            labels_keep,
            paste0("wma_", labels_keep),
            paste0("wma_", selected_wmts())
          )
          
          filt_vox <- filt_vox[filt_vox[[ann_var]] %in% labels_keep, ]
          
        } else if(input$wm_handling == "associated_only"){
          
          labels_keep <- c(
            labels_keep,
            paste0("wma_", labels_keep),
            paste0("wma_", selected_wmts())
          )
          
          filt_vox <- filt_vox[filt_vox[[ann_var]] %in% labels_keep, ]
          filt_vox <- filt_vox[stringr::str_detect(filt_vox[[ann_var]], pattern = "^wma_"),]
          
        } else if(input$wm_handling == "all"){
          
          filt_vox <- 
            filt_vox[filt_vox[[ann_var]] %in% labels_keep | (filt_vox$is_wm & !filt_vox$is_tract), ]
          
        } else if(input$wm_handling == "none"){
          
          filt_vox <- filt_vox[filt_vox[[ann_var]] %in% labels_keep, ]
          
        }
        
        filt_vox$selection <- "primary"
        
        # 3. Add safety margin
        if(!is.null(input$safety_margin) && input$safety_margin != 0){
          
          if(length(input$safety_margin) == 0){
            
            shiny::showNotification(
              ui = "Select at least one brain structure to be considered for the safety margin.",
              type = "error"
            )
            
            shiny::req(FALSE)
            
          }
          
          confuns::give_feedback(
            msg = "Computing safety margin. This can take a few seconds.", 
            verbose = TRUE, 
            with.time = TRUE
          )
          
          pattern <- stringr::str_c(input$safety_margin_include, collapse = "|")
          voxel_margin <- 
            dplyr::filter(
              .data = shiny::isolate({voxel_df()}),
              stringr::str_detect(string = ann_macro, pattern = pattern)
            )
          
          sft_margin_vox <- 
            comp_dist_to_closest_voxel(voxel_margin = voxel_margin, voxel_ref = filt_vox) %>% 
            dplyr::filter(dist <= input$safety_margin) %>% 
            dplyr::mutate(selection = "margin", dist = NULL)
          
          confuns::give_feedback(
            msg = "Done.", 
            verbose = TRUE, 
            with.time = TRUE
          )
          
          # comp_dist_to_closest_voxel removes potential duplicates!
          filt_vox <- rbind(filt_vox, sft_margin_vox)
          
        }
        
        if(nrow(filt_vox) == 0){
          
          shiny::showNotification(
            ui = "Selection resulted in no remaining tissue.",
            type = "error"
          )
          
          shiny::req(FALSE)
          
        }
        
        voxel_subset(filt_vox)
        
      })
      
      
      shiny::observeEvent(c(input$resetVS, trigger_reset()), {
        
        for(picker_id in c("macro", "ann_parc", "wm_tracts", "subcortical", "corpus_callosum")){
          
          shinyWidgets::updatePickerInput(
            session = session, 
            inputId = picker_id, 
            selected = character()
          )
          
        }
        
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "wm_handling",
          selected = "none"
        )
        
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "hemisphere",
          selected = c("right", "left", "midline")
        )
        
        shiny::updateNumericInput(
          session = session,
          inputId = "safety_margin",
          value = 0
        )
        
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "safety_margin_include",
          selected = c("^(?!wma).*lobe$", "^wma", "wm_tract", "corpus_callosum", "subcortical")
        )
        
        voxel_subset({
          
          dplyr::filter(voxel_df(), FALSE)
          
        })
        
      }, ignoreInit = FALSE)
      

      # module output -----------------------------------------------------------
      
      module_output <- shiny::reactive({ voxel_subset() })

      
      
    }
  )
}