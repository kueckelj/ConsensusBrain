

# Corpus Callosum and Subcortical -----------------------------------------

moduleBrainTissueSelectionUI <- function(id, title){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      style = css_styles$CB_box,
      shiny::h4(shiny::strong("Selection Criteria"), style = "margin-bottom: 10px;"),
      shiny::fluidRow(
        shiny::column(
          width = 6, 
          align = "left", 
          shiny::uiOutput(ns("area_selection"))
        )
      ), 
      shiny::fluidRow(
        shiny::column(
          width = 5, 
          align = "left", 
          shinyWidgets::radioGroupButtons(
            inputId = ns("wm_handling"),
            label = "White Matter:",
            choices = c("Associated" = "associated", "None" = "none"),
            justified = FALSE,
            selected = "associated",
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          )
        ), 
        shiny::column(
          width = 4, 
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
      ), 
      shiny::fluidRow(
        shiny::column(
          width = 12, 
          shiny::uiOutput(ns("score_specific"))
        )
      ),
      shiny::fluidRow(
        #style = "margin-top: -25px;",
        shiny::column(
          width = 6, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("applyVS"),
            label = "Select", 
            width = "80%",
            class = "CB-action-btn"
          )
          
        ), 
        shiny::column(
          width = 6, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("resetVS"),
            label = "Reset",
            width = "80%",
            class = "CB-action-btn"
          )
        ) 
      )
    )
  )
  
}

moduleBrainTissueSelectionServer <- function(id,
                                             voxel_df_input,
                                             label = c("Subcortical", "Corpus Callosum")){
  
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      # ----- debuggin
      shiny::observeEvent(input$test,{
        
        print(voxel_df())
        
      }, ignoreInit = TRUE)
      
      # ----- steady values
      ns <- session$ns
      
      spc <- shiny_picker_choices
      
      picker_options <- 
        shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          container = "body", 
          dropupAuto = TRUE, 
          liveSearch = TRUE, 
          size = 15
        )
      
      # ----- renderUI
      
      output$area_selection <- shiny::renderUI({
        
        shiny::req(choices()) 
        
        shinyWidgets::pickerInput(
          inputId = ns("area_selection"), 
          label = "Brain Area:",
          choices = choices(), 
          choicesOpt = list(style = unlist(progress_values())),
          selected = character(), 
          multiple = TRUE, 
          options = picker_options
        )
        
      })
      
      output$score_specific <- shiny::renderUI({
        
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("score_specific"),
          label = "With Score:",
          choices = score_set_up$choices,
          justified = FALSE,
          selected = character(),
          checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
        )
        
      })
      
      # ----- reactiveValues
      
      voxel_df <- shiny::reactiveVal(data.frame())
      
      choices <- shiny::reactive({
        
        # ann_dk_adj / ann_dt_adj have the same subcortical segmentation!
        spc$ann_dk_adj[[label]]
        
      })
      
      progress_values <- shiny::reactive({
        
        pv <- 
          comp_progress_values(
            voxel_df = voxel_df_input(), 
            var = "ann_dk_adj", 
            values = choices()
          )  
        
        list(
          style = paste0(
            "background: linear-gradient(to right, #228B2280 ", pv * 100, "%, #D3D3D340 ", pv * 100, "%); 
             padding: 5px; border-radius: 5px;"
          )
        )
        
      })
      
      # ----- observeEvents
      
      # apply voxel selection criteria 
      shiny::observeEvent(input$applyVS, {
        
        # by area
        selected_areas <- input$area_selection
        
        if(input$wm_handling == "associated"){
          
          selected_areas <- c(selected_areas, paste0("wma_", selected_areas))
          
        }
        
        out <- 
          dplyr::filter(
            .data = voxel_df_input(), 
            ann_dk_adj %in% {{selected_areas}}  
          )
        
        # by hemisphere
        if(shiny::isTruthy(input$hemisphere)){
          
          out <- 
            dplyr::filter(
              .data = out, 
              hemisphere %in% input$hemisphere
            )
          
        }
        
        # by specific scores
        if(shiny::isTruthy(input$score_specific)){
          
          out <- 
            dplyr::filter(
              .data = out, 
              CBscore %in% input$score_specific
            )
          
        }
        
        # check selection criteria
        if(nrow(out) == 0){
          
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No Tissue Selected",
            text = "No tissue matches your selection criteria. Please adjust your filters and try again.",
            type = "error", 
            showCloseButton = TRUE
          )
          
          shiny::req(FALSE)
          
        }
        
        voxel_df({ out })
        
      })
      
      # ----- module output
      
      module_output <- shiny::reactive({ voxel_df() })
      
      return(module_output)
      
      
    }
  )
  
}



# Lobes -------------------------------------------------------------------

moduleBrainTissueSelectionLobeUI <- function(id, title){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      style = css_styles$CB_box,
      shiny::h4(shiny::strong("Selection Criteria"), style = "margin-bottom: 10px;"),
      shiny::fluidRow(
        shiny::column(
          width = 6, 
          align = "left", 
          shinyWidgets::pickerInput(
            inputId = ns("parc_atlas"), 
            label = "Cortex Parcellation:", 
            choices = c("Desikan-Kiliany" = "ann_dk_adj", "Destrieux" = "ann_dt_adj"), 
            selected = "ann_dk_adj"
          )
        ), 
        shiny::column(
          width = 6, 
          align = "left", 
          shiny::uiOutput(ns("area_selection"))
        )
      ), 
      shiny::fluidRow(
        shiny::column(
          width = 5, 
          align = "left", 
          shinyWidgets::radioGroupButtons(
            inputId = ns("wm_handling"),
            label = "White Matter:",
            choices = c("Associated" = "associated", "None" = "none"),
            justified = FALSE,
            selected = "associated",
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          )
        ), 
        shiny::column(
          width = 4, 
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
      ), 
      shiny::fluidRow(
        shiny::column(
          width = 12, 
          shiny::uiOutput(ns("score_specific"))
        )
      ),
      shiny::fluidRow(
        #style = "margin-top: -25px;",
        shiny::column(
          width = 6, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("applyVS"),
            label = "Select", 
            width = "80%",
            class = "CB-action-btn"
          )
          
        ), 
        shiny::column(
          width = 6, 
          align = "center", 
          shiny::actionButton(
            inputId = ns("resetVS"),
            label = "Reset",
            width = "80%",
            class = "CB-action-btn"
          )
        ) 
      )
    )
  )
  
}


moduleBrainTissueSelectionLobeServer <- function(id, 
                                                 lobe,
                                                 voxel_df_input){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      # ----- debuggin
      shiny::observeEvent(input$test,{
        
        print(voxel_df())
        
      }, ignoreInit = TRUE)
      
      # ----- steady values
      ns <- session$ns
      
      spc <- shiny_picker_choices
      
      picker_options <- 
        shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          container = "body", 
          dropupAuto = TRUE, 
          liveSearch = TRUE, 
          size = 15
        )
      
      # ----- renderUI
      
      output$area_selection <- shiny::renderUI({
        
        shiny::req(input$parc_atlas) 
        choices_vec <- purrr::flatten_chr(choices())
        
        shinyWidgets::pickerInput(
          inputId = ns("area_selection"), 
          label = "Brain Area:",
          choices = choices_vec, 
          choicesOpt = list(style = unlist(progress_values())),
          selected = character(), 
          multiple = TRUE,
          options = picker_options
        )
        
      })
      
      output$score_specific <- shiny::renderUI({
        
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("score_specific"),
          label = "With Score:",
          choices = score_set_up$choices,
          justified = FALSE,
          selected = character(),
          checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
        )
        
      })
      
      # ----- reactiveValues
      
      voxel_df <- shiny::reactiveVal(data.frame())
      
      choices <- shiny::reactive({
        
        spc[[input$parc_atlas]][stringr::str_c(stringr::str_to_title(lobe), "Lobe", sep = " ")]
        
        
      })
      
      progress_values <- shiny::reactive({
        
        pv <- 
          comp_progress_values(
            voxel_df = voxel_df_input(), 
            var = input$parc_atlas, 
            values = choices()
          )  
        
        list(
          style = paste0(
            "background: linear-gradient(to right, #228B2280 ", pv * 100, "%, #D3D3D340 ", pv * 100, "%); 
             padding: 5px; border-radius: 5px;"
          )
        )
        
      })
      
      # ----- observeEvents
      
      # apply voxel selection criteria 
      shiny::observeEvent(input$applyVS, {
        
        # by area
        atlas_var <- input$parc_atlas
        selected_areas <- input$area_selection
        
        if(input$wm_handling == "associated"){
          
          selected_areas <- c(selected_areas, paste0("wma_", selected_areas))
          
        }
        
        out <- 
          dplyr::filter(
            .data = voxel_df_input(), 
            !!rlang::sym(atlas_var) %in% {{selected_areas}}  
          )
        
        # by hemisphere
        if(shiny::isTruthy(input$hemisphere)){
          
          out <- 
            dplyr::filter(
              .data = out, 
              hemisphere %in% input$hemisphere
            )
          
        }
        
        # by specific scores
        if(shiny::isTruthy(input$score_specific)){
          
          out <- 
            dplyr::filter(
              .data = out, 
              CBscore %in% input$score_specific
            )
          
        }
        
        # check selection criteria
        if(nrow(out) == 0){
          
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No Tissue Selected",
            text = "No tissue matches your selection criteria. Please adjust your filters and try again.",
            type = "error", 
            showCloseButton = TRUE
          )
          
          shiny::req(FALSE)
          
        }
        
        voxel_df({ out })
        
      })
      
      # ----- module output
      
      module_output <- shiny::reactive({ voxel_df() })
      
      return(module_output)
      
      
    }
  )
  
}