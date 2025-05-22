



# Lobes -------------------------------------------------------------------

moduleBrainTissueSelectionUI <- function(id, height = 300){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
        "
        .CB-action-btn {
          background-color: rgba(200, 200, 200, 0.2);
          color: black;
          border: 1px solid #ccc;
          border-radius: 5px;
          padding: 8px 16px;
          font-weight: bold;
          transition: all 0.2s ease-in-out;
          width: 50%;
          display: block;
          margin: 0 auto;
          text-align: center;
        }

        .CB-action-btn:hover {
          background-color: rgba(200, 200, 200, 0.4);
        }
      "
        )
      )
    ),
    # Overall Box
    shiny::div(
      style = paste0(
        "background-color: white;",
        "border: 1px solid #ccc;",
        "border-radius: 5px;",
        "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
        "display: flex;",
        "flex-direction: column;",
        "flex-wrap: wrap;",
        "justify-content: space-between;",  # Ensures even distribution
        "padding: 1.5%;",
        "width: 100%;",
        "height:", height, "px;",
        sep = " "
      ),

      # Title at the top
      shiny::h4(
        shiny::uiOutput(ns("header_brain_tissue_selection")),
        style = "margin-bottom: 10px; text-align: left;"
        ) %>% add_helper("selection_criteria"),

      # Brain area selection at the top
      shiny::div(
        style = "flex-grow: 0;",
        shiny::fluidRow(
          shiny::uiOutput(ns("parc_atlas")),
          shiny::uiOutput(ns("brain_area"))
        )
      ),

      # Middle section (evenly distributed)
      shiny::div(
        style = "flex-grow: 1; display: flex; flex-direction: column; justify-content: center;",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            align = "left",
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("hemisphere"),
              label = "Hemisphere:",
              choices = c("Left" = "left", "Right" = "right"),
              justified = TRUE,
              selected = c("left", "right"),
              checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
            ) %>% add_helper("hemisphere")
          ),
          shiny::column(
            width = 6,
            align = "center",
            shiny::uiOutput(outputId = ns("remaining_only"))
          )
        )
      ),

      # Action buttons at the bottom
      shiny::div(
        style = "flex-grow: 0;",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            align = "center",
            shiny::actionButton(
              inputId = ns("applyVS"),
              label = "Select",
              width = "40%",
              class = "CB-action-btn"
            )
          )
        )
      )
    )
  )

}

moduleBrainTissueSelectionServer <- function(id,
                                             macro_area,
                                             voxel_df_input){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      is_lobe <- shiny::reactive({

        length(macro_area()) == 1 &&
          stringr::str_detect(macro_area(), pattern = "lobe$")

      })

      label <- shiny::reactive({

        ifelse(macro_area() == "fiber_tract", "Fiber Tract", "Atlas Label")

      })

      # ----- debuggin
      shiny::observeEvent(input$test,{

        print(voxel_df())

      }, ignoreInit = TRUE)

      # ----- steady values
      ns <- session$ns

      picker_options <-
        shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          container = "body",
          dropupAuto = TRUE,
          liveSearch = TRUE,
          size = 15
        )

      # ----- renderUI

      output$header_brain_tissue_selection <- shiny::renderUI({

        macro_area_label <-
          ifelse(
            test = macro_area() == "fiber_tract",
            yes = "Fiber Tract",
            no = make_pretty_label(macro_area())
            )

        shiny::strong(glue::glue("Brain Region: {macro_area_label}"))

      })

      output$parc_atlas <- shiny::renderUI({

        shiny::req(is_lobe())

        shiny::column(
          width = 6,
          align = "left",
          shinyWidgets::pickerInput(
            inputId = ns("parc_atlas"),
            label = "Parcellation Atlas:",
            choices = c("Desikan-Kiliany" = "ann_dk_adj", "Destrieux" = "ann_dt_adj"),
            selected = "ann_dk_adj"
          ) %>% add_helper("parcellation_atlas")
        )

      })

      output$brain_area <- shiny::renderUI({

        #shiny::req(choices())
        #shiny::req(progress_values())

        shiny::column(
          width = 6,
          align = "left",
          shinyWidgets::pickerInput(
            inputId = ns("brain_area"),
            label = paste0(label(), ":"),
            choices = NULL, #choices(),
            choicesOpt = list(), #list(style = unlist(progress_values())),
            selected = character(),
            multiple = TRUE,
            options = picker_options
          ) %>% add_helper("brain_area")
        )

      })

      # currently not active
      output$specific_score <- shiny::renderUI({

        shiny::req(input$remaining_only)

        all_scores <- score_set_up$choices

        assigned_scores <-
          voxel_df_input()[["CBscore"]][voxel_df_input()[[ann_var()]] %in% unname(choices())] %>%
          unique() %>%
          sort()

        shinyWidgets::pickerInput(
          inputId = ns("specific_score"),
          label = "Choose Scores:",
          choices = all_scores[all_scores %in% assigned_scores],
          selected = character()
        )

      })

      output$remaining_only <- shiny::renderUI({

        shiny::req({

          any(voxel_df_input()[voxel_df_input()$ann_macro %in% macro_area(),][["CBscore"]] != 0)

        })

        shinyWidgets::switchInput(
          inputId = ns("remaining_only"),
          label = "Scoring Filter",
          size = "normal",
          onLabel = "Unscored Only",
          offLabel = "All",
          value = FALSE,
          width = "100%"
        ) %>%
          add_helper("remaining_only")

      })

      # ----- reactiveValues

      voxel_df <- shiny::reactiveVal(data.frame())

      # ----- reactive (Expressions)

      # ann_dk_adj and ann_dt_adj agree on everything except for lobe parcellation
      ann_var <- shiny::reactive({

        if(is_lobe()){

          shiny::req(input$parc_atlas)
          input$parc_atlas

        } else if(macro_area() == "fiber_tract") {

          "fiber_tract"

        } else {

          "ann_dk_adj"

        }


        })

      choices <- shiny::reactive({

        shiny::req(ann_var())

        if(ann_var() == "fiber_tract"){

          choices_out <- sort(unique(voxel_df_input()[["fiber_tract"]]))
          choices_out <- choices_out[choices_out != "none"]

        } else {

          if(stringr::str_detect(macro_area(), pattern = "lobe$")){

            choices_out <- cortical_regions[[ann_var()]][[macro_area()]]

          } else {

            choices_out <-
              dplyr::filter(.data = voxel_df_input(), ann_macro %in% macro_area())[[ann_var()]] %>%
              unique() %>%
              sort()

          }

        }

        choices_out <- sort(choices_out)

        names(choices_out) <- make_pretty_label(choices_out)

        return(choices_out)

      })

      progress_values <- shiny::reactive({

        pv <-
          comp_progress_values(
            voxel_df = voxel_df_input(),
            var = ann_var(),
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

      # update brain area picker
      shiny::observeEvent({ list(choices(), progress_values(), label()) }, {

        shiny::req(choices(), progress_values())

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "brain_area",
          label = paste0(label(), ":"),
          choices = choices(),
          choicesOpt = list(style = unlist(progress_values())),
          selected = character()
        )

      })

      # apply selection criteria
      shiny::observeEvent(input$applyVS, {

        out <- voxel_df_input()

        if(!shiny::isTruthy(input$brain_area)){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = glue::glue("Missing {label()}!"),
            text = glue::glue("You need to select at least one {tolower(label())}."),
            type = "error"
          )

          shiny::req(FALSE)

        }


        # by area
        out <- out[out[[ann_var()]] %in% input$brain_area,]

        # by hemisphere
        if(shiny::isTruthy(input$hemisphere)){

          out <- out[out$hemisphere %in% input$hemisphere, ]

        } else {

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Hemisphere missing!",
            text = "You need to select at least one hemisphere.",
            type = "error"
          )

          shiny::req(FALSE)

        }

        # by white matter
        if(is.logical(input$wm_assoc)){

          if(!input$wm_assoc){

            out <- out[!out$is_wm,]

          }

        }

        # only remaining score
        if(isTRUE(input$remaining_only)){

          out <- out[out$CBscore == 0,]

        }

        # check selection criteria
        if(nrow(out) == 0){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No Tissue Selected",
            text = "No tissue matches your combination of selection criteria. Please adjust your filters and try again.",
            type = "error",
            showCloseButton = TRUE
          )

          shiny::req(FALSE)

        }

        # ensure that the value changes with every select
        # even if the voxel selection remains the same
        attr(out, which = "counter") <- input$applyVS

        voxel_df({ out })

      })

      # ----- module output

      module_output <- shiny::reactive({ voxel_df() })

      return(module_output)


    }
  )

}
