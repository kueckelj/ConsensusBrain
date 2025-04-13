

moduleWorkflowMacroAreaUI <- function(id, height = 275) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      style = "width: 100%; height: 100%",
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              shinyWidgets::radioGroupButtons(
                inputId = "workflow_region",
                label = NULL,
                choices = workflow_tabs,
                selected = "frontal_lobe",
                status = "primary",
                size = "lg",
                width = "100%",
                justified = TRUE
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            align = "center",
            moduleBrainTissueSelectionUI(id = ns("selection"),  height = height)
          ),
          shiny::column(
            width = 4,
            align = "left",
            shiny::div(
              style = paste0(
                "background-color: white;",
                "border: 1px solid #ccc;",
                "border-radius: 5px;",
                "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
                "padding: 1.5%;",
                "position: relative;",
                "height: ", height, "px;"
              ),
              # Header positioned in the first 50px
              shiny::div(
                style = paste0(
                  "position: absolute;",
                  "width: 100%",
                  "text-align: left;",
                  "z-index: 2;",
                  "font-size: 16px;",
                  "font-weight: bold;",
                  "border-radius: 5px;"
                ),
                shiny::uiOutput(ns("header_progress"))

              ),
              shiny::div(
                style = paste0(
                  "position: absolute;",
                  "left: 0;",
                  "width: 100%;",
                  "border-radius: 5px;",
                  "padding: 1.5%;",
                  "height: ", height, "px;",
                  "z-index: 1;"
                ),
                shiny::plotOutput(outputId = ns("progress_plot"), width = "95%", height = paste0(height*0.9, "px"))
              )
            )
          ),
          shiny::column(
            width = 4,
            align = "center",
            moduleScoreAssignmentUI(id = ns("score_assignment"), height = height)
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            moduleMriUI(id = ns("mri"))
          )
        )
      )
    )
  )


}


moduleWorkflowMacroAreaServer <- function(id,
                                          macro_area,
                                          voxel_df_input,
                                          nifti_input){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      selection <-
        moduleBrainTissueSelectionServer(
          id = "selection",
          macro_area = shiny::reactive({ macro_area() }),
          voxel_df_input = shiny::reactive({ voxel_df_input() })
        )

      module_mri <-
        moduleMriServer(
          id = "mri",
          mode_init = "selection",
          voxel_df = shiny::reactive({ voxel_df_input() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          external_selection = shiny::reactive({ selection() })
        )

      mo_score_assignment <-
        moduleScoreAssignmentServer(
          id = "score_assignment",
          voxel_df_input = shiny::reactive({ voxel_df_input() }),
          voxels_margin_input = shiny::reactive({ module_mri()$voxels_margin }),
          voxels_selected_input = shiny::reactive({ module_mri()$voxels_selected })
        )

      # ----- Dynamic UI

      output$header_progress <- shiny::renderUI({

        if(macro_area() == "wm_tract"){

          n_assigned <-
            sum(
              voxel_df_input()[["wm_tract"]] != "none" &
              voxel_df_input()[["CBscore"]] != 0
            )

          n_obs <- sum(voxel_df_input()[["wm_tract"]] != "none")

        } else {

          n_assigned <-
            sum(
              voxel_df_input()[["ann_macro"]] %in% macro_area() &
              voxel_df_input()[["CBscore"]] != 0
              )

          n_obs <- sum(voxel_df_input()[["ann_macro"]] %in% macro_area())

        }

        progress <- round((n_assigned/n_obs)*100, 2)

        shiny::h4(shiny::strong(glue::glue("Progress: {macro_area_label()} ({progress}%)")))

      })

      # ----- reactive (Expressions)

      macro_area_label <- shiny::reactive({

        ifelse(
          test = macro_area() == "wm_tract",
          yes = "White Matter Tract",
          no = make_pretty_label(macro_area())
        )

      })

      # ----- observeEvents

      progress_exists <- shiny::reactive({

        scores_macro_area <-
          dplyr::filter(
            .data = voxel_df_input(),
            ann_macro == macro_area()
          ) %>%
          dplyr::pull(var = "CBscore")

        any(scores_macro_area != 0)

      })

      # ----- outputs

      output$progress_plot <- shiny::renderPlot({

        if(macro_area() == "wm_tract"){

          dplyr::filter(voxel_df_input(), wm_tract != "none") %>%
            circular_progress_plot()

        } else {

          dplyr::filter(voxel_df_input(), ann_macro %in% macro_area()) %>%
            circular_progress_plot()

        }

      })

      module_output <- shiny::reactive({ mo_score_assignment() })

      return(module_output)

    }
  )

}
