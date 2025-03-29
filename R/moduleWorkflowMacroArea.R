

moduleWorkflowMacroAreaUI <- function(id, macro_area = NULL) {

  ns <- shiny::NS(id)

  shiny::div(
    style = "width: 100%; height: 100%; padding-right: 10px;",
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          align = "center",
          moduleBrainTissueSelectionUI(id = ns("selection"), macro_area = macro_area)
        ),
        shiny::column(
          width = 4,
          align = "center",
          moduleScoreAssignmentUI(id = ns("score_assignment"))
        ),
        shiny::column(
          width = 4,
          align = "center",
          shiny::div(
            style = paste0(
              "background-color: white;",
              "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
              "border: 1px solid #ccc;",
              "border-radius: 5px;",
              "width: 550px;",
              "height: 300px;",
              "position: relative;"
            ),
            # Header positioned in the first 50px
            shiny::div(
              style = paste0(
                "position: absolute;",
                "padding-top: 7.5px;",
                "padding-left: 7.5px;",
                "width: 100%",
                "text-align: left;",
                "z-index: 2;",
                "font-size: 16px;",
                "font-weight: bold;",
                "border-radius: 5px;"

              ),
              shiny::uiOutput(ns("header_progress"))

            ),
            # Plot positioned below the header
            shiny::div(
              style = paste0(
                "position: absolute;",
                "left: 0;",
                "width: 100%;",
                "border-radius: 5px;",
                "height: 300px;",  # Fills remaining space
                "z-index: 1;"
              ),
              shiny::plotOutput(outputId = ns("progress_plot"), width = "90%", height = "290px")
            ),
            shiny::div(
              style = paste0(
                "position: absolute;",
                "bottom: 0;",
                "left: 0;",
                "width: 100%;",
                "border-radius: 5px;",
                "text-align: left;",
                "z-index: 3;",  # Ensures button is above the plot
                "background-color: rgba(255, 255, 255, 0.8);",  # Optional: adds contrast
                "padding: 5px;"
              ),
              shiny::splitLayout(
                cellWidths = c("25%", "75%"),
                shiny::actionButton(
                  inputId = ns("viewer3D"),
                  label = "3D View",
                  icon = shiny::icon("brain"),
                  style = paste0(
                    "height: 37px;",
                    "width: 90px;",
                    "font-size: 14px;",
                    "padding: 6px;",
                    "text-align: center;",
                    "background-color: white;",  # Ensures white background
                    "color: black;",  # Ensures text color remains readable
                    "border-radius: 4px;"  # Optional: rounded corners for a polished look
                  )
                ),
                shiny::helpText("The progress made within the brain region of this workflow tab.")
              )
            )
          )
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

}


moduleWorkflowMacroAreaServer <- function(id,
                                          macro_area,
                                          voxel_df_input,
                                          non_brain_template,
                                          nifti_input){

  macro_color <- unname(consensus_clrps[["ann_macro"]][macro_area])

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      voxel_df <- shiny::reactive({

        dplyr::mutate(voxel_df_input(), color = alpha("forestgreen", alpha_val))

      })

      voxel_df_output <- shiny::reactiveVal(data.frame())

      mo_selection <-
        moduleBrainTissueSelectionServer(
          id = "selection",
          macro_area = macro_area,
          voxel_df_input = shiny::reactive({ voxel_df() })
        )

      module_mri <-
        moduleMriServer(
          id = "mri",
          mode_init = "selection",
          non_brain_template = non_brain_template,
          voxel_df = shiny::reactive({ voxel_df() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          external_selection = shiny::reactive({ mo_selection() })
        )

      mo_score_assignment <-
        moduleScoreAssignmentServer(
          id = "score_assignment",
          macro_area = macro_area,
          voxel_df_input = shiny::reactive({ module_mri()$voxel_df })
        )

      # ----- Dynamic UI

      output$header_progress <- shiny::renderUI({

        vdf <- voxel_df_input()

        if(macro_area == "wm_tract"){

          vdf <- dplyr::filter(vdf, wm_tract != "none")

        } else {

          vdf <- dplyr::filter(vdf, ann_macro %in% {{macro_area}})

        }

        progress <- round((sum(vdf$CBscore != 0)/nrow(vdf))*100, 2)

        shiny::h4(shiny::strong("Progress: ", progress, "%"))

      })

      # ----- observeEvents

      # 3D viewer
      shiny::observeEvent(input$viewer3D, {

        shiny::showModal(
          ui = shiny::modalDialog(
            shinycssloaders::withSpinner(
              ui_element = plotly::plotlyOutput(outputId = ns("plot3D")),
              id = ns("plot3D_spinner")
            ),
            title = glue::glue("Score Assignment of {make_pretty_label(macro_area)}"),
            easyClose = TRUE,
            size = "l"
          )
        )

      })


      # ----- outputs

      output$plot3D <- plotly::renderPlotly({

        vdf <-
          dplyr::mutate(
            .data = voxel_df_input(),
            CBscore = dplyr::if_else(ann_macro == {{macro_area}}, true = CBscore, false = 0)
          ) %>%
          make_CBscore_label(voxel_df = ., score_set_up = score_set_up) %>%
          dplyr::mutate(Score = as.character(CBscore_label))

        clrp_adjust <-
          purrr::set_names(
            x = score_set_up$colors,
            nm = names(score_set_up$choices)
          )

        plot_brain_3d(
          voxel_df = vdf,
          color_by = "Score",
          clrp_adjust = clrp_adjust,
          group_highlight = names(clrp_adjust)[names(clrp_adjust) != "Missing"],
          opacity_hide = 0.05,
          pt_size = 1,
          paper_bgcolor = "black",
          hoverinfo = "Score"
        )

      })

      output$progress_plot <- shiny::renderPlot({

        vdf <- voxel_df_input()

        if(is.character(macro_area)){

          if(macro_area == "wm_tract"){

            vdf <- dplyr::filter(vdf, wm_tract != "none")

          } else {

            vdf <- dplyr::filter(vdf, ann_macro %in% {{macro_area}})

          }

        } else {

          color_done <- "brown"

        }


        circular_progress_plot(vdf, score_set_up = score_set_up)

      })

      module_output <- shiny::reactive({ mo_score_assignment() })

      return(module_output)

    }
  )

}
