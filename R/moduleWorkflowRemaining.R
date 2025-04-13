
moduleWorkflowRemainingUI <- function(id, height = 275){

  ns <- shiny::NS(id)

  shiny::div(
    style = "width: 100%; height: 100%; padding-right: 10px;",
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          align = "center",
          shiny::div(
            style = paste0(
              "background-color: white;",
              "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
              "display: flex;",
              "flex-direction: column;",
              "padding: 10px;",
              "border: 1px solid #ccc;",
              "border-radius: 5px;",
              "width: 100%;",
              "flex-wrap: wrap;",
              "height:", height, "px;",
              "align-items: center;",
              "justify-content: center;",
              sep = " "
            ),
            shiny::uiOutput(ns("rem_tissue_ui"))
          )
        ),
        shiny::column(
          width = 4,
          align = "center",
          shiny::div(
            style = paste0(
              "background-color: black;",
              "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
              "display: flex;",
              "flex-direction: column;",
              "padding: 10px;",
              "border: 1px solid #ccc;",
              "border-radius: 5px;",
              "width: 100%",
              "flex-wrap: wrap;",
              "height: ", height, "px;",
              "align-items: center;",  # <-- Added to center horizontally
              "justify-content: center;",  # <-- Updated to center vertically
              sep = " "
            ),
            plotly::plotlyOutput(outputId = ns("selectionViewer3D"), height = paste0(height*0.95, "px"), width = "100%")
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


}

moduleWorkflowRemainingServer <- function(id,
                                          voxel_df_input,
                                          nifti_input){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      # ----- Dynamic UI

      output$rem_tissue_ui <- shiny::renderUI({

        if(req_update()){

          if(progress() < 75){

            text <- shiny::helpText("A progress of at least 75% is required for this option.")

          } else {

            text <- NULL

          }

          shiny::div(
            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
            shiny::actionButton(
              inputId = ns("identify_remaining_regions"),
              label = "Identify Remaining Regions",
              icon = shiny::icon(name = "refresh"),
              style = "font-size: 20px; padding: 15px 30px; font-weight: bold;",
              disabled = progress() < 75
            ),
            text
          )

        } else {

          # JavaScript for coloring
          lc <- length(score_set_up$choices)

          rrc <- remaining_regions_colors()[remaining_brois()]

          java_script <- purrr::map2_chr(
            .x = names(rrc),
            .y = unname(rrc),
            .f = function(value, color){
              glue::glue(
                "$(\"input:radio[name='\" + inputID + \"'][value='{value}']\").parent().css({{ 'background-color': '{color}', 'color': 'black' }});"
              )
            }
          )

          java_script <- stringr::str_c(java_script, collapse = "")

          shiny::tagList(
            # Container: Centers and Controls Width
            shiny::div(
              style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",

              # Radio Buttons: 75% width with auto margin
              shiny::div(
                style = "width: 90%; margin: auto;",
                shiny::h4(shiny::strong("Remaining Brain Regions:")),
                shinyWidgets::radioGroupButtons(
                  inputId = ns("rem_regions"),
                  choices = names(rrc),
                  selected = character(),
                  direction = "horizontal",
                  size = "lg",
                  justified = FALSE,
                  individual = TRUE,
                  width = "100%"
                )
              )
            ),

            # JavaScript: Apply background colors
            shiny::tags$script(
              shiny::HTML(
                glue::glue(
                  "$(document).ready(function() {{
                    var inputID = '{ns('rem_regions')}';
                    {java_script}
                  }});"
                )
              )
            )
          )
        }
      })

      # ----- Reactive Values

      remaining_df <- shiny::reactiveVal(value = data.frame())

      remaining_regions_colors <- shiny::reactiveVal(value = character(0))

      req_update <- shiny::reactiveVal(value = TRUE)

      selected_broi <- shiny::reactiveVal(value = data.frame())

      # ----- Reactive (Expressions)

      progress <- shiny::reactive({ comp_progress(voxel_df_input()) })

      remaining_brois <- shiny::reactive({

        shiny::req(nrow(remaining_df()) != 0)

        sort(unique(remaining_df()[["broi"]]))

      })

      # ----- Observers

      shiny::observeEvent(input$rem_regions, {

        shiny::req(length(input$rem_regions) == 1)

        sel_region <- input$rem_regions

        selected_broi({

          dplyr::filter(remaining_df(), broi == {{sel_region}})

        })

      }, ignoreInit = TRUE)

      # update the regions of remaining tissues
      shiny::observeEvent(input$identify_remaining_regions, {

        min_size <- 1000

        voxel_broi <-
          dplyr::filter(voxel_df_input(), CBscore == 0) %>%
          dplyr::mutate(selected = TRUE) %>%
          identify_brois()

        broi_sizes <-
          dplyr::filter(voxel_broi, !is.na(broi)) %>%
          dplyr::group_by(broi) %>%
          dplyr::summarise(count = dplyr::n())

        broi_keep <-
          dplyr::filter(broi_sizes, count >= {{min_size}}) %>%
          dplyr::pull(broi)

        voxel_broi <-
          dplyr::filter(voxel_broi, broi %in% {{broi_keep}}) %>%
          dplyr::group_by(broi) %>%
          dplyr::mutate(broi = paste0("Brain Region ", dplyr::cur_group_id()))

        regions <- stringr::str_extract(voxel_broi$broi, "[0-9]*$")
        regions_num <- as.numeric(regions)

        # ensure equal spacing of buttons
        if(max(regions_num) >= 10){

          regions[regions_num < 10] <- paste0("0", regions[regions_num < 10])

          voxel_broi$broi <- paste0("Brain Region ", regions)

        }

        remaining_regions_colors({

          ggsci::pal_d3("category20b")(dplyr::n_distinct(voxel_broi$broi)) %>%
          purrr::set_names(nm = unique(voxel_broi$broi))

        })

        remaining_df({ dplyr::select(voxel_broi, id, broi) })

        rm(voxel_broi)

        req_update(FALSE)

      }, ignoreInit = TRUE)


      shiny::observeEvent(voxel_df_input(), {

        shiny::req(nrow(remaining_df()) != 0)

        ids_with_score <-
          dplyr::filter(voxel_df_input(), CBscore != 0) %>%
          dplyr::pull(id)

        remaining_df({

          dplyr::filter(remaining_df(), !id %in% {{ids_with_score}} )

        })

      })

      # ----- outputs

      output$selectionViewer3D <- plotly::renderPlotly({

        shiny::validate(
          shiny::need(
            expr = progress() > 75,
            message = "Reach a progress of 75% or higher."
          )
        )

        shiny::validate(
          shiny::need(
            expr = !req_update(),
            message = "Identify remaining brain regions."
          )
        )

        shiny::validate(
          shiny::need(
            expr = any(remaining_df()$broi != "none"),
            message = "No remaining brain tissue."
          )
        )

        dplyr::left_join(x = voxel_df_input(), y = dplyr::select(remaining_df(), id, broi), by = "id") %>%
        dplyr::mutate(broi = tidyr::replace_na(broi, replace = "none")) %>%
        trim_brain_3d(var = "broi", val_missing = "none", fct = 0.15) %>%
        plot_brain_3d(
          voxel_df = .,
          color_by = "broi",
          group_highlight = unique(remaining_df()$broi),
          opacity_hide = 0.025,
          clrp_adjust = c(remaining_regions_colors(), "none" = "lightgrey"),
          pt_size = 1,
          paper_bgcolor = "black",
          show_legend = FALSE
        )

      })

      # ----- Modules

      module_mri <-
        moduleMriServer(
          id = "mri",
          mode_init = "selection",
          voxel_df = shiny::reactive({ voxel_df_input() }),
          color_selected = shiny::reactive({ unname(remaining_regions_colors()[input$rem_regions]) }),
          nifti_input = shiny::reactive({ nifti_input() }),
          external_selection = shiny::reactive({ selected_broi() })
        )

      mo_score_assignment <-
        moduleScoreAssignmentServer(
          id = "score_assignment",
          voxel_df_input = shiny::reactive({ voxel_df_input() }),
          voxels_margin_input = shiny::reactive({ module_mri()$voxels_margin }),
          voxels_selected_input = shiny::reactive({ module_mri()$voxels_selected })
        )


      module_output <- shiny::reactive({ mo_score_assignment() })

      return(module_output)

    }
  )

}
