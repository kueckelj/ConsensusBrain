
moduleWorkflowRemainingUI <- function(id){

  ns <- shiny::NS(id)

  shiny::div(
    style = "width: 100%; height: 100%; padding-right: 10px;",
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          align = "center",
          shiny::div(
            style = paste(
              "background-color: white;",
              "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
              "display: flex;",
              "flex-direction: column;",
              "padding: 10px;",
              "border: 1px solid #ccc;",
              "border-radius: 5px;",
              "width: 550px;",
              "flex-wrap: wrap;",
              "height: 325px;",
              "align-items: center;",  # <-- Added to center horizontally
              "justify-content: center;",  # <-- Updated to center vertically
              sep = " "
            ),
            shiny::uiOutput(ns("rem_tissue_ui"))
          )
        ),
        shiny::column(
          width = 4,
          align = "center",
          moduleScoreAssignmentUI(id = ns("score_assignment"), height = "325px")
        ),
        shiny::column(
          width = 4,
          align = "center",
          shiny::div(
            style = paste(
              "background-color: black;",
              "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
              "display: flex;",
              "flex-direction: column;",
              "padding: 10px;",
              "border: 1px solid #ccc;",
              "border-radius: 5px;",
              "width: 550px;",
              "flex-wrap: wrap;",
              "height: 325px;",
              "align-items: center;",  # <-- Added to center horizontally
              "justify-content: center;",  # <-- Updated to center vertically
              sep = " "
            ),
            plotly::plotlyOutput(outputId = ns("selectionViewer3D"), height = "300px", width = "100%")
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

moduleWorkflowRemainingServer <- function(id,
                                          non_brain_template,
                                          voxel_df_input,
                                          nifti_input){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      # ----- Dynamic UI
      output$rem_tissue_ui <- shiny::renderUI({

        if(req_update()){

          # Action Button: Centered and More Prominent
          shiny::div(
            style = "display: flex; align-items: center; justify-content: center; height: 100%;",
            shiny::actionButton(
              inputId = ns("identify_remaining_regions"),
              label = "Identify Remaining Regions",
              icon = shiny::icon(name = "refresh"),
              style = "font-size: 20px; padding: 15px 30px; font-weight: bold;"
            )
          )

        } else {

          # JavaScript for coloring
          lc <- length(score_set_up$choices)

          java_script <- purrr::map2_chr(
            .x = names(rem_regions_colored()),
            .y = unname(rem_regions_colored()),
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
                  choices = names(rem_regions_colored()),
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
            tags$script(HTML(glue::glue("
        $(document).ready(function() {{
          var inputID = '{ns('rem_regions')}';
          {java_script}
        }});
      ")))
          )
        }
      })

      # ----- Reactive Values

      remaining_df <- shiny::reactiveVal(value = data.frame())

      req_update <- shiny::reactiveVal(value = TRUE)

      selected_broi <- shiny::reactiveVal(value = data.frame())

      # ----- Reactive (Expressions)

      rem_regions_colored <- shiny::reactive({

        shiny::req(nrow(remaining_df()) != 0)

        broi_colors <-
          dplyr::distinct(remaining_df(), broi, color) %>%
          dplyr::filter(broi != "none") %>%
          dplyr::arrange(broi)

        purrr::set_names(x = broi_colors$color, nm = broi_colors$broi)

      })

      proc_progress <- shiny::reactive({

        sum(voxel_df_input()$CBscore != 0)/nrow(voxel_df_input())

      })

      remaining_brois <- shiny::reactive({

        shiny::req(nrow(remaining_df()) != 0)

        sort(unique(remaining_df()[["broi"]]))

      })

      voxel_df_broi_colored <- shiny::reactive({

        shiny::validate(
          shiny::need(
            expr = shiny::isTruthy(rem_regions_colored()),
            message = "Identify remaining brain regions."
          )
        )

        if(shiny::isTruthy(input$rem_regions)){

          out <-
            dplyr::mutate(
              .data = voxel_df_input(),
              color = ggplot2::alpha(unname(rem_regions_colored()[input$rem_regions]), 0.45)
            )

        } else {

          out <- voxel_df_input()

        }

        return(out)

      })

      # ----- Observers

      shiny::observeEvent(input$rem_regions, {

        shiny::req(length(input$rem_regions) == 1)

        sel_region <- input$rem_regions

        selected_broi({

          dplyr::filter(remaining_df(), broi == {{sel_region}}) %>%
            dplyr::mutate(color = ggplot2::alpha(color, 0.45))

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

        broi_out <- unique(voxel_broi$broi)

        clr_vector <-
          ggsci::pal_d3("category20b")(length(broi_out)) %>%
          purrr::set_names(nm = broi_out)

        voxel_broi$color <- clr_vector[as.character(voxel_broi$broi)]

        remaining_df({

          dplyr::left_join(
            x = dplyr::select(voxel_df_input(), -dplyr::any_of(c("broi", "color"))),
            y = dplyr::select(voxel_broi, id, broi, color),
            by = "id"
          ) %>%
            dplyr::mutate(broi = tidyr::replace_na(broi, replace = "none"))

        })

        req_update(FALSE)

      }, ignoreInit = TRUE)

      # integrate selection of area modules
      shiny::observeEvent(voxel_df_input(), {

        shiny::req(nrow(remaining_df()) != 0)

        ids_with_score <-
          dplyr::filter(voxel_df_input(), CBscore != 0) %>%
          dplyr::pull(id)

        remaining_df({

          dplyr::mutate(
            .data = remaining_df(),
            broi = dplyr::if_else(id %in% {{ids_with_score}}, true = "none", false = broi)
          )

        })

      })

      # ----- outputs

      output$selectionViewer3D <- plotly::renderPlotly({

        shiny::validate(
          shiny::need(
            expr = shiny::isTruthy(remaining_brois()),
            message = "Click on refresh to identify remaining brain tissue."
          )
        )

        plot_input <-
          remaining_df() %>%
          trim_brain_3d(var = "broi", val_missing = "none", fct = 0.25)

        brois <- unique(plot_input$broi)
        brois <- brois[brois != "none"]

        plot_brain_3d(
          voxel_df = plot_input,
          color_by = "broi",
          group_highlight = brois,
          opacity_hide = 0.025,
          clrp_adjust = c(rem_regions_colored(), "none" = "lightgrey"),
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
          non_brain_template = non_brain_template,
          voxel_df = shiny::reactive({ voxel_df_broi_colored() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          external_selection = shiny::reactive({ selected_broi() }),
          external_selection_opts = list(use_colors = TRUE)
        )

      mo_score_assignment <-
        moduleScoreAssignmentServer(
          id = "score_assignment",
          voxel_df_input = shiny::reactive({ module_mri()$voxel_df })
        )


      module_output <- shiny::reactive({ mo_score_assignment() })

      return(module_output)

    }
  )

}
