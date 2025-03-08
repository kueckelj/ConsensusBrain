
moduleScoreAssignmentUI <- function(id) {

  ns <- shiny::NS(id)

  # set up JavaScript according to score set up
  lc <- length(score_set_up$choices)

  java_script <-
    purrr::map2_chr(
      .x = score_set_up$choices[2:lc],
      .y = score_set_up$colors[2:lc],
      .f = function(value, color){

        glue::glue(
          "$(\"input:radio[name='\" + inputID + \"'][value='{value}']\").parent().css({{ 'background-color': '{color}', 'color': 'black' }});"
        )

      }
    )

  java_script <- stringr::str_c(java_script, collapse = "")

  # output UI
  shiny::tagList(
    shiny::div(
      style = c(css_styles$CB_box, "height: 350px;"),

      shiny::div(
        # Container for the radio buttons
        shiny::column(
          width = 5,
          align = "center",
          shiny::h4(shiny::strong(score_set_up$label)),
          shinyWidgets::radioGroupButtons(
            inputId = ns("CBscore"),
            label = NULL,
            selected = character(),
            choiceValues = unname(score_set_up$choices)[2:lc],
            choiceNames = names(score_set_up$choices)[2:lc],
            direction = "vertical",
            justified = TRUE,
            individual = FALSE,
            size = "normal",
            width = "100%"
          ),
          shiny::uiOutput(outputId = ns("assign_score"))
        ),
        # Progress plot
        shiny::column(
          width = 7,
          align = "center",
          shiny::h4(shiny::strong("Progress")),
          shiny::plotOutput(outputId = ns("progress_plot"), height = "200px")
        )
      ),

      # JavaScript: Apply background colors (Dynamically uses namespace)
      tags$script(HTML(glue::glue("
        $(document).ready(function() {{
          var inputID = '{ns('CBscore')}';
          {java_script}
        }});
      "))),

      # JavaScript: Highlight Selected Button (Uses Correct Namespace)
      tags$script(HTML(glue::glue("
        $(document).ready(function() {{
          var inputID = '{ns('CBscore')}';
          $(\"input:radio[name='\" + inputID + \"']\").change(function() {{
            $(\"input:radio[name='\" + inputID + \"']\").parent().css({{ 'border': 'none', 'filter': 'brightness(1)' }});  // Reset all
            $(this).parent().css({{ 'border': '2px solid white', 'filter': 'brightness(1.2)', 'font-weight': 'bold' }}); // Highlight selected
          }});
        }});
      ")))
    )
  )
}


moduleScoreAssignmentServer <- function(id,
                                        macro_area = NULL,
                                        voxel_df_input,
                                        voxel_df_progress = NULL){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      # ----- debug

      shiny::observe({


      })

      # ----- renderUI
      output$assign_score <- shiny::renderUI({

        shiny::validate(
          shiny::need(
            expr = {nrow(voxel_df_selected()) != 0},
            message = "Select the brain tissue to assign a score to."
          )
        )

        shiny::validate(
          shiny::need(
            expr = shiny::isTruthy(input$CBscore),
            message = "Choose the score you want to assign."
          )
        )

        shiny::actionButton(
          inputId = ns("assign_score"),
          label = "Assign Score",
          width = "100%"
        )

      })

      # -----
      voxel_df_with_score <- shiny::reactiveVal(value = data.frame())

      # ----- reactive (Expressions)

      voxel_df_selected <- shiny::reactive({

        shiny::req("selected" %in% names(voxel_df_input()))
        dplyr::filter(voxel_df_input(), selected)

        })

      # ----- observeEvents

      shiny::observeEvent(input$assign_score, {

        shiny::req(voxel_df_selected())

        voxel_df_with_score({

            dplyr::filter(voxel_df_selected(), selected) %>%
            dplyr::mutate(CBscore_new = as.numeric(input$CBscore))

          })

      }, ignoreInit = TRUE)

      # ----- outputs

      output$progress_plot <- shiny::renderPlot({

        vdf <- voxel_df_input()

        if(is.character(macro_area)){

          color_done <- unname(consensus_clrps[["ann_macro"]][macro_area])

          if(macro_area == "wm_tract"){

            vdf <- dplyr::filter(vdf, wm_tract != "none")

          } else {

            vdf <- dplyr::filter(vdf, ann_macro %in% {{macro_area}})

          }

        } else {

          color_done <- "brown"

        }

        progress <- sum(vdf$CBscore != 0) / nrow(vdf)

        circular_progress_plot(progress, color_done = color_done)

      })

      # ----- module output

      module_output <- shiny::reactive({  voxel_df_with_score()  })


    }
  )

}
