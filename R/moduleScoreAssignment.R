
moduleScoreAssignmentUI <- function(id) {

  ns <- shiny::NS(id)

  # Output UI with improved layout
  shiny::tagList(
    shiny::div(
      style = glue::glue("{css_styles$CB_box} height: 300px;"),

      # Header (Always at the Top)
      shiny::div(
        style = "flex-shrink: 0;",
        shiny::h4(shiny::strong(paste0(score_set_up$label, " - Score Assignment")))
      ),

      # Main Content (Centered)
      shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "left",
          shiny::uiOutput(ns("CBscore_main"))
        ),
        shiny::column(
          width = 6,
          align = "left",
          shiny::uiOutput(ns("CBscore_margin"))
        )
      ),

      # Assign Score Button (Always at the Bottom)
      shiny::div(
        style = "flex-shrink: 0;",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            align = "center",
            shiny::uiOutput(ns("assign_score"))
          )
        )
      )
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

      # Set up JavaScript according to score setup
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
        ) %>%
        stringr::str_c(collapse = "")

      # ----- debug

      shiny::observe({


      })

      # ----- renderUI
      output$assign_score <- shiny::renderUI({

        shiny::req(nrow(voxel_df_selected()) != 0)

        if(any(voxel_df_selected()$is_margin)){

          ready_for_score_assignment <-
            shiny::isTruthy(input$CBscore_main) &
            shiny::isTruthy(input$CBscore_margin)

          message <- "Choose the scores you want to assign."

        } else {

          ready_for_score_assignment <-
            shiny::isTruthy(input$CBscore_main)

          message <- "Choose the score you want to assign."

        }

        shiny::validate(
          shiny::need(
            expr = ready_for_score_assignment,
            message = message
          )
        )

        shiny::actionButton(
          inputId = ns("assign_score"),
          label = "Assign Score",
          width = "40%",
          style = css_styles$CB_action_button
        )

      })

      output$CBscore_main <- shiny::renderUI({

        shiny::validate(
          shiny::need(
            expr = any(voxel_df_input()$selected),
            message = "No tissue selected."
          )
        )

        shiny::tagList(
          shinyWidgets::radioGroupButtons(
            inputId = ns("CBscore_main"),
            label = "Main Selection:",
            selected = character(),
            choiceValues = unname(score_set_up$choices)[2:lc],
            choiceNames = names(score_set_up$choices)[2:lc],
            direction = "vertical",
            justified = TRUE,  # Ensures full width
            individual = FALSE,
            size = "normal",
            width = "100%"  # Fills the column width
          ) %>% add_helper("score_main"),
          tags$script(HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_main')}';
            {java_script}
          }});
          "))),
          # JavaScript: Highlight Selected Button (Uses Correct Namespace)
          tags$script(HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_main')}';
            $(\"input:radio[name='\" + inputID + \"']\").change(function() {{
              $(\"input:radio[name='\" + inputID + \"']\").parent().css({{ 'border': 'none', 'filter': 'brightness(1)' }});
              $(this).parent().css({{ 'border': '2px solid white', 'filter': 'brightness(1.2)', 'font-weight': 'bold' }});
            }});
          }});
          ")))
        )


      })

      output$CBscore_margin <- shiny::renderUI({

        shiny::validate(
          shiny::need(
            expr = any(voxel_df_input()$is_margin & voxel_df_input()$selected),
            message = "No margin confirmed."
          )
        )

        shiny::tagList(
          shinyWidgets::radioGroupButtons(
            inputId = ns("CBscore_margin"),
            label = "Margin:",
            selected = character(),
            choiceValues = unname(score_set_up$choices)[2:lc],
            choiceNames = names(score_set_up$choices)[2:lc],
            direction = "vertical",
            justified = TRUE,  # Ensures full width
            individual = FALSE,
            size = "normal",
            width = "100%"  # Fills the column width
          ) %>% add_helper("score_margin"),
          tags$script(HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_margin')}';
            {java_script}
          }});
          "))),
          tags$script(HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_margin')}';
            $(\"input:radio[name='\" + inputID + \"']\").change(function() {{
              $(\"input:radio[name='\" + inputID + \"']\").parent().css({{ 'border': 'none', 'filter': 'brightness(1)' }});
              $(this).parent().css({{ 'border': '2px solid white', 'filter': 'brightness(1.2)', 'font-weight': 'bold' }});
            }});
          }});
          ")))
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

        if(any(voxel_df_selected()$is_margin)){

          voxel_df_with_score({

            dplyr::filter(voxel_df_selected(), selected) %>%
            dplyr::mutate(
              CBscore_new =
                dplyr::if_else(
                  condition = is_margin,
                  true = as.numeric(input$CBscore_margin),
                  false = as.numeric(input$CBscore_main)
                  )
            )

          })

        } else {

          voxel_df_with_score({

            dplyr::filter(voxel_df_selected(), selected) %>%
            dplyr::mutate(CBscore_new = as.numeric(input$CBscore_main))

          })

        }

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Score assigned!",
          type = "success"
        )

      }, ignoreInit = TRUE)

      # ----- module output

      module_output <- shiny::reactive({  voxel_df_with_score()  })


    }
  )

}
