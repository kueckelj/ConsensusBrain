
moduleScoreAssignmentUI <- function(id, height = "300px") {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      style = glue::glue("
        background-color: white;
        box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
        display: flex;
        flex-direction: column;
        padding: 10px;
        border: 1px solid #ccc;
        height: {height};
        width: 550px;
        border-radius: 5px;"),

      # Header (Top)
      shiny::div(
        style = "flex-shrink: 0; text-align: left;",
        shiny::h4(shiny::strong(paste0(score_set_up$label, " - Score Assignment"))) %>%
          add_helper("score_description")
      ),

      # Main Content (Middle, grows to fill space)
      shiny::div(
        style = "flex-grow: 1;",
        shiny::fluidRow(
          shiny::column(width = 1),
          shiny::column(width = 5, align = "left", shiny::uiOutput(ns("CBscore_main"))),
          shiny::column(width = 5, align = "left", shiny::uiOutput(ns("CBscore_margin"))),
          shiny::column(width = 1)
        )
      ),

      # Action Button (Bottom, pushed down)
      shiny::div(
        style = "margin-top: auto; text-align: center;",
        shiny::uiOutput(ns("assign_score"))
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
            expr = any(voxel_df()$selected),
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
            expr = any(voxel_df()$is_margin & voxel_df()$selected),
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

      voxel_df <- shiny::reactiveVal(value = data.frame())
      voxel_df_with_score <- shiny::reactiveVal(value = data.frame())

      # ----- reactive (Expressions)

      voxel_df_selected <- shiny::reactive({

        shiny::req("selected" %in% names(voxel_df()))
        dplyr::filter(voxel_df(), selected)

        })

      # ----- observeEvents

      shiny::observeEvent(voxel_df_input(), {

        if(!identical(x = voxel_df(), y = voxel_df_input())){

          voxel_df({ voxel_df_input() })

        }

      })

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
