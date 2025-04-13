
moduleScoreAssignmentUI <- function(id, height = 300) {

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
        width: 100%;
        height: {height}px;
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

      shiny::div(
        style = "margin-top: auto; text-align: center;",
        shiny::column(
          width = 3,
          shiny::uiOutput(ns("force_score"))
        ),
        shiny::column(
          width = 6,
          align = "center",
          shiny::uiOutput(ns("assign_score_opts"))
        ),
        shiny::column(
          width = 3,
          align = "center",
          shiny::uiOutput(ns("clear_score"))
        )
      )
    )
  )
}



moduleScoreAssignmentServer <- function(id,
                                        voxels_selected_input,
                                        voxels_margin_input,
                                        voxel_df_input,
                                        ...){

  ab_style <-
    stringr::str_replace(
      string = css_styles$CB_action_button,
      pattern = "width: 50%",
      replacement = "width: 100%"
    )

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

      # ----- renderUI
      output$assign_score_opts <- shiny::renderUI({

        shiny::req(any(voxel_df()$selected))

        if(length(voxels_margin()) != 0){

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

        shiny::tagList(
          shiny::actionButton(
            inputId = ns("assign_score"),
            label = "Assign",
            style = ab_style
          )
        )

      })

      output$force_score <- shiny::renderUI({

        shiny::req(any(voxel_df()$CBscore[voxel_df()$selected] != 0))

        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("force_score"),
          label = NULL,
          choices = "Force",
          selected = character(),
          inline = TRUE,
          status = "primary"
        ) %>% add_helper("force_score")

      })

      force_score <- shiny::reactive({

        !is.null(input$force_score) && "Force" %in% input$force_score

      })

      output$clear_score <- shiny::renderUI({

        shiny::req(any(voxel_df()$CBscore[voxel_df()$selected] != 0))

        shiny::actionButton(
          inputId = ns("clear_score"),
          label = "Clear",
          icon = shiny::icon("trash"),
          width = "100%",
          style = ab_style
        )

      })

      voxels_margin <- shiny::reactiveVal(value = character(0))
      voxels_selected <- shiny::reactiveVal(value = character(0))

      shiny::observeEvent(voxels_margin_input(), {

        if(!identical(x = sort(voxels_margin()), y = sort(voxels_margin_input()))){

          voxels_margin({ voxels_margin_input() })

        }

      })

      shiny::observeEvent(voxels_selected_input(), {

        if(!identical(x = sort(voxels_selected()), y = sort(voxels_selected_input()))){

          voxels_selected({ voxels_selected_input() })

        }

      })

      shiny::observeEvent(voxels_selected(), {

        if(length(voxels_selected()) != 0 & !show_CBscore_main()){

          show_CBscore_main({ TRUE })

        } else if(length(voxels_selected()) == 0 & show_CBscore_main()){

          show_CBscore_main({ FALSE })

        }

      })

      shiny::observeEvent(voxels_margin(), {

        if(length(voxels_margin()) != 0 & !show_CBscore_margin()){

          show_CBscore_margin({ TRUE })

        } else if(length(voxels_margin()) == 0 & show_CBscore_margin()){

          show_CBscore_margin({ FALSE })

        }

      })

      show_CBscore_main <- shiny::reactiveVal(value = FALSE)

      show_CBscore_margin <- shiny::reactiveVal(value = FALSE)

      output$CBscore_main <- shiny::renderUI({

        shiny::validate(
          shiny::need(
            expr = show_CBscore_main(),
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
          shiny::tags$script(shiny::HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_main')}';
            {java_script}
          }});
          "))),
          # JavaScript: Highlight Selected Button (Uses Correct Namespace)
          shiny::tags$script(shiny::HTML(glue::glue("
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
            expr = show_CBscore_margin(),
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
          shiny::tags$script(shiny::HTML(glue::glue("
          $(document).ready(function() {{
            var inputID = '{ns('CBscore_margin')}';
            {java_script}
          }});
          "))),
          shiny::tags$script(shiny::HTML(glue::glue("
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

      voxel_df <- shiny::reactive({

        dplyr::mutate(voxel_df_input(), selected = id %in% voxels_selected())

      })

      # ----- observeEvents

      voxel_df_with_score <- shiny::reactiveVal({ data.frame() })

      shiny::observeEvent(input$assign_score, {

        if(length(voxels_margin()) != 0){

          vdf_ws <-
            dplyr::filter(voxel_df(), selected) %>%
            dplyr::mutate(
              CBscore_new =
                dplyr::if_else(
                  condition = id %in% voxels_margin(),
                  true = as.numeric(input$CBscore_margin),
                  false = as.numeric(input$CBscore_main)
                ),
              force = force_score()
            )

        } else {

          vdf_ws <-
            dplyr::filter(voxel_df(), selected) %>%
            dplyr::mutate(
              CBscore_new = as.numeric(input$CBscore_main),
              force = force_score()
            )

        }

        text <- NULL
        if(all(vdf_ws$CBscore_new < vdf_ws$CBscore) & !force_score()){

          title <- "Cannot Overwrite Higher Risk Scores"
          text <- paste(
            "The whole region you selected already has a higher risk score than the one you're trying to assign.",
            "To allow ConsensusBrain to overwrite higher scores with lower ones, please enable the 'Force' option on the left."
          )

          shinyWidgets::sendSweetAlert(
            session = session,
            title = title,
            text = text,
            type = "warning"
          )

          shiny::req(FALSE)

        } else if(all(vdf_ws$CBscore_new == vdf_ws$CBscore)){

          title <- "Nothing Happened"
          text <- "The whole region you selected already has the exact score you're trying to assign."

          shinyWidgets::sendSweetAlert(
            session = session,
            title = title,
            text = text,
            type = "warning"
          )

          shiny::req(FALSE)

        } else if(any(vdf_ws$CBscore_new < vdf_ws$CBscore) & !force_score()) {

          text <- paste(
            "Info: Part of the region you selected already had a higher risk score than the one you assigned.",
            "This was not updated since 'Force' was not enabled."
          )

        }

        voxel_df_with_score({ vdf_ws })

        shinyWidgets::sendSweetAlert(
          session = session,
          title = glue::glue("Score {val}!", val = ifelse(force_score(), "forced", "assigned")),
          text = text,
          type = "success"
        )

        # update to ensure no forcing for next selection
        shinyWidgets::updateAwesomeCheckboxGroup(
          session = session,
          inputId = "force_score",
          selected = character()
        )

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$clear_score, {

        voxel_df_with_score({

          dplyr::filter(voxel_df(), selected) %>%
          dplyr::mutate(CBscore_new = 0, force = TRUE)

        })

        # works regardless of force but ensure that it is unchecked
        # for new selection
        shinyWidgets::updateAwesomeCheckboxGroup(
          session = session,
          inputId = "force_score",
          selected = character()
        )

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Score removed!",
          type = "success"
        )

      }, ignoreInit = TRUE)

      # ----- module output

      module_output <- shiny::reactive({  voxel_df_with_score()  })


    }
  )

}
