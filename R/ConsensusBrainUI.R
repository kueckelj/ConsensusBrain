

ConsensusBrainUI <- function(){

  shinydashboard::dashboardPage(

    # header =
    shinydashboard::dashboardHeader(title = "ConsensusBrain"),

    # sidebar =
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        id = "sidebar_menu",
        shinydashboard::menuItem(
          text = "Welcome",
          tabName = "tab_welcome",
          icon = shiny::icon("home")
        ),
        shinydashboard::menuItem(
          text = "Workflow",
          tabName = "tab_workflow",
          icon = shiny::icon("brain")
        ),
        shinydashboard::menuItem(
          text = "Remaining",
          tabName = "tab_remaining",
          icon = shiny::icon("puzzle-piece")
        ),
        shinydashboard::menuItem(
          text = "Progress",
          icon = shiny::icon("flag", lib = "glyphicon"),
          tabName = "tab_progress",
          selected = FALSE
        )
      ),
      shiny::br(),
      shiny::column(
        width = 12,
        align = "center",
        shiny::actionButton(
          inputId = "logout",
          label = "Logout",
          icon = shiny::icon("sign-out-alt"),
          style = c(
          "width: 90%;
           height: 36px;
           line-height: 36px;
           padding: 0;
           margin: 0;
           display: inline-block;
           text-align: center;
           color: black;
           font-weight: bold;
           background-color: white;
           border: 2px solid black;
           border-left: 0;")
        )
      )
    ),

    # body =
    shinydashboard::dashboardBody(

      # busy spinner
      shinybusy::add_busy_spinner(spin = "cube-grid", color = "tomato", timeout = 1500),

      # java script options
      shinyjs::useShinyjs(),

      # CSS
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML("
          .progress {
            height: 60px !important;
          }
          .progress-bar {
            font-size: 30px;
            line-height: 60px;
          }
        ")
        )
      ),

      # tabs
      shinydashboard::tabItems(


        # Welcome -----------------------------------------------------------------



        # Score Assignment --------------------------------------------------------

        # Workflow
        shinydashboard::tabItem(
          tabName = "tab_workflow", # tab_frontal_lobe

          moduleWorkflowMacroAreaUI(id = "workflow", width = 475)

        ),

        # Remaining
        shinydashboard::tabItem(
          tabName = "tab_remaining",

          moduleWorkflowRemainingUI(id = "workflow_remaining", width = 500)

        ),

        # Progress ----------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_progress",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                style = "margin-top: 10px;",
                shinyWidgets::progressBar(
                  id = "progress_bar",
                  value = 0,
                  display_pct = TRUE,
                  status = "primary",
                  striped = TRUE
                )
              )
            )
          ),
          shiny::div(
            style = "width: 100%; padding-right: 10px;",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                moduleMriUI(id = "progress", width = 500)
              )
            )
          ),
          shiny::div(
            style = "width: 100%; margin-top: 30px;",
            shiny::fluidRow(
              shiny::column(width = 3),
              shiny::column(
                width = 3,
                align = "center",
                shiny::div(
                  style = "display: block; width: 100%;",
                  shiny::downloadButton(
                    outputId = "save_progress_button",
                    label = "Save Progress",
                    style = c(
                      "background-color: #0275d8;
                       color: white;
                       font-weight: 600;
                       font-size: 16px;
                       padding: 12px 24px;
                       border-radius: 6px;
                       border: none;
                       width: 100%;
                       text-align: center;
                       box-shadow: 0px 2px 4px rgba(0,0,0,0.1);
                       "),
                    width = "80%"
                  )
                )
              ),
              shiny::column(
                width = 3,
                align = "center",
                shiny::uiOutput(outputId = "save_finalized_results")
              ),
              shiny::column(width = 3)
            )
          )
        )
      )
    )
  )

}

