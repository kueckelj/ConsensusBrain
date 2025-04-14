

ConsensusBrainUI <- function(){

  shinydashboard::dashboardPage(

    # header =
    shinydashboard::dashboardHeader(
      title = shiny::HTML("ConsensusBrain")
      ),

    # sidebar =
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,

      # Menu
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

      # Logout button
      shiny::column(
        width = 12,
        align = "center",
        shiny::actionButton(
          inputId = "logout",
          label = "Logout",
          icon = shiny::icon("sign-out-alt"),
          style = "
        width: 90%;
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
        border-left: 0;"
        )
      ),

      # Help text at bottom
      shiny::tags$div(
        style = "
      position: absolute;
      bottom: 10px;
      width: 100%;
      text-align: center;
      font-size: 11px;
      color: gray;
      padding: 5px;",
        shiny::tags$img(
          src = "www/rano_resect_logo_nbg.png",
          style = "width: 100%;"
        ),
        shiny::helpText(
          shiny::HTML("ConsensusBrain<sup style='font-size: 7.5px; color: gray;'>&copy;</sup> was developed by Jan Kückelhaus and Philipp Karschnia.")
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

          moduleWorkflowMacroAreaUI(id = "workflow")

        ),

        # Remaining
        shinydashboard::tabItem(
          tabName = "tab_remaining",

          moduleWorkflowRemainingUI(id = "workflow_remaining")

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
          shiny::fluidRow(
            shiny::column(
              width = 12,
              moduleMriUI(id = "progress")
            )
          ),
          shiny::fluidRow(
            shiny::column(width = 3),
            shiny::column(
              width = 3,
              align = "center",
              shiny::div(
                style = "display: block; width: 100%; margin-top: 5%;",
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

}

