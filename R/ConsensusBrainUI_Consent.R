ConsensusBrainUI_Consent <- function(project = ""){

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
          text = "Consent",
          icon = shiny::icon("brain"),
          tabName = "tab_consent",
          selected = TRUE
        ),
        shinydashboard::menuItem(
          text = "Adjust",
          tabName = "tab_adjust",
          icon = shiny::icon("edit"),
          selected = FALSE
        )
      ),

      shiny::br(),

      # Help text at bottom
      shiny::tags$div(
        style = c(
          "position: absolute;
         bottom: 10px;
         width: 100%;
         text-align: center;
         font-size: 11px;
         color: gray;
         padding: 5px;"
        ),
        shiny::tags$img(
          src = ifelse(local_launch(), "www/rano_resect_Logo_nbg.png", "rano_resect_logo_nbg.png"),
          style = "width: 100%;"
        ),
        shiny::helpText(
          shiny::HTML(
            "ConsensusBrain<sup style='font-size: 7.5px; color: gray;'>&copy;</sup>
            is an initiative of the RANOResect research group and was developed by
            Jan Kueckelhaus and Philipp Karschnia at the Department of Neurosurgery,
            University Hospital Erlangen (Chair: Prof. Dr. O. Schnell)."
          )
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
          .video-container {
            background-color: white;
            border-radius: 5px;
            box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
            display: flex;
            flex-direction: column;
            padding: 1.5%;
            padding-bottom: 0.5%;
            padding-top: 0.125%;
            width: 100%;
            margin-bottom: 1.5%;
          }
        ")
        ),
        shiny::tags$script(
          shiny::HTML(
            "
            var idleTime = 0;
            var idleInterval;

            function resetIdleTime() {
              idleTime = 0;
              console.log('Idle time reset');
            }

            $(document).ready(function () {
              idleInterval = setInterval(function () {
                idleTime++;
                console.log('Idle time:', idleTime);
                if (idleTime === 10) {
                  Shiny.setInputValue('idle_warning', true, {priority: 'event'});
                }
                if (idleTime === 15) {
                  Shiny.setInputValue('disconnect', true, {priority: 'event'});
                }
              }, 60000); // every minute (60k miliseconds)

              $(document).on('mousemove keypress click scroll', resetIdleTime);
            });

            $(document).on('click', '#terms_link', function(e) {
             e.preventDefault();
             Shiny.setInputValue('terms_link_clicked', true, {priority: 'event'});
            });
            "
          )
        )
      ),

      # tabs
      shinydashboard::tabItems(

        # Consent ----------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_consent",
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shinyWidgets::actionBttn(
                inputId = "open_consent_intro",
                label = "Intro Text",
                style = "simple",
                color = "primary",
                icon = shiny::icon("book-open")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              moduleMriUI(id = "consent")
            )
          ),
          shiny::fluidRow(
            shiny::column(width = 2),
            shiny::column(
              width = 4,
              align = "center",
              shiny::uiOutput(outputId = "submit_consent")
            ),
            shiny::column(
              width = 4,
              align = "center",
              shiny::uiOutput(outputId = "download_adjustments")
            ),
            shiny::column(width = 2)
          )
        ),

        # Adjust --------------------------------------------------------

        # Workflow
        shinydashboard::tabItem(
          tabName = "tab_adjust",

          moduleWorkflowMacroAreaUI(id = "adjust")

        )

      )
    )
  )

}
