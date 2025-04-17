

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
          tabName = "tab_introduction",
          text = "Introduction",
          icon = shiny::icon("info-circle")
        ),
        shinydashboard::menuItem(
          text = "Guide",
          tabName = "tab_guide",
          icon = shiny::icon("graduation-cap"),
          shinydashboard::menuSubItem(
            text = "Workflow",
            tabName = "tab_tut_workflow"
          ),
          shinydashboard::menuSubItem(
            text = "Selection",
            tabName = "tab_tut_selection"
          ),
          shinydashboard::menuSubItem(
            text = "Refinement",
            tabName = "tab_tut_refinement"
          )
        ),
        shinydashboard::menuItem(
          text = "Workflow",
          tabName = "tab_workflow",
          icon = shiny::icon("tasks"),
          shinydashboard::menuSubItem(
            text = "Brain Regions",
            tabName = "tab_wf_brain_regions",
            icon = shiny::icon("brain")
          ),
          shinydashboard::menuSubItem(
            text = "Remaining",
            tabName = "tab_wf_remaining",
            icon = shiny::icon("puzzle-piece")
          )
        ),
        shinydashboard::menuItem(
          text = "Progress",
          icon = shiny::icon("flag-checkered"),
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
           border-left: 0;"
          )
        )
      ),

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
            Jan Kückelhaus and Philipp Karschnia at the Department of Neurosurgery,
            University Hospital Erlangen."
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
        )
      ),

      # tabs
      shinydashboard::tabItems(

        # Home -----------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_introduction",
          shiny::tags$iframe(
            src = ifelse(local_launch(), "www/Introduction.html", "Introduction.html"),
            width = '100%',
            height = 1000,
            style = "border:none;"
          )
        ),


        # Guide -------------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_tut_workflow",
          shiny::fluidRow(
            videoBox(name = "mri_interface"),
            videoBox(name = "selection_and_score_assignment")
          ),
          shiny::fluidRow(
            videoBox(name = "the_progress_tab"),
            videoBox(name = "score_clearing")
          ),
          shiny::fluidRow(
            videoBox(name = "score_overwriting_rules")
          )
        ),

        shinydashboard::tabItem(
          tabName = "tab_tut_selection",
          shiny::fluidRow(
            videoBox(name = "brain_regions_and_atlases"),
            videoBox(name = "paintbrush")
          ),
          shiny::fluidRow(
            videoBox(name = "paintbrush_sphere"),
            videoBox(name = "paintbrush_ray")
          )
        ),

        shinydashboard::tabItem(
          tabName = "tab_tut_refinement",
          shiny::fluidRow(
            videoBox(name = "paintbrush_erase"),
            videoBox(name = "undo_and_debris_removal")
          ),
          shiny::fluidRow(
            videoBox(name = "safety_margin")
          )
        ),

        # Score Assignment --------------------------------------------------------

        # Workflow
        shinydashboard::tabItem(
          tabName = "tab_wf_brain_regions",

          moduleWorkflowMacroAreaUI(id = "workflow")

        ),

        # Remaining
        shinydashboard::tabItem(
          tabName = "tab_wf_remaining",

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

