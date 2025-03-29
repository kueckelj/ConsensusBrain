
ConsensusBrainUI <- function(){

  shinydashboard::dashboardPage(

    # header =
    shinydashboard::dashboardHeader(title = "ConsensusBrain"),

    # sidebar =
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Welcome",
          tabName = "tab_welcome"
        ),
        shinydashboard::menuItem(
          text = "Worfklow",
          tabName = "tab_workflow",
          selected = TRUE,
          shinydashboard::menuSubItem(
            text = "Frontal Lobe",
            tabName = "tab_frontal_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Temporal Lobe",
            tabName = "tab_temporal_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Parietal Lobe",
            tabName = "tab_parietal_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Occipital Lobe",
            tabName = "tab_occipital_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Insular Lobe",
            tabName = "tab_insular_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Cingulate Lobe",
            tabName = "tab_cingulate_lobe"
          ),
          shinydashboard::menuSubItem(
            text = "Subcortical",
            tabName = "tab_subcortical"
          ),
          shinydashboard::menuSubItem(
            text = "Corpus Callosum",
            tabName = "tab_corpus_callosum"
          ),
          shinydashboard::menuSubItem(
            text = "Infratentorial",
            tabName = "tab_infratentorial"
          ),
          shinydashboard::menuSubItem(
            text = "White Matter Tracts",
            tabName = "tab_wm_tracts"
          ),
          shinydashboard::menuSubItem(
            text = "Remaining",
            tabName = "tab_remaining"
          )
        ),
        shinydashboard::menuItem(
          text = "Progress",
          tabName = "tab_progress",
          selected = FALSE
        )
      ),
      shiny::actionButton(inputId = "test", label = "Test")
    ),

    # body =
    shinydashboard::dashboardBody(

      # busy spinner
      shinybusy::add_busy_spinner(spin = "cube-grid", color = "tomato", timeout = 1500),

      # tabs
      shinydashboard::tabItems(

        # Score Assignment --------------------------------------------------------

        # Frontal Lobe
        shinydashboard::tabItem(
          tabName = "tab_frontal_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_frontal_lobe",
            macro_area = "frontal_lobe"
            )

        ),

        # Temporal Lobe
        shinydashboard::tabItem(
          tabName = "tab_temporal_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_temporal_lobe",
            macro_area = "temporal_lobe"
            )

        ),

        # Parietal Lobe
        shinydashboard::tabItem(
          tabName = "tab_parietal_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_parietal_lobe",
            macro_area = "parietal_lobe"
            )

        ),

        # Occipital Lobe
        shinydashboard::tabItem(
          tabName = "tab_occipital_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_occipital_lobe",
            macro_area = "occipital_lobe"
            )

        ),

        # Insular Lobe
        shinydashboard::tabItem(
          tabName = "tab_insular_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_insular_lobe",
            macro_area = "insular_lobe"
            )

        ),

        # Cingulate Lobe
        shinydashboard::tabItem(
          tabName = "tab_cingulate_lobe",

          moduleWorkflowMacroAreaUI(
            id = "workflow_cingulate_lobe",
            macro_area = "cingulate_lobe"
            )

        ),

        # Subcortical
        shinydashboard::tabItem(
          tabName = "tab_subcortical",

          moduleWorkflowMacroAreaUI(
            id = "workflow_subcortical",
            macro_area = "subcortical"
            )

        ),

        # Corpus Callosum
        shinydashboard::tabItem(
          tabName = "tab_corpus_callosum",

          moduleWorkflowMacroAreaUI(
            id = "workflow_corpus_callosum",
            macro_area = "corpus_callosum"
            )

        ),

        # Infratentorial
        shinydashboard::tabItem(
          tabName = "tab_infratentorial",

          moduleWorkflowMacroAreaUI(
            id = "workflow_infratentorial",
            macro_area = "infratentorial"
            )

        ),

        # White Matter Tracts
        shinydashboard::tabItem(
          tabName = "tab_wm_tracts",

          moduleWorkflowMacroAreaUI(
            id = "workflow_wm_tracts",
            macro_area = "white_matter_tracts"
            )

        ),

        # Remaining
        shinydashboard::tabItem(
          tabName = "tab_remaining",

          moduleWorkflowRemainingUI(
            id = "workflow_remaining"
            )

        ),

        # Progress ----------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_progress",
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::div(
                style = paste0(
                  "background-color: white;",
                  "box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);",
                  "border: 1px solid #ccc;",
                  "height: 400px;",
                  "position: relative;"
                ),
                # Header positioned in the first 50px
                shiny::div(
                  style = paste0(
                    "position: absolute;",
                    "padding-top: 7.5px;",
                    "padding-left: 7.5px;",
                    "width: 100%",
                    "text-align: left;",
                    "z-index: 2;",
                    "font-size: 16px;",
                    "font-weight: bold;"
                  ),
                  shiny::uiOutput("header_progress")
                ),
                # Plot positioned below the header
                shiny::div(
                  style = paste0(
                    "position: absolute;",
                    "left: 0;",
                    "width: 100%;",
                    "height: 300px;",  # Fills remaining space
                    "z-index: 1;"
                  ),
                  shiny::plotOutput(outputId = "circular_progress_plot", width = "100%", height = "400px")
                )
              )
            ),
            shiny::column(
              width = 5,
              shinycssloaders::withSpinner(
                ui_element = plotly::plotlyOutput("brain3D_progress_plot", height = "400px"),
                id = "brain3D_progress_plot_spinner"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              moduleMriUI(id = "progress")
            )
          )
        )

      )

    )
  )

}

