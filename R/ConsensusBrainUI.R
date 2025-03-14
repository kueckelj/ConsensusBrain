
ConsensusBrainUI <- function(){

  shinydashboard::dashboardPage(

    # header =
    shinydashboard::dashboardHeader(title = "ConsensusBrain"),

    # sidebar =
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Brain Template",
          tabName = "tab_brain_template",
          shinydashboard::menuSubItem(
            text = "3D",
            tabName = "tab_brain_template_3D",
            selected = FALSE
          ),
          shinydashboard::menuSubItem(
            text = "MRI",
            tabName = "tab_brain_template_MRI",
            selected = FALSE
          )
        ),
        shinydashboard::menuItem(
          text = "Score Assignment",
          tabName = "tab_score_assignment",
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

          moduleWorkflowMacroAreaUI(id = "workflow_frontal_lobe")

        ),

        # Temporal Lobe
        shinydashboard::tabItem(
          tabName = "tab_temporal_lobe",

          moduleWorkflowMacroAreaUI(id = "workflow_temporal_lobe")

        ),

        # Parietal Lobe
        shinydashboard::tabItem(
          tabName = "tab_parietal_lobe",

          moduleWorkflowMacroAreaUI(id = "workflow_parietal_lobe")

        ),

        # Occipital Lobe
        shinydashboard::tabItem(
          tabName = "tab_occipital_lobe",

          moduleWorkflowMacroAreaUI(id = "workflow_occipital_lobe")

        ),

        # Insular Lobe
        shinydashboard::tabItem(
          tabName = "tab_insular_lobe",

          moduleWorkflowMacroAreaUI(id = "workflow_insular_lobe")

        ),

        # Cingulate Lobe
        shinydashboard::tabItem(
          tabName = "tab_cingulate_lobe",

          moduleWorkflowMacroAreaUI(id = "workflow_cingulate_lobe")

        ),

        # Subcortical
        shinydashboard::tabItem(
          tabName = "tab_subcortical",

          moduleWorkflowMacroAreaUI(id = "workflow_subcortical")

        ),

        # Corpus Callosum
        shinydashboard::tabItem(
          tabName = "tab_corpus_callosum",

          moduleWorkflowMacroAreaUI(id = "workflow_corpus_callosum")

        ),

        # Infratentorial
        shinydashboard::tabItem(
          tabName = "tab_infratentorial",

          moduleWorkflowMacroAreaUI(id = "workflow_infratentorial")

        ),

        # White Matter Tracts
        shinydashboard::tabItem(
          tabName = "tab_wm_tracts",

          moduleWorkflowMacroAreaUI(id = "workflow_wm_tracts")

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
              width = 6,
              plotly::plotlyOutput("brain3D_progress_plot", height = "500px")
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput(outputId = "circular_progress_plot", height = "500px")
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

