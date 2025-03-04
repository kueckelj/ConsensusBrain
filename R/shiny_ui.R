
ConsensusBrain_ui <- function(){
  
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
        
        # Brain Annotations
        shinydashboard::tabItem(
          tabName = "tab_brain_template_MRI", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleVoxelSelectionUI(id = "tabBT_VoxelSelection")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "tabBT_Score")
            )
          ),
          
          shiny::br(),
          
          shiny::fluidRow(
            
            # MRI column 1
            shiny::column(
              width = 4,
              align = "left",
              moduleMriPlaneUI(id = "tabBT_MRI_sag", plane = "sag")
            ),
            
            # MRI column 2
            shiny::column(
              width = 4, 
              align = "left",
              moduleMriPlaneUI(id = "tabBT_MRI_axi", plane = "axi"), 
            ),
            
            # MRI column 3
            shiny::column(
              width = 4, 
              align = "left",
              moduleMriPlaneUI(id = "tabBT_MRI_cor", plane = "cor"), 
            )
            
          ) 
        ), 
        

        # Score Assignment --------------------------------------------------------
        
        # Frontal Lobe
        shinydashboard::tabItem(
          tabName = "tab_frontal_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "frontal_lobe", title = "Frontal Lobe")
            ), 
            shiny::column(
              width = 4, 
              moduleScoreAssignmentUI(id = "frontal_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "frontal_lobe")
            )
          )
          
        ), 
        # Temporal Lobe
        shinydashboard::tabItem(
          tabName = "tab_temporal_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "temporal_lobe", title = "Temporal Lobe")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "temporal_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "temporal_lobe")
            )
          )
        ), 
        # Parietal Lobe
        shinydashboard::tabItem(
          tabName = "tab_parietal_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "parietal_lobe", title = "Parietal Lobe")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "parietal_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "parietal_lobe")
            )
          )
        ), 
        # Occpital Lobe
        shinydashboard::tabItem(
          tabName = "tab_occipital_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "occipital_lobe", title = "Occipital Lobe")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "occipital_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "occipital_lobe")
            )
          )
        ), 
        # Insular Lobe
        shinydashboard::tabItem(
          tabName = "tab_insular_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "insular_lobe", title = "Insular Lobe")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "insular_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "insular_lobe")
            )
          )
        ),
        # Cingulate Lobe
        shinydashboard::tabItem(
          tabName = "tab_cingulate_lobe", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionLobeUI(id = "cingulate_lobe", title = "Cingulate Lobe")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "cingulate_lobe")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "cingulate_lobe")
            )
          )
        ),
        # Subcortical
        shinydashboard::tabItem(
          tabName = "tab_subcortical", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionUI(id = "subcortical", title = "Subcortical")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "subcortical")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "subcortical")
            )
          )
        ),
        # Corpus Callosum
        shinydashboard::tabItem(
          tabName = "tab_corpus_callosum", 
          
          shiny::fluidRow(
            shiny::column(
              width = 4, 
              moduleBrainTissueSelectionUI(id = "corpus_callosum", title = "Corpus Callosum")
            ), 
            shiny::column(
              width = 2, 
              moduleScoreAssignmentUI(id = "corpus_callosum")
            )
          ), 
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              moduleMriUI(id = "corpus_callosum")
            )
          )
        ),

        # Progress ----------------------------------------------------------------

        shinydashboard::tabItem(
          tabName = "tab_progress", 
          shiny::fluidRow(
            shiny::column(
              width = 6, 
              plotly::plotlyOutput("brain3D_progress_plot", height = "750px") 
            ), 
            shiny::column(
              width = 6, 
              shiny::plotOutput(outputId = "circular_progress_plot")
            )
          )
        )
        
      )
      
    )
  )
  
}

