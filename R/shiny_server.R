
ConsensusBrain_server <- function(input, output, session){
  
  # defined values 
  mri_slices_wd <- 
    purrr::map(
      .x = mri_list_T1, 
      .f = ~ purrr::map_lgl(.x, .f = ~ !length(unique(.x))==1) %>% which() 
    ) %>% 
    purrr::flatten_int() %>% 
    range()
  
  #mri_frame <- c(min(mri_slices_wd)-5, max(mri_slices_wd)+5)
  mri_frame <- c(1, 256)
  
  global_settings <- global_settings_default
  
  cb_df <- shiny::reactiveVal({
    
    dplyr::mutate(
      .data = ctp_df, 
      # variables for interactive selection + refinement
      selected = FALSE, 
      broi = FALSE, 
      color = alpha("forestgreen", 0.45), 
      # variables for score assignment
      CBscore = 0 
    )
    
  })
  

# Brain Template - MRI ----------------------------------------------------
  
  # --- test
  
  shiny::observeEvent(input$test,{
    
    print(cb_df() %>% filter(CBscore != 0))
    
    
  }, ignoreInit = TRUE)
  
  # modules 
  mo_score_assignment_tabBT <- 
    moduleScoreAssignmentServer(
      id = "tabBT_Score", 
      voxel_df_input = shiny::reactive({ mo_mri_control_tabBT()$voxel_df })
    )
  
  mo_voxel_selection <- 
    moduleVoxelSelectionServer(
      id = "tabBT_VoxelSelection", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_control_tabBT <- 
    moduleMriControlServer(
      id = "tabBT_MRI_control", 
      mri_sag_out = shiny::reactive({ mo_tbt_MRI_sag() }), 
      mri_axi_out = shiny::reactive({ mo_tbt_MRI_axi() }), 
      mri_cor_out = shiny::reactive({ mo_tbt_MRI_cor() }), 
      voxel_df_input = shiny::reactive({ cb_df() }), 
      external_selection = shiny::reactive({ mo_voxel_selection() })
    )
  
  mo_tbt_MRI_sag <- 
    moduleMriPlaneServer(
      id = "tabBT_MRI_sag",
      mri_control = shiny::reactive({ mo_mri_control_tabBT() }),
      mri_list = shiny::reactive({ mri_list_T1 }), 
      mri_frame = mri_frame,
      plane = "sag"
    )
  
  mo_tbt_MRI_axi <- 
    moduleMriPlaneServer(
      id = "tabBT_MRI_axi", 
      mri_control = shiny::reactive({ mo_mri_control_tabBT() }),
      mri_list = shiny::reactive({ mri_list_T1 }), 
      mri_frame = mri_frame,
      plane = "axi"
    )
  
  mo_tbt_MRI_cor <- 
    moduleMriPlaneServer(
      id = "tabBT_MRI_cor",
      mri_control = shiny::reactive({ mo_mri_control_tabBT() }),
      mri_list = shiny::reactive({ mri_list_T1 }),
      mri_frame = mri_frame,
      plane = "cor"
    )

# Tab Score Assignment ------------------------------------------------------
  
  # Frontal Lobe ------------------------------------------------------------
  
  mo_selection_frontal_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "frontal_lobe", 
      lobe = "frontal", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_frontal_lobe <- 
    moduleMriServer(
      id = "frontal_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_frontal_lobe() })
    )
  
  mo_score_frontal_lobe <- 
    moduleScoreAssignmentServer(
      id = "frontal_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_frontal_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_frontal_lobe(), {

    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_frontal_lobe())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Temporal Lobe ------------------------------------------------------------
  
  mo_selection_temporal_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "temporal_lobe", 
      lobe = "temporal", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_temporal_lobe <- 
    moduleMriServer(
      id = "temporal_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_temporal_lobe() })
    )
  
  mo_score_temporal_lobe <- 
    moduleScoreAssignmentServer(
      id = "temporal_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_temporal_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_temporal_lobe(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_temporal_lobe())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Parietal Lobe ------------------------------------------------------------
  
  mo_selection_parietal_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "parietal_lobe", 
      lobe = "parietal", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_parietal_lobe <- 
    moduleMriServer(
      id = "parietal_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_parietal_lobe() })
    )
  
  mo_score_parietal_lobe <- 
    moduleScoreAssignmentServer(
      id = "parietal_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_parietal_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_parietal_lobe(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_parietal_lobe())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Occipital Lobe ------------------------------------------------------------
  
  mo_selection_occipital_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "occipital_lobe", 
      lobe = "occipital", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_occipital_lobe <- 
    moduleMriServer(
      id = "occipital_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_occipital_lobe() })
    )
  
  mo_score_occipital_lobe <- 
    moduleScoreAssignmentServer(
      id = "occipital_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_occipital_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_occipital_lobe(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_occipital_lobe())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Insular Lobe ------------------------------------------------------------
  
  mo_selection_insular_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "insular_lobe", 
      lobe = "insular", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_insular_lobe <- 
    moduleMriServer(
      id = "insular_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_insular_lobe() })
    )
  
  mo_score_insular_lobe <- 
    moduleScoreAssignmentServer(
      id = "insular_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_insular_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_insular_lobe(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_insular_lobe())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Cingulate Lobe ------------------------------------------------------------
  
  mo_selection_cingulate_lobe <- 
    moduleBrainTissueSelectionLobeServer(
      id = "cingulate_lobe", 
      lobe = "cingulate", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_cingulate_lobe <- 
    moduleMriServer(
      id = "cingulate_lobe", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_cingulate_lobe() })
    )
  
  mo_score_cingulate_lobe <- 
    moduleScoreAssignmentServer(
      id = "cingulate_lobe", 
      voxel_df_input = shiny::reactive({ mo_mri_cingulate_lobe()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_cingulate_lobe(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_cingulate_lobe())
      
    })
    
  }, ignoreInit = TRUE)

  # Subcortical -------------------------------------------------------------
  
  mo_selection_subcortical <- 
    moduleBrainTissueSelectionServer(
      id = "subcortical", 
      label = "Subcortical", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_subcortical <- 
    moduleMriServer(
      id = "subcortical", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_subcortical() })
    )
  
  mo_score_subcortical <- 
    moduleScoreAssignmentServer(
      id = "subcortical", 
      voxel_df_input = shiny::reactive({ mo_mri_subcortical()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_subcortical(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_subcortical())
      
    })
    
  }, ignoreInit = TRUE)
  
  # Corpus Callosum  --------------------------------------------------------
  
  mo_selection_corpus_callosum <- 
    moduleBrainTissueSelectionServer(
      id = "corpus_callosum", 
      label = "Corpus Callosum", 
      voxel_df_input = shiny::reactive({ cb_df() })
    )
  
  mo_mri_corpus_callosum <- 
    moduleMriServer(
      id = "corpus_callosum", 
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }), 
      mri_list = shiny::reactive({ mri_list_T1 }), 
      external_selection = shiny::reactive({ mo_selection_corpus_callosum() })
    )
  
  mo_score_corpus_callosum <- 
    moduleScoreAssignmentServer(
      id = "corpus_callosum", 
      voxel_df_input = shiny::reactive({ mo_mri_corpus_callosum()$voxel_df })
    )
  
  shiny::observeEvent(mo_score_corpus_callosum(), {
    
    cb_df({
      
      update_CBscore(cb_df = cb_df(), update_df = mo_score_corpus_callosum())
      
    })
    
  }, ignoreInit = TRUE)

# Tab Progress ----------------------------------------------------------------
  
  output$brain3D_progress_plot <- plotly::renderPlotly({
    
    CBscore_label_colors <- c(
      "Missing"      = "lightgrey",  # Gray
      "Low"          = "#4CAF50",  # Green
      "Low-Medium"   = "#8BC34A",  # Light Green
      "Medium"       = "#FFC107",  # Yellow
      "Medium-High"  = "#FF9800",  # Orange
      "High"         = "#F44336"   # Red
    )
    
    cb_df_plot <- 
      dplyr::mutate(
        .data = cb_df(), 
        CBscore_label = 
          dplyr::case_when(
            CBscore == 0 ~ "Missing", 
            CBscore == 1 ~ "Low", 
            CBscore == 2 ~ "Low-Medium", 
            CBscore == 3 ~ "Medium", 
            CBscore == 4 ~ "Medium-High", 
            CBscore == 5 ~ "High"
          )
      )
    
    plot_brain_3d(
      voxel_df = cb_df_plot, 
      color_by = "CBscore_label", 
      group_highlight = c("Low", "Low-Medium", "Medium", "Medium-High", "High"),
      clrp_adjust = CBscore_label_colors, 
      opacity_hide = 0.05,
      pt_size = 2.5, 
      show_legend = FALSE, 
      hoverinfo = c("CBscore_label"), 
      paper_bgcolor = "black"
    )
    
  })
  
  
  output$circular_progress_plot <- shiny::renderPlot({
    
    progress <- sum(cb_df()$CBscore != 0)/nrow(cb_df())
    
    circular_progress_plot(progress = progress) 
    
  })

  
  
}









