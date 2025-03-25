
ConsensusBrainServer <- function(input, output, session, nifti_object){


  # Global ------------------------------------------------------------------
  shinyhelper::observe_helpers()



  cb_df <- shiny::reactiveVal({

    dplyr::mutate(
      .data = ctp_df,
      # variables for interactive selection + refinement
      selected = FALSE,
      broi = FALSE,
      color = alpha("forestgreen", alpha_val),
      is_margin = FALSE,
      is_margin_cand = FALSE,
      smart_label =
        dplyr::case_when(
          is_wm ~ "wm",
          ann_macro == "corpus_callosum" ~ "cc",
          TRUE ~ ann_dk_adj
        ),
      # variables for score assignment
      CBscore = 0
    )

  })

  global_settings <- global_settings_default

  nifti_input <- shiny::reactiveVal(value = nifti_object)


  # debug
  shiny::observeEvent(input$test, {

    assign("cb_df", cb_df(), envir = .GlobalEnv)

  })

# Tab Score Assignment ------------------------------------------------------

  # Frontal Lobe ------------------------------------------------------------

  mo_score_frontal_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_frontal_lobe",
      macro_area = "frontal_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_frontal_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_frontal_lobe())

    })

  }, ignoreInit = TRUE)

  # Temporal Lobe ------------------------------------------------------------

  mo_score_temporal_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_temporal_lobe",
      macro_area = "temporal_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_temporal_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_temporal_lobe())

    })

  }, ignoreInit = TRUE)

  # Parietal Lobe ------------------------------------------------------------

  mo_score_parietal_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_parietal_lobe",
      macro_area = "parietal_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_parietal_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_parietal_lobe())

    })

  }, ignoreInit = TRUE)

  # Occipital Lobe ------------------------------------------------------------

  mo_score_occipital_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_occipital_lobe",
      macro_area = "occipital_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_occipital_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_occipital_lobe())

    })

  }, ignoreInit = TRUE)

  # Insular Lobe ------------------------------------------------------------

  mo_score_insular_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_insular_lobe",
      macro_area = "insular_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_insular_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_insular_lobe())

    })

  }, ignoreInit = TRUE)

  # Cingulate Lobe ------------------------------------------------------------

  mo_score_cingulate_lobe <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_cingulate_lobe",
      macro_area = "cingulate_lobe",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_cingulate_lobe(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_cingulate_lobe())

    })

  }, ignoreInit = TRUE)

  # Subcortical  ------------------------------------------------------------

  mo_score_subcortical <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_subcortical",
      macro_area = "subcortical",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_subcortical(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_subcortical())

    })

  }, ignoreInit = TRUE)

  # Corpus Callosum  ---------------------------------------------------------

  mo_score_corpus_callosum <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_corpus_callosum",
      macro_area = "corpus_callosum",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_corpus_callosum(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_corpus_callosum())

    })

  }, ignoreInit = TRUE)

  # Infratentorial  ---------------------------------------------------------

  mo_score_infratentorial <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_infratentorial",
      macro_area = "infratentorial",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_infratentorial(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_infratentorial())

    })

  }, ignoreInit = TRUE)

  # White Matter Tracts -----------------------------------------------------

  mo_score_wm_tracts <-
    moduleWorkflowMacroAreaServer(
      id = "workflow_wm_tracts",
      macro_area = "wm_tract",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_wm_tracts(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_wm_tracts())

    })

  }, ignoreInit = TRUE)



  # Remaining ---------------------------------------------------------------

  mo_score_remaining <-
    moduleWorkflowRemainingServer(
      id = "workflow_remaining",
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_remaining(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_remaining())

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

    circular_progress_plot(cb_df(), score_set_up = score_set_up)

  })


}









