
ConsensusBrainServer <- function(input, output, session, nifti_object){


  # Global ------------------------------------------------------------------
  shinyhelper::observe_helpers()
  shinyalert::useShinyalert()



  cb_df <- shiny::reactiveVal({

    dplyr::mutate(
      .data = ctp_df,
      # variables for score assignment
      CBscore = 0,
      CBscore_label = "Missing",
      # variables for interactive selection + refinement
      selected = FALSE,
      broi = FALSE,
      color = alpha("forestgreen", alpha_val),
      is_margin = FALSE,
      is_margin_cand = FALSE
      )

  })

  non_brain_template <- load_non_brain_template()

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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
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
      non_brain_template = non_brain_template,
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_remaining(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_remaining())

    })

  }, ignoreInit = TRUE)




# Tab Progress ----------------------------------------------------------------

  output$header_progress <- shiny::renderUI({

    progress <- ceiling((sum(cb_df()$CBscore != 0)/nrow(cb_df()))*100)

    header <- paste0("Progress: ", progress, "%" )

    shiny::h3(shiny::strong(header))

  })

  module_mri <-
    moduleMriServer(
      id = "progress",
      mode_init = "inspection",
      non_brain_template = non_brain_template,
      voxel_df = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )


  output$brain3D_progress_plot <- plotly::renderPlotly({

    CBscore_label_colors <-
      purrr::set_names(
        x = score_set_up$colors,
        nm = names(score_set_up$choices)
      )

    cb_df_plot <-
      trim_brain_3d(cb_df(), var = "CBscore_label", val_missing = "Missing", fct = 0.35)

    plot_brain_3d(
      voxel_df = cb_df_plot,
      color_by = "CBscore_label",
      group_highlight = names(score_set_up$choices[-1]),
      clrp_adjust = CBscore_label_colors,
      opacity_hide = 0.025,
      pt_size = 1.5,
      show_legend = FALSE,
      hoverinfo = c("CBscore_label"),
      paper_bgcolor = "black"
    )

  })


  output$circular_progress_plot <- shiny::renderPlot({

    circular_progress_plot(cb_df(), score_set_up = score_set_up)

  })


}









