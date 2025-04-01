
ConsensusBrainServer <- function(input, output, session, nifti_object){

  # Global ------------------------------------------------------------------
  shinyhelper::observe_helpers()

  cb_df <- shiny::reactiveVal({

    dplyr::mutate(
      .data = load_consensus_template(),
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

  output$progress_highlight_groups <- shiny::renderUI({

    shiny::req(input$progress_highlight_var)

    if(input$progress_highlight_var == "ann_macro"){

      choices <- sort(unique(cb_df()[["ann_macro"]]))

      names(choices) <- make_pretty_label(choices)

    } else if(input$progress_highlight_var == "ann_dk_adj"){

      choices <- cortical_regions_dk

      names(choices) <- make_pretty_label(names(choices))

      choices <-
        purrr::map(
          .x = choices,
          .f = ~ purrr::set_names(x = .x, nm = make_pretty_label(.x))
        )

      choices$Subcortical <- sort(unique(cb_df()$subcortical))
      names(choices$Subcortical) <- make_pretty_label(choices$Subcortical)

    } else if(input$progress_highlight_var == "ann_dt_adj"){

      choices <- cortical_regions_dt

      names(choices) <- make_pretty_label(names(choices))

      choices <-
        purrr::map(
          .x = choices,
          .f = ~ purrr::set_names(x = .x, nm = make_pretty_label(.x))
        )

      choices$Subcortical <- sort(unique(cb_df()$subcortical))
      names(choices$Subcortical) <- make_pretty_label(choices$Subcortical)

    } else if(input$progress_highlight_var == "wm_tract"){

      choices <- sort(unique(cb_df()[["wm_tract"]]))
      choices <- choices[choices != "none"]

      names(choices) <- make_pretty_label(choices)


    } else if(input$progress_highlight_var == "CBscore_label"){

      choices <- names(score_set_up$choices)

    }

    shinyWidgets::pickerInput(
      inputId = "progress_highlight_groups",
      label = "Label:",
      choices = choices,
      multiple = TRUE,
      width = "100%",
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        liveSearch = TRUE,
        container = "body",
        size = 15
      )
    )

  })

  highlight_groups <- shiny::reactiveVal(value = character(0))
  highlight_hemispheres <- shiny::reactiveVal(value = character(0))
  highlight_var <- shiny::reactiveVal(value = character(0))

  shiny::observeEvent(input$progress_highlight_reset, {

    highlight_hemispheres({ character(0) })
    highgligh_groups({ character(0) })
    highlight_var({ character(0) })

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "progress_highlight_groups",
      selected = character(0)
    )

    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "progress_highlight_hemisphere",
      selected = c("left", "right")
    )


  })

  shiny::observeEvent(input$progress_highlight_update, {

    highlight_groups({ input$progress_highlight_groups })

    highlight_hemispheres({ input$progress_highlight_hemisphere })

    highlight_var({input$progress_highlight_var })

  })

  voxel_df_progress <- shiny::reactive({

    if(length(highlight_var()) == 1){

      out <- cb_df()
      out <- out[out[[highlight_var()]] %in% highlight_groups(), ]
      out <- out[out$hemisphere %in% highlight_hemispheres(), ]

      if(nrow(out) == 0){

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "No Tissue Selected",
          text = "No tissue matches your combination of selection criteria.",
          type = "warning",
          showCloseButton = TRUE
        )

      }

      # output
      dplyr::mutate(.data = cb_df(), highlighted = id %in% out$id)

    } else {

     # output
     dplyr::mutate(.data = cb_df(), highlighted = FALSE)

    }

  })

  module_mri <-
    moduleMriServer(
      id = "progress",
      mode_init = "inspection",
      non_brain_template = non_brain_template,
      voxel_df = shiny::reactive({ voxel_df_progress() }),
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

  # Login -------------------------------------------------------------------

  # open login modal
  shiny::observeEvent(input$nothing, {

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::tags$h2(shiny::strong("Please log in"), style = "padding-top: 0;"),

        shiny::helpText(
          "Welcome to ConsensusBrain! If this is your first time here, please enter your name on the left and click on Login.
          If you've visited before and want to continue where you left off, click 'Upload' and select the file you previously
          downloaded that contains your saved progress."
        ),

        footer = shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 8, align = "center",
            shiny::actionButton(inputId = "login_username", label = "Login", width = "60%", disabled = TRUE)
          ),
          shiny::column(width = 2)
        ),

        easyClose = TRUE,
        size = "xl",

        shinyjs::hidden(
          shiny::div(
            id = "login_panel",
            style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
            shiny::fluidRow(
              shiny::column(
                width = 6,
                align = "left",
                style = "border-right: 1px solid lightgrey; height: 100%;",
                shiny::textInput(
                  inputId = "username",
                  label = shiny::tagList(shiny::icon("person"), "Username:"),
                  width = "95%"
                ),
                shiny::uiOutput(outputId = "helptext_username")
              ),
              shiny::column(
                width = 6,
                align = "left",
                shiny::fileInput(
                  inputId = "upload_file",
                  label = shiny::tagList(shiny::icon("upload"), "Upload Progress File:"),
                  width = "100%",
                  accept = ".RDS"
                )
              )
            )
          )
        )
      )
    )

  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Dynamic UI
  output$helptext_username <- shiny::renderUI({

    shiny::req(input$username)
    shiny::req(stringr::str_length(input$username) > 2)

    if(valid_username()){

      shiny::helpText("Click on Login to proceed.")

    } else {

      shiny::helpText("Invalid username. Only letters and empty space is allowed.")

    }

  })

  output$helptext_upload_file <- shiny::renderUI({

    shiny::req(CBfile())

    if(!methods::is(CBfile(), "ProgressCB")){

      shiny::helpText("Invalid file.")

    } else {

      shiny::helpText("Click on Continue to proceed.")

    }

  })

  # Reactive Values and Expressions
  CBfile <- shiny::reactiveVal(value = NULL)

  valid_username <- shiny::reactive({

    shiny::req(is.character(input$username))

    all(
      purrr::map_lgl(
        .x = strsplit(input$username, split = "")[[1]],
        .f = ~ stringr::str_detect(.x, pattern = stringr::str_c(unique(c(letters, LETTERS, " ")), collapse = "|"))
      )
    ) & stringr::str_length(input$username) > 2

  })

  # misc observers
  shiny::observeEvent(input$upload_file, {

    req(input$upload_file)

    file_path <- input$upload_file$datapath

    cb_file <- readRDS(file_path)

    if(!methods::is(cb_file, "ProgressCB")){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Invalid Input File!",
        text = "The selected file does not contain progress of a ConsensusBrain session.",
        type = "error"
      )

      CBfile({ NULL })
      shiny::req(FALSE)

    } else {

      CBfile({ cb_file })

    }


  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observeEvent(input$username, {

    if(valid_username()){

      shiny::updateActionButton(
        session = session,
        inputId = "login_username",
        disabled = FALSE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "login_username",
        disabled = TRUE
      )

    }

  })

  # process login data
  shiny::observeEvent(input$login_username, {

    # get directory and load CBfile
    CBfile({ ProgressCB(user_name = input$username, scores_assigned = 0) })

  })

  shiny::observeEvent(CBfile(), {

    shiny::removeModal(session = session)

    if(length(CBfile()@scores_assigned) != 0){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Progress Loaded!",
        text = glue::glue("Welcome back, {cb_file@user_name}."),
        type = "success",
        btn_labels = "Continue"
      )

    }

    cb_df({

      dplyr::arrange(cb_df(), id, .by_group = FALSE) %>%
        dplyr::mutate(
          CBscore = CBfile()@scores_assigned,
          CBscore_label = names(score_set_up$choices)[CBscore+1]
        )

    })


  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Save progress handler
  output$save_progress <- shiny::downloadHandler(
    filename = function() {

      name_abbr <-
        strsplit(CBfile()@user_name, split = " ")[[1]]%>%
        purrr::map_chr(.f = ~ toupper(strsplit(.x, split = "")[[1]][1])) %>%
        stringr::str_c(collapse = "")

      paste0("consensusbrain_progress_",name_abbr, "_", format(Sys.Date(), "%d%m"), ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_file@scores_assigned <-
        dplyr::arrange(cb_df(), id, .by_group = FALSE) %>%
        dplyr::pull(CBscore)

      saveRDS(cb_file, file)

    }
  )

  # Final output handler
  output$save_final_output <- shiny::downloadHandler(
    filename = function() {

      name_abbr <-
        strsplit(CBfile()@user_name, split = " ")[[1]]%>%
        purrr::map_chr(.f = ~ toupper(strsplit(.x, split = "")[[1]][1])) %>%
        stringr::str_c(collapse = "")

      paste0("consensusbrain_final_output_", name_abbr, ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_file@scores_assigned <-
        dplyr::arrange(cb_df(), id, .by_group = FALSE) %>%
        dplyr::pull(CBscore)

      saveRDS(cb_file, file)

    }
  )


}









