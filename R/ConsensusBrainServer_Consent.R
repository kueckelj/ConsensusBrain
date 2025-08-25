ConsensusBrainServer_Consent <- function(input, output, session, nifti_object, project = ""){

  require(oro.nifti)
  require(shiny)
  require(shinyBS)

  # Global ------------------------------------------------------------------
  shinyhelper::observe_helpers()

  cb_df <- shiny::reactiveVal({

    dplyr::left_join(
      x = load_consensus_template(subcortical = FALSE, t1 = FALSE),
      y = preliminary_consensus@progress,
      by = c("x", "y", "z")
    )

  })

  global_settings <- global_settings_default

  nifti_input <- shiny::reactiveVal(value = nifti_object)

  reminder_disabled <- shiny::reactiveVal(value = FALSE)

  saving_counter <- shiny::reactiveVal(value = -1) # initiating adds 1 -> 0
  saving_reminder <- shiny::reactiveVal(value = 5)

  # idle warning
  shiny::observeEvent(input$idle_warning, {

    shiny::showModal(
      shiny::modalDialog(
        title = "Inactivity Warning",
        "You have been inactive for 10 minutes. The session will disconnect in 5 minutes unless activity resumes.",
        easyClose = TRUE
      )
    )

  })

  result_altered <- shiny::reactiveVal(value = FALSE)


  # Intro text --------------------------------------------------------------

  # triggers once at the initiation
  shiny::observe({

    showModalIntro()

  })

  shiny::observeEvent(input$open_intro_consent, {

    showModalIntro()

  })

  shiny::observeEvent(input$open_intro_adjust, {

    showModalIntro()

  })

  shiny::observeEvent(input$close_intro, {

    shiny::removeModal()

  })

  shiny::observeEvent(input$open_guide_consent, {

    showModalGuideConsent()

  })

  shiny::observeEvent(input$close_guide_consent, {

    shiny::removeModal()

  })

  shiny::observeEvent(input$open_guide_adjust, {

    showModalGuideAdjust()

  })

  shiny::observeEvent(input$close_guide_adjust, {

    shiny::removeModal()

  })

  # Tab Consent ------------------------------------------------------------

  output$submit_consent <- shiny::renderUI({

    if(!result_altered()){

      style = "display: block; width: 100%; margin-top: 5%;"

    } else {

      style = "display: block; width: 100%; margin-top: 5%; opacity: 0.5;"

    }

    shiny::div(
      style = style,
      shinyWidgets::actionBttn(
        inputId = "submit_consent",
        label = "Submit Consent",
        style = "gradient",
        color = "success",
        icon = shiny::icon("thumbs-up"),
        block = TRUE
      )
    )

  })

  output$submit_consent_helptext <- shiny::renderUI({

    if(!iv$is_valid()){

      shiny::helpText("Please enter the required information.")

    } else {

      shiny::helpText("Click on Confirm to submit.")

    }

  })

  # Summon submit consent modal
  submit_consent_meta <-
    shiny::reactiveVal(
      value = list(
        first_name = character(),
        last_name = character(),
        email = character(),
        email_confirm = character()
      )
    )

  # - input validation
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("userInp_first_name", shinyvalidate::sv_required())
  iv$add_rule("userInp_first_name", ~ if(!valid_name_length(.)) "Two or more letters required.")
  iv$add_rule("userInp_first_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")
  iv$add_rule("userInp_last_name", shinyvalidate::sv_required())
  iv$add_rule("userInp_last_name", ~ if(!valid_name_length(.)) "Two or more letters required.")
  iv$add_rule("userInp_last_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")
  iv$add_rule("userInp_email", shinyvalidate::sv_required())
  iv$add_rule("userInp_email", shinyvalidate::sv_email())
  iv$add_rule("userInp_email_confirm", shinyvalidate::sv_required())
  iv$add_rule("userInp_email_confirm", function(value) {
    if(value != input$userInp_email){
      "The email addresses do not match."
    }
  })
  iv$enable()

  shiny::observe({

    submit_consent_meta({
      list(
        first_name = input$userInp_first_name,
        last_name = input$userInp_last_name,
        email = input$userInp_email,
        email_confirm = input$userInp_email_confirm
      )
    })

    if(iv$is_valid()){

      shiny::updateActionButton(
        session = session,
        inputId = "submit_consent_confirm",
        disabled = FALSE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "submit_consent_confirm",
        disabled = FALSE
      )

    }

  })

  shiny::observeEvent(input$submit_consent, {

    if(result_altered()){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Map has been changed!",
        text = "You have changed the score of at least one brain region.
        Please navigate to the Adjust-tab and use the `Download Adjustments` button to download the map you would consent to and send it to us by mail.",
        type = "info"
      )

      shiny::req(FALSE)

    }

    showModalSubmitConsent()

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$submit_consent_confirm, {

    scm <- submit_consent_meta()

    send_mail(
      subject = "ConsensusBrain: Consent successfully submitted",
      text = glue::glue(
        "Dear {scm$first_name} {scm$last_name},

        We have received your consent for the ConsensusBrain resectability map.

        We truly appreciate your time and contribution to this study.

        Best regards,

        Philipp Karschnia and Jan Kueckelhaus
        "
      ),
      to = scm$email,
      password = Sys.getenv("GMX_PASS")
    )

    send_mail(
      subject = glue::glue("Consent: {scm$first_name} {scm$last_name}"),
      text = glue::glue("Received Consent from {scm$first_name} {scm$last_name} (E-Mail: {scm$email})."),
      to = "philipp.karschnia@uk-erlangen.de",
      password = Sys.getenv("GMX_PASS")
    )

    send_mail(
      subject = glue::glue("Consent: {scm$first_name} {scm$last_name}"),
      text = glue::glue("Received Consent from {scm$first_name} {scm$last_name} (E-Mail: {scm$email})."),
      to = "jankueckelhaus@gmx.de",
      password = Sys.getenv("GMX_PASS")
    )

    shiny::removeModal()

    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Consent submitted!",
      text = glue::glue(
        "Your consent has been submitted! You can close this page now."
      ),
      type = "success"
    )

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$submit_consent_cancel, {

    shiny::removeModal()

  })

  module_mri_consent <-
    moduleMriServer(
      id = "consent",
      mode_init = "inspection",
      mode_opts = list(inspection_var = "CBscore_smooth"),
      voxel_df = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() }),
      reset_quick_select = shiny::reactive({ reset_quick_select() })
    )

  # Tab Adjust ----------------------------------------------------------------

  # Dynamic UI
  output$download_adjustments <- shiny::renderUI({

    if(result_altered()){

      style = "display: block; width: 100%; margin-top: 5%;"

    } else {

      style = "display: block; width: 100%; margin-top: 5%; opacity: 0.5;"

    }

    shiny::div(
      style = style,
      shinyWidgets::actionBttn(
        inputId = "download_adjustments",
        label = "Download Adjustments",
        style = "gradient",
        color = "primary",
        icon = shiny::icon("download"),
        block = TRUE
      )
    )

  })

  output$download_adjustments_confirmX <- shiny::renderUI({

    if(iv$is_valid()){

      shiny::downloadButton(
        outputId = "download_adjustments_confirm",
        label = "Download",
        width = "100%"
      )

    } else {

      shiny::helpText("Please enter your credentials.")

    }

  })

  # Reactive Values
  reset_quick_select <- shiny::reactiveVal(value = 0)

  # Module Output
  module_mri_adjust <-
    moduleMriServer(
      id = "adjust",
      mode_init = "inspection",
      mode_opts = list(highlight_hover = TRUE, quick_select = TRUE, inspection_var = "CBscore_smooth"),
      voxel_df = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() }),
      reset_quick_select = shiny::reactive({ reset_quick_select() })
    )

  # Handle quick selection
  voxels_quick_selected <- shiny::reactiveVal(value = character())

  shiny::observe({

    if(!identical(x = voxels_quick_selected(), y = module_mri_adjust()$voxels_quick_select)){

      voxels_quick_selected({ module_mri_adjust()$voxels_quick_select })

    }

  })

  # Summon rescore modal
  shiny::observeEvent(voxels_quick_selected(), {

    if(length(voxels_quick_selected()) != 0){

      region_qselected <- dplyr::filter(cb_df(), id %in% voxels_quick_selected())

      atlas  <- module_mri_adjust()$highlight_var
      region <- unique(region_qselected[[atlas]])
      hemisphere <- unique(region_qselected[["hemisphere"]])

      showModalQuickRescore(
        atlas = atlas,
        region = region,
        hemisphere = hemisphere
      )

    }

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$rescore_confirm, {

    shiny::removeModal()

    shinyWidgets::sendSweetAlert(
      session = session,
      title = "New score received!",
      text = "Integrating the score in the map. This should only take a few seconds.",
      type = "success",
      btn_labels = NA
    )

    cb_df({

      dplyr::mutate(
        .data = cb_df(),
        CBscore = dplyr::if_else(
          condition = id %in% voxels_quick_selected(),
          true = as.numeric(input$CBrescore),
          false = CBscore
          )
      ) %>%
        smooth_cbscore_continuous(sigma = 1.5, snap_to_half_steps = FALSE)

    })

    reset_quick_select({ reset_quick_select()+1 })
    result_altered(TRUE)

    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Map adjusted!",
      type = "success"
    )

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$rescore_cancel, {

    reset_quick_select({ reset_quick_select()+1 })
    shiny::removeModal()

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$CBrescore, {

    if(shiny::isTruthy(input$CBscore)){

      shiny::updateActionButton(
        session = session,
        inputId = "rescore_confirm",
        disabled = FALSE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "rescore_confirm",
        disabled = FALSE
      )

    }

  }, ignoreInit = TRUE)

  output$plot_region_score_distr <- shiny::renderPlot({

    df <- dplyr::filter(cb_df(), id %in% voxels_quick_selected())

    add_cb_gradient_bg <- function() {
      pal <- grDevices::colorRampPalette(score_set_up$colors[-1], space = "Lab")
      r   <- grDevices::as.raster(matrix(pal(1024), nrow = 1))
      ggplot2::annotation_raster(r, xmin = 0.95, xmax = 4.05, ymin = -Inf, ymax = Inf)
    }

    ggplot2::ggplot(df, ggplot2::aes(x = CBscore_smooth)) +
      add_cb_gradient_bg() +
      ggplot2::geom_density(
        color = "black",
        fill = ggplot2::alpha("lightgrey", 0.5),
        linewidth = 0.9
        ) +
      ggplot2::scale_x_continuous(
        limits = c(0.95, 4.05),
        expand = c(0, 0),
        breaks = 1:4,
        labels = c("Low", "Low-Medium", "Medium-High", "High")
      ) +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(vjust = 0.25),
        panel.grid = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "#f9fafb", color = NA),
        text = ggplot2::element_text(size = 15)
      ) +
      ggplot2::labs(x = "Consensus-Based Resectability Risk", y = "Density")

  })

  # Summon modal download adjustments
  shiny::observeEvent(input$download_adjustments, {

    if(!result_altered()){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Map has not been changed!",
        text = "No adjustments have been made to the preliminary map.
        If you consent without any further adjustments, navigate to the Consent-tab and click on `Submit Consent`.",
        type = "info"
      )

      shiny::req(FALSE)

    }

    showModalDownloadAdjustments()

  }, ignoreInit = TRUE)

  download_adjustments_clicked <- shiny::reactiveVal(value = 0)
  output$download_adjustments_confirm <- shiny::downloadHandler(
    filename = function() {

      scm <- submit_consent_meta()

      paste0("CB_adjusted_", scm$first_name, "_", scm$last_name, ".RDS")

    },
    content = function(file) {

      # download
      output <- cb_df()[,c("x", "y", "z", "CBscore")]
      download_adjustments_clicked({ download_adjustments_clicked()+1 })

      saveRDS(output, file)

    }
  )

  shiny::observeEvent(download_adjustments_clicked(), {

    if(download_adjustments_clicked() > 0){

      shiny::removeModal()

      # send by mail
      output <- cb_df()[,c("x", "y", "z", "CBscore")]
      scm <- submit_consent_meta()
      file <- paste0("CB_adjusted_", scm$first_name, "_", scm$last_name, ".RDS")
      tf <- file.path(tempdir(), file)
      saveRDS(output, file = tf)

      send_mail(
        subject = glue::glue("Adjusted Consent: {scm$first_name} {scm$last_name}"),
        text = glue::glue("Received adjusted consent from {scm$first_name} {scm$last_name} (E-Mail: {scm$email})."),
        to = "jankueckelhaus@gmx.de",
        password = Sys.getenv("GMX_PASS"),
        attachment_path = tf
      )

      send_mail(
        subject = glue::glue("Adjusted Consent: {scm$first_name} {scm$last_name}"),
        text = glue::glue("Received adjusted consent from {scm$first_name} {scm$last_name} (E-Mail: {scm$email})."),
        to = "philipp.karschnia@uk-erlangen.de",
        password = Sys.getenv("GMX_PASS"),
        attachment_path = tf
      )

      unlink(tf)

      # give feedback
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Almost done!",
        text = tags$html(
          "Please email the file you downloaded to philipp.karschnia@uk-erlangen.de."
        ),
        type = "success"
      )

    }

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$download_adjustments_cancel, {

    shiny::removeModal()

  }, ignoreInit = TRUE)

}



