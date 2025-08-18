
ConsensusBrainServer <- function(input, output, session, nifti_object, project = ""){

  require(oro.nifti)
  require(shiny)
  require(shinyBS)

  # Global ------------------------------------------------------------------
  shinyhelper::observe_helpers()

  cb_df <- shiny::reactiveVal({

    dplyr::mutate(
      .data = load_consensus_template(subcortical = FALSE, t1 = FALSE),
      CBscore = 0
      )

  })

  global_settings <- global_settings_default

  nifti_input <- shiny::reactiveVal(value = nifti_object)

  reminder_disabled <- shiny::reactiveVal(value = FALSE)

  saving_counter <- shiny::reactiveVal(value = -1) # initiating adds 1 -> 0
  saving_reminder <- shiny::reactiveVal(value = 5)

  # debug
  shiny::observeEvent(finalized_clicked(), {

    if(finalized_clicked() > 0){

      showModalFinalized()

    }

  })

  shiny::observeEvent(input$send_finalized_file, {

    temp_path <- tempfile(fileext = ".rds")
    saveRDS(CBfile(), temp_path)

    pw <- Sys.getenv("GMX_PASS")

    send_mail(
      subject = paste0("ConsensusBrain Finalized - ", userName(CBfile())),
      text = paste0("Sent at ", Sys.time()),
      attachment_path = temp_path,
      password = pw
    )

    unlink(temp_path)

    shiny::removeModal()

    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Done!",
      text = "The file has been sent.",
      type = "success",
      btn_labels = "Continue"
    )

  })

  shiny::observeEvent(input$dont_send_finalized_file, {

    shiny::removeModal()

  })

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


  # Login -------------------------------------------------------------------

  # open login modal
  shiny::observeEvent(input$logout, {

    showModalWelcome()

    # ignoreInit = FALSE !!! -> is displayed when opening the app
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  shiny::observeEvent(input$back_to_welcome, {

    showModalWelcome()

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Input Validation
  iv <- shinyvalidate::InputValidator$new()

  # all
  iv$add_rule("userInp_years_of_experience", shinyvalidate::sv_required())
  iv$add_rule("userInp_annual_case_load", shinyvalidate::sv_required())

  # Original Project
  if(project == ""){

    iv$add_rule("userInp_first_name", shinyvalidate::sv_required())
    iv$add_rule("userInp_first_name", ~ if(!valid_name_length(.)) "Two or more letters required.")
    iv$add_rule("userInp_first_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")

    iv$add_rule("userInp_last_name", shinyvalidate::sv_required())
    iv$add_rule("userInp_last_name", ~ if(!valid_name_length(.)) "Two or more letters required.")
    iv$add_rule("userInp_last_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")

    iv$add_rule("userInp_affiliation", shinyvalidate::sv_required())

    iv$add_rule("userInp_email", shinyvalidate::sv_required())
    iv$add_rule("userInp_email", shinyvalidate::sv_email())

    iv$add_rule("userInp_email_confirm", shinyvalidate::sv_required())
    iv$add_rule("userInp_email_confirm", function(value) {
      if(value != input$userInp_email){
        "The email addresses do not match."
      }
    })
    iv$add_rule("userInp_country", shinyvalidate::sv_required())

  }

  # RECAP: Resectability Evaluation Comparison Across Practice levels
  if(project == "recap"){

    iv$add_rule("userInp_academic_degree", shinyvalidate::sv_required())
    iv$add_rule("userInp_n_institutions", shinyvalidate::sv_required())
    iv$add_rule("userInp_scientific_focus", shinyvalidate::sv_required())
    iv$add_rule("userInp_total_case_load", shinyvalidate::sv_required())

  }

  iv$enable()

  shiny::observeEvent(input$new_user, {

    shiny::removeModal()

    showModalNewUser(session = session, project = project)

  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$terms_read, {

    shiny::removeModal()

    showModalNewUser(session = session, project = project)

  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$terms_link_clicked, {

    shiny::removeModal()

    shiny::showModal(
      shiny::modalDialog(
        title = "Terms of Use",

        html_terms_of_use, # this is supposed to be a shiny::tagList()

        footer = shiny::tagList(
          shiny::fluidRow(
            shiny::column(width = 4),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = "terms_read",
                label = "Return",
                width = "100%"
              )
            ),
            shiny::column(width = 4)
          )
        ),

        easyClose = FALSE
      )
    )

  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  valid_user_info <- shiny::reactiveVal(value = FALSE)

  user_meta <- shiny::reactive({

    list(
      academic_degree = input$userInp_academic_degree,
      affiliation = input$userInp_affiliation,
      annual_case_load = input$userInp_annual_case_load,
      country = input$userInp_country,
      email = input$userInp_email,
      first_name = input$userInp_first_name,
      last_name = input$userInp_last_name,
      n_institutions = input$userInp_n_institutions,
      perc_surgery_awake = input$userInp_perc_surgery_awake,
      perc_surgery_ionm = input$userInp_perc_surgery_ionm,
      scientific_focus = input$userInp_scientific_focus,
      terms_of_use = input$userInp_terms_of_use,
      total_case_laod = input$userInp_total_case_load,
      years_of_experience = input$userInp_years_of_experience
    ) %>%
      purrr::discard(.p = is.null)

  })

  # Dynamic UI
  output$helptext_login <- shiny::renderUI({

    if(!iv$is_valid()){

      shiny::helpText("Please enter the required information.")

    } else if(!isTRUE(input$userInp_terms_of_use)) {

      shiny::helpText("Please agree to the terms of use.")

    } else {

      shiny::helpText("Click on Login to proceed.")

    }

  })

  output$helptext_upload_file <- shiny::renderUI({

    shiny::req(CBfile())

    if(!methods::is(CBfile(), "UserCB")){

      shiny::helpText("Invalid file.")

    } else {

      shiny::helpText("Click on Continue to proceed.")

    }

  })

  # Reactive Values and Expressions
  CBfile <- shiny::reactiveVal(value = NULL)

  # misc observers
  shiny::observeEvent(input$upload_file, {

    req(input$upload_file)

    file_path <- input$upload_file$datapath

    cb_file <- readRDS(file_path)

    if(!methods::is(cb_file, "UserCB")){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Invalid Input File!",
        text = "The selected file does not contain user information of a ConsensusBrain session.",
        type = "error"
      )

      CBfile({ NULL })
      shiny::req(FALSE)

    } else {

      CBfile({ cb_file })

    }


  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # process login data
  shiny::observeEvent(input$login, {

    if(!iv$is_valid()){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Incomplete User Credentials",
        shiny::helpText("Please provide all the required information.")
      )

      shiny::req(FALSE)

    }

    if(!input$userInp_terms_of_use){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Agreement Required",
        shiny::helpText("Please check the box confirming that you've read and acknowledged the terms of use.")
      )

      shiny::req(FALSE)

    }

    cb_file <-
      UserCB(
        created = Sys.time(),
        last_update = Sys.time(),
        user_meta = user_meta()
      )

    CBfile({ cb_file })

  })

  shiny::observeEvent(CBfile(), {

    shiny::removeModal(session = session)

    if(nrow(CBfile()@progress) != 0){

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Progress Loaded!",
        text = glue::glue("Welcome back, {CBfile()@user_name}."),
        type = "success",
        btn_labels = "Continue"
      )

      cb_df({

        dplyr::left_join(
          x = dplyr::select(cb_df(), -dplyr::any_of("CBscore")),
          y = CBfile()@progress,
          by = c("x", "y", "z")
        )

      })

    } else {

      cb_df({ dplyr::mutate(cb_df(), CBscore = 0) })

    }

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Workflow ------------------------------------------------------------

  mo_score_workflow <-
    moduleWorkflowMacroAreaServer(
      id = "workflow",
      macro_area = shiny::reactive({ input$workflow_region }),
      voxel_df_input = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  shiny::observeEvent(mo_score_workflow(), {

    cb_df({

      update_CBscore(cb_df = cb_df(), update_df = mo_score_workflow())

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


  # Reminder ----------------------------------------------------------------

  shiny::observeEvent(cb_df(), {

    if(!reminder_disabled()){

      saving_counter({ saving_counter() + 1 })

      if(saving_counter() == saving_reminder()){

        shiny::showModal(
          session = session,
          ui = shiny::modalDialog(
            title = "Better safe than sorry!",
            shiny::helpText("If you want to save your progress, click on the button below!"),
            footer = shiny::tagList(
              shiny::column(
                width = 12,
                align = "center", # applies to all column-children
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::numericInput(
                      inputId = "saving_reminder",
                      label = NULL,
                      value = saving_reminder(),
                      min = 1,
                      max = Inf,
                      step = 1,
                      width = "80%"
                    ),
                    shiny::helpText("Remind counter.")
                  ),
                  shiny::column(
                    width = 3,
                    shinyWidgets::awesomeCheckboxGroup(
                      inputId = "reminder_disabled",
                      label = NULL,
                      choices = "Don't remind again",
                      selected = character(),
                      inline = TRUE,
                      status = "primary",
                      width = "80%"
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::downloadButton(
                      outputId = "save_progress_button_reminder",
                      label = "Save Progress",
                      width = "80%"
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::actionButton(
                      inputId = "dont_save",
                      label = "No, thanks.",
                      width = "80%"
                    )
                  )
                )
              )
            )
          )
        )

      }

    }

  })

  shiny::observeEvent(input$dont_save, {

    # apply update
    saving_counter({ 0 })
    saving_reminder({ input$saving_reminder })
    reminder_disabled({ "Don't remind" %in% input$reminder_disabled })

    shiny::removeModal()

    })

  # Tab Progress ----------------------------------------------------------------

  # Dynamic UI
  output$save_finalized_results <- shiny::renderUI({

    style <- "display: block; width: 100%; margin-top: 5%;"
    if(progress() < 95){

      style <- paste0(style, "opacity: 0.4")
      text <- "A progress of 95% or higher is required for finalization."

    } else {

      style <- paste0(style, "opacity: 1")
      text <- "Click to finish!"

    }

    shiny::tagList(
      shiny::div(
        style = style,
        shiny::downloadButton(
          outputId = "save_finalized_results_button",
          label = "Finalize",
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
      ),
      shiny::helpText(text)
    )

  })

  # Module Output
  module_mri <-
    moduleMriServer(
      id = "progress",
      mode_init = "inspection",
      voxel_df = shiny::reactive({ cb_df() }),
      nifti_input = shiny::reactive({ nifti_input() })
    )

  # Reactive (Expressions)
  progress <- shiny::reactive({ comp_progress(cb_df()) })

  # Observe Events
  shiny::observeEvent(progress(), {

    shinyWidgets::updateProgressBar(
      session = session,
      id = "progress_bar",
      value = progress()
    )

  }, ignoreInit = TRUE)

  # Save progress handler
  output$save_progress_button <- shiny::downloadHandler(
    filename = function() {

      name_abbr <-
        userNameAbbr(CBfile()) %>%
        stringr::str_c(., "_")

      paste0("CB_progress_", name_abbr, format(Sys.Date(), "%m%d"), ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_file@progress <- dplyr::select(cb_df(), x, y, z, CBscore)

      cb_file@last_update <- Sys.time()

      saveRDS(cb_file, file)

    }
  )

  output$save_progress_button_reminder <- shiny::downloadHandler(
    filename = function() {

      # apply update
      saving_counter({ 0 })
      saving_reminder({ input$saving_reminder })
      reminder_disabled({ "Don't remind" %in% input$reminder_disabled })

      shiny::removeModal()

      # then save
      name_abbr <-
        userNameAbbr(CBfile()) %>%
        stringr::str_c(., "_")

      paste0("CB_progress_", name_abbr, format(Sys.Date(), "%m%d"), ".rds")

    },
    content = function(file){

      cb_file <- CBfile()

      cb_file@progress <- dplyr::select(cb_df(), x, y, z, CBscore)

      cb_file@last_update <- Sys.time()

      saveRDS(cb_file, file)

    }
  )

  finalized_clicked <- shiny::reactiveVal(value = 0)

  # Final output handler
  output$save_finalized_results_button <- shiny::downloadHandler(
    filename = function() {

      finalized_clicked({ finalized_clicked() + 1 })

      name <-
        stringr::str_replace_all(
          string = userName(CBfile()),
          pattern = " ",
          replacement = "_"
          ) %>%
        stringr::str_c("_", .)

      paste0("CB_finalized", name, ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_df({ assign_remaining_scores_by_NN(cb_df()) })

      cb_file@progress <- dplyr::select(cb_df(), x, y, z, CBscore)

      saveRDS(cb_file, file)

    }
  )

}









