
ConsensusBrainServer <- function(input, output, session, nifti_object){


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
  shiny::observeEvent(input$test, {

    print(input$sidebar_menu)
    print(workflow_macro_area())

  })



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

  iv$add_rule("userInp_first_name", shinyvalidate::sv_required())
  iv$add_rule("userInp_last_name", shinyvalidate::sv_required())
  iv$add_rule("userInp_affiliation", shinyvalidate::sv_required())
  iv$add_rule("userInp_country", shinyvalidate::sv_required())
  iv$add_rule("userInp_email", shinyvalidate::sv_required())
  iv$add_rule("userInp_email_confirm", shinyvalidate::sv_required())
  iv$add_rule("userInp_years_of_experience", shinyvalidate::sv_required())
  iv$add_rule("userInp_annual_case_load", shinyvalidate::sv_required())

  iv$add_rule("userInp_first_name", ~ if(!valid_name_length(.)) "Two or more letters required.")
  iv$add_rule("userInp_last_name", ~ if(!valid_name_length(.)) "Two or more letters required.")

  iv$add_rule("userInp_first_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")
  iv$add_rule("userInp_last_name", ~ if(!valid_name_symbols(.)) "Only letters are allowed.")

  iv$add_rule("userInp_email", shinyvalidate::sv_email())
  iv$add_rule("userInp_email_confirm", function(value) {
    if(value != input$userInp_email){
      "The email addresses do not match."
    }
  })

  iv$enable()

  shiny::observeEvent(input$new_user, {

    shiny::removeModal()

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::tags$h2(shiny::strong("New User"), style = "padding-top: 0;"),
        shiny::column(
          width = 12,
          align = "left",
          shiny::br(),
          shiny::splitLayout(
            cellWidths = "50%",
            shiny::textInput(
              inputId = "userInp_first_name",
              label = shiny::tagList(shiny::icon("person"), "First Name:"),
              value = ifelse(local_launch(session), "Jan", ""),
              width = "100%"
            ),
            shiny::textInput(
              inputId = "userInp_last_name",
              label = "Last Name:",
              value = ifelse(local_launch(session), "Kueckelhaus", ""),
              width = "100%"
            )
          ),
          shiny::splitLayout(
            cellWidths = "50%",
            shiny::textInput(
              inputId = "userInp_email",
              value = ifelse(local_launch(session), "jankueckelhaus@gmx.de", ""),
              label = shiny::tagList(shiny::icon("envelope"), "E-Mail:")
            ),
            shiny::textInput(
              inputId = "userInp_email_confirm",
              value = ifelse(local_launch(session), "jankueckelhaus@gmx.de", ""),
              label = shiny::tagList(shiny::icon("envelope"), "E-Mail (Confirm):")
            )
          ),
          shiny::textInput(
            inputId = "userInp_affiliation",
            label = shiny::tagList(shiny::icon("institution"), "Affiliation:"),
            width = "100%",
            value = ifelse(local_launch(session), "Department of Neurosurgery, University Clinic Erlangen", ""),
            placeholder = "As denoted in publications."
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = "userInp_country",
                label = shiny::tagList(shiny::icon("globe"), "Country:"),
                choices = c("", countrycode::codelist$country.name.en),
                selected = ifelse(local_launch(session), "Germany", ""),
                width = "100%"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_years_of_experience",
                label = shiny::tagList(shiny::icon("briefcase"), "Years of Experience:"),
                value = ifelse(local_launch(session), 20, NA_integer_),
                min = 0,
                max = 100,
                step = 1
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_annual_case_load",
                label = shiny::tagList(shiny::icon("notes-medical"), "Annual Case Load:"),
                value = ifelse(local_launch(session), 250, NA_integer_),
                min = 0,
                max = 1000,
                step = 50
              )
            )
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = "back_to_welcome",
                label = shiny::tagList(shiny::icon("arrow-left"), "Back"),
                width = "100%"
              )
            ),
            shiny::column(width = 2),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = "login",
                label = shiny::tagList(shiny::icon("sign-in-alt"), "Login"),
                width = "100%"
              )
            ),
            shiny::column(width = 4)
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::uiOutput(outputId = "helptext_login")
              ),
          )
        ),
        footer = shiny::tagList()
      )
    )

  })

  valid_user_info <- shiny::reactiveVal(value = FALSE)

  user_meta <- shiny::reactive({

    list(
      affiliation = input$userInp_affiliation,
      annual_case_load = input$userInp_annual_case_load,
      country = input$userInp_country,
      email = input$userInp_email,
      first_name = stringr::str_to_title(input$userInp_first_name),
      last_name = stringr::str_to_title(input$userInp_last_name),
      years_of_experience = input$userInp_years_of_experience
    )

  })

  # Dynamic UI
  output$helptext_login <- shiny::renderUI({

    if(iv$is_valid()){

      shiny::helpText("Click on Login to proceed.")

    } else {

     shiny::helpText("Please enter the required information.")

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

  # Save progress handler
  output$save_progress_button <- shiny::downloadHandler(
    filename = function() {

      name_abbr <-
        strsplit(userName(CBfile()), split = " ")[[1]]%>%
        purrr::map_chr(.f = ~ toupper(strsplit(.x, split = "")[[1]][1])) %>%
        stringr::str_c(collapse = "")

      paste0("CB_progress_", name_abbr, "_", format(Sys.Date(), "%d%m"), ".rds")

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
        strsplit(userName(CBfile()), split = " ")[[1]]%>%
        purrr::map_chr(.f = ~ toupper(strsplit(.x, split = "")[[1]][1])) %>%
        stringr::str_c(collapse = "")

      paste0("CB_progress_", name_abbr, "_", format(Sys.Date(), "%d%m"), ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_file@progress <- dplyr::select(cb_df(), x, y, z, CBscore)

      cb_file@last_update <- Sys.time()

      saveRDS(cb_file, file)

    }
  )

  # Final output handler
  output$save_finalized_results_button <- shiny::downloadHandler(
    filename = function() {

      name <-
        stringr::str_replace_all(
          string = userName(CBfile()),
          pattern = " ",
          replacement = "_"
          )

      paste0("CB_finalized_", name, ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_df({ assign_remaining_scores_by_NN(cb_df()) })

      cb_file@progress <-
        dplyr::select(cb_df(), x, y, z, CBscore)

      saveRDS(cb_file, file)

    }
  )


}









