
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


  # Tab Progress ----------------------------------------------------------------

  # Dynamic UI
  output$save_finalized_results <- shiny::renderUI({

    if(progress() < 95){

      style <- "opacity: 0.4"
      text <- "A progress of 95% or higher is required for finalization."

    } else {

      style <- "opacity: 1"
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

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::tags$h2(shiny::strong("Welcome to ConsensusBrain!"), style = "padding-top: 0;"),

        shiny::helpText(
          "If this is your first time here, please enter your name on the left and click on Login.
          If you've visited before and want to continue where you left off, click 'Browse...' and select the file you previously
          downloaded that contains your saved progress - a file that ends with .rds!"
        ),

        shiny::div(
          id = "login_panel",
          style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
          shiny::fluidRow(
            shiny::column(
              width = 6,
              align = "left",
              style = "border-right: 1px solid lightgrey; height: 100%;",
              shiny::textInput(
                inputId = "first_name",
                label = shiny::tagList(shiny::icon("person"), "Username:"),
                width = "95%",
                value = "",
                placeholder = "First Name"
              ),
              shiny::textInput(
                inputId = "last_name",
                label = NULL,
                width = "95%",
                value = "",
                placeholder = "Last Name"
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
        ),

        footer = shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 8, align = "center",
            shiny::actionButton(inputId = "login_username", label = "Login", width = "60%", disabled = TRUE)
          ),
          shiny::column(width = 2)
        ),
        easyClose = FALSE,
        size = "xl"
      )
    )

  # ignoreInit = FALSE !!! -> is displayed when opening the app
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Dynamic UI
  output$helptext_username <- shiny::renderUI({

    if(valid_username()){

      shiny::helpText("Click on Login to proceed.")

    } else {

      if(!valid_username_length() | !valid_username_parts()){

        shiny::helpText("Please enter first and last name. (Two letters minimum each.)")

      } else {

        shiny::helpText("Invalid username. Only letters and empty space is allowed.")

      }

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

  input_username <- shiny::reactive({

    paste0(
      stringr::str_to_title(input$first_name),
      " ",
      stringr::str_to_title(input$last_name)
    )

  })

  valid_username <- shiny::reactive({

     valid_username_length() &
     valid_username_parts() &
     valid_username_symbols()

  })

  valid_username_length <- shiny::reactive({

    length(strsplit(input_username(), split = " ")[[1]]) >= 2

  })

  valid_username_parts <- shiny::reactive({

    all(
      purrr::map_lgl(
        .x = strsplit(input_username(), split = " ")[[1]],
        .f = ~ stringr::str_length(.x) > 2
      )
    )

  })

  valid_username_symbols <- shiny::reactive({

    all(
      purrr::map_lgl(
        .x = strsplit(input_username(), split = "")[[1]],
        .f = ~ stringr::str_detect(.x, pattern = stringr::str_c(unique(c(letters, LETTERS, " ")), collapse = "|"))
      )
    )

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

  shiny::observeEvent(valid_username(), {

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
    CBfile({ ProgressCB(user_name = input_username()) })

  })

  shiny::observeEvent(CBfile(), {

    shiny::removeModal(session = session)

    if(nrow(CBfile()@scores_assigned) != 0){

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
          y = CBfile()@scores_assigned,
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
        strsplit(CBfile()@user_name, split = " ")[[1]]%>%
        purrr::map_chr(.f = ~ toupper(strsplit(.x, split = "")[[1]][1])) %>%
        stringr::str_c(collapse = "")

      paste0("CB_progress_", name_abbr, "_", format(Sys.Date(), "%d%m"), ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_file@scores_assigned <- dplyr::select(cb_df(), x, y, z, CBscore)

      saveRDS(cb_file, file)

    }
  )

  # Final output handler
  output$save_finalized_results_button <- shiny::downloadHandler(
    filename = function() {

      name <-
        stringr::str_replace_all(
          string = CBfile()@user_name,
          pattern = " ",
          replacement = "_"
          )

      paste0("CB_finalized_", name, ".rds")

    },
    content = function(file) {

      cb_file <- CBfile()

      cb_df({ assign_remaining_scores_by_NN(cb_df()) })

      cb_file@scores_assigned <-
        dplyr::select(cb_df(), x, y, z, CBscore)

      saveRDS(cb_file, file)

    }
  )


}









