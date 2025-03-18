

moduleMriControlUI <- function(id){

  ns <- shiny::NS(id)

  style_box <-
    glue::glue("background-color: white;
                border-radius: 10px;
                box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
                margin-top: 20px;
                padding-left: 10px;
                padding-right: 10px;
                display: flex;
                flex-direction: column;
                width: 90%;
                height: 100px;")

  shiny::tagList(
    shiny::fluidRow(
      shiny::div(
        style = style_box,
        shiny::uiOutput(outputId = ns("mri_buttons"))
      )
    )
  )



  }

#' @param mri_sag_out,mri_axi_out,mri_cor_out The output values of the plane
#' specific MRI modules: A named list as a reactive expression.
#' See [`moduleMriPlane`] for specifics.

moduleMriControlServer <- function(id,
                                   mri_sag_out,
                                   mri_axi_out,
                                   mri_cor_out,
                                   voxel_df_input,
                                   mode_init = "selection",
                                   external_selection = function(){ NULL },
                                   external_selection_opts = list()
                                   ){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      shiny::observeEvent(input$test, {

        print("------")
        print(reactiveValuesToList(outlines))
        print(outlines_control())
        print(outlines_valid())

      }, ignoreInit = TRUE)

      shiny::observe({

        #print(voxel_df_input())

      })

      # 1. Global ---------------------------------------------------------------

      # Dynamic UI --------------------------------------------------------------

      style_ab <- "height: 37px; font-size: 14px; padding: 6px; text-align: center;"

      output$interaction_dims <- shiny::renderUI({

        shiny::tagList(
          shiny::h5(shiny::strong("Dimensions:")) %>% add_helper("selection_dimensions"),
          shinyWidgets::radioGroupButtons(
            inputId = ns("interaction_dims"),
            label = NULL,
            selected = "3D",
            choiceNames = list(
              tags$span(style = "font-size: 16px;", "2D"),
              tags$span(style = "font-size: 16px;", "3D")
            ),
            choiceValues = c("2D", "3D"),
            direction = "horizontal",
            justified = TRUE,
            individual = FALSE,
            size = "normal",
            width = "100%"
          )
        )

      })

      output$mri_buttons <- shiny::renderUI({

        if(mode() == "selection"){

          # choices
          choices <-
            c(
              # Region-Click
              '<i class="fas fa-hand-pointer" style="font-size: 1.5em;" data-toggle="tooltip" title="Region-Click"></i>' = "region_click",
              # Outline
              '<i class="fas fa-circle-notch" style="font-size: 1.5em;" data-toggle="tooltip" title="Outline"></i>' = "outline",
              # Paintbrush
              '<i class="fas fa-paint-brush" style="font-size: 1.5em;" data-toggle="tooltip" title="Paintbrush"></i>' = "paintbrush",
              # Paintbrush + Eraser
              '<span class="fa-stack" style="font-size: 0.8em; width: 1.5em;" data-toggle="tooltip" title="Eraser-Paintbrush">
                    <i class="fas fa-paint-brush fa-stack-1x" style="position: absolute; top: -2px; left: -3.5px; font-size: 1.5em;"></i>
                    <i class="fas fa-eraser fa-stack-1x" style="position: absolute; bottom: -7.5px; left: 6.25px; font-size: 1.4em; color: black;"></i>
                    </span>' = "paintbrush_erase",
              # Safety margin
              '<i class="fas fa-ruler" style="font-size: 1.5em;" data-toggle="tooltip" title="Margin"></i>' = "margin"
            )

          # selected
          if(shiny::isTruthy(shiny::isolate({ input$selection_tool }))){

            selected <- shiny::isolate({ input$selection_tool })

          } else {

            selected <- "region_click"

          }

          html_out <-
            shiny::tagList(
              shiny::column(
                width = 1,
                align = "left",
                shiny::div(style = "height: 35px;"),
                shiny::actionButton(
                  inputId = ns("viewer3D"),
                  label = "3D View",
                  icon = shiny::icon("brain"),
                  width = "100%",
                  disabled = FALSE,
                  style = style_ab
                )
              ),
              shiny::column(
                width = 2,
                align = "left",
                shiny::h5(shiny::strong("Selection Controls:")),
                shiny::splitLayout(
                  shiny::actionButton(
                    inputId = ns("selection_undo"),
                    label = "Undo",
                    icon = shiny::icon("undo"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  ),
                  shiny::actionButton(
                    inputId = ns("selection_redo"),
                    label = "Redo",
                    icon = shiny::icon("redo"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  ),
                  shiny::actionButton(
                    inputId = ns("selection_reset"),
                    label = "Trash",
                    icon = shiny::icon("trash"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  ),
                  cellWidths = "33%"
                )
              ),
              shiny::column(
                width = 4,
                align = "left",
                shiny::h5(shiny::strong("Selection Tool:")) %>% add_helper("selection_tool"),
                shinyWidgets::radioGroupButtons(
                  inputId = ns("selection_tool"),
                  label = NULL,
                  selected = selected,
                  choices = choices,
                  direction = "horizontal",
                  justified = TRUE,
                  individual = FALSE,
                  size = "normal",
                  width = "100%"
                )
              ),
              shiny::uiOutput(outputId = ns("selection_tool_opts")),
              shiny::actionButton(ns("test"), "Test")
            )

        }

      })

      output$outline_opts <- shiny::renderUI({

        shiny::req(input$selection_tool == "outline")

        shiny::tagList(
          shiny::column(
            width = 3,
            align = "left",
            shiny::h5(shiny::strong("Outline:")) %>% add_helper("outline_options"),
            shiny::splitLayout(
              cellWidths = c("33%", "33%", "33%"),
              shiny::actionButton(
                inputId = ns("outline_select"),
                label = "Select",
                disabled = TRUE,
                width = "100%"
              ),
              shiny::actionButton(
                inputId = ns("outline_deselect"),
                label = "Deselect",
                disabled = TRUE,
                width = "100%"
              ),
              shiny::actionButton(
                inputId = ns("outline_reset_all"),
                label = "Trash",
                disabled = FALSE,
                width = "100%",
                icon = shiny::icon("trash")
              )
            )
          )
        )

      })

      output$paintbrush_opts <- shiny::renderUI({

        shiny::req(stringr::str_detect(input$selection_tool, pattern = "paintbrush"))

        shiny::tagList(
          shiny::column(
            width = 1,
            align = "left",
            shiny::h5(shiny::strong("Brush-Size:")),
            shiny::splitLayout(
              shiny::actionButton(
                inputId = ns("pbr_increase"),
                label = shiny::HTML('<span class="fa-stack" style="font-size: 0.8em; width: 1.5em;">
                          <i class="fas fa-paint-brush fa-stack-1x" style="position: absolute; top: -3px; left: -3.5px; font-size: 1.5em;"></i>
                          <i class="fas fa-plus fa-stack-1x" style="position: absolute; bottom: -6px; left: 6.5px; font-size: 0.9em; color: black;"></i>
                        </span>'),
                width = "100%",
                style = "height: 35px; font-size: 14px; padding: 6px; text-align: center;"
              ),

              shiny::actionButton(
                inputId = ns("pbr_decrease"),
                label = shiny::HTML('<span class="fa-stack" style="font-size: 0.8em; width: 1.5em;">
                          <i class="fas fa-paint-brush fa-stack-1x" style="position: absolute; top: -3px; left: -3.5px; font-size: 1.5em;"></i>
                          <i class="fas fa-minus fa-stack-1x" style="position: absolute; bottom: -6px; left: 6.5px; font-size: 0.9em; color: black;"></i>
                        </span>'),
                width = "100%",
                style = "height: 35px; font-size: 14px; padding: 6px; text-align: center;"
              ),
              cellWidths = "50%"
            )
          ),
          shiny::column(
            width = 1,
            align = "left",
            shiny::uiOutput(outputId = ns("interaction_dims")),
          )
        )

      })

      output$region_click_opts <- shiny::renderUI({

        shiny::tagList()

      })

      output$margin_opts <- shiny::renderUI({

        if(length(shiny::isolate({ current_selection() })) != 0){

          shiny::tagList(
            shiny::column(
              width = 3,
              align = "left",
              shiny::h5(
                shiny::strong("Margin [mm]:"),
                style = "margin-bottom: 0px;"
              ),
              shiny::sliderInput(
                inputId = ns("margin_dist"),
                label = NULL,
                value = 0,
                min = 0,
                max = 15,
                step = 0.01,
                width = "100%"
              )
            ),
            shiny::column(
              width = 2,
              align = "left",
              shiny::div(style = "height: 35px;"),
              shiny::splitLayout(
                shiny::actionButton(
                  inputId = ns("margin_confirm"),
                  label = "Confirm",
                  icon = shiny::icon("check"),
                  width = "100%",
                  disabled = TRUE,
                  style = style_ab
                ),
                shiny::uiOutput(outputId = ns("margin_reset"))
              )
            )
          )

        } else {

          shiny::column(
            width = 3,
            shiny::h5(shiny::strong("Margin [mm]:")),
            shiny::helpText("No tissue selected.")
          )

        }

      })

      output$margin_reset <- shiny::renderUI({

        shiny::actionButton(
          inputId = ns("margin_reset"),
          label = "Trash",
          icon = shiny::icon("trash"),
          width = "100%",
          disabled = !contains_selected_margin(),
          style = style_ab
        )

      })

      output$selection_tool_opts <- shiny::renderUI({

        shiny::req(input$selection_tool)

        if(input$selection_tool == "outline"){

          shiny::uiOutput(outputId = ns("outline_opts"))

        } else if(stringr::str_detect(input$selection_tool, "paintbrush")){

          shiny::uiOutput(outputId = ns("paintbrush_opts"))

        } else if(stringr::str_detect(input$selection_tool, "region_click")) {

          shiny::uiOutput(outputId = ns("region_click_opts"))

        } else if(input$selection_tool == "margin"){

          shiny::uiOutput(outputId = ns("margin_opts"))

        }

      })


      # Reactive Values ---------------------------------------------------------

      paintbrush_radius_fct <- shiny::reactiveVal(value = 0.025)

      stacks <- shiny::reactiveValues(selection = list(), selection_undone = list())

      voxel_df <- shiny::reactiveVal(value = data.frame())



      # Reactive Expressions ----------------------------------------------------

      interaction_dims <- shiny::reactive({ input$interaction_dims })


      # Observe Events ----------------------------------------------------------

      # -- increase/decrease paintbrush radius
      shiny::observeEvent(input$pbr_increase, {

        paintbrush_radius_fct({ paintbrush_radius_fct()*1.1 })

      })

      shiny::observeEvent(input$pbr_decrease, {

        paintbrush_radius_fct({ paintbrush_radius_fct()*0.9 })

      })

      shiny::observeEvent(voxel_df_input(), {

        if(!identical(x = shiny::isolate({voxel_df()}), y = voxel_df_input())){

          voxel_df({ voxel_df_input() })

        }

      }, ignoreInit = FALSE)

      # 2. Purpose: Manage MRI Modes --------------------------------------------

      mode <- shiny::reactiveVal(mode_init)

      shiny::observeEvent(input$mode,{

        if(!identical(x = shiny::isolate({mode()}), y = input$mode)){

          mode(input$mode)

        }

      })

      shiny::observeEvent(mri_sag_out()$mode, {

        if(!identical(x = shiny::isolate({mode()}), y = mri_sag_out()$mode)){

          mode(mri_sag_out()$mode)

        }

      })

      shiny::observeEvent(mri_axi_out()$mode, {

        if(!identical(x = shiny::isolate({mode()}), y = mri_axi_out()$mode)){

          mode(mri_axi_out()$mode)

        }

      })

      shiny::observeEvent(mri_cor_out()$mode, {

        if(!identical(x = shiny::isolate({mode()}), y = mri_cor_out()$mode)){

          mode(mri_cor_out()$mode)

        }

      })

      # 3. Purpose: Manage Slice State and Orientation -------------------------

      slice_state <- shiny::reactiveValues(sag = 128, axi = 128, cor = 128)

      # observe orientation changes in the sagittal MRI
      shiny::observe({

        if(mri_sag_out()$slice_pos$sag != shiny::isolate(slice_state$sag)){

          slice_state$sag <- mri_sag_out()$slice_pos$sag

        }

        if(mri_sag_out()$slice_pos$axi != shiny::isolate(slice_state$axi)){

          slice_state$axi <- mri_sag_out()$slice_pos$axi

        }

        if(mri_sag_out()$slice_pos$cor != shiny::isolate(slice_state$cor)){

          slice_state$cor <- mri_sag_out()$slice_pos$cor

        }

      })

      # observe orientation changes in the axial MRI
      shiny::observe({

        if(mri_axi_out()$slice_pos$sag != shiny::isolate(slice_state$sag)){

          slice_state$sag <- mri_axi_out()$slice_pos$sag

        }

        if(mri_axi_out()$slice_pos$axi != shiny::isolate(slice_state$axi)){

          slice_state$axi <- mri_axi_out()$slice_pos$axi

        }

        if(mri_axi_out()$slice_pos$cor != shiny::isolate(slice_state$cor)){

          slice_state$cor <- mri_axi_out()$slice_pos$cor

        }

      })

      # observe orientation changes in the coronal MRI
      shiny::observe({

        if(mri_cor_out()$slice_pos$sag != shiny::isolate(slice_state$sag)){

          slice_state$sag <- mri_cor_out()$slice_pos$sag

        }

        if(mri_cor_out()$slice_pos$axi != shiny::isolate(slice_state$axi)){

          slice_state$axi <- mri_cor_out()$slice_pos$axi

        }

        if(mri_cor_out()$slice_pos$cor != shiny::isolate(slice_state$cor)){

          slice_state$cor <- mri_cor_out()$slice_pos$cor

        }

      })

      # observe updates in voxel selection and shift focus
      shiny::observeEvent(updated_voxels(), {

        voxel_subset <- dplyr::filter(voxel_df(), id %in% updated_voxels())

        slice_state$sag <- mean(voxel_subset$x)
        slice_state$axi <- mean(voxel_subset$y)
        slice_state$cor <- mean(voxel_subset$z)

      }, ignoreInit = TRUE, ignoreNULL = TRUE)


      # 4. Purpose: Manage Selection Mode ---------------------------------------

      outlines <- shiny::reactiveValues(
        sag = data.frame(),
        axi = data.frame(),
        cor = data.frame()
      )

      outlines_control <- shiny::reactive({ reactiveValuesToList(outlines) })

      outlines_valid <- shiny::reactive({ purrr::keep(outlines_control(), .p = ~ nrow(.x) > 3) })

      # Reactive Values ---------------------------------------------------------

      updated_voxels <- shiny::reactiveVal(character())

      # Reactive (Expressions) --------------------------------------------------

      contains_selected_margin <- shiny::reactive({

        any(voxel_df()$is_margin & voxel_df()$selected)

      })

      # centralized vector of selected voxels
      # - character(0) when initialized
      # - character(n) after observing selection changes in either MRI instance
      current_selection <- shiny::reactive({ sort(voxel_df()$id[voxel_df()$selected]) })

      previous_selection <- shiny::reactive({

        n <- length(stacks$selection)

        if(n > 1){

          out <- stacks$selection[[n-1]]

        } else {

          out <- character()

        }

        return(out)

      })


      # ----- outlines
      shiny::observeEvent(mri_sag_out()$drawing_outline_confirmed, {

        print("trigger sag")
        if(!identical(x = shiny::isolate({ outlines$sag }), y = mri_sag_out()$drawing_outline_confirmed)){

          outlines$sag <- mri_sag_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(mri_axi_out()$drawing_outline_confirmed, {
print("trigger axi")
        if(!identical(x = shiny::isolate({ outlines$axi }), y = mri_axi_out()$drawing_outline_confirmed)){

          outlines$axi <- mri_axi_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(mri_cor_out()$drawing_outline_confirmed, {
print("trigger cor")
        if(!identical(x = shiny::isolate({ outlines$cor }), y = mri_cor_out()$drawing_outline_confirmed)){

          outlines$cor <- mri_cor_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      # ----- selection tools
      selection_erase <- shiny::reactive({

        shiny::req(input$selection_tool)
        stringr::str_detect(input$selection_tool, pattern = "_erase$")

      })

      selection_tool <- shiny::reactive({

        if(shiny::isTruthy(input$selection_tool)){

          stringr::str_remove(input$selection_tool, pattern = "_erase$")

        } else {

          "none"

        }

      })

      # Observe Events ----------------------------------------------------------

      shiny::observeEvent(external_selection(), {

        new_voxel_df <-
          dplyr::mutate(
            .data = voxel_df(),
            selected = id %in% external_selection()[["id"]]
          )

        # 1. update voxel_df
        voxel_df({ new_voxel_df })

        # 2. switch modes
        if(nrow(external_selection()) == 0){

          mode("inspection")

        } else {

          mode("selection")

        }

        # 3. update slice state
        xmax <-
          dplyr::filter(.data = voxel_df(), selected) %>%
          dplyr::group_by(x) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(x)

        ymax <-
          dplyr::filter(.data = voxel_df(), selected) %>%
          dplyr::group_by(y) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(y)

        zmax <-
          dplyr::filter(.data = voxel_df(), selected) %>%
          dplyr::group_by(z) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(z)

        slice_state$sag <- xmax[1]
        slice_state$axi <- ymax[1]
        slice_state$cor <- zmax[1]

      }, ignoreInit = TRUE)

      # observe safety margin init
      shiny::observeEvent(input$selection_tool, {

        if(input$selection_tool == "margin"){

          if(length(current_selection()) == 0){

            shiny::showNotification(
              ui = "No brain tissue has been selected yet.",
              type = "error",
              duration = 5
            )

            shiny::req(FALSE)

          } else {

            shiny::showNotification(
              ui = "Identifying margin candidates.",
              type = "message",
              duration = 3
            )

            voxel_df({

              dplyr::mutate(
                .data = voxel_df(),
                is_margin = dplyr::if_else(is_margin & !selected, true = FALSE, false = is_margin)
              ) %>%
              prepare_margin_selection(dist_max = 15)

            })

          }

        }

      })

      # ----- selection changes by plane

      # observe selection changes in the sagittal MRI
      shiny::observeEvent(mri_sag_out()$voxel_df, {

        # changes in selection in MRI instance
        if(!identical(x = mri_sag_out()$voxel_df, y = shiny::isolate({ voxel_df() }))){

          voxel_df({ mri_sag_out()$voxel_df })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the axial MRI
      shiny::observeEvent(mri_axi_out()$voxel_df, {

        # changes in selection in MRI instance
        if(!identical(x = mri_axi_out()$voxel_df, y = shiny::isolate({ voxel_df() }))){

          voxel_df({ mri_axi_out()$voxel_df })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the coronal MRI
      shiny::observeEvent(mri_cor_out()$voxel_df, {

        # changes in selection in MRI instance
        if(!identical(x = mri_cor_out()$voxel_df, y = shiny::isolate({ voxel_df() }))){

          voxel_df({ mri_cor_out()$voxel_df })

        }

      }, ignoreInit = TRUE)

      # ----- outline management
      shiny::observeEvent(outlines_valid(), {

        if(length(outlines_valid()) != 0){

          shiny::updateActionButton(
            session = session,
            inputId = "outline_select",
            disabled = FALSE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_deselect",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "outline_select",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_deselect",
            disabled = TRUE
          )

        }

      }, ignoreInit = TRUE)

      # apply outlines
      shiny::observeEvent(input$outline_select, {

        selected_ids <-
          identify_obs_in_outlines(
            voxel_df = voxel_df(),
            outlines = outlines_valid()
            )

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = selected | id %in% {{selected_ids}}
          )

        })

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$outline_deselect, {

        selected_ids <-
          identify_obs_in_outlines(
            voxel_df = voxel_df(),
            outlines = outlines_valid()
          )

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = dplyr::if_else(id %in% {{selected_ids}}, true = FALSE, false = selected)
          )

        })

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$outline_reset_all, {

        outlines$sag <- data.frame()
        outlines$axi <- data.frame()
        outlines$cor <- data.frame()

      })

      # ----- stack management

      shiny::observeEvent(current_selection(), {

        if(length(current_selection()) != 0){

          if(!identical(x = current_selection(), y = dplyr::last(stacks$selection))){

            stacks <-
              add_to_stack(
                stacks = stacks,
                which = "selection",
                what = current_selection()
              )

          }

          shiny::updateActionButton(
            session = session,
            inputId = "selection_reset",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "selection_undo",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_redo",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_reset",
            disabled = TRUE
          )

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_undo, {

        # add to undone stack for "redo" option
        stacks <-
          add_to_stack(
            stacks = stacks,
            which = "selection_undone",
            what = dplyr::last(stacks$selection)
          )

        # update voxel data.frame
        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = id %in% previous_selection()
          )

        })

        # reduce stack and disable button if only one remains
        stacks <- reduce_stack(stacks, which = "selection")

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_redo, {

        # update voxel data.frame
        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = id %in% dplyr::last(stacks$selection_undone)
          )

        })

        # update stacks
        stacks <-
          add_to_stack(
            stacks = stacks,
            which = "selection",
            what = dplyr::last(stacks$selection_undone)
          )

        stacks <- reduce_stack(stacks, which = "selection_undone")

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_reset, {

        stacks$selection <- list()
        stacks$selection_undone <- list()

        voxel_df({ dplyr::mutate(voxel_df(), selected = FALSE) })

      })

      shiny::observeEvent(stacks$selection,{

        if(length(stacks$selection) > 1){

          shiny::updateActionButton(
            session = session,
            inputId = "selection_undo",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "selection_undo",
            disabled = TRUE
          )

        }

      })

      shiny::observeEvent(stacks$selection_undone,{

        if(length(stacks$selection_undone) != 0){

          shiny::updateActionButton(
            session = session,
            inputId = "selection_redo",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "selection_redo",
            disabled = TRUE
          )

        }

      })

      # --- safety margin
      shiny::observeEvent(input$margin_dist, {

        margin_ids <- voxel_df()[voxel_df()$dist <= input$margin_dist,][["id"]]

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            is_margin_cand = id %in% {{margin_ids}},
            color = dplyr::if_else(is_margin_cand & !selected, true = alpha("#FFD966", {{alpha_val}}), false = color)
          )

        })

        # update button state if unconfirmed margin exists
        if(any(voxel_df()$is_margin_cand)){

          shiny::updateActionButton(
            session = session,
            inputId = "margin_confirm",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "margin_confirm",
            disabled = TRUE
          )

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$margin_confirm, {

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = selected | is_margin_cand,
            is_margin = selected & is_margin_cand,
            color = dplyr::if_else(is_margin, true = alpha("#F4A460", alpha_val), false = color),
            is_margin_cand = FALSE
            )

        })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

        if(contains_selected_margin()){

          shiny::updateActionButton(
            session = session,
            inputId = "margin_reset",
            disabled = FALSE
          )

        }

      })

      shiny::observeEvent(input$margin_reset, {

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = dplyr::if_else(is_margin, true = FALSE, false = selected)
            )

        })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

      })

      # 3D viewer
      shiny::observeEvent(input$viewer3D, {

        shiny::showModal(
          ui = shiny::modalDialog(
            plotly::plotlyOutput(outputId = ns("plot3D")),
            easyClose = TRUE,
            size = "l"
          )
        )

      })

      output$plot3D <- plotly::renderPlotly({

        voxels3D <- dplyr::filter(voxel_df(), selected | is_margin)

        hemispheres <- unique(voxels3D$hemisphere)

        plot_input <-
          dplyr::filter(voxel_df(), hemisphere %in% hemispheres) %>%
          dplyr::mutate(
            selection = dplyr::case_when(
              selected & !is_margin ~ "Selected Region",
              selected & is_margin ~ "Margin Confirmed",
              is_margin_cand ~ "Margin Unconfirmed",
              TRUE ~ "Not Selected"
              )
            )

        clrp_adjust_df <- dplyr::distinct(plot_input, selection, color)

        clrp_adjust <-
          purrr::set_names(
            x = clrp_adjust_df[["color"]],
            nm = clrp_adjust_df[["selection"]]
            )

        clrp_adjust[names(clrp_adjust) == "Not Selected"] <- "lightgrey"

        groups_highlight <- names(clrp_adjust)[names(clrp_adjust) != "Not Selected"]

        plot_brain_3d(
          voxel_df = plot_input,
          color_by = "selection",
          group_highlight = groups_highlight,
          opacity_hide = 0.05,
          pt_size = 1,
          clrp_adjust = clrp_adjust,
          paper_bgcolor = "black",
          show_legend = FALSE
        )

      })

      # 5. Manage Refinement Mode -----------------------------------------------
      voxels_margin <- shiny::reactiveVal(data.frame()) #!!!!

      # Module Output ----------------------------------------------------------

      module_output <- shiny::reactive({

        list(
          hover_var = input$hover_var,
          interaction_dims = interaction_dims(),
          mode = mode(),
          outlines = outlines_control(),
          pbr_fct = paintbrush_radius_fct(),
          slice_state = slice_state,
          selected_voxels = current_selection(),
          selection_erase = selection_erase(),
          selection_tool = selection_tool(),
          voxel_df = voxel_df(),
          voxels_margin = voxels_margin()
        )

      })

      return(module_output)

    }
  )

}






