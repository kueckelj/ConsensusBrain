

moduleMriControlUI <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(shiny::uiOutput(ns("mri_control_ui")))

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
                                   inspection_var = function(){ "CBscore_label" },
                                   external_selection = function(){ NULL },
                                   external_selection_opts = list()
                                   ){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      shiny::observeEvent(input$test, {

        print("------")
        assign("voxel_df", voxel_df(), envir = .GlobalEnv)

      }, ignoreInit = TRUE)

      shiny::observe({


      })

      # 1. Global ---------------------------------------------------------------

      # Dynamic UI --------------------------------------------------------------

      style_ab <- "height: 37px; font-size: 14px; padding: 6px; text-align: center;"

      style_box <-
        c(
          "background-color: white;
           border-radius: 5px;
           box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
           margin-top: 20px;
           padding-left: 10px;
           padding-right: 10px;
           display: flex;
           flex-direction: column;
           width: 95%;
           height: 100px;"
        )

      output$margin_opts <- shiny::renderUI({

        if(length(shiny::isolate({ current_selection() })) != 0){

          shiny::tagList(
            shiny::column(
              width = 2,
              align = "left",
              shiny::div(style = "height: 35px;"),
              shiny::splitLayout(
                cellWidths = c("66%", "33%"),
                shiny::actionButton(
                  inputId = ns("margin_confirm"),
                  label = "Confirm Margin",
                  icon = shiny::icon("check"),
                  width = "100%",
                  disabled = TRUE,
                  style = style_ab
                ),
                shiny::uiOutput(outputId = ns("margin_reset"))
              )
            ),
            shiny::column(
              width = 3,
              align = "left",
              shiny::h5(
                shiny::strong("Margin [mm]:"),
                style = "margin-bottom: 0px;"
              ) %>% add_helper("margin_dist"),
              shiny::sliderInput(
                inputId = ns("margin_dist"),
                label = NULL,
                value = 0,
                min = 0,
                max = 15,
                step = 0.01,
                width = "100%"
              )
            )
          )

        } else {

          shiny::column(
            width = 4,
            shiny::h5(shiny::strong("Margin [mm]:")),
            shiny::helpText("No tissue selected.")
          )

        }

      })

      output$margin_reset <- shiny::renderUI({

        shiny::actionButton(
          inputId = ns("margin_reset"),
          label = "Trash", #"Trash",
          icon = shiny::icon("trash"),
          width = "100%",
          disabled = !contains_selected_margin(),
          style = style_ab
        )

      })

      output$mri_buttons <- shiny::renderUI({

       if(mode() == "selection"){

          # choices
          choices <-
            c(
              # Region-Click
              '<i class="fas fa-hand-pointer" style="font-size: 1.5em;" data-toggle="tooltip" title="Region-Click"></i>' = "region_click",
              # Outline - does not work properly - improve later
              #'<i class="fas fa-circle-notch" style="font-size: 1.5em;" data-toggle="tooltip" title="Outline"></i>' = "outline",
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
                  disabled = TRUE,
                  style = style_ab
                )
              ),
              shiny::column(
                width = 2,
                align = "left",
                shiny::h5(shiny::strong("Selection Control:")) %>% add_helper("selection_control"),
                shiny::splitLayout(
                  cellWidths = "33%",
                  shiny::actionButton(
                    inputId = ns("selection_undo"),
                    label = "Undo",
                    icon = shiny::icon("undo"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  ),
                  shiny::actionButton(
                    inputId = ns("selection_debris_rm"),
                    label = "Debris",
                    icon = shiny::icon("broom"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  ),
                  shiny::actionButton(
                    inputId = ns("selection_reset"),
                    label = " All", # empty space!
                    icon = shiny::icon("trash"),
                    width = "100%",
                    disabled = TRUE,
                    style = style_ab
                  )
                )
              ),
              shiny::column(
                width = 2,
                align = "left",
                shiny::h5(shiny::strong("Selection Tool:")) %>% add_helper("selection_tool"),
                shinyWidgets::radioGroupButtons(
                  inputId = ns("selection_tool"),
                  label = NULL,
                  selected = "region_click",
                  choices = choices,
                  direction = "horizontal",
                  justified = TRUE,
                  individual = FALSE,
                  size = "normal",
                  width = "100%"
                )
              ),
              shiny::uiOutput(outputId = ns("selection_tool_opts"))
            )

        }

      })

      output$mri_control_ui <- shiny::renderUI({

        if(mode() == "selection"){

          shiny::fluidRow(
            #shiny::actionButton(ns("test"), "Test"),
            shiny::div(
              style = style_box,
              shiny::uiOutput(outputId = ns("mri_buttons"))
            )
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
                disabled = TRUE,
                width = "100%",
                icon = shiny::icon("trash")
              )
            )
          )
        )

      })

      output$paintbrush_opts <- shiny::renderUI({

        shiny::req(input$selection_tool == "paintbrush")

        shiny::tagList(
          shiny::uiOutput(outputId = ns("paintbrush_mode")),
          shiny::uiOutput(outputId = ns("paintbrush_radius")),
          shiny::uiOutput(outputId = ns("paintbrush_direction")),
          shiny::uiOutput(outputId = ns("paintbrush_depth")),
          shiny::uiOutput(outputId = ns("selection_scope"))
        )

      })

      output$paintbrush_erase_opts <- shiny::renderUI({

        shiny::req(input$selection_tool == "paintbrush_erase")

        shiny::tagList(
          shiny::uiOutput(outputId = ns("paintbrush_mode")),
          shiny::uiOutput(outputId = ns("paintbrush_radius")),
          shiny::uiOutput(outputId = ns("paintbrush_direction")),
          shiny::uiOutput(outputId = ns("paintbrush_depth")),
          shiny::uiOutput(outputId = ns("selection_scope"))
        )

      })

      output$paintbrush_depth <- shiny::renderUI({

        shiny::req(input$selection_tool)
        shiny::req(stringr::str_detect(input$selection_tool, "paintbrush"))
        shiny::req(input$paintbrush_mode)

        if(input$paintbrush_mode == "Beam" & paintbrush_direction() != "both"){

          shiny::column(
            width = 1,
            align = "left",
            shiny::h5(shiny::strong("Depth [mm]:")) %>% add_helper("paintbrush_depth"),
            shiny::numericInput(
              inputId = ns("paintbrush_depth"),
              label = NULL,
              value = shiny::isolate({ paintbrush_depth() }),
              step = 1,
              min = 1,
              max = 256
            )
          )

        } else {

          shiny::tagList()

        }

      })

      output$paintbrush_direction <- shiny::renderUI({

        shiny::req(input$paintbrush_mode)

        if(input$paintbrush_mode == "Beam"){

          beam_choices <- c(
            # Forward
            '<div style="display: flex; justify-content: center; align-items: center; height: 100%;">
     <i class="fas fa-arrow-up-long" style="font-size: 1.5em;" data-toggle="tooltip" title="Forward (in viewing direction)"></i>
   </div>' = "forward",

            # Backward
            '<div style="display: flex; justify-content: center; align-items: center; height: 100%;">
     <i class="fas fa-arrow-down-long" style="font-size: 1.5em;" data-toggle="tooltip" title="Backward (opposite to viewing direction)"></i>
   </div>' = "backward",

            # Both
            '<div style="display: flex; justify-content: center; align-items: center; height: 100%;">
     <i class="fas fa-arrows-up-down" style="font-size: 1.5em;" data-toggle="tooltip" title="Both directions"></i>
   </div>' = "both"
          )

          shiny::column(
            width = 1,
            align = "left",
            shiny::h5(shiny::strong("Direction:")) %>% add_helper("paintbrush_direction"),
            shinyWidgets::radioGroupButtons(
              inputId = ns("paintbrush_direction"),
              label = NULL,
              choices = beam_choices,
              selected = shiny::isolate({ paintbrush_direction() }),
              direction = "horizontal",
              justified = TRUE,
              individual = FALSE,
              size = "normal",
              width = "100%"
            )
          )

        } else {

          shiny::tagList()

        }

      })

      output$paintbrush_mode <- shiny::renderUI({

        shiny::req(stringr::str_detect(input$selection_tool, "paintbrush"))

        shiny::column(
          width = 2,
          align = "left",
          shiny::h5(shiny::strong("3D Brush Mode:")) %>% add_helper("paintbrush_mode"),
          shinyWidgets::radioGroupButtons(
            inputId = ns("paintbrush_mode"),
            label = NULL,
            choices = c("Sphere", "Beam"),
            selected = shiny::isolate({ paintbrush_mode() }),
            direction = "horizontal",
            justified = TRUE,
            individual = FALSE,
            size = "normal",
            width = "100%"
          )
        )

      })

      output$paintbrush_radius <- shiny::renderUI({

        shiny::req(input$selection_tool)
        shiny::req(stringr::str_detect(input$selection_tool, "paintbrush"))

        shiny::column(
          width = 1,
          align = "left",
          shiny::h5(shiny::strong("Radius [mm]:")),
          shiny::numericInput(
            inputId = ns("paintbrush_radius"),
            label = NULL,
            value = shiny::isolate({ paintbrush_radius() }),
            step = 0.5,
            min = 1,
            max = 256
          )
        )

      })

      output$region_click_opts <- shiny::renderUI({

        shiny::tagList(
          shiny::uiOutput(outputId = ns("selection_scope"))
        )

      })

      output$selection_scope <- shiny::renderUI({

        shiny::req(input$paintbrush_mode)
        shiny::req(selection_scope())
        shiny::req(selection_tool())

        if(stringr::str_detect(selection_tool(), "paintbrush") & input$paintbrush_mode == "Beam" |
           selection_tool() == "region_click"){

          if(stringr::str_detect(selection_tool(), "paintbrush")){

            choices <-
              c("None" = "none",
                "Macroanatomical" = "ann_macro",
                "Desikan-Kiliany" = "ann_dk_adj",
                "Destrieux" = "ann_dt_adj")

            selected <- selection_scope()[["paintbrush"]]

            helper_name <- "selection_scope_paintbrush"

          } else {

            choices <-
              c("Desikan-Kiliany" = "ann_dk_adj",
                "Destrieux" = "ann_dt_adj")

            selected <- selection_scope()[["region_click"]]

            helper_name <- "selection_scope_region_click"

          }

          shiny::column(
            width = 2,
            align = "left",
            shiny::h5(shiny::strong("Selection Scope:")) %>% add_helper(helper_name),
            shinyWidgets::pickerInput(
              inputId = ns("selection_scope"),
              label = NULL,
              choices = choices,
              selected = selected,
              options = shinyWidgets::pickerOptions(dropupAuto = TRUE)
            )
          )

        } else {

          shiny::tagList()

        }

      })

      output$selection_tool_opts <- shiny::renderUI({

        shiny::req(input$selection_tool)

        if(input$selection_tool == "outline"){

          shiny::uiOutput(outputId = ns("outline_opts"))

        } else if(input$selection_tool == "paintbrush"){

          shiny::uiOutput(outputId = ns("paintbrush_opts"))

        } else if(input$selection_tool == "paintbrush_erase"){

          shiny::uiOutput(outputId = ns("paintbrush_erase_opts"))

        } else if(input$selection_tool == "region_click") {

          shiny::uiOutput(outputId = ns("region_click_opts"))

        } else if(input$selection_tool == "margin"){

          shiny::uiOutput(outputId = ns("margin_opts"))

        }

      })


      # Reactive Values ---------------------------------------------------------

      paintbrush_depth <- shiny::reactiveVal(value = 10)
      paintbrush_direction <- shiny::reactiveVal(value = "forward")
      paintbrush_mode <- shiny::reactiveVal(value = "Sphere")
      paintbrush_radius <- shiny::reactiveVal(value = 5)

      shiny::observeEvent(input$paintbrush_depth,{ paintbrush_depth({ input$paintbrush_depth }) })
      shiny::observeEvent(input$paintbrush_direction,{ paintbrush_direction({ input$paintbrush_direction }) })
      shiny::observeEvent(input$paintbrush_mode,{ paintbrush_mode({ input$paintbrush_mode }) })
      shiny::observeEvent(input$paintbrush_radius,{ paintbrush_radius({ input$paintbrush_radius }) })

      # paintbrush/paintbrush_erase share their value
      selection_scope <- shiny::reactiveVal(value = list(region_click = "ann_dk_adj", paintbrush = "ann_macro"))

      shiny::observeEvent(input$selection_scope, {

        sel_scope <- selection_scope()

        if(input$selection_tool == "region_click"){

          sel_scope[["region_click"]] <- input$selection_scope

        } else {

          sel_scope[["paintbrush"]] <- input$selection_scope

        }

        selection_scope({ sel_scope })

      })

      stacks <- shiny::reactiveValues(selection = list())

      voxel_df <- shiny::reactiveVal(value = data.frame())

      # Reactive Expressions ----------------------------------------------------


      # Observe Events ----------------------------------------------------------

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

      # observe cursor on MRI

      cursor_on_plane <- shiny::reactiveVal(value = character(0))

      shiny::observe({

        lgl <-
          c(
            mri_sag_out()$cursor_on_mri,
            mri_axi_out()$cursor_on_mri,
            mri_cor_out()$cursor_on_mri
            )

        cop <- c("sag", "axi", "cor")[lgl]

        if(length(cop) == 0){ cop <- "none" }

        cursor_on_plane({ cop })

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

        if(!identical(x = shiny::isolate({ outlines$sag }), y = mri_sag_out()$drawing_outline_confirmed)){

          outlines$sag <- mri_sag_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(mri_axi_out()$drawing_outline_confirmed, {

        if(!identical(x = shiny::isolate({ outlines$axi }), y = mri_axi_out()$drawing_outline_confirmed)){

          outlines$axi <- mri_axi_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(mri_cor_out()$drawing_outline_confirmed, {

        if(!identical(x = shiny::isolate({ outlines$cor }), y = mri_cor_out()$drawing_outline_confirmed)){

          outlines$cor <- mri_cor_out()$drawing_outline_confirmed

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(outlines_control(), {

        if(sum(purrr::map_lgl(outlines_control(), .f = ~nrow(.x) != 0)) != 0){

          shiny::updateActionButton(
            session = session,
            inputId = "outline_reset_all",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "outline_reset_all",
            disabled = TRUE
          )

        }

      }, ignoreInit = TRUE)

      # ----- selection tools
      selection_erase <- shiny::reactive({

        shiny::req(input$selection_tool)
        stringr::str_detect(input$selection_tool, pattern = "_erase$")

      })

      selection_tool <- shiny::reactiveVal(value = "region_click")

      shiny::observeEvent(input$selection_tool, {

        # prerequisites for selection tools
        if(input$selection_tool == "margin" & length(shiny::isolate({ current_selection() })) == 0){

          shiny::showNotification(
            ui = "The margin tool can not be used without a selection.",
            type = "warning"
          )

          shinyWidgets::updateRadioGroupButtons(
            session = session,
            inputId = "selection_tool",
            selected = selection_tool()
          )

          shiny::req(FALSE)

        }

        if(input$selection_tool == "paintbrush_erase" & length(shiny::isolate({ current_selection() })) == 0){

          shiny::showNotification(
            ui = "The paintbrush eraser tool can not be used without a selection.",
            type = "warning"
          )

          shinyWidgets::updateRadioGroupButtons(
            session = session,
            inputId = "selection_tool",
            selected = selection_tool()
          )

          shiny::req(FALSE)

        }

        # ensure certain things before switching
        if(selection_tool() == "margin"){

          if(any(shiny::isolate({ voxel_df()$is_margin_cand }))){

            shiny::showModal(
              shiny::modalDialog(
                title = "Unconfirmed Margin",
                shiny::helpText("You have not confirmed your latest margin updates."),
                footer =
                  shiny::fluidRow(
                    shiny::column(
                      width = 4,
                      align = "center",
                      offset = 1,
                        shiny::actionButton(
                          inputId = ns("margin_confirm_modal"),
                          label = "Confirm & Proceed",
                          width = "75%"
                        )
                    ),
                    shiny::column(
                      width = 4,
                      align = "center",
                      offset = 1,
                      shiny::actionButton(
                        inputId = ns("margin_dont_confirm_modal"),
                        label = "Don't confirm",
                        width = "75%"
                      )
                    )
                  )
              ),
            )

            shiny::req(FALSE)

          }

        }

        # update
        selection_tool(input$selection_tool)

        # ensure certain things after switching
        if(selection_tool() == "margin"){

          if(length(shiny::isolate({ current_selection() })) == 0){

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
                .data = shiny::isolate({ voxel_df() }),
                is_margin = dplyr::if_else(is_margin & !selected, true = FALSE, false = is_margin)
              ) %>%
                prepare_margin_selection(dist_max = 15)

            })

          }

        }

      })




      # Observe Events ----------------------------------------------------------

      shiny::observeEvent(external_selection(), {

        new_voxel_df <-
          dplyr::mutate(
            .data = voxel_df(),
            color = dplyr::if_else(id %in% external_selection()[["id"]], true = alpha("forestgreen", alpha_val), false = color),
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

      # ----- selection changes by plane

      # observe selection changes in the sagittal MRI
      shiny::observeEvent(mri_sag_out()$voxel_df, {

        ids_selected_new <- sort(mri_sag_out()$voxel_df$id[mri_sag_out()$voxel_df$selected])
        ids_selected_old <- shiny::isolate({ sort(voxel_df()$id[voxel_df()$selected]) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

         # adds to selection?
         new_only <- setdiff(x = ids_selected_new, y = ids_selected_old)

         voxel_df({

           dplyr::mutate(
             .data = voxel_df(),
             # new_only derives from within the MRI plane and is a main selection
             color = dplyr::if_else(id %in% {{new_only}}, true = alpha("forestgreen", alpha_val), false = color),
             is_margin = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin),
             is_margin_cand = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin_cand),
             selected = id %in% {{ids_selected_new}}
           )

         })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the axial MRI
      shiny::observeEvent(mri_axi_out()$voxel_df, {

        ids_selected_new <- sort(mri_axi_out()$voxel_df$id[mri_axi_out()$voxel_df$selected])
        ids_selected_old <- shiny::isolate({ sort(voxel_df()$id[voxel_df()$selected]) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

          # adds to selection?
          new_only <- setdiff(x = ids_selected_new, y = ids_selected_old)

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              # new_only derives from within the MRI plane and is a main selection
              color = dplyr::if_else(id %in% {{new_only}}, true = alpha("forestgreen", alpha_val), false = color),
              is_margin = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin),
              is_margin_cand = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin_cand),
              selected = id %in% {{ids_selected_new}}
            )

          })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the coronal MRI
      shiny::observeEvent(mri_cor_out()$voxel_df, {

        ids_selected_new <- sort(mri_cor_out()$voxel_df$id[mri_cor_out()$voxel_df$selected])
        ids_selected_old <- shiny::isolate({ sort(voxel_df()$id[voxel_df()$selected]) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

          # adds to selection?
          new_only <- setdiff(x = ids_selected_new, y = ids_selected_old)

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              # new_only derives from within the MRI plane and is a main selection
              color = dplyr::if_else(id %in% {{new_only}}, true = alpha("forestgreen", alpha_val), false = color),
              is_margin = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin),
              is_margin_cand = dplyr::if_else(id %in% {{new_only}}, true = FALSE, false = is_margin_cand),
              selected = id %in% {{ids_selected_new}}
            )

          })

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

          shiny::updateActionButton(
            session = session,
            inputId = "viewer3D",
            disabled = FALSE
          )

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
            inputId = "selection_debris_rm",
            disabled = FALSE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_reset",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "viewer3D",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_undo",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_debris_rm",
            disabled = FALSE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "selection_reset",
            disabled = TRUE
          )

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_undo, {

        # update margin selection if required
        if(input$selection_tool == "margin"){

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              #is_margin = dplyr::if_else(is_margin & !selected, true = FALSE, false = is_margin),
              selected = id %in% previous_selection()
            ) %>%
              prepare_margin_selection(dist_max = 15)

          })

          shiny::updateSliderInput(
            session = session,
            inputId = "margin_dist",
            value = 0
          )

        } else {

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              selected = id %in% previous_selection()
            )

          })

        }

        # reduce stack and disable button if only one remains
        stacks <- reduce_stack(stacks, which = "selection")

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_debris_rm, {

        vdf_with_debris <- identify_debris(voxel_df())

        debris_ids <-
          dplyr::filter(vdf_with_debris, debris == "0") %>%
          dplyr::pull(id)

        if(length(debris_ids) != 0){

          voxel_df({

            dplyr::mutate(
              .data = vdf_with_debris,
              selected = dplyr::if_else(id %in% {{debris_ids}}, true = FALSE, false = selected),
              debris = NULL
            )

          })

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Debris found and removed!",
            type = "success"
          )

        } else {

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No debris remaining!",
            type = "info"
          )

        }

      })

      shiny::observeEvent(input$selection_debris_rm_modal, {

        vdf_with_debris <- identify_debris(voxel_df())

        debris_ids <-
          dplyr::filter(vdf_with_debris, debris == "0") %>%
          dplyr::pull(id)

        voxel_df({

          dplyr::mutate(
            .data = vdf_with_debris,
            selected = dplyr::if_else(id %in% {{debris_ids}}, true = FALSE, false = selected),
            debris = NULL
          )

        })

      })

      shiny::observeEvent(input$selection_reset, {

        shinyalert::shinyalert(
          title = "Are you sure?",
          text = "Do you really want to trash this selection? This action cannot be undone.",
          type = "warning",
          size = "s",
          showCancelButton = TRUE,
          confirmButtonText = "Trash",
          cancelButtonText = "Cancel",
          closeOnEsc = FALSE,
          closeOnClickOutside = FALSE,
          callbackR = function(confirmed) {
            if (isTRUE(confirmed)) {
              stacks$selection <- list()
              voxel_df({ dplyr::mutate(voxel_df(), selected = FALSE) })
            }
          }
        )
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

      # ----- margin management
      shiny::observeEvent(input$margin_dist, {

        shiny::req("dist" %in% names(voxel_df()))

        margin_ids <- voxel_df()[voxel_df()$dist <= input$margin_dist,][["id"]]

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            is_margin_cand = id %in% {{margin_ids}},
            color = dplyr::if_else(is_margin_cand & !selected, true = alpha("steelblue", {{alpha_val}}), false = color)
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
            color = dplyr::if_else(is_margin, true = alpha("cadetblue", alpha_val*1.25), false = color),
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

      shiny::observeEvent(input$margin_confirm_modal, {

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            selected = selected | is_margin_cand,
            is_margin = selected & is_margin_cand,
            color = dplyr::if_else(is_margin, true = alpha("cadetblue", alpha_val*1.25), false = color),
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

        # update selection tool
        shiny::removeModal()
        selection_tool(input$selection_tool)

      })

      shiny::observeEvent(input$margin_dont_confirm_modal, {

        voxel_df({

          dplyr::mutate(.data = voxel_df(), is_margin_cand = FALSE)

        })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

        # update selection tool
        shiny::removeModal()
        selection_tool(input$selection_tool)

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
      observeEvent(input$viewer3D, {

        shiny::showNotification(
          ui = "Rendering 3D visualization.",
          type = "message",
          duration = 3
        )

        vdf_with_debris <- identify_debris(voxel_df())

        debris_ids <-
          dplyr::filter(vdf_with_debris, debris == "0") %>%
          dplyr::pull(id)

        # Conditionally add footer button if debris exists
        debris_button <- if(length(debris_ids) != 0) {

          shiny::actionButton(
            inputId = ns("selection_debris_rm_modal"),
            label = "Debris",
            icon = icon("broom")
          )

        } else {

          NULL

        }

        footer <- shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          debris_button,
          shiny::actionButton(
            inputId = ns("close_modal"),
            label = "Close",
          )
        )

        shiny::showModal(
          shiny::modalDialog(
            shinycssloaders::withSpinner(
              ui_element = plotly::plotlyOutput(outputId = ns("plot3D")),
              id = ns("plot3D_spinner")
            ),
            easyClose = TRUE,
            size = "l",
            footer = footer
          )
        )
      })

      shiny::observeEvent(input$close_modal, {

        shiny::removeModal()

      })

      output$plot3D <- plotly::renderPlotly({

        if(mode() == "inspection"){




        } else if(mode() == "selection"){

          voxels3D <- dplyr::filter(voxel_df(), selected | is_margin)

          hemispheres <- unique(voxels3D$hemisphere)

          plot_input <-
            dplyr::filter(voxel_df(), hemisphere %in% hemispheres) %>%
            dplyr::mutate(
              selection = dplyr::case_when(
                selected & !is_margin ~ "Main Selection",
                selected & is_margin ~ "Margin Confirmed",
                is_margin_cand ~ "Margin Unconfirmed",
                TRUE ~ "Not Selected"
              )
            )

          plot_input <-
            trim_brain_3d(
              plot_input = plot_input,
              var = "selection",
              val_missing = "Not Selected",
              fct = 0.5
            )

          clrp_adjust_df <-
            dplyr::distinct(plot_input, selection, color) %>%
            dplyr::mutate(color = ggplot2::alpha(color, 1))

          clrp_adjust <-
            purrr::set_names(
              x = clrp_adjust_df[["color"]],
              nm = clrp_adjust_df[["selection"]]
            )

          clrp_adjust[names(clrp_adjust) == "Not Selected"] <- "lightgrey"

          groups_highlight <- names(clrp_adjust)[names(clrp_adjust) != "Not Selected"]

        }

        plot_brain_3d(
          voxel_df = plot_input,
          color_by = "selection",
          group_highlight = groups_highlight,
          opacity_hide = 0.05,
          pt_size = 1.25,
          clrp_adjust = clrp_adjust,
          paper_bgcolor = "black",
          show_legend = FALSE
        )

      })

      # Module Output ----------------------------------------------------------

      module_output <- shiny::reactive({

        if(mode() == "inspection"){

          output <-
            list(
              cursor_on_plane = cursor_on_plane(),
              slice_state = slice_state,
              voxel_df = voxel_df()
            )

        } else if(mode() == "selection"){

          if(selection_tool() == "region_click"){

            sel_scope <- selection_scope()[["region_click"]]

          } else {

            sel_scope <- selection_scope()[["paintbrush"]]

          }

          output <-
            list(
              cursor_on_plane = cursor_on_plane(),
              outlines = outlines_control(),
              outlines_reset = input$outline_reset_all,
              paintbrush_direction = paintbrush_direction(),
              paintbrush_depth = paintbrush_depth(),
              paintbrush_mode = paintbrush_mode(),
              paintbrush_radius = paintbrush_radius(),
              slice_state = slice_state,
              selected_voxels = current_selection(),
              selection_erase = selection_erase(),
              selection_scope = sel_scope,
              selection_tool = selection_tool(),
              voxel_df = voxel_df()
            )

        }

        return(output)

      })

      return(module_output)

    }
  )

}






