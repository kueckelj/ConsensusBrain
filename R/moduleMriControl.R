

moduleMriControlUI <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("mri_control_ui")),
    #shiny::actionButton(ns("test"), "Test"),
    NULL
    )

  }

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

      }, ignoreInit = TRUE)

      # 1. Global ---------------------------------------------------------------

      # paintbrush/paintbrush_erase share their value
      selection_tool <- shiny::reactiveVal(value = "region_click")
      selection_scope <- shiny::reactiveVal(value = list(region_click = "ann_dk_adj", paintbrush = "ann_macro"))
      selection_state <- shiny::reactiveVal(value = character())

      # Dynamic UI --------------------------------------------------------------

      style_ab <- "height: 37px; font-size: 14px; padding: 6px; text-align: center;"

      style_box <-
        c(
          "background-color: white;
           border-radius: 5px;
           box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
           margin-top: 10px;
           padding-left: 10px;
           padding-right: 10px;
           display: flex;
           flex-direction: column;
           width: 95%;
           height: 90px;"
        )

      output$highlight_groups <- shiny::renderUI({

        shiny::req(input$highlight_var)

        if(input$highlight_var == "ann_macro"){

          choices <- sort(unique(voxel_df()[["ann_macro"]]))

          names(choices) <- make_pretty_label(choices)

        } else if(input$highlight_var == "ann_dk_adj"){

          choices <- cortical_regions_dk

          names(choices) <- make_pretty_label(names(choices))

          choices <-
            purrr::map(
              .x = choices,
              .f = ~ purrr::set_names(x = .x, nm = make_pretty_label(.x))
            )

          choices$Subcortical <-
            dplyr::filter(voxel_df(), ann_macro == "subcortical") %>%
            dplyr::pull(var = "ann_dk_adj") %>%
            unique() %>%
            sort()

          names(choices$Subcortical) <- make_pretty_label(choices$Subcortical)

        } else if(input$highlight_var == "ann_dt_adj"){

          choices <- cortical_regions_dt

          names(choices) <- make_pretty_label(names(choices))

          choices <-
            purrr::map(
              .x = choices,
              .f = ~ purrr::set_names(x = .x, nm = make_pretty_label(.x))
            )

          choices$Subcortical <-
            dplyr::filter(voxel_df(), ann_macro == "subcortical") %>%
            dplyr::pull(var = "ann_dt_adj") %>%
            unique() %>%
            sort()

          names(choices$Subcortical) <- make_pretty_label(choices$Subcortical)

        } else if(input$highlight_var == "wm_tract"){

          choices <- sort(unique(voxel_df()[["wm_tract"]]))
          choices <- choices[choices != "none"]

          names(choices) <- make_pretty_label(choices)

        } else if(input$highlight_var == "CBscore"){

          choices <- score_set_up$choices

        }

        shiny::tagList(
          shiny::h5(shiny::strong("Region:")),
          shinyWidgets::pickerInput(
            inputId = ns("highlight_groups"),
            label = NULL,
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
        )

      })

      output$margin_opts <- shiny::renderUI({

        if(length(shiny::isolate({ selection_state() })) != 0){

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
          disabled = length(voxels_margin()) != 0,
          style = style_ab
        )

      })

      output$mri_buttons <- shiny::renderUI({

      if(mode() == "inspection"){

        html_out <-
          shiny::tagList(
            shiny::column(
              width = 2,
              align = "left",
              shiny::h5(shiny::strong("Highlight Control:")),
              shiny::splitLayout(
                cellWidths = "50%",
                shiny::actionButton(
                  inputId = ns("highlight_update"),
                  label = "Highlight",
                  icon = shiny::icon("refresh"),
                  width = "100%",
                  style = style_ab
                ),
                shiny::actionButton(
                  inputId = ns("highlight_reset"),
                  label = "Reset",
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
              shiny::h5(shiny::strong("Highlight Scope:")) %>% add_helper("highlight_scope"),
              shinyWidgets::pickerInput(
                inputId = ns("highlight_var"),
                choices = c(
                  "Macroanatomical" = "ann_macro",
                  "Desikan-Kiliany" = "ann_dk_adj",
                  "Destrieux" = "ann_dt_adj",
                  "White Matter Tracts" = "wm_tract",
                  "Score" = "CBscore"
                ),
                width = "100%",
                multiple = FALSE,
                options = shinyWidgets::pickerOptions(
                  dropupAuto = TRUE,
                  container = "body",
                  size = 15
                )
              )
            ),
            shiny::column(
              width = 2,
              align = "left",
              shiny::uiOutput(outputId = ns("highlight_groups"))
            ),
            shiny::column(
              width = 2,
              align = "left",
              shiny::h5(shiny::strong("Hemisphere:")),
              shinyWidgets::checkboxGroupButtons(
                inputId = ns("highlight_hemisphere"),
                label = NULL,
                choices = c("Left" = "left", "Right" = "right"),
                justified = TRUE,
                selected = c("left", "right"),
                checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
              )
            ),
            shiny::column(width = 2),
            shiny::column(
              width = 2,
              align = "left",
              shiny::h5(shiny::strong("Hover:")) %>% add_helper("hover_vars"),
              shinyWidgets::pickerInput(
                inputId = ns("hover_vars"),
                choices = c(
                  "Macroanatomical" = 1,
                  "Desikan-Kiliany" = 2,
                  "Destrieux" = 3,
                  "White Matter Tracts" = 4,
                  "Score" = 5
                ),
                selected = c(1, 2, 5),
                width = "100%",
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  dropupAuto = TRUE,
                  container = "body",
                  size = 15
                )
              )
            )
          )

      } else if(mode() == "selection"){

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

        if(mode() == "inspection"){

          shiny::fluidRow(
            #shiny::actionButton(ns("test"), "Test"),
            shiny::div(style = "height: 20px;"),
            shiny::div(
              style = style_box,
              shiny::uiOutput(outputId = ns("mri_buttons"))
            )
          )

        } else if(mode() == "selection"){

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

        if(input$paintbrush_mode == "Ray" & paintbrush_direction() != "both"){

          shiny::column(
            width = 1,
            align = "left",
            shiny::h5(shiny::strong("Depth [mm]:")) %>% add_helper("paintbrush_depth"),
            shiny::numericInput(
              inputId = ns("paintbrush_depth"),
              label = NULL,
              value = shiny::isolate({ paintbrush_depth() }),
              step = 1,
              min = 0,
              max = 256
            )
          )

        } else {

          shiny::tagList()

        }

      })

      output$paintbrush_direction <- shiny::renderUI({

        shiny::req(input$paintbrush_mode)

        if(input$paintbrush_mode == "Ray"){

          Ray_choices <- c(

            # Forward (arrow up with ceiling line)
            '<div style="display: flex; flex-direction: column; align-items: center; height: 100%;">
     <div style="width: 20px; height: 2px; background-color: black; margin-bottom: 2px;"></div>
     <i class="fas fa-arrow-up-long" style="font-size: 1.2em;" data-toggle="tooltip" title="Forward (in viewing direction)"></i>
   </div>' = "forward",

            # Backward (arrow down with floor line)
            '<div style="display: flex; flex-direction: column; align-items: center; height: 100%;">
     <i class="fas fa-arrow-down-long" style="font-size: 1.2em;" data-toggle="tooltip" title="Backward (opposite to viewing direction)"></i>
     <div style="width: 20px; height: 2px; background-color: black; margin-top: 2px;"></div>
   </div>' = "backward"
          )


          shiny::column(
            width = 1,
            align = "left",
            shiny::h5(shiny::strong("Direction:")) %>% add_helper("paintbrush_direction"),
            shinyWidgets::radioGroupButtons(
              inputId = ns("paintbrush_direction"),
              label = NULL,
              choices = Ray_choices,
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
            choices = c("Sphere", "Ray"),
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

        shiny::req(selection_scope())
        shiny::req(selection_tool())

        if(selection_tool() == "region_click"){

          choices <-
            c("Desikan-Kiliany" = "ann_dk_adj",
              "Destrieux" = "ann_dt_adj")

          selected <- selection_scope()[["region_click"]]

          helper_name <- "selection_scope_region_click"

          label <- "Region Scope:"

          shiny::column(
            width = 2,
            align = "left",
            shiny::h5(shiny::strong(label)) %>% add_helper(helper_name),
            shinyWidgets::pickerInput(
              inputId = ns("selection_scope"),
              label = NULL,
              choices = choices,
              selected = selected,
              options = shinyWidgets::pickerOptions(dropupAuto = TRUE)
            )
          )

        } else if(stringr::str_detect(selection_tool(), "paintbrush") & paintbrush_mode() == "Ray"){

          choices <-
            c("None" = "none",
              "Macroanatomical" = "ann_macro",
              "Desikan-Kiliany" = "ann_dk_adj",
              "Destrieux" = "ann_dt_adj")

          selected <- selection_scope()[["paintbrush"]]

          helper_name <- "selection_scope_paintbrush"

          label <- "Brush Scope:"

          shiny::column(
            width = 2,
            align = "left",
            shiny::h5(shiny::strong(label)) %>% add_helper(helper_name),
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

      paintbrush_depth <- shiny::reactiveVal(value = 25)
      paintbrush_direction <- shiny::reactiveVal(value = "forward")
      paintbrush_mode <- shiny::reactiveVal(value = "Sphere")
      paintbrush_radius <- shiny::reactiveVal(value = 7.5)

      shiny::observeEvent(input$paintbrush_depth,{ paintbrush_depth({ input$paintbrush_depth }) })
      shiny::observeEvent(input$paintbrush_direction,{ paintbrush_direction({ input$paintbrush_direction }) })
      shiny::observeEvent(input$paintbrush_mode,{ paintbrush_mode({ input$paintbrush_mode }) })
      shiny::observeEvent(input$paintbrush_radius,{ paintbrush_radius({ input$paintbrush_radius }) })

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

      voxel_df <- shiny::reactive({

        if(mode() == "inspection"){

          voxel_df_input()

        } else if(mode() == "selection"){

          dplyr::mutate(voxel_df_input(), selected = id %in% selection_state())

        }

      })

      # Reactive Expressions ----------------------------------------------------


      # Observe Events ----------------------------------------------------------

      # 2. Purpose: Manage MRI Modes --------------------------------------------

      mode <- shiny::reactiveVal(mode_init)

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

      # 4. Manage Inspection Mode -----------------------------------------------

      voxels_highlighted <- shiny::reactiveVal(value = character())

      shiny::observeEvent(input$highlight_update, {

        highlight <- voxel_df()[[input$highlight_var]] %in% input$highlight_groups
        hemispheres <- voxel_df()$hemisphere %in% input$highlight_hemisphere

        if(sum(highlight & hemispheres) == 0){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "No Tissue Remaining",
            text = "No tissue matches your combination of highlight criteria.",
            type = "warning",
            showCloseButton = TRUE
          )

          shiny::req(FALSE)

        }

        voxels_highlighted({ voxel_df()[["id"]][highlight & hemispheres] })

        shiny::updateActionButton(
          session = session,
          inputId = "highlight_reset",
          disabled = FALSE
        )

        xmax <-
          dplyr::filter(.data = voxel_df(), id %in% voxels_highlighted()) %>%
          dplyr::group_by(x) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(x)

        ymax <-
          dplyr::filter(.data = voxel_df(), id %in% voxels_highlighted()) %>%
          dplyr::group_by(y) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(y)

        zmax <-
          dplyr::filter(.data = voxel_df(), id %in% voxels_highlighted()) %>%
          dplyr::group_by(z) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
          dplyr::filter(count == max(count)) %>%
          dplyr::pull(z)

        slice_state$sag <- xmax[1]
        slice_state$axi <- ymax[1]
        slice_state$cor <- zmax[1]

      })

      shiny::observeEvent(input$highlight_reset, {

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "highlight_groups",
          selected = character(0)
        )

        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "highlight_hemisphere",
          selected = c("left", "right")
        )

        voxels_highlighted({ character(0) })

        shiny::updateActionButton(
          session = session,
          inputId = "highlight_reset",
          disabled = TRUE
        )

      })

      # 5. Purpose: Manage Selection Mode ---------------------------------------

      outlines <- shiny::reactiveValues(
        sag = data.frame(),
        axi = data.frame(),
        cor = data.frame()
      )

      outlines_control <- shiny::reactive({ reactiveValuesToList(outlines) })

      outlines_valid <- shiny::reactive({ purrr::keep(outlines_control(), .p = ~ nrow(.x) > 3) })

      paintbrush_masks <- shiny::reactive({

        purrr::keep(
          .x =
            list(
              sag = mri_sag_out()$paintbrushed_ids,
              axi = mri_axi_out()$paintbrushed_ids,
              cor = mri_cor_out()$paintbrushed_ids
            ),
          .p = ~ length(.x) != 0
        )

      })

      # Reactive Values ---------------------------------------------------------

      # Reactive (Expressions) --------------------------------------------------

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

      shiny::observeEvent(input$selection_tool, {

        # prerequisites for selection tools
        if(input$selection_tool == "margin" & length(shiny::isolate({ selection_state() })) == 0){

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

        if(input$selection_tool == "paintbrush_erase" & length(shiny::isolate({ selection_state() })) == 0){

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

          if(length(voxels_margin_cand()) != 0){

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

          if(length(shiny::isolate({ selection_state() })) == 0){

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

          }

        }

      })

      # Observe Events ----------------------------------------------------------

      shiny::observeEvent(external_selection(), {

        selection_state({ external_selection()[["id"]] })

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
      shiny::observeEvent(mri_sag_out()$plane_selection_state, {

        ids_selected_new <- sort(mri_sag_out()$plane_selection_state)
        ids_selected_old <- shiny::isolate({ sort(selection_state()) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

          selection_state({ ids_selected_new })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the axial MRI
      shiny::observeEvent(mri_axi_out()$plane_selection_state, {

        ids_selected_new <- sort(mri_axi_out()$plane_selection_state)
        ids_selected_old <- shiny::isolate({ sort(selection_state()) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

          selection_state({ ids_selected_new })

        }

      }, ignoreInit = TRUE)

      # observe selection changes in the coronal MRI
      shiny::observeEvent(mri_cor_out()$plane_selection_state, {

        ids_selected_new <- sort(mri_cor_out()$plane_selection_state)
        ids_selected_old <- shiny::isolate({ sort(selection_state()) })

        # changes in selection in MRI instance
        if(!identical(x = ids_selected_new, y = ids_selected_old)){

          selection_state({ ids_selected_new })

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

        # currently not active

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$outline_deselect, {

        # currently not active

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$outline_reset_all, {

        outlines$sag <- data.frame()
        outlines$axi <- data.frame()
        outlines$cor <- data.frame()

      })

      # ----- stack management

      shiny::observeEvent(selection_state(), {

        if(length(selection_state()) != 0){

          shiny::updateActionButton(
            session = session,
            inputId = "viewer3D",
            disabled = FALSE
          )

          if(!identical(x = selection_state(), y = dplyr::last(stacks$selection))){

            stacks <-
              add_to_stack(
                stacks = stacks,
                which = "selection",
                what = selection_state()
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

        selection_state({ previous_selection() })

        # update margin selection if required
        if(input$selection_tool == "margin"){

          shiny::updateSliderInput(
            session = session,
            inputId = "margin_dist",
            value = 0
          )

        }

        # reduce stack and disable button if only one remains
        stacks <- reduce_stack(stacks, which = "selection")

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$selection_debris_rm, {

        debris_ids <-
          identify_debris(voxel_df()) %>%
          dplyr::filter(debris == "0") %>%
          dplyr::pull(var = "id")

        if(length(debris_ids) != 0){

          selection_state({ selection_state()[!selection_state() %in% debris_ids] })

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
          closeOnClickOutside = TRUE,
          callbackR = function(confirmed) {
            if (isTRUE(confirmed)) {
              stacks$selection <- list()
              selection_state({ character(0) })
              voxels_margin({ character(0) })
              voxels_margin_cand({ character(0) })
            }
          }
        )
      })

      # only changes in case of a score assignment
      shiny::observeEvent(voxel_df_input(), {

        stacks$selection <- list()
        selection_state({ character(0) })
        voxels_margin({ character(0) })
        voxels_margin_cand({ character(0) })

      }, ignoreInit = TRUE)

      shiny::observeEvent(stacks$selection,{

        if(length(stacks$selection) != 0){

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
      voxels_dist <- shiny::reactive({

        prepare_margin_selection(
          voxel_df = voxel_df(),
          dist_max = 15,
          voxels_margin = shiny::isolate({ voxels_margin() })
        )

      })

      voxels_margin <- shiny::reactiveVal(value = character())
      voxels_margin_cand <- shiny::reactiveVal(value = character())

      shiny::observeEvent(voxels_margin(), {

        if(length(voxels_margin()) != 0){

          shiny::updateActionButton(
            session = session,
            inputId = "margin_reset",
            disabled = FALSE
          )

        } else {

          shiny::updateActionButton(
            session = session,
            inputId = "margin_reset",
            disabled = FALSE
          )

        }

      })

      shiny::observeEvent(voxels_margin_cand(), {

        if(length(voxels_margin_cand()) != 0){

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

      })

      shiny::observeEvent(input$margin_confirm, {

        selection_state({ c(selection_state(), voxels_margin_cand()) })
        voxels_margin({ unique(c(voxels_margin(), voxels_margin_cand())) })
        voxels_margin_cand({ character(0) })


        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$margin_confirm_modal, {

        selection_state({ c(selection_state(), voxels_margin_cand()) })
        voxels_margin({ unique(c(voxels_margin(), voxels_margin_cand())) })
        voxels_margin_cand({ character(0) })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

        # update selection tool
        shiny::removeModal()
        selection_tool(input$selection_tool)

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$margin_dist, {

        shiny::req(nrow(voxels_dist()) != 0)

        voxels_margin_cand({ voxels_dist()[voxels_dist()$dist <= input$margin_dist,][["id"]] })

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$margin_dont_confirm_modal, {

        voxels_margin_cand({ character(0) })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

        # update selection tool
        shiny::removeModal()
        selection_tool(input$selection_tool)

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$margin_reset, {

        selection_state({ selection_state()[!selection_state() %in% voxels_margin()] })
        voxels_margin({ character(0) })
        voxels_margin_cand({ character(0) })

        shiny::updateSliderInput(
          session = session,
          inputId = "margin_dist",
          value = 0
        )

      }, ignoreInit = TRUE)

      # 3D viewer
      shiny::observeEvent(input$viewer3D, {

        shiny::showNotification(
          ui = "Rendering 3D visualization.",
          type = "message",
          duration = 3
        )

        if(mode() == "inspection") {

          debris_button <- NULL

        } else if(mode() == "selection"){

          debris_ids <-
            identify_debris(voxel_df()) %>%
            dplyr::filter(debris == "0") %>%
            dplyr::pull(var = "id")

          # Conditionally add footer button if debris exists
          debris_button <- if(length(debris_ids) != 0) {

            shiny::actionButton(
              inputId = ns("selection_debris_rm"),
              label = "Debris",
              icon = icon("broom")
            )

          } else {

            NULL

          }

        }

        footer <- shiny::div(
          style = "display: flex; align-items: center; gap: 10px;",
          debris_button,
          shiny::actionButton(
            inputId = ns("close_modal3D"),
            label = "Close",
          )
        )

        shiny::showModal(
          shiny::modalDialog(
            shinycssloaders::withSpinner(
              ui_element = plotly::plotlyOutput(outputId = ns("plot3D")),
              id = ns("plot3D_spinner")
            ),
            easyClose = FALSE,
            size = "l",
            footer = footer
          )
        )
      })

      shiny::observeEvent(input$close_modal3D, {

        shinyjs::runjs(sprintf("Plotly.purge('%s');", ns("plot3D")))

        shiny::removeModal()

      })

      output$plot3D <- plotly::renderPlotly({

         if(mode() == "selection"){

          hemispheres <-
            dplyr::filter(voxel_df(), selected) %>%
            dplyr::pull(var = "hemisphere") %>%
            unique()

          dplyr::filter(voxel_df(), hemisphere %in% hemispheres) %>%
            dplyr::mutate(
              selection = dplyr::case_when(
                selected & !id %in% voxels_margin() ~ "Main Selection",
                selected & id %in% voxels_margin() ~ "Margin Confirmed",
                id %in% voxels_margin_cand() ~ "Margin Unconfirmed",
                TRUE ~ "Not Selected"
              )
            ) %>%
            trim_brain_3d(
              var = "selection",
              val_missing = "Not Selected",
              fct = 0.15
            ) %>%
            plot_brain_3d(
              voxel_df = .,
              color_by = "selection",
              group_highlight = c("Main Selection", "Margin Confirmed", "Margin Unconfirmed"),
              opacity_hide = 0.05,
              pt_size = 1.25,
              clrp_adjust = colorsCB3D,
              paper_bgcolor = "black",
              show_legend = FALSE
            )

        }

      })

      # Module Output ----------------------------------------------------------

      module_output <- shiny::reactive({

        if(mode() == "inspection"){

          output <-
            list(
              cursor_on_plane = cursor_on_plane(),
              hover_vars = as.numeric(input$hover_vars),
              slice_state = slice_state,
              voxels_highlighted = voxels_highlighted()
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
              paintbrush_masks = paintbrush_masks(),
              paintbrush_mode = paintbrush_mode(),
              paintbrush_radius = paintbrush_radius(),
              slice_state = slice_state,
              selection_erase = selection_erase(),
              selection_scope = sel_scope,
              selection_tool = selection_tool(),
              voxels_margin = voxels_margin(), # only margin
              voxels_margin_cand = voxels_margin_cand(), # unconfirmed margin
              voxels_selected = selection_state() # main + margin
            )

        }

        return(output)

      })

      return(module_output)

    }
  )

}






