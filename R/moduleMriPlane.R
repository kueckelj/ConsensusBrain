

moduleMriPlaneUI <- function(id,
                             plane,
                             title = "MRI",
                             ...) {

  ns <- shiny::NS(id)

  rmin <- min(brain_dims[[plane]])
  rmax <- max(brain_dims[[plane]])

  slider_min <- min(brain_dims_slider[[plane]])
  slider_max <- max(brain_dims_slider[[plane]])

  shiny::tagList(
    shiny::tags$head(
      tags$script(shiny::HTML(glue::glue("
  document.addEventListener('DOMContentLoaded', function() {
    const plot = document.getElementById('{{ns('mriInteractionPlot')}}');
    const cursor = document.getElementById('{{ns('brushCursor')}}');

    if (plot && cursor) {
      // Static initial style for brush circle
      cursor.style.border = '2px solid green';
      cursor.style.borderRadius = '50%';
      cursor.style.pointerEvents = 'none';
      cursor.style.position = 'absolute';
      cursor.style.zIndex = '9999';
      cursor.style.transform = 'translate(-50%, -50%)';
      cursor.style.display = 'none';

      // Mouse tracking
      plot.addEventListener('mousemove', function(e) {
        const rect = plot.getBoundingClientRect();
        cursor.style.left = (e.clientX - rect.left) + 'px';
        cursor.style.top = (e.clientY - rect.top) + 'px';
      });

      // Show/hide cursor
      plot.addEventListener('mouseenter', function() {
        cursor.style.display = 'block';
      });

      plot.addEventListener('mouseleave', function() {
        cursor.style.display = 'none';
        Shiny.setInputValue('{{ns('mouse_state_plot')}}', 'up', {priority: 'event'});
      });

      plot.addEventListener('mousedown', function() {
        Shiny.setInputValue('{{ns('mouse_state_plot')}}', 'down', {priority: 'event'});
      });

      plot.addEventListener('mouseup', function() {
        Shiny.setInputValue('{{ns('mouse_state_plot')}}', 'up', {priority: 'event'});
      });

      // Shiny messages
      Shiny.addCustomMessageHandler('{{ns('updateBrushSize')}}', function(sizePx) {
        cursor.style.width = sizePx + 'px';
        cursor.style.height = sizePx + 'px';
      });

      Shiny.addCustomMessageHandler('{{ns('updateBrushColor')}}', function(color) {
        cursor.style.borderColor = color;
      });

      Shiny.addCustomMessageHandler('{{ns('updateBrushLinetype')}}', function(linetype) {
        cursor.style.borderStyle = linetype;
      });

      Shiny.addCustomMessageHandler('{{ns('changeCursor')}}', function(cursorType) {
        if (plot) {
          plot.style.cursor = cursorType;
        }
      });

      // Scrolling
      let scrollTimer = null;

      plot.addEventListener('wheel', function(e) {
        e.preventDefault();

        if (scrollTimer) return; // ignore if waiting

        const direction = e.deltaY > 0 ? -1 : 1;
        Shiny.setInputValue('{{ns('scroll_delta')}}', direction, {priority: 'event'});

        scrollTimer = setTimeout(function() {
          scrollTimer = null;
        }, 75);  // adjust delay to taste
      });

      }
      });

      // Zooming
      Shiny.addCustomMessageHandler('{{ns('set_zoom')}}', function(data) {
        const zoom = data.zoom;
        const allImgs = document.querySelectorAll('#{{ns('mriSlicePlot')}} .zoomable-image');

        allImgs.forEach(function(img) {
          img.style.transform = 'scale(' + zoom + ')';
          img.style.transformOrigin = 'center center';
        });

      });

      // Ensure that initial slice is shown directly
      Shiny.addCustomMessageHandler('{{ns('show_slice')}}', function(slice_idx) {
        for (let i = {{rmin}}; i <= {{rmax}}; i++) {
          const el = document.getElementById('slice_' + i + '_{{id}}' + '-'); // with + '-'! due to how ns() works
          if (el) {
            el.style.display = (i === slice_idx) ? 'block' : 'none';
          }
        }
      });

      setTimeout(function() {
        Shiny.setInputValue('{{ns('show_slice_direct')}}', 128, {priority: 'event'});
      }, 100);

", .open = "{{", .close = "}}"))),


      shiny::tags$style(
        shiny::HTML(
          "
               #{{ns('brushCursor')}} {
                position: absolute;
                border: 2px solid red;
                border-radius: 50%;
                pointer-events: none;
                z-index: 9999;
                display: none;
                transform: translate(-50%, -50%);
              }

             .mri-btn {
              align-items: center;
              cursor: pointer;
              justify-content: center;
              border: 1px solid #ccc;
              background-color: rgba(200, 200, 200, 0.2);
              transition: all 0.2s ease-in-out;
             }

             .btn-mri-btn {
              border: 1px solid #ccc;
              display: flex;
              justify-content: center;
              align-items: center;
              margin-top: 0;
              marign-bottom: 0;
              }

            .mri-btn-icon {
             font-size: 1.5em;
             }

            .irs-min, .irs-max { display: none !important; }

            .mri-container {
             background-color: white;
             border-radius: 5px;
             box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
             display: flex;
             flex-direction: column;
             padding: 1.5%;
             width: 100%;
             margin: 0 !important;
            }

           .mri-container .btn > .fa {
            font-size: 4em;
            vertical-align: middle;
            }

           .multiple-plots-wrapper {
            display: flex;
            flex-direction: row;
            width: 100%;
            aspect-ratio: 1 / 1;
            }

           .multiple-plots {
            position: relative;
            overflow: hidden;
            width: 100%;
            aspect-ratio: 1 / 1;
            margin: 0 !important;
            padding: 0 !important;
           }

           .multiple-plots > * {
            position: absolute;
            width: 100%;
            height: 100%;
           }

           .slider-container {
            display: flex;
            flex-direction: column-reverse;
            align-items: center;
            width: 100%;
            }

           .slider-labels {
            display: flex;
            justify-content: space-between;
            width: 100%;
            font-weight: bold;
            color: grey;
            margin-bottom: -15px;
            }
          "
        )
      )
    ),

    shiny::div(
      class = "mri-container",
      # Top Control
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center; gap: 5%;",
        # Left Control group
        shiny::div(
          style = "display: flex; gap: 2.5%;",
          shiny::actionButton(
            inputId = ns("zoom_in"),
            label = NULL,
            icon = shiny::icon("search-plus", class = "mri-btn-icon"),
            class = "mri-btn"
            ),
          shiny::actionButton(
            inputId = ns("zoom_out"),
            label = NULL,
            icon = shiny::icon("search-minus", class = "mri-btn-icon"),
            class = "mri-btn"
          )
        ),
        # Right Control Group
        shiny::div(
          style = "display: flex; gap: 2.5%; align-items: center; flex: 1;",
          shiny::actionButton(
            inputId = ns("mri_backward"),
            label = NULL,
            icon = shiny::icon("step-backward", class = "mri-btn-icon"),
            class = "mri-btn"
            ),
          shiny::div(
            class = "slider-container",
            shiny::sliderInput(
              inputId = ns("mri_slider_slice"),
              label = NULL,
              min = slider_min,
              max = slider_max,
              value = round(slider_max/2),
              ticks = FALSE,
              width = "100%"
            ),
            shiny::div(
              class = "slider-labels",
              shiny::span(mri_slider_labels[[plane]][1]),
              shiny::span(mri_slider_labels[[plane]][2])
            )
          ),
          shiny::actionButton(
            inputId = ns("mri_forward"),
            label = NULL,
            icon = shiny::icon("step-forward", class = "mri-btn-icon"),
            class = "mri-btn"
            ),
          shinyWidgets::dropdownButton(
            circle = TRUE,
            icon = shiny::icon("gear"),
            right = TRUE,
            size = "sm",
            status = "mri-btn",

            shiny::br(),

            shinyWidgets::materialSwitch(
              inputId = ns("show_localizer"),
              label = "Show Localizer",
              value = TRUE,
              status = "success"
            ),

            shiny::uiOutput(ns("show_score_ui")),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] === true", ns("show_score")),
              shiny::div(
                shiny::sliderInput(
                  inputId = ns("transp_score"),
                  label = "Transparency Score",
                  value = 0.75,
                  min = 0,
                  max = 1,
                  step = 0.01,
                  width = "100%"
                )
              )
            )

          )
         )
        ),
      # MRI Plot
      shiny::div(
        class = "multiple-plots-wrapper",
        shiny::div(
          class = "multiple-plots",
          style = "background-color: white;",
          #shiny::plotOutput(ns("mriSlicePlot"), height = "100%", width = "100%"),
          shiny::uiOutput(ns("mriSlicePlot")),
          shiny::plotOutput(ns("mriInspectionPlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriScorePlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriSelectionStatePlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriVisualAidPlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriLocalizerPlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriHoverInfoPlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriPaintbrushPlot"), height = "100%", width = "100%"),
          shiny::plotOutput(ns("mriOutlinePlot"), height = "100%", width = "100%"),
          shiny::plotOutput(
            outputId = ns("mriInteractionPlot"),
            height = "100%",
            click = shiny::clickOpts(id = ns("mriPlot_click"), clip = TRUE),
            #brush = shiny::brushOpts(id = ns("mriPlot_brush"), delayType = "debounce", resetOnNew = TRUE),
            dblclick = shiny::dblclickOpts(id = ns("mriPlot_dblclick"), clip = TRUE),
            hover = shiny::hoverOpts(id = ns("mriPlot_hover"), delay = 100, delayType = "throttle", clip = TRUE)
          ),
          shiny::div(id = ns("brushCursor"))
        )
      ),
      # Bottom Options
      shiny::div(
        style = "margin-top: 1.5%; margin-bottom: 1%",
        #shiny::actionButton(ns("test"), "Test"),
        shiny::uiOutput(ns("options_bottom"))
      )

    )
  )
}

moduleMriPlaneServer <- function(id,
                                 plane,
                                 nifti_input,
                                 voxel_df_input,
                                 mri_control,
                                 color_selected = function(){ colorsCB$selected },
                                 mode_init = "inspection",
                                 mode_opts = list(),
                                 reset_quick_select = NULL
                                 ){

  # define once
  plane_ccs <- switch_axis_label(plane)

  ra_ccs <- req_axes_2d(plane)
  ra_mri <- req_axes_2d(plane, mri = TRUE)

  col_axis <- ra_mri["col"]
  row_axis <- ra_mri["row"]

  mri_grid <- tidyr::expand_grid(col = 1:256, row = 1:256)

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      # ----- Debugging/Testing
      shiny::observeEvent(input$test,{

        print("-----------------------")
        print("Test")#
        print(voxels_quick_select())

      }, ignoreInit = TRUE)

      # --- scrolling (in dev)
      shiny::observeEvent(input$scroll_delta, {

        if(pb_mask_on_this_plane()){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Unconfirmed Paintbrush Drawing!",
            text = glue::glue(
              "There is an unconfirmed paintbrush drawing on this MRI.
              Please confirm or reset it before scrolling."
            ),
            type = "info"
          )

          shiny::req(FALSE)

        }

        new_val <- min(max(slice_idx() + input$scroll_delta, min_slices()), max_slices())
        slice_idx(new_val)

      })


      # 1. Global ---------------------------------------------------------------

      cursor_on_mri <- shiny::reactiveVal(value = FALSE)


      # Dynamic UI --------------------------------------------------------------

      output$options_bottom <- shiny::renderUI({

        if(mode() == "inspection"){

          if(isTRUE(mode_opts$quick_select)){

            if(cursor_on_brain_tissue()){

              shiny::column(
                offset = 1,
                width = 10,
                align = "center",
                shiny::helpText("Double-click to change the score of this region.")
              )

            } else {

              what  <- ifelse(isTRUE(mri_control_input$highlight_hover), "highlight and select", "select")
              shiny::column(
                offset = 1,
                width = 10,
                align = "center",
                shiny::helpText(glue::glue("Hover over brain regions to {what}."))
              )

            }

          }

        } else if(mode() == "selection"){

          if(selection_tool() == "region_click"){

            shiny::column(
              offset = 1,
              width = 10,
              align = "center",
              shiny::helpText("Hover over a brain area and double click to select.")
            )

          } else if(selection_tool() == "outline"){

            shiny::column(
              offset = 1,
              width = 10,
              shiny::splitLayout(
                cellWidths = c("25%", "25%", "25%", "5%", "20%"),
                shiny::actionButton(
                  inputId = ns("outline_confirm"),
                  label = "Confirm",
                  icon = shiny::icon(name = "check"),
                  width = "100%",
                  disabled = btns_disabled_outline()
                ),
                shiny::actionButton(
                  inputId = ns("outline_backward"),
                  label = "Backwards",
                  icon = shiny::icon(name = "step-backward"),
                  width = "100%",
                  disabled = btns_disabled_outline()
                ),
                shiny::actionButton(
                  inputId = ns("outline_reset"),
                  label = "Reset",
                  icon = shiny::icon(name = "trash"),
                  width = "100%",
                  disabled = btns_disabled_outline()
                ),
                shiny::div(),
                shiny::actionButton(
                  inputId = ns("outline_focus"),
                  label = "Focus",
                  icon = shiny::icon(name = "crosshairs"),
                  width = "100%",
                  disabled = btns_disabled_outline()
                )
              ),
              shiny::uiOutput(ns("outline_helptext"))
            )

          } else if(mri_control_input$selection_tool == "paintbrush"){

            if(length(data$paintbrush) != 0 & !drawing_active()){

              shiny::column(
                offset = 1,
                width = 10,
                align = "center",
                shiny::splitLayout(
                  cellWidths = "33%",
                  shiny::actionButton(
                    inputId = ns("paintbrush_confirm"),
                    label = "Confirm",
                    width = "100%",
                    icon = shiny::icon("check"),
                    disabled = btns_disabled_paintbrush()
                  ),
                  shiny::actionButton(
                    inputId = ns("paintbrush_backward"),
                    label = "Backward",
                    icon = shiny::icon(name = "step-backward"),
                    width = "100%",
                    disabled = btns_disabled_paintbrush()
                  ),
                  shiny::actionButton(
                    inputId = ns("paintbrush_reset"),
                    label = "Reset",
                    icon = shiny::icon(name = "trash"),
                    width = "100%",
                    disabled = btns_disabled_paintbrush()
                  )
                ),
                shiny::uiOutput(ns("paintbrush_helptext"))
              )

            } else {

              shiny::uiOutput(ns("paintbrush_helptext"))

            }

          } else if(mri_control_input$selection_tool == "paintbrush_erase"){

            if(length(data$paintbrush_erase) != 0 & !drawing_active()){

              shiny::column(
                offset = 1,
                width = 10,
                align = "center",
                shiny::splitLayout(
                  cellWidths = "33%",
                  shiny::actionButton(
                    inputId = ns("paintbrush_erase_confirm"),
                    icon = shiny::icon("eraser"),
                    label = "Erase",
                    width = "100%",
                    disabled = btns_disabled_paintbrush_erase()
                  ),
                  shiny::actionButton(
                    inputId = ns("paintbrush_erase_backward"),
                    label = "Backward",
                    icon = shiny::icon(name = "step-backward"),
                    width = "100%",
                    disabled = btns_disabled_paintbrush_erase()
                  ),
                  shiny::actionButton(
                    inputId = ns("paintbrush_erase_reset"),
                    label = "Reset",
                    icon = shiny::icon(name = "trash"),
                    width = "100%",
                    disabled = btns_disabled_paintbrush_erase()
                  )
                ),
                shiny::uiOutput(ns("paintbrush_helptext"))
              )

            } else {

              shiny::uiOutput(ns("paintbrush_helptext"))

            }

          } else if(selection_tool() == "margin"){

            shiny::column(
              offset = 1,
              width = 10,
              align = "center",
              shiny::helpText("Use the slider to adjust the margin.")
            )

          }

        }

      })

      output$outline_helptext <- shiny::renderUI({

        if(!drawing_active()){

          word <- ifelse(length(data$paintbrush_erase) != 0, "continue", "start")

          shiny::helpText(glue::glue("Double-click on the MRI to {word} outlining."))

        } else {

          shiny::helpText("Double-click to stop outlining.")

        }

      })

      output$paintbrush_helptext <- shiny::renderUI({

        if(pb_mask_on_this_plane() & !drawing_active()){

          shiny::helpText("Use either button or double-click to continue.")

        } else if(pb_mask_on_diff_plane()){

          name <- tolower(mri_planes_pretty[names(mri_control_input$paintbrush_masks)])

          shiny::helpText(glue::glue("Drawing on {name} plane."))

        } else {

          if(!drawing_active()){

            word <- ifelse(length(data$paintbrush_erase) != 0, "continue", "start")

            shiny::helpText(glue::glue("Double-click on the MRI to {word} brushing."))

          } else {

            shiny::helpText("Double-click to stop brushing.")

          }

        }

      })

      output$show_score_ui <- shiny::renderUI({

        if(mode_init == "selection"){

          shinyWidgets::materialSwitch(
            inputId = ns("show_score"),
            label = "Show Score",
            value = FALSE,
            status = "success"
          )

        }

      })

      # Reactive Values ---------------------------------------------------------

      btns_disabled_outline <- shiny::reactiveVal(value = TRUE)
      btns_disabled_paintbrush <- shiny::reactiveVal(value = TRUE)
      btns_disabled_paintbrush_erase <- shiny::reactiveVal(value = TRUE)

      drawing_active <- shiny::reactiveVal(value = FALSE)

      max_slices <- shiny::reactiveVal(value = 256)
      min_slices <- shiny::reactiveVal(value = 1)

      mode <- shiny::reactiveVal(mode_init)

      mri_control_input <-
        shiny::reactiveValues(
          # all modes
          cursor_on_plane = character(1),

          # inspection
          highlight_hover = logical(1),
          highlight_var = character(1),
          hover_vars = numeric(1),
          voxels_highlighted = character(1),

          # selection
          drawing_on_plane = character(1),
          outlines = list(),
          outlines_reset = numeric(1),
          paintbrush_depth = numeric(1),
          paintbrush_direction = character(1),
          paintbrush_masks = list(),
          paintbrush_mode = character(1),
          paintbrush_radius = numeric(1),
          slice_state = list(),
          selected_voxels = character(1),
          selection_erase = logical(1),
          selection_reset = numeric(1),
          selection_scope = character(1),
          selection_tool = character(1),
          voxels_margin = character(1),
          voxels_margin_cand = character(1),
          voxels_selected = character(1)
        )

      plane_selection_state <- shiny::reactiveVal(value = character())

      #slice_df_hover_highlight is defined below due to dependency reasons

      slice_idx <- shiny::reactiveVal(value = 128)
      slice_idx_debounce <- shiny::debounce(r = slice_idx, millis = 200)

      slice_pos <- shiny::reactiveValues(sag = 128, axi = 128, cor = 128)

      stacks <- shiny::reactiveValues(zoom = list())

      # Reactive Expressions ----------------------------------------------------

      border_color <- shiny::reactive({

        if(nrow(drawing_outline_confirmed()) != 0){

          "orange"

        } else {

          NA

        }

      })

      cursor_on_brain_tissue <- shiny::reactive({ !is.null(slice_df_hover()) && nrow(slice_df_hover()) != 0 })

      cursor_on_mri <- shiny::reactiveVal(value = FALSE)

      cursor_down <- shiny::reactive({

        is.character(input$mouse_state_plot) && input$mouse_state_plot == "down"

        })

      cursor_pos <- shiny::reactive({ c(input$mriPlot_hover$x, input$mriPlot_hover$y) })

      col_min <- shiny::reactive({ min(mri_range()[["col"]]) })
      col_max <- shiny::reactive({ max(mri_range()[["col"]]) })
      col_seq <- shiny::reactive({ col_min():col_max() })

      dragging_localizers <- shiny::reactiveVal(value = character())

      drawing_on_diff_plane <- shiny::reactive({

        length(mri_control_input$drawing_on_plane) == 1 &&
          mri_control_input$drawing_on_plane != plane

      })

      hover_show <- shiny::reactive({

        if(mode() == "inspection"){

          TRUE

        } else if(mode() == "selection") {

          !stringr::str_detect(interaction_tool(), pattern = "outline|paintbrush")

        }

      })

      hover_text <- shiny::reactive({

        shiny::req(cursor_pos())

        if(shiny::isTruthy(slice_df_hover()) & !drawing_active()){

          # Ensure data exists after filtering
          if(nrow(slice_df_hover()) == 0) {

            text <- c("No brain tissue")

          } else if(mode() == "inspection") {

            text <-
              c(
                paste0("Macro: ", make_pretty_label(slice_df_hover()[["ann_macro"]])),
                paste0("Desikan-Kiliany: ", make_pretty_label(slice_df_hover()[["ann_dk_adj"]])),
                paste0("Destrieux: ", make_pretty_label(slice_df_hover()[["ann_dt_adj"]])),
                paste0("Tract: ", make_pretty_label(slice_df_hover()[["wm_tract"]])),
                paste0("Score: ", names(score_set_up$choices)[slice_df_hover()[["CBscore"]]+1])
              )

            text <- stringr::str_c(text[mri_control_input$hover_vars], collapse = "\n")

          } else if(mode() == "selection"){

            text <-
              paste0(
                "Scope: ", ann_var_names[selection_scope()],
                "\nRegion:  ", make_pretty_label(slice_df_hover()[[selection_scope()]])
              )

          }

        } else {

          text <- ""

        }

        return(text)

      })

      inspection_template <- shiny::reactive({

        shiny::req(nrow(voxel_df()) != 0)

        dplyr::rename(
          .data = voxel_df()[voxel_df()[[plane_ccs]] == slice_idx(), ],
          !!!ra_ccs
        )

      })

      interaction_color <- shiny::reactive({

        if(mode() == "selection"){

          selection_color()

        }

      })

      interaction_erase <- shiny::reactive({

        if(mode() == "selection"){

          selection_erase()

        }

      })

      # interaction data.frame of the current slice
      interaction_template <- shiny::reactive({

        if(mode() == "inspection"){

          inspection_template()

        } else  if(mode() == "selection"){

          # filter for voxels in range to speed up paintbrush (de)-selection
          # when zooming in
           dplyr::filter(
             .data = selection_template(),
             within_range(x = col, r = range(col_seq())) &
             within_range(x = row, r = range(row_seq()))
           )

        }

      })

      interaction_tool <- shiny::reactive({

        if(mode() == "selection"){

          selection_tool()

        }

      })

      localizer_active_h <- shiny::reactive({

        if(shiny::isTruthy(input$mriPlot_hover)){ # cursor_on_mri() invalidates too late! - why?

          show_localizer() &&
            !drawing_active() &&
            abs(round(cursor_pos()[2], 0) - slice_pos[[ra_mri[["row"]]]]) <= mri_side_length()*0.01

        } else {

          FALSE

        }

      })

      localizer_active_v <- shiny::reactive({

        if(shiny::isTruthy(input$mriPlot_hover)){ # cursor_on_mri() invalidates too late! - why?

          show_localizer() &&
            !drawing_active() &&
            abs(round(cursor_pos()[1], 0) - slice_pos[[ra_mri[["col"]]]]) <= mri_side_length()*0.01

        } else {

          FALSE

        }

      })

      localizers_active <- shiny::reactive({

        c("v", "h")[c(localizer_active_v(), localizer_active_h())]

      })

      localizers_hover <- shiny::reactive({ length(localizers_active()) != 0 })

      localizer_lwd <- shiny::reactive({ ifelse(cursor_on_mri(), 1, 1.5) })

      localizer_lwd_v <- shiny::reactive({ ifelse(localizer_active_v(), 2, localizer_lwd()) })

      localizer_lwd_h <- shiny::reactive({ ifelse(localizer_active_h(), 2, localizer_lwd()) })

      localizer_col_v <- shiny::reactive({ ifelse(cursor_on_mri() & !localizer_active_v(), ggplot2::alpha("red", 0.5), "red") })

      localizer_col_h <- shiny::reactive({ ifelse(cursor_on_mri() & !localizer_active_h(), ggplot2::alpha("red", 0.5), "red") })

      localizer_lty <- shiny::reactive({ ifelse(cursor_on_mri(), "solid", "solid") })

      # positioning of the vertical localizer
      localizer_x0_v <- shiny::reactive({

        ifelse(
          test = "v" %in% dragging_localizers(),
          yes = abs(round(cursor_pos()[1])),
          no = slice_pos[[ra_mri["col"]]]
        )

        })

      localizer_x1_v <- shiny::reactive({

        ifelse(
          test = "v" %in% dragging_localizers(),
          yes = abs(round(cursor_pos()[1])),
          no = slice_pos[[ra_mri["col"]]]
        )

      })

      localizer_y0_v <- shiny::reactive({ ifelse(show_localizer_ray_h(), NA, row_min()) })

      localizer_y1_v <- shiny::reactive({ ifelse(show_localizer_ray_h(), NA, row_max()) })

      # positioning of the horizontal localizer
      localizer_y0_h <- shiny::reactive({

        ifelse(
          test = "h" %in% dragging_localizers(),
          yes = abs(round(cursor_pos()[2])),
          no = slice_pos[[ra_mri["row"]]]
        )

      })

      localizer_y1_h <- shiny::reactive({

        ifelse(
          test = "h" %in% dragging_localizers(),
          yes = abs(round(cursor_pos()[2])),
          no = slice_pos[[ra_mri["row"]]]
        )

      })

      localizer_x0_h <- shiny::reactive({ ifelse(show_localizer_ray_v(), NA, col_min()) })

      localizer_x1_h <- shiny::reactive({ ifelse(show_localizer_ray_v(), NA, col_max()) })

      mri_range <- shiny::reactive({

        shiny::req(stacks)

        if(length(stacks$zoom) == 0){

          shiny::req(nifti_input())

          col_range <- brain_dims[[col_axis]]
          row_range <- brain_dims[[row_axis]]

          out <- list(col = col_range, row = row_range)

        } else {

          out <- dplyr::last(x = stacks$zoom)

        }

        return(out)

      })

      mri_range_orig <-
        list(
          col = brain_dims[[col_axis]],
          row = brain_dims[[row_axis]]
          )

      mri_slice <- shiny::reactive({

        shiny::req(nifti_input())
        shiny::req(slice_idx())

        get_slice(nifti_input(), plane = plane, slice = slice_idx())

        })

      # (col and row are always equally long - pick col)
      mri_side_length <- shiny::reactive({ as.numeric(dist(mri_range()[["col"]])) })

      paintbrush_radius <- shiny::reactive({ mri_control_input$paintbrush_radius })

      paintbrush_selected <- shiny::reactive({

        interaction_template()$selected |
        interaction_template()$id %in% data$paintbrush

        })

      pb_mask_on_this_plane <- shiny::reactive({

        length(data$paintbrush) != 0 |
        length(data$paintbrush_erase) != 0

        })

      pb_mask_on_diff_plane <- shiny::reactive({

        length(mri_control_input$paintbrush_masks) == 1 &&
          names(mri_control_input$paintbrush_masks) != plane

      })

      ray_depth <- shiny::reactive({

        pb_dir <- mri_control_input$paintbrush_direction
        pb_depth <- mri_control_input$paintbrush_depth

        if(pb_dir == "forward"){

          out <- -pb_depth

        } else if(pb_dir == "backward"){

          out <- pb_depth

        }

        return(out)

      })

      ray_dir <- shiny::reactive({

        if(mri_control_input$paintbrush_mode == "Ray"){

          mri_control_input$paintbrush_direction

        } else {

          NULL

        }

      })

      row_min <- shiny::reactive({ min(mri_range()[["row"]]) })
      row_max <- shiny::reactive({ max(mri_range()[["row"]]) })
      row_seq <- shiny::reactive({ row_min():row_max() })

      show_localizer <- shiny::reactive({ input$show_localizer })

      show_localizer_ray_v <- shiny::reactive({

        if(is.null(mri_control_input$paintbrush_mode)){

          FALSE

        } else {

          mri_control_input$selection_tool %in% c("paintbrush", "paintbrush_erase") &
          mri_control_input$paintbrush_mode == "Ray" &
          mri_control_input$paintbrush_depth != 0 &
          unname(ra_mri["col"]) %in% mri_control_input$cursor_on_plane

        }

      })

      show_localizer_ray_h <- shiny::reactive({

        if(is.null(mri_control_input$paintbrush_mode)){

          FALSE

        } else {

          mri_control_input$selection_tool %in% c("paintbrush", "paintbrush_erase") &
          mri_control_input$paintbrush_mode == "Ray" &
          mri_control_input$paintbrush_depth != 0 &
          unname(ra_mri["row"]) %in% mri_control_input$cursor_on_plane

        }

      })

      show_score <- shiny::reactive({ input$show_score })

      slice_axi <- shiny::reactive({ mri_control_input$slice_state$axi })
      slice_cor <- shiny::reactive({ mri_control_input$slice_state$cor })
      slice_sag <- shiny::reactive({ mri_control_input$slice_state$sag })

      slice_df <- shiny::reactive({ voxel_df()[voxel_df()[[plane_ccs]] == slice_pos[[plane]],] })

      slice_df_hover <- shiny::reactive({

        shiny::req(!drawing_active())
        shiny::req(slice_df())

        # circumvent without req()!
        if(shiny::isTruthy(cursor_pos())){

          slice_dfh <- slice_df()

          hover_x <- round(cursor_pos()[1])
          hover_y <- round(cursor_pos()[2])

          slice_dfh <- slice_dfh[slice_dfh[[ra_ccs["col"]]] == hover_x, ]
          slice_dfh <- slice_dfh[slice_dfh[[ra_ccs["row"]]] == hover_y, ]

          return(slice_dfh)

        } else {

          return(NULL)

        }

      })

      slice_df_hover_highlight <- shiny::debounce(r = slice_df_hover, millis = 175)

      voxel_df <- shiny::reactive({

        dplyr::mutate(
          .data = voxel_df_input(),
          selected = id %in% c(mri_control_input$voxels_selected)
        )

      })


      # ----- Observers

      # Observe Events ----------------------------------------------------------

      shiny::observeEvent(mri_control(), {

        for(nm in names(mri_control())){

          slot_data <- mri_control()[[nm]]

          if(!identical(x = slot_data, y = mri_control_input[[nm]])){

            mri_control_input[[nm]] <- slot_data

          }

        }

      })

      # slice positioning
      shiny::observeEvent(slice_idx_debounce(), {

        if(slice_pos[[plane]] != slice_idx_debounce()){

          slice_pos[[plane]] <- slice_idx_debounce()

        }

      })

      shiny::observeEvent(slice_pos[[plane]], {

        if(slice_pos[[plane]] != slice_idx()){

          slice_idx({ slice_pos[[plane]] })

        }

      })

      shiny::observeEvent(input$mriPlot_hover, {

        if(shiny::isTruthy(input$mriPlot_hover)){

          if(!cursor_on_mri()){

            cursor_on_mri({ TRUE })

          }

        } else {

          cursor_on_mri({ FALSE })

        }

      }, ignoreNULL = FALSE)

      # --- drawing logic
      shiny::observeEvent(cursor_pos(),{

        shiny::req(mode() != "inspection")

        if(drawing_active() & interaction_tool() == "outline"){

          data$outline$col <- c(data$outline$col, cursor_pos()[1])
          data$outline$row <- c(data$outline$row, cursor_pos()[2])

        } else if(drawing_active() & selection_tool() == "paintbrush"){

          new_ids <-
            identify_obs_within_radius2D(
              cursor_pos = cursor_pos(),
              radius = paintbrush_radius(),
              interaction_template = interaction_template()[!interaction_template()$selected,],
              preselected_ids = data$paintbrush,
              selection_scope = selection_scope()
            )

          if(length(new_ids) != 0){

            data$paintbrush <- c(data$paintbrush, new_ids)

            if(mri_control_input$paintbrush_mode == "Sphere"){

              data$cursor_pos[[length(data$cursor_pos)+1]] <- cursor_pos()

            }

          }

        } else if(drawing_active() & selection_tool() == "paintbrush_erase"){

          new_ids <-
            identify_obs_within_radius2D(
              cursor_pos = cursor_pos(),
              radius = paintbrush_radius(),
              interaction_template = interaction_template()[interaction_template()$selected,],
              preselected_ids = data$paintbrush_erase
            )

          if(length(new_ids) != 0){

            data$paintbrush_erase <- c(data$paintbrush_erase, new_ids)

            if(mri_control_input$paintbrush_mode == "Sphere"){

              data$cursor_pos[[length(data$cursor_pos)+1]] <- cursor_pos()

            }

          }

        }

      })

      # reset outline from the controller side
      shiny::observeEvent(mri_control_input$outlines_reset, {

        data$outline$col <- numeric()
        data$outline$row <- numeric()

        drawing_outline_confirmed <- data.frame(col = numeric(), row = numeric())

        for(outline_btn in c("confirm", "backward", "reset", "focus")){

          if(outline_btn == "confirm"){

            icon <- shiny::icon(name = "check")

          } else {

            icon <- NULL

          }

          shiny::updateActionButton(
            session = session,
            inputId = paste0("outline_", outline_btn),
            disabled = TRUE,
            icon = icon
          )

        }

      }, ignoreInit = TRUE)


      # --- forward/backward
      shiny::observeEvent(input$mri_forward,{

        if(pb_mask_on_this_plane()){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Unconfirmed Paintbrush Drawing!",
            text = glue::glue(
              "There is an unconfirmed paintbrush drawing on this MRI.
              Please confirm or reset it before using the slider."
            ),
            type = "info"
          )

          shiny::req(FALSE)

        }

        if(slice_idx() != max_slices()){

          slice_idx({ slice_idx() + 1})

        }

      })

      shiny::observeEvent(input$mri_backward,{

        if(pb_mask_on_this_plane()){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Unconfirmed Paintbrush Drawing!",
            text = glue::glue(
              "There is an unconfirmed paintbrush drawing on this MRI.
              Please confirm or reset it before using the slider."
            ),
            type = "info"
          )

          shiny::req(FALSE)

        }

        if(slice_idx() != min_slices()){

          slice_idx({ slice_idx() - 1})

        }

      })

      shiny::observeEvent(slice_pos[[plane]], {

        shiny::req(input$mri_slider_slice)

        if(shiny::isolate(input$mri_slider_slice) != slice_pos[[plane]]){

          #shinyWidgets::updateNoUiSliderInput(
          shiny::updateSliderInput(
            session = session,
            inputId = "mri_slider_slice",
            value = slice_pos[[plane]]
          )

        }

      })

      # --- zoom in
      zoom_const <- 0.05
      zoom_factor <- shiny::reactiveVal(value = 1)

      shiny::observeEvent(input$zoom_in,{

        # new mri range
        new_zoom_fct <- zoom_factor() + zoom_const
        nmr <- update_mri_range_zoom(mri_range = mri_range_orig, zoom_fct = new_zoom_fct)

        # update localizer
        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_row",
          range = nmr$row,
          value = round(mean(nmr$row))
        )

        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_col",
          range = nmr$col,
          value = round(mean(nmr$col))
        )

        # stack zoom ranges - update plots
        stacks <- add_to_stack(stacks, which = "zoom", what = nmr)
        zoom_factor({ new_zoom_fct })


      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      shiny::observeEvent(input$mriPlot_brush, {

        nmr <-
          list(
            col = c(floor(input$mriPlot_brush$xmin), ceiling(input$mriPlot_brush$xmax)),
            row = c(floor(input$mriPlot_brush$ymin), ceiling(input$mriPlot_brush$ymax))
          )

        # ensure both axes are equally long
        length_col <- as.numeric(dist(nmr$col))
        length_row <- as.numeric(dist(nmr$row))

        if(length_col != length_row){

          length_max <- ifelse(length_col>length_row, yes = length_col, no = length_row)

          half <- ceiling(length_max/2)

          nmr <- purrr::map(nmr, .f = function(r){

            mid <- round(mean(r))

            c(mid-half, mid+half)

          })

        }

        # stack zoom ranges
        stacks <- add_to_stack(stacks, which = "zoom", what = nmr)

      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # --- zoom out
      shiny::observeEvent(input$zoom_out,{

        # zoom stacks remaining?
        if(length(stacks$zoom) != 0){

          stacks <- reduce_stack(stacks, which = "zoom")
          zoom_factor({ zoom_factor() - zoom_const })

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(zoom_factor(), {

        session$sendCustomMessage(
          ns("set_zoom"),
          list(zoom = zoom_factor())
        )

      })

      # localizer movement
      shiny::observeEvent(cursor_down(), {

        if(cursor_down()){

          if(localizers_hover()){

            dragging_localizers({ localizers_active() })

          }

        } else if(!cursor_down()){

          # either of two localizers was dragged
          if(length(dragging_localizers()) != 0){

            if("v" %in% dragging_localizers()){

              slice_pos[[col_axis]] <- round(cursor_pos()[1])

            }

            if("h" %in% dragging_localizers()){

              slice_pos[[row_axis]] <- round(cursor_pos()[2])

            }

            dragging_localizers({ character() })

          }

        }

      })

      shiny::observeEvent(localizers_active(), {

        if(localizers_hover()){

          session$sendCustomMessage(ns("changeCursor"), "grab")

        } else {

          session$sendCustomMessage(ns("changeCursor"), "crosshair")

        }

      })

      # --- updates of slice_pos from the controller side
      shiny::observeEvent(slice_sag(), {

        if(!is.null(slice_sag()) && shiny::isolate({slice_pos$sag}) != slice_sag()) {

          slice_pos$sag <- slice_sag()

        }

      })

      shiny::observeEvent(slice_axi(), {

        if(!is.null(slice_axi()) && shiny::isolate({slice_pos$axi}) != slice_axi()) {

          slice_pos$axi <- slice_axi()

        }

      })

      shiny::observeEvent(slice_cor(), {

        if(!is.null(slice_cor()) && shiny::isolate({slice_pos$cor}) != slice_cor()) {

          slice_pos$cor <- slice_cor()

        }

      })

      # Observe Events (Internal Slider Updates) --------------------------------

      shiny::observeEvent(input$mri_slider_slice,{

        if(pb_mask_on_this_plane()){

          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Unconfirmed Paintbrush Drawing!",
            text = glue::glue(
              "There is an unconfirmed paintbrush drawing on this MRI.
              Please confirm or reset it before using the slider."
            ),
            type = "info"
          )

          shiny::updateSliderInput(
            session = session,
            inputId = "mri_slider_slice",
            value = slice_pos[[plane]]
          )

          shiny::req(FALSE)

        }

        slice_pos[[plane]] <- input$mri_slider_slice

      })

      # Outputs (Plots) ---------------------------------------------------------

      output$mriInteractionPlot <- shiny::renderPlot({

        plot_mri_frame(col = col_seq(), row = row_seq())

      }, bg = "transparent")

      output$mriPaintbrushPlot <- shiny::renderPlot({

        shiny::req(stringr::str_detect(selection_tool(), "paintbrush"))

        # plotting context for all drawing options
        plot_mri_frame(col = col_seq(), row = row_seq())

        if(drawing_active() & selection_tool() == "paintbrush"){

          graphics::rect(
            xleft = interaction_template()[paintbrush_selected(),]$col - 0.5,
            xright = interaction_template()[paintbrush_selected(),]$col + 0.5,
            ybottom = interaction_template()[paintbrush_selected(),]$row - 0.5,
            ytop = interaction_template()[paintbrush_selected(),]$row + 0.5,
            col = interaction_template()[paintbrush_selected(),]$color,
            border = NA
          )

        } else if(drawing_active() & selection_tool() == "paintbrush_erase"){

          graphics::rect(
            xleft = interaction_template()[interaction_template()$selected,]$col - 0.5,
            xright = interaction_template()[interaction_template()$selected,]$col + 0.5,
            ybottom = interaction_template()[interaction_template()$selected,]$row - 0.5,
            ytop = interaction_template()[interaction_template()$selected,]$row + 0.5,
            col = dplyr::if_else(
              condition =
                interaction_template()[interaction_template()$selected,]$id %in%
                data$paintbrush_erase,
              true = ggplot2::alpha("red", 0.45),
              false = interaction_template()[interaction_template()$selected,]$color
            ),
            border = NA
          )

        }

      }, bg = "transparent")

      output$mriOutlinePlot <- shiny::renderPlot({

        shiny::req(nrow(drawing_outline()) != 0 & selection_tool() == "outline")

        plot_mri_frame(
          col = drawing_outline()$col,
          row = drawing_outline()$row,
          type = "l",
          lwd = 3,
          color = "orange",
          xlim = range(col_seq()),
          ylim = rev(range(row_seq())),
          bg = NA
        )

        if(!drawing_active()){

          graphics::polypath(
            x = drawing_outline()$col,
            y = drawing_outline()$row,
            col = ggplot2::alpha("orange", 0.15),
            border = border_color(),
            lwd = 3,
            lty = "solid"
          )

        }

      }, bg = "transparent")

      output$mriHoverInfoPlot <- shiny::renderPlot({

        shiny::req(mri_range())
        side_length <- mri_side_length()

        # -- initiate plot
        plot_mri_frame(col = col_seq(), row = row_seq())

        # -- hover text
        if(isTRUE(hover_show())){

          shiny::req(hover_text())

          x_pos <- min(mri_range()[["col"]]) + side_length*0.0125
          y_pos <- max(mri_range()[["row"]]) - side_length*0.0125

          graphics::text(
            x = x_pos,
            y = y_pos,
            labels = hover_text(),
            col = "white",
            cex = 1,
            font = 2,
            adj = c(0, 0)
          )

        }

      }, bg = "transparent")

      output$mriLocalizerPlot <- shiny::renderPlot({

        shiny::req(show_localizer())

        plot_mri_frame(col = col_seq(), row = row_seq())

        if(plane == "sag"){

          # vertical
          segments(
            x0 = localizer_x0_v(), x1 = localizer_x1_v(),
            y0 = localizer_y0_v(), y1 = localizer_y1_v(),
            col = localizer_col_v(), lwd = localizer_lwd_v(), lty = localizer_lty()
          )

          # horizontal
          segments(
            x0 = localizer_x0_h(), x1 = localizer_x1_h(),
            y0 = localizer_y0_h(), y1 = localizer_y1_h(),
            col = localizer_col_h(), lwd = localizer_lwd_h(), lty = localizer_lty()
          )

          if(show_localizer_ray_v()){

            ray_x <- slice_pos[[ra_mri[["col"]]]] - ray_depth()

            if(!within_range(ray_x, r = range(voxel_df()[[ra_ccs["col"]]]))){

              if(ray_dir() == "forward"){

                ray_x <- max(voxel_df()[[ra_ccs["col"]]])

                depth <- ray_x - slice_pos[[ra_mri[["col"]]]]

                text_y <-
                  c(
                    row_min() + diff(range(row_seq())) * 0.06,
                    row_max() - diff(range(row_seq())) * 0.04
                  )

                srt <- 270

              } else if(ray_dir() == "backward"){

                ray_x <- min(voxel_df()[[ra_ccs["col"]]])

                depth <- slice_pos[[ra_mri[["col"]]]] - ray_x

                text_y <-
                  c(
                    row_max() - diff(range(row_seq())) * 0.06,
                    row_min() + diff(range(row_seq())) * 0.04
                  )

                srt <- 90

              }

              label <- c(paste(depth, "mm"), "Max.")

            } else {

              label <- paste0(abs(ray_depth()), "mm")

              if(ray_dir() == "forward"){

                text_y <- row_min() + diff(range(row_seq())) * 0.05

                srt <- 270

              } else if(ray_dir() == "backward"){

                text_y <- row_max() - diff(range(row_seq())) * 0.05

                srt <- 90

              }

            }

            if(ray_dir() == "forward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] + diff(range(col_seq())) * 0.025

            } else if(ray_dir() == "backward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] - diff(range(col_seq())) * 0.025

            }

            # vertical
            segments(
              x0 = ray_x, x1 = ray_x,
              y0 = row_min(), y1 = row_max(),
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(row_seq())*0.0001,
              font = 1,
              srt = srt,
              adj = c(0.5, 0.5)
            )

          }

          if(show_localizer_ray_h()){

            ray_y <- slice_pos[[ra_mri[["row"]]]] + ray_depth()

            if(!within_range(ray_y, r = range(voxel_df()[[ra_ccs["row"]]]))){

              if(ray_dir() == "forward"){

                ray_y <- min(voxel_df()[[ra_ccs["row"]]])

                depth <- slice_pos[[ra_mri[["row"]]]] - ray_y

              } else if(ray_dir() == "backward"){

                ray_y <- max(voxel_df()[[ra_ccs["row"]]])

                depth <- ray_y - slice_pos[[ra_mri[["row"]]]]

              }

              text_x <-
                c(
                  col_min() + diff(range(col_seq())) * 0.06,
                  col_max() - diff(range(col_seq())) * 0.04
                )

              label <- c(paste(depth, "mm"), "Max")

            } else {

              text_x <- col_min() + diff(range(col_seq())) * 0.05

              label <- paste0(abs(ray_depth()), "mm")

            }

            if(ray_dir() == "forward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] - diff(range(row_seq())) * 0.015

            } else if(ray_dir() == "backward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] + diff(range(row_seq())) * 0.015

            }

            # horizontal
            segments(
              x0 = col_min(), x1 = col_max(),
              y0 = ray_y, y1 = ray_y,
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(col_seq())*0.0001,
              font = 1,
              adj = c(0.5, 0.5)
            )

          }

        } else if(plane == "axi"){

          # vertical
          segments(
            x0 = localizer_x0_v(), x1 = localizer_x1_v(),
            y0 = localizer_y0_v(), y1 = localizer_y1_v(),
            col = localizer_col_v(), lwd = localizer_lwd_v(), lty = localizer_lty()
          )

          # horizontal
          segments(
            x0 = localizer_x0_h(), x1 = localizer_x1_h(),
            y0 = localizer_y0_h(), y1 = localizer_y1_h(),
            col = localizer_col_h(), lwd = localizer_lwd_h(), lty = localizer_lty()
          )

          if(show_localizer_ray_v()){

            ray_x <- slice_pos[[ra_mri[["col"]]]] + ray_depth()

            if(!within_range(ray_x, r = range(voxel_df()[[ra_ccs["col"]]]))){

              if(ray_dir() == "backward"){

                ray_x <- max(voxel_df()[[ra_ccs["col"]]])

                depth <- ray_x - slice_pos[[ra_mri[["col"]]]]

                text_y <-
                  c(
                    row_min() + diff(range(row_seq())) * 0.06,
                    row_max() - diff(range(row_seq())) * 0.04
                  )

                srt <- 270

              } else if(ray_dir() == "forward"){

                ray_x <- min(voxel_df()[[ra_ccs["col"]]])

                depth <- slice_pos[[ra_mri[["col"]]]] - ray_x

                text_y <-
                  c(
                    row_max() - diff(range(row_seq())) * 0.06,
                    row_min() + diff(range(row_seq())) * 0.04
                  )

                srt <- 90

              }

              label <- c(paste(depth, "mm"), "Max.")

            } else {

              if(ray_dir() == "backward"){

                text_y <- row_min() + diff(range(row_seq())) * 0.05

                srt <- 270

              } else if(ray_dir() == "forward"){

                text_y <- row_max() - diff(range(row_seq())) * 0.05

                srt <- 90

              }

              label <- paste0(abs(ray_depth()), "mm")

            }

            if(ray_dir() == "backward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] + diff(range(col_seq())) * 0.025

            } else if(ray_dir() == "forward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] - diff(range(col_seq())) * 0.025

            }

            # vertical
            segments(
              x0 = ray_x, x1 = ray_x,
              y0 = row_min(), y1 = row_max(),
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(row_seq())*0.0001,
              font = 1,
              srt = srt,
              adj = c(0.5, 0.5)
            )

          }

          if(show_localizer_ray_h()){

            ray_y <- slice_pos[[ra_mri[["row"]]]] - ray_depth()

            if(!within_range(ray_y, r = range(voxel_df()[[ra_ccs["row"]]]))){

              if(ray_dir() == "backward"){

                ray_y <- min(voxel_df()[[ra_ccs["row"]]])

                depth <- slice_pos[[ra_mri[["row"]]]] - ray_y

              } else if(ray_dir() == "forward"){

                ray_y <- max(voxel_df()[[ra_ccs["row"]]])

                depth <- ray_y - slice_pos[[ra_mri[["row"]]]]

              }

              text_x <-
                c(
                  col_min() + diff(range(col_seq())) * 0.06,
                  col_max() - diff(range(col_seq())) * 0.04
                )

              label <- c(paste(depth, "mm"), "Max")

            } else {

              text_x <- col_min() + diff(range(col_seq())) * 0.05

              label <- paste0(abs(ray_depth()), "mm")

            }

            if(ray_dir() == "backward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] - diff(range(row_seq())) * 0.015

            } else if(ray_dir() == "forward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] + diff(range(row_seq())) * 0.015

            }

            # horizontal
            segments(
              x0 = col_min(), x1 = col_max(),
              y0 = ray_y, y1 = ray_y,
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(col_seq())*0.0001,
              font = 1,
              adj = c(0.5, 0.5)
            )

          }

        } else if(plane == "cor"){

          # vertical
          segments(
            x0 = localizer_x0_v(), x1 = localizer_x1_v(),
            y0 = localizer_y0_v(), y1 = localizer_y1_v(),
            col = localizer_col_v(), lwd = localizer_lwd_v(), lty = localizer_lty()
          )

          # horizontal
          segments(
            x0 = localizer_x0_h(), x1 = localizer_x1_h(),
            y0 = localizer_y0_h(), y1 = localizer_y1_h(),
            col = localizer_col_h(), lwd = localizer_lwd_h(), lty = localizer_lty()
          )

          if(show_localizer_ray_v()){

            ray_x <- slice_pos[[ra_mri[["col"]]]] + ray_depth()

            if(!within_range(ray_x, r = range(voxel_df()[[ra_ccs["col"]]]))){

              if(ray_dir() == "backward"){

                ray_x <- max(voxel_df()[[ra_ccs["col"]]])

                depth <- ray_x - slice_pos[[ra_mri[["col"]]]]

                text_y <-
                  c(
                    row_min() + diff(range(row_seq())) * 0.06,
                    row_max() - diff(range(row_seq())) * 0.04
                  )

                srt <- 270

              } else if(ray_dir() == "forward"){

                ray_x <- min(voxel_df()[[ra_ccs["col"]]])

                depth <- slice_pos[[ra_mri[["col"]]]] - ray_x

                text_y <-
                  c(
                    row_max() - diff(range(row_seq())) * 0.0,
                    row_min() + diff(range(row_seq())) * 0.04
                  )

                srt <- 90

              }

              label <- c(paste(depth, "mm"), "Max.")

            } else {

              if(ray_dir() == "backward"){

                text_y <- row_min() + diff(range(row_seq())) * 0.05

                srt <- 270

              } else if(ray_dir() == "forward"){

                text_y <- row_max() - diff(range(row_seq())) * 0.05

                srt <- 90

              }

              label <- paste0(abs(ray_depth()), "mm")

            }

            if(ray_dir() == "backward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] + diff(range(col_seq())) * 0.025

            } else if(ray_dir() == "forward"){

              text_x <- slice_pos[[ra_mri[["col"]]]] - diff(range(col_seq())) * 0.025

            }

            # vertical
            segments(
              x0 = ray_x, x1 = ray_x,
              y0 = row_min(), y1 = row_max(),
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(row_seq())*0.0001,
              font = 1,
              srt = srt,
              adj = c(0.5, 0.5)
            )

          }

          if(show_localizer_ray_h()){

            ray_y <- slice_pos[[ra_mri[["row"]]]] + ray_depth()

            if(!within_range(ray_y, r = range(voxel_df()[[ra_ccs["row"]]]))){

              if(ray_dir() == "forward"){

                ray_y <- min(voxel_df()[[ra_ccs["row"]]])

                depth <- slice_pos[[ra_mri[["row"]]]] - ray_y

              } else if(ray_dir() == "backward"){

                ray_y <- max(voxel_df()[[ra_ccs["row"]]])

                depth <- ray_y - slice_pos[[ra_mri[["row"]]]]

              }

              text_x <-
                c(
                  col_min() + diff(range(col_seq())) * 0.06,
                  col_max() - diff(range(col_seq())) * 0.04
                )

              label <- c(paste(depth, "mm"), "Max")

            } else {

              text_x <- col_min() + diff(range(col_seq())) * 0.05

              label <- paste0(abs(ray_depth()), "mm")

            }

            if(ray_dir() == "forward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] - diff(range(row_seq())) * 0.015

            } else if(ray_dir() == "backward"){

              text_y <- slice_pos[[ra_mri[["row"]]]] + diff(range(row_seq())) * 0.015

            }

            # horizontal
            segments(
              x0 = col_min(), x1 = col_max(),
              y0 = ray_y, y1 = ray_y,
              col = "red", lwd = localizer_lwd(), lty = "dotted"
            )

            text(
              x = text_x,
              y = text_y,
              labels = label,
              col = "red",
              cex = 1.25 - diff(col_seq())*0.0001,
              font = 1,
              adj = c(0.5, 0.5)
            )

          }

        }

      }, bg = "transparent")

      output$mriVisualAidPlot <- shiny::renderPlot({

        shiny::req(mri_range())
        side_length <- mri_side_length()

        # -- initiate plot
        plot_mri_frame(col = col_seq(), row = row_seq())

        # -- scale bar (currently not active)
        if(FALSE){

          # rounds to 5er steps
          dist_display <- (round(side_length) * 0.1 / 5) * 5
          lwd <- 1.5
          col <- "white"

          # side left
          graphics::segments(
            x0 = min(mri_range()[["col"]]) + (side_length*0.05),
            x1 = min(mri_range()[["col"]]) + (side_length*0.05),
            y0 = min(mri_range()[["row"]]) + (side_length*0.04),
            y1 = min(mri_range()[["row"]]) + (side_length*0.06),
            col = col,
            lwd = lwd
          )

          # main horizontal
          graphics::segments(
            x0 = min(mri_range()[["col"]]) + (side_length*0.05),
            x1 = min(mri_range()[["col"]]) + (side_length*0.05) + dist_display,
            y0 = min(mri_range()[["row"]]) + (side_length*0.05),
            y1 = min(mri_range()[["row"]]) + (side_length*0.05),
            col = col,
            lwd = lwd
          )

          # side right
          graphics::segments(
            x0 = min(mri_range()[["col"]]) + (side_length*0.05) + dist_display,
            x1 = min(mri_range()[["col"]]) + (side_length*0.05) + dist_display,
            y0 = min(mri_range()[["row"]]) + (side_length*0.04),
            y1 = min(mri_range()[["row"]]) + (side_length*0.06),
            col = col,
            lwd = lwd
          )

          # dist text
          graphics::text(
            x = min(mri_range()[["col"]]) + (side_length*0.05) + (dist_display*1.1),
            y = min(mri_range()[["row"]]) + (side_length*0.0575),
            labels = paste0(dist_display, "mm"),
            col = col,
            cex = 1,
            font = 1,
            adj = c(0, 0)
          )

        }

        # -- mode and interaction aid
        if(shiny::isTruthy(interaction_tool())){

          # update label
          label <-
            paste0(
              "Tool: ",
              stringr::str_remove_all(interaction_tool(), pattern = "erase$") %>%
              stringr::str_to_title() %>%
              stringr::str_replace_all(string = ., pattern = "_", replacement = " ")
            )

          if(interaction_erase()){

            label <- paste0(label, " - Erase")

          }

          if(selection_tool() == "paintbrush"){

            label <- paste0(label, "\nMode: ", mri_control_input$paintbrush_mode)

            if(mri_control_input$paintbrush_mode == "Ray") {

              label <- paste0(label, " (", stringr::str_to_title(mri_control_input$paintbrush_direction), ")")

              if(is.null(selection_scope())){

                scope <- "None"

              } else {

                scope <- ann_var_names[selection_scope()]

              }

              label <- paste0(label, "\nScope: ", ann_var_names[selection_scope()])

            }

          }

        } else { label = NA }

        x_pos <- min(mri_range()[["col"]]) + (side_length*0.0125)
        y_pos <- min(mri_range()[["row"]]) + (side_length*0.0125)

        # dist text
        graphics::text(
          x = x_pos,
          y = y_pos,
          labels = label,
          col = "white",
          cex = 1,
          font = 2,
          adj = c(0, 1)
        )

      }, bg = "transparent")

      output$mriInspectionPlot <- shiny::renderPlot({

        shiny::req(mode() == "inspection")
        shiny::req(inspection_template())

        plot_mri_frame(col = col_seq(), row = row_seq())

        voxels_show <- inspection_template()
        shiny::req(nrow(voxels_show) != 0)

        highlight <- voxels_show$id %in% mri_control_input$voxels_highlighted

        if(isTRUE(mri_control_input$highlight_hover) && cursor_on_brain_tissue() && !cursor_down()){

          hvar <- mri_control_input$highlight_var
          hem <- slice_df_hover_highlight()[["hemisphere"]]

          highlight_hover <-
            voxels_show[[hvar]] == slice_df_hover_highlight()[[hvar]] &
            voxels_show[["hemisphere"]] == slice_df_hover_highlight()[["hemisphere"]]

          highlight <- highlight | highlight_hover

        }

        if(mode_opts$inspection_var == "CBscore"){

          col <- score_set_up$colors[voxels_show$CBscore+1]

        } else if(mode_opts$inspection_var == "CBscore_smooth"){

          col <- map_cbscore_colors(voxels_show$CBscore_smooth, colors = score_set_up$colors[2:5])

        }

        col[highlight] <- ggplot2::alpha(col[highlight], 0.6)
        col[!highlight] <- ggplot2::alpha(col[!highlight], ifelse(any(highlight), 0.2, 0.4))

        graphics::rect(
          xleft = voxels_show$col - 0.5,
          xright = voxels_show$col + 0.5,
          ybottom = voxels_show$row - 0.5,
          ytop = voxels_show$row + 0.5,
          col = col,
          border = NA
        )

      }, bg = "transparent")

      output$mriSelectionStatePlot <- shiny::renderPlot({

        # TRUE if not paintbrushing
        shiny::req(show_selection_state())

        if(selection_tool() == "margin"){

          voxels_show <-
            dplyr::filter(
              .data = interaction_template(),
              selected |
              id %in% mri_control_input$voxels_margin_cand
              )

        } else if(selection_tool() == "paintbrush") {

          voxels_show <-
            dplyr::filter(
              .data = interaction_template(),
              selected | id %in% data$paintbrush
              ) %>%
            dplyr::mutate(
              color = dplyr::case_when(
                id %in% data$paintbrush & id %in% non_brain_template$id ~ ggplot2::alpha(color, 0.2),
                id %in% data$paintbrush & !id %in% non_brain_template$id ~ ggplot2::alpha(color, alpha_val),
                TRUE ~ color
              )
            )

        } else if(selection_tool() == "paintbrush_erase"){

          voxels_show <-
            dplyr::mutate(
              .data = dplyr::filter(interaction_template(), selected),
              color = dplyr::case_when(
                id %in% data$paintbrush_erase ~ ggplot2::alpha(colorsCB$erased, alpha_val),
                TRUE ~ color
              )
            )

        } else {

          voxels_show <- dplyr::filter(interaction_template(), selected)

        }

        plot_mri_frame(col = col_seq(), row = row_seq())

        graphics::rect(
          xleft = voxels_show$col - 0.5,
          xright = voxels_show$col + 0.5,
          ybottom = voxels_show$row - 0.5,
          ytop = voxels_show$row + 0.5,
          col = voxels_show$color,
          border = NA
        )

      }, bg = "transparent")

      output$mriScorePlot <- shiny::renderPlot({

        shiny::req(show_score())
        shiny::req(input$transp_score)

        plot_mri_frame(col = col_seq(), row = row_seq())

        voxels_show <- dplyr::filter(interaction_template(), !is.na(CBscore))

        shiny::req(nrow(voxels_show) != 0)

        if("CBscore_smooth" %in% names(voxels_show) ){

          col <- map_cbscore_colors(voxels_show$CBscore_smooth, colors = score_set_up$colors[2:5])

        } else {

          col <- score_set_up$colors[voxels_show$CBscore+1]

        }

        graphics::rect(
          xleft = voxels_show$col - 0.5,
          xright = voxels_show$col + 0.5,
          ybottom = voxels_show$row - 0.5,
          ytop = voxels_show$row + 0.5,
          col = ggplot2::alpha(col, 1-input$transp_score),
          border = NA
        )

      }, bg = "transparent")

      output$mriSlicePlot <- renderUI({

        if(plane == "axi"){

          shiny::showNotification(
            ui = "MRI data is loading. This can take a few seconds - even if the red busy indicator is not displayed.",
            type = "message",
            duration = 10
          )

        }

        print("hello3")

        rp <- range(brain_dims[plane])
        X <- rp[1]:rp[2]

        tags$div(
          id = ns("mriSlicePlot"),
          style = "width: 100%; height: 100%; overflow: hidden; border: 1px solid #ccc;",
          shiny::withProgress(
            message = glue::glue("Loading {mri_planes_pretty[plane]}."),
            value = 0,
            expr = {

              lapply(X = X, FUN = function(i) {

                shiny::incProgress(amount = 1/length(X), detail = "Progress:")

                tags$img(
                  id = paste0("slice_", i, "_", ns("")),
                  src = pre_rendered_slices[[plane]][[i]],
                  class = "zoomable-image",
                  style =
                    ifelse(
                      test = i==128,
                      yes = "display: block; position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                      no = "display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
                    )
                )
              })

            }
          )

        )

      })

      shiny::observe({

        session$sendCustomMessage(ns("show_slice"), slice_idx())

      })

      # 1. Mode: Inspection -----------------------------------------------------

      # ----- Reactive values

      voxels_quick_select <- shiny::reactiveVal(value = character())

      # ----- Observe Events

      shiny::observeEvent(reset_quick_select(), {

        voxels_quick_select({ character() })

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "inspection" & !localizers_hover())

        slice_pos[[plane]] <- slice_idx()
        slice_pos[[col_axis]] <- round(cursor_pos()[1])
        slice_pos[[row_axis]] <- round(cursor_pos()[2])

        if(isTRUE(mode_opts$quick_select)){

          if(!cursor_on_brain_tissue()){

            shiny::showNotification(
              ui = "Can not select non-brain tissue.",
              duration = 5,
              type = "error"
            )

            shiny::req(FALSE)

          }

          hvar <- mri_control_input$highlight_var
          brain_region <- slice_df_hover()[[hvar]]
          hemisphere <- slice_df_hover()[["hemisphere"]]

          voxels_quick_select({

            dplyr::filter(
              .data = voxel_df(),
              !!rlang::sym(hvar) == {{brain_region}} & hemisphere == {{hemisphere}}
            )[["id"]]

          })

        }

      })

      # 2. Mode: Selection ------------------------------------------------------

      # ----- Reactive Values

      # empty flexible data container for temporary progress
      data <-
        shiny::reactiveValues(
          cursor_pos = list(),
          outline = list(col = numeric(0), row = numeric(0)),
          paintbrush = character(),
          paintbrush_erase = character()
        )

      shiny::observeEvent(mri_control_input$selection_reset, {

        data$cursor_pos <- list()
        data$paintbrush <- character()
        data$paintbrush_erase <- character()
        paintbrushed_ids({ character() })

      }, ignoreInit = TRUE)

      drawing_outline_confirmed <- shiny::reactiveVal(value = data.frame(col = numeric(), row = numeric()))

      selection_tool <- shiny::reactiveVal(value = "region_click")

      # ----- Reactive (Expressions)

      drawing_outline <- shiny::reactive({

        shiny::req(selection_tool() == "outline")

        data.frame(
          col = data$outline$col,
          row = data$outline$row
        )

      })

      selection_color <- shiny::reactive({

        ifelse(!selection_erase(), yes = colorsCB$not_selected, no = colorsCB$erased)

      })

      selection_erase <- shiny::reactive({ mri_control_input$selection_erase })

      # none = NULL for identify_obs_within_radius2D/3D()
      selection_scope <- shiny::reactive({

        if(stringr::str_detect(selection_tool(), "paintbrush")){

          if(mri_control_input$paintbrush_mode == "Sphere" |
             mri_control_input$selection_scope == "none"){

            NULL

          } else {

            mri_control_input$selection_scope

          }

        } else {

          if(mri_control_input$selection_scope == "none"){

            NULL

          } else {

            mri_control_input$selection_scope

          }

        }

      })

      selection_slices <- shiny::reactive({

        if(mri_control_input$paintbrush_mode == "Ray"){

          if(ray_depth() == 0){

            out <- slice_idx()

          } else {

            if(mri_control_input$paintbrush_direction == "forward") {

              if(plane == "cor"){

                out <- (slice_idx()+mri_control_input$paintbrush_depth):slice_idx()

              } else {

                out <- (slice_idx()-mri_control_input$paintbrush_depth):slice_idx()

              }

            } else if(mri_control_input$paintbrush_direction == "backward"){

              if(plane == "cor"){

                out <- slice_idx():(slice_idx()-mri_control_input$paintbrush_depth)

              } else {

                out <- slice_idx():(slice_idx()+mri_control_input$paintbrush_depth)

              }

            }

          }

        } else {

          out <- NULL

        }

        return(out)

      })

      selection_template <- shiny::reactive({

        shiny::req(nrow(voxel_df()) != 0)

        out <-
          dplyr::rename(voxel_df()[voxel_df()[[plane_ccs]]==slice_idx(),], !!!ra_ccs) %>%
          dplyr::mutate(
            color = dplyr::case_when(
              selected & id %in% mri_control_input$voxels_margin ~ ggplot2::alpha(colorsCB$margin, alpha = alpha_val),
              selected ~ ggplot2::alpha(color_selected(), alpha = alpha_val),
              id %in% mri_control_input$voxels_margin_cand ~ ggplot2::alpha(colorsCB$margin_cand, alpha = alpha_val),
              TRUE ~ ggplot2::alpha(colorsCB$not_selected, alpha = alpha_val)
            )
          )

        if(stringr::str_detect(selection_tool(), "paintbrush")){

          nbt_slice <-
            dplyr::rename(non_brain_template[non_brain_template[[plane_ccs]]==slice_idx(),], !!!ra_ccs) %>%
            dplyr::mutate(color = ggplot2::alpha(colorsCB$not_selected, 0.1), selected = FALSE)

          out <- dplyr::add_row(.data = out, nbt_slice)

          # during drawing: highlight NEW drawing by increasing transparency of old selection
          if(drawing_active()){

            out <-
              dplyr::mutate(
                .data = out,
                color = dplyr::case_when(
                  selected | id %in% non_brain_template$id ~ ggplot2::alpha(color, 0.3),
                  TRUE ~ ggplot2::alpha(color, 0.6)
                  )
              )

          }

        }

        return(out)

      })

      slices_paintbrush <- shiny::reactive({

         c(
           (slice_idx()-paintbrush_radius()):(slice_idx()-1),
           slice_idx(),
           (slice_idx()+1):(slice_idx()+paintbrush_radius())
           )

      })

      shiny::observeEvent(mri_control_input$selection_tool, {

        if(!identical(x = selection_tool(), y = mri_control_input$selection_tool)){

          # selection_tool() is not updated yet, reset things if required
          if(stringr::str_detect(selection_tool(), pattern = "paintbrush")){

            data[[selection_tool()]] <- character()
            paintbrushed_ids(character(0))

          } else if(selection_tool() == "outline"){

            drawing_outline_confirmed({ data.frame() })

          }

          # update value
          selection_tool({ mri_control_input$selection_tool })

        }

      }, ignoreInit = TRUE)

      show_selection_state <- shiny::reactive({

        mode() %in% c("selection") &
        # if active paintbrushing, selection state is displayed dynamically by interaction plot
        !(stringr::str_detect(interaction_tool(), pattern = "paintbrush")  & drawing_active())

      })

      # ----- Observers

      # --- pass selection from controler
      shiny::observeEvent(mri_control_input$voxels_selected, {

        if(!identical(x = sort(plane_selection_state()), y = sort(mri_control_input$voxels_selected))){

          plane_selection_state({ mri_control_input$voxels_selected })

        }

      })


      # --- (de-)selection by region click
      # --- dblclick + region_click -> updates voxel_df() immediately
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "region_click" & !localizers_hover())

        # validity check
        if(!cursor_on_brain_tissue()){

          shiny::showNotification(
            ui = "Can not select non-brain tissue.",
            duration = 5,
            type = "error"
          )

          shiny::req(FALSE)

        }

        hvar <- selection_scope()
        brain_region <- slice_df_hover()[[hvar]]
        hemisphere <- slice_df_hover()[["hemisphere"]]

        # extract required data
        voxel_subset <-
          dplyr::filter(
            .data = voxel_df(),
            !!rlang::sym(hvar) == {{brain_region}} & hemisphere == {{hemisphere}}
          )

        # apply
        voxel_ids_area <- voxel_subset$id

        if(!slice_df_hover()$selected){

          plane_selection_state({

            dplyr::mutate(
              .data = voxel_df(),
              selected = id %in% {{voxel_ids_area}} | selected
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

        } else if(slice_df_hover()$selected){

          plane_selection_state({

            dplyr::mutate(
              .data = voxel_df(),
              selected = dplyr::if_else(id %in% {{voxel_ids_area}}, true = FALSE, false = selected)
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

        }

        slice_pos[[col_axis]] <- round(input$mriPlot_dblclick$x)
        slice_pos[[row_axis]] <- round(input$mriPlot_dblclick$y)
        slice_pos[[plane]] <- slice_idx()

      })

      # --- selection by outline
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "outline" & !localizers_hover())

        # initiate drawing logic for outline
        if(!drawing_active()){

          drawing_active({ TRUE })

          # from drawing to stop drawing: save results as selected voxels
        } else if(drawing_active()){

          if(nrow(drawing_outline()) > 3){

            btns_disabled_outline({ FALSE })

            for(outline_btn in c("confirm", "backward", "reset")){

              shiny::updateActionButton(
                session = session,
                inputId = paste0("outline_", outline_btn),
                disabled = btns_disabled_outline()
              )

            }

          }

          drawing_active({ FALSE })

        }

      })

      shiny::observeEvent(input$outline_confirm, {

        # confirm drawing outline
        if(nrow(drawing_outline_confirmed()) == 0){

          drawing_outline_confirmed({

            dplyr::mutate(drawing_outline(), slice = slice_idx())

            })

          shiny::updateActionButton(
            session = session,
            inputId = "outline_confirm",
            label = "Adjust",
            icon = shiny::icon(name = "wrench")
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_backward",
            disabled = TRUE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_focus",
            disabled = FALSE
          )

        } else {

          drawing_outline_confirmed({ data.frame() })

          shiny::updateActionButton(
            session = session,
            inputId = "outline_confirm",
            label = "Confirm",
            icon = shiny::icon(name = "check")
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_backward",
            disabled = FALSE
          )

          shiny::updateActionButton(
            session = session,
            inputId = "outline_focus",
            disabled = TRUE
          )

        }

      })

      shiny::observeEvent(input$outline_backward, {

        new_length <- round(nrow(drawing_outline())*0.9)

        data$outline$col <- data$outline$col[1:new_length]
        data$outline$row <- data$outline$row[1:new_length]

      })

      shiny::observeEvent(input$outline_reset, {

        data$outline$col <- numeric()
        data$outline$row <- numeric()

        drawing_outline_confirmed({ data.frame() })

        for(outline_btn in c("confirm", "backward", "reset", "focus")){

          if(outline_btn == "confirm"){

            icon <- shiny::icon(name = "check")

          } else {

            icon <- NULL

          }

          shiny::updateActionButton(
            session = session,
            inputId = paste0("outline_", outline_btn),
            disabled = TRUE,
            icon = icon
          )

        }

      })

      shiny::observeEvent(input$outline_focus, {

        slice_pos[[col_axis]] <- mean(data$outline$col)
        slice_pos[[row_axis]] <- mean(data$outline$row)

      })

      # --- selection by paintbrush
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "paintbrush" & !localizers_hover())

        # initiate drawing logic for selection + paintbrush
        if(!drawing_active()){

          if(drawing_on_diff_plane()){

            name <- tolower(mri_planes_pretty[mri_control_input$drawing_on_plane])

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Paintbrushing on different plane!",
              text = glue::glue(
                "You are currently paintbrushing on the {name} MRI. First, finish the
                drawing process by double-clicking on the {name} MRI. Then, decide what
                do with the drawing - confirm or reset. Then you can draw here."
              ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          if(pb_mask_on_diff_plane()){

            name <- tolower(mri_planes_pretty[names(mri_control_input$paintbrush_masks)])

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Drawing on different plane!",
              text = glue::glue(
                "There is a paintbrush drawing on the {name} MRI.
                Please confirm or reset it before paintbrushing on a different plane."
                ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          slice_pos[[col_axis]] <- round(cursor_pos()[1])
          slice_pos[[row_axis]] <- round(cursor_pos()[2])

          drawing_active({ TRUE })

        } else if(drawing_active()){

          drawing_active({ FALSE })

          if(length(data$paintbrush) != 0){

            btns_disabled_paintbrush({ FALSE })

          }

          slice_pos[[col_axis]] <- round(cursor_pos()[1])
          slice_pos[[row_axis]] <- round(cursor_pos()[2])

          paintbrushed_ids({ data$paintbrush })

        }

      }, ignoreInit = TRUE)

      # apply buttons
      shiny::observeEvent(input$paintbrush_confirm, {

        if(mri_control_input$paintbrush_mode == "Sphere"){

          plane_selection_state({

            apply_paintbrush_sphere(
              voxel_df = voxel_df(),
              cp_list = data$cursor_pos,
              plane = plane,
              slice = slice_idx(),
              radius = paintbrush_radius(),
              selection_scope = NULL,
              erase = FALSE
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

          data$cursor_pos <- list()

        } else if(mri_control_input$paintbrush_mode == "Ray"){

          plane_selection_state({

            propagate_selection_3D(
              voxel_df = voxel_df(),
              selection_mask = dplyr::mutate(selection_template(), selected = id %in% data$paintbrush),
              selection_plane = plane,
              selection_scope = selection_scope(),
              selection_slice = slice_idx(),
              selection_slices = selection_slices(),
              erase = FALSE
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

        }

        # disable buttons
        data$paintbrush <- character()
        paintbrushed_ids({ data$paintbrush })
        btns_disabled_paintbrush(TRUE)

      })

      shiny::observeEvent(input$paintbrush_backward, {

        n <- round(length(data$paintbrush)*0.9)

        data$paintbrush <- data$paintbrush[1:n]

      })

      shiny::observeEvent(input$paintbrush_reset, {

        data$cursor_pos <- list()
        data$paintbrush <- character()
        paintbrushed_ids(data$paintbrush)
        btns_disabled_paintbrush(TRUE)

      })

      # --- deselection by paintbrush
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "paintbrush_erase" & !localizers_hover())

        # initiate drawing logic for selection + paintbrush
        if(!drawing_active()){

          if(length(mri_control_input$voxels_selected) == 0){

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "No selection to erase",
              text = glue::glue(
                "You are trying to use the paintbrush erase tool - but there
                is no brain tissue selected that can be erased."
              ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          if(!any(selection_template()$selected)){

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "No selection to erase on this MRI slice!",
              text = glue::glue(
                "You are trying to use the paintbrush erase tool - but on this MRI slice there
                is no brain tissue selected that can be erased."
              ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          if(drawing_on_diff_plane()){

            name <- tolower(mri_planes_pretty[mri_control_input$drawing_on_plane])

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Paintbrushing (Erase) on different plane!",
              text = glue::glue(
                "You are currently paintbrushing on the {name} MRI. First, finish the
                drawing process by double-clicking on the {name} MRI. Then, decide what
                do with the drawing - confirm or reset. Then you can draw here."
              ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          if(pb_mask_on_diff_plane()){

            name <- tolower(mri_planes_pretty[names(mri_control_input$paintbrush_masks)])

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Drawing on different plane!",
              text = glue::glue(
                "There is a paintbrush drawing on the {name} MRI.
                Please confirm the deselection by clicking on Erase or reset it before drawing on a different plane."
              ),
              type = "info"
            )

            shiny::req(FALSE)

          }

          slice_pos[[col_axis]] <- round(cursor_pos()[1])
          slice_pos[[row_axis]] <- round(cursor_pos()[2])

          drawing_active(TRUE)

        } else if(drawing_active()){

          if(length(data$paintbrush_erase) != 0){

            btns_disabled_paintbrush_erase({ FALSE })

          }

          slice_pos[[col_axis]] <- round(cursor_pos()[1])
          slice_pos[[row_axis]] <- round(cursor_pos()[2])

          paintbrushed_ids({ data$paintbrush_erase })
          drawing_active({ FALSE })

        }

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$paintbrush_erase_confirm, {

        if(mri_control_input$paintbrush_mode == "Sphere"){

          plane_selection_state({

            apply_paintbrush_sphere(
              voxel_df = voxel_df(),
              cp_list = data$cursor_pos,
              plane = plane,
              slice = slice_idx(),
              radius = paintbrush_radius(),
              selection_scope = selection_scope(),
              erase = TRUE
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

          data$cursor_pos <- list()

        } else if(mri_control_input$paintbrush_mode == "Ray"){

          plane_selection_state({

            propagate_selection_3D(
              voxel_df = voxel_df(),
              selection_mask = dplyr::mutate(selection_template(), selected = id %in% data$paintbrush_erase),
              selection_plane = plane,
              selection_scope = selection_scope(),
              selection_slice = slice_idx(),
              selection_slices = selection_slices(),
              erase = TRUE
            ) %>%
              dplyr::filter(selected) %>%
              dplyr::pull(var = "id")

          })

        }

        # reset and disable buttons
        data$paintbrush_erase <- character(0)
        paintbrushed_ids(data$paintbrush_erase)
        btns_disabled_paintbrush_erase(TRUE)

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$paintbrush_erase_backward, {

        n <- round(length(data$paintbrush_erase)*0.9)
        data$paintbrush_erase <- data$paintbrush_erase[1:n]

      }, ignoreInit = TRUE)

      shiny::observeEvent(input$paintbrush_erase_reset, {

        data$cursor_pos <- list()
        data$paintbrush_erase <- character(0)
        paintbrushed_ids(data$paintbrush_erase)
        btns_disabled_paintbrush_erase(TRUE)

      }, ignoreInit = TRUE)

      # stop drawing when leaving the plot
      shiny::observeEvent(cursor_on_mri(), {

        if(drawing_active() & !cursor_on_mri()){

          drawing_active({ FALSE })

          if(selection_tool() == "paintbrush"){

            if(length(data$paintbrush) != 0){

              btns_disabled_paintbrush({ FALSE })

            }

            paintbrushed_ids({ data$paintbrush })

          } else if(selection_tool() == "paintbrush_erase"){

            if(length(data$paintbrush_erase) != 0){

              btns_disabled_paintbrush_erase({ FALSE })

            }

            paintbrushed_ids({ data$paintbrush_erase })
            drawing_active({ FALSE })

          }

        }

      })

      # 1. Color changes
      shiny::observeEvent(selection_tool(), {

        if(stringr::str_detect(selection_tool(), pattern = "paintbrush")){

          session$sendCustomMessage(ns("updateBrushColor"), selection_color())

        } else {

          session$sendCustomMessage(ns("updateBrushColor"), "transparent")

        }

      })

      # 2. Linetype changes
      shiny::observeEvent(drawing_active(), {

        session$sendCustomMessage(ns("updateBrushLinetype"), ifelse(drawing_active(), "solid", "dotted"))

      })

      # 3. Brush size changes
      shiny::observe({

        plot_width_px <- session$clientData[[paste0("output_", ns("mriInteractionPlot"), "_width")]]
        shiny::req(plot_width_px)

        size_px <- plot_width_px * (paintbrush_radius()*2 / mri_side_length())
        session$sendCustomMessage(ns("updateBrushSize"), size_px)

      })

      # LAST: Module Output -----------------------------------------------------

      paintbrushed_ids <- shiny::reactiveVal(value = character(0))

      module_output <- shiny::reactive({

        list(
          cursor_on_mri = cursor_on_mri(),
          drawing_active = drawing_active(),
          drawing_outline_confirmed = drawing_outline_confirmed(),
          paintbrushed_ids = paintbrushed_ids(),
          plane_selection_state = plane_selection_state(),
          slice_pos = slice_pos,
          voxels_quick_select = voxels_quick_select()
        )

        })

      return(module_output)

    }
  )

}
