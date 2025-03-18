
colors <- c("green",
            "blue",
            "yellow",
            "red",
            "orange",
            "purple",
            "brown")

moduleMriPlaneUI <- function(id,
                             plane,
                             title = "MRI",
                             slider_min = 1,
                             slider_max = 256,
                             pd = 5, # padding
                             siamp = 30, # size items around mri plot
                             showValue = TRUE,
                             wmp = 500, # width mri plot
                             cf = F) {

  ns <- shiny::NS(id)

  bgc <- function(cf){

    if(cf){

      sample(colors, size = 1) %>%
        paste0(., "; border: 1px solid black;")

    } else {

      "white"

    }

  }

  # size around mri plot -> add padding
  samp <- pd*2 + siamp

  # width mri container
  wmc <- wmp + samp

  # HTML + CSS
  {shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(
        shiny::HTML(
        "$(document).ready(function(){
         $('[data-toggle=\"tooltip\"]').tooltip();});"
        )
      ),
      shiny::tags$style(
        shiny::HTML(
          glue::glue("
                     .CB-control-group {
                      display: flex;
                      flex-direction: column;
                      align-items: center;
                      justify-content: flex-start;
                      gap: 10px; /* Adds spacing between checkbox group and buttons */
                    }

                    /* Checkbox Group (Forces Vertical Alignment) */
                    .CB-checkbox-group {
                      display: flex;
                      flex-direction: column;
                      align-items: center;
                      justify-content: flex-start;
                      width: auto;
                    }

                    /* Ensure each button inside the checkbox group is aligned properly */
                    .CB-checkbox-group .btn-group {
                      flex-direction: column !important;  /* Force buttons to stack vertically */
                      display: flex !important;
                      align-items: center !important;
                      width: auto !important;
                    }

                    .CB-checkbox-group .btn {
                      width: {{siamp}}px !important;  /* Square button width */
                      height: {{siamp}}px !important; /* Square button height */
                      padding: 0 !important;
                      display: flex;
                      align-items: center;
                      justify-content: center;
                      border: 1px solid #ccc;
                      transition: all 0.2s ease-in-out
                    }

                    /* Change background color when selected */
                    .CB-checkbox-group .btn.active {
                      background-color: steelblue !important;
                      color: white !important;
                      border-color: steelblue !important;
                    }

                    /* Ensure icons inside remain white when selected */
                    .CB-checkbox-group .btn.active i {
                      color: white !important;
                    }

                    /* Action Buttons Group */
                    .CB-btn-group {
                      display: flex;
                      flex-direction: column;
                      align-items: center;
                      gap: 2.5px; /* Adds spacing between buttons */
                    }

                    /* Adjust button styles to ensure they remain square and aligned */
                    .CB-btn {
                      width: {{siamp}}px !important;  /* Square button width */
                      height: {{siamp}}px !important; /* Square button height */
                      padding: 0 !important;
                      display: flex;
                      align-items: center;
                      justify-content: center;
                      border: 1px solid #ccc;
                      background-color: rgba(200, 200, 200, 0.2);
                      font-size: 18px;
                      cursor: pointer;
                      transition: all 0.2s ease-in-out;
                    }

                    .CB-btn-slider {
                      width: {{siamp}}px !important;  /* Square button width */
                      height: {{siamp}}px !important; /* Square button height */
                      padding: 0 !important;
                      display: flex;
                      align-items: center;
                      justify-content: center;
                      border: 1px solid #ccc;
                      border-radius: 5px;
                      background-color: rgba(200, 200, 200, 0.2);
                      font-size: 18px;
                      cursor: pointer;
                      transition: all 0.2s ease-in-out;
                    }

                    /* Hover Effect */
                    .CB-btn:hover {
                      background-color: rgba(200, 200, 200, 0.5);
                    }

                    .btn-dropdownbutton{
                      width: {{siamp}}px;
                      height: {{siamp}}px;
                      padding: 0;
                      display: flex;
                      align-items: center;
                      justify-content: center;
                      border: 1px solid #ccc;
                      border-radius: 5px;
                      background-color: rgba(200, 200, 200, 0.2);
                      font-size: 18px;
                      cursor: pointer;
                      transition: all 0.2s ease-in-out;
                      }


                    /* Hide default min/max labels */
                    .irs-min, .irs-max { display: none !important; }

                    /* Custom labels above the slider */
                    .slider-container {
                      display: flex;
                      flex-direction: column-reverse; /* Moves labels above the slider */
                      align-items: center;
                      width: 100%;
                    }

                    .slider-labels {
                      display: flex;
                      justify-content: space-between;
                      width: 100%;
                      font-weight: bold;
                      color: grey; /* Grey label color */
                      margin-bottom: -15px; Space between label and slider */
                    }
                     ",
          .open = "{{", .close = "}}")
        )
      )
    ),
    shiny::div( # MRI Box
      style =
        glue::glue("
                    background-color: white;
                    border-radius: 10px;
                    box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
                    display: flex;
                    flex-direction: column;
                    padding: 5px;
                    width: {wmc+10}px;
                    height:100%;"),
      #shiny::actionButton(ns("test"), label= "Test"),
      shiny::div( # Above MRI Plot
        style = glue::glue("
                      display: flex;
                      align-items: flex-end;
                      justify-content: center;
                      height: {samp*1.5}px;
                      width: {wmc}px;
                      margin-bottom: -10px;
                      background-color: {bgc(cf)}"),
        shiny::div(
          style = glue::glue("
                             display: flex;
                             align-itmes: flex-end;
                             justify-content: center;
                             height: {samp};
                             width: {samp*1.5};
                             margin: 10px;
                             margin-left: 5px;
                             margin-bottom: 20px;
                             background-color: {bgc(cf)};
                             gap: 2.5px;"),
            shiny::actionButton(
              inputId = ns("zoom_in"),
              label = NULL,
              class = "CB-btn-slider",
              icon = shiny::icon("search-plus")
            ),
            shiny::actionButton(
              inputId = ns("zoom_out"),
              label = NULL,
              class = "CB-btn-slider",
              icon = shiny::icon("search-minus")
            )
        ),
        shiny::div(
          style = glue::glue("
                        display: flex;
                        justify-content: flex-end;
                        width: {samp}px;
                        margin: 10px;
                        margin-bottom: 20px;
                        background-color: {bgc(cf)}"),
          shiny::actionButton(
            inputId = ns("mri_backward"),
            label = NULL,
            class = "CB-btn-slider",
            icon = shiny::icon("step-backward")
          )
        ),
        shiny::div(
          style = glue::glue("
                        /*flex-grow: 1;  Allow slider to take remaining space */
                        display: flex;
                        justify-content: center;
                        padding-top:5px;
                        background-color: {bgc(cf)}"),
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
        shiny::div(
          style = glue::glue("
                        display: flex;
                        justify-content: flex-start;
                        width: {samp*2}px;
                        margin: 10px;
                        margin-bottom: 20px;
                        background-color: {bgc(cf)};
                        gap: 10px;"),
          shiny::actionButton(
            inputId = ns("mri_forward"),
            label = NULL,
            class = "CB-btn-slider",
            icon = shiny::icon("step-forward")
          ),
          shinyWidgets::dropdownButton(
            status = "dropdownbutton",
            inputId = ns("mri_opts_dropdown"),
            icon = shiny::icon("gear"),
            circle = FALSE,
            size = "sm",
            tooltip = "MRI Options",
            # dropdown content
            shiny::div( # Checkbox Buttons
              class = "CB-checkbox-group",
              shinyWidgets::checkboxGroupButtons(
                inputId = ns("mri_opts"),
                label = NULL,
                selected = c("localizer_lines", "link", "hover_show"),
                choices = c(
                  '<i class="fas fa-link" style="font-size: 1.5em;" data-toggle="tooltip" title="Link MRI"></i>' = "link",
                  #'<i class="fas fa-ruler-horizontal" style="font-size: 1.5em;" data-toggle="tooltip" title="Scale Bar"></i>' = "scale_bar" # not necessary?
                  '<i class="fas fa-plus" style="font-size: 2em;" data-toggle="tooltip" title="MRI-Localizer"></i>' = "localizer_lines",
                  '<i class="fas fa-mouse-pointer" style="font-size: 1.5em;" data-toggle="tooltip" title="Tissue-Label"></i>' = "hover_show"
                ),
                direction = "vertical",
                justified = FALSE,
                individual = FALSE,
                size = "sm",
                width = "100%"
              )
            )
          )
        )
      ),

      shiny::div( # Buttons + MRI Plot + Vertical Slider
        style = glue::glue("
                            display: flex;
                            flex-direction: row;
                            height: {wmp}px;
                            width: {wmc}px;"),
        shiny::div( # MRI Plot
          style = glue::glue("
                                  display: flex;
                                  width: {wmp}px;
                                  height: {wmp}px"),
          class = "multiple-plots",
          shiny::tags$style(
            glue::glue(".multiple-plots { position: relative; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriSlicePlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriSelectionStatePlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriVisualAidPlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriLocalizerPlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriHoverInfoPlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriOutlinePlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriPaintbrushPlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }
                        #{{ns('mriInteractionPlot')}} { position: absolute; width: {{wmp}}px; height: {{wmp}}px; }", # always top
                       .open = "{{",
                       .close = "}}")
          ),
          shiny::plotOutput(
            outputId = ns("mriSlicePlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriSelectionStatePlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),

          shiny::plotOutput(
            outputId = ns("mriVisualAidPlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriLocalizerPlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriHoverInfoPlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriPaintbrushPlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriOutlinePlot"),
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px")
          ),
          shiny::plotOutput(
            outputId = ns("mriInteractionPlot"), # always top layer
            height = glue::glue("{wmp}px"),
            width = glue::glue("{wmp}px"),
            click = shiny::clickOpts(id = ns("mriPlot_click"), clip = TRUE),
            brush = shiny::brushOpts(id = ns("mriPlot_brush"), delayType = "debounce", resetOnNew = TRUE),
            dblclick = shiny::dblclickOpts(id = ns("mriPlot_dblclick"), clip = TRUE),
            hover = shiny::hoverOpts(id = ns("mriPlot_hover"), delay = 100, delayType = "throttle", clip = TRUE)
          )
        ),
        shiny::div( # Vertical Slider
          style =
            glue::glue("
                        display: flex;
                        justify-content: center;
                        width: {samp}px;
                        height: {wmp}px;
                        background-color: {bgc(cf)};"),
          shinyWidgets::noUiSliderInput(
            inputId = ns("mri_slider_row"),
            label = NULL,
            min = slider_min,
            max = slider_max,
            step = 1,
            value = round(slider_max/2),
            orientation = "vertical",
            direction = "ltr",
            update_on = "end",
            color = "steelblue",
            height = glue::glue("{wmp}px"),
            tooltips = FALSE
          )
        )
      ),
      shiny::div( # Below MRI Plot
        style = glue::glue("
                            display: flex;
                            flex-direction: row;
                            width: {wmc};
                            height: {samp};
                            background-color: black"),
        shiny::div( # Horizontal Slider
          style = glue::glue("
                              display: flex;
                              align-items: center;
                              justify-content: center;
                              height: {samp}px;
                              width: {wmp}px;
                              padding-top:{pd*1.5}px;
                              background-color: {bgc(cf)}"),
          shinyWidgets::noUiSliderInput(
            inputId = ns("mri_slider_col"),
            label = NULL,
            min = slider_min,
            max = slider_max,
            step = 1,
            value = round(slider_max/2),
            orientation = "horizontal",
            direction = "ltr",
            update_on = "end",
            color = "steelblue",
            width = "100%",
            tooltips = FALSE
          )
        ),
        shiny::div( # empty
          style = glue::glue("
                              display: flex;
                              width: {samp}px;
                              height: {samp}px;
                              background-color: {bgc(cf)}")
        )
      ),
      shiny::fluidRow(
        shiny::uiOutput(outputId = ns("options_bottom"))
      )
    )
  )}
}

#' @param id The module id.
#' @param plane The plane in which the MRI data set is represented.
#' @param mri_list Named list of length three (names: sag, axi, cor). Each
#' slot corresponds to a list of raster matrices which in turn corresponds
#' to a MRI slice of the respective plane. Each value in each slice corresponds
#' to a color in HEX code for the respective pixel.
#' @param interaction_templates A list of length n, where n corresponds
#' to the number of slices of the respective plane represented by this module instance.
#' Each slot is a data.frame (variables: col, row, id, selected). ID corresponds
#' to the voxel ID in the global voxel data.frame.
#' @param voxel_df The global 3D representation of the MRI data set. Each
#' row corresponds to a voxel.
#' @param slice_sag,slice_axi,slice_cor Numeric value that indicate the currently
#' plotted slice of the respective plane. Provided as reactive expressions -
#' should be provided from the output of the connected MRI control module.
#' @param mri_control The output of the connected MRI control module. Provided
#' as a reactive expression.
#'
#'
#' @section Module Modes:
#'
#' **Selection**
#' Mode in which the objective is to interactively select voxels in one
#' slice or across multiple slices. The objective of the selection process can
#' vary and is managed by the MRI control module. The result of the selection
#' process (all selected voxels across all slices) is returned as a character
#' vector of voxel IDs upon every click on `input$selection_confirm`.

moduleMriPlaneServer <- function(id,
                                 plane,
                                 nifti_input,
                                 mri_control,
                                 mode = function(){ "inspection" },
                                 mode_init = "inspection"
                                 ){

  # define once
  axis_ccs <- switch_axis_label(plane)

  ra_ccs <- req_axes_2d(plane)
  ra_mri <- req_axes_2d(plane, mri = TRUE)

  col_axis <- ra_mri["col"]
  row_axis <- ra_mri["row"]

  # account for inversed y-representation of the axial row slider
  #voxel_df[[ra_ccs["row"]]] <- (256 - voxel_df[[ra_ccs["row"]]])+1

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      # ----- Debugging/Testing
      shiny::observeEvent(input$test,{

        #assign(x = "data", value = reactiveValuesToList(data), envir = .GlobalEnv)
        print("-----------------------")
        print("Test")
        voxel_df({ ctp_df })

      }, ignoreInit = TRUE)

      shiny::observe({

      })



      # 1. Global ---------------------------------------------------------------


      # Dynamic UI --------------------------------------------------------------

      output$options_bottom <- shiny::renderUI({

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
                disabled = outline_btns_disabled()
              ),
              shiny::actionButton(
                inputId = ns("outline_backward"),
                label = "Backwards",
                icon = shiny::icon(name = "step-backward"),
                width = "100%",
                disabled = outline_btns_disabled()
              ),
              shiny::actionButton(
                inputId = ns("outline_reset"),
                label = "Reset",
                icon = shiny::icon(name = "trash"),
                width = "100%",
                disabled = outline_btns_disabled()
              ),
              div(),  # Empty div creates spacing
              shiny::actionButton(
                inputId = ns("outline_focus"),
                label = "Focus",
                icon = shiny::icon(name = "crosshairs"),
                width = "100%",
                disabled = outline_btns_disabled()
              )
            )
          )

        } else if(selection_tool() == "paintbrush"){

          shiny::column(
            offset = 1,
            width = 10,
            align = "center",
            shiny::helpText("Double click to start brushing. Double click again to stop and confirm.")
          )

        } else if(selection_tool() == "safety_margin"){

          shiny::column(
            offset = 1,
            width = 10,
            align = "center",
            shiny::helpText("Use the slider to adjust the margin.")
          )

        }

      })


      # Reactive Values ---------------------------------------------------------

      stacks <- shiny::reactiveValues(zoom = list())

      drawing_active <- shiny::reactiveVal(value = FALSE)

      hover_show <- shiny::reactive({

        shiny::req(input$mri_opts)

        if(mode() == "inspection"){

          out <- "hover_show" %in% input$mri_opts

        } else {

          out <-
            "hover_show" %in% input$mri_opts &
            !stringr::str_detect(interaction_tool(), pattern = "outline|paintbrush")

        }

        return(out)

      })

      mri_range <- shiny::reactive({

        shiny::req(stacks)

        if(length(stacks$zoom) == 0){

          shiny::req(nifti_input())

          col_range <- c(1,dim(nifti_input())[which(mri_planes == unname(col_axis))])
          row_range <- c(1,dim(nifti_input())[which(mri_planes == unname(row_axis))])

          out <- list(col = col_range, row = row_range)

        } else {

          out <- dplyr::last(x = stacks$zoom)

        }

        return(out)

      })

      mode <- shiny::reactiveVal(mode_init)

      outline_btns_disabled <- shiny::reactiveVal(value = TRUE)

      reselect <- shiny::reactiveVal(value = "")

      slice_pos <- shiny::reactiveValues(sag = 128, axi = 128, cor = 128)

      # is immediately updated by the controller input
      voxel_df <- shiny::reactiveVal(data.frame())

      shiny::observeEvent(mri_control()$voxel_df, {

        if(!identical(x = shiny::isolate({ voxel_df() }), y = mri_control()$voxel_df)){

          voxel_df({ mri_control()$voxel_df })

        }

      })

      # Reactive Expressions ----------------------------------------------------

      cursor_on_brain_tissue <- shiny::reactive({ nrow(slice_df_hover()) != 0 })
      cursor_on_mri <- shiny::reactive({ shiny::isTruthy(input$mriPlot_hover )})
      cursor_pos <- shiny::reactive({ c(input$mriPlot_hover$x, input$mriPlot_hover$y) })

      col_min <- shiny::reactive({ min(mri_range()[["col"]]) })
      col_max <- shiny::reactive({ max(mri_range()[["col"]]) })
      col_seq <- shiny::reactive({ col_min():col_max() })

      erase <- shiny::reactiveVal(TRUE) #!!!!

      hover_text <- shiny::reactive({

        shiny::req(cursor_pos())

        if(shiny::isTruthy(slice_df_hover()) & !drawing_active()){

          shiny::req(slice_idx())

          # Ensure data exists after filtering
          if(nrow(slice_df_hover()) == 0) {

            text <- c("No brain tissue")

          } else {

            text <-
              make_pretty_label(slice_df_hover()[[hover_var()]]) %>%
              paste0(" (", slice_df_hover()[["tissue_type"]], ")")

          }

        } else {

          text <- ""

        }

        return(text)

      })

      hover_var <- shiny::reactive({ "ann_dk_adj" })

      interaction_color <- shiny::reactive({

        if(mode() == "selection"){

          selection_color()

        }

      })

      interaction_dims <- shiny::reactive({ mri_control()$interaction_dims })

      interaction_erase <- shiny::reactive({

        if(mode() == "selection"){

          selection_erase()

        }

      })

      # interaction data.frame of the current slice
      interaction_template <- shiny::reactive({

        if(mode() == "selection"){

          selection_template()

        }

      })

      interaction_tool <- shiny::reactive({

        if(mode() == "selection"){

          selection_tool()

        }

      })

      mri_slice <- shiny::reactive({

        shiny::req(nifti_input())
        shiny::req(slice_idx())

        get_slice(nifti_input(), plane = plane, slice = slice_idx())

        })

      # (col and row are always equally long - pick col)
      mri_side_length <- shiny::reactive({ as.numeric(dist(mri_range()[["col"]])) })

      n_slices <- shiny::reactive({ dim(nifti_input())[which(c("sag", "cor", "axi") == plane)] })

      paintbrush_radius <- shiny::reactive({ mri_side_length() * paintbrush_radius_fct() })
      paintbrush_radius_fct <- shiny::reactive({ mri_control()$pbr_fct })
      paintbrush_selected <- shiny::reactive({ interaction_template()$id %in% paintbrush_selected_ids() })
      paintbrush_selected_ids <- shiny::reactive({ data$paintbrush })

      row_min <- shiny::reactive({ min(mri_range()[["row"]]) })
      row_max <- shiny::reactive({ max(mri_range()[["row"]]) })
      row_seq <- shiny::reactive({ row_min():row_max() })

      selection3D <- shiny::reactive({

        all({

          purrr::map_lgl(
            .x = c("x", "y", "z"),
            .f = function(axis){

              slices <- dplyr::filter(voxel_df(), selected)[[axis]]

              dplyr::n_distinct(slices) > 1

            }
          )

        })


      })

      slice_axi <- shiny::reactive({ mri_control()$slice_state$axi })
      slice_cor <- shiny::reactive({ mri_control()$slice_state$cor })
      slice_sag <- shiny::reactive({ mri_control()$slice_state$sag })

      slice_df <- shiny::reactive({ voxel_df()[voxel_df()[[axis_ccs]] == slice_idx(),] })
      slice_df_hover <- shiny::reactive({

        shiny::req(!drawing_active())
        shiny::req(slice_df())

        # circumevent without req()!
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
      slice_idx <- shiny::reactive({ slice_pos[[plane]] })


      # ----- Observers

      # Observe Events ----------------------------------------------------------

      shiny::observeEvent(mri_control()$mode, {

        if(!identical(x = mri_control()$mode, y = mode())){

          mode({ mri_control()$mode })

        }

      })

      # --- automatically de-select localizers if on MRI
      shiny::observeEvent(cursor_on_mri(), { #!!!!global options?

        shiny::req(mode() == "selection")

        # if TRUE, trigger = cursor enters mri plot
        if(cursor_on_mri()){

          if("localizer_lines" %in% input$mri_opts){

            reselect(c(reselect(), "localizer_lines"))

            shinyWidgets::updateCheckboxGroupButtons(
              session = session,
              inputId = "mri_opts",
              selected = input$mri_opts[!input$mri_opts == "localizer_lines"]
            )

          }

          # if TRUE, trigger = cursor leaves mri plot
        } else if(!cursor_on_mri()){

          # reselect localizer lines if required
          if("localizer_lines" %in% reselect()){

            shinyWidgets::updateCheckboxGroupButtons(
              session = session,
              inputId = "mri_opts",
              selected = c(input$mri_opts, "localizer_lines")
            )

            reselect(reselect()[!reselect() %in% "localizer_lines"])

          }

        }

      })

      # --- drawing logic
      shiny::observeEvent(cursor_pos(),{

        shiny::req(mode() != "inspection")

        if(drawing_active() & interaction_tool() == "outline"){

          data$outline$col <- c(data$outline$col, cursor_pos()[1])
          data$outline$row <- c(data$outline$row, cursor_pos()[2])

        } else if(drawing_active() & stringr::str_detect(interaction_tool(), pattern = "paintbrush")){

          if(!interaction_erase()){

            data$paintbrush <-
              c(
                data$paintbrush,
                identify_obs_within_radius(
                  cursor_pos = cursor_pos(),
                  radius = paintbrush_radius(),
                  interaction_template = interaction_template(),
                  preselected_ids = data$paintbrush
                )
              )

          } else if(interaction_erase()) { # do not select but erase

            voxels_erase <-
              identify_obs_within_radius(
                cursor_pos = cursor_pos(),
                radius = paintbrush_radius(),
                interaction_template = interaction_template()
              )

            data$paintbrush <- setdiff(x = data$paintbrush, y = voxels_erase)

          }

        }

      })

      # --- update mode if it has changed within the controller
      shiny::observeEvent(mri_control()$mode, {

        if(!identical(mode(), mri_control()$mode)) {

          mode(mri_control()$mode)

        }

      }, ignoreNULL = TRUE)

      # adjust outline from the controler side
      shiny::observeEvent(mri_control()$outlines, {
print("outlines usage")
        print(mri_control()$outlines)
        plane_outline <-  mri_control()$outlines[[plane]]

        # use to reset everything
        if(nrow(plane_outline) == 0){

          data$outline$col <- numeric()
          data$outline$row <- numeric()

          drawing_outline_confirmed <- data.frame(col = numeric(), row = numeric())

        # use to adjust (currently not required)
        } else if(!identical(x = mri_control()$outlines[[plane]], drawing_outline_confirmed())){

          data$outline$col <- plane_outline$col
          data$outline$row <- plane_outline$row

          if(nrow(drawing_outline_confirmed()) != 0){

            drawing_outline_confirmed(plane_outline)

          }

        }

      }, ignoreInit = TRUE)


      # --- forward/backward
      shiny::observeEvent(input$mri_forward,{

        if(slice_pos[[plane]] != n_slices()){

          slice_pos[[plane]] <- slice_pos[[plane]]+1

        }

      })

      shiny::observeEvent(input$mri_backward,{

        if(slice_pos[[plane]] != 1){

          slice_pos[[plane]] <- slice_pos[[plane]]-1

        }

      })

      # --- zoom in
      shiny::observeEvent(input$zoom_in,{

        fct <- 0.05
        # new mri range
        nmr <-
          purrr::map(
            .x = mri_range(),
            .f = function(r){

              d <- as.numeric(dist(r))
              zoom <- d*fct

              r[1] <- round(r[1]+zoom)
              r[2] <- round(r[2]-zoom)

              return(r)

            }
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

        # stack zoom ranges
        stacks <- add_to_stack(stacks, which = "zoom", what = nmr)

      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      shiny::observeEvent(input$mriPlot_brush, {

        nmr <-
          list(
            col =
              c(floor(input$mriPlot_brush$xmin), ceiling(input$mriPlot_brush$xmax)),
            row =
              c(floor(input$mriPlot_brush$ymin), ceiling(input$mriPlot_brush$ymax))
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

        # stack zoom ranges
        stacks <- add_to_stack(stacks, which = "zoom", what = nmr)

      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # --- zoom out
      shiny::observeEvent(input$zoom_out,{

        # zoom stacks remaining?
        if(length(stacks$zoom) != 0){

          stacks <- reduce_stack(stacks, which = "zoom")

          # update localizer
          shinyWidgets::updateNoUiSliderInput(
            session = session,
            inputId = "mri_slider_row",
            range = dplyr::last(stacks$zoom)$row
          )

          shinyWidgets::updateNoUiSliderInput(
            session = session,
            inputId = "mri_slider_col",
            range = dplyr::last(stacks$zoom)$col
          )

        }

      }, ignoreInit = TRUE)

      # --- updates of slice_pos from the controller side
      shiny::observeEvent(slice_sag(), {

        shiny::req("link" %in% input$mri_opts)

        if(!is.null(slice_sag()) && shiny::isolate({slice_pos$sag}) != slice_sag()) {
          slice_pos$sag <- slice_sag()
        }

      })

      shiny::observeEvent(slice_axi(), {

        shiny::req("link" %in% input$mri_opts)

        if(!is.null(slice_axi()) && shiny::isolate({slice_pos$axi}) != slice_axi()) {
          slice_pos$axi <- slice_axi()
        }

      })

      shiny::observeEvent(slice_cor(), {

        shiny::req("link" %in% input$mri_opts)

        if(!is.null(slice_cor()) && shiny::isolate({slice_pos$cor}) != slice_cor()) {
          slice_pos$cor <- slice_cor()
        }

      })

      # Observe Events (Internal Slider Updates) --------------------------------

      shiny::observeEvent(input$mri_slider_col,{

        if(input$mri_slider_col != shiny::isolate(slice_pos[[col_axis]])){

          slice_pos[[col_axis]] <- input$mri_slider_col

        }

      })

      shiny::observeEvent(input$mri_slider_row,{

        if(input$mri_slider_row != shiny::isolate({slice_pos[[row_axis]]})){

          slice_pos[[row_axis]] <- input$mri_slider_row

        }

      })

      shiny::observeEvent(input$mri_slider_slice,{

        slice_pos[[plane]] <- input$mri_slider_slice

      })

      # Observe Events (Slice Pos Effect) ---------------------------------------

      # - plane
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

      # - col & row
      if(plane == "sag"){

        shiny::observeEvent(slice_pos$axi, {

          shiny::req(input$mri_slider_row)

          if(shiny::isolate({input$mri_slider_row}) != slice_pos$axi){

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_row",
              value = slice_pos$axi
            )

          }

        })

        shiny::observeEvent(slice_pos$cor, {

          shiny::req(input$mri_slider_col)

          if(shiny::isolate({input$mri_slider_col}) != slice_pos$cor) {

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_col",
              value = slice_pos$cor
            )

          }

        })

      } else if(plane == "axi") {

        shiny::observeEvent(slice_pos$cor, {

          shiny::req(input$mri_slider_row)

          if(shiny::isolate({input$mri_slider_row}) != slice_pos$cor) {

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_row",
              value = slice_pos$cor
            )

          }

        })

        shiny::observeEvent(slice_pos$sag, {

          shiny::req(input$mri_slider_col)

          if(shiny::isolate({input$mri_slider_col}) != slice_pos$sag){

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_col",
              value = slice_pos$sag
            )

          }

        })

      } else if (plane == "cor") {

        shiny::observeEvent(slice_pos$axi, {

          shiny::req(input$mri_slider_row)

          if(shiny::isolate({input$mri_slider_row}) != slice_pos$axi) {

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_row",
              value = slice_pos$axi
            )

          }
        })

        shiny::observeEvent(slice_pos$sag, {

          shiny::req(input$mri_slider_col)

          if(shiny::isolate({input$mri_slider_col}) != slice_pos$sag) {

            shinyWidgets::updateNoUiSliderInput(
              session = session,
              inputId = "mri_slider_col",
              value = slice_pos$sag
            )

          }

        })

      }


      # Outputs (Plots) ---------------------------------------------------------

      output$mriInteractionPlot <- shiny::renderPlot({

        plot_mri_frame(col = col_seq(), row = row_seq())

      }, bg = "transparent")

      output$mriPaintbrushPlot <- shiny::renderPlot({

        shiny::req(cursor_pos() & stringr::str_detect(selection_tool(), "paintbrush"))

        # plotting context for all drawing options
        plot_mri_frame(col = col_seq(), row = row_seq())

        if(shiny::isTruthy(cursor_pos())){

          symbols(
            x = cursor_pos()[1],
            y = cursor_pos()[2],
            circles = paintbrush_radius(),
            inches = FALSE,
            add = TRUE,
            fg = interaction_color(),
            lwd = 2,
            lty = ifelse(drawing_active(), "solid", "dotted")
          )

        }

        # paintbrush_selected() is updated live during paintbrushing and
        # corresponds to a logical vector indicating is selected or not
        # (paintbrushing is initiated with all selected_voxels() regardless
        # of how they have been selected during the selection progress)
        if(drawing_active()){

          graphics::rect(
            xleft = interaction_template()[paintbrush_selected(),]$col - 0.5,
            xright = interaction_template()[paintbrush_selected(),]$col + 0.5,
            ybottom = interaction_template()[paintbrush_selected(),]$row - 0.5,
            ytop = interaction_template()[paintbrush_selected(),]$row + 0.5,
            col = interaction_template()[paintbrush_selected(),]$color,
            border = NA
          )

        }

      }, bg = "transparent")

      border_color <- shiny::reactive({

        if(nrow(drawing_outline_confirmed()) != 0){

          selection_color()

        } else {

          NA

        }

      })

      output$mriOutlinePlot <- shiny::renderPlot({

        shiny::req(nrow(drawing_outline()) != 0)

        plot_mri_frame(
          col = drawing_outline()$col,
          row = drawing_outline()$row,
          type = "l",
          lwd = 3,
          color = selection_color(),
          xlim = range(col_seq()),
          ylim = rev(range(row_seq())),
          bg = NA
        )

        if(!drawing_active()){

          graphics::polypath(
            x = drawing_outline()$col,
            y = drawing_outline()$row,
            col = alpha(selection_color(), 0.15),
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
        if(hover_show()){

          shiny::req(hover_text())

          x_pos <- min(mri_range()[["col"]]) + side_length*0.0125
          y_pos <- max(mri_range()[["row"]]) - side_length*0.0125

          bg_colors <-
            mri_slice()[min(mri_range()[["col"]]):x_pos, max(mri_range()[["row"]]):y_pos] %>%
            as.character()

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

        shiny::req(!cursor_on_mri())
        shiny::req("localizer_lines" %in% input$mri_opts)

        plot_mri_frame(col = col_seq(), row = row_seq())

        if(plane == "sag"){

          # vertical
          segments(
            x0 = slice_pos$cor, x1 = slice_pos$cor,
            y0 = row_min(), y1 = row_max(),
            col = "red", lwd = 1.5
          )

          # horizontal
          segments(
            x0 = col_min(), x1 = col_max(),
            y0 = slice_pos$axi, y1 = slice_pos$axi,
            col = "red", lwd = 1.5
          )

        } else if(plane == "axi"){

          # vertical
          segments(
            x0 = slice_pos$sag, x1 = slice_pos$sag,
            y0 = row_min(), y1 = row_max(),
            col = "red", lwd = 1.5
          )

          # horizontal
          segments(
            x0 = col_min(), x1 = col_max(),
            y0 = slice_pos$cor, y1 = slice_pos$cor,
            col = "red", lwd = 1.5
          )

        } else if(plane == "cor"){

          # vertical
          segments(
            x0 = slice_pos$sag, x1 = slice_pos$sag,
            y0 = row_min(), y1 = row_max(),
            col = "red", lwd = 1.5
          )

          # horizontal
          segments(
            x0 = col_min(), x1 = col_max(),
            y0 = slice_pos$axi, y1 = slice_pos$axi,
            col = "red", lwd = 1.5
          )

        }

      }, bg = "transparent")

      output$mriVisualAidPlot <- shiny::renderPlot({

        shiny::req(input$mri_opts)
        shiny::req(mri_range())
        side_length <- mri_side_length()

        # -- initiate plot
        plot_mri_frame(col = col_seq(), row = row_seq())

        # -- scale bar (currently not active)
        if("scale_bar" %in% input$mri_opts){

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
        label <- paste0("Mode: ", stringr::str_to_title(mode()))

        if(shiny::isTruthy(interaction_tool())){

          # plot paintbrush size if required
          if(stringr::str_detect(interaction_tool(), pattern = "paintbrush")){

            symbols(
              x = max(mri_range()[["col"]]) - (side_length*0.0125) - paintbrush_radius(),
              y = max(mri_range()[["row"]]) - (side_length*0.0125) - paintbrush_radius(),
              circles = paintbrush_radius(),
              inches = FALSE,
              add = TRUE,
              fg = interaction_color(),
              lwd = 3,
              lty = ifelse(drawing_active(), "solid", "dotted")
            )

          }

          # update label

          label_interaction <-
            paste0(
              "\nTool: ",
              stringr::str_remove_all(interaction_tool(), pattern = "erase$") %>%
               stringr::str_to_title() %>%
               stringr::str_replace_all(string = ., pattern = "_", replacement = " ")
            )

          if(interaction_erase()){

            label_interaction <- paste0(label_interaction, " - Erase")

          }

          label <- paste0(label, label_interaction)

        }

        x_pos <- max(mri_range()[["col"]]) - (side_length*0.0125)
        y_pos <- min(mri_range()[["row"]]) + (side_length*0.05)

        # dist text
        graphics::text(
          x = x_pos,
          y = y_pos,
          labels = label,
          col = "white",
          cex = 1,
          font = 2,
          adj = c(1, 0.5)
        )


      }, bg = "transparent")

      output$mriSelectionStatePlot <- shiny::renderPlot({

        shiny::req(show_selection_state())

        if(selection_tool() == "margin"){

          voxels_show <- dplyr::filter(interaction_template(), selected | is_margin_cand)

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

      output$mriSlicePlot <- shiny::renderPlot({

        shiny::req(mri_slice())

        plot_mri_frame(col = col_seq(), row = row_seq())

        graphics::rasterImage(
          image = mri_slice()[row_seq(), col_seq()],
          xleft = col_min(),
          xright = col_max(),
          ybottom = row_max(),
          ytop = row_min(),
          interpolate = FALSE
        )

      })



      # Output ------------------------------------------------------------------


      # 2. Mode: Selection ------------------------------------------------------

      # ----- Reactive Values

      # empty flexible data container for temporary progress
      data <-
        shiny::reactiveValues(
          outline = list(col = numeric(0), row = numeric(0)),
          paintbrush = character()
        )

      drawing_outline_confirmed <- shiny::reactiveVal(value = data.frame(col = numeric(), row = numeric()))

      selection_backward_count <- shiny::reactiveVal(value = 0)

      # is updated upon dbl_clicking when in selection mode to end drawing mode
      selection_mask <- shiny::reactiveVal(value = data.frame())

      selection_tool <- shiny::reactiveVal(value = "region_click")

      # ----- Reactive (Expressions)

      drawing_outline <- shiny::reactive({

        shiny::req(selection_tool() == "outline")

        data.frame(
          col = data$outline$col,
          row = data$outline$row
        )

      })

      selected_brain_areas <- shiny::reactive({

        hvar <- hover_var()

        out <-
          dplyr::filter(voxel_df(), id %in% selected_voxels()) %>%
          dplyr::distinct(hemisphere, !!rlang::sym(hvar))

        return(out)

      })

      # only voxels of the currently selected slice (=pixels) !!!!remove?
      selected_pixels <- shiny::reactive({

        selection_template() %>%
          dplyr::filter(selected) %>%
          dplyr::pull(id)

      })

      # all voxels selected within this MRI plane -> given to the controller
      # -> distributed to the remaining MRI instances
      # updated after every selection/erasing instance
      selected_voxels <- shiny::reactive({ voxel_df()$id[voxel_df()$selected] })

      selected_voxels_control <- shiny::reactive({ mri_control()$selected_voxels })

      selection_color <- shiny::reactive({ ifelse(!selection_erase(), "forestgreen", "red") })

      selection_erase <- shiny::reactive({ mri_control()$selection_erase })

      selection_template <- shiny::reactive({

        shiny::req(nrow(voxel_df()) != 0)

        dplyr::select(
          .data = voxel_df()[voxel_df()[[axis_ccs]] == slice_idx(), ],
          id,
          selected,
          color,
          is_margin,
          is_margin_cand,
          !!!ra_ccs
        )

      })

      shiny::observeEvent(mri_control()$selection_tool, {

        if(!identical(x = selection_tool(), y = mri_control()$selection_tool)){

          selection_tool({ mri_control()$selection_tool })

        }

      }, ignoreInit = TRUE)

      show_selection_state <- shiny::reactive({

        mode() %in% c("selection") &
        # if active paintbrushing, selection state is displayed dynamically by interaction plot
        !(stringr::str_detect(interaction_tool(), pattern = "paintbrush")  & drawing_active())

      })

      # ----- Observers



      # Observe Events ----------------------------------------------------------

      # ensure that no erasing is possible if the current plane does not
      # have any selected pixels

      # --- ensure that selection_backward increases linearly
      # (circumvent weird behavior in MRI control, where the AB value sometimes resets to 1)
      shiny::observeEvent(input$selection_backward,{

        selection_backward_count({ selection_backward_count() + 1 })

      })

      # --- reset selection completely
      shiny::observeEvent(input$selection_reset,{

        voxel_df({ dplyr::mutate(voxel_df(), selected = FALSE) })

      })


      # Observe Events (MRI Interaction) ----------------------------------------

      # --- selection by outline
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "outline")

        # initiate drawing logic for outline
        if(!drawing_active()){

          drawing_active(TRUE)

          # from drawing to stop drawing: save results as selected voxels
        } else if(drawing_active()){

          if(nrow(drawing_outline()) > 3){

            outline_btns_disabled({ FALSE })

            for(outline_btn in c("confirm", "backward", "reset")){

              shiny::updateActionButton(
                session = session,
                inputId = paste0("outline_", outline_btn),
                disabled = outline_btns_disabled()
              )

            }

          }

          drawing_active(FALSE)

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
            label = "Adjust",
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
        outline_btns_disabled({ TRUE })

        for(outline_btn in c("confirm", "backward", "reset", "focus")){

          if(outline_btn == "confirm"){

            icon <- shiny::icon(name = "check")

          } else {

            icon <- NULL

          }

          shiny::updateActionButton(
            session = session,
            inputId = paste0("outline_", outline_btn),
            disabled = outline_btns_disabled(),
            icon = icon
          )

        }

      })

      shiny::observeEvent(input$outline_focus, {

        slice_pos[[col_axis]] <- mean(data$outline$col)
        slice_pos[[row_axis]] <- mean(data$outline$row)

      })

      # --- dblclick + paintbrush -> updates selection_mask()
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "paintbrush")

        # initiate drawing logic for selection + paintbrush
        if(!drawing_active()){

          # initiate with selected voxels of this slice (=pixels)
          # selection_erase() -> reduced with setdiff()
          # !selection_erase() -> expanded with c()
          data$paintbrush <- selected_pixels()

          drawing_active(TRUE)

          # from drawing to stop drawing: update selection_mask()
        } else if(drawing_active()){

          if(!selection_erase()){

            mask_ids <- setdiff(paintbrush_selected_ids(), selected_pixels())

            selection_mask({

              dplyr::filter(.data = selection_template(), id %in% {{mask_ids}})

            })

          } else if(selection_erase()) {

            mask_ids <- setdiff(selected_pixels(), paintbrush_selected_ids())

            selection_mask({

              # selected stays as is, erased tells which voxels to erase
              dplyr::mutate(
                .data = selection_template(),
                erased = id %in% {{mask_ids}}
                )

            })

          }

          drawing_active(FALSE)

        }

      }, ignoreInit = TRUE)

      # --- dblclick + region_click -> updates voxel_df() immediately
      shiny::observeEvent(input$mriPlot_dblclick, {

        shiny::req(mode() == "selection" & selection_tool() == "region_click")

        # validity check
        if(!cursor_on_brain_tissue()){

          shiny::showNotification(
            ui = "Can not select non-brain tissue.",
            duration = 5,
            type = "error"
          )

          shiny::req(FALSE)

        }

        hvar <- hover_var()
        brain_region <- slice_df_hover()[[hvar]]
        hemisphere <- slice_df_hover()[["hemisphere"]]

        # 1. scenario: not erasing
        # extract required data
        if(FALSE){ #interaction_dims() == "2D"

          slice_indices <- slice_idx()

          voxel_subset <-
            slice_df()[slice_df()[[hover_var()]] == brain_region, ] %>%
            dplyr::filter(hemisphere == {{hemisphere}})

        } else if(TRUE){ # interaction_dims() == "3D"

          voxel_subset <-
            dplyr::filter(
              .data = voxel_df(),
              !!rlang::sym(hvar) == {{brain_region}} & hemisphere == {{hemisphere}}
            )

        }

        # apply
        voxel_ids_area <- voxel_subset$id

        if(!slice_df_hover()$selected){

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              selected = id %in% {{voxel_ids_area}} | selected
            )

          })

        } else if(slice_df_hover()$selected){

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              selected = dplyr::if_else(id %in% {{voxel_ids_area}}, true = FALSE, false = selected)
            )

          })

        }

        # Done. End of the observer updates slice position.

        # --- update slice position --- #
        slice_pos[[plane]] <- slice_idx()

        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_col",
          value = round(input$mriPlot_dblclick$x)
        )

        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_row",
          value = round(input$mriPlot_dblclick$y)
        )

        # Observer done.

      })

      # transfer changes from the selection mask to the voxel_df()
      shiny::observeEvent(selection_mask(), {

        shiny::req(nrow(selection_mask()) != 0)

        if(interaction_dims() == "2D"){

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              selected = dplyr::if_else(
                condition = id %in% selection_mask()$id,
                true = !selection_erase(),
                false = selected
                )
            )

          })

        } else if(interaction_dims() == "3D"){

          if(!selection_erase()){

            voxel_df({

              propagate_selection_3D(
                voxel_df = voxel_df(),
                selection_mask = selection_mask(),
                selection_plane = plane,
                erase = FALSE
              )

            })

          } else {

            voxel_df({

              apply_erase_mask(
                voxel_df = voxel_df(),
                erase_mask = selection_mask(),
                erase_mask_slice = slice_idx(),
                erase_mask_plane = plane
              )

            })

          }



        }

        # update localizers
        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_row",
          value = round(mean(selection_mask()$row))
        )

        shinyWidgets::updateNoUiSliderInput(
          session = session,
          inputId = "mri_slider_col",
          value = round(mean(selection_mask()$col))
        )

        selection_mask(data.frame())

      }, ignoreInit = TRUE)

      # SORT --------------------------------------------------------------------

      # LAST: Module Output -----------------------------------------------------

      module_output <- shiny::reactive({

        list(
          drawing_outline_confirmed = drawing_outline_confirmed(),
          slice_pos = slice_pos,
          voxel_df = voxel_df()
        )

        })


      return(module_output)


    }
  )

}
