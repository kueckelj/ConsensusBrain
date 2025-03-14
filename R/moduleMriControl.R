

moduleMriControlUI <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style(
      shiny::HTML(
        glue::glue("
                   .common-button {
                    height: 40px; /* Adjust based on preference */
                    width: 100px;  /* Adjust based on preference */
                    font-size: 14px; /* Uniform font size */
                    border-radius: 5px; /* Optional: rounded corners */
                    border: 1px solid #ccc; /* Light border */
                    background-color: #f8f9fa; /* Light grey background */
                    color: #333; /* Text color */
                    text-align: center;
                    display: inline-flex;
                    align-items: center;
                    justify-content: center;
                    padding: 5px;
                    transition: background-color 0.2s ease-in-out;
                    }

                    .common-button:hover {
                    background-color: #e9ecef; /* Slightly darker on hover */
                    }",
                   .open = "{{", .close = "}}")
        )
    ),
    shiny::div(
      style =
        glue::glue("
                    background-color: white;
                    border-radius: 10px;
                    box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
                    margin-top: 20px;
                    padding-left: 10px;
                    padding-right: 10px;
                    display: flex;
                    flex-direction: column;
                    width: 1000px;
                    height: 100px;"),
      shiny::fluidRow(

        # depends on mode()
        shiny::uiOutput(outputId = ns("mri_buttons"))#,
       # shiny::actionButton(inputId = ns("test"), label = "Test")

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
        print("Test")
        assign("voxel_df", voxel_df(), envir = .GlobalEnv)

      }, ignoreInit = TRUE)

      shiny::observe({

        #print(voxel_df_input())

      })

      # 1. Global ---------------------------------------------------------------

      # Dynamic UI --------------------------------------------------------------

      output$interaction_dims <- shiny::renderUI({

        shiny::tagList(
          shiny::h5(shiny::strong("Dimensions:")),
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
              '<i class="fas fa-ruler" style="font-size: 1.5em;" data-toggle="tooltip" title="Safety-Margin"></i>' = "safety_margin"
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
                width = 2,
                align = "left",
                shiny::uiOutput(outputId = ns("interaction_dims")),
              ),
              shiny::column(
                width = 5,
                align = "left",
                shiny::h5(shiny::strong("Interaction Tool:")),
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
              shiny::column(
                width = 2,
                align = "left",
                shiny::h5(shiny::strong("Back/Trash:")),
                shiny::splitLayout(
                  shiny::actionButton(
                    inputId = ns("selection_backward"),
                    label = NULL,
                    icon = shiny::icon("step-backward", lib = "glyphicon"),
                    width = "100%"
                  ),
                  shiny::actionButton(
                    inputId = ns("selection_reset"),
                    label = NULL,
                    icon = shiny::icon("trash", lib = "glyphicon"),
                    width = "100%"
                  ),
                  cellWidths = "50%"
                )
              )
            )

        }

      })

      output$paintbrush_size <- shiny::renderUI({

        shiny::req(stringr::str_detect(input$selection_tool, pattern = "paintbrush"))

        shiny::column(
          width = 3,
          align = "left",
          shiny::h5(shiny::strong("Brush-Size:")),
          shiny::splitLayout(
            shiny::actionButton(
              inputId = ns("pbr_increase"),
              label = shiny::HTML('<span class="fa-stack" style="font-size: 0.8em; width: 1.5em;">
                          <i class="fas fa-paint-brush fa-stack-1x" style="position: absolute; top: -3px; left: -3.5px; font-size: 1.8em;"></i>
                          <i class="fas fa-plus fa-stack-1x" style="position: absolute; bottom: -8px; left: 8.5px; font-size: 1.2em; color: black;"></i>
                        </span>'),
              width = "100%"
            ),

            shiny::actionButton(
              inputId = ns("pbr_decrease"),
              label = shiny::HTML('<span class="fa-stack" style="font-size: 0.8em; width: 1.5em;">
                          <i class="fas fa-paint-brush fa-stack-1x" style="position: absolute; top: -3px; left: -3.5px; font-size: 1.8em;"></i>
                          <i class="fas fa-minus fa-stack-1x" style="position: absolute; bottom: -8px; left: 8.5px; font-size: 1.2em; color: black;"></i>
                        </span>'),
              width = "100%"
            ),
            cellWidths = "33%"
          )
        )

      })

      output$safety_margin_ui <- shiny::renderUI({

        if(length(shiny::isolate({ selected_voxels_control() })) != 0){

          shiny::column(
            width = 3,
            align = "left",
            shiny::h5(shiny::strong("Safety Margin [mm]:")),
            shiny::sliderInput(
              inputId = ns("safety_margin_val"),
              label = NULL,
              value = 0,
              min = 0,
              max = 15,
              step = 0.01
            )
          )

        } else {

          shiny::column(
            width = 3,
            shiny::h5(shiny::strong("Safety Margin [mm]:")),
            shiny::helpText("No tissue selected.")
          )

        }



      })

      output$selection_tool_opts <- shiny::renderUI({

        shiny::req(input$selection_tool)

        if(stringr::str_detect(input$selection_tool, "paintbrush")){

          shiny::uiOutput(outputId = ns("paintbrush_size"))

        } else if(input$selection_tool == "safety_margin"){

          shiny::uiOutput(outputId = ns("safety_margin_ui"))

        } else {

          shiny::column(width = 3)

        }

      })


      # Reactive Values ---------------------------------------------------------

      paintbrush_radius_fct <- shiny::reactiveVal(value = 0.025)

      stacks <- shiny::reactiveValues(selection = list(character()))

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

      updated_voxels <- shiny::reactiveVal(character())

      # centralized vector of selected voxels
      # - character(0) when initialized
      # - character(n) after observing selection changes in either MRI instance
      # -> part of Module output, distribution to remaining MRI instances
      selected_voxels_control <- shiny::reactive({

        voxel_df()$id[voxel_df()$selected]

      })

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

        if(input$selection_tool == "safety_margin"){

          if(length(selected_voxels_control()) == 0){

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
              duration = 5
            )

            voxel_df({

              prepare_margin_selection(voxel_df(), dist_max = 15)

            })

          }

        }

      })

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

      # stack selection changes
      shiny::observeEvent(selected_voxels_control(), {

        stacks <-
          add_to_stack(
            stacks = stacks,
            which = "selection",
            what = selected_voxels_control()
          )

      })

      # --- Observe selection backward steps
      counter_sag <- shiny::reactiveVal(value = 0)
      counter_axi <- shiny::reactiveVal(value = 0)
      counter_cor <- shiny::reactiveVal(value = 0)

      shiny::observeEvent(mri_sag_out()$selection_backward, {

        diff <- mri_sag_out()$selection_backward - counter_sag()

        if(diff == 1){
          stacks <- reduce_stack(stacks, which = "selection")
        }

        counter_sag(mri_sag_out()$selection_backward)

      })

      shiny::observeEvent(mri_axi_out()$selection_backward, {

        diff <- mri_axi_out()$selection_backward - counter_axi()

        if(diff == 1){

          stacks <- reduce_stack(stacks, which = "selection")

        }

        counter_axi(mri_axi_out()$selection_backward)

      })

      shiny::observeEvent(mri_cor_out()$selection_backward, {

        diff <- mri_cor_out()$selection_backward - counter_cor()

        if(diff == 1){

          stacks <- reduce_stack(stacks, which = "selection")

        }

        counter_cor(mri_cor_out()$selection_backward)

      })

      shiny::observeEvent(stacks$selection, {

        last_selection <- dplyr::last(stacks$selection)

        # Ensure the stack is not empty and that we are not repeating the same state
        if(!is.null(last_selection) && !identical(last_selection, shiny::isolate({ selected_voxels_control() }))) {

          voxel_df({

            dplyr::mutate(
              .data = voxel_df(),
              selected = id %in% {{last_selection}}
            )

          })

        }
      }, ignoreNULL = TRUE)

      # --- safety margin
      shiny::observeEvent(input$safety_margin_val, {

        shiny::req(input$safety_margin_val != 0)

        margin_ids <- voxel_df()[voxel_df()$dist <= input$safety_margin_val,][["id"]]

        voxel_df({

          dplyr::mutate(
            .data = voxel_df(),
            is_margin = id %in% {{margin_ids}},
            selected = selected | is_margin,
            color = dplyr::if_else(is_margin, true = alpha("steelblue", 0.45), false = color)
          )

        })

      }, ignoreInit = TRUE)

      # 5. Manage Refinement Mode -----------------------------------------------
      voxels_margin <- shiny::reactiveVal(data.frame()) #!!!!


      # Module Output ----------------------------------------------------------

      module_output <- shiny::reactive({

        list(
          hover_var = input$hover_var,
          interaction_dims = interaction_dims(),
          mode = mode(),
          pbr_fct = paintbrush_radius_fct(),
          slice_state = slice_state,
          selected_voxels = selected_voxels_control(),
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






