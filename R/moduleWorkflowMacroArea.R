

moduleWorkflowMacroAreaUI <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        moduleBrainTissueSelectionUI(id = ns("selection"))
      ),
      shiny::column(
        width = 4,
        moduleScoreAssignmentUI(id = ns("score_assignment"))
      ),
      shiny::column(
        width = 4,
        plotly::plotlyOutput(outputId = ns("selectionViewer3D"), height = "350px")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        moduleMriUI(id = ns("mri"))
      )
    )
  )

}

moduleWorkflowMacroAreaServer <- function(id,
                                          macro_area,
                                          voxel_df_input,
                                          mri_list_T1){

  macro_color <- unname(consensus_clrps[["ann_macro"]][macro_area])

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      voxel_df <- shiny::reactive({

        dplyr::mutate(voxel_df_input(), color = alpha({{macro_color}}, 0.45))

      })

      voxel_df_output <- shiny::reactiveVal(data.frame())

      mo_selection <-
        moduleBrainTissueSelectionServer(
          id = "selection",
          macro_area = macro_area,
          voxel_df_input = shiny::reactive({ voxel_df() })
        )

      module_mri <-
        moduleMriServer(
          id = "mri",
          mode_init = "inspection",
          voxel_df = shiny::reactive({ voxel_df() }),
          mri_list = shiny::reactive({ mri_list_T1 }),
          external_selection = shiny::reactive({ mo_selection() })
        )

      mo_score_assignment <-
        moduleScoreAssignmentServer(
          id = "score_assignment",
          macro_area = macro_area,
          voxel_df_input = shiny::reactive({ module_mri()$voxel_df })
        )

      output$selectionViewer3D <- plotly::renderPlotly({

        shiny::req(FALSE)

        selected_voxels <- dplyr::filter(module_mri()$voxel_df, selected)

        shiny::validate(
          shiny::need(
            expr = nrow(selected_voxels) != 0,
            message = "No brain tissue selected."
          )
        )

        hemispheres <- unique(selected_voxels$hemisphere)

        plot_input <-
          dplyr::filter(
            .data = module_mri()$voxel_df,
            hemisphere %in% hemispheres
          ) %>%
          dplyr::mutate(
            selection = dplyr::if_else(selected, "selected", "not_selected")
          )

        plot_brain_3d(
          voxel_df = plot_input,
          color_by = "selection",
          group_highlight = "selected",
          opacity_hide = 0.05,
          pt_size = 1,
          clrp_adjust = c("selected" = macro_color, "not_selected" = "lightgrey"),
          paper_bgcolor = "black",
          show_legend = FALSE
          )

      })

      module_output <- shiny::reactive({ mo_score_assignment() })

      return(module_output)

    }
  )

}
