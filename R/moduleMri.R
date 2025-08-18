


moduleMriUI <- function(id = "mri", ...){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),
    shiny::fluidRow(

      # MRI column 1
      shiny::column(
        width = 4,
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_sag"), plane = "sag", ...)
      ),

      # MRI column 2
      shiny::column(
        width = 4,
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_axi"), plane = "axi", ...),
      ),

      # MRI column 3
      shiny::column(
        width = 4,
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_cor"), plane = "cor", ...),
      )

    ),

    shiny::fluidRow(
      shiny::column(
        width = 12,
        align = "center",
        moduleMriControlUI(id = ns("mri_control"))
      )
    )
  )


}

moduleMriServer <- function(id = "mri",
                            voxel_df = function(){ load_consensus_template() },
                            nifti_input = function(){ mni_template },
                            mode_init = "inspection",
                            mode_opts = character(),
                            color_selected = function(){ colorsCB$selected },
                            external_selection = function(){ NULL },
                            external_selection_opts = list(),
                            reset_quick_select = function(){ NULL }){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      ns <- session$ns

      mri_plane_sag <-
        moduleMriPlaneServer(
          id = "mri_plane_sag",
          mode_init = mode_init,
          mode_opts = mode_opts,
          color_selected = shiny::reactive({ color_selected() }),
          mri_control = shiny::reactive({ mri_control() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          voxel_df_input = shiny::reactive({ voxel_df() }),
          plane = "sag",
          reset_quick_select = shiny::reactive({ reset_quick_select() })
        )

      mri_plane_axi <-
        moduleMriPlaneServer(
          id = "mri_plane_axi",
          mode_init = mode_init,
          mode_opts = mode_opts,
          color_selected = shiny::reactive({ color_selected() }),
          mri_control = shiny::reactive({ mri_control() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          voxel_df_input = shiny::reactive({ voxel_df() }),
          plane = "axi",
          reset_quick_select = shiny::reactive({ reset_quick_select() })
        )

      mri_plane_cor <-
        moduleMriPlaneServer(
          id = "mri_plane_cor",
          mode_init = mode_init,
          mode_opts = mode_opts,
          color_selected = shiny::reactive({ color_selected() }),
          mri_control = shiny::reactive({ mri_control() }),
          nifti_input = shiny::reactive({ nifti_input() }),
          voxel_df_input = shiny::reactive({ voxel_df() }),
          plane = "cor",
          reset_quick_select = shiny::reactive({ reset_quick_select() })
        )

      mri_control <-
        moduleMriControlServer(
          id = "mri_control",
          mode_init = mode_init,
          mode_opts = mode_opts,
          mri_sag_out = shiny::reactive({ mri_plane_sag() }),
          mri_axi_out = shiny::reactive({ mri_plane_axi() }),
          mri_cor_out = shiny::reactive({ mri_plane_cor() }),
          voxel_df_input = shiny::reactive({ voxel_df() }),
          external_selection = shiny::reactive({ external_selection() }),
          external_selection_opts = external_selection_opts
        )

      module_output <- shiny::reactive({ mri_control() })

      return(module_output)

    }
  )

}
