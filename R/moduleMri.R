


moduleMriUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::br(), 
    shiny::fluidRow(
      
      # MRI column 1
      shiny::column(
        width = 4,
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_sag"), plane = "sag")
      ),
      
      # MRI column 2
      shiny::column(
        width = 4, 
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_axi"), plane = "axi"), 
      ),
      
      # MRI column 3
      shiny::column(
        width = 4, 
        align = "center",
        moduleMriPlaneUI(id = ns("mri_plane_cor"), plane = "cor"), 
      )
      
    ) 
  )

  
}

moduleMriServer <- function(id, 
                            voxel_df, 
                            mri_list, 
                            mri_frame = c(1, 256),
                            mode_init = "inspection",
                            external_selection = function(){ NULL }){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      ns <- session$ns
      
      mri_plane_sag <- 
        moduleMriPlaneServer(
          id = "mri_plane_sag",
          mri_control = shiny::reactive({ mri_control() }),
          mri_list = shiny::reactive({ mri_list() }), 
          mri_frame = mri_frame,
          plane = "sag"
        )
      
      mri_plane_axi <- 
        moduleMriPlaneServer(
          id = "mri_plane_axi", 
          mri_control = shiny::reactive({ mri_control() }),
          mri_list = shiny::reactive({ mri_list() }), 
          mri_frame = mri_frame,
          plane = "axi"
        )
      
      mri_plane_cor <- 
        moduleMriPlaneServer(
          id = "mri_plane_cor",
          mri_control = shiny::reactive({ mri_control() }),
          mri_list = shiny::reactive({ mri_list() }),
          mri_frame = mri_frame,
          plane = "cor"
        )
      
      mri_control <- 
        moduleMriControlServer(
          id = "mri_control", 
          mode_init = mode_init,
          mri_sag_out = shiny::reactive({ mri_plane_sag() }), 
          mri_axi_out = shiny::reactive({ mri_plane_axi() }), 
          mri_cor_out = shiny::reactive({ mri_plane_cor() }), 
          voxel_df_input = shiny::reactive({ voxel_df() }), 
          external_selection = shiny::reactive({ external_selection() })
        )
      
      module_output <- shiny::reactive({ mri_control() })
      
      return(module_output)
      
    }
  )
  
}