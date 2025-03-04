


# moduleBrain3D -----------------------------------------------------------

moduleBrain3D_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "background-color: white; padding: 10px; border-radius: 5px",
    plotly::plotlyOutput(ns("plot_Brain3d"), height = "750px"), 
    moduleVoxelSelection_ui(id = ns("highlightBrain3D"))
  )
  
  
}

moduleBrain3D_server <- function(id, voxel_df){
  
  ns <- shiny::NS(id)
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){

      mo_highlight <- 
        moduleVoxelSelection_server(
          id = "highlightBrain3D", 
          voxel_df = voxel_df
          )
      
      voxel_subset <- shiny::reactive({
        
        if(shiny::isTruthy(mo_highlight())){
          
          voxel_df[voxel_df$id %in% mo_highlight()$id,]
          
        } else {
          
          voxel_df
          
        }
        
      })
      
      output$plot_Brain3d <- plotly::renderPlotly({
        
        vdf <- voxel_subset()
        
        plot_brain_3d(
          voxel_df = voxel_df_sub(), 
          color_by = "ann_macro", 
          clrp_adjust = ctp_clrp, 
          pt_size = 2.5, 
          show_legend = FALSE, 
          hoverinfo = c("ann_macro", "ann_dk_adj", "wm_tracts")
        )
        
      })
      
    }
  )
  
}







# moduleVoxelColors -------------------------------------------------------

moduleVoxelColors_ui <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      style = css_styles$CB_box,
      shiny::h4("Color Controls", style = "margin-bottom: 10px;"),
      shinyWidgets::pickerInput(
        inputId = ns("color_var"), 
        label = "Highlight:", 
        choices = 
          c("Macroanatomical" = "ann_macro",
            "White Matter Tracts" = "wm_tract", 
            "Subcortical" = "subcortical", 
            "Selection Type" = "selection", 
            "Score" = "CBscore"
          ), 
        selected = "selection"
      ), 
      shiny::plotOutput(outputId = ns("color_legend"))
    )
  )
  
}


moduleVoxelColors_server <- function(id, 
                                     voxel_subset, # reactive expression from moduleVoxelSelection
                                     mri_list_in){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      observeEvent(input$test,{
        
        print(voxel_subset())
        
      })
      
      # ----- reactive (events)
      mri_list_out <- shiny::reactive({
        
        shiny::req(input$color_var)
        
        voxel_sub <- voxel_subset()
        mri_list_adj <- mri_list_in
        
        if(shiny::isTruthy(voxel_sub) && nrow(voxel_sub) != 0){
          
          if(any((voxel_sub$is_wm & !voxel_sub$is_tract))){
            
            voxel_sub_wm <- voxel_sub[(voxel_sub$is_wm & !voxel_sub$is_tract),]
            
            mri_list_adj <- 
              exchange_raster_colors(
                mri_list = mri_list_adj, 
                voxel_df = voxel_sub_wm, 
                color_new = c(input$color_var, "t1"), 
                color_weights = c(0.4, 0.6), 
                clrp_adjust2 = ctp_clrp[[shiny::isolate(input$color_var)]]
              )
            
          }
          
          if(any((!voxel_sub$is_wm | voxel_sub$is_tract))){
            
            voxel_sub_nwm <- voxel_sub[(!voxel_sub$is_wm | voxel_sub$is_tract),]
            
            mri_list_adj <- 
              exchange_raster_colors(
                mri_list = mri_list_adj, 
                voxel_df = voxel_sub_nwm, 
                color_new = c(input$color_var, "t1"), 
                color_weights = c(0.6, 0.4), 
                clrp_adjust2 = ctp_clrp[[shiny::isolate(input$color_var)]]
              )
            
          }
          
        } 
        
        return(mri_list_adj)
        
      })
      
      # ----- outputs
      output$color_legend <- shiny::renderPlot({
        
        plot_color_legend(colors = ctp_clrp[[input$color_var]])
        
      })
      
      # ----- module output
      return(mri_list_out)
      
    }
  )
  
}


# moduleVoxelSelection ----------------------------------------------------





