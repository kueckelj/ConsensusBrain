

moduleMriControlUI <- function(id){ }

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
                                   external_selection_modes = c("inspection", "refinement")
                                   ){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){

      ns <- session$ns
      
      shiny::observeEvent(input$test, {
        
        print(slice_state)
        
      })
      
      shiny::observe({
        
        #print(voxel_df_input())
        
      })

      # 1. Global ---------------------------------------------------------------

      # Reactive Values ---------------------------------------------------------
      
      voxel_df <- shiny::reactiveVal(value = data.frame())
      
      stacks <- shiny::reactiveValues(selection = list(character()))
      

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
      
      
      shiny::observeEvent(external_selection(), {
        
        # 1. update voxel_df
        voxel_df({
          
          dplyr::mutate(
            .data = voxel_df(), 
            selected = id %in% external_selection()[["id"]]
          )
          
        })
        
        # 2. switch modes
        if(shiny::isTruthy(external_selection_modes)){
          
          if(nrow(external_selection()) == 0){
            
            mode(external_selection_modes[1])
            
          } else {
            
            mode(external_selection_modes[2])
            
          }
          
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
      shiny::observeEvent(FALSE, {
        
        if(length(selected_voxels_control()) != 0){
          
          voxels_margin({
            
            identify_obs_within_safety_margin(
              voxel_df = voxel_df(), 
              selected_voxels = selected_voxels_control(), 
              distance = 25, # compute for once max, and filter live
              restrict_hemisphere = TRUE
            )
            
          })
          
        } else {
          
          voxels_margin({
            
            # return empty df with appropriate column names
            dplyr::filter(voxel_df(), id == "none")
            
          })
          
        }
        
      })
      
      # 5. Manage Refinement Mode -----------------------------------------------
      voxels_margin <- shiny::reactiveVal(data.frame()) #!!!!
      
      shiny::observeEvent(mode(), {
        
        if(mode() == "refinement"){
          
          voxel_df({ enter_refinement_mode(voxel_df = shiny::isolate({ voxel_df() })) })
          
        } else if(mode() == "selection"){
          
          voxel_df({ leave_refinement_mode(voxel_df = shiny::isolate({ voxel_df() })) })
          
        }
        
      })
      
      
      # Module Output ----------------------------------------------------------
      
      module_output <- shiny::reactive({
        
        list(
          hover_var = input$hover_var,
          mode = mode(),
          slice_state = slice_state, 
          selected_voxels = selected_voxels_control(), 
          voxel_df = voxel_df(), 
          voxels_margin = voxels_margin()
        )
        
      })
      
      return(module_output)
      
    }
  )
  
}






