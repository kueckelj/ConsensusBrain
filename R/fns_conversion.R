

# MRI Data ----------------------------------------------------------------

# NIfTI object to MRI raster list
nifti_to_mri_list <- function(nifti, clrsp = c("black", "white"), verbose = TRUE){
  
  purrr::map(
    .x = mri_planes, 
    .f = function(plane){
      
      n_slices <- dim(nifti)[which(plane == mri_planes)]
      
      confuns::give_feedback(
        msg = glue::glue("Converting {n_slices} slices of plane '{plane}'."), 
        verbose = verbose
      )
      
      pb <- confuns::create_progress_bar(total = n_slices)
      
      purrr::map(
        .x = 1:n_slices, 
        .f = function(slice){
          
          if(verbose){ pb$tick() }
          
          slice_df <- get_slice_df(input = nifti, slice = slice, plane = plane)
          
          slice_df_to_raster(
            slice_df = slice_df, 
            clrsp = clrsp
          )
          
        }
      )
      
    }
  ) %>% 
    purrr::set_names(nm = mri_planes)
  
}

# NIfTI object to voxel data.frame
nifti_to_voxel_df <- function(nifti, black_rm = TRUE, verbose = TRUE){
  
  # iterate over coronar plane
  cor_vals <- dim(nifti)[2]
  
  pb <- confuns::create_progress_bar(cor_vals)
  
  voxel_df <- 
    purrr::map_df(
      .x = 1:cor_vals, 
      .f = function(cor_slice){
        
        if(verbose){ pb$tick() }
        
        df <- get_slice_df(input = nifti, plane = "cor", slice = cor_slice)
        
        names(df)[c(1,2)] <- c("x", "y")
        df$z <- cor_slice
        
        out <- dplyr::select(df, x, y, z, value)
        
        return(out)
        
      }
    )
  
  if(black_rm){
    
    voxel_df <- dplyr::filter(voxel_df, value != 0)
    
  }
  
  voxel_df <- dplyr::mutate(voxel_df, id = paste0("x", x, "y", y, "z", z))
  voxel_df <- dplyr::select(voxel_df, id, dplyr::everything())
  
  return(voxel_df)
  
}

# voxel data.frame to MRI raster list
voxel_df_to_mri_list <- function(voxel_df,
                                 var = NULL, 
                                 clrp = "default", 
                                 clrp_adjust = NULL,
                                 clrsp = c("black", "white"),
                                 color = "black",
                                 color_missing = "white",
                                 n_slices = c(sag = 256, axi = 256, cor = 256),
                                 verbose = T){
  
  purrr::map(
    .x = mri_planes, 
    .f = function(plane){
      
      ccs_axis <- plane_to_ccs(plane)
      
      slices_with_data <- sort(unique(voxel_df[[ccs_axis]]))
      
      n_slices_plane <- unname(n_slices[plane])
      
      req_axes <- req_axes_2d(plane, mri = TRUE)
      
      orig_frame <- 
        tidyr::expand_grid(
          col = 1:n_slices[req_axes["col"]], 
          row = 1:n_slices[req_axes["row"]]
          )
      
      confuns::give_feedback(
        msg = glue::glue("Converting {n_slices_plane} slices of CCS axis '{ccs_axis}' ({plane})."), 
        verbose = verbose
      )
      
      pb <- confuns::create_progress_bar(total = n_slices_plane)
      
      purrr::map(
        .x = 1:n_slices_plane, 
        .f = function(slice){
          
          if(verbose){ pb$tick() }
          
          if(slice %in% slices_with_data){
            
            slice_df <- get_slice_df(input = voxel_df, slice = slice, plane = plane, var = var)
            
            slice_df_to_raster(
              slice_df = slice_df, 
              clrp = clrp, 
              clrp_adjust = clrp_adjust,
              clrsp = clrsp,
              color = color, 
              color_missing = color_missing, 
              orig_frame = orig_frame
            )
            
          } else {
            
            slice_df_to_raster(
              slice_df = orig_frame, 
              color = grDevices::rgb(0, 0, 0)
            )
            
          }
          
        }
      )
      
    }
  ) %>% purrr::set_names(nm = mri_planes)
  
}


# MRI Slice Data ----------------------------------------------------------


# extract slice as slice_df
# use to go from 3D to 2D
get_slice_df <- function(input,
                         slice,
                         plane,
                         n_slices = c(sag = 256, axi = 256, cor = 256),
                         var = NULL){
  
  require(oro.nifti)
  
  # 1. for nifti objects
  if(oro.nifti::is.nifti(input)){
    
    if(slice < 1){
      
      axis <- which(plane == c("sag", "cor", "axi"))
      
      slice_index_use <-
        (dim(image_data)[axis]*slice) %>%
        round()
      
    } else {
      
      slice_index_use <- slice
      
    }
    
    if(plane == "axi"){
      
      slice_out <- input[,,slice_index_use]
      
    } else if(plane == "sag"){
      
      slice_out <- input[slice_index_use,,]
      
    } else if(plane == "cor"){
      
      slice_out <- input[,slice_index_use,]
      
    } else {
      
      stop("Invalid plane Choose 'axi', 'sag', or 'cor'.")
      
    }
    
    # also works for matrics
    out <- slice_raster_to_df(slice_out)
    
    # 2. for voxel data.frames
  } else if(is.data.frame(input)){
    
    cols_select <- c(req_axes_2d(plane), "value" = var)
    
    axis <- plane_to_ccs(plane = plane)
    
    if(slice < 1){
      
      slice_index_use <- round(max(input[[axis]])*slice)
      
    } else {
      
      slice_index_use <- slice
      
    }
    
    out <- 
      dplyr::filter(input, !!rlang::sym(axis) == {{slice_index_use}}) %>% 
      dplyr::select(!!!cols_select)
    
    # 3. for mri lists
  } else if(is.list(input)){
    
    slice_rst <- input[[plane]][[slice]]
    
    out <- slice_raster_to_df(slice_rst)
    
  }
  
  return(out)
  
}


# slice data.frame to slice raster
slice_df_to_raster <- function(slice_df,
                               clrp = "default", 
                               clrp_adjust = NULL, 
                               clrsp = c("black", "white"),
                               color = "black", 
                               color_missing = "white", 
                               orig_frame = NULL){
  
  # if values exist, map colors
  if("value" %in% names(slice_df)){
    
    # map numeric color spectrum
    if(is.numeric(slice_df$value)){
      
      if(all(slice_df$value == 0)){
        
        slice_df$colors <- grDevices::rgb(red = 0, green = 0, blue = 0)
        
      } else {
        
        if(length(clrsp) == 2){
          
          ramp <- grDevices::colorRamp(clrsp)
          
          normalized_values <- 
            (slice_df$value - min(slice_df$value, na.rm = TRUE)) /
            (max(slice_df$value, na.rm = TRUE) - min(slice_df$value, na.rm = TRUE))
          
          col_out <- ramp(normalized_values)
          
          slice_df$colors <- 
            grDevices::rgb(
              red = col_out[,1], 
              green = col_out[,2],
              blue = col_out[,3],
              maxColorValue = 255
              )
          
        } else {
          
          clrsp <- clrsp[1]
          
          slice_df$colors <- map_values_to_colors(values = slice_df$value, clrsp = clrsp[1])
          
        }
        
      } 
      
      # map discrete color palette
    } else {
      
      # ensure factor variable
      if(is.logical(slice_df$value)){
        
        slice_df$value <- factor(as.character(slice_df$value), levels = c("TRUE", "FALSE"))
        
      } else if(is.character(slice_df$value)){
        
        slice_df$value <- factor(slice_df$value, levels = sort(unique(slice_df$value)))
        
      }
      
      clr_vec <- 
        confuns::color_vector(
          clrp = clrp,
          names = levels(slice_df$value), 
          clrp.adjust = clrp_adjust
        )
      
      slice_df$colors <- clr_vec[slice_df$value]
      
    }
    
    # set one color (only useful for "subsetted" slice data.frames)
  } else {
    
    slice_df$colors <- color
    
  }
  
  if(is.data.frame(orig_frame)){
    
    slice_df <- dplyr::left_join(x = orig_frame, y = slice_df, by = c("col", "row"))
    
  }
  
  reshape2::dcast(
    data = slice_df,
    formula = row ~ col,
    fill = color_missing,
    value.var = "colors")[,-1] %>% 
    as.matrix() %>% 
    grDevices::as.raster(x = .)
  
}

# slice raster to slice data.frame
# also works for matrix or array
slice_raster_to_df <- function(slice_rst){
  
  reshape2::melt(data = as.matrix(slice_rst), varnames = c("col", "row"), value.name = "value") %>% 
    tibble::as_tibble()
  
}

