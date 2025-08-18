

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

######

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

#####

slice_raster_to_df <- function(slice_rst){

  reshape2::melt(data = as.matrix(slice_rst), varnames = c("col", "row"), value.name = "value") %>%
    tibble::as_tibble()

}
