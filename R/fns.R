

# objects -----------------------------------------------------------------

# do not change the order!
ccs_labels <- c("x", "z", "y")
mri_planes <- c("sag", "cor", "axi")
mri_planes_pretty <- c("sag" = "Sagittal", "cor" = "Coronal",  "axi" = "Axial")

# functions ---------------------------------------------------------------

abbr <- function(text){

  if(stringr::str_length(text) > 20){

    words <- unlist(strsplit(text, "\\s+"))
    abbreviated <- paste0(substr(words, 1, 6), ".")

    paste(abbreviated, collapse = " ") %>%
      stringr::str_replace_all(pattern = "\\.\\.", replacement = ".")

  } else {

    text

  }

}

add_helper <- function(shiny_tag, name){

  help_info <- CB_help[[name]]

  if(is.null(help_info$title)){ title <- make_pretty_label(name) }

  shinyhelper::helper(
    shiny_tag = shiny_tag,
    title = title,
    type = help_info$type,
    content = help_info$content
    )

}

add_to_stack <- function(stacks, which, what){

  stacks[[which]][[length(stacks[[which]])+1]] <- what

  return(stacks)

}

adjust_sub_colors <- function(df,
                              main_colors,
                              main_var,
                              sub_var,
                              clr.bottom = "white",
                              offset = 10,
                              alphabetically = FALSE){

  offset <- base::as.integer(offset)
  is_value(offset, mode = "integer") # trans successful?

  sub_list <- base::vector(mode = "list", length = base::length(main_colors))

  for(i in base::seq_along(main_colors)){

    clr <- base::unname(main_colors)[i]
    main_group <- base::names(main_colors)[i]

    sub_groups <-
      dplyr::filter(df, !!rlang::sym(main_var) == {{main_group}}) %>%
      dplyr::pull(var = !!rlang::sym(sub_var))

    n_sub_groups <- base::length(sub_groups)

    clr_palette <-
      grDevices::colorRampPalette(colors = c(clr, clr.bottom))(n_sub_groups+offset)

    if(base::isTRUE(alphabetically)){

      sub_groups <- base::sort(sub_groups)

    }

    sub_list[[i]] <-
      purrr::set_names(
        x = clr_palette[1:n_sub_groups],
        nm = sub_groups
      )

  }

  color_vec_out <- purrr::flatten_chr(sub_list)

  return(color_vec_out)

}


apply_erase_mask <- function(voxel_df,
                             erase_mask,
                             erase_mask_slice,
                             erase_mask_plane){

  voxel_df <- identify_brois(voxel_df)

  erase_mask_mapping <-
    map_refinement_mask_to_broi(
      mask = dplyr::filter(erase_mask, selected),
      slice = erase_mask_slice,
      plane = erase_mask_plane,
      voxel_df = voxel_df
    )

  for(broi in names(erase_mask_mapping)){

    voxel_df <-
      refine3D_erase(
        voxel_df = voxel_df,
        broi = broi,
        erase_mask = erase_mask_mapping[[broi]],
        erase_mask_slice = erase_mask_slice,
        erase_mask_plane = erase_mask_plane
      )

  }

  voxel_df$broi <- NULL

  return(voxel_df)

}

apply_paintbrush_sphere <- function(voxel_df,
                                    cp_list,
                                    plane,
                                    slice,
                                    radius,
                                    selection_scope = NULL,
                                    erase = FALSE){

  if(FALSE){

    assign("cp_list", cp_list, envir = .GlobalEnv)
    assign("voxel_df", voxel_df, envir = .GlobalEnv)
    assign("radius", radius, envir = .GlobalEnv)
    assign("selection_scope", selection_scope, envir = .GlobalEnv)
    print("assigned paintbrush sphere")

  }

  plane_ccs <- switch_axis_label(plane)

  left <- slice-radius
  right <- slice+radius

  paintbrush3D_template <-
    dplyr::filter(
      .data = voxel_df,
      dplyr::between(!!rlang::sym(plane_ccs), left = {{left}}, right = {{right}})
    )

  if(erase){

    paintbrush3D_template <- dplyr::filter(paintbrush3D_template, selected)

  } else if(!erase){

    paintbrush3D_template <- dplyr::filter(paintbrush3D_template, !selected)

  }

  nm <- c(unname(req_axes_2d(plane)), plane_ccs)

  cp_list <-
    purrr::map(
      .x = cp_list,
      .f = ~ purrr::set_names(x = c(.x, slice), nm = nm)
    )

  selected_ids <- vector(mode = "list", length = length(cp_list))

  for(i in seq_along(cp_list)){

    presel_ids <- purrr::flatten_int(selected_ids)

    selected_ids[[i]] <-
      identify_obs_within_radius3D(
        cursor_pos = cp_list[[i]],
        radius = radius,
        interaction_template = paintbrush3D_template[!paintbrush3D_template$id %in% presel_ids,],
        selection_scope = selection_scope
      )

  }

  selected_ids_flat <- purrr::flatten_int(selected_ids)

  if(erase){

    voxel_df <-
      dplyr::mutate(
        .data = voxel_df,
        selected = selected & !id %in% {{selected_ids_flat}}
      )

  } else if(!erase){

    voxel_df <-
      dplyr::mutate(
        .data = voxel_df,
        selected = selected | id %in% {{selected_ids_flat}}
      )

  }

  return(voxel_df)

}

assign_remaining_scores_by_NN <- function(voxel_df){

  if(any(voxel_df$CBscore == 0)){

    score_missing <- dplyr::filter(voxel_df, CBscore == 0)
    score_exists <- dplyr::filter(voxel_df, CBscore != 0)

    nn_out <-
      RANN::nn2(
        data = score_exists[,c("x", "y", "z")],
        query = score_missing[,c("x", "y", "z")],
        k = 1
      )

    score_missing$CBscore <- score_exists$CBscore[as.vector(nn_out$nn.idx)]

    voxel_df <-
      rbind(score_exists, score_missing) %>%
      dplyr::arrange(id, .by_group = FALSE)

  }

  return(voxel_df)

}

ccs_to_plane <- function(axis){

  out <- c("x" = "sag", "y" = "axi", "z" = "cor")[axis]

  unname(out)

}

buffer_range <- function(r, buffer){

  r[1] <- r[1]-buffer
  r[2] <- r[2]+buffer

  return(r)

}


circular_progress_plot <- function(voxel_df, score_set_up) {

  voxel_df <- make_CBscore_label(voxel_df, score_set_up)

  df_summary <- voxel_df %>%
    dplyr::count(CBscore_label) %>%
    dplyr::mutate(
      fraction = n / sum(n),
      cumulative = cumsum(fraction),
      ymin = lag(cumulative, default = 0),
      ymax = cumulative
    )

  # Add missing factor levels explicitly (ensures unused labels still show in legend)
  missing_levels <- setdiff(names(score_set_up$choices), df_summary$CBscore_label)
  if (length(missing_levels) > 0) {
    df_summary <- dplyr::bind_rows(
      df_summary,
      tibble::tibble(CBscore_label = missing_levels, fraction = 0, ymin = 0, ymax = 0, n = 0)
    )
  }

  # Ensure correct ordering of factor levels in df_summary
  df_summary$CBscore_label <- factor(df_summary$CBscore_label, levels = names(score_set_up$choices))

  perc <- sum(voxel_df$CBscore_label != names(score_set_up$choices)[1]) / nrow(voxel_df)
  perc <- paste0(round(perc * 100, 0), "%")  # Ensures percentage formatting

  # Create the optimized donut plot with ordered legend and center text
  ggplot2::ggplot(
    data = df_summary,
    mapping = ggplot2::aes(ymin = ymin, ymax = ymax, xmin = 0.8, xmax = 1.6, fill = CBscore_label)
  ) +
    ggplot2::geom_rect(color = "black") +
    ggplot2::coord_polar(theta = "y") +  # Convert to polar coordinates
    ggplot2::theme_void() +  # Remove background, axis, and grid
    ggplot2::scale_fill_manual(
      values = set_names(nm = names(score_set_up$choices), x = score_set_up$colors),
      drop = FALSE,  # Ensures unused levels appear in the legend
      limits = names(score_set_up$choices),  # Ensures correct order in legend
      guide = ggplot2::guide_legend(override.aes = list(alpha = 1, color = NA))  # Ensure visibility
    ) +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 14),  # Increase legend text size
      legend.title = ggplot2::element_text(size = 16, face = "bold"),  # Increase legend title size
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm")  # Minimize outer margins
    ) +
    ggplot2::labs(fill = "Score") +
    ggplot2::xlim(0, 2)
}




# voxels in voxel_df are related to voxels in voxel_ref
# -> voxel_ref contains the voxels corresponding to the tissue around which a safety margin is computed
# -> voxel_margin contains the voxels corresponding to the tissue considered for the safety margin

comp_dist_to_closest_voxel <- function(voxel_margin, voxel_ref){

  # ensure that brain tissue considered for the margin is not part
  # of the reference tissue -> prevent self-matching
  voxel_margin <- voxel_margin[!voxel_margin$id %in% voxel_ref$id, ]

  if(nrow(voxel_margin) != 0 & nrow(voxel_ref) != 0){

    nn_out <-
      RANN::nn2(
        data = as.matrix(voxel_ref[, ccs_labels]),
        query = as.matrix(voxel_margin[, ccs_labels]),
        searchtype = "priority",
        k = 1
      )

    voxel_margin$dist <- as.numeric(nn_out$nn.dists[,1])

  } else {

    voxel_margin$dist <- NA

  }

  return(voxel_margin)

}

comp_outline_bb <- function(outlines){

  prel <-
    purrr::imap(
      .x = outlines,
      .f = function(outline, plane){

        ra_ccs <- req_axes_2d(plane, ccs_val = FALSE)

        dplyr::rename(outline, !!!ra_ccs) %>%
          purrr::map(.f = ~ round(range(.x)))

      }
    )  %>% purrr::flatten()

  out <- list()

  out$x <-
    purrr::flatten_int(prel[names(prel) == "x"]) %>%
    range()

  out$y <-
    purrr::flatten_int(prel[names(prel) == "y"]) %>%
    range()

  out$z <-
    purrr::flatten_int(prel[names(prel) == "z"]) %>%
    range()

  return(out)

}


comp_progress_values <- function(voxel_df, var, values){

  if(!is.character(values)){

    values <- purrr::flatten_chr(values)

  }

  out <-
    purrr::map_dbl(
      .x = unname(values),
      .f = function(val){

        vals <- c(val, paste0("wma_", val))

        has_score <- sum(voxel_df[["CBscore"]][voxel_df[[var]] %in% vals] != 0)

        needs_score <- sum(voxel_df[[var]] %in% vals)

        has_score/needs_score

      }
    )

  names(out) <- unname(values)

  return(out)

}

comp_selection_bb <- function(voxel_df, distance = NULL){

  purrr::map(
    .x = ccs_labels,
    .f = function(axis){

      range(voxel_df[voxel_df$selected,][[axis]], na.rm = TRUE) %>%
        buffer_range(buffer = distance)

    }
  ) %>%
    purrr::set_names(nm = ccs_labels)

}

#' @export
ConsensusBrain <- function(nifti_object = NULL){

  # draw from package data
  if(is.null(nifti_object)){ nifti_object <- mni_template }

  shiny::shinyApp(
    ui = ConsensusBrainUI,
    server = function(input, output, session){

      ConsensusBrainServer(
        input = input,
        output = output,
        session = session,
        nifti_object = nifti_object
      )

    }
  )

}

#' @export
ConsensusBrain2 <- function() {
  shiny::shinyApp(
    ui = fluidPage(h2("🚀 Hello from ConsensusBrain!")),
    server = function(input, output, session) {}
  )
}



CBscore_label_var <- function(voxel_df, score_set_up){

  voxel_df$CBscore_label <- ""

  for(choice in names(score_set_up$choices)){

    val <- score_set_up$choices[choice]

    voxel_df$CBscore_label[voxel_df$CBscore == val] <- choice

  }

  return(voxel_df)

}


# Function to check whether a color subset is dark or bright
determine_color_theme <- function(colors_check,
                                  color_dark = "white",
                                  color_bright = "black",
                                  threshold = 0.5) {

  # Convert colors to RGB values
  rgb_values <- col2rgb(colors_check) / 255  # Normalize RGB values between 0 and 1

  # Compute relative luminance using standard perception formula
  luminance <- 0.2126 * rgb_values[1, ] + 0.7152 * rgb_values[2, ] + 0.0722 * rgb_values[3, ]

  # Compute the average luminance of the selected colors
  avg_luminance <- mean(luminance)

  # Define threshold (0.5 is commonly used)
  ifelse(avg_luminance < threshold, color_dark, color_bright)

}

enter_refinement_mode <- function(voxel_df){

  dplyr::mutate(
    .data = voxel_df,
    color = dplyr::if_else(selected, alpha(color, 0.55), alpha("steelblue", 0.45))
  )

}

exchange_raster_colors <- function(mri_list,
                                   voxel_df,
                                   color_new,
                                   color_weights = NULL,
                                   clrp = list(),
                                   clrsp = list(),
                                   clrp_adjust = NULL, # takes a list
                                   clrp_adjust2 = NULL, # takes a vector
                                   verbose = TRUE){

  color_list <-
    purrr::map(
      .x = color_new,
      .f = function(cn){

        if(cn %in% colnames(voxel_df)){

          if(is.numeric(voxel_df[[cn]])){

            if(is.null(clrsp[[cn]])){

              clrsp_in <- c("black", "white")

            } else {

              clrsp_in <- clrsp[[cn]]

            }

            out <-
              map_values_to_colors(values = voxel_df[[cn]], clrsp = clrsp_in)

          } else {

            if(is.logical(voxel_df[[cn]])){

              voxel_df[[cn]] <-
                factor(as.character(voxel_df[[cn]]), levels = c("TRUE", "FALSE"))

            } else if(is.character(voxel_df[[cn]])){

              voxel_df[[cn]] <-
                factor(voxel_df[[cn]], levels = sort(unique(voxel_df[[cn]])))

            }

            cp_adj <- clrp_adjust[[cn]]

            if(!is.null(clrp_adjust2)){

              exchange_names <-
                names(clrp_adjust2)[names(clrp_adjust2) %in% levels(voxel_df[[cn]])]

              for(nm in exchange_names ){

                if(!is.null(cp_adj)){

                  cp_adj[nm] <- clrp_adjust2[nm]

                } else { # initiate chr vector

                  cp_adj <- character()
                  cp_adj[nm] <- clrp_adjust2[nm]

                }

              }

            }

            cvec <-
              confuns::color_vector(
                clrp = ifelse(is.null(clrp[[cn]]), "default", clrp[[cn]]),
                names = levels(voxel_df[[cn]]),
                clrp.adjust = cp_adj
              )

            out <- unname(cvec[voxel_df[[cn]]])

          }

        } else if(confuns::is_color(cn)){

          out <- cn

        } else {

          stop(glue::glue("Value {cn} is an invalid in put for `color_new`."))

        }

      }
    )

  voxel_df$color <- blend_colors(color_list = color_list, weights = color_weights)

  mri_list_out <-
    purrr::imap(
      .x = mri_list,
      .f = function(plane_list, plane){

        al <- switch_axis_label(plane)

        req_axes <- req_axes_2d(plane)

        slices <- sort(unique(voxel_df[[al]]))

        plane_list[slices] <-
          purrr::map(
            .x = slices,
            .f = function(slice){

              slice_rst <- plane_list[[slice]]

              slice_df <- voxel_df[voxel_df[[al]]==slice,]

              slice_rst[cbind(slice_df[[req_axes["row"]]], slice_df[[req_axes["col"]]])] <- slice_df$color

              return(slice_rst)

            }
          )

        return(plane_list)

      }
    )

}



comp_progress <- function(voxel_df){

  floor((sum(voxel_df$CBscore != 0)/nrow(voxel_df))*100)

}

get_slice <- function(nifti, plane, slice){

  if(plane == "sag"){

    t(nifti[slice,,])

  } else if(plane == "axi"){

    t(nifti[,,slice])

  } else if(plane == "cor"){

    t(nifti[,slice,])

  }

}

ggpLayer_MRI <- function(lim_col = 256, lim_row = 256, lab_fill = NULL, ...){

  list(
    ggplot2::coord_fixed(
      ratio = lim_col/lim_row,
      xlim = c(1,lim_col),
      ylim = c(1,lim_row),
      ...),
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.grid = ggplot2::element_blank()
      ),
    ggplot2::labs(x = "Column", y = "Row", fill = lab_fill)
  )

}

ggpLayer_slice <- function(slice_df,
                           color_by = "value",
                           clrp = "default",
                           clrp_adjust = NULL,
                           clrsp = c("black", "white"),
                           ...){

  if(is.character(color_by)){

    if(is.logical(slice_df[[color_by]])){

      slice_df[[color_by]] <- as.character(slice_df[[color_by]])

    }

    mapping <- ggplot2::aes(x = col, y = row, fill = .data[[color_by]])

    out <-
      list(
        ggnewscale::new_scale_fill(),
        ggplot2::geom_raster(data = slice_df, mapping = mapping, ...),
        confuns::scale_color_add_on(
          aes = "fill",
          variable = slice_df[[color_by]],
          clrp = clrp,
          clrp.adjust = clrp_adjust,
          clrsp = clrsp
        )
      )

  } else {

    mapping <- ggplot2::aes(x = col, y = row)

    out <-
      list(
        ggplot2::geom_raster(data = slice_df, mapping = mapping, ...)
      )

  }

  return(out)

}


identify_brois <- function(voxel_df){

  voxel_df$broi <- NA_character_
  voxel_subset_sel <- dplyr::filter(voxel_df, selected)

  hemispheres <- unique(voxel_subset_sel$hemisphere)

  voxel_subset_sel <-
    purrr::map_dfr(
      .x = hemispheres,
      .f = function(hem){

        voxel_hem <- voxel_subset_sel[voxel_subset_sel$hemisphere == hem,]

        dbscan_out <-
          dbscan::dbscan(
            x = as.matrix(voxel_hem[,c("x", "y", "z")]),
            minPts = 2,
            eps = 2
          )

        voxel_hem <-
          dplyr::mutate(
            .data = voxel_hem,
            broi = paste0("broi_", dbscan_out$cluster),
            broi = dplyr::if_else(broi == "broi_0", true = NA, false = broi)
          ) %>%
          dplyr::filter(!is.na(broi)) %>%
          dplyr::mutate(broi = paste0(broi, "_", {{hem}}))

        return(voxel_hem)

      }
    )

  voxel_df <- rbind(voxel_subset_sel, voxel_df[!voxel_df$selected,])

  return(voxel_df)

}

identify_debris <- function(voxel_df, eps = 1.5, minPts = 8, min_size = 250){

  voxel_selection <- dplyr::filter(voxel_df, selected)

  dbscan_out <- dbscan::dbscan(x = voxel_selection[,c("x", "y", "z")], eps = eps, minPts = minPts)

  voxel_selection$debris <- as.character(dbscan_out$cluster)

  debris_cluster <-
    dplyr::filter(voxel_selection, debris != "0") %>%
    dplyr::group_by(debris) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(count <= {{min_size}}) %>%
    dplyr::pull(debris)

  voxel_selection <-
    dplyr::mutate(
      .data = voxel_selection,
      debris = dplyr::if_else(debris %in% {{debris_cluster}}, "0", debris)
    )

  dplyr::left_join(
    x = voxel_df,
    y = voxel_selection[,c("id", "debris")],
    by = "id"
  )

}

identify_edge_voxels <- function(selection_mask, slice_template, selection_scope = "smart_label"){

  slice_template[[selection_scope]][!slice_template$is_brain] <- NA

  nn_out <-
    RANN::nn2(
      data = slice_template[, c("col", "row")],
      query = selection_mask[selection_mask$is_brain & selection_mask$selected,c("col", "row")],
      k = 9
    )

  label_mtr <-
    matrix(
      data = as.numeric(is.na(slice_template[[selection_scope]][nn_out$nn.idx])),
      nrow = nrow(nn_out$nn.idx),
      ncol = 9
    )

  label_mtr <- label_mtr[,2:9]

  rownames(label_mtr) <- selection_mask$id[selection_mask$is_brain & selection_mask$selected]

  selection_mask$is_edge <-
    selection_mask$id %in% rownames(label_mtr)[rowSums(label_mtr) >=2] &
    selection_mask$is_brain

  return(selection_mask)

}

identify_obs_in_outlines <- function(voxel_df, outlines){

  outlines <- purrr::keep(outlines, .p = ~ nrow(.x) > 3)

  bb <- comp_outline_bb(outlines)

  outline_sub <-
    dplyr::filter(
      .data = voxel_df,
      x > min(bb$x) & x < max(bb$x) &
        y > min(bb$y) & y < max(bb$y) &
        z > min(bb$z) & z < max(bb$z)
    )

  ids_all <-
    purrr::imap(
      .x = outlines,
      .f = function(outline, plane){

        axis_plane <- switch_axis_label(plane)
        ra_ccs <- req_axes_2d(plane)

        slices <- sort(unique(outline_sub[[axis_plane]]))

        plane_ids <-
          purrr::map(
            .x = slices,
            .f = function(slice){

              slice_df <-
                dplyr::filter(outline_sub, !!rlang::sym(axis_plane) == {{slice}}) %>%
                dplyr::select(id, !!!ra_ccs)

              identify_obs_in_polygon(
                interaction_template = slice_df,
                polygon_df = outline,
                strictly = FALSE,
                opt = "keep"
              )[["id"]]

            }
          ) %>%
          purrr::flatten_chr()

        return(plane_ids)

      }
    ) %>%
    purrr::flatten_chr()


  ids_count <- table(ids_all)
  ids_selected <- names(ids_count[ids_count == length(outlines)])

  return(ids_selected)

}


identify_obs_in_polygon <- function(interaction_template,
                                    polygon_df,
                                    strictly,
                                    opt = "keep"){

  res <-
    sp::point.in.polygon(
      point.x = interaction_template[["col"]],
      point.y = interaction_template[["row"]],
      pol.x = polygon_df[["col"]],
      pol.y = polygon_df[["row"]]
    )

  inside <- if(base::isTRUE(strictly)){ 1 } else { c(1,2,3) }

  if(opt == "keep"){

    interaction_template <- interaction_template[res %in% inside, ]

  } else if(opt == "remove"){

    interaction_template <- interaction_template[!res %in% inside, ]

  } else {

    interaction_template[[opt]] <- res %in% inside

  }

  return(interaction_template)

}

identify_obs_within_radius2D <- function(cursor_pos,
                                         radius,
                                         interaction_template,
                                         preselected_ids = character(),
                                         selection_scope = NULL) {

  interaction_template[!interaction_template$id %in% preselected_ids, ]

  distances <-
    (interaction_template$col - cursor_pos[1])^2 +
    (interaction_template$row - cursor_pos[2])^2

  if(is.character(selection_scope)){

    current_label <- interaction_template[which(round(distances)==0)[1], ][[selection_scope]]
    current_hem <- interaction_template[which(round(distances) ==0)[1], ][["hemisphere"]]

    same_label <- interaction_template[[selection_scope]] == current_label
    same_hem <- interaction_template$hemisphere == current_hem
    within_rad <- distances <= radius^2

    indices_ret <- which((same_label & same_hem & within_rad) | distances == 0)

    return(interaction_template$id[indices_ret])

  } else {

    return(interaction_template$id[which(distances <= radius^2)])

  }

}


identify_obs_within_radius3D <- function(cursor_pos,
                                         radius,
                                         interaction_template,
                                         selection_scope = NULL) {

  distances <-
    (interaction_template[["x"]] - cursor_pos["x"])^2 +
    (interaction_template[["y"]] - cursor_pos["y"])^2 +
    (interaction_template[["z"]] - cursor_pos["z"])^2

  if(is.character(selection_scope)){

    current_label <- interaction_template[which(round(distances)==0)[1], ][[selection_scope]]
    current_hem <- interaction_template[which(round(distances) ==0)[1], ][["hemisphere"]]

    same_label <- interaction_template[[selection_scope]] == current_label
    same_hem <- interaction_template$hemisphere == current_hem
    within_rad <- distances <= radius^2

    indices_ret <- which((same_label & same_hem & within_rad) | distances == 0)

    return(interaction_template$id[indices_ret])


  } else {

    return(interaction_template$id[which(distances <= radius^2)])

  }

}

identify_obs_within_safety_margin <- function(voxel_df,
                                              selected_voxels,
                                              distance,
                                              restrict_hemisphere = TRUE){

  voxel_ref <- dplyr::filter(voxel_df, id %in% {{selected_voxels}})

  bb <-
    purrr::map(
      .x = voxel_ref[,c("x", "y", "z")],
      .f = ~ buffer_range(r = range(.x), buffer = distance)
    )

  voxel_margin <-
    dplyr::filter(
      .data = voxel_df,
      # not part of selected voxels
      !id %in% {{selected_voxels}} &
        # within bounding box of potential distance
        x > bb$x[[1]] & x < bb$x[[2]] &
        y > bb$y[[1]] & y < bb$y[[2]] &
        z > bb$z[[1]] & z < bb$z[[2]]
    )

  if(isTRUE(restrict_hemisphere)){

    voxel_margin <- voxel_margin[voxel_margin$hemisphere %in% unique(voxel_ref$hemisphere),]

  }

  # compute distance
  voxel_margin <-
    comp_dist_to_closest_voxel(voxel_margin, voxel_ref) %>%
    dplyr::filter(dist <= {{distance}})

  return(voxel_margin)

}

identify_updated_voxels <- function(new, old){

  if(length(new) > length(old)){

    ids <- new[!new %in% old]

  } else if(length(new) < length(old)) {

    ids <- old[!old %in% new]

  } else {

    ids <- setdiff(x = new, y = old)

  }

  return(ids)

}

load_consensus_template <- function(as_df = TRUE, t1 = TRUE, subcortical = TRUE){

  data("consensus_template")

  if(as_df){

    consensus_template <-
      cbind(
        as.data.frame(consensus_template$numeric),
        as.data.frame(consensus_template$labels)
      ) %>%
      dplyr::left_join(x = ., y = consensus_template$ann_macro, by = "ann_dk_adj") %>%
      dplyr::mutate(
        hemisphere = dplyr::if_else(x > 127, true = "left", false = "right"),
        id = dplyr::row_number()
      ) %>%
      tibble::as_tibble()

    if(isFALSE(t1)){ consensus_template$t1 <- NULL }

    if(isTRUE(subcortical)){

      consensus_template <-
        dplyr::mutate(
          .data = consensus_template,
          subcortical =
            dplyr::case_when(
             stringr::str_detect(ann_macro, pattern = "subcortical|corpus_callosum") ~ ann_dk_adj,
            stringr::str_detect(ann_macro, pattern = "wm_tract") ~ ann_macro,
           TRUE ~ "none"
          )
        )

    }

  }

  return(consensus_template)

}

load_non_brain_template <- function(){

  data("non_brain_template")

  dplyr::mutate(
    .data = tibble::as_tibble(non_brain_template),
    hemisphere = dplyr::if_else(x > 127, true = "left", false = "right")
  )

}

make_CBscore_label <- function(voxel_df, score_set_up){

  voxel_df$CBscore_label <- ""

  for(i in seq_along(score_set_up$choices)){

    value <- score_set_up$choices[i]
    name <- names(score_set_up$choices)[i]
    voxel_df$CBscore_label[voxel_df$CBscore == value] <- name

  }

  voxel_df$CBscore_label <- factor(voxel_df$CBscore_label, levels = names(score_set_up$choices))

  return(voxel_df)

}

make_pretty_label <- function(labels){

  pretty <-
    stringr::str_replace_all(string = labels, pattern = "CC", replacement = "C.Callosum") %>%
    stringr::str_replace_all(string = ., pattern = "G_", replacement = "Gyr._") %>%
    stringr::str_replace_all(string = ., pattern = "S_", replacement = "Sulc._") %>%
    stringr::str_replace_all(string = ., pattern = "_|-", replacement = " ")  %>%
    stringr::str_replace_all(string = ., pattern = "_", replacement = " ") %>%
    stringr::str_to_title(string = .)  %>%
    stringr::str_replace_all(string = ., pattern = "^Wma", replacement = "WM Ass. With") %>%
    stringr::str_replace_all(string = ., pattern = "^Wmt", replacement = "WM-Tract")

  return(pretty)

}

map_refinement_mask_to_broi <- function(mask,
                                        slice,
                                        plane,
                                        voxel_df){

  axis_ccs <- switch_axis_label(plane)
  ra_ccs <- req_axes_2d(plane)

  broi_mask <-
    voxel_df[voxel_df[[axis_ccs]] == slice, ] %>%
    dplyr::filter(!is.na(broi)) %>%
    dplyr::select(id, broi, !!!ra_ccs)

  nn_out <-
    RANN::nn2(
      data = broi_mask[, c("col", "row")],
      query = mask[, c("col", "row")],
      searchtype = "priority",
      k = 1
    )

  mask$broi <- broi_mask$broi[nn_out$nn.idx]

  mapped_brois <- sort(unique(mask$broi))

  out <-
    purrr::map(
      .x = mapped_brois,
      .f = ~ mask[mask$broi == .x, ]
    ) %>%
    purrr::set_names(nm = mapped_brois)

  return(out)

}

map_values_to_colors <- function(values, clrsp = "Viridis", n = 100){

  seq_clrsp <-
    purrr::flatten_chr(confuns::all_color_spectra()[c("Sequential single hue", "Sequential multi hue")])

  norm_values <-
    (values - min(values, na.rm = TRUE)) /
    (max(values, na.rm = TRUE) - min(values, na.rm = TRUE))

  if(length(clrsp) == 1 && clrsp %in% seq_clrsp){

    col_fun <- colorspace::sequential_hcl(n, palette = clrsp)

    colors <- col_fun[ceiling(norm_values * (n - 1)) + 1]

  } else if(length(clrsp) == 1 && confuns::is_color(clrsp)){

    ramp <- grDevices::colorRamp(c(alpha("white", 0), clrsp))

    col_out <- ramp(norm_values)

    colors <-
      grDevices::rgb(
        red = col_out[,1],
        green = col_out[,2],
        blue = col_out[,3],
        maxColorValue = 255
      )

  } else if(length(clrsp) == 2 && all(confuns::is_color(clrsp))){

    ramp <- grDevices::colorRamp(clrsp)

    col_out <- ramp(norm_values)

    colors <-
      grDevices::rgb(
        red = col_out[,1],
        green = col_out[,2],
        blue = col_out[,3],
        maxColorValue = 255
      )

  } else {

    stop("Invalid input for `clrsp`.")

  }

  return(colors)

}

plot_mri_frame <- function(col,
                           row,
                           type = "n",
                           color = NULL,
                           xlim = NULL,
                           ylim = NULL,
                           bg = NA,
                           ...){

  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = bg)

  # default to col/row; allow specifics if drawing
  if(is.null(xlim)){ xlim <- range(col) }
  if(is.null(ylim)){ ylim <- rev(range(row)) }

  plot(
    x = col,
    y = row,
    type = type,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    xlab = "",
    ylab = "",
    xlim = xlim,
    ylim = ylim,
    col = color,
    ...
  )

}

plane_to_ccs <- function(plane){

  out <- c("sag" = "x", "axi" = "y", "cor" = "z")[plane]

  unname(out)

}

prepare_margin_selection <- function(voxel_df, dist_max, voxels_margin){

  voxel_df <- identify_brois(voxel_df)

  brois <- unique(voxel_df$broi)
  brois <- brois[!is.na(brois)]

  cand_df <-
    purrr::map_df(
      .x = brois,
      .f = function(broi){

        voxel_broi <- dplyr::filter(voxel_df, broi == {{broi}} & !id %in% {{voxels_margin}})

        voxel_margin <- dplyr::filter(voxel_df, !selected)

        hemisphere_vals <-
          dplyr::pull(voxel_broi, hemisphere) %>%
          unique()

        if(length(hemisphere_vals) == 1){

          voxel_margin <- dplyr::filter(voxel_margin, hemisphere == hemisphere_vals)

        }

        bb <- comp_selection_bb(voxel_broi, distance = dist_max)

        voxel_margin <-
          dplyr::filter(
            .data = voxel_margin,
            # within bounding box of potential distance
            x > bb$x[[1]] & x < bb$x[[2]] &
            y > bb$y[[1]] & y < bb$y[[2]] &
            z > bb$z[[1]] & z < bb$z[[2]]
          )

        voxel_margin <-
          comp_dist_to_closest_voxel(voxel_margin, voxel_ref = voxel_broi) %>%
          dplyr::filter(dist <= {{dist_max}})

        return(voxel_margin[, c("id", "dist")])

      }
    ) %>%
    dplyr::distinct()

  return(cand_df)

}

propagate_selection_unscoped <- function(voxel_df,
                                         selection_mask,
                                         selection_plane,
                                         selection_slices = NULL,
                                         erase = FALSE){


  # 2D -> 3D
  ccs_axis <- switch_axis_label(selection_plane)
  rename_inp <- req_axes_2d(selection_plane, ccs_val = FALSE)

  # prepare slice vector
  if(is.null(selection_slices)){

    selection_slices <- unique(voxel_df[[ccs_axis]])

  }

  selection_mask <-
    dplyr::filter(selection_mask, selected) %>%
    dplyr::mutate(selection_flag = !{{erase}}) %>%
    dplyr::rename(!!!rename_inp)

  voxel_df <-
    dplyr::left_join(
      x = voxel_df,
      y = selection_mask[,c(names(rename_inp), "selection_flag")],
      by = names(rename_inp)
    ) %>%
    dplyr::mutate(
      selected = dplyr::if_else(
        condition = !is.na(selection_flag) & !!rlang::sym(ccs_axis) %in% {{selection_slices}},
        true = selection_flag,
        false = selected
      ),
      selection_flag = NULL
    )

  return(voxel_df)

}

propagate_selection_scoped <- function(voxel_df,
                                       selection_mask,
                                       selection_plane,
                                       selection_slice,
                                       selection_scope,
                                       selection_slices = NULL,
                                       erase = FALSE){

  if(FALSE){

    assign("voxel_df", voxel_df, envir = .GlobalEnv)
    assign("selection_mask", selection_mask, envir = .GlobalEnv)
    assign("selection_plane", selection_plane, envir = .GlobalEnv)
    assign("selection_slice", selection_slice, envir = .GlobalEnv)
    assign("selection_slices", selection_slices, envir = .GlobalEnv)
    assign("selection_scope", selection_scope, envir = .GlobalEnv)

  }

  # at the beginning
  empty_slice_template <- tidyr::expand_grid(col = 1:256, row = 1:256)
  ccs_axis <- switch_axis_label(selection_plane)
  ra_ccs <- req_axes_2d(selection_plane, mri = FALSE)

  voxel_df <-
    dplyr::add_row(
      .data = dplyr::mutate(voxel_df, non_brain = FALSE),
      dplyr::mutate(load_non_brain_template(), non_brain = TRUE)
    )

  selection_mask <- dplyr::filter(selection_mask, selected)
  selected_hemispheres <- unique(selection_mask$hemisphere)

  scope_labels <- unique(selection_mask[[selection_scope]])

  ids_by_scope_label <-
    purrr::map(
      .x = scope_labels,
      .f = function(scope_label){

        scope_label_df <-
          voxel_df[voxel_df[[selection_scope]] == scope_label & voxel_df$hemisphere %in% selected_hemispheres,] %>%
          dplyr::rename(!!!ra_ccs)

        scope_label_mask <- selection_mask[selection_mask[[selection_scope]] == scope_label,]

        # prepare upper and lower lists
        # use max(slices_with_tissue), cause masks are picked by numeric slice idx!
        if(!is.numeric(selection_slices)){

          selection_slices_iter <- sort(unique(scope_label_df[[ccs_axis]]))

        } else {

          selection_slices_iter <- selection_slices

          # prevent hemisphere-transgression in case of preselected selection_slices
          # by paintbrush mode 'Beam' with depth > hemisphere end
          if(selection_plane == "sag"){

            selection_slices_iter <-
              # scope_label_df is subsetted by 'selected_hemispheresÄ
              # in sagital plane you can only draw on one hemisphere at a time
              selection_slices_iter[selection_slices_iter %in% scope_label_df$x]

          }

        }

        selection_masks <- vector(mode = "list", length = max(selection_slices_iter))
        selection_masks[[selection_slice]] <- scope_label_mask

        # iterate over lower list
        # slice input -> down to lowest
        if(selection_slice != min(selection_slices_iter)){

          slices_lower <- (selection_slice-1):min(selection_slices_iter)

          for(i in seq_along(slices_lower)){

            slice_idx <- slices_lower[i]

            ## prepare current candidates df
            cand_df <- scope_label_df[scope_label_df[[ccs_axis]] == slice_idx, ]

            if(nrow(cand_df) == 0){ break }

            ## get previous selection_mask
            prev_smask <- selection_masks[[slice_idx+1]]

            nn_out <-
              RANN::nn2(
                data = cand_df[,c("col", "row", ccs_axis)],
                query = prev_smask[,c("col", "row", ccs_axis)],
                k = 1
              )

            ids <- unique(cand_df$id[nn_out$nn.idx][nn_out$nn.dists < 1.5])

            ### assemble current selection_mask
            cand_df$selected <- cand_df$id %in% ids

            new_selection_mask <- cand_df[cand_df$selected,]

            if(nrow(new_selection_mask) == 0){ break }

            selection_masks[[slice_idx]] <- new_selection_mask

          }

        }

        # iterate over upper list
        # slice input -> up to highest
        if(selection_slice != max(selection_slices_iter)){

          slices_higher <- (selection_slice+1):max(selection_slices_iter)

          for(i in seq_along(slices_higher)){

            slice_idx <- slices_higher[i]

            ## prepare current candidates df
            cand_df <- scope_label_df[scope_label_df[[ccs_axis]] == slice_idx, ]

            if(nrow(cand_df) == 0){ break }

            ## get previous selection_mask
            prev_smask <- selection_masks[[slice_idx-1]]

            nn_out <-
              RANN::nn2(
                data = cand_df[,c("col", "row", ccs_axis)],
                query = prev_smask[,c("col", "row", ccs_axis)],
                k = 1
              )

            ids <- unique(cand_df$id[nn_out$nn.idx][nn_out$nn.dists < 1.5])

            ### assemble current selection_mask
            cand_df$selected <- cand_df$id %in% ids

            new_selection_mask <- cand_df[cand_df$selected,]

            if(nrow(new_selection_mask) == 0){ break }

            selection_masks[[slice_idx]] <- new_selection_mask

          }

        }

        ids_out <-
          purrr::discard(selection_masks, .p = is.null) %>%
          purrr::map(.f = ~ .x[["id"]]) %>%
          purrr::flatten_int()

        return(ids_out)

      }
    )

  # extract and apply selection
  ids_all <- purrr::flatten_int(ids_by_scope_label)

  # remove non brain voxels ()
  voxel_df <- dplyr::filter(voxel_df, !non_brain)

  if(erase){

    voxel_df$selected <- voxel_df$selected & !voxel_df$id %in% ids_all

  } else {

    voxel_df$selected <- voxel_df$selected | voxel_df$id %in% ids_all

  }

  print("Done.")

  return(voxel_df)

}


propagate_selection_3D <- function(voxel_df,
                                   selection_mask,
                                   selection_plane,
                                   selection_slice,
                                   selection_scope = NULL,
                                   selection_slices = NULL,
                                   erase = FALSE){

  if(is.null(selection_scope)){

    propagate_selection_unscoped(
      voxel_df = voxel_df,
      selection_mask = selection_mask,
      selection_plane = selection_plane,
      selection_slices = selection_slices,
      erase = erase
    )

  } else {

    propagate_selection_scoped(
      voxel_df = voxel_df,
      selection_mask = selection_mask,
      selection_plane = selection_plane,
      selection_slice = selection_slice,
      selection_scope = selection_scope,
      selection_slices = selection_slices,
      erase = erase
    )

  }

}




# goal: propagate the voxels to be erased along the slice levels by mapping
# the selection state of the erase mask voxels
# of the resepective previous slice to the tissue voxels of the current slice
# this results in a list of broi masks where erased == TRUE indicates whether
# the voxel should be erased according to the 2D erase mask
# after the loop, the erased == TRUE voxels of the list are erased
# from the input voxel data.frame by setting selected = FALSE

# erase_mask_input: A slice data.frame that corresponds to the broi mask.
# vars: id, col, row, selected, erased
# selected indicates whether the voxel is part of the broi
# erased indicates whether the voxel of the broi mask was erased

refine3D_erase <- function(voxel_df,
                           broi,
                           erase_mask,
                           erase_mask_slice,
                           erase_mask_plane){

  ccs_axis <- switch_axis_label(erase_mask_plane)
  ra_ccs <- req_axes_2d(erase_mask_plane, mri = FALSE)

  slices_with_tissue <-
    unique(voxel_df[voxel_df$broi == broi & !is.na(voxel_df$broi),][[ccs_axis]])

  # use 1:max(slices_with_tissue), cause masks are picked by numeric slice idx!
  erase_masks <- vector(mode = "list", length = max(slices_with_tissue))
  erase_masks[[erase_mask_slice]] <- erase_mask[erase_mask$selected,]

  # slice input -> down to lowest
  if(erase_mask_slice != min(slices_with_tissue)){

    slices_lower <- (erase_mask_slice-1):min(slices_with_tissue)

    for(i in seq_along(slices_lower)){

      slice_idx <- slices_lower[i]

      # get broi mask
      slice_df <-
        dplyr::select(
          .data = voxel_df[voxel_df[[ccs_axis]] == slice_idx,],
          id, broi, selected, !!!ra_ccs
        )

      broi_mask <- slice_df[slice_df$selected & slice_df$broi == broi & !is.na(slice_df$broi),]

      # get erase mask of previous slice
      # +1 to get from previous slice, cause the slice index decreases with the loop
      erase_mask_prev <- erase_masks[[(slice_idx+1)]]

      # map broi voxels in current slice to the selection state in the previous slice
      nn_out <-
        RANN::nn2(
          data = erase_mask_prev[, c("col", "row")],
          query = broi_mask[, c("col", "row")],
          searchtype = "priority",
          k = 1
        )

      # transfer label, erased = TRUE -> erase
      broi_mask$erased <- erase_mask_prev$erased[nn_out$nn.idx]
      erase_masks[[slice_idx]] <- broi_mask

    }

  }

  # slice input -> up to highest
  if(erase_mask_slice != max(slices_with_tissue)){

    slices_higher <- (erase_mask_slice+1):max(slices_with_tissue)

    for(i in seq_along(slices_higher)){

      slice_idx <- slices_higher[i]

      # get broi mask
      slice_df <-
        dplyr::select(
          .data = voxel_df[voxel_df[[ccs_axis]] == slice_idx,],
          id, broi, selected, !!!ra_ccs
        )

      broi_mask <- slice_df[slice_df$selected & slice_df$broi == broi & !is.na(slice_df$broi),]

      # get erase mask of previous slice
      # +1 to get from previous slice, cause the slice index decreases with the loop
      erase_mask_prev <- erase_masks[[(slice_idx-1)]]

      # map broi voxels in current slice to the selection state in the previous slice
      nn_out <-
        RANN::nn2(
          data = erase_mask_prev[, c("col", "row")],
          query = broi_mask[, c("col", "row")],
          searchtype = "priority",
          k = 1
        )

      # transfer label, erased = TRUE -> erase
      broi_mask$erased <- erase_mask_prev$erased[nn_out$nn.idx]
      erase_masks[[slice_idx]] <- broi_mask

    }

  }

  erase_ids <-
    purrr::discard(erase_masks, .p = is.null) %>%
    purrr::map(.f = ~ .x[.x$erased, ][["id"]]) %>%
    purrr::flatten_chr()

  voxel_df$selected <- voxel_df$selected & !voxel_df$id %in% erase_ids

  return(voxel_df)

}


req_axes_2d <- function(plane, mri = FALSE, ccs_val = TRUE){

  if(plane == "sag"){

    out <- c("col" = "z", "row" = "y")

  } else if(plane == "axi"){

    out <- c("col" = "x", "row" = "z")

  } else if(plane == "cor"){

    out <- c("col" = "x", "row" = "y")

  } else {

    stop("Invalid `orientation` input. ")

  }

  if(isTRUE(mri)){

    out <- switch_axis_label(out)

  }

  if(!ccs_val){

    out <- purrr::set_names(x = names(out), nm = unname(out))

  }

  return(out)

}

switch_axis_label <- function(label){

  purrr::map_chr(
    .x = label,
    .f = function(l){

      if(l %in% mri_planes){

        plane_to_ccs(l)

      } else if(l %in% ccs_labels) {

        ccs_to_plane(l)

      }

    }
  )

}


plot_brain_2d <- function(voxel_df,
                          plane,
                          slice,
                          color_by = "t1",
                          clrp = "default",
                          clrp_adjust = NULL,
                          clrsp = c("black", "white"),
                          nrow = NULL,
                          ncol = NULL){

  combinations <- tidyr::expand_grid(planes = {{plane}}, slices = {{slice}})

  out_df <-
    purrr::map2_dfr(
      .x = combinations$planes,
      .y = combinations$slices,
      .f = function(plane, slice){

        get_slice_df(input = voxel_df, plane = plane, slice = slice, var = color_by) %>%
          dplyr::mutate(plane = {{plane}}, slice = {{slice}})

      }
    )

  if(length(plane) > 1 & length(slice) > 1){

    facet_add_on <-
      ggplot2::facet_grid(
        rows = ggplot2::vars(slice),
        cols = ggplot2::vars(plane)
      )

  } else if(length(plane) == 1 & length(slice) == 1) {

    facet_add_on <- NULL

  } else {

    var <- ifelse(length(plane) > length(slice), "plane", "slice")
    form <- paste0(". ~ ", var)

    facet_add_on <- ggplot2::facet_wrap(facets = as.formula(form), nrow = nrow, ncol = ncol)

  }

  ggplot2::ggplot(out_df, mapping = ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_raster(mapping = ggplot2::aes(fill = value)) +
    confuns::scale_color_add_on(
      aes = "fill",
      variable = out_df$value,
      clrp = clrp,
      clrsp = clrsp,
      clrp.adjust = clrp_adjust
    ) +
    facet_add_on +
    ggpLayer_MRI(lab_fill = color_by)


}


plot_brain_3d <- function(voxel_df,
                          color_by,
                          type = c("scatter3d", "mesh3d"),
                          group_highlight = NULL,
                          hoverinfo = NULL,
                          side_highlight = NULL,
                          clrp_adjust = NULL, # predefined, named vectors
                          clrp_adjust2 = NULL, # adjustemnts to clrp_adjust
                          opacity_hide = 0.15,
                          pt_clrsp = "Viridis",
                          pt_size = 1.5,
                          eye = list(x = 0, y = 0, z = 2),
                          show_legend = TRUE,
                          colorful = FALSE,
                          mode = "markers",
                          plot_bgcolor = "white",
                          paper_bgcolor = "white",
                          ...){ # Default frontal perspective

  type <- type[1]
  ## Create dynamic formula for `color`
  color_formula <- as.formula(paste("~", color_by))

  voxel_df$y <- max(voxel_df$y) - voxel_df$y

  if(is.character(hoverinfo)){

    voxel_df$hoverinfo <- ""

    for(var in hoverinfo){

      var_name <-
        stringr::str_remove(var, pattern = "^ann_") %>%
        stringr::str_remove(string = ., pattern = "_adj$") %>%
        stringr::str_to_title()

      if(var != "ann_macro"){ var_name <- toupper(var_name) }

      if(var != color_by){ voxel_df[[var]] <- make_pretty_label(voxel_df[[var]]) }

      voxel_df$hoverinfo <-
        paste0(voxel_df$hoverinfo, "<br>", var_name, ": ", voxel_df[[var]])

    }

  } else {

    voxel_df$hoverinfo <- ""

  }

  ## Generate color palette for categorical variables
  if (!is.numeric(voxel_df[[color_by]])) {

    if(colorful){

      voxel_df[[color_by]] <- factor(voxel_df[[color_by]], levels = sample(unique(voxel_df[[color_by]])))

    }

    unique_labels <- sort(unique(voxel_df[[color_by]]))
    n_labels <- length(unique_labels)
    colors <- scales::hue_pal()(n_labels)
    color_map <- setNames(colors, unique_labels)
    if(!is.null(clrp_adjust)){

      for(n in names(clrp_adjust)){

        color_map[n] <- clrp_adjust[n]

      }

    }

    if(!is.null(clrp_adjust2)){

      for(n in names(clrp_adjust2)){

        color_map[n] <- clrp_adjust2[n]

      }

    }

    voxel_df$color <- color_map[voxel_df[[color_by]]]
    voxel_df[[color_by]] <- factor(voxel_df[[color_by]], levels = unique_labels)

  }

  ## Plot
  if(is.numeric(voxel_df[[color_by]])){

    # Numeric values
    p <- plotly::plot_ly(
      data = voxel_df,
      x = ~x, y = ~z, z = ~y, # exchange z and y axis, cause plotly rotates around z axis
      color = color_formula,
      type = type,
      mode = "markers",
      marker = list(size = pt_size, colorscale = pt_clrsp, colorbar = list(title = color_by))
    )

  } else {

    # Grouped (categorical) values
    p <- plotly::plot_ly()

    for (i in seq_along(unique_labels)) {

      label <- unique_labels[i]
      voxel_df_subset <- filter(voxel_df, !!rlang::sym(color_by) == {{label}})

      if(is.character(group_highlight)){

        if(label %in% group_highlight){

          opacity <- 1

        } else {

          opacity <- opacity_hide

        }

      } else {

        opacity <- 1

      }

      n <- ifelse(nrow(voxel_df_subset) < 700000, nrow(voxel_df_subset), 700000)

      set.seed(123)

      p <- plotly::add_trace(
        p = p,
        data = dplyr::slice_sample(voxel_df_subset, n = n),
        x = ~x, y = ~z, z = ~y, # exchange z and y axis, cause plotly rotates around z axis
        type = type,
        mode = mode,
        text = ~ hoverinfo,
        hoverinfo = "text",
        marker = list(size = pt_size, opacity = opacity, color = color_map[[label]]),
        name = label,
        showlegend = show_legend
      )

    }
  }

  ## Add camera with user-defined `eye`
  p <- plotly::layout(
    p,
    scene = list(
      xaxis = list(title = "Sagittal (x)", range = c(min(voxel_df$x), max(voxel_df$x))),
      yaxis = list(title = "Coronal (z)", range = c(min(voxel_df$z), max(voxel_df$z))), # switch y & z
      zaxis = list(title = "Axial (y)", range = c(min(voxel_df$y), max(voxel_df$y))),
      camera = list(
        eye = eye,
        ...# Use the `eye` argument for dynamic camera positioning
      )
    ),
    paper_bgcolor = paper_bgcolor,
    plot_bgcolor = plot_bgcolor
  )

  return(p)

}

reduce_stack <- function(stacks, which){

  if(length(stacks[[which]]) == 0){

    # nothing

  } else if(length(stacks[[which]]) == 1){

    stacks[[which]] <- list()

  } else {

    stacks[[which]] <- stacks[[which]][1:(length(stacks[[which]])-1)]

  }

  return(stacks)

}


saturate_colors <- function(cols, sat = 1.5) {
  # Convert colors to HSV
  hsv_vals <- rgb2hsv(col2rgb(cols))

  # Increase saturation
  hsv_vals[2, ] <- pmin(hsv_vals[2, ] * sat, 1)  # Cap saturation at 1

  # Convert back to hexadecimal
  out <- hsv(hsv_vals[1, ], hsv_vals[2, ], hsv_vals[3, ])

  names(out) <- names(cols)

  return(out)

}

score_label_colors <- function(score_set_up){

  purrr::set_names(nm = names(score_set_up$choices), x = score_set_up$colors)

}

trim_brain_3d <- function(plot_input, var, val_missing, fct = 0.5){

  # pre-deselect infratentorial
  contains_cerebellum <-
    dplyr::filter(plot_input, ann_dk_adj == "Cerebellum") %>%
    dplyr::summarise(test = any(!!rlang::sym(var) != {{val_missing}}))

  if(!contains_cerebellum$test){

    plot_input <- dplyr::filter(plot_input, ann_dk_adj != "Cerebellum")

  }

  contains_brainstem <-
    dplyr::filter(plot_input, ann_dk_adj == "Brain-Stem") %>%
    dplyr::summarise(test = any(!!rlang::sym(var) != {{val_missing}}))

  if(!contains_brainstem$test){

    plot_input <- dplyr::filter(plot_input, ann_dk_adj != "Brain-Stem")

  }

  # select hemispheres
  hemispheres <-
    dplyr::filter(plot_input, !!rlang::sym(var) != {{val_missing}}) %>%
    dplyr::pull(var = "hemisphere") %>%
    unique()

  plot_input <- dplyr::filter(plot_input, hemisphere %in% {{hemispheres}})

  # trim remaining
  plot_input_main <- dplyr::filter(plot_input, !!rlang::sym(var) != {{val_missing}} | !is_wm)

  plot_input_side <- dplyr::filter(plot_input, !id %in% plot_input_main$id)

  keep <- round(nrow(plot_input_side)*fct)
  plot_input_side <- dplyr::slice_sample(plot_input_side, n = {{keep}})

  # merge back to output
  rbind(plot_input_main, plot_input_side)

}

to_rgb_format <- function(color) {

  rgb_values <- grDevices::col2rgb(color)

  r <- rgb_values[1]
  g <- rgb_values[2]
  b <- rgb_values[3]

  formatted_rgb <- sprintf("rgb(%d, %d, %d)", r, g, b)

  return(formatted_rgb)
}


update_CBscore <- function(cb_df, update_df){

  out <-
    dplyr::left_join(x = cb_df, y = update_df[,c("id", "force", "CBscore_new")], by = "id") %>%
    dplyr::mutate(
      CBscore =
        dplyr::case_when(
          is.na(CBscore_new) ~ CBscore,
          CBscore_new > CBscore ~ CBscore_new,
          CBscore_new < CBscore & force ~ CBscore_new,
          TRUE ~ CBscore
        )
    ) %>%
    dplyr::select(-CBscore_new, -force)

  return(out)

}

CBscore_to_label <- function(cb_df){

  for(i in seq_along(score_set_up$choices)){

    num_val <- unname(score_set_up$choices)[i]
    label <- names(score_set_up$choices)[i]

    cb_df$CBscore_label[cb_df$CBscore == num_val] <- label

  }

  return(cb_df)

}

within_range <- function(x, r){

  x > min(r) & x < max(r)

}

