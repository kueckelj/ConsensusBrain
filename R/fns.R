

# objects -----------------------------------------------------------------

# do not change the order!

#' @export
ccs_labels <- c("x", "z", "y")

#' @export
mri_planes <- c("sag", "cor", "axi")

#' @export
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

  if(is.null(help_info$title)){ help_info$title <- make_pretty_label(name) }

  shinyhelper::helper(
    shiny_tag = shiny_tag,
    title = help_info$title,
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

animate2D <- function(voxel_df,
                      color_by,
                      planes = c("axi", "cor", "sag"),
                      score_alpha = 0.4,
                      layer_colors = list(),
                      fill_lab = str_to_title(color_by),
                      fps = 5){

  for(plane in planes){

    message(glue::glue("Working on plane {plane}."))
    message("Preparing data.")

    slices <- sort(unique(voxel_df[[switch_axis_label(plane)]]))

    pb <- confuns::create_progress_bar(length(slices))

    out_df <-
      purrr::map_dfr(
        .x = slices,
        .f = function(slice){

          pb$tick()

          dplyr::left_join(
            x = dplyr::rename(get_slice_df(mni_template, plane = plane, slice = slice), t1 = value),
            y = get_slice_df(input = voxel_df, plane = plane, slice = slice, var = color_by),
            by = c("col", "row")
          ) %>%
            dplyr::mutate(plane = {{plane}}, slice = {{slice}})

        }
      ) %>%
      dplyr::mutate(
        t1_alpha = dplyr::if_else(is.na(value), true = 1, false = 1-{{score_alpha}})
      )

    out_df$row <- max(out_df$row) - out_df$row

    vdf <- dplyr::filter(out_df, !is.na(value))
    crange <- range(vdf$col)
    rrange <- range(vdf$row)

    cdist <- as.numeric(dist(crange))
    rdist <- as.numeric(dist(rrange))

    if(cdist > rdist){

      buffer <- (cdist-rdist)/2

      xlim <- crange
      ylim <- c(min(rrange)-buffer, max(rrange)+buffer)

    } else if(rdist > cdist){

      buffer <- (rdist-cdist)/2

      xlim <- c(min(crange)-buffer, max(crange)+buffer)
      ylim <- rrange

    } else {

      xlim <- crange
      ylim <- rrange

    }

    out_df <-
      dplyr::filter(
        .data = out_df,
        !is.na(value) &
        dplyr::between(x = col, left = xlim[1], right = xlim[2]) &
        dplyr::between(x = row, left = ylim[1], right = ylim[2])
      )

    p <-
      ggplot2::ggplot(out_df, mapping = ggplot2::aes(x = col, y = row)) +
      ggplot2::theme_void() +
      ggplot2::geom_raster(mapping = ggplot2::aes(fill = t1), alpha = out_df$t1_alpha) +
      confuns::scale_color_add_on(
        aes = "fill",
        variable = out_df$t1,
        clrsp = c("black", "white"),
        guide = "none"
      ) +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_raster(mapping = ggplot2::aes(fill = value), alpha = score_alpha) +
      layer_colors +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim = xlim,
        ylim = ylim,
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "black"),
        panel.grid = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank()
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = fill_lab) +
      gganimate::transition_manual(slice)

    message("Preparing animation. This can take a few moments.")

    anim <- gganimate::animate(p, fps = fps)

    message("Saving animation.")

    gganimate::anim_save(
      filename = paste0("animations/consensus_", plane, "_", color_by, ".gif"),
      animation = anim
    )

    message("Done.")

  }

}


animate3D_prepare <- function(fig,
                              n_frames = 360,
                              angle_range = c(0, 360),
                              paper_bgcolor = "black",
                              vwidth = 600,
                              vheight = 600){

  require(plotly)
  require(htmlwidgets)
  require(webshot2)
  require(av)

  # 1. Set up temporary directory for frames
  frame_dir <- tempdir()
  # Double-check that it's the correct path and not something critical
  if(dir.exists(frame_dir)){
    unlink(frame_dir, recursive = TRUE, force = TRUE)
    dir.create(frame_dir)  # optional: recreate empty dir if needed
  }

  angles <- seq(angle_range[1], angle_range[2], length.out = n_frames)

  pb <- confuns::create_progress_bar(total = length(angles))

  # 2. Create and save each frame
  for(i in seq_along(angles)){

    pb$tick()

    rad <- angles[i] * pi / 180
    eye <- list(x = 1.5 * cos(rad), y = 1.5 * sin(rad), z = 0.6)
    camera <- list(eye = eye)

    fig <-
      fig %>%
      layout(scene = list(camera = camera),
             margin = list(l = 0, r = 0, b = 0, t = 0),
             paper_bgcolor = paper_bgcolor)

    # Save widget to temp HTML
    html_path <- file.path(frame_dir, sprintf("frame_%03d.html", i))
    htmlwidgets::saveWidget(fig, file = html_path, selfcontained = TRUE)

    # Convert to PNG using webshot2
    png_path <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    webshot2::webshot(html_path, file = png_path, vwidth = vwidth, vheight = vheight, quiet = TRUE)

  }

  return(frame_dir)

}

animate3D_render <- function(frame_dir,
                             frame_rate,
                             filename,
                             folder = "animations"){

  output <- file.path(folder, paste0(filename, ".mp4"))

  # 3. Encode PNGs to video
  png_files <- list.files(frame_dir, pattern = "frame_\\d+\\.png$", full.names = TRUE)
  av::av_encode_video(png_files, framerate = frame_rate, output = output)

}

animate3D_clear <- function(frame_dir, force = FALSE){

  if(dir.exists(frame_dir)){

    files <- list.files(frame_dir)
    print(files)

    if(isTRUE(force)){

      clear <- TRUE

    } else {

      clear <- askYesNo("Do you want to clear this directory")

    }

    if(isTRUE(clear)){

      unlink(frame_dir, recursive = TRUE, force = TRUE)
      message("Cleared.")

    }

  } else {

    message("Already cleared.")

  }

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


circular_progress_plot <- function(voxel_df, ...) {

  score_set_up <- ConsensusBrain::score_set_up

  voxel_df <- make_CBscore_label(voxel_df, score_set_up)

  df_summary <-
    voxel_df %>%
    dplyr::count(CBscore_label) %>%
    dplyr::mutate(
      fraction = n / sum(n),
      cumulative = cumsum(fraction),
      ymin = dplyr::lag(cumulative, default = 0),
      ymax = cumulative
    )

  # Add missing factor levels explicitly (ensures unused labels still show in legend)
  missing_levels <- setdiff(names(score_set_up$choices), df_summary$CBscore_label)

  if (length(missing_levels) > 0) {

    df_summary <-
      dplyr::bind_rows(
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
      values = purrr::set_names(nm = names(score_set_up$choices), x = score_set_up$colors),
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
ConsensusBrain <- function(nifti_object = NULL, project = ""){

  # draw from package data
  if(is.null(nifti_object)){ nifti_object <- ConsensusBrain::mni_template }

  if(local_launch()){ shiny::addResourcePath("www", "inst/app/www") }

  shiny::shinyApp(
    ui = ConsensusBrainUI(project = project),
    server = function(input, output, session){

      ConsensusBrainServer(
        input = input,
        output = output,
        session = session,
        nifti_object = nifti_object,
        project = project
      )

    }
  )

}

#' @export
launchCB <- function(project = ""){

  shiny::runApp(
    appDir = ConsensusBrain(project = project),
    launch.browser = TRUE
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

find_intro_html <- function(dir, project){

  if(project != ""){

    dir <- stringr::str_replace(dir, "Introduction", paste0("Introduction_", project))

  }

  return(dir)

}

from_nifti_with_instructions <- function(nifti, instr, dbscan = FALSE){

  if(instr$slice < 1){

    # assumes RAS+ orientation!
    mx <-
      dplyr::case_when(
        instr$plane == "sag" ~ dim(nifti)[1],
        instr$plane == "cor" ~ dim(nifti)[2],
        instr$plane == "axi" ~ dim(nifti)[3]
      )

    instr$slice <- round(instr$slice * mx)

  }

  slice_df <-
    imgR::get_slice_df(
      input = nifti,
      orientation = instr$plane,
      slice = instr$slice
    ) %>%
    dplyr::rename(col = Var1, row = Var2) %>%
    dplyr::mutate(
      plane = instr$plane,
      slice = instr$slice
    ) %>%
    dplyr::filter(value != 0)

  if(isTRUE(dbscan)){

    if(nrow(slice_df) != 0){

      dbscan_out <- dbscan::dbscan(x = slice_df[, c("col", "row")], eps = 1.5, minPts = 8)
      slice_df$dbscan <- as.character(dbscan_out$cluster)
      slice_df <- dplyr::filter(slice_df, dbscan != 0)

    } else {

      slice_df$dbscan <- ""

    }

  }

  return(slice_df)

}

generate_color_gradient <- function(colors, n) {
  stopifnot(length(colors) >= 2)
  grDevices::colorRampPalette(colors)(n)
}


get_brain_dim <- function(nifti, plane, slice){

  slice_df <-
    get_slice_df(nifti, plane = plane, slice = slice) %>%
    dplyr::filter(value != 0)

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
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
      ),
    ggplot2::labs(x = NULL, y = NULL, fill = lab_fill)
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

#' @export
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



#' @title Identify Observations Within a 2D Radius
#'
#' @description
#' Returns the IDs of all observations in a 2D interaction template that lie within
#' a specified radius of a cursor position. Optionally restricts the selection to a
#' specific label and hemisphere based on the observation at the center of the cursor.
#'
#' @param cursor_pos A numeric vector of length 2 indicating the cursor position (`c(col, row)`).
#' @param radius A numeric value specifying the radius within which to select observations.
#' @param interaction_template A data frame containing at least the columns `id`, `col`, `row`,
#' `hemisphere`, and optionally a label column (specified by `selection_scope`).
#' @param preselected_ids A character vector of IDs that should be excluded from selection. Defaults to `character()`.
#' @param selection_scope Optional character string naming the column in `interaction_template` used
#' to constrain the selection to observations with the same label and hemisphere as the observation under the cursor.
#'
#' @return A character vector of `id`s from `interaction_template` that fall within the specified radius
#' and match the optional label and hemisphere constraints.
#' @export

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

list_sequences <- function(dir_session, space = "reg", ...){

  if(space == "dicom"){

    list.files(file.path(dir_session, "dicom"), ...)

  } else {

    list.files(file.path(dir_session, "nifti"), ...)

  }

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

local_launch <- function(session = NULL){

  if(!is.null(session)){

    hostname <- session$clientData$url_hostname

    grepl("localhost", hostname) || grepl("^127\\.0\\.0\\.1$", hostname)

  } else {

    dir.exists("/Users/heilandr/lab/projects/ConsensusBrain")

  }

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

mask_bb <- function(dir_session, mask, space = "reg"){

  require(ConsensusBrain)

  mask_df <-
    oro.nifti::readNIfTI(file.path(dir_session, "nifti", paste0("mask_", mask, "_", space, ".nii.gz"))) %>%
    ConsensusBrain::nifti_to_voxel_df(black_rm = T, verbose = FALSE)

  purrr::map(
    .x = switch_axis_label(ConsensusBrain::mri_planes),
    .f = ~ range(mask_df[[.x]])
  ) %>%
    purrr::set_names(nm = ConsensusBrain::mri_planes)

}



plot_mri_frame <- function(col = c(1,256),
                           row = c(1,256),
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
    x = NA,
    y = NA,
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


#' @title Visualize MRI slices from structured NIfTI inputs
#'
#' @description
#' Visualizes single or multiple slices from MRI sequences based on a structured directory and file naming convention.
#' Files are expected to follow the format `<type>_<sequence>_<space>.nii.gz`, where:
#'
#' - `type`: denotes the kind of image (e.g., `"brainm"` for skull-stripped, `"raw"` for original, `"mask"` for binary masks).
#' - `sequence`: the MRI modality (e.g., `"t1"`, `"t1ce"`, `"t2"`, `"flair"`).
#' - `space`: the spatial context (e.g., `"orig"` for unregistered space, `"regRef"` for session-specific reference-registered space, `"regMNI"` for mni-registered space).
#'
#' Each image is read, sliced, and displayed based on instructions passed through the `planes` and `slices` arguments.
#'
#' @param input A path to a directory containing NIfTI files named according to the `<type>_<sequence>_<space>.nii.gz` convention,
#'              a path to a single NIfTI file, or a NIfTI object directly.
#' @param type A string specifying the file prefix (e.g., `"brainm"` for skull-stripped images). Default is `"brainm"`.
#' @param space A string specifying the spatial suffix (e.g., `"regRef"`, `"regMNI"`, `"orig"`). Default is `"fsMNI"`.
#' @param sequences Character vector of MRI sequences to load (e.g., `"t1ce"`, `"flair"`). Can also include `"mni"` to overlay a reference.
#' @param planes Character vector specifying which anatomical planes to plot: any combination of `"sag"`, `"axi"`, `"cor"`.
#' @param slices A numeric vector of slice indices (or fractions < 1 to indicate percent of axis) to display. If `NULL` and a mask is given,
#'        slices are chosen to best capture the mask extent.
#' @param mask Optional string indicating which mask to load (e.g., `"tumor"`). The function expects a file named `mask_<mask>_<space>.nii.gz`.
#' @param mask_alpha Alpha transparency of the mask overlay.
#' @param mask_color Fill and border color of the mask.
#' @param mask_linesize Line width of the mask outline.
#' @param grid Optional grid to overlay (either vector of values or fraction of image size).
#' @param grid_x,grid_y Vectors of x/y-positions for gridlines (overrides `grid` if set separately).
#' @param grid_alpha Alpha transparency of the grid lines.
#' @param grid_color Color of grid lines.
#' @param grid_linesize Line width of grid lines.
#' @param slice_num Logical; if `TRUE`, displays slice numbers as text overlay.
#' @param slice_num_fct Numeric vector (length 1 or 2) for horizontal and vertical placement of slice numbers as fraction of image size.
#' @param slice_num_size Font size of the slice number labels.
#' @param guide Type of fill legend guide to use (default `"none"`).
#' @param cols,rows Column and row variables for faceting.
#' @param ncol,nrow Numeric; number of columns and rows for facet layout (if applicable).
#'
#' @details
#' If `input` is a directory, the function searches for files matching the naming scheme `<type>_<sequence>_<space>.nii.gz` inside the `nifti/` subdirectory.
#' If a mask is provided, and `slices` is `NULL`, the function identifies the most informative slices based on mask voxel density per plane.
#'
#' Slices are extracted using custom axis conversion and projected as 2D images with optional overlays for masks and grid lines.
#'
#' The function supports multi-sequence and multi-plane faceting. To avoid overplotting, it restricts combinations of more than two varying dimensions (`plane`, `slice`, `sequence`).
#'
#' @return A `ggplot` object with the requested slice visualization.
#'
#' @examples
#' \dontrun{
#' # Plot one axial slice from T1-CE image, with tumor mask overlay
#' plot_subject(input = "sub-001/",
#'              type = "brainm",
#'              space = "regRef",
#'              sequences = "t1ce",
#'              mask = "tumor",
#'              planes = "axi",
#'              slices = 128)
#'
#' # Plot 3 anatomical planes through the most informative tumor slices
#' plot_subject(input = "sub-002/",
#'              type = "brainm",
#'              space = "regRef",
#'              sequences = "t1ce",
#'              mask = "tumor")
#' }
#'
#' @import ggplot2 dplyr tidyr purrr oro.nifti
#' @export

plot_subject <- function(input,
                         type = "raw",
                         space = "regMNI",
                         sequences = "t1ce",
                         planes = c("sag", "axi", "cor"),
                         slices = NULL,
                         mask = NULL,
                         mask_alpha = 0.25,
                         mask_color = "red",
                         mask_linesize = 1,
                         grid = NULL,
                         grid_x = NULL,
                         grid_y = NULL,
                         grid_alpha = 0.25,
                         grid_color = "white",
                         grid_linesize = 0.25,
                         slice_num = TRUE,
                         slice_num_fct = 0.975,
                         slice_num_size = 2.5,
                         guide = "none",
                         offset_slices = 0,
                         offset_dir = "left",
                         mni_path = NULL,
                         mni_dir = "/Users/heilandr/lab/data/mri/MNI_templates/MNI152",
                         cols = NULL,
                         rows = NULL,
                         ncol = NULL,
                         nrow = NULL){

  require(oro.nifti)

  stopifnot(length(type) == 1)
  stopifnot(length(space) == 1)

  slices_rm <- FALSE

  # start with mask, if required
  if(length(mask) == 1){

    if(!is.character(input) && !file.info(input)[["isdir"]]){

      warning("Parameter `mask` can not be used with a non-directory value for `input`.")

    } else {

      mask_path <- file.path(input, "nifti", paste0("mask_", mask, "_", space, ".nii.gz"))

      if(!file.exists(mask_path)){

        mp <- file.path("/nifti", basename(mask_path))
        stop(glue::glue("Mask path '{mp}' not found in '{input}'."))

      }

      nifti_mask <- oro.nifti::readNIfTI(mask_path, reorient = FALSE)

      # if no slice specified -> pick slices that contain most mask pixels
      if(is.null(slices)){

        nifti_mask_df <-
          ConsensusBrain::nifti_to_voxel_df(nifti_mask, verbose = FALSE) %>%
          dplyr::filter(value != 0)

        seq_instr <-
          purrr::map_df(
            .x = ConsensusBrain::switch_axis_label(planes),
            .f = function(ccs_axis){

              dplyr::group_by(nifti_mask_df, !!rlang::sym(ccs_axis)) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                dplyr::slice_max(order_by = n, with_ties = FALSE) %>%
                dplyr::transmute(
                  plane = ConsensusBrain::switch_axis_label(ccs_axis),
                  slice = !!rlang::sym(ccs_axis)
                )

            }
          )

        slices_rm <- TRUE

      } else {

        # make seq instructions
        seq_instr <- tidyr::expand_grid(plane = planes, slice = slices)

      }

      # iterate over seq_instr
      mask_layer <-
        purrr::map_df(
          .x = 1:nrow(seq_instr),
          .f = function(i){

            mask_df <-
              from_nifti_with_instructions(nifti_mask, seq_instr[i, ], dbscan = TRUE) %>%
              dplyr::select(col, row, value, dbscan) %>%
              dplyr::filter(value != 0)

            if(nrow(mask_df) != 0){

              out <-
                purrr::map_df(
                  .x = unique(mask_df$dbscan),
                  .f = function(group){

                    dplyr::filter(mask_df, dbscan == {{group}}) %>%
                      dplyr::select(col, row) %>%
                      as.matrix() %>%
                      concaveman::concaveman(points = ., concavity = 2) %>%
                      tibble::as_tibble() %>%
                      magrittr::set_colnames(value = c("col", "row")) %>%
                      dplyr::mutate(
                        plane = as.character(seq_instr[i,"plane"]),
                        slice = seq_instr[i,][["slice"]],
                        dbscan = {{group}}
                      )

                  }
                )

            } else {

              out <- NULL

            }

            return(out)

          }
        ) %>%
        ggplot2::geom_polygon(
          data = .,
          mapping = ggplot2::aes(group = dbscan),
          alpha = mask_alpha,
          color = mask_color,
          fill = mask_color
        )

    }

  } else {

    mask_layer <- NULL
    seq_instr <- NULL

  }

  # make seq_instr if not already created during mask reading
  if(is.null(seq_instr)){

    slices <- if(is.null(slices)){ 0.5 } else { slices }

    # make seq instructions
    seq_instr <- tidyr::expand_grid(plane = planes, slice = slices, sequence = sequences)

  }

  if(length(slices) == 1 && all(slices < 1)){ slices_rm <- TRUE }

  # make image data list from input
  if(oro.nifti::is.nifti(input)){

    nifti_lst <- purrr::set_names(list(input), nm = "Input (Nifti)")
    sequences <- "Input (Nifti)"

    # make list from folder or file
  } else if(is.character(input)){

    # input does not exist
    if(is.na(file.info(input)[["isdir"]])){

      stop("Input path '{input}' does not exist.")

      # input is folder: make sequence paths from folder
    } else if(file.info(input)[["isdir"]]){

      seq_paths <- vector(mode = "character", length = length(sequences))

      file_names <- paste0(type, "_", sequences[sequences != "mni"], "_", space, ".nii.gz")
      seq_paths[sequences != "mni"] <- file.path(input, "nifti", file_names)

      if("mni" %in% sequences){

        if(is.character(mni_path)){

          # path to 182x218x182 brain
          seq_paths[sequences == "mni"] <- mni_path

        } else {

          file_mni <- paste0(type, "_mni_orig.nii.gz")
          mni_path <- file.path(mni_dir, file_mni)
          seq_paths[sequences == "mni"] <- mni_path

        }

      }

      # check file availability
      for(sp in seq_paths){

        if(!file.exists(sp)){

          stop(glue::glue("Did not find '{sp}'."))

        }

      }

      # input is file: make sequence paths from input path
    } else if(!file.info(input)[["isdir"]]) {

      seq_paths <- input
      sequences <- "Input (Dir)"

    }

    nifti_lst <-
      purrr::map(seq_paths, .f = ~ oro.nifti::readNIfTI(.x, reorient = FALSE)) %>%
      purrr::set_names(nm = sequences)

  } else {

    stop("Invalid input.")

  }

  # make seq_df for plotting
  plot_df_seq <-
    purrr::imap_dfr(
      .x = nifti_lst,
      .f = function(nifti, seq){

        # iterate over seq_instr
        purrr::map_df(
          .x = 1:nrow(seq_instr),
          .f = ~ from_nifti_with_instructions(nifti, seq_instr[.x, ])
        ) %>%
          dplyr::filter(value != 0) %>%
          dplyr::mutate(sequence = {{seq}})

      }
    )

  # facets required?
  multiple <-
    purrr::keep(
      .x = c("plane", "slice", "sequence"),
      .p = ~ dplyr::n_distinct(plot_df_seq[[.x]]) > 1
    )

  if(isTRUE(slices_rm)){ multiple <- multiple[multiple != "slice"]}

  if(length(multiple) == 3){

    stop("Too many dimensions to plot. Either `planes`, `sequences` or `slices` must be of length 1.")

  } else if(length(multiple) == 2){

    cols <- ifelse(is.null(cols), multiple[1], cols)
    rows <- ifelse(is.null(rows), multiple[2], rows)

    facet_formula <- as.formula(paste(cols, "~", rows, collapse = " "))
    facet_layer <- ggplot2::facet_grid(facet_formula)

  } else if(length(multiple) == 1){

    facet_formula <- as.formula(paste0(". ~ ", multiple))
    facet_layer <- ggplot2::facet_wrap(facet_formula, ncol = ncol, nrow = nrow)

  } else {

    facet_layer <- NULL

  }

  # T1
  intensity_layer <-
    purrr::map(
      .x = sequences,
      .f = function(seq){

        list(
          ggplot2::geom_raster(
            data = plot_df_seq[plot_df_seq$sequence == seq,],
            mapping = ggplot2::aes(x = col, y = row, fill = value)
          ),
          ggplot2::scale_fill_gradient(low = "black", high = "white", guide = guide),
          ggnewscale::new_scale_fill()
        )

      }
    )

  # Grid
  if(is.numeric(grid)){

    smaller1 <- which(grid < 1)
    grid[smaller1] <- purrr::map_int(grid[smaller1], .f = ~ .x * max(plot_df_seq[,c("col", "row")]))

    grid_x <- grid_y <- grid

  }

  grid_layer <-
    list(
      if(is.numeric(grid_x)){ ggplot2::geom_vline(xintercept = grid_x, alpha = grid_alpha, color = grid_color, linewidth = grid_linesize) },
      if(is.numeric(grid_y)){ ggplot2::geom_hline(yintercept = grid_y, alpha = grid_alpha, color = grid_color, linewidth = grid_linesize) }
    )

  # slice number
  if(isTRUE(slice_num) && !"slice" %in% multiple){

    if(length(slice_num_fct) == 1){ slice_num_fct <- rep(slice_num_fct, 2) }

    # assumes that all images are in the same space
    nifti <- nifti_lst[[1]]

    seq_instr <-
      purrr::map_df(
        .x = 1:nrow(seq_instr),
        .f = function(i){

          instr <- seq_instr[i,]

          if(instr$slice < 1){

            # assumes RAS+ orientation!
            mx <-
              dplyr::case_when(
                instr$plane == "sag" ~ dim(nifti)[1],
                instr$plane == "cor" ~ dim(nifti)[2],
                instr$plane == "axi" ~ dim(nifti)[3]
              )

            instr$slice <- round(instr$slice*mx)

          }

          return(instr)

        }
      )

    slice_num_df <-
      dplyr::mutate(
        .data = seq_instr,
        col = max(plot_df_seq$col)*slice_num_fct[1],
        row = max(plot_df_seq$row)*slice_num_fct[2],
        label = paste0("N=",slice)
      )

    slice_num_layer <-
      ggplot2::geom_text(
        data = slice_num_df,
        mapping = ggplot2::aes(label = paste0("Sl.: ", slice)),
        color = "white",
        size = slice_num_size,
        hjust = 1
      )

  } else {

    slice_num_layer <- NULL

  }

  ggplot2::ggplot(mapping = ggplot2::aes(x = col, y = row)) +
    intensity_layer +
    mask_layer +
    slice_num_layer +
    grid_layer +
    facet_layer +
    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(color = "white"),
      strip.background = ggplot2::element_rect(fill = "black", color = "black"),
      strip.text = ggplot2::element_text(color = "white"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "black")
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = NULL, y = NULL)

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

pre_render_slices <- function(nifti, npixels = 1500, plane = "axi") {

  # required axes
  ra <- req_axes_2d(plane, mri = TRUE)

  rcol <- brain_dims[[ra["col"]]]
  rrow <- brain_dims[[ra["row"]]]

  dc <- as.numeric(dist(rcol))
  dr <- as.numeric(dist(rrow))

  # range plane
  rp <- brain_dims[[plane]]
  col_seq <- rcol[1]:rcol[2]
  row_seq <- rrow[1]:rrow[2]
  slice_seq <- rp[1]:rp[2]

  lst <- vector(mode = "list", length = 256)

  lst[slice_seq] <-
    lapply(slice_seq, function(i) {
      #pb$tick()

      slice <- get_slice(nifti, plane = plane, slice = i)

      slice <- slice[row_seq, col_seq]

      slice <- slice - min(slice, na.rm = TRUE)
      slice <- slice / max(slice, na.rm = TRUE)

      tf <- tempfile(fileext = ".png")
      png(tf, width = npixels, height = npixels, bg = "white")
      par(mar = c(0, 0, 0, 0))
      plot_mri_frame(col = col_seq, row = row_seq)
      rasterImage(
        image = slice,
        xleft = min(col_seq),
        ybottom = max(row_seq),
        xright = max(col_seq),
        ytop = min(row_seq),
        interpolate = FALSE
      )
      dev.off()

      on.exit(unlink(tf))  # ensures cleanup even if something fails
      base64enc::dataURI(file = tf, mime = "image/png")

    })

  lst

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

resource_file <- function(file){

  ifelse(local_launch(), file.path("www", file), file)

}



#' @export
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
                          hover_axes = FALSE,
                          side_highlight = NULL,
                          clrp_adjust = NULL, # predefined, named vectors
                          clrp_adjust2 = NULL, # adjustemnts to clrp_adjust
                          opacity_hide = 0.15,
                          pt_clrsp = "Viridis",
                          pt_size = 1.5,
                          eye = list(x = 0, y = 0, z = 2),
                          show_axes = TRUE,
                          show_legend = TRUE,
                          colorful = FALSE,
                          mode = "markers",
                          plot_bgcolor = "white",
                          paper_bgcolor = "white",
                          source = "plotly_source",
                          scene = NULL,
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

  if(isTRUE(hover_axes)){

    for(i in 1:3){

      ccs_label <- ccs_labels[i]
      mri_label <- mri_planes[i]

      voxel_df$hoverinfo <-
        paste0(
          voxel_df$hoverinfo,
          "<br>", stringr::str_to_title(mri_label), ": ", voxel_df[[ccs_label]]
          )

    }

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
  if (is.numeric(voxel_df[[color_by]])) {

    max_points <- 100000
    color_range <- range(voxel_df[[color_by]], na.rm = TRUE)

    voxel_list <- split(voxel_df, ceiling(seq_len(nrow(voxel_df)) / max_points))
    p <- plotly::plot_ly(source = source)

    # Determine the colorscale
    if (length(pt_clrsp) == 1 && is.character(pt_clrsp)) {
      # Single palette name like "Viridis" or "Inferno"
      colorscale <- pt_clrsp
    } else if (length(pt_clrsp) >= 2 && is.character(pt_clrsp)) {
      # User-provided full colorscale
      n_colors <- length(pt_clrsp)
      colorscale <- lapply(seq_len(n_colors), function(i) {
        list((i - 1) / (n_colors - 1), pt_clrsp[i])
      })
    } else {
      stop("pt_clrsp must be either a single named palette or a vector of colors.")
    }

    for (i in seq_along(voxel_list)) {
      sub_df <- voxel_list[[i]]

      p <- plotly::add_trace(
        p = p,
        data = sub_df,
        x = ~x, y = ~z, z = ~y,
        type = type,
        mode = mode,
        text = ~hoverinfo,
        hoverinfo = "text",
        marker = list(
          size = pt_size,
          color = sub_df[[color_by]],
          colorscale = colorscale,
          cmin = color_range[1],
          cmax = color_range[2],
          colorbar = if (i == 1) list(title = color_by) else NULL
        ),
        showlegend = FALSE,
        name = NULL
      )
    }
  } else {

    # Grouped (categorical) values
    p <- plotly::plot_ly(source = source)

    for (i in seq_along(unique_labels)) {

      label <- unique_labels[i]
      voxel_df_subset <- dplyr::filter(voxel_df, !!rlang::sym(color_by) == {{label}})

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

      p <-
        plotly::add_trace(
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

  if(is.null(scene)){

    if(show_axes){

      scene <-
        list(
          xaxis = list(title = "Sagittal (x)", range = c(min(voxel_df$x), max(voxel_df$x))),
          yaxis = list(title = "Coronal (z)", range = c(min(voxel_df$z), max(voxel_df$z))), # switch y & z
          zaxis = list(title = "Axial (y)", range = c(min(voxel_df$y), max(voxel_df$y))),
          camera = list(
            eye = eye,
            ...
          )
        )

    } else {

      scene <-
        list(
          xaxis = list(title = '', showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          yaxis = list(title = '', showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          zaxis = list(title = '', showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          camera = list(
            eye = eye,
            ...
          )
        )

    }

  }

  ## Add camera with user-defined `eye`
  p <- plotly::layout(
    p,
    scene = scene,
    paper_bgcolor = paper_bgcolor,
    plot_bgcolor = plot_bgcolor
  )

  return(p)

}

#' Plot Brain in 3D
#'
#' This function creates a 3D scatter plot of brain vertices, allowing users to color points by numeric or categorical variables.
#'
#' @param vdf A data frame containing vertex coordinates and metadata (e.g., x, y, z, and color_by columns).
#' @param color_by A string defining the column to use for coloring vertices.
#' @param parcellation A string specifying the parcellation column (optional).
#' @param parc_highlight A vector of parcellation labels to highlight (optional).
#' @param side_highlight A string specifying the hemisphere to highlight ("left" or "right") (optional).
#' @param parc_rm A vector of parcellation labels to remove from the plot (optional).
#' @param side_rm A string specifying the hemisphere to remove ("left" or "right") (optional).
#' @param vert_rm A vector of vertex IDs to remove from the plot (optional).
#' @param pt_clrsp A string specifying the colorscale for numeric variables (default: "Viridis").
#' @param pt_size A numeric value specifying the size of the points in the plot (default: 1.5).
#'
#' @return A plotly object containing the 3D scatter plot.
#' @examples
#' vdf <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   z = rnorm(100),
#'   color_by = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' plot_brain_surface_3d(vdf, color_by = "color_by")
#' @export
plot_brain_surface_3d <- function(vdf,
                                  color_by,
                                  parcellation = NULL,
                                  parc_highlight = NULL,
                                  side_highlight = NULL,
                                  parc_rm = NULL,
                                  side_rm = NULL,
                                  vert_rm = NULL,
                                  pt_clrsp = "Viridis",
                                  pt_size = 1.5){

  require(plotly)

  ## Prepare and subset
  if(is.character(side_rm)){
    vdf <- dplyr::filter(vdf, hem != {{side_rm}})

  }

  if(is.character(vert_rm)){
    vdf <- dplyr::filter(vdf, !vert_id %in% {{vert_rm}})

  }

  if(is.character(color_by)){
    if(is.character(parc_highlight)){
      stopifnot(is.character(parcellation))
      vdf[[parcellation]][!vdf[[parcellation]] %in% parc_highlight] <- NA
      vdf[[color_by]][!vdf[[parcellation]] %in% parc_highlight] <- NA

    }

    if(is.character(side_highlight)){
      vdf[[parcellation]][!vdf$hem %in% side_highlight] <- NA
      vdf[[color_by]][!vdf$hem %in% side_highlight] <- NA

    }

  }

  ## Create dynamic formula for `color`
  color_formula <- as.formula(paste("~", color_by))

  ## Generate color palette for categorical variables
  if(!is.numeric(vdf[[color_by]])){
    unique_labels <- sort(unique(vdf[[color_by]]))
    n_labels <- length(unique_labels)
    colors <- scales::hue_pal()(n_labels)  # Generate default ggplot2-like colors
    color_map <- setNames(colors, unique_labels)  # Map colors to categories
    vdf$color <- color_map[vdf[[color_by]]]  # Assign discrete colors
    vdf[[color_by]] <- factor(vdf[[color_by]], levels = unique_labels)  # Factor for legend ordering

  }

  ## Plot
  if(is.numeric(vdf[[color_by]])){
    # Numeric values
    p <- plot_ly(
      data = vdf,
      x = ~x, y = ~y, z = ~z, color = color_formula,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = pt_size, colorscale = pt_clrsp, colorbar = list(title = color_by))
    )

  } else {
    # Grouped (categorical) values
    p <- plot_ly()

    for(i in seq_along(unique_labels)){
      label <- unique_labels[i]
      vdf_subset <- vdf[vdf[[color_by]] == label, ]
      p <- add_trace(
        p = p,
        data = vdf_subset,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = pt_size, color = color_map[[label]]),
        name = label,
        showlegend = TRUE
      )

    }

    # Add proxy traces for the legend (2D traces for larger markers)
    for(i in seq_along(unique_labels)){
      label <- unique_labels[i]
      p <- add_trace(
        p = p,
        type = "scatter",
        mode = "markers",
        x = NA, y = NA,  # Legend-only proxy
        marker = list(size = 20, color = color_map[[label]]),  # Larger marker size
        name = label,
        showlegend = TRUE
      )

    }

    # Adjust legend to be at the bottom with multiple columns
    p <- layout(
      p = p,
      legend = list(
        orientation = "h",       # Horizontal legend
        x = 0.5,                 # Center legend horizontally
        y = -0.2,                # Position legend below the plot
        xanchor = "center",
        yanchor = "top",
        tracegroupgap = 5,       # Gap between groups
        title = list(text = color_by),
        font = list(size = 10),  # Legend font size
        itemwidth = 30,          # Width of legend items
        itemclick = "toggle",    # Allow toggling items
        columns = 3              # Number of legend columns
      )
    )

  }

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


send_mail <- function(subject,
                      text,
                      from = "consensusbrain@gmx.de",
                      to = "consensusbrain@gmx.de",
                      username = "consensusbrain@gmx.de",
                      attachment_path = NULL,
                      password = NULL){

  email <-
    emayili::envelope() %>%
    emayili::from(from) %>%
    emayili::to(to) %>%
    emayili::subject(subject) %>%
    emayili::text(text)

  if(is.character(attachment_path)){

    email <- emayili::attachment(msg = email, path = attachment_path)

  }

  smtp <-
    emayili::server(
      host = "mail.gmx.net",
      port = 587,
      username = username,
      password = password
    )

  smtp(email)

}

showModalFinalized <- function(){

  shiny::showModal(
    shiny::modalDialog(
      title = "Thank you for participating!",
      shiny::helpText(
        "You’ve completed ConsensusBrain. The file you just downloaded contains your final results."
      ),
      shiny::helpText(
        "Like the intermediate progress files, this final file can be reloaded into ConsensusBrain at any time for review or adjustments."
      ),
      shiny::helpText(
        "If you're certain that your results are final, you can submit them directly by clicking 'Send'. Otherwise, you may choose to send the file manually to the study supervisors."
      ),
      shiny::div(
        id = "finalized_panel",
        style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            align = "left",
            style = "border-right: 1px solid lightgrey; height: 100%;",
            shiny::actionButton(
              inputId = "send_finalized_file",
              label = "Send",
              icon = shiny::icon("paper-plane"),
              width = "100%"
            )
          ),
          shiny::column(
            width = 6,
            align = "left",
            shiny::actionButton(
              inputId = "dont_send_finalized_file",
              label = "No thanks",
              icon = shiny::icon("ban"),
              width = "100%"
            )
          )
        )
      ),
      footer = shiny::tagList(),
      easyClose = FALSE
    )
  )

}

showModalNewUser <- function(session, project = ""){

  if(project == ""){

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::tags$h2(shiny::strong("New User"), style = "padding-top: 0;"),
        shiny::column(
          width = 12,
          align = "left",
          shiny::br(),
          shiny::splitLayout(
            cellWidths = "50%",
            shiny::textInput(
              inputId = "userInp_first_name",
              label = shiny::tagList(shiny::icon("person"), "First Name:"),
              value = ifelse(local_launch(session), "Jan", ""),
              width = "100%"
            ),
            shiny::textInput(
              inputId = "userInp_last_name",
              label = "Last Name:",
              value = ifelse(local_launch(session), "Kueckelhaus", ""),
              width = "100%"
            )
          ),
          shinyBS::bsPopover(
            id = "userInp_first_name",
            title = NULL,
            content = "Enter your first ame as it should appear in a publication.",
            placement = "bottom",
            trigger = "hover"
          ),
          shinyBS::bsPopover(
            id = "userInp_last_name",
            title = NULL,
            content = "Enter your last ame as it should appear in a publication.",
            placement = "bottom",
            trigger = "hover"
          ),
          shiny::splitLayout(
            cellWidths = "50%",
            shiny::textInput(
              inputId = "userInp_email",
              value = ifelse(local_launch(session), "jankueckelhaus@gmx.de", ""),
              label = shiny::tagList(shiny::icon("envelope"), "E-Mail:")
            ),
            shiny::textInput(
              inputId = "userInp_email_confirm",
              value = ifelse(local_launch(session), "jankueckelhaus@gmx.de", ""),
              label = shiny::tagList(shiny::icon("envelope"), "E-Mail (Confirm):")
            )
          ),
          shiny::textInput(
            inputId = "userInp_affiliation",
            label = shiny::tagList(shiny::icon("institution"), "Affiliation:"),
            width = "100%",
            value = ifelse(local_launch(session), "Department of Neurosurgery, University Clinic Erlangen", ""),
            placeholder = "As denoted in publications."
          ),
          shinyBS::bsPopover(
            id = "userInp_affiliation",
            title = NULL,
            content = "Enter your affiliation as it should appear in a publication.",
            placement = "bottom",
            trigger = "hover"
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = "userInp_country",
                label = shiny::tagList(shiny::icon("globe"), "Country:"),
                choices = stringr::str_subset(c("", countrycode::codelist$country.name.en), pattern = "German Democratic Republic", negate = TRUE),
                selected = ifelse(local_launch(session), "Germany", ""),
                width = "100%"
              ),
              shinyBS::bsPopover(
                id = "userInp_country",
                title = NULL,
                content = "The country where your affiliation is based (not your country of birth).",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_years_of_experience",
                label = shiny::tagList(shiny::icon("briefcase"), "Years of Experience:"),
                value = ifelse(local_launch(session), 20, NA_integer_),
                min = 0,
                max = 100,
                step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_years_of_experience",
                title = NULL,
                content = "Years of experience as a neurosurgeon, counted from the first day of residency to the present.",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_annual_case_load",
                label = shiny::tagList(shiny::icon("notes-medical"), "Annual Case Load:"),
                value = ifelse(local_launch(session), 250, NA_integer_),
                min = 0,
                max = 1000,
                step = 50
              ),
              shinyBS::bsPopover(
                id = "userInp_annual_case_load",
                title = "Annual Case Load",
                content = "Your personal annual case load for intra-parenchymal glioma surgeries.",
                placement = "bottom",
                trigger = "hover"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::sliderInput(
                inputId = "userInp_perc_surgery_awake",
                label = "Awake Surgery [%]",
                value = 0,
                min = 0, max = 100, step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_perc_surgery_awake",
                title = "Awake Surgery [%]",
                content = "The percentage of glioma surgeries you conduct with the patient beeing awake.",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 6,
              shiny::sliderInput(
                inputId = "userInp_perc_surgery_ionm",
                label = "Surgery with IONM [%]",
                value = 0,
                min = 0, max = 100, step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_perc_surgery_ionm",
                title = "Surgery with IONM [%]",
                content = "The percentage of glioma surgeries you conduct with intraoperative neuromonitoring.",
                placement = "bottom",
                trigger = "hover"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 10,
              align = "left",
              shinyWidgets::awesomeCheckbox(
                inputId = "userInp_terms_of_use",
                label = HTML(
                  'I acknowledge and agree to the <a href="#" id="terms_link">Terms of Use</a>.'
                ),
                value = FALSE
              )
            )
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = "back_to_welcome",
                label = shiny::tagList(shiny::icon("arrow-left"), "Back"),
                width = "100%"
              )
            ),
            shiny::column(width = 2),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = "login",
                label = shiny::tagList(shiny::icon("sign-in-alt"), "Login"),
                width = "100%"
              )
            ),
            shiny::column(width = 4)
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::uiOutput(outputId = "helptext_login")
            ),
          )
        ),
        footer = shiny::tagList()
      )
    )

  } else if(project == "recap"){

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::tags$h2(shiny::strong("New User"), style = "padding-top: 0;"),
        shiny::column(
          width = 12,
          align = "left",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_years_of_experience",
                label = shiny::tagList(shiny::icon("briefcase"), "Years of Experience:"),
                value = ifelse(local_launch(session), 20, NA_integer_),
                min = 0,
                max = 100,
                step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_years_of_experience",
                title = NULL,
                content = "Years of experience as a neurosurgeon, counted from the first day of residency to the present.",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_annual_case_load",
                label = shiny::tagList(shiny::icon("notes-medical"), "Annual Case Load:"),
                value = ifelse(local_launch(session), 250, NA_integer_),
                min = 0,
                max = Inf,
                step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_annual_case_load",
                title = "Annual Case Load",
                content = "Your personal annual case load for intra-parenchymal glioma surgeries.",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_total_case_load",
                label = shiny::tagList(shiny::icon("notes-medical"), "Total Case Load:"),
                value = ifelse(local_launch(session), 250, NA_integer_),
                min = 0,
                max = Inf,
                step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_total_case_load",
                title = "Total Case Load",
                content =  "The total number of gliomas you have operated so far",
                placement = "bottom",
                trigger = "hover"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "userInp_n_institutions",
                label = shiny::tagList(shiny::icon("building-columns"), "Institution Count:"),
                value = ifelse(local_launch(session), 1, NA_integer_),
                min = 0,
                max = 100,
                step = 1
              ),
              shinyBS::bsPopover(
                id = "userInp_n_institutions",
                title = NULL,
                content = "The total number of institutions you have worked at as a neurosurgeon.",
                placement = "bottom",
                trigger = "hover"
              )
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = "userInp_academic_degree",
                label = shiny::tagList(shiny::icon("graduation-cap"), "Academic Degree:"),
                selected = ifelse(local_launch(session), "None", NA_character_),
                choices = c("None", "Dr. med.", "PD Dr. med.", "Prof. Dr. med.")
              )
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = "userInp_scientific_focus",
                label = shiny::tagList(shiny::icon("microscope"), "Scientific Focus:"),
                selected = ifelse(local_launch(session), "Neuroncology", NA_character_),
                selectize = TRUE,
                choices =
                  c("Epilepsy",
                    "Neurooncology",
                    "Pediatric",
                    "Skull Base", "Spine",
                    "Trauma",
                    "Vascular",
                    "Other"
                    ),
                multiple = TRUE
              ),
              shinyBS::bsPopover(
                id = "userInp_scientific_focus",
                title = NULL,
                content = "The focus of your scientific work. Multiple choices can be made!",
                placement = "top",
                trigger = "hover"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 10,
              align = "left",
              shinyWidgets::awesomeCheckbox(
                inputId = "userInp_terms_of_use",
                label = HTML(
                  'I acknowledge and agree to the <a href="#" id="terms_link">Terms of Use</a>.'
                ),
                value = FALSE
              )
            )
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::actionButton(
                inputId = "back_to_welcome",
                label = shiny::tagList(shiny::icon("arrow-left"), "Back"),
                width = "100%"
              )
            ),
            shiny::column(width = 2),
            shiny::column(
              width = 4,
              shiny::actionButton(
                inputId = "login",
                label = shiny::tagList(shiny::icon("sign-in-alt"), "Login"),
                width = "100%"
              )
            ),
            shiny::column(width = 4)
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::uiOutput(outputId = "helptext_login")
            )
          )
        ),
        footer = shiny::tagList()
      )
    )

  }



}

showModalWelcome <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = shiny::tags$div(
        style = "display: flex; align-items: center; justify-content: space-between;",

        shiny::tags$h2(
          shiny::strong("Welcome!"),
          style = "margin: 0; padding-top: 0; width = 70%;"
        ),

        shiny::tags$img(
          src = ifelse(local_launch(), "www/rano_resect_logo.jpeg", "rano_resect_logo.jpeg"),
          style = "width: 30%;"
        )
      ),

      shiny::helpText(
        "If this is your first time here at ConsensusBrain, please click on New User.
          If you've visited before and want to continue where you left off, click 'Browse...' and select the file you previously
          downloaded that contains your saved progress - a file that ends with .rds!"
      ),

      shiny::div(
        id = "login_panel",
        style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            align = "left",
            style = "border-right: 1px solid lightgrey; height: 100%;",
            shiny::actionButton(
              inputId = "new_user",
              label = "New User",
              icon = shiny::icon("person"),
              width = "100%"
            )
          ),
          shiny::column(
            width = 6,
            align = "left",
            shiny::fileInput(
              inputId = "upload_file",
              label = NULL,
              width = "100%",
              accept = ".RDS"
            )
          )
        )
      ),
      footer = shiny::tagList(),
      easyClose = FALSE,
      size = "xl"
    )
  )

}


score_label_colors <- function(score_set_up){

  purrr::set_names(nm = names(score_set_up$choices), x = score_set_up$colors)

}

trim_brain_3d <- function(plot_input, var, val_missing, fct = 0.5, hem = TRUE){

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

  if(hem){

    # select hemispheres
    hemispheres <-
      dplyr::filter(plot_input, !!rlang::sym(var) != {{val_missing}}) %>%
      dplyr::pull(var = "hemisphere") %>%
      unique()

    plot_input <- dplyr::filter(plot_input, hemisphere %in% {{hemispheres}})

  }

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


update_mri_range_zoom <- function(mri_range, zoom_fct) {
  # Extract original ranges
  col <- mri_range$col
  row <- mri_range$row

  # Compute center
  cx <- mean(col)
  cy <- mean(row)

  # Compute new half-width/height after zooming in
  col_half_range <- diff(col) / 2 / zoom_fct
  row_half_range <- diff(row) / 2 / zoom_fct

  # Create new zoomed-in ranges
  col_zoomed <- c(cx - col_half_range, cx + col_half_range)
  row_zoomed <- c(cy - row_half_range, cy + row_half_range)

  # Return updated list
  list(
    col = col_zoomed,
    row = row_zoomed
  )
}

within_range <- function(x, r){

  x > min(r) & x < max(r)

}

valid_name_length <- function(name){

  is.character(name) &&
  length(strsplit(name, split = "")[[1]]) >= 2

}

valid_name_symbols <- function(name){

  is.character(name) &&
  all(
    purrr::map_lgl(
      .x = strsplit(name, split = "")[[1]],
      .f = ~ stringr::str_detect(.x, pattern = stringr::str_c(unique(c(letters, LETTERS, " ")), collapse = "|"))
    )
  )

}

videoBox <- function(name, width = 6){

  help_name <- paste0("video_", name)

  shiny::column(
    width = width,
    shiny::div(
      class = "video-container",
      shiny::strong(shiny::h4(CB_help[[help_name]]$title)) %>% add_helper(help_name),
      shiny::tags$video(
        src = paste0(resource_file(name), ".mp4"),
        width = "100%",
        type = "video/mp4",
        controls = NA
      ),
      shiny::helpText(CB_help[[help_name]]$short)
    )
  )

}

