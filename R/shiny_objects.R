
global_settings_default <-
  list(
    localizer =
      purrr::map(.x = mri_planes, .f = ~ list(col = "red", lwd = 1.5)) %>%
      purrr::set_names(nm = mri_planes),
    mri_slider = list(ticks = TRUE)
  )


vars_brain_ann <- c("hemisphere", "ann_macro", "ann_dk_adj", "ann_dt_adj", "wm_tract")

ann_var_names <-
  c("hemisphere" = "Hemisphere",
    "ann_macro" = "Macroanatomical",
    "ann_dk_adj" = "Desikan-Kiliany",
    "ann_dt_adj" = "Destrieux",
    "wm_tract" = "White Matter Tract")

ann_var_names_rev <- purrr::set_names(x = names(ann_var_names), nm = unname(ann_var_names))

css_styles <-
  list(
    CB_action_button = ".CB-action-btn {
                        background-color: rgba(200, 200,  200, 0.2);
                        color: black;
                        border: 1px solid #ccc;
                        border-radius: 5px;
                        padding: 8px 16px;
                        font-weight: bold;
                        transition: all 0.2s ease-in-out;
                        width: 50%;  /* Make the button fill 50% of the column */
                        display: block;
                        margin: 0 auto; /* Center it */",
    CB_box =
             "background-color: white;
              box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
              display: flex;
              flex-direction: column;
              padding: 10px;
              border: 1px solid #ccc;
              max-width: 100%;
              flex-wrap: wrap;"
  )


# rules:
# 1. first value of choices must correspond to the missing (not-yet-assigned) value
# 2. all values of choices must be of the same class
# 3. length of $choices must be equal to length of $colors
score_set_up = list(
  label = "Resection Risk",
  choices = c("Missing" = 0,
              "Low" = 1,
              "Low-Medium" = 2,
              "Medium" = 3,
              "Medium-High" = 4,
              "High" = 5),
  colors = c("lightgrey", "#4CAF50", "#8BC34A", "#FFC107", "#FF9800", "#F44336")
)


mri_slider_labels <- list(
  sag = c("Right", "Left"),        # Sagittal: Moves left - right
  axi = c("Superior", "Inferior"), # Axial: Moves top - bottom
  cor = c("Anterior", "Posterior") # Coronal: Moves front - back
)


allowedCols <- c("#D4AF37", "#EEDD82", "#ADFF2F", "#a1c935", "#556B2F",
                  "#40E0D0","forestgreen" , "#4682B4", "#6495ED", "#0F52BA",
                  "#483D8B", "#9467bd", "#663399", "#8A2BE2", "#BA55D3", "#6b2d5c")



cortical_regions_dk <-
  list(
    frontal_lobe =
      c("caudalmiddlefrontal", "lateralorbitofrontal", "medialorbitofrontal", "paracentral",
        "parsopercularis", "parsorbitalis", "parstriangularis", "precentral",
        "rostralmiddlefrontal", "superiorfrontal"),
    parietal_lobe = c("inferiorparietal", "postcentral", "precuneus", "supramarginal", "superiorparietal"),
    occipital_lobe = c("cuneus", "lateraloccipital", "lingual", "pericalcarine"),
    temporal_lobe =
      c("Amygdala", "entorhinal", "fusiform", "Hippocampus", "inferiortemporal", "middletemporal", "parahippocampal", "superiortemporal", "transversetemporal"),
    insular_lobe = "insula",
    cingulate_lobe = c("caudalanteriorcingulate", "isthmuscingulate", "posteriorcingulate", "rostralanteriorcingulate")
  )

cortical_regions_dt <- list(
  frontal_lobe = c(
    "G_and_S_frontomargin",
    "G_and_S_transv_frontopol",
    "G_front_inf-Opercular",
    "G_front_inf-Orbital",
    "G_front_inf-Triangul",
    "G_front_middle",
    "G_front_sup",
    "G_precentral",
    "S_front_inf",
    "S_front_middle",
    "S_front_sup",
    "S_precentral-inf-part",
    "S_precentral-sup-part",
    "G_orbital",
    "G_rectus",
    "G_subcallosal",
    "S_orbital_lateral",
    "S_orbital_med-olfact",
    "S_orbital-H_Shaped",
    "S_suborbital",
    "S_central"  # Shared with Parietal
  ),

  temporal_lobe = c(
    "Amygdala",
    "G_temp_sup-G_T_transv",
    "G_temp_sup-Lateral",
    "G_temp_sup-Plan_polar",
    "G_temp_sup-Plan_tempo",
    "G_temporal_inf",
    "G_temporal_middle",
    "G_oc-temp_lat-fusifor",
    "G_oc-temp_med-Lingual",
    "G_oc-temp_med-Parahip",
    "Hippocampus",
    "Pole_temporal",
    "S_temporal_inf",
    "S_temporal_sup",
    "S_temporal_transverse",
    "S_oc-temp_lat",
    "S_oc-temp_med_and_Lingual",
    "S_collat_transv_ant",   # Shared with Occipital
    "S_collat_transv_post"   # Shared with Occipital
  ),

  parietal_lobe = c(
    "G_pariet_inf-Angular",
    "G_pariet_inf-Supramar",
    "G_parietal_sup",
    "G_postcentral",
    "G_precuneus",
    "S_postcentral",
    "S_intrapariet_and_P_trans",
    "S_subparietal",
    "S_interm_prim-Jensen",
    "S_central",  # Shared with Frontal
    "S_pericallosal",  # Shared with Cingulate
    "S_parieto_occipital"  # Shared with Occipital
  ),

  occipital_lobe = c(
    "G_and_S_occipital_inf",
    "G_occipital_middle",
    "G_occipital_sup",
    "G_cuneus",
    "Pole_occipital",
    "S_calcarine",
    "S_parieto_occipital",  # Shared with Parietal
    "S_occipital_ant",
    "S_oc_middle_and_Lunatus",
    "S_oc_sup_and_transversal",
    "S_collat_transv_ant",  # Shared with Temporal
    "S_collat_transv_post"  # Shared with Temporal
  ),

  insular_lobe = c(
    "G_Ins_lg_and_S_cent_ins",
    "G_insular_short",
    "S_circular_insula_ant",
    "S_circular_insula_inf",
    "S_circular_insula_sup"
  ),

  cingulate = c(
    "G_and_S_cingul-Ant",
    "G_and_S_cingul-Mid-Ant",
    "G_and_S_cingul-Mid-Post",
    "G_cingul-Post-dorsal",
    "G_cingul-Post-ventral",
    "S_cingul-Marginalis",
    "S_pericallosal"  # Shared with Parietal
  )
)

cortical_regions <- list(ann_dk_adj = cortical_regions_dk, ann_dt_adj = cortical_regions_dt)


alpha_val <- 0.5
