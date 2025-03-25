


CB_help <-
  list(

    "brain_area" =
      list(
        content = c(
          "Select the brain areas to initiate your selection. You can refine this selection interactively on the MRI planes."
        ),
        type = "inline"
      ),

    "hemisphere" =
      list(
        content = c(
          "Select the hemisphere of interest. At least one must be chosen."
        ),
        type = "inline"
      ),

    "margin_dist" =
      list(
        content = c(
          "Use the slider to define the margin distance from the selected structure.
          Click 'Confirm Margin' to finalize the margin as part of your selection.
          A successful confirmation results in a color change and enables score
          assignment in the 'Score Assignment' box.",
          "",
          "Once confirmed, the margin becomes part of your selection and can be
          refined using the paintbrush-eraser tool.",
          "",
          "The 'Trash' button removes all margins from the current selection.",
          "",
          "If you make an unintended selection or deselection, you can undo
          changes using the 'Undo' button on the left."
        ),
        type = "inline"
      ),

    "outline_options" =
      list(
        content = c(
          "After confirming at least two (optionally three) outlines, use the
          resulting 3D shape to select brain tissue ('Select') or remove it ('Deselect').",
          "",
          "Click 'Reset Outlines' after applying (de-)selection to remove the
          drawings. This does not undo your selection but clears the outlines.",
          "",
          "If the (de-)selection does not meet your expectations, click the
          selection step-backward button on the right to undo the changes and
          refine your outlines."
        ),
        type = "inline"
      ),

    "parcellation_atlas" =
      list(
        content = c(
          "Choose the parcellation atlas for selecting brain areas from cerebral
          lobes. Currently supported options are Desikan-Killiany (less detailed)
          and Destrieux (more detailed)."
        ),
        type = "inline"
      ),

    "paintbrush_depth" =
      list(
        content = c(
          "Sets how many slices the brush selection is propagated through in Beam mode.",
          "",
          "The number defines how deep the brush stroke should go along the selected direction.",
          "",
          "For example, a depth of 5mm will apply the current 2D brush selection to 5 slices."
        ),
        type = "inline"
      ),

    "paintbrush_direction" =
      list(
        content = c(
          "Controls the direction in which the 2D selection is propagated across slices in Beam mode.",
          "",
          "- **Forward**: Propagates the selection in the direction you are looking (e.g., from inferior to superior in axial view).",
          "- **Backward**: Propagates opposite to your view direction.",
          "- **Both**: Propagates equally in both directions from the current slice.",
          "",
          "The direction is relative to your viewing plane and allows targeted or symmetric selection across depth.",
          "",
          "When **Forward** or **Backward** are selected, hovering over a plane makes the respective other two planes visualize the direction as well as the depth with a dotted line."
        ),
        type = "inline"
      ),

    "paintbrush_mode" =
      list(
        content = c(
          "Defines how the selection is extended from 2D into 3D when using the paintbrush tool.",
          "",
          "**Sphere** mode selects voxels in a spherical neighborhood around each brush position, including adjacent slices.",
          "",
          "**Beam** mode propagates the 2D brush selection across slices in a straight line (e.g., superior-inferior), preserving the exact 2D shape while extending its depth.",
          "",
          "Beam mode is useful when you want to apply the same brush stroke across several slices, e.g., for tube-shaped structures or lesion segments."
        ),
        type = "inline"
      ),

    "progress_indicator_tab" =
      list(
        content = c(
          "This plot displays the selection progress within the brain region for
          this Workflow Tab. Click 'View 3D' to visualize progress in 3D, where
          all other brain regions will be shown in grey."
        ),
        type = "inline"
      ),

    "remaining_only" =
      list(
        content = c(
          "Toggle between two options:",
          "",
          "Unscored Only:",
          "Selection criteria apply only to brain tissue that has not yet been
          assigned a score. For example, if you've selected a specific gyrus or
          subcortical structure, refined it with the paintbrush eraser, and
          assigned a score, selecting 'Unscored Only' ensures that your criteria
          apply only to unscored tissue.",
          "",
          "All Tissues:",
          "Selection criteria apply to all relevant brain tissue, regardless of
          whether it has already been scored. This includes both previously scored
          and unscored tissue that meets your selection criteria."
        ),
        type = "inline"
      ),

    "selection_criteria" =
      list(
        content = c(
          "Use these input options to manually define the brain region for selection.",
          "",
          "This selection can be refined interactively using the toolbox below
          the three MRI planes. Note: Updating the selection criteria here will
          overwrite any refinements."
        ),
        type = "inline"
      ),

    "selection_dimensions" =
      list(
        content =
          "Choose whether your selection applies in 2D (only within the current
        MRI slice) or in 3D (propagated throughout the brain).",
        type = "inline"
      ),

    "selection_scope_region_click" =
      list(
        content = c(
          "The Selection Scope defines which anatomical label is used to determine the 3D region selected when using Region-Click mode.",
          "",
          "When the user double-clicks on a voxel in the MRI view, all voxels across the brain that share the same anatomical label and belong to the same hemisphere are immediately selected.",
          "",
          "The available scopes (e.g., Macroanatomical, Desikan-Kiliany, Destrieux) determine the level of anatomical detail. For example, selecting 'Desikan-Kiliany' allows region-clicking to highlight a full cortical area like the superior frontal gyrus.",
          "",
          "This ensures that the selection is anatomically meaningful and consistent, based on the parcellation currently in use."
        ),
        type = "inline"
      ),

    "selection_scope_paintbrush" =
      list(
        content = c(
          "The Selection Scope determines how anatomical boundaries are respected during interactive brushing and subsequent 3D propagation.",
          "",
          "When set to 'None', the brush selects voxels purely based on spatial proximity, without considering anatomical labels or hemisphere boundaries.",
          "",
          "When a parcellation is selected (e.g., Macroanatomical, Desikan-Kiliany, Destrieux), the brush only includes voxels that share the same anatomical label and hemisphere as the voxel initially clicked. This ensures that your selection stays within a clearly defined anatomical region.",
          "",
          "This behaviour becomes particularly important after 'official selection' — that is, when the current 2D brush selection is confirmed and used as the basis for 3D propagation.",
          "",
          "In this case, the propagation respects the selected scope by spreading the selection only across voxels that:",
          "- belong to the same anatomical label,",
          "- lie within the same hemisphere, and",
          "- are spatially continuous based on nearest-neighbour proximity.",
          "",
          "This prevents the brush from unintentionally crossing into adjacent but anatomically distinct regions during 3D extension, providing precise and meaningful anatomical segmentation."
        ),
        type = "inline"
      ),

    "selection_tool" =
      list(
        content =
          "Select a tool for interactive (de-)selection of brain tissue.",
        type = "inline"
      ),

    "score_main" =
      list(
        content = "Assign a score to the selected brain tissue.",
        type = "inline"
      ),

    "score_margin" =
      list(
        content =
          "Assign a score to the brain tissue defined as the margin of your main
        selection. This score can be the same as the main selection score.",
        type = "inline"
      )
  )

