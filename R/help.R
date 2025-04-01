


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
          assignment in the 'Score Assignment' box - specifically for margin selections.",
          "",
          "Once confirmed, the margin becomes part of your selection and can be
          refined using the paintbrush-eraser tool.",
          "",
          "The 'Trash' button removes all margins from the current selection.",
          "",
          "(Confirmation of a margin counts as a selection and you can undo
          margin confirmations using the 'Undo' under selection controls.)"
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
          "Sets how many slices the brush selection is propagated through in Ray mode.",
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
          "Controls the direction in which the 2D selection is propagated across slices in Ray mode.",
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
          "**Ray** mode propagates the 2D brush selection across slices in a straight line (e.g., superior-inferior).
          How the selection is propagated depends on the chosen brush scope."
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
          assigned a score. For example, if you've already selected a specific gyrus or
          subcortical structure, refined it with the paintbrush eraser, and
          assigned a score, selecting 'Unscored Only' ensures that only brain tissue
          is selected that has not received a score yet if you use the same selection
          criteria again.",
          "",
          "Disregard:",
          "Selection criteria apply to the whole brain, regardless of
          whether the certain regions have already been scored."
        ),
        type = "inline"
      ),

    "score_description" =
      list(
        content = c(
          "The resectability score reflects the surgical risk associated with removing tissue in a specific region. It guides the planning and use of intraoperative techniques such as neuromonitoring or awake mapping.",
          "",
          "**1 — Safely resectable**: The region can be removed without significant risk to function. Standard resection is expected to be safe.",
          "",
          "**2 — Resectable with low to medium risk**: The region is resectable, but functional risk is present. Resection should involve intraoperative neuromonitoring or mapping techniques. Awake surgery may be considered depending on the functional anatomy.",
          "",
          "**3 — Resectable with medium to high risk**: Resection may still be possible, but the risk to critical functions is higher. Advanced neuromonitoring and awake mapping are typically required.",
          "",
          "**4 — Not resectable**: The region cannot be safely resected due to its functional importance or anatomical constraints. Surgery should avoid this area."
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

    "selection_control" =
      list(
        content = c(
          "**Undo**: Reverts the most recent selection or deselection action. Useful for correcting accidental edits.",
          "",
          "**Clean Debris**: Identifies and removes small groups of spatially isolated voxels that are not part of any meaningful structure.",
          "",
          "**Trash All**: Clears the entire current selection, including manually and automatically selected voxels. This action cannot be undone."
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
        content = c(
          "Choose a tool to interactively define, refine, or erase tissue selections in the MRI viewer.",
          "",
          "**Region-Click**: Double-click a voxel to select its entire anatomical region across the brain, based on the selected selection scope (e.g., a gyrus or white matter region). This is a quick way to select meaningful structures in 3D.",
          #"**Outline**: Draw a freehand outline directly on a 2D MRI slice to define a custom region. The shape can be confirmed and used to select all underlying voxels. Useful for irregular or non-anatomical structures.",
          #"",
          "",
          "**Paintbrush**: Use a circular brush to paint voxels on the current slice. Depending on the selected brush mode (Sphere or Ray), the selection is extended across slices. Ideal for marking localized or tube-like regions.",
          "",
          "**Eraser (Paintbrush + Eraser)**: Works like the paintbrush but in erase mode. You can remove previously selected voxels using the same brush-based interaction. ⚠️ A selection must exist to use this tool.",
          "",
          "**Margin**: Once a selection exists, this tool allows you to add a safety margin around it. The margin thickness can be adjusted, and margins can be confirmed or deleted. ⚠️ Requires an existing selection.",
          "",
          "Use 'Undo' to step back through previous selections and 'Trash' to clear all selections. The chosen tool will define the interaction logic until changed."
        ),
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

