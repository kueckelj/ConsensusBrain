


CB_help <-
  list(

    "brain_area" =
      list(
        content = c(
          "Select the brain areas to initiate your selection. You can refine this selection interactively on the MRI planes."
        ),
        type = "inline"
      ),

    "force_score" =
      list(
        content = c(
          "When refining your selection using the interaction tools, it's common for your selection to include voxels that already have an assigned score.
          By default, **score assignment will not overwrite higher scores**.
          For example, if your selection overlaps voxels already scored as 'Low-Medium' and you assign 'Safely', the existing 'Low-Medium' voxels will remain unchanged,
          while unscored voxels will receive the new 'Safely' score.",
          "",
          "To allow lower scores to overwrite higher ones, check the **Force** option before assigning."
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

    "highlight_control" =
      list(
        content = c(
          "You can highlight specific brain regions on the MRI to make them stand out for easier inspection.
           Use the selection options on the right to choose which regions to highlight.
           Click 'Highlight' to apply your selection.
           Use 'Reset' to remove the current highlighting."
        ),
        type = "inline"
      ),

    "highlight_hover" =
      list(
        content = c(
          "Set to Yes if you want to highlight the region you are currently hovering over."
        ),
        type = "inline"
      ),

    "highlight_scope" =
      list(
        content = c(
          "Select the atlas from which to pick the regions you want to highlight.
          This also affects what is highlighted when hovering over the MRI - if Highlight on Hover is set to Yes."
        ),
        type = "inline"
      ),

    "hover_vars" =
      list(
        content = c(
          "Select what to display as meta information when hovering over the MRI."
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
          "For example, a depth of 5mm will apply the current 2D brush selection to 5 slices.",
          "",
          "You can set depth to 0mm to prevent any 3D propagation and only affect the selection in the slice you are currently drawing on."
        ),
        type = "inline"
      ),

    "paintbrush_direction" =
      list(
        content = c(
          "Controls the direction in which the 2D selection is propagated across slices in Ray mode after clicking on Confirm.",
          "",
          "- **Forward**: Propagates the selection in the direction you are looking (e.g., from inferior to superior in axial view).",
          "- **Backward**: Propagates opposite to your view direction.",
          "",
          "If depth is set to 0mm the selection only applies to the slice you are currently working in."
        ),
        type = "inline"
      ),

    "paintbrush_mode" =
      list(
        content = c(
          "Defines how the paintbrush drawing on a slice is extended from 2D into 3D after clicking on Confirm.",
          "",
          "**Sphere** mode selects voxels in a spherical neighborhood around each brush position, including adjacent slices.",
          "",
          "**Ray** mode propagates the 2D brush selection across slices in a straight line (e.g., superior-inferior).
          How the selection is propagated depends on the chosen scope."
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
          "All:",
          "Selection criteria apply to the whole brain, regardless of
          whether the certain regions have already been scored."
        ),
        type = "inline"
      ),

    "score_description" =
      list(
        content = c(
          "The resectability score reflects the surgical risk associated with removing tissue from a specific brain region. It supports preoperative planning and guides the use of intraoperative techniques such as neuromonitoring or awake mapping.",
          "",
          "**1 — Safely resectable**: The region can be removed without significant risk to function. Standard resection is expected to be safe.",
          "",
          "**2 — Resectable with low to medium risk**: The region is resectable, but carries some functional risk. Intraoperative neuromonitoring or mapping should be used. Awake surgery may be considered depending on functional anatomy.",
          "",
          "**3 — Resectable with medium to high risk**: Resection may be possible, but the risk to critical functions is higher. Advanced neuromonitoring and awake mapping are typically required.",
          "",
          "**4 — Not resectable**: The region cannot be safely removed due to its functional importance or anatomical constraints. Resection should be avoided in this area.",
          "",
          "Click **Assign Score** to apply the selected score to your current selection. If any part of the selection already has a score,
          you can click **Clear** to remove it and reset those voxels to 'Missing'.",
          "",
          "When refining your selection using the interaction tools, it's common for your selection to include voxels that already have an assigned score.
          By default, **score assignment will not overwrite higher scores**.
          For example, if your selection overlaps voxels scored as 'Low-Medium' and you assign 'Safely', the existing 'Low-Medium' voxels will remain unchanged,
          while unscored voxels will receive the new 'Safely' score.",
          "",
          "To allow lower scores to overwrite higher ones, check the **Force** option before assigning."
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
          "**Clean Debris**: Automatically removes small, isolated clusters of voxels that are likely not part of the intended selection.
          This is especially useful after manually erasing with the paintbrush tool, where a few voxels might remain in scattered locations.
          The cleanup ensures that only meaningful selections are kept. If the cleanup removed selections you want to keep, you can always undo
          the removal by clicking on Undo!",
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

    "selection_view" =
      list(
        content = c(
          "Only available if any brain tissue is selected!",
          "",
          "Use the 3D button to visualize your current selection rendered in 3D.
          Use the focus button on the right to shift the localizers to focus on
          your current selection."
        ),
        type = "inline"
      ),

    "selection_tool" =
      list(
        content = c(
          "Choose a tool to interactively define, refine, or erase tissue selections in the MRI viewer.",
          "",
          #"**Region-Click**: Double-click a voxel to select its entire anatomical region across the brain, based on the selected selection scope (e.g., a gyrus or white matter region). This is a quick way to select meaningful structures in 3D.",
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

    # Workflow tab
    "video_mri_interface" = list(
      title = "The MRI Interface",
      short = "Overview of the MRI viewer and available controls.",
      type = "inline",
      content = c(
        "The MRI interface displays the brain in three orthogonal orientations: sagittal, axial, and coronal (left to right).",
        "Depending on the selected interaction tool, contextual information appears at the top and bottom left to guide your actions.",
        "Below the MRI, an information box provides tool-specific instructions.",
        "Scroll or use the slider and the corresponding arrow buttons at the top to navigate through slices in the active plane. The localizer lines in each view indicate the current position of the other two planes.",
        "When hovering over a localizer the cursor shows a 'grab'-symbol. This indicates that you can click + hold and drag the localizer to the desired level.",
        "Zoom in and out using the buttons in the upper left corner of the interface."
      )
    ),

    "video_selection_and_score_assignment" = list(
      title = "Selection and Score Assignment",
      short = "How to inspect a selection and assign a resectability risk score.",
      type = "inline",
      content = c(
        "Selected brain tissue appears in light blue on the MRI across all views.",
        "To inspect your selection in 3D, use the 3D Viewer button at the bottom left. This is useful for verifying whether the selection behaves as expected across adjacent slices, as 2D views can sometimes be misleading.",
        "Click the crosshairs icon to center the view on the current selection across all planes, making navigation easier.",
        "To assign a score, use the buttons located at the top right. These options are only enabled if a voxel selection exists. Select a score and confirm using the Assign button.",
        "After submission, you’ll receive feedback confirming success or explaining any issues. The progress donut chart will automatically update to reflect your changes."
      )
    ),

    "video_the_progress_tab" = list(
      title = "The Progress Tab",
      short = "Visualize and inspect your current scoring progress.",
      type = "inline",
      content = c(
        "The Progress tab shows the distribution of assigned scores across the brain using color-coding. This helps you quickly assess which regions have been reviewed and which remain unscored.",
        "Use the Highlight options to emphasize specific brain regions or score levels.",
        "You can configure the hover behavior in the bottom right corner, choosing what metadata should be shown when pointing to a brain area."
      )
    ),

    "video_score_clearing" = list(
      title = "Clearing Scores",
      short = "Remove scores from a selected brain region.",
      type = "inline",
      content = c(
        "If a selection contains voxels with existing scores, a Clear button appears next to the Assign Score option.",
        "Clicking Clear will reset the score for those voxels to 'Missing', allowing you to revise your assessment or leave the region unscored."
      )
    ),

    "video_score_overwriting_rules" = list(
      title = "Score Overwriting Rules",
      short = "Understand how overlapping scores are handled.",
      type = "inline",
      content = c(
        "When assigning a score to a region that already contains scored voxels, the app enforces protection against unintentionally lowering risk assessments.",
        "By default, voxels with higher existing scores cannot be overwritten by a lower score.",
        "If this happens, a feedback message will inform you that some voxels were excluded from the update.",
        "If you are confident that the entire selection should receive the new score, even if it's lower, you can activate the Force option. This will apply the score regardless of previous assignments."
      )
    ),

    # Selection tab
    "video_brain_regions_and_atlases" = list(
      title = "Selection via Brain Regions and Atlases",
      short = "Use predefined labels and atlases to select regions.",
      type = "inline",
      content = c(
        "The selection bar at the top allows you to choose a brain region to work with.",
        "Depending on your selection, the region-specific options update dynamically, allowing refined targeting through the Atlas Label selector.",
        "For example, in the case of cerebral lobes, you can choose between different atlases to control the granularity of the selection.",
        "Click Select to confirm your choice. The selected region appears on the MRI in light blue."
      )
    ),

    "video_paintbrush" = list(
      title = "Using the Paintbrush",
      short = "Interactively draw custom selections in the MRI.",
      type = "inline",
      content = c(
        "The paintbrush tool enables you to draw selections directly onto the MRI. You can configure the brush size in the bottom bar.",
        "There are two modes: Sphere and Ray. These define how the 2D drawing is translated into 3D voxel selection. Separate videos explain each mode.",
        "To use the tool, follow these steps:",
        "1. Double-click the MRI to activate the brush (you'll see the indicator switch from dotted to solid).",
        "2. Move the cursor to draw over the area of interest.",
        "3. Double-click again to stop drawing. You can resume with another double-click or undo unwanted segments.",
        "4. Once satisfied, click Confirm to finalize your drawing. The result will be interpreted in 3D according to your selected mode."
      )
    ),

    "video_paintbrush_sphere" = list(
      title = "Paintbrush: Sphere Mode",
      short = "Apply brush strokes using a spherical 3D expansion.",
      type = "inline",
      content = c(
        "In Sphere mode, the drawn circle is interpreted as a sphere in 3D space. As you move the cursor across slices, this sphere selects nearby voxels.",
        "This results in a 3D volume that reflects both your 2D input and the brush radius — useful for general anatomical areas where boundaries are less strict."
      )
    ),

    "video_paintbrush_ray" = list(
      title = "Paintbrush: Ray Mode",
      short = "Apply brush strokes in a directional, depth-controlled manner.",
      type = "inline",
      content = c(
        "Ray mode extends your drawing along a ray into the brain volume. You can configure both the depth and the direction of propagation.",
        "While hovering, the other two MRI views show localizer lines that help visualize how far and in which direction your selection will extend.",
        "You can also define a brush Scope, which restricts selection based on anatomical context.",
        "If no scope is selected ('None'), the brush includes voxels based purely on spatial proximity.",
        "If a parcellation is chosen (e.g., Desikan-Killiany, Destrieux), the brush will only select voxels that share the same anatomical label and hemisphere as the starting point.",
        "This ensures your selection respects anatomical boundaries and avoids spillover into unrelated regions."
      )
    ),

    # Refinement tab
    "video_paintbrush_erase" = list(
      title = "Paintbrush Erase Mode",
      short = "Remove voxels from an existing selection.",
      type = "inline",
      content = c(
        "The Paintbrush-Eraser tool works similarly to the regular paintbrush, but instead of adding voxels, it removes them from your current selection.",
        "Use the same brush size and mode configuration as with standard drawing.",
        "After confirming, inspect the result in the MRI and 3D viewer. You can undo erasure actions using the Backward button if needed."
      )
    ),

    "video_undo_and_debris_removal" = list(
      title = "Undoing Changes & Debris Removal",
      short = "Undo unwanted changes and automatically clean up small, isolated voxel clusters.",
      type = "inline",
      content = c(
        "Changes in your selection can always be undone stepwise by clicking on Undo.",
        "During erasing or selection refinement, small disconnected voxel clusters can remain.",
        "Click Clean Debris to automatically remove these. It targets tiny, likely irrelevant clusters based on spatial isolation.",
        "If the cleanup removes desired areas, you can easily undo the action using the Undo button."
      )
    ),

    "video_safety_margin" = list(
      title = "Safety Margin",
      short = "Expand your selection with a configurable buffer.",
      type = "inline",
      content = c(
        "To account for uncertainty or ensure full inclusion of surrounding tissue, you can apply a safety margin to your selection.",
        "Click the Margin option, then use the slider to define the radius (up to 15 mm).",
        "Once applied, margin voxels are marked separately. You can assign a distinct score to the margin in the scoring interface."
      )
    )

  )



