


CB_help <-
  list(

    "outline_options" =
      list(
        content = c(
          "Once you have at least two (optionally three) outlines confirmed,
           use the 3D shape resulting from your outlines to select brain
           tissue by clicking on 'Select' or deselect it by clicking
           on 'Deselet'.",
          "",
          "To reset the drawings after your (de-)selection has
           been applied click on 'Reset Outlines'. This does not undo your
           (de)-selectoin but simply resets the drawings.",
          "",
          "If you find that the resulting (de)-selection does not fit
           your expectations you can click on the selection step-backward
           button on the right to undo the changes and adjust your outlines."),
        type = "inline"
      ),

    "selection_dimensions" =
      list(
        content =
          "Whether you want your selection to be applied 2D (only in the slice you are currently working in) or
           whether you want your selection to be propagated 3D throughout the brain.",
        type = "inline"
      ),
    "selection_tool" =
      list(
        content =
          "Choose the tool you want to use for interactive (de-)selection of brain tissue.",
        type = "inline"
      )
  )
