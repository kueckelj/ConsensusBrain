



ConsensusBrainFile <- 
  methods::setClass(
    Class = "ConsensusBrainFile", 
    slots = 
      list(
        
        # user info
        affiliation = "character",
        prename = "character", 
        name = "character", 
        
        # session info
        last_updated = "Date", 
        n_sessions = "numeric",
        options_global = "list",
        
        # results
        score_assignment = "list",
        voxel_scores = "matrix"
        
      )
  )
