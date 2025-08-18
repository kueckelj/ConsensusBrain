



# DEPRECATED!
#' @export
ProgressCB <-
  setClass(
    Class = "ProgressCB",
    slots = list(
      link = "character",
      scores_assigned = "data.frame",
      user_affiliation = "character",
      user_name = "character"
    )
  )

# Currently active!
#' @export
UserCB <-
  setClass(
    Class = "UserCB",
    slots = list(
      created = "POSIXct",
      last_update = "POSIXct",
      progress = "data.frame",
      user_meta = "list",
      user_name = "character"
    )
  )


userName <- function(object){

  stringr::str_c(object@user_meta$first_name, object@user_meta$last_name, sep = " ")

}

userNameAbbr <- function(object){

  userName(object) %>%
    abbreviate() %>%
    unname()

}


