



#' @title ProgressCB Class
#' @description S4 class to store and track score assignment progress.
#'
#' @slot link The weblink under which to store the progress.
#' @slot score_assigned A matrix indicating the scores assigned by the user.
#' Rownames correspond to voxel ids.
#' @slot user_affiliation A character string specifying the user's institutional affiliation.
#' @slot user_name A character string with the name of the user.
#'
#' @export
ProgressCB <-
  setClass(
    Class = "ProgressCB",
    slots = list(
      link = "character",
      scores_assigned = "numeric",
      user_affiliation = "character",
      user_name = "character"
    )
  )

