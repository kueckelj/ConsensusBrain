



#' @title ProgressCB Class
#' @description S4 class to store and track score assignment progress.
#'
#' @slot link The weblink under which to store the progress.
#' @slot score_assigned A data.frame indicating the scores assigned by the user.
#' Required columns: CBscore, x, y, z. (x,y,z are used for left_join())!
#' @slot user_affiliation A character string specifying the user's institutional affiliation.
#' @slot user_name A character string with the name of the user.
#'
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


