# TODO(David): Find a better place to put this single file.

#' Adjust the timestamped data
#'  When the time is reset in mpc (e.g, if we restart), time intervals are messed up.
#'  This fixes them by assuming zero delay after the restart.
#'
#' @param time a vector of timestamps
#' @return adjusted timestamps
#' @importFrom magrittr %>%
#' @export
#' @examples
adjust_timestamps <- function(time){
  dt = time - lag(time, default=time[1])
  dt = ifelse(is.na(dt), 0, dt)
  dt = ifelse(dt<0, 0, dt)
  return(cumsum(dt))
}
