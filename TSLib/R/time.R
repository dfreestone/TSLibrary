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
adjust_timestamps <- function(timestamp){
  timestamp = zoo::na.locf(timestamp, na.rm=F)
  dt = timestamp - lag(timestamp, default=timestamp[1])

  # Negatives exist because of computer reboots
  # Assume no actual time gap (the time gap cannot be known)
  dt[dt<0] = 0
  return(cumsum(dt))
}
