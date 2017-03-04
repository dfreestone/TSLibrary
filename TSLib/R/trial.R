#' Return trial numbers
#'
#' @param events event codes
#' @param pattern a list of event codes for pattern search
#' @return an array of trial numbers for each event
#' @export
#' @examples
#' trial = trialdef(events, pattern)
#trialdef = function(df, pattern)
trialdef = function(events, pattern)
{
  if (!is.numeric(events)){
    pattern = codesfor(events, pattern)
    events = as.numeric(events)
  }
  trialdefc(events, pattern,
            length(events), length(pattern)-1)
}


# ------------------------------------------------------------------------------
# Keeping, for now. But probably not needed
# #' Return the trial time
# #'  (meant for use in a group_by)
# #'
# #' @param t: absolute trial time
# #' @return time relative to the start of the trial
# #' @examples
# #' data = group_by(subject, session, trial) %>%
# #'        mutate(trialtime = relative_time(time))
# relative_time = function(t)
# {
#   t - t[1]
# }
#
# #' Return the absolute trial numbers
# #'  (meant for ignoring session/subject boundaries)
# #'
# #' @param t: relative trial numbers
# #' @return absolute trial number, ignoring boundaries
# #' @examples
# #' data = group_by(subject) %>%
# #'        mutate(trialN = cumulative_trial(trial))
# cumulative_trial = function(t)
# {
#   t = c(0, t)
#   n = length(t)
#   cumsum(t[1:n-1]!=t[2:n])
# }
