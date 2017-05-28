#' Return trial numbers
#'
#' @param events event codes
#' @param pattern a list of event codes for pattern search
#' @return an array of trial numbers for each event
#' @export
#' @examples
#' trial = trialdef(events, pattern)
#trialdef = function(df, pattern)
trialdef = function(events, pattern, fromfirst=FALSE)
{
  if (!is.numeric(events)){
    pattern = codesfor(events, pattern)
    events = as.numeric(events)
  }
  trialdefc(events, pattern,
            length(events), length(pattern)-1,
            fromfirst)
}

# TODO(David): Maybe this belongs in TSExperiment?
#' Return feeding period numbers for the Automated system
#'
#' @param events event codes
#' @return an array of trial numbers for each event
#' @export
#' @examples
#'
feeding_periods <- function(events){
  # Ensure unique feeding period IDs by offsetting them.
  #   (10*feeding period works as the offset because there cannot be
  #    more than 9 feeding periods in a day.)
  uevents = unique(data$event)
  feeding_events = as.character(events[grepl("On_FeedingPeriod", events)])

  trial = rep(0, length(events))
  for (e in feeding_events){
    pattern = c(e, gsub("On", "Off", e))
    offset = 10*as.numeric(substr(e, nchar(e), nchar(e)))
    tmp =  offset * trialdef(events, pattern)
    trial = ifelse(tmp>0, tmp, trial)
  }
  return(trial)
}
