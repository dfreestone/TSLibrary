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

#' Return feeding periods
#'
#' @param events event codes
#' @return an array of feeding period numbers for each event
#' @export
#' @examples
#' periods = feeding_periods(events)
feeding_periods <- function(events){
  # Ensure unique feeding period IDs by offsetting them.
  #   (10*feeding period works as the offset because there cannot be
  #    more than 9 feeding periods in a day.)
  uevents = unique(events)
  feeding_events = as.character(uevents[grepl("On_FeedingPeriod", uevents)])

  trial = rep(0, length(events))
  for (e in feeding_events){
    pattern = c(e, gsub("On", "Off", e))
    offset = 10*as.numeric(substr(e, nchar(e), nchar(e)))
    tmp =  offset * trialdef(events, pattern)
    trial = ifelse(tmp>0, tmp, trial)
  }
  return(trial)
}

#' Return the time of day given a known anchor time
#'
#' @param date date vector
#' @param time time vector
#' @param event event codes
#' @param anchor_time the known wall time of the anchor event
#' @param anchor_event the event for which the wall time is known
#' @return an array of datetimes relative to the anchor event
#' @export
#' @examples
#' timeofday = time_of_day(date, time, event, anchor_time, anchor_event)
time_of_day <- function(date, time, event, anchor_time, anchor_event){
  # It is crucial to pick the most stable time you have,
  #   and it should be early in the day to detect failures that occur after.
  require(lubridate)

  timeofday = make_datetime(year(date), month(date), day(date),
                            anchor_time[1], anchor_time[2])
  timefromanchor = time = time - time[which(event==anchor_event)]
  timeofday = update(timeofday, seconds=timefromanchor)

  return(timeofday)
}

#' Return diagnostics for common MedPC errors
#'
#' @param events event codes
#' @return an data_frame for every on event type
#' @export
#' @examples
#' out = diagnostics(events)
diagnostics <- function(events){
  require(dplyr)
  # TODO(David): Add feeder -> detection events?
  uevents = unique(events)
  on_events = as.character(uevents[grepl("On_", uevents)])

  df = data_frame()
  for (e in on_events){
    pattern = c(e, gsub("On", "Off", e))

    # number of on to off events found
    npairs =  max(trialdef(events, pattern))

    # number of each event individually
    on_count = sum(events==pattern[1])
    off_count = sum(events==pattern[2])

    df = bind_rows(df,
                   data_frame(event = e,
                              npairs = npairs,
                              on_count = on_count,
                              off_count = off_count,
                              difference = on_count - off_count))
  }
  return(df)
}
