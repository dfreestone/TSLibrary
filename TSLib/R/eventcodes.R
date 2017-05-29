#' Load several event code files into a data.frame
#'
#' @param Files event code files to load
#' @return data.frame will loaded event codes
#' @export
#' @examples
read_eventcodes = function(Files)
{
  require(dplyr)
  bind_rows(lapply(Files, read_eventcode))
}

#' Load a single file of event codes
#'
#' @param File event code file to load
#' @return data.frame of event codes
#' @export
#' @examples
read_eventcode = function(File)
{
  require(readr)
  read_csv(File, col_names=c("event", "code"), col_types="ci")
}

#' Convert numeric event codes to named event codes
#'
#' @param codes List of numeric event codes
#' @param eventcodes List of named:numeric event code mappings
#' @return list of named event codes for each of the numeric codes in 'codes'
#' @export
#' @examples
convert_codes_to_events = function(codes, eventcodes)
{
  factor(codes,  labels=eventcodes$event, levels = eventcodes$code)
}

#' Return event codes for a list of events
#'
#' @param events events in TSData, must be a factor
#' @param codes list of event names
#' @return event codes associated with the events
#' @export
#' @examples
#' eventcodes = codesfor(data, c("trial_start", "trial_end"))
codesfor = function(events, codes)
{
  sapply(codes, function(c) codefor(events, c),  USE.NAMES = FALSE)
}

#' Return an event code for a single event
#'  preserves negation (i.e., "-trial_start)
#'
#' @param events Events in TSData, must be a factor
#' @param codestr a single event
#' @return the event code associated with the event
#' @export
#' @examples
#' eventcodes = codefor(data, "trial_start")
#' eventcodes = codefor(data, "-trial_start")
codefor = function(events, codestr)
{
  if (!is.factor(events))
    stop("events should be a factor variable of event names")

  ifelse(startsWith(codestr, "-"),
         -which(levels(events)==substring(codestr, 2)),
         which(levels(events)==codestr))
}
