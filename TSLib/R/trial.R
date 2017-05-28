# TODO(David): Now that there's only one function in here, maybe move it?
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
