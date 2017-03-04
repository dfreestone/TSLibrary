#' Return raster information for each trial
#'
#' @param event named event column from a data frame
#' @param pattern the c(on, off, end) event information
#' @return data.frame(trial, ontime, offtime) for each state event
#' @export
#' @examples
#' raster(data, c("LeftLeverOn", "LeftLeverOff", "trial_end"))
raster = function(event, pattern)
{
  pattern = codesfor(event, pattern)
  events = as.numeric(event)
  return(rasterc(events, pattern, length(events)))
}

#' Add a raster plot as a geom_segment to an existing ggplot
#'
#' @param data the data.frame
#' @param pattern the c(on, off, end) event information
#' @param ax The ggplot handle
#' @param color the color of the points
#' @return ax the updated ggplot handle
#' @export
#' @examples
#' raster(data, c("LeftLeverOn", "LeftLeverOff", "trial_end"))
rasterplot = function(data, pattern, ax, color="blue")
{

  data = data %>%
    group_by(subject) %>%
    mutate(response = raster(event, pattern)) %>%
    filter(response>0) %>%
    select(c(subject, trial_time, trialN, response)) %>%
    group_by(subject) %>%
    mutate(alt = rep(c("ontime", "offtime"), length(subject)/2)) %>%
    spread(alt, trial_time)

  ax + geom_segment(aes(x=ontime, xend=offtime, y=trialN, yend=trialN),
                    data=data, color=color)
}
