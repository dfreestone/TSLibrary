# Check to see if no data was written for > 6 hours

#' Obtain and plot the detection latencies
#'
#' @param df timestamped data file
#' @return df the same timestamped data file
#' @export
#' @examples
DetectionLatency <- function(df, return_ax=FALSE){
  trial_n = df %>%
    dplyr::group_by(subject) %>%
    dplyr::summarize(left_food = sum(event=="On_Left_Pellet", na.rm=TRUE),
                     right_food = sum(event=="On_Right_Pellet", na.rm=TRUE))


  trial_left = df %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(trial = TSLib::trialdef(event, c("On_Left_Feeder", "On_Left_Pellet"))) %>%
    dplyr::filter(trial > 0) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(time = time - time[1]) %>%
    dplyr::filter(event == "On_Left_Pellet") %>%
    dplyr::group_by(subject) %>%
    dplyr::arrange(subject, time) %>%
    dplyr::mutate(fraction = seq(1, n()) / n())

  trial_right = df %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(trial = TSLib::trialdef(event, c("On_Right_Feeder", "On_Right_Pellet"))) %>%
    dplyr::filter(trial > 0) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(time = time - time[1]) %>%
    dplyr::filter(event == "On_Right_Pellet") %>%
    dplyr::group_by(subject) %>%
    dplyr::arrange(subject, time) %>%
    dplyr::mutate(fraction = seq(1, n()) / n())

  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  path = file.path(dropbox, "figures", "detection_latency.png")

  ax = ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept=0.5, size=1.2) +
    ggplot2::geom_line(ggplot2::aes(x=time, y=fraction), size=1.2, color="#3399ff", data=trial_left) +
    ggplot2::geom_line(ggplot2::aes(x=time, y=fraction), size=1.2, color="#ff6666", data=trial_right) +
    ggplot2::labs(x="Detection Latency", y="Cumulative Distribution") +
    ggplot2::theme(text = element_text(size=20)) +
    ggplot2::coord_cartesian(xlim=c(0, 1)) +
    ggplot2::facet_wrap(~subject)

  if (return_ax == TRUE){
    return(ax)
  } else {
    ax +  ggplot2::ggsave(path, dpi=300)
   return(df)
  }
}

#' Obtain and plot the cumulative food amounts
#'
#' @param df timestamped data file
#' @return df same timestamped file
#' @export
#' @examples
CumulativeFoodAmount = function(df, return_ax=FALSE){

  # This really should use On_x_Pellet rather than feeder...
  foodtime = df %>%
    filter(event=="On_Left_Feeder" | event=="On_Right_Feeder") %>%
    group_by(subject) %>%
    mutate(time = (time - time[1]) / (60*60),
           cumulative = 1:length(time),
           foodamt = 0.02 * cumulative)

  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  path = file.path(dropbox, "figures", "food_amounts.png")

  ax = ggplot2::ggplot(data=foodtime) +
    ggplot2::geom_hline(yintercept=3, size=1.3) +
    ggplot2::geom_step(ggplot2::aes(x=time, y=foodamt), size=1.3, color="#3399ff") +
    ggplot2::labs(x="Time (hours)", y="Cumulative Food Amount (grams)") +
    ggplot2::theme(text = element_text(size=20)) +
    ggplot2::facet_wrap(~subject)

  if (return_ax == TRUE){
    return(ax)
  } else {
    ax +  ggplot2::ggsave(path, dpi=300)
   return(df)
  }
}

#' Obtain and plot the cumulative number of deliveries while the feeder is blocked
#'
#' @param df timestamped data file
#' @return df same timestamped file
#' @export
#' @examples
CumulativeBlockedDeliveries <- function(df, return_ax=FALSE){
  blocked_left = df %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(time = time - time[1],
                  trial = TSLib::trialdef(event, c("On_Left_Pellet", "On_Left_Feeder", "Off_Left_Pellet"))) %>%
    dplyr::filter(trial>0) %>%
    dplyr::mutate(blockedfood = event=="On_Left_Feeder") %>%
    dplyr::filter(blockedfood>0) %>%
    dplyr::mutate(number = ifelse(n()>0, seq(1, length(time)), 0)) %>%
    dplyr::select(c(subject, time, number))

  blocked_right = df %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(time = time - time[1],
                  trial = TSLib::trialdef(event, c("On_Right_Pellet", "On_Right_Feeder", "Off_Right_Pellet"))) %>%
    dplyr::filter(trial>0) %>%
    dplyr::mutate(blockedfood = event=="On_Right_Feeder") %>%
    dplyr::filter(blockedfood>0) %>%
    dplyr::mutate(number = ifelse(n()>0, seq(1, length(time)), 0)) %>%
    dplyr::select(c(subject, time, number))

  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  path = file.path(dropbox, "figures", "cumulative_blocked.png")

  ax = ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=time, y=number), size=1.2, color="#3399ff", data=blocked_left) +
    ggplot2::geom_line(ggplot2::aes(x=time, y=number), size=1.2, color="#ff6666", data=blocked_right) +
    ggplot2::labs(x="Time (hours)", y="Cumulative Blocked Deliveries") +
    ggplot2::theme(text = element_text(size=20)) +
    ggplot2::facet_wrap(~subject)

  if (return_ax == TRUE){
    return(ax)
  } else {
    ax +  ggplot2::ggsave(path, dpi=300)
   return(df)
  }
}
