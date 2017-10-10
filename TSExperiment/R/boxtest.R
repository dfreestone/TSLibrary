# boxtest
#    perform standard box tests and email the results
#
# Author(s) : David Freestone (freestoned@wpunj.edu)
#      Date : 2017-05-28
#
# This code is covered under the MIT license
# Copyright (c) , David M. Freestone
# All rights reserved.
#

# -------------------------------------------------------------------- #
# Statistics from h experiment (19,501,302 data points over 220 days)  #
# -------------------------------------------------------------------- #
#
# Amount of food:
#   |mean  |sd    |median  |iqr
#   |2.87  |1.16  |3       |1.02
#
# Left detection latency
#
#   |mean  |sd     |median |iqr
#   |30.35 |694.47 |0.47   |0.43
#
# Right detection latency
#
#   |mean  |sd       |median |iqr
#   |98.81 |1549.66  |0.51   |0.2
#
# Number of blocked deliveries
#   |mean  |sd       |median |iqr
#   |12.54 |37.49    |2      |10
#
# TODO(David): Duration of blocked pellets
#
# The number of discrepencies between the on and offs
#   |mean  |sd     |median |iqr | max | min
#   |0.18  |0.50   |0      |0   |  3  | -1

# TODO(David): Break this up into functions so we can ouput sections
#               to the TSBrowser



#' Perform a boxtest
#'
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
Boxtest <- function(){
  require(dplyr)
  require(TSLib)
  require(ggplot2)

  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  figure_path = file.path(dropbox, "output")
  output_filename = file.path(dropbox, "output", "output.txt")

  # Thresholds for the boxtest
  # TODO(David): Move this to the caller or to a file?
  anchor_event = "Off_Daytime"
  anchor_time = c(10, 30)

  # Values chosen based on the "h" experiment (see above)
  min_food_per_day = 2.8           # grams
  median_latency_tolerance = 0.25  # proportion
  num_blocked_tolerance = 12       # count (mean of "h" experiment because median is 2)
  maximum_time_without_data = 6    # hours
  maximum_number_of_on_off_discrepencies = 1 # count


  start_time = Sys.time()
  sink(file = output_filename)
  cat("---------------------\n")
  cat("Box checks started at", format(start_time), "\n")
  cat("---------------------\n")
  sink()



  # ---------------- #
  # Read Data        #
  # ---------------- #
  # DEBUG with known sessions (in case there aren't 0.999 files)
  # files = "/Users/dmf025/Dropbox/lab/experiments/active/k_time_vs_magnitude/data/mpc/*.042"
  # file = "/Users/dmf025/Dropbox/lab/experiments/system/mouse_eventcodes.csv"
  # data = Sys.glob(files) %>%
  #   mpc_load_files() %>%
  #   mpc_tidy(file=file)
  data = ReadActiveExperimentFiles()
  recent_date = max(data$date)



  # ---------------- #
  # Analysis         #
  # ---------------- #

  ## Summary information about the day ##
  summary = data %>%
    group_by(subject, date) %>%
    summarize(time = max(time)/3600,
                     nDaytime = sum(event=="On_Daytime", na.rm=TRUE),
                     nNighttime = sum(event=="Off_Daytime", na.rm=TRUE))

  ## Summary information about the feeding periods ##
  summary_feedingperiods = data %>%
    group_by(subject, date) %>%
    mutate(feeding = feeding_periods(event)) %>%
    filter(feeding>0) %>%
    mutate(feeding = cumsum(feeding != lag(feeding, default=0))) %>%
    group_by(subject, date, feeding) %>%
    summarize(hasVariables = any(event=="Variable")) %>%
    group_by(subject, date) %>%
    summarize(nFeedingPeriods = length(unique(feeding)),
                     AllFeedingPeriodsHaveVariables = nFeedingPeriods == sum(hasVariables))

  # Summary information about the amount of food
  summary_foodamount = data %>%
    group_by(subject, date) %>%
    summarize(food = sum(event %in% c("On_Left_Feeder", "On_Right_Feeder")),
                     amount = 0.02 * food)


  ## Summary information about the detection latencies ##
  df = data %>%
    group_by(subject, date) %>%
    mutate(left = trialdef(event, c("On_Left_Feeder", "On_Left_Pellet"), fromfirst=TRUE),
                  right = trialdef(event, c("On_Right_Feeder", "On_Right_Pellet"), fromfirst=TRUE))

  left_latencies = df %>%
    group_by(subject, date, left) %>%
    mutate(time = time - time[1]) %>%
    slice(n()) %>%
    filter(left>0) %>%
    arrange(subject, date, time) %>%
    group_by(subject) %>%
    mutate(p = (1:n()) / n(),
           mdn = median(time))

  right_latencies = df %>%
    group_by(subject, date, right) %>%
    mutate(time = time - time[1]) %>%
    slice(n()) %>%
    filter(right>0) %>%
    arrange(subject, date, time) %>%
    group_by(subject) %>%
    mutate(p = (1:n()) / n(),
           mdn = median(time))

  df1 = left_latencies %>%
    group_by(subject, date) %>%
    summarize(left_n = n(),
              left_mdn = mdn[1])

  df2 = right_latencies %>%
    group_by(subject, date) %>%
    summarize(right_n = n(),
              right_mdn = mdn[1])

  summary_latencies = left_join(df1, df2, by=c("subject", "date"))
  rm(df, df1, df2)

  ## Summary information about the blocked deliveries ##
  blocked_deliveries = data %>%
    group_by(subject, date) %>%
    mutate(left = trialdef(event, c("On_Left_Pellet", "On_Left_Feeder", "Off_Left_Pellet")),
                  right = trialdef(event, c("On_Right_Pellet", "On_Right_Feeder", "Off_Right_Pellet"))) %>%
    filter(event %in% c("On_Left_Feeder", "On_Right_Feeder"))

  summary_blocked = blocked_deliveries %>%
    group_by(subject, date) %>%
    summarize(amount = sum(left>0) + sum(right>0))


  ## Get the actual wall time of the last event ##
  summary_lastevent = data %>%
    group_by(subject, date) %>%
    mutate(timeofday = time_of_day(date, time, event, anchor_time, anchor_event)) %>%
    slice(n())


  ## Diagnostics for on-off pairs ##
  summary_discrepencies= data %>%
    group_by(subject, date) %>%
    do(diagnostics(.$event))


  # -------------------------- #
  # Output checks to text file #
  # -------------------------- #
  sink(file = output_filename, append=TRUE)
  cat(sprintf("[%s]: All event times are less than 24 hours\n",
              ifelse(all(summary$time<24), "OK", "FAILED")))

  cat(sprintf("[%s]: Only 1 daytime per day\n",
              ifelse(all(summary$nDaytime==1), "OK", "FAILED")))

  cat(sprintf("[%s]: Only 1 nighttime per day\n",
              ifelse(all(summary$nNighttime==1), "OK", "FAILED")))

  cat(sprintf("[%s]: Variable found per feeding phase\n",
              ifelse(all(summary_feedingperiods$AllFeedingPeriodsHaveVariables), "OK", "FAILED")))

  cat(sprintf("[%s]: All Ons and Offs are consistent (within %d).\n",
              ifelse(all(abs(summary_discrepencies$difference)<=maximum_number_of_on_off_discrepencies),
                     "OK", "FAILED"), maximum_number_of_on_off_discrepencies))

  cat(sprintf("[%s]: All animals received more than %2.1f grams of food\n",
              ifelse(all(summary_foodamount$amount>min_food_per_day), "OK", "FAILED"),
              min_food_per_day))

  cat(sprintf("[%s]: Food detection latencies are normal (within %1.2f from 0.5)\n",
              ifelse(all(abs(summary_latencies$left_mdn - 0.5)<median_latency_tolerance
                         & abs(summary_latencies$right_mdn - 0.5)<median_latency_tolerance),
                     "OK", "FAILED"), median_latency_tolerance))

  cat(sprintf("[%s]: Blocked food deliveries are normal (under %d)\n",
              ifelse(all(summary_blocked$amount <= num_blocked_tolerance), "OK", "FAILED"),
              num_blocked_tolerance))

  cat(sprintf("[%s]: Last event was more recent than %s hours\n",
              ifelse(all(difftime(force_tz(start_time, "UTC"), summary_lastevent$timeofday, units="hours") < maximum_time_without_data),
                     "OK", "FAILED"), maximum_time_without_data))

  sink()

  all_tests_passed = all(all(summary$time<24),
                         all(summary$nDaytime==1),
                         all(summary$nNighttime==1),
                         all(summary_feedingperiods$AllFeedingPeriodsHaveVariables),
                         all(summary$nNighttime==1), # TODO(David): Fix this one
                         all(summary_foodamount$amount>min_food_per_day),
                         all(abs(summary_latencies$left_mdn - 0.5)<median_latency_tolerance & abs(summary_latencies$right_mdn - 0.5)<median_latency_tolerance),
                         all(difftime(force_tz(start_time, "UTC"), summary_lastevent$timeofday, units="hours") < maximum_time_without_data),
                         all(abs(summary_discrepencies$difference)<=maximum_number_of_on_off_discrepencies))


  # ---------------- #
  # Figures          #
  # ---------------- #

  ## Food amounts ##
  ggplot(summary_foodamount) +
    labs(x="subject", y="food amount", title=paste0("date: ", recent_date)) +
    theme(text = element_text(size=18)) +
    geom_bar(aes(x=subject, y=amount), stat="identity") +
    geom_hline(yintercept=3, size=1.2, linetype="dashed") +
    ggsave(file.path(figure_path, "food_amounts.pdf"),
                    dpi=600, height=6, width=6, units="in", device=cairo_pdf)

  feeding_periods = data %>%
    group_by(subject, date) %>%
    filter(event %in% c("On_FeedingPeriod1", "On_FeedingPeriod2",
                               "Off_FeedingPeriod1", "Off_FeedingPeriod2"))

  data %>%
    group_by(subject, date) %>%
    filter(event %in% c("On_Left_Feeder", "On_Right_Feeder")) %>%
    mutate(amount = 0.02 * (1:n())) %>%
    ggplot() +
    theme(text = element_text(size=18)) +
    labs(x="time (in hours)", y="food amount", title=paste0("date: ", recent_date)) +
    coord_cartesian(xlim=c(0, 24)) +
    geom_vline(aes(xintercept=time/3600), size=0.5, linetype="dashed", data=filter(feeding_periods, startsWith(as.character(event), "On_"))) +
    geom_vline(aes(xintercept=time/3600), size=0.5, linetype="dashed", color="#ff7ca3", data=filter(feeding_periods, startsWith(as.character(event), "Off_"))) +
    geom_hline(yintercept=min_food_per_day, size=0.75, color="gray70") +
    geom_line(aes(x=time/3600, y=amount), size=1.2, color="#70a6ff") +
    facet_wrap(~subject) +
    ggsave(file.path(figure_path, "food_amounts_cumulative.pdf"),
                    dpi=600, height=9, width=9, units="in", device=cairo_pdf)

  ## Blocked deliveries ##
  ggplot(summary_blocked) +
    labs(x="subject", y="blocked deliveries", title=paste0("date: ", recent_date)) +
    theme(text = element_text(size=18)) +
    geom_hline(yintercept=num_blocked_tolerance, size=0.75, color="gray70") +
    geom_bar(aes(x=subject, y=amount), stat="identity") +
    ggsave(file.path(figure_path, "blocked_deliveries.pdf"),
                    dpi=600, height=6, width=6, units="in", device=cairo_pdf)

  df = blocked_deliveries %>%
    group_by(subject, date) %>%
    mutate(left = cumsum(left>0),
                  right = cumsum(right>0))

  ggplot(df) +
    theme(text = element_text(size=18)) +
    labs(x="time (in hours)", y="blocked deliveries", title=paste0("date: ", df$date[1])) +
    coord_cartesian(xlim=c(0, 24)) +
    geom_vline(aes(xintercept=time/3600), size=0.5, linetype="dashed", data=filter(feeding_periods, startsWith(as.character(event), "On_"))) +
    geom_vline(aes(xintercept=time/3600), size=0.5, linetype="dashed", color="#ff7ca3", data=filter(feeding_periods, startsWith(as.character(event), "Off_"))) +
    geom_line(aes(x=time/3600, y=left), size=1.2, color="#84c3ff", data=filter(df, left>0)) +
    geom_line(aes(x=time/3600, y=right), size=1.2, color="#ff7ff6", data=filter(df, right>0)) +
    facet_wrap(~subject) +
    ggsave(file.path(figure_path, "blocked_deliveries_cumulative.pdf"),
                    dpi=600, height=9, width=9, units="in", device=cairo_pdf)
  rm(df)

  ## Detection latencies ##
  ggplot(summary_latencies) +
    theme(text = element_text(size=18)) +
    labs(x="detection latency", y="cumulative fraction", title=paste0("date: ", recent_date)) +
    coord_cartesian(ylim=c(0, 1)) +
    geom_hline(yintercept=0.5, size=0.5, linetype="dashed") +
    geom_jitter(aes(x=subject, y=time, group=subject), shape=19, alpha=1/2, color="#84c3ff", width=0.1, data=left_latencies) +
    geom_jitter(aes(x=subject, y=time, group=subject), shape=19, alpha=1/2, color="#ff7ff6", width=0.1, data=right_latencies) +
    geom_point(aes(x=subject, y=mdn, group=subject), size=4, shape=19, alpha=1/2, color="#41a1fc", data=left_latencies) +
    geom_point(aes(x=subject, y=mdn, group=subject), size=4, shape=19, alpha=1/2, color="#ff42f1", data=right_latencies) +
    ggsave(file.path(figure_path, "detection_latencies.pdf"),
                    dpi=600, height=9, width=9, units="in", device=cairo_pdf)

  ggplot() +
    theme(text = element_text(size=18)) +
    labs(x="detection latency", y="cumulative fraction", title=paste0("date: ", recent_date)) +
    coord_cartesian(xlim=c(0, 1)) +
    geom_vline(aes(xintercept=0.5), size=0.5, linetype="dashed", data=filter(feeding_periods, startsWith(as.character(event), "On_"))) +
    geom_line(aes(x=time, y=p), size=1.2, color="#84c3ff", data=left_latencies) +
    geom_line(aes(x=time, y=p), size=1.2, color="#ff7ff6", data=right_latencies) +
    facet_wrap(~subject) +
    ggsave(file.path(figure_path, "detection_latencies_cumulative.pdf"),
                    dpi=600, height=9, width=9, units="in", device=cairo_pdf)



  # ---------------- #
  # Clean up         #
  # ---------------- #
  sink(file = output_filename, append=TRUE)
  cat("\nBoxtest figures written to disk.\n")
  cat(sprintf("Box checks finished at %s [%s seconds]\n", format(Sys.time()),
              round(Sys.time() - start_time, digits=3)))
  sink()

  EmailConfirm(sprintf("[Freestone-lab] Boxtest results: %s",
                       ifelse(all_tests_passed, "OK.", "FAILED.")),
               body=sprintf(paste(readLines(output_filename, encoding="UTF-8"), collapse="\n")),
               attachments=c(file.path(figure_path, "detection_latencies.pdf"),
                             file.path(figure_path, "blocked_deliveries.pdf"),
                             file.path(figure_path, "food_amounts.pdf")))

}
