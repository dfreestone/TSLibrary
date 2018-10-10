#' Load all mpc files
#'
#' @param files files to load
#' @param resolution the time resolution
#' @return data.frame with all files together
#' @export
#' @examples
#' data = find_files("/Users/dfreesto/Dropbox/Data/h/*.999") %>%
#'        mpc_load_files()
mpc_load_files <- function(files) {
  files %>%
    purrr::map_df(mpc_load_file)
}

#' Load a single mpc file
#'
#' @param file file to load
#' @param resolution the time resolution
#' @param writeFArray the event codes for the start and end of writing variables to the F array
#' @return data.frame of a habitest file
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data = mpc_load_file(file)
mpc_load_file <- function(file) {
  # as.numeric converts string names in NA by coercion, and outputs a warning.
  # suppress all warnings, for now, so that real warnings can be read later
  oldw <- getOption("warn")
  options(warn = -1)

  # TODO(David): Below is NOT the writetime, its the time the program started.
  #             it would be nice to get the write time.
  # writetime = paste0(raw[11], ":", raw[12], ".", raw[13]),
  df <- readr::read_csv(file, col_names = c("raw"), col_types = "c") %>%
    #dplyr::mutate(flush = mpcflushesc(as.numeric(.$raw), length(.$raw))) %>%
    dplyr::group_by(flush) %>%
    dplyr::mutate(
      subject = raw[7],
      box = as.numeric(raw[10]),
      date = paste0(
        stringr::str_pad(raw[4], width = 2, side = "left", pad = "0"), "/",
        stringr::str_pad(raw[5], width = 2, side = "left", pad = "0"), "/",
        raw[6]
      )
      # NOTE(David): Commented out below on 09.01.2018. Delete after a while
      #timestamp = as.numeric(raw),
      #event = round(1000 * (timestamp - floor(timestamp)))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(subject, box, date, raw)

  # If we issue a flush command (any of them), it does not seal the array. The
  #   result are loads of zeros at the end of the array, that really mess up the
  #   mpcflushesc algorithm, since the number of elements is not written to the disk
  # I don't know why it does this (or how it's possible), but that completely
  # messes up the reading of the next chunk. But, since we're processing one file
  # at a time, we can just take the info from the previous chunk, because it'll
  # necessarily be the same

  options(warn = oldw)
  return(df)
}

#' Cleanup mpc data.
#'
#' @param df data.frame of loaded mpc files
#' @param eventcodes A data_frame of event codes
#' @param files A list of csv files that specify event codes
#' @return data.frame with named event codes, and session numbers
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data = find_files("~/Dropbox/Data/h/*.999") %>%
#'        mpc_load_files() %>%
#'        mpc_tidy(eventCodeFiles)
mpc_tidy <- function(df, resolution = 0.01, eventcodes = NULL, files = NULL) {

  # as.numeric converts string names in NA by coercion, and outputs a warning.
  # suppress all warnings, for now, so that real warnings can be read later
  oldw <- getOption("warn")
  options(warn = -1)

  # convert to factors / dates
  df %<>%
    mutate(
      subject = factor(subject),
      date = lubridate::mdy(date),
      raw = as.numeric(raw),
      possible_time = resolution * floor(raw),
      possible_event = round(1000 * (raw - floor(raw))),
      variable = NA
    )

  if (!is.null(files)) {
    eventcodes <- read_eventcodes(files)
  }

  # Find the write array indices
   if (!is.null(eventcodes)) {
     eventcodes <- bind_rows(eventcodes, data_frame(event = "Variable", code = -1))
     Farray_pattern <- c(
       (eventcodes %>% filter(event == "On_WriteVariables"))$code,
       (eventcodes %>% filter(event == "Off_WriteVariables"))$code
     )
     df %<>%
       group_by(subject, date) %>%
       mutate(on_write = trialdef(possible_event, Farray_pattern, fromfirst = TRUE)) %>%
       group_by(subject, date, on_write) %>%
       mutate(index = 1:n(),
              between_variable = ifelse((on_write > 0) & (index != 1) & (index != n()),
                                        TRUE, FALSE),
              variable = ifelse(between_variable, raw, NA),
              possible_event = ifelse(between_variable, -1, possible_event)) %>%
       ungroup()
   } # if

  df %<>%
    filter((possible_event > 0) | (possible_event == -1)) %>%
    mutate(time = possible_time,
           event = possible_event)

  # NOTE(David): The adjusting timestamps is no longer needed after 2017.
  # Adjust timestamps and insert event codes
  #   adjusting the timestamps is necessary because sometimes
  #   [1] the timestamps reset because of overflow
  #   [2] the timestamps reset because we rebooted medpc
  #   this makes the timestamps unreliable for time-of-day estimation anyway
  #   so convert things relative to the time since the start of the date
  #   (which is time since the first event, this is often near the time of the
  #    daytime onset, but does not have to be.)
  #df %<>%
  #  arrange(subject, date) %>%
  #  group_by(subject, date) %>%
  #  #filter((event != 0)) %>%
  #  mutate(
  #    #timestamp = adjust_timestamps(floor(timestamp)), DMF time adjusts at midnight now. Thi isn't needed
  #    time = ifelse(event == 0, NA, resolution * floor(timestamp))
  #  ) %>%
  #  ungroup()

  if (!is.null(eventcodes)) {
    df %<>%
      mutate(event = convert_codes_to_events(event, eventcodes))
  }

  if ("variable" %in% colnames(df)) {
    df <- select(df, subject, box, date, time, event, variable)
  } else {
    df <- select(df, subject, box, date, time, event)
  }

  options(warn = oldw)
  return(df)
} # mpc_tidy
