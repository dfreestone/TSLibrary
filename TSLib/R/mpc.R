#' Load all mpc files
#'
#' @param files files to load
#' @param resolution the time resolution
#' @return data.frame with all files together
#' @export
#' @examples
#' data = find_files("/Users/dfreesto/Dropbox/Data/h/*.999") %>%
#'        mpc_load_files()
mpc_load_files = function(files, resolution=0.01)
{
  dplyr::bind_rows(lapply(files, mpc_load_file, resolution=resolution))
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
mpc_load_file = function(file)
{
  # as.numeric converts string names in NA by coercion, and outputs a warning.
  # suppress all warnings, for now, so that real warnings can be read later
  oldw <- getOption("warn")
  options(warn = -1)

  # TODO(David): Probably find a way to speed up the variable/value stuff...
  df = readr::read_csv(file, col_names=c("raw"), col_types="c") %>%
    dplyr::mutate(flush = mpcflushesc(as.numeric(.$raw), length(.$raw))) %>%
    dplyr::group_by(flush) %>%
    dplyr::mutate(subject=raw[7],
                  date=paste0(raw[4], "/", raw[5], "/20", raw[6]),
                  timestamp = as.numeric(raw),
                  event=round(1000 * (timestamp - floor(timestamp)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(subject, date, timestamp, event)

  options(warn=oldw)
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
mpc_tidy = function(df, resolution=0.01, eventcodes=NULL, files=NULL)
{
  if (!is.null(files)) {
    eventcodes = read_eventcodes(files)
  }

  # convert to factors / dates
  df = df %>%
    dplyr::mutate(subject = factor(subject),
                  date = lubridate::mdy(date))

  # Find the write array indices
  if (!is.null(eventcodes)){
    eventcodes = bind_rows(eventcodes, data_frame(event="Variable", code=-1))
    Farray_pattern = c((eventcodes %>% filter(event=="On_WriteVariables"))$code,
                       (eventcodes %>% filter(event=="Off_WriteVariables"))$code)
    df = df %>%
      dplyr::group_by(subject, date) %>%
      dplyr::mutate(variable = trialdef(event, Farray_pattern),
                    between_variable = variable & event!=Farray_pattern[1] & event!=Farray_pattern[2],
                    variable = ifelse(between_variable, timestamp, NA),
                    timestamp = ifelse(between_variable, NA, timestamp),
                    event = ifelse(between_variable, -1, event))
  } # if

  # Adjust timestamps and insert event codes
  df = df %>%
    dplyr::mutate(event = convert_codes_to_events(event, eventcodes),
                  time = ifelse(is.na(timestamp), NA, resolution*floor(timestamp))) %>%
    dplyr::arrange(subject, date) %>%
    dplyr::group_by(subject, date) %>%
    dplyr::mutate(time = adjust_timestamps(time)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(event))

  if ("variable" %in% colnames(df)){
    df = dplyr::select(df, subject, date, time, event, variable)
  } else {
    df = dplyr::select(df, subject, date, time, event)
  }

    return(df)
} # mpc_tidy
