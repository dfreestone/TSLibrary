# Name   : json.R
# Author : David Freestone (david.freestone@bucknell.edu)
# Date   : 02.17.2017
#
# This time_event module is covered under the modified BSD license
# Copyright (c) 2015, David M. Freestone
# All rights reserved.
#
# Description:
#

#' Return a list of the data ready for json
#'
#' @param df A single data frame (from a single subject/day)
#' @return A list ready for jsonification
#' @importFrom magrittr %>%
#' @export
#' @examples
DataListForJson <- function(df){
  jsonlist = list(subject=as.character(df$subject[1]),
                  time = df$time,
                  event = as.numeric(df$event))

  if ("variable" %in% colnames(df)){
    jsonlist = c(jsonlist, list(variable=df$variable))
  }

  return(jsonlist)
}

#' Write a single day's worth of Json Data
#'
#' @param df A processed data file (subject, day, json_lists)
#' @param protocol The active protocol
#' @param conditions The active conditions
#' @param eventcodes The active event codes
#' @param path The save directory (data/json)
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteJsonDataFile <- function(df, path, eventcodes, protocol, conditions){
  needs(jsonlite)
  filename = file.path(path, paste0("Data_", gsub("/", "_", df$date[1]), ".txt"))

  eventcodes = as.list(eventcodes)
  json = list(date=df$date[1], data=df$jsonlist,
              protocol=protocol, conditions=conditions, eventcodes=eventcodes)
  write_json(json, filename)
  return(NULL)
}

#' Return the next archive number
#'
#' @param mpcpath The path to the data/mpc directory
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
NextArchive <- function(mpcpath){
  require(tools)
  archive_number = Sys.glob(file.path(mpcpath, "*")) %>%
    lapply(function(f) as.integer(file_ext(f))) %>%
    unlist() %>%
    unique()

  archive_number = archive_number[!is.na(archive_number)]
  return(ifelse(length(archive_number)<2, 1, 1+max(archive_number[archive_number<999])))
}

#' Archive the .999 files to the data/mpc directory
#'
#' @param mpcpath The path to the data/mpc directory (write to json first!)
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
ArchiveMPC <- function(mpcpath){
  archive_number = NextArchive(mpcpath)

  jsonpath = file.path(dirname(mpcpath), "json")

  current_filenames = Sys.glob(file.path(mpcpath, "*.999"))
  new_filenames = current_filenames %>%
    lapply(function(f) {gsub(".999", sprintf(".%03d", archive_number), f)})

  mapply(file.rename, current_filenames, new_filenames)

}

#' Write an experiment's Json files, and archive the mpc files
#'
#' @param experiment An active experiment ID
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteJsonDataFiles <- function(experiment){
  needs(dplyr, TSLib)

  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  files = Sys.glob(file.path(dropbox, paste0(experiment, "_*"), "data", "mpc",
                             paste0(experiment, "_*.999")))
  if (length(files)<1){
    return(NULL)
  }

  mpcpath = dirname(files)[1]
  jsonpath = file.path(dirname(mpcpath), "json")
  eventcodepath = file.path(dirname(dirname(mpcpath)), "experiment")

  eventcodes = Sys.glob(file.path(eventcodepath, paste0(experiment, "_eventcodes.csv"))) %>%
    append(file.path(dirname(dropbox), "system", "mouse_eventcodes.csv")) %>%
    read_eventcodes()

  protocol = file.path(dirname(dirname(mpcpath)), "experiment", paste0(experiment, "_protocol.mpc")) %>%
    readLines(encoding="UTF-8")

  conditions = file.path(dirname(dirname(mpcpath)), "experiment", paste0(experiment, "_conditions.csv")) %>%
    readLines(encoding="UTF-8")

  result_ = files %>%
    mpc_load_files() %>%
    mpc_tidy(eventcodes=eventcodes) %>%
    group_by(subject, date) %>%
    do(jsonlist = DataListForJson(.)) %>%
    group_by(date) %>%
    do(result = WriteJsonDataFile(., jsonpath, eventcodes, protocol, conditions))

  result_ = ArchiveMPC(mpcpath)
}

#' Archive all active experiments (write json, archive mpc)
#'
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteActiveJson <- function(){
  active = ActiveExperiments() %>%
    lapply(WriteJsonDataFiles)
}

#' Write an experiment's Json files, after the experiment went inactive.
#'
#' @param path A path to a valid experiment folder (most likely inactive.)
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteJsonDataFilesFromInactiveExperiment <- function(path){
  needs(dplyr, TSLib)

  #TODO(David): This function is hacked from the WriteJsonDataFiles function because
  #             I've found I've needed it twice. This should really be cleaned up.
  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  experiment = ExperimentID(basename(path))
  files = Sys.glob(file.path(path, "data", "mpc", paste0(experiment, "_*")))
  if (length(files)<1){
    return(NULL)
  }

  mpcpath = dirname(files)[1]
  jsonpath = file.path(dirname(mpcpath), "json")
  eventcodepath = file.path(dirname(dirname(mpcpath)), "experiment")

  eventcodes = Sys.glob(file.path(eventcodepath, paste0(experiment, "_eventcodes.csv"))) %>%
    append(file.path(dirname(dropbox), "system", "mouse_eventcodes.csv")) %>%
    read_eventcodes()

  protocol = file.path(dirname(dirname(mpcpath)), "experiment", paste0(experiment, "_protocol.mpc")) %>%
    readLines(encoding="UTF-8")

  conditions = file.path(dirname(dirname(mpcpath)), "experiment", paste0(experiment, "_conditions.csv")) %>%
    readLines(encoding="UTF-8")

  result_ = files %>%
    mpc_load_files() %>%
    mpc_tidy(eventcodes=eventcodes) %>%
    distinct() %>%
    group_by(subject, date) %>%
    do(jsonlist = DataListForJson(.)) %>%
    group_by(date) %>%
    do(result = WriteJsonDataFile(., jsonpath, eventcodes, protocol, conditions))

  # NOTE(David): This is only useful if we need to archive...
  #  result_ = ArchiveMPC(mpcpath)
}



#' Read in a single json file
#'
#' @param file A json file to load
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadJsonFile <- function(file){
  needs(dplyr, jsonlite, TSLib)

  json = read_json(file)

  date = unlist(json$date, use.names=FALSE)
  protocol = unlist(json$protocol, use.names=FALSE)
  conditions = unlist(json$conditions, use.names=FALSE)
  eventcodes = data_frame(event = unlist(json$eventcodes$event, use.names=FALSE),
                          code = unlist(json$eventcodes$code, use.names=FALSE))

  n = length(json$data)
  df = data_frame()
  for (i in 1:n){
    subject = unlist(json$data[[i]]$subject, use.names=FALSE)
    time = unlist(json$data[[i]]$time, use.names=FALSE)
    event = as.integer(unlist(json$data[[i]]$event, use.names=FALSE))

    df = bind_rows(df, data_frame(subject=subject, date=date, time=time, event=event))

    if ("variable" %in% names(json$data[[i]])){
      variable = as.integer(unlist(json$data[[i]]$variable, use.names=FALSE))
      df = mutate(df, variable = variable)
    }
  }
  df = df %>%
    mutate(event = convert_codes_to_events(event, eventcodes))
  return(df)
}

#' Read in an entire experiment worth of json data
#'
#' @param experiment An experiment ID
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadExperimentJson <-function(experiment){
  needs(dplyr)
  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  result = Sys.glob(file.path(dirname(dropbox), "**", paste0(experiment, "_*"), "data", "json", "*.txt")) %>%
    lapply(ReadJsonFile) %>%
    bind_rows() %>%
    mutate(experiment = experiment)
  return(result)
}

#' Read in all active experiment json files
#'
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadActiveJson <-function(){
  needs(dplyr)
  result = ActiveExperiments() %>%
    lapply(ReadExperimentJson) %>%
    bind_rows() %>%
    mutate(experiment = factor(experiment))
}

#' Write all the json files from all the mpc files (not just the active ones)
#'
#' @return NULL
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteAllJson <- function(mpcpath){
  needs(dplyr, TSLib)
  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  files = Sys.glob(file.path(mpcpath, "*.*"))

  if (length(files)<1){
    return(NULL)
  }

  jsonpath = file.path(dirname(mpcpath), "json")
  eventcodepath = file.path(dirname(dirname(mpcpath)), "experiment")

  experiment = ExperimentID(basename(dirname(dirname(mpcpath))))
  eventcodes = Sys.glob(file.path(eventcodepath, paste0(experiment, "_eventcodes.csv"))) %>%
    append(file.path(dirname(dropbox), "system", "mouse_eventcodes.csv")) %>%
    read_eventcodes()

  protocol = "NULL"
  conditions = "NULL"

  result_ = files %>%
    mpc_load_files() %>%
    mpc_tidy(eventcodes=eventcodes) %>%
    distinct() %>%
    group_by(subject, date) %>%
    do(jsonlist = DataListForJson(.)) %>%
    group_by(date) %>%
    do(result = WriteJsonDataFile(., jsonpath, eventcodes, protocol, conditions))
}
