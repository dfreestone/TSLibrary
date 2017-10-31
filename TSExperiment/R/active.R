# Name   : active.R
# Author : David Freestone (david.freestone@bucknell.edu)
# Date   : 02.17.2017
#
# This time_event module is covered under the modified BSD license
# Copyright (c) 2015, David M. Freestone
# All rights reserved.
#
# Description:
#
GLOBAL_DEBUG = FALSE

#' Return the dropbox paths for the local and experimental computers
#'
#' @return list of dropbox files (local machine and experimental machine)
#' @importFrom magrittr %>%
#' @export
#' @examples
DropBoxPaths <- function(){
  require(jsonlite)
  #TODO(David): Fix this ugly hack
  if (Sys.info()['sysname'] == "Windows"){
    user = Sys.info()["user"]
    DropboxDB = file.path("C:", "Users", user, "AppData",
                          "Local", "Dropbox", "info.json")
  } else {
    DropboxDB = file.path("~", ".dropbox", "info.json")
  }

  if (file.exists(DropboxDB)){
    dropbox = read_json(DropboxDB)
    LocalActiveExperimentPath = file.path(ifelse(!is.null(dropbox$personal$path), dropbox$personal$path, dropbox$business$path),
                                          "lab", "experiments", "active")
    RemoteActiveExperimentPath = 'C:\\Users\\gallistellab\\Dropbox'
  } else {
    stop("No dropbox path on this computer")
  }
  return(list(LocalActiveExperimentPath=LocalActiveExperimentPath,
              RemoteActiveExperimentPath=RemoteActiveExperimentPath))
}

#' Returns the experiment ID from the full experiment name
#'
#' @param experiment Full name of an experiment to identify by its ID
#' @return experiment ID
#' @importFrom magrittr %>%
#' @export
#' @examples
ExperimentID <- function(experiment){
  return(unlist(rbind(lapply(experiment, function(e) {unlist(strsplit(e, "_"))[[1]]}))))
}

#' Returns the experiment name from the experiment ID
#'
#' @param experimentID The ID for an active experiment.
#' @return experiment name
#' @importFrom magrittr %>%
#' @export
#' @examples
ExperimentName <- function(experiment){
  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  return(basename(Sys.glob(file.path(dropbox, paste0(experiment, "_*")))))
}

#' Returns the experiment Path from the experiment ID
#'
#' @param experimentID The ID for an active experiment.
#' @return experiment path
#' @importFrom magrittr %>%
#' @export
#' @examples
ExperimentPath <- function(experiment){
  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  return(Sys.glob(file.path(dropbox, paste0(experiment, "_*"))))
}

#' Returns a list of the active experiments
#'
#' @return List of active experiments
#' @importFrom magrittr %>%
#' @importFrom magrittr equals
#' @export
#' @examples
ActiveExperiments <- function(){
  needs(magrittr)
  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  valid_conditions = Sys.glob(file.path(dropbox, "*", "experiment", "*_conditions.csv")) %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    ExperimentID()

  valid_protocols = Sys.glob(file.path(dropbox, "*", "experiment", paste0(valid_conditions, "_protocol.mpc")))  %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    ExperimentID()


  valid_mpcfolder = Sys.glob(file.path(dropbox, "*", "data", "mpc")) %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    ExperimentID()

  valid_jsonfolder = Sys.glob(file.path(dropbox, "*", "data", "json")) %>%
    dirname() %>%
    dirname() %>%
    basename() %>%
    ExperimentID()

  valid_experiments = rbind(valid_conditions, valid_protocols, valid_mpcfolder, valid_jsonfolder)
  possible_experiments = unique(c(valid_experiments))

  colnames(valid_experiments) = possible_experiments

  isValid = valid_experiments %>%
    apply(1, function(x) {equals(x, possible_experiments)}) %>%
    t()

  if(!all(isValid)){
    print(isValid)

    if (GLOBAL_DEBUG==TRUE){
      warning("One or more experiments are not valid")
    } else{
      stop("One or more experiments are not valid")
    }
  }

  experiments = possible_experiments[isValid %>% apply(2, all)]
  return(experiments[!is.na(experiments)])
}

#' Return the conditions file
#'
#' @param File The conditions file to load
#' @return The loaded conditions file
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadConditionsFile <- function(File){
  require(readr)
  require(dplyr)
  df = suppressMessages(read_csv(File, skip=1)) %>%
    mutate(expt = basename(dirname(dirname(File))))
  return(df)
}

#' Return list of active conditions
#'
#' @return The active conditions, checks for errors
#' @importFrom magrittr %>%
#' @export
#' @examples
ActiveConditions <- function(){
  require(dplyr)
  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  active = ActiveExperiments()

  conditions = Sys.glob(file.path(dropbox, paste0(active, "_*"), "experiment", "*_conditions.csv")) %>%
    lapply(ReadConditionsFile) %>%
    bind_rows()

  duplicates = conditions %>%
    group_by(Cabinet, Box) %>%
    summarize(isDup = n()>1) %>%
    ungroup() %>%
    filter(isDup == TRUE)

  if (nrow(duplicates)>0){
    print(duplicates)
    if (GLOBAL_DEBUG){
      warning("Duplicate macro entries found")
    } else{
      stop("Duplicate macro entries found")
    }
  }
  return(conditions)
}

#' Read all the .999 files from an active experiment
#'
#' @return The ts data frame
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadActiveExperimentFile <- function(experiment){
  needs(TSLib)
  dropbox = DropBoxPaths()$LocalActiveExperimentPath

  files = Sys.glob(file.path(dropbox, paste0(experiment, "_*"), "data", "mpc",
                             paste0(experiment, "_*.999")))
  if (length(files)<1){
    return(NULL)
  }

  mpcpath = dirname(files)[1]
  eventcodepath = file.path(dirname(dirname(mpcpath)), "experiment")

  eventcodes = Sys.glob(file.path(eventcodepath, paste0(experiment, "_eventcodes.csv"))) %>%
    append(file.path(dirname(dropbox), "system", "mouse_eventcodes.csv")) %>%
    read_eventcodes()

  return(files %>%
           mpc_load_files() %>%
           mpc_tidy(eventcodes=eventcodes))
}

#' Read all the .999 files from all active experiments
#'
#' @return The ts data frame
#' @importFrom magrittr %>%
#' @export
#' @examples
ReadActiveExperimentFiles <-function(){
  needs(dplyr)
  return(ActiveExperiments() %>%
           lapply(ReadActiveExperimentFile) %>%
           bind_rows())
}

#' Returns the number of .999 files
#'
#' @return Count of .999 files
#' @importFrom magrittr %>%
#' @export
#' @examples
RecentExperimentActivity <- function(){
  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  experiments = ActiveExperiments()
  files = Sys.glob(file.path(dropbox, paste0(experiments, "_*"), "data", "mpc",
                             paste0(experiments, "_*.999")))
  return(length(files))
}
