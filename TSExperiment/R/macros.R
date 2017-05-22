# Name   : macros.R
# Author : David Freestone (david.freestone@bucknell.edu)
# Date   : 02.17.2017
#
# This time_event module is covered under the modified BSD license
# Copyright (c) 2015, David M. Freestone
# All rights reserved.
#
# Description:
#

#' Write a single subject macro
#'
#' @param df Single row in a conditions file
#' @return TRUE
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteSubjectMacros <- function(df){

  box = df$Box
  cabinet = df$Cabinet
  exptPath = df$expt
  mouse = df$Mouse
  protocol = df$Protocol
  expt = c(strsplit(protocol, "_"))[[1]][1]

  submac_name = paste0("Cabinet", cabinet, "_Box", box, ".mac")

  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  pcdropbox = DropBoxPaths()$RemoteActiveExperimentPath

  macroPath = file.path(dropbox, "macros", "submacs")
  if (!dir.exists(macroPath)){
    dir.create(macroPath, recursive=TRUE)
  }
  cabinetPath = file.path(dirname(macroPath), paste0("Cabinet", cabinet, ".mac"))
  macroPath = file.path(macroPath, submac_name)
  savePath = paste0(pcdropbox, "\\", "lab", "\\", "experiments", "\\", "active", "\\", exptPath, "\\", "data", "\\", "mpc", "\\", expt, "_", mouse, ".999")

  write(sprintf("LOAD BOX %d SUBJ %s EXPT %s GROUP 1 PROGRAM %s \r", box, mouse, expt, protocol), macroPath)
  write(sprintf("FILENAME BOX %d %s \r", box, savePath), macroPath, append=TRUE)

  names = colnames(df)
  for (i in 1:length(names)){
    name = names[i]
    if (any(startsWith(name, c("F", "D")))){
      value = df[[i]]
      line =  sprintf("Set %s Value %f MAINBOX %d BOXES %d \r", name, value, box, box)
      write(line, macroPath, append=TRUE)
    }
  }

  loadMacroFrom = paste0(pcdropbox, "\\", "lab", "\\", "experiments", "\\", "active", "\\", "macros", "\\", "submacs", "\\", submac_name)
  write(sprintf("PLAYMACRO %s \r", loadMacroFrom), cabinetPath, append=TRUE)

  return(TRUE)
}

#' Write the macros for a conditions file
#'
#' @param conditions A conditions file
#' @return None
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteMacros <- function(conditions){
  dropbox = DropBoxPaths()$LocalActiveExperimentPath
  macropath = file.path(dropbox, "macros")

  # Delete the macros, if they already exist
  unlink(macropath, recursive=TRUE)

  conditions %>%
    dplyr::group_by(Mouse) %>%
    dplyr::do(write=WriteSubjectMacros(.))

  write("START BOXES 1 2 3 4 5 6 7 8", file.path(macropath, "CabinetA.mac"), append=TRUE)
  write("START BOXES 1 2 3 4 5 6 7 8", file.path(macropath, "CabinetB.mac"), append=TRUE)
}

#' Write the macros for all active experiments
#'
#' @return None
#' @importFrom magrittr %>%
#' @export
#' @examples
WriteActiveMacros <- function(){
  ActiveConditions() %>%
    WriteMacros()
}


