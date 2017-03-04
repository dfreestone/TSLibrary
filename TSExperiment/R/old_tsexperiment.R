##TODO(David): TSprotocol should attach a specific protocol to a subject/session
#
## Note all outward facing functions take input as Data, all inward facing functions
## take input as df.
#
##' Workhorse for creating and updating a TSExperiment
##'
##' @param ExperimentPath The path to the experiment folder
##' @param EventCodePaths The paths to the event code files
##' @param DataPaths The paths to the data files
##' @param ProtocolPaths The paths to the protocols
##' @importFrom magrittr %>%
##' @return NULL
##' @export
##' @examples
#TSupdate <- function(ExperimentPath, DataPaths=NULL, EventCodePaths=NULL, ProtocolPaths=NULL){
#
#  dir.create(file.path(ExperimentPath, "TSData"), showWarnings = FALSE)
#
#  # Read user paths, or create if userpaths.csv doesn't exist.
#  if (file.exists(TSuserpaths(ExperimentPath))){
#    userpaths = readr::read_csv(TSuserpaths(ExperimentPath), col_types="cc")
#  } else{
#    if (is.null(EventCodePaths) | is.null(DataPaths)){
#      stop("There is no userpaths.csv file. You must supply Event Code and Data Paths so we know where to look")
#    }
#    userpaths = dplyr::bind_rows(list(dplyr::data_frame(PathType = "EventCode", Path = EventCodePaths),
#                                      dplyr::data_frame(PathType = "Data", Path = DataPaths),
#                                      dplyr::data_frame(PathType = "Protocol", Path = ProtocolPaths)))
#    readr::write_csv(userpaths, TSuserpaths(ExperimentPath))
#  }
#
#
#  #TODO(David): Allow the user to overwrite or append? Currently OVERWRITES
#  #             Uncomment the 3 lines in the below three segments to change this
#  #             from OVERWRITES to APPENDS
#  #
#  #             The drawback to this is that the user must now supply the paths
#  #             everytime, because it will rewrite the userpaths for every new
#  #             computer that runs the code with different paths.
#  if (!is.null(DataPaths)){
#    userpaths = dplyr::data_frame(PathType = "Data", Path = DataPaths) %>%
##                dplyr::setdiff(userpaths) %>%
##                dplyr::bind_rows(list(userpaths, .)) %>%
##                dplyr::distinct() %>%
#                dplyr::arrange(PathType)
#    readr::write_csv(userpaths, TSuserpaths(ExperimentPath))
#  } # DataPaths
#
#  if (!is.null(EventCodePaths)){
#    userpaths = dplyr::data_frame(PathType = "EventCode", Path = EventCodePaths) %>%
##                dplyr::setdiff(userpaths) %>%
##                dplyr::bind_rows(list(userpaths, .)) %>%
##                dplyr::distinct() %>%
#                dplyr::arrange(PathType)
#    readr::write_csv(userpaths, TSuserpaths(ExperimentPath))
#  } # EventCodePaths
#
#  if (!is.null(ProtocolPaths)){
#    userpaths = dplyr::data_frame(PathType = "Protocol", Path = ProtocolPaths) %>%
##                dplyr::setdiff(userpaths) %>%
##                dplyr::bind_rows(list(userpaths, .)) %>%
##                dplyr::distinct() %>%
#                dplyr::arrange(PathType)
#    readr::write_csv(userpaths, TSuserpaths(ExperimentPath))
#  } # ProtocolPaths
#
#  EventCodes = userpaths %>%
#               filter(PathType == "EventCode") %>%
#               select(Path)
#  DataFiles = userpaths %>%
#              filter(PathType == "Data") %>%
#              select(Path) %>%
#              sapply(Sys.glob)
#
#  if (file.exists(TSDataFiles(ExperimentPath))){
#    pFiles = readr::read_csv(TSDataFiles(ExperimentPath), col_names="File", col_types="c")$File
#    pFiles = sapply(pFiles, basename, USE.NAMES=FALSE)
#    nFiles = sapply(DataFiles, basename, USE.NAMES=FALSE)
#    DataFiles = DataFiles[!(nFiles %in% pFiles)]
#  } # file.exists
#
#  Data = dplyr::data_frame()
#  if (length(DataFiles)>0){
#    Data = DataFiles %>%
#      mpc_load_files() %>%
#      mpc_tidy(files=EventCodes$Path)
#
#    TScheckprotocol(ExperimentPath)
#    TSchecksubjects(ExperimentPath, Data)
#    TSsave(ExperimentPath, Data)
#    write(DataFiles, TSDataFiles(ExperimentPath), sep=",", append=TRUE)
#  }
#  return(NULL)
#} # TSrun
#
##' Load an TSExperiment Data File
##'
##' @param ExperimentPath The path to the experiment folder
##' @return A TSData frame
##' @importFrom magrittr %>%
##' @export
##' @examples
#TSload <- function(ExperimentPath){
#  Data = dplyr::data_frame()
#  if (file.exists(TSDataFile(ExperimentPath))){
#    Data = readr::read_csv(TSDataFile(ExperimentPath), col_types="ccdcd") %>%
#           dplyr::mutate(subject=factor(subject),
#                         date=factor(date),
#                         event=factor(event))
#    TSchecksubjects(ExperimentPath, Data)
#  } # file.exists
#  return(Data)
#}
#
##' Add subject information to the TSData frame
##'
##' @param ExperimentPath The path to the experiment folder
##' @param Data The TSData frame
##' @return A TSData frame with the subject information added
##' @importFrom magrittr %>%
##' @export
##' @examples
#TSaddSubjectData <- function(ExperimentPath, Data){
#  SubjectInfo = readr::read_csv(TSSubjectFile(ExperimentPath), col_types="ccccccccccc") %>%
#                dplyr::mutate(subject=factor(subject))
#  df = dplyr::left_join(Data, SubjectInfo, by="subject")
#  return(df)
#}
#
##' Defines the trial
##'
##' @param df The data
##' @param trialname The name to give to the trial
##' @param trial The pattern to search for
##' @return Data with a column of trial information
##' @importFrom magrittr %>%
##' @export
##' @examples
#TSdefinetrial <- function(df, trialname, trial){
#  # TODO(David): And multiple trials at the same time?
#  trialname = paste0("trial_", trialname)
#  trialval <- lazyeval::interp(~ trialdef(event, trial), trial=trial)
#  return(Data %>%
#           dplyr::group_by(subject, date) %>%
#           dplyr::mutate_(.dots=setNames(list(trialval), trialname)))
#}
#
##' Lists unique dates in TSData
##'
##' @param Data The data
##' @return The unique dates
##' @export
##' @examples
#TSlistdate <- function(Data){
#  return(as.character(unique(Data$date)))
#}
#
##' Lists unique sessions in TSData
##'
##' @param Data The data
##' @return The unique sessions
##' @export
##' @examples
#TSlistsession <- function(Data){
#  return(as.character(unique(Data$session)))
#}
#
##' Lists unique subjects in TSData
##'
##' @param Data The data
##' @return The unique subjects
##' @export
##' @examples
#TSlistsubject <- function(Data){
#  return(as.character(unique(Data$subject)))
#}
#
##' Lists unique trials in TSData
##'
##' @param Data The data
##' @return The unique trials
##' @export
##' @examples
#TSlisttrials <- function(Data){
#  cols = colnames(Data)
#  return(gsub("trial_", "", cols[(startsWith(colnames(Data), "trial_"))]))
#}
#
##' Lists unique trial numbers from a trial in TSData
##'
##' @param Data The data
##' @param trialname The trial name to list
##' @return The unique trial numbers
##' @export
##' @examples
#TSlisttrialnumbers <- function(Data, trialname){
#  trialname = paste0("trial_", trialname)
#  return(as.character(unique(Data[[trialname]])))
#}
#
##' Returns the time event record from a single trial (time is relative)
##'
##' @param Data The data
##' @param Subject The subject
##' @param Date The date
##' @param Trial The trial name
##' @param Trialnumber The trial number
##' @return The time event record from a single trial
##' @importFrom magrittr %>%
##' @export
##' @examples
#TSlisttrialdata <- function(Data, Subject, Date, Trial, Trialnumber){
#  trialname = paste0("trial_", Trial)
#  subjectfilter = lazyeval::interp(~subject == Subject, Subject = Subject)
#  datefilter = lazyeval::interp(~date == Date, Date = Date)
#  trialfilter = lazyeval::interp(~trialname == Trialnumber, trialname=as.name(trialname))
#  return(Data %>%
#           dplyr::ungroup() %>%
#           dplyr::filter_(subjectfilter, datefilter, trialfilter) %>%
#           dplyr::mutate(time = time - time[1]) %>%
#           dplyr::select(c(time, event)))
#}
#
##' Checks to see if there's been a change to the protocol
##'
##' @param ExperimentPath The path to the experiment
##' @return NULL
##' @importFrom magrittr %>%
##' @export
##' @examples
#TScheckprotocol <- function(ExperimentPath){
#  protocol = readr::read_csv(TSuserpaths(ExperimentPath), col_types="cc") %>%
#             dplyr::filter(PathType == "Protocol") %>%
#             dplyr::select(Path)
#  protocol = protocol$Path
#
#  hash = protocol%>%
#         readLines(encoding = "UTF-8") %>%
#         digest::sha1()
#
#  if (file.exists(TSprotocolFileList(ExperimentPath))){
#    protocols = readr::read_csv(TSprotocolFileList(ExperimentPath), col_types="cc")
#    if (!(hash %in% protocols$hash)){
#      TSaddprotocol(ExperimentPath, protocol, hash)
#    }
#  } else{
#    TSaddprotocol(ExperimentPath, protocol, hash)
#  }
#}
#
##' Adds a protocol
##'
##' @param ExperimentPath The path to the experiment
##' @param protocol The path to the new protocol
##' @param hash The hash (checksum) of the new protocol
##' @return NULL
##' @importFrom magrittr %>%
##' @export
##' @examples
#TSaddprotocol <- function(ExperimentPath, protocol, hash){
#  filenameWithExt = basename(protocol)
#  filenameWithoutExt = filenameWithExt %>% substr(1, nchar(.)-4)
#  extension = filenameWithExt %>% substr(nchar(.)-3, nchar(.))
#
#  newfilename = file.path(TSprotocolPath(ExperimentPath),
#                          paste0(filenameWithoutExt, "_", hash, extension))
#
#  dir.create(TSprotocolPath(ExperimentPath), showWarnings = FALSE)
#  file.copy(protocol, newfilename, copy.date=TRUE)
#
#  cols = !file.exists(TSprotocolFileList(ExperimentPath))
#  readr::write_csv(dplyr::data_frame(hash=hash, file=basename(newfilename)),
#            TSprotocolFileList(ExperimentPath),
#            append=TRUE, col_names=cols)
#}
#
#
#
#
## Internal Functions -----------------------------------------------------------
#TSuserpaths <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "UserPaths.csv"))
#}
#
#TSEventCodeFile <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "EventCodes.csv"))
#}
#
#TSDataFiles <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "DataFiles.csv"))
#}
#
#TSDataFile <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "Data.csv"))
#}
#
#TSSubjectFile <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "Subjects.csv"))
#}
#
#TSprotocolPath <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "Protocols"))
#}
#
#TSprotocolFileList <- function(ExperimentPath){
#  return(file.path(ExperimentPath, "TSData", "Protocols.csv"))
#}
#
#TSsave <- function(ExperimentPath, Data){
#  colnames = !file.exists(TSDataFile(ExperimentPath))
#  readr::write_csv(Data, TSDataFile(ExperimentPath), append=TRUE, col_names=colnames)
#  return(NULL)
#}
#
#TSsubject <- function(ExperimentPath, subjects){
#  df = dplyr::data_frame(subject=subjects,
#                         laboratory=NA,
#                         experiment_name=NA,
#                         protocol=NA,
#                         supplier=NA,
#                         species=NA,
#                         strain=NA,
#                         sex=NA,
#                         arrival_date=NA,
#                         arrival_weight=NA,
#                         analysis=NA)
#  colnames = !file.exists(TSSubjectFile(ExperimentPath))
#  readr::write_csv(df, TSSubjectFile(ExperimentPath), na="", append=TRUE, col_names=colnames)
#}
#
#TSchecksubjects <- function(ExperimentPath, Data){
#  if (!file.exists(TSSubjectFile(ExperimentPath))){
#    TSsubject(ExperimentPath, unique(Data$subject))
#  } else{
#    SubjectInfo = readr::read_csv(TSSubjectFile(ExperimentPath), col_types="ccccccccccc")
#    SubjectsInData = unique(Data$subject)
#    SubjectsOnFile = factor(SubjectInfo$subject)
#
#    pna = sum(is.na(SubjectInfo)) / prod(dim(SubjectInfo))
#    if (pna>0.5){
#      warning("Most of the Subjects file has not been filled out. Fix this.")
#    }
#
#    SubjectsnotOnFile = setdiff(SubjectsInData, SubjectsOnFile)
#    if (length(SubjectsnotOnFile)>0){
#      TSsubject(ExperimentPath, SubjectsnotOnFile)
#      warning("At least one subject in the data but not in the subjects file. These were added to the subjects file")
#    }
#
#    SubjectsnotInData = setdiff(SubjectsOnFile, SubjectsInData)
#    if (length(SubjectsnotInData)>0){
#      warning("At least one subject in the subjects file but not in the data. Fix this.")
#    }
#  }
#}
#
#TSdiagnostics <- function(Data){
#  # Checks common Errors like a on without an off
#  #   This is as easy as a series of trialdefs that go from on to on without an
#  #   off in between. This will return the start, stop, number, and everthing in
#  #   between. The user can decide what to do with this information.
#  return(NULL)
#}
#
#TSstartsession <- function(){
## Starts a session in the 24/7 chamber?
# return(NULL)
#}
#
#TSendsession <- function(){
## Starts a session in the 24/7 chamber?
# return(NULL)
#}
#
#TSactive <- function(){
#  # A file with a list of active experiments (ActiveExperiments)
#  # Maybe we can use this to do away with the dependency on "ExperimentPath"
#  return(NULL)
#}
#
#TSAnalysis <- function(){
#  # Runs the automated analysis associated with each subject
#  return(NULL)
#}
#
#TSroomtemperature <- function(){
#  # Allow for the room temperature input on certain days (High/Low)
#  return(NULL)
#}
#
#TSraster <- function(){
#  # Plots a raster.
#  #   (should be moved to analysis.R or plots.R)
#  return(NULL)
#}
#
#TScumulativerecord <- function(){
#  # Plots a cumulative record.
#  #   (should be moved to analysis.R or plots.R)
#  return(NULL)
#}
#
#TScdf <- function(){
#  # Plots a cumulative distribution function
#  #   (should be moved to analysis.R or plots.R)
#  return(NULL)
#}
#
#TSorderevents <- function(){
#  # Reorders events with the same timestampe based on some priority of events.
#  return(NULL)
#}
#
#TSeventname <- function(){
#  # returns the event name associated with an event code
#  #   (this is redundant with codesfor, but backward. Maybe remove?)
#  return(NULL)
#}
#
#TSedit <- function(){
#  # Adds or removes an event code based on matching codes.
#  return(NULL)
#}
#
#TSaddeventcodes <- function(){
#  # Adds event codes to the list of event codes
#  #   (may be redundant with codes already in TSLib)
#  return(NULL)
#}
#
#TSbegin <- function(){
#  # Sets up an experiment structure
#  #   (probably redundant with TSupdate)
#  return(NULL)
#}
#
#TSaddlog <- function(){
#  # Adds notes (to specific date/time/subject?)
#  return(NULL)
#}
#
#TSemail <- function(){
#  # Emails user
#  #   (redundant with email in TSLib?)
#  return(NULL)
#}
#
#TSprotocol <- function(){
#  # Adds a protocol for a particular subject for a particular session?
#  return(NULL)
#}
#
## Notes on the following functions ---------------------------------------------
##
## The following functions played prominantly in Randy's TSLib Matlab toolbox,
## but do not have a place here. combineover is only useful if the data is stored
## in a Matlab structure. Both functions are superseced by mutate and summarize.
#TScombineover <- function(){
#  # Combines the result over one layer of the Matlab structure
#  #   Its basically summarize
#  # See notes above
#  return(NULL)
#}
#
#TSapplystat <- function(){
#  # Applies a statistic to the same level of the Matlab structure, can input
#  #   more than one stat to apply
#  #   Its basically mutate
#  # See notes above
#  return(NULL)
#}
