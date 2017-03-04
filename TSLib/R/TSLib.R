#' TSLib: Analysis tools for timestamped behavioral data
#'
#' @docType package
#' @name TSLib
#' @useDynLib TSLib
NULL

# Logic of the TSLib package:
#
# TSLib is a combination of Russ Church's timelab functions and Randy Gallistel's
# TSLib, both for Matlab. But the tidyverse has a slightly different storage model
# than either of those, and is likely to be better supported moving forward. So it
# seems useful to build TSLib on its back.
#
# The critical feature of Russ's library is a series of useful analysis functions
# applied directly to a session's worth of data. The critical feature of Randy's
# library is a useful set of user-facing high level functions for working with a
# tailored data structure. It stores meta-data (even the protocols that ran the
# experiment) along with the data itself. There is a user-interface, and a set of
# useful plotting functions. The analysis workhorse is applystat, but there aren't
# many specific functions for common computations.
#
# This package seeks to integrate the two libraries within the tidyverse.
#
# The organizational structure is this:
#   [1]: A set of functions for manipulating the underlying time event data.
#         This is common to both libraries, althought [c] below is more developed
#         in Russ's.
#       [a]: functions for reading and combining data from several formats
#       [b]: highly optimized functions for defining trials, obtaining raster
#             information, etc. These are written in Rcpp.
#       [c]: common functions that organically grow from my own analysis.
#   [2]: A set of functions for organizing and storing meta-data.
#         The logic here is a little different from Randy's. In his TSLib, all
#         the data was stored in the same Matlab structure. The problem with this
#         is that it forces the user to use the proprietary Matlab .mat format,
#         and if that single .mat file is lost, everything is. Instead, we use a
#         a series of csv files (so anything can read them) stored in a common
#         location (the TSData directory). Each can be read independently, but
#         there are a set of functions that are aware of their identity. This
#         fulfills the goal of keeping the metadata with the data itself.
#   [3]: A set of functions for displaying information in tables or graphs. These
#         can be used on their own, but their point is to be used with the user-
#         interface.
#   [4]: A shiny app.
#
# The reason for this organizational structure is to allow flexibility at all levels.
# My vision is that users will primarily use layer [2], sometimes dipping to
# layer [1] for more control, and sometimes rising to layer [3] to avoid writing
# their own display / plotting functions.
#
# Critically, this is not meant to replace the tidyverse. Instead, it is a set of
# useful functions for standard data munging and analysis with time stamped
# behavioral data. The analysis itself should still be done using standard R
# packages. For example, the user can read in data with TSupdate, load the data
# with TSload, and then summarize the data with dplyr, and analyze it with rstan.
#
# This approach gives the best of all worlds: A useful set of analysis and plotting
# functions that are data aware, coupled with the integration of metadata with
# timestamped data, while providing all the flexibility that R offers.


# NOTE(David): If it fails to load, you need a new callr:
# source("https://install-github.me/mangothecat/callr")


#___________________________________________________________________________________________
# List of Library Files
# ___________________________________________________________________________________________

#
#  Creating, Loading and Saving Experiment Structures
#   [?] TSinitexperiment            - Creates a new Experiment Structure
#   [?] TSloadexperiment            - Loads an Experiment Structure mat-file
#   [?] TSsaveexperiment            - Saves an Experiment Structure to mat-file
#   [?] TSbegin                     - Leads the user through the process of creating an experimental structure
#   [?] TSstartsession              - Leads user through process of starting a
#                                   session
#
#  Loading Session Data
#   [x] TSloadsessions              - Loads in MED-PC data files
#   [?] TSsetloadparameters         - Sets several loading parameters
#   [?] TSsetoverwritemode          - Sets the overwritemode value
#   [x] TSsetdata                   - Sets the active data
#
#  Creating trials
#   [trialdef] TSdefinetrial               - Creates a trial definition
#   [x] TSsettrial                  - Sets the current trial
#
#  Creating statistics
#   [groupby] TSapplystat                 - Computes user-specified statistics from
#                                   data or results in one or more fields
#                                   in the structure and puts results
#                                   into one or more new fields
#   [groupby] TSsessionstat               - Computes user-specified statistics taking
#                                   TSData as input
#   [groupby] TStrialstat                 - Creates trial statistics from definitions
#                                   user-specified and function
#   [groupby] TScombineover               - Creates fields at a higher level by combining lower data
#   [x] TSlimit                     - Limits what subjects, sessions, phases,
#                                   trials or raw data fields are active
#
#  Working with Experiment structure
#   [trialdef] TSmatch                     - Searches TSdata for event-sequence matches
#   [trialdef] TSparse                     - Parses TSdata matches and evaluates mcode against them
#   [?] TSedit                      - Modifies or inserts TSdata codes found by matching
#
#  Handling Event Codes
#   [import_eventcodes] TSdeclareeventcodes         - Call this to declare the event codes as global and use them
#   [import_eventcodes] TSimporteventcodes          - Imports a text readable list of codenames and values
#   [export_eventcodes] TSexporteventcodes          - Exports event codes from the Experiment to a text file
#   [?] TSsetdefaulteventcodes      - Sets the TSlib default event codes when no Experiment is loaded
#   [add_eventcodes] TSaddeventcodes             - Adds one or more event codes to the current Experiment
#   [rm_eventcodes] TSrmeventcodes              - Removes one or more event codes from the current Experiment
#
#  Special Plotting Functions
#   [__raster__, others] TSraster                    - Creates a raster plot of Time Stamped Data.
#
#  Gui Tools
#   [?] TSexperimentbrowser         - General browsing tool for the Experiment
#   [?] TSrastergui                 - GUI interface to TSraster, makes raster plots
#
#  Miscellaneous
#   [rm_data] TSrmfield                   - Recursively removes fields from the structure
#   [x] TScheckconsistency          - Checks the consistency of the structure.
#   [x] TSaddsubjects               - Allows the user to add subjects to the
#                                 structure by answering the prompts
#
# Installer/Updater
#   [NEED pip] DownloadTSlib               - Download a copy of TSlib
#   [NEED pip] UpdateTSlib                 - Updates installed TSlib to the most curren version
#""
#
#
