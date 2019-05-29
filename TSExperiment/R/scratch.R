# TSExperiment::CreateActiveExperiment('the_data_trail',
#                                      '~/Dropbox (William Paterson)/lab/manuscripts/the_data_trail/R',
#                                      '~/Desktop/xx_conditions.txt',
#                                      '~/Desktop/xx_protocol.mpc',
#                                      '~/Desktop/xx_eventcodes.txt')
#
#
#
#  name <- 'test_experiment'
#  location <- '~/Dropbox (William Paterson)/lab/experiments/experiments'
#
#  conditionsFile <- '~/Desktop/xx_conditions.txt'
#  protocolFile <- '~/Desktop/xx_protocol.mpc'
#  eventcodesFile <- '~/Desktop/xx_eventcodes.txt'
#
#  CreateActiveExperiment(name, location, conditionsFile, protocolFile, eventcodesFile)
#
#  library(magrittr)
#  library(tidyverse)
#
#  data_path <- '~/Dropbox (William Paterson)/lab/experiments/experiments/t_train_minimum_cv/data/mpc/'
#  df <- Sys.glob(file.path(data_path, "*")) %>%
#    TSLib::mpc_load_files() %>%
#    TSLib::mpc_tidy()
#
# df %<>%
#   slice(6:26)
#
# # the conditions file is from a different experiment, but it doesn't matter here...
# df %<>%
#   mutate(subject = 'M097')
#
#  # Any column that's not a number should be a factor
# df %<>%
#    mutate_if(~!is.numeric(.), factor) %>%
#   select(-variable)
#
#  lst <- df %>%
#    select_if(is.factor) %>%
#    map(function(x){list(name = as.character(unique(x)),
#                         code = as.numeric(unique(x)))})
#
#  old_json <- jsonlite::read_json('~/Dropbox (William Paterson)/lab/manuscripts/the_data_trail/R/xx_the_data_trail/data/xx_dataset.json',
#                                  simplifyVector = TRUE)
#  old_json[['data']] <- NULL
#  json <- append(old_json,
#                  list(data = df,
#                  data_factors = lst))
#
#  jsonlite::write_json(json,
#                       path = '~/Desktop/test.json',
#                       dataframe = 'columns',
#                       factor = 'integer',
#                       pretty = TRUE)
#
#  d2 <- jsonlite::read_json(path = '~/Desktop/test.json',
#                            simplifyVector = TRUE)
#
#  data.in <- d2$data %>%
#    as_tibble()
#
#  # TODO(David): Put the factors back...
#
#
