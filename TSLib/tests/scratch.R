library(magrittr)
library(tidyverse)

data_path <- file.path('~/Dropbox (William Paterson)/lab/experiments/experiments/u_flash_discrimination/data/mpc')


files <- Sys.glob(file.path(data_path, "u_*.*"))
files <- files[!endsWith(files, '.050')]

df <- TSLib::mpc_load_files(files)
