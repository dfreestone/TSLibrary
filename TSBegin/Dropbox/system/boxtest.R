library(tidyverse)
library(lubridate)
library(TSExperiment)

result_ = ReadActiveExperimentFiles() %>%
  filter(date == max(date)) %>%
  DetectionLatency() %>%
  CumulativeFoodAmount() %>%
  CumulativeBlockedDeliveries()

EmailConfirm("[Freestone-lab] Your mouse script has run.")
