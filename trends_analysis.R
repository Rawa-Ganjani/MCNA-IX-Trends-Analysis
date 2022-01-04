rm(list = ls(all = T))
library(xlsx)
library(dplyr)
library(hypegrammaR)
library(surveyweights)
library(readr)
library(readxl)
library(lubridate)
library(doParallel)
library(foreach)
library(expss)

source(file = "functions/correct_zeros.R")

aggregate <- "all"
disaggregate <- "all"
dap_name <- "dap_trends_analysis"

source("script_2021.R")

source("script_2020.R")

source("script_2019.R")

source("script_2018.R")

source("script_2017.R")

findings <- rbind(findings_2021, findings_2020, findings_2019, findings_2018, findings_2017)

write.csv(findings,
          sprintf("output/results_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)
