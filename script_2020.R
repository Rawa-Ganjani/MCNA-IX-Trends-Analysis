source("functions/2020/recoding_function.R")

df <- read.csv("input/2020/datasets/cleaned/MCNA-VIII_Dataset.csv", 
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop <- read.csv("input/2020/datasets/cleaned/loop.csv", 
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))

names(loop)[names(loop) == "ï..X_uuid"] <- "X_submission__uuid"

df$district <- df$district_mcna
df$governorate <- df$governorate_mcna
df$all <- "all"

samplingframe_strata <- read.csv("input/2020/sampling_frame/sampling_frame.csv") %>% filter(!is.na(population))

#STRATA WEIGHTING
weight_fun <-
  map_to_weighting(
    sampling.frame = samplingframe_strata,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = df
  )

df <- recoding_2020(df, loop)

df$weight <- weight_fun(df)

weight_fun <- function(df){
  df$weight
}

dap <- read.csv(sprintf("input/2020/dap/%s.csv", dap_name))

dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate

df$all <- "all"
df$gender_head <- case_when(df$female_headed > 0 ~ "female", T ~ "male")

usecores <- detectCores() - 2
cl <- makeCluster(usecores)
registerDoParallel(cl)

results <- foreach(i = 1:nrow(dap)) %dopar% {
  library(dplyr)
  library(hypegrammaR)
  
  result <- from_analysisplan_map_to_output(
    data = df,
    analysisplan = dap[i,],
    weighting = weight_fun,
    confidence_level = 0.9
  )
  
  bind_rows(lapply(result[[1]], function(x) {
    x$summary.statistic
  }))
}
stopCluster(cl)

summary <- bind_rows(lapply(results, function(x) {
  x
}))

summary <- correct.zeroes(summary)

saveRDS(results, paste(sprintf("output/2020/RDS/result_2020_%s_%s_%s.RDS", aggregate, disaggregate, today())))

write.csv(summary,
          sprintf("output/2020/raw_results/raw_results_2020_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary <- left_join(summary, dap, by=c("dependent.var"="dependent.variable"))

summary <-
  summary[summary$dependent.var.value != "0", ] %>% mutate(
    sub.research.question = case_when(
      dependent.var.value != "1" ~ as.character(dependent.var.value),
      T ~ sub.research.question
    )
  )

summary <-
  summary %>% dplyr::select(
    "Sector" = "ï..Sector",
    "Indicator" = "research.question",
    "Sub Indicator" =  "sub.research.question",
    "Aggregation" = "repeat.var.value",
    "Disaggregation" = "independent.var.value",
    "Percentages" = "numbers",
    "variable_name" = "dependent.var"
  )


if(aggregate == "all"){
  summary <- summary %>% dplyr::select(-Aggregation)
}

if(disaggregate == "all"){
  summary <- summary %>% dplyr::select(-Disaggregation)
}

summary <- summary[!is.na(summary$Percentages), ]

write.csv(summary,
          sprintf("output/2020/formatted_results/formatted_results_2020_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)


summary$year <- "2020"
findings_2020 <- summary





