source("functions/2018/recoding_function.R")

df <-
  read.csv(
    "input/2018/dataset/2018_household level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

loop <-
  read.csv(
    "input/2018/dataset/2018_individual level_dataset.csv",
    stringsAsFactors = F,
    na.strings = c("", "na", "NA", "NaN", "#N/A")
  )

if(disaggregate != "population_group") {
  df <- df[df$population_group != "host",]
  loop <- loop[loop$X_submission__uuid %in% df$X_uuid, ]
}

dap <-
  read.csv(
    sprintf("input/2018/dap/%s.csv", dap_name),
    stringsAsFactors = F
  )

dap$repeat.for.variable <- aggregate

dap$independent.variable <- disaggregate

sampling_frame <-
  read.csv(
    "input/2018/sampling_frame/sampling_frame.csv",
    stringsAsFactors = F
  )

df <- recoding_2018(df, loop)

df$all <- "all"
df$gender_head <- case_when(df$female_headed == 1 ~ "female", T ~ "male")

df$weight <- sampling_frame$weights[match(df$strata, sampling_frame$strata)]

weight.function <- function(df){
  df$weight
}


usecores <- detectCores() - 2
cl <- makeCluster(usecores)
registerDoParallel(cl)

results <- foreach(i = 1:nrow(dap)) %dopar% {
  library(dplyr)
  library(hypegrammaR)
  
  result <- from_analysisplan_map_to_output(
    data = df,
    analysisplan = dap[i,],
    weighting = weight.function,
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

saveRDS(results, paste(sprintf("output/2018/RDS/result_2018_%s_%s_%s.RDS", aggregate, disaggregate, today())))

write.csv(summary,
          sprintf("output/2018/raw_results/raw_results_2018_%s_%s_%s.csv", aggregate, disaggregate, today()),
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
          sprintf("output/2018/formatted_results/formatted_results_2018_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary$year <- "2018"
findings_2018 <- summary

