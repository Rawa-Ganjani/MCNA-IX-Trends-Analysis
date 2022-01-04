source("functions/2019/recoding_function.R")

df <- read.csv("input/2019/dataset/MCNA-VII_Dataset.csv",
                     stringsAsFactors = F, check.names = F)
loop <- read.csv("input/2019/dataset/loop_merged.csv", stringsAsFactors = F)

samplingframe_strata <- read.csv("input/2019/sampling_frame/sampling_frame.csv")

df$all <- "all"
df$governorate <- df$governorate_mcna

weight_fun <-
  map_to_weighting(
    sampling.frame = samplingframe_strata,
    sampling.frame.population.column = "population",
    sampling.frame.stratum.column = "stratum",
    data.stratum.column = "strata",
    data = df
  )

df <- recoding_2019(df, loop)

df$weight <- weight_fun(df)

if(disaggregate == "all") {
  df <- df[df$population_group != "host",]
  loop <- loop[loop$X_submission__uuid %in% df$X_uuid, ]
}

weight_fun <- function(df){
  df$weight
}

dap <- read.csv(sprintf("input/2019/dap/%s.csv", dap_name))

df$all <- "all"
dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate

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

saveRDS(results, paste(sprintf("output/2019/RDS/result_2019_%s_%s_%s.RDS", aggregate, disaggregate, today())))

write.csv(summary,
          sprintf("output/2019/raw_results/raw_results_2019_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary <- left_join(summary, dap, by=c("dependent.var"="dependent.variable"))

summary <-
  summary[summary$dependent.var.value != "0", ] %>% mutate(
    sub.research.question = case_when(
      dependent.var.value != "1" ~ as.character(dependent.var.value),
      T ~ sub.research.question
    )
  )

if(!disaggregate %in% c("all", "population_group") &
   length(unique(df[[disaggregate]])) == 2) {
  for (indicator in unique(summary$dependent.var)) {
    if (length(unique(na.omit(df[[indicator]]))) > 1) {
      df$indi <- df[[indicator]]
      df$disaggr <- df[[disaggregate]]
      chisquare <- chisq.test(x = df$indi, y = df$disaggr)
      summary$p_value[summary$dependent.var == indicator] <-
        chisquare$p.value
    }
  }
  summary <-
    summary %>% dplyr::select(
      "Sector" = "ï..Sector",
      "Indicator" = "research.question",
      "Sub Indicator" =  "sub.research.question",
      "Aggregation" = "repeat.var.value",
      "Disaggregation" = "independent.var.value",
      "Percentages" = "numbers",
      "variable_name" = "dependent.var",
      "p_value"
    )
}else{
  summary <-
    summary %>% dplyr::select(
      "Sector" = "ï..Sector",
      "Indicator" = "research.question",
      "Sub Indicator" =  "sub.research.question",
      "Aggregation" = "repeat.var.value",
      "Disaggregation" = "independent.var.value",
      "Percentages" = "numbers",
      "variable_name" = "dependent.var")
}

if(aggregate == "all"){
  summary <- summary %>% dplyr::select(-Aggregation)
}

if(disaggregate == "all"){
  summary <- summary %>% dplyr::select(-Disaggregation)
}

summary <- summary[!is.na(summary$Percentages), ]

write.csv(summary,
          sprintf("output/2019/formatted_results/formatted_results_2019_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary$year <- "2019"
findings_2019 <- summary


