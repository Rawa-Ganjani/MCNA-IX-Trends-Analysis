source("functions/2021/recoding_script.R")

df <-
  read_excel(
    "input/2021/dataset/MCNA_IX_2021_cleaned_dataset_RM_2708.xlsx",
    sheet = 5,
    na = c("NA", "", NA),
    guess_max = 50000
  )

loop <-
  read_excel(
    "input/2021/dataset/MCNA_IX_2021_cleaned_dataset_RM_2708.xlsx",
    sheet = 6,
    na = c("NA", "", NA),
    guess_max = 50000
  )

if(disaggregate != "population_group") {
  df <- df[df$population_group != "host",]
  loop <- loop[loop$X_submission__uuid %in% df$X_uuid, ]
}

dap <- read.csv(sprintf("input/2021/dap/%s.csv", dap_name))

df$all <- "all"
dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate

df$weight <- as.numeric(df$weight)

#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun <- function(df) {
  df$weight
}

df <- recoding_2021(df, loop)

df$gender_head <- case_when(df$female_headed > 0 ~ "female", T ~ "male")
df$district <- df$district_mcna
df$governorate <- df$governorate_mcna

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

saveRDS(results, paste(sprintf("output/2021/RDS/result_2021_%s_%s_%s.RDS", aggregate, disaggregate, today())))

write.csv(summary,
          sprintf("output/2021/raw_results/raw_results_2021_%s_%s_%s.csv", aggregate, disaggregate, today()),
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
          sprintf("output/2021/formatted_results/formatted_results_2021_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary$year <- "2021"
findings_2021 <- summary













