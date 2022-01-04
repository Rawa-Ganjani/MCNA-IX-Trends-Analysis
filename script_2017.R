source("functions/2017/recoding_script.R")

df <-
  read_csv(
    "input/2017/Dataset_MCNA_IV.csv",
    na = c("", "NA", NA)
  )

convert <- function(x){
  if("TRUE" %in% unique(x) | "FALSE" %in% unique(x)){
  x = case_when(x == "TRUE" ~ 1, x == "FALSE" ~ 0, T ~ as.numeric(x))
  }
  return(x)
}

df <- df %>% mutate_all(.funs = convert)

df <- recoding_2017(df)

dap <-
  read.csv("input/2017/dap/dap_trends_analysis.csv"
  )

sampling_frame <- read_csv("input/2017/sampling_frame.csv")

sampling_frame$district <- gsub("[ ]|[-]|[']", "", tolower(sampling_frame$district))

df$strata <- gsub("[ ]|[-]|[']", "", tolower(df$District))

sampling_frame <- sampling_frame[sampling_frame$district %in% df$strata,]

dap$repeat.for.variable <- aggregate
dap$independent.variable <- disaggregate

df$all <- "all"
df$population_group <- "idp_out_camp"
df$district <- df$District
df$governorate <- df$Governorate

df <- df[,c(dap$dependent.variable, "strata", "all", "district", "population_group", "governorate")]

weight.function <- map_to_weighting(
  sampling.frame = sampling_frame,
  data.stratum.column = "strata",
  sampling.frame.population.column = "pop",
  sampling.frame.stratum.column = "district",
  data = df
)

df$weight <- weight.function(df)

weight.function <- function(df){
  df$weight
}

df$X_uuid <- c(1:nrow(df))

df$gender_head <- case_when(df$female_headed == 1 ~ "female", T ~ "male")

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

saveRDS(results, paste(sprintf("output/2017/RDS/result_2017_%s_%s_%s.RDS", aggregate, disaggregate, today())))

write.csv(summary,
          sprintf("output/2017/raw_results/raw_results_2017_%s_%s_%s.csv", aggregate, disaggregate, today()),
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
          sprintf("output/2017/formatted_results/formatted_results_2017_%s_%s_%s.csv", aggregate, disaggregate, today()),
          row.names = F)

summary$year <- "2017"
findings_2017 <- summary


