recoding_2017 <- function(df) {
  names(df) <- gsub("[/]", ".", names(df))
  df$received_aid <-
    case_when(
      df$assistance_group.assistance_received_since_crisis.cash == 1 |
        df$assistance_group.assistance_received_since_crisis.food == 1 |
        df$assistance_group.assistance_received_since_crisis.fuel == 1 |
        df$assistance_group.assistance_received_since_crisis.other_NFI == 1 |
        df$assistance_group.assistance_received_since_crisis.water == 1 |
        df$assistance_group.assistance_received_since_crisis.seasonal_items == 1 |
        df$assistance_group.assistance_received_since_crisis.shelter == 1 ~ 1,
      is.na(
        df$assistance_group.assistance_received_since_crisis.cash
      ) ~ NA_real_,
      T ~ 0
    )
  
  df <- df %>% mutate(
    received_aid_cash = case_when(
      assistance_group.assistance_received_since_crisis.cash == 1 ~ 1,
      is.na(assistance_group.assistance_received_since_crisis.cash) ~ NA_real_,
      T ~ 0
    ),
    received_aid_food = case_when(
      assistance_group.assistance_received_since_crisis.food == 1 ~ 1,
      is.na(assistance_group.assistance_received_since_crisis.food) ~ NA_real_,
      T ~ 0
    ),
    received_aid_water = case_when(
      assistance_group.assistance_received_since_crisis.water == 1 ~ 1,
      is.na(assistance_group.assistance_received_since_crisis.water) ~ NA_real_,
      T ~ 0
    ),
    received_aid_fuel = case_when(
      assistance_group.assistance_received_since_crisis.fuel == 1 ~ 1,
      is.na(assistance_group.assistance_received_since_crisis.fuel) ~ NA_real_,
      T ~ 0
    ),
    received_aid_shelter = case_when(
      assistance_group.assistance_received_since_crisis.shelter == 1 ~ 1,
      is.na(
        assistance_group.assistance_received_since_crisis.shelter
      ) ~ NA_real_,
      T ~ 0
    ),
    received_aid_seasonal_items = case_when(
      assistance_group.assistance_received_since_crisis.seasonal_items == 1 ~ 1,
      is.na(
        assistance_group.assistance_received_since_crisis.seasonal_items
      ) ~ NA_real_,
      T ~ 0
    ),
    received_aid_other_nfi = case_when(
      assistance_group.assistance_received_since_crisis.other_NFI == 1 ~ 1,
      is.na(
        assistance_group.assistance_received_since_crisis.other_NFI
      ) ~ NA_real_,
      T ~ 0
    ),
    received_aid_none = case_when(
      assistance_group.assistance_received_since_crisis.none == 1 ~ 1,
      is.na(assistance_group.assistance_received_since_crisis.none) ~ NA_real_,
      T ~ 0
    )
  )
  
  
  df$critical_shelter <-
    case_when(
      df$shelter.shelter_type %in% c(
        "Abandoned_building",
        "Container",
        "damaged_building",
        "Open_Air",
        "public_building",
        "religious_building",
        "School",
        "Tent",
        "Unfinished_building"
      ) ~ 1,
      T ~ 0
    )
  
  df$attending_informal <-
    sum_row(df[, c(
      "education.nonformal_education.nonformal_edu_females_6_11",
      "education.nonformal_education.nonformal_edu_males_6_11",
      "education.nonformal_education.nonformal_edu_females_12_14",
      "education.nonformal_education.nonformal_edu_males_12_14",
      "education.nonformal_education.nonformal_edu_females_15_17",
      "education.nonformal_education.nonformal_edu_males_15_17"
    )], na.rm = T)
  
  df$not_attending_any <-
    sum_row(df[, c(
      "education.never_attended.never_school_females_6_11",
      "education.never_attended.never_school_males_6_11",
      "education.never_attended.never_school_females_12_14",
      "education.never_attended.never_school_males_12_14",
      "education.never_attended.never_school_females_15_17",
      "education.never_attended.never_school_males_15_17"
    )], na.rm = T)
  
  df$children <-
    sum_row(df[, c(
      "males_6_11",
      "females_6_11",
      "males_12_14",
      "females_12_14",
      "males_15_17",
      "females_15_17"
    )], na.rm = T)
  
  df$female_attending_informal <-
    sum_row(df[, c(
      "education.nonformal_education.nonformal_edu_females_6_11",
      "education.nonformal_education.nonformal_edu_females_12_14",
      "education.nonformal_education.nonformal_edu_females_15_17"
    )], na.rm = T)
  
  df$female_not_attending_any <-
    sum_row(df[, c(
      "education.never_attended.never_school_females_6_11",
      "education.never_attended.never_school_females_12_14",
      "education.never_attended.never_school_females_15_17"
    )], na.rm = T)
  
  
  df$female_children <-
    sum_row(df[, c(
      "females_6_11",
      "females_12_14",
      "females_15_17"
    )], na.rm = T)
  
  df$male_attending_informal <-
    sum_row(df[, c(
      "education.nonformal_education.nonformal_edu_males_6_11",
      "education.nonformal_education.nonformal_edu_males_12_14",
      "education.nonformal_education.nonformal_edu_males_15_17"
    )], na.rm = T)
  
  df$male_not_attending_any <-
    sum_row(df[, c(
      "education.never_attended.never_school_males_6_11",
      "education.never_attended.never_school_males_12_14",
      "education.never_attended.never_school_males_15_17"
    )], na.rm = T)
  
  
  df$male_children <-
    sum_row(df[, c(
      "males_6_11",
      "males_12_14",
      "males_15_17"
    )], na.rm = T)
  
  df$not_attending_formal <-
    case_when(sum_row(df[, c("attending_informal", "not_attending_any")]) > 0 ~ 1,
              df$children == 0 ~ NA_real_,
              T ~ 0)
  
  df$female_not_attending_formal <-
    case_when(sum_row(df[, c("female_attending_informal", "female_not_attending_any")]) > 0 ~ 1,
              df$female_children == 0 ~ NA_real_,
              T ~ 0)
  df$male_not_attending_formal <-
    case_when(sum_row(df[, c("male_attending_informal", "male_not_attending_any")]) > 0 ~ 1,
              df$male_children == 0 ~ NA_real_,
              T ~ 0)
  
  df$dropped_from_school <-
    case_when(df$children == 0 ~ NA_real_,
              sum_row(df[, c(
                "education.dropped_out.dropped_out_females_6_11",
                "education.dropped_out.dropped_out_males_6_11",
                "education.dropped_out.dropped_out_females_12_14",
                "education.dropped_out.dropped_out_males_12_14",
                "education.dropped_out.dropped_out_females_15_17",
                "education.dropped_out.dropped_out_males_15_17"
              )], na.rm = T) > 0 ~ 1,
              T ~ 0)
  
  df <- df %>% mutate(
    education_barriers_safety = case_when(
      education.barriers_ed.school_barriers.security == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_cost = case_when(
      education.barriers_ed.school_barriers.cost == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_unable_to_register = case_when(
      education.barriers_ed.school_barriers.space == 1 |
        education.barriers_ed.school_barriers.missedtoomuch == 1 |
        education.barriers_ed.school_barriers.too_young == 1 |
        education.barriers_ed.school_barriers.moving == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_health_issue = case_when(
      education.barriers_ed.school_barriers.traumatized == 1 |
        education.barriers_ed.school_barriers.disabled == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    
    education_barriers_physical_limitation = case_when(
      education.barriers_ed.school_barriers.school_too_far == 1 |
        education.barriers_ed.school_barriers.no_transport == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_poor_infrastructure = case_when(
      education.barriers_ed.school_barriers.badcondition == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_working = case_when(
      education.barriers_ed.school_barriers.working == 1 |
        education.barriers_ed.school_barriers.marriage == 1 |
        education.barriers_ed.school_barriers.chores == 1 |
        education.barriers_ed.school_barriers.begging == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_parental_refusal = case_when(
      education.barriers_ed.school_barriers.customs == 1 |
        education.barriers_ed.school_barriers.unecessary == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    ),
    education_barriers_other = case_when(
      education.barriers_ed.school_barriers.newarrival == 1 ~ 1,
      is.na(education.barriers_ed.school_barriers) ~ NA_real_,
      T ~ 0
    )
  )
  
  df$fcs <-
    as.numeric(df$food_security.fcs.cereals) * 2 + as.numeric(df$food_security.fcs.nuts_seed) * 3 + as.numeric(df$food_security.fcs.milk_dairy) * 4 + as.numeric(df$food_security.fcs.meat) * 4 +
    as.numeric(df$food_security.fcs.vegetables) + as.numeric(df$food_security.fcs.fruits) + as.numeric(df$food_security.fcs.oil_fats) * 0.5 + as.numeric(df$food_security.fcs.sweets) * 0.5
  
  df$fcs_category <- case_when(df$fcs <= 28.5 ~ "Poor",
                               df$fcs < 42.5 ~ "Borderline",
                               T ~ "Acceptable")
  
  df$food_expenditure_share <-
    df$livelihoods.expenses.food_exp_basic_needs / sum_row(df[, startsWith(names(df), "livelihoods.expenses.")], na.rm = T)
  
  df$food_expenditure_share <-
    case_when(df$food_expenditure_share > 0.5 ~ 1, T ~ 0)
  
  df$difficulty_access_healthcare <-
    case_when(
      df$health.access_difficulty == "yes" ~ 1,
      is.na(df$health.access_difficulty) ~ NA_real_,
      T ~ 0
    )
  
  df$difficulty_access_healthcare_cost <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.healthcare_cost == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_lack_qualified_staff <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.unqualified_staff_hosp == 1 |
        df$health.difficulty_type.unqualified_staff_phc == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_lack_civil_doc <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.civ_docs_problems == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_no_referral_phc <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.no_referral_phc == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_phc_closed <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.phc_closed == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_too_far <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.distance_to_treatmentcenter == 1 |
        df$health.difficulty_type.no_transport  == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_staff_refused_treatment <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.refused_treatment == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_lack_medicine <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.no_medicine_hosp == 1 |
        df$health.difficulty_type.no_medicine_phc == 1 |
        df$health.difficulty_type.no_medicine_pharm == 1 ~ 1,
      T ~ 0
    )
  df$difficulty_access_healthcare_no_treatment_phc <-
    case_when(
      is.na(df$health.difficulty_type) ~ NA_real_,
      df$health.difficulty_type.no_offered_treatment_phc == 1 |
        df$health.difficulty_type.no_offered_treatment_hosp == 1 ~ 1,
      T ~ 0
    )
  
  df$female_headed <- case_when(df$HoH.hhh_sex == "female" ~ 1, T ~ 0)
  
  df$no_movement_3 <-
    case_when(df$Intentions_group.intention_to_move == "no" ~ 1, T ~ 0)
  
  df$unemployed_seeking_work <-
    case_when(
      df$livelihoods.unemployed_seeking_work == "yes" ~ 1,
      is.na(df$livelihoods.unemployed_seeking_work) ~ NA_real_,
      T ~ 0
    )
  
  df$n_children_working <-
    sum_row(df[, c(
      "livelihoods.G_hh_employment.work_females_12_14",
      "livelihoods.G_hh_employment.work_females_15_17",
      "livelihoods.G_hh_employment.work_males_12_14",
      "livelihoods.G_hh_employment.work_males_15_17",
      "livelihoods.G_hh_employment.work_females_6_11",
      "livelihoods.G_hh_employment.work_males_6_11"
    )], na.rm = T)
  
  df$children_working <- case_when(df$children == 0 ~ NA_real_,
                                  df$n_children_working > 0 ~ 1,
                                  T ~ 0)
  
  df$employment_barrier_competition <-
    case_when(
      df$livelihoods.problem_type.increased_competition == 1 ~ 1,
      is.na(df$livelihoods.problem_type) ~ NA_real_,
      T ~ 0
    )
  
  df$employment_barrier_too_far <-
    case_when(
      df$livelihoods.problem_type.distance_job == 1 ~ 1,
      is.na(df$livelihoods.problem_type) ~ NA_real_,
      T ~ 0
    )
  
  df$employment_barrier_only_low_available <-
    case_when(
      df$livelihoods.problem_type.inadequate_jobs == 1 ~ 1,
      is.na(df$livelihoods.problem_type) ~ NA_real_,
      T ~ 0
    )
  
  df$employment_barrier_no_wasta <-
    case_when(
      df$livelihoods.problem_type.family_connections == 1 ~ 1,
      is.na(df$livelihoods.problem_type) ~ NA_real_,
      T ~ 0
    )
  
  df$insufficient_income <-
    case_when(df$livelihoods.total_income_all < 480000 ~ 1, T ~ 0)
  
  
  df$market_barrier <-
    case_when(df$food_security.diffic_market_access == "yes" ~ 1, T ~ 0)
  
  df$too_much_debt <-
    case_when(df$livelihoods.how_much_debt > 505000 ~ 1, T ~ 0)
  
  df$basic_services <- case_when(df$livelihoods.basic_needs == "no" ~ 1, T ~ 0)
  
  df$health_expenditure_share <-
    df$livelihoods.expenses.medical_exp_basic_needs / sum_row(df[, startsWith(names(df), "livelihoods.expenses.")], na.rm = T)
  
  df$health_expenses <-
    case_when(df$health_expenditure_share > 0.25 ~ 1, T ~ 0)
  
  df$risk_eviction <-
    case_when(
      df$shelter.eviction_threat == "yes" ~ 1,
      is.na(df$shelter.eviction_threat) ~ NA_real_,
      T ~ 0
    )
  
  df$movement_restriction <-
    case_when(
      df$protection.freedom_restriction == "yes" ~ 1,
      is.na(df$protection.freedom_restriction) ~ NA_real_,
      T ~ 0
    )
  
  df$hh_missing_doc <-
    case_when(
      df$protection.civil_documents_missing.id_card == 1 |
        df$protection.civil_documents_missing.food_ration_card == 1 |
        df$protection.civil_documents_missing.birth_certificate == 1 |
        df$protection.civil_documents_missing.citizenship_certificate == 1 ~ 1,
      T ~ 0
    )
  
  
  df$water_source <- case_when(
    df$wash.water_source %in% c("dug_well", "other", "river_spring", "water_trucking") ~ "unimproved",
    T ~ "improved"
  )
  
  df$imptoved_water_source <- case_when(
    df$water_source == "unimproved" ~ 1,
    T ~ 0
  )
  
  df$improved_sanitation <- case_when(
    df$wash.latrines.private == 1 ~ 1,
    TRUE ~ 0
  )
  
  df$treat_water <-
    case_when(
      df$wash.treat_drink_water_how.boiling == 1 |
        df$wash.treat_drink_water_how.chlorination == 1 |
        df$wash.treat_drink_water_how.filter == 1 ~ 1,
      T ~ 0
    )
  
  return(df)
}