recoding_2019 = function(df, loop) {
  df$received_aid      <- ifelse(df$aid_received == "yes", 1, 0)
  
  df$received_aid_cash    <- df$aid_type.cash
  df$received_aid_food   <- df$aid_type.food
  df$received_aid_water  <- df$aid_type.water
  df$received_aid_fuel   <- df$aid_type.fuel
  df$received_aid_shelter    <- df$aid_type.shelter
  df$received_aid_seasonal_items   <- df$aid_type.seasonal_items
  df$received_aid_other_nfi <- df$aid_type.other_nfi
  df$received_aid_protection    <- df$aid_type.protection
  df$received_aid_education   <- df$aid_type.education
  df$received_aid_healthcare   <- df$aid_type.healthcare
  
  df$aid_satisfaction <-
    case_when(df$aid_satisfaction == "yes" ~ 1,
              is.na(df$aid_satisfaction) ~ NA_real_ ,
              TRUE ~ 0)
  
  df$critical_shelter <- ifelse(
    df$shelter_type %in%
      c(
        "unfinished_abandoned_building",
        "damaged_building",
        "tent",
        "religious_building",
        "public_building",
        "non_residential",
        "container",
        "makeshift_shelter",
        "rhu"
      ),
    1,
    0
  )
  
  df$shelter.shelter_type <- df$shelter_type
  
  loop <-
    loop %>% mutate(
      age_class = case_when(age < 18 ~ "Children",
                            age < 60 ~ "Adult",
                            age >= 60 ~ "Elderly"),
      female_headed = case_when(relationship == "head" &
                                  sex == "female" ~ 1,
                                TRUE ~ 0),
      not_attending_school = case_when(attend_formal_ed == "no" ~ 1,
                                       TRUE ~ 0),
      female_not_attending_school = case_when(attend_formal_ed == "no" & sex == "female" ~ 1,
                                       TRUE ~ 0),
      male_not_attending_school = case_when(attend_formal_ed == "no" & sex == "male" ~ 1,
                                       TRUE ~ 0),
      child_married = case_when(
        marital_status %in% c("divorced", "married", "separated", "widowed") &
          age < 18 ~ 1,
        TRUE ~ 0
      ),
      child_working = case_when(age < 18 &
                                  work == "yes" ~ 1, TRUE ~ 0),
      school_aged_child = case_when(age >= 6 &
                                      age < 18 ~ 1, TRUE ~ 0),
      single_headed = case_when(relationship == "head" &
                                  marital_status != "married" ~ 1,
                                TRUE ~ 0),
      disabled = case_when(
        age < 5 ~ NA_real_,
        difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
          difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
          difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
          difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
          difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all") |
          difficulty_washing %in% c("a_lot_of_difficulty", "cannot_do_at_all") ~ 1,
        TRUE ~ 0
      ),
      unemployed_seek_work = case_when(
        loop$age < 18 ~ NA_real_,
        loop$work == "no" &
          loop$actively_seek_work %in% c("yes", "no_opportunity") ~ 1,
        TRUE ~ 0
      ),
      female_unemployed_seek_work = case_when(
        loop$age < 18 ~ NA_real_,
        loop$work == "no" &
          loop$actively_seek_work %in% c("yes", "no_opportunity") &
          loop$sex == "female" ~ 1,
        TRUE ~ 0
      ),
      female_adult = case_when(sex == "female" & age > 17 ~ 1, T ~ 0),
      female_school_age = case_when(sex == "female" & age %in% c(6:17) ~ 1, T ~ 0),
      male_school_age = case_when(sex == "male" & age %in% c(6:17) ~ 1, T ~ 0),
      dropped = case_when(as.Date(drop_out, format = "%Y-%m-%d") > as.Date("2017-09-01") ~ 1, T ~ 0)
    )
  
  
  sum_loop <- loop %>% group_by(X_submission__uuid) %>%
    summarize(
      children = sum(age_class == "Children", na.rm = T),
      female_headed = sum(female_headed, na.rm = T),
      not_attending_school = sum(not_attending_school, na.rm = T),
      school_aged_child = sum(school_aged_child, na.rm = T),
      child_married = sum(child_married, na.rm = T),
      child_working = sum(child_working, na.rm = T),
      single_headed = sum(single_headed, na.rm = T),
      members_disabled = sum(disabled, na.rm = T),
      difficulty_accessing_services = sum(difficulty_accessing_services == "yes", na.rm = T),
      unemployed_seek_work = sum(unemployed_seek_work, na.rm = T),
      female_unemployed_seeking_work = sum(female_unemployed_seek_work, na.rm = T),
      female_not_attending_school = sum(female_not_attending_school, na.rm = T),
      male_not_attending_school = sum(male_not_attending_school, na.rm = T),
      female_school_age = sum(female_school_age, na.rm = T),
      male_school_age = sum(male_school_age, na.rm = T),
      female_adult = sum(female_adult, na.rm = T),
      dropped = sum(dropped, na.rm = T)
    )
  
  df <- left_join(df, sum_loop, by = c("X_uuid" = "X_submission__uuid"))
  
  df$female_headed <- case_when(df$female_headed > 0 ~ 1, T ~ 0)
  df$single_headed <- case_when(df$single_headed > 0 ~ 1, T ~ 0 )
  
  df$not_attending_formal <-
    case_when(df$school_aged_child == 0 ~ NA_real_,
              df$not_attending_school > 0 ~ 1,
              T ~ 0)
  
  df$female_not_attending_formal <-
    case_when(df$female_school_age == 0 ~ NA_real_,
              df$female_not_attending_school > 0 ~ 1,
              T ~ 0)
  
  df$male_not_attending_formal <-
    case_when(df$male_school_age == 0 ~ NA_real_,
              df$male_not_attending_school > 0 ~ 1,
              T ~ 0)
  
  df$dropped_from_school <-
    case_when(
      df$school_aged_child == 0 ~ NA_real_,
      df$dropped > 0 ~ 1,
      T ~ 0
    )
  
  df$education_barriers_not_function <-
    df$reasons_not_attend.school_closed
  df$education_barriers_safety <- df$reasons_not_attend.not_safe
  df$education_barriers_cost <- df$reasons_not_attend.cannot_afford
  df$education_barriers_unable_to_register <-
    df$reasons_not_attend.impossible_to_enrol
  df$education_barriers_physical_limitation <-
    df$reasons_not_attend.cannot_go_physically
  df$education_barriers_overcrowded <- case_when(df$reasons_not_attend.overcrowded == 1 |
                                                   df$reasons_not_attend.lack_of_staff == 1 ~ 1,
                                                 is.na(df$reasons_not_attend) ~ NA_real_,
                                                 T ~ 0)
  df$education_barriers_poor_infrastructure <-
    df$reasons_not_attend.poor_infrastructure
  df$education_barriers_curriculum_not_adapted <-
    df$reasons_not_attend.curriculum
  df$education_barriers_parental_refusal <-
    df$reasons_not_attend.parental_refusal
  df$education_barriers_working <-
    df$reasons_not_attend.children_working
  df$education_barriers_lack_interest <-
    df$reasons_not_attend.uninterested
  df$education_barriers_other <- df$reasons_not_attend.other
  
  df$fcs <-
    (as.numeric(df$cereals) * 2) + (as.numeric(df$nuts_seed) * 3) + (as.numeric(df$milk_dairy) * 4) + (as.numeric(df$meat) * 4) +
    as.numeric(df$vegetables) + as.numeric(df$fruits) + (as.numeric(df$oil_fats) * 0.5) + (as.numeric(df$sweets) * 0.5)
  
  df$fcs_category <- case_when(df$fcs <= 28.5 ~ "Poor",
                               df$fcs < 42.5 ~ "Borderline",
                               T ~ "Acceptable")
  
  df$stress <-
    ifelse(
      df$selling_assets %in% c("no_already_did", "yes") |
        df$borrow_debt  %in% c("no_already_did", "yes") |
        df$reduce_spending %in% c("no_already_did", "yes") |
        df$spending_savings %in% c("no_already_did", "yes")   ,
      1,
      0
    )
  df$crisis <-
    ifelse(
      df$selling_transportation_means %in% c("no_already_did", "yes") |
        df$change_place  %in% c("no_already_did", "yes") |
        df$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$emergency <-
    ifelse(
      df$child_dropout_school %in% c("no_already_did", "yes") |
        df$adult_risky  %in% c("no_already_did", "yes") |
        df$family_migrating %in% c("no_already_did", "yes") |
        df$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  df$food_expenditure_share <-
    case_when(df$food_exp / df$tot_expenditure > 0.5 ~ 1, T ~ 0)
  
  df$hospital_access_services <- ifelse(
    df$distance_hospital %in% c("between_2km_5km", "within_2km") &
      df$hospital_emergency_ser == "yes" &
      df$hospital_maternity_ser == "yes" &
      df$hospital_surgical_ser == "yes" &
      df$hospital_pediatric_ser == "yes",
    1,
    0
  )
  
  df$clinic_access <-
    ifelse(df$distance_health_service %in% c("between_2km_5km", "within_2km"),
           1,
           0)
  
  df$difficulty_access_healthcare <-
    case_when(
      df$health_accessed == "yes" &
        (df$health_barriers.civ_docs_problems == 1 |
        df$health_barriers.cost == 1 |
        df$health_barriers.distance_to_treatmentcenter == 1 |
        df$health_barriers.no_medicine == 1 |
        df$health_barriers.no_offered_treatment == 1 |
        df$health_barriers.no_referral_phc == 1 |
        df$health_barriers.not_inclusive == 1 |
        df$health_barriers.phc_closed == 1 |
        df$health_barriers.refused_treatment == 1 |
        df$health_barriers.unqualified_staff == 1) ~ 1,
      df$health_accessed == "yes" ~ 0,
      T ~ NA_real_
    )
  
  df$difficulty_access_healthcare_cost    <- df$health_barriers.cost
  df$difficulty_access_healthcare_lack_qualified_staff   <-
    df$health_barriers.unqualified_staff
  df$difficulty_access_healthcare_lack_civil_doc  <-
    df$health_barriers.civ_docs_problems
  df$difficulty_access_healthcare_no_referral_phc   <-
    df$health_barriers.no_referral_phc
  df$difficulty_access_healthcare_phc_closed    <-
    df$health_barriers.phc_closed
  df$difficulty_access_healthcare_too_far   <-
    df$health_barriers.distance_to_treatmentcenter
  df$difficulty_access_healthcare_staff_refused_treatment  <-
    df$health_barriers.refused_treatment
  df$difficulty_access_healthcare_lack_medicine <-
    df$health_barriers.no_medicine
  df$difficulty_access_healthcare_no_treatment_phc   <-
    df$health_barriers.no_offered_treatment
  df$difficulty_access_healthcare_not_inclusive_disability    <-
    df$health_barriers.not_inclusive
  
  df$hh_disabled <- case_when(df$members_disabled > 0 ~ 1, TRUE ~ 0)
  
  df$members_disabled <- case_when(df$members_disabled > 0 ~ 1, TRUE ~ 0)
  
  df$difficulty_disability_services <-
    case_when(df$difficulty_accessing_services > 0 ~ 1,
              df$hh_disabled == 0 ~ NA_real_,
              TRUE ~ 0)
  
  df$difficulty_accessing_female_services <-
    ifelse(df$women_specialised_services == "no", 1, 0)
  
  
  df$no_movement_3 <-
    case_when(
      df$movement_intentions == "remain" ~ 1,
      is.na(df$movement_intentions) ~ NA_real_,
      TRUE ~ 0
    )
  df$no_movement_12 <-
    case_when(
      df$movement_intentions12 %in% c("current", "remain") ~ 1,
      is.na(df$movement_intentions12) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$no_movement_returnee_12 <-
    case_when(
      df$movement_intentions_b12 == "remain" ~ 1,
      is.na(df$movement_intentions_b12) ~ NA_real_,
      TRUE ~ 0
    )
  
  df$return_security_aoo <- df$reason_to_return_to_aoo.security_stable
  df$return_security_aod <-
    case_when(
      df$reason_to_return_to_aoo.no_safe_in_area_of_displace == 1 |
        df$reason_to_return_to_aoo.forced_security == 1 |
        df$reason_to_return_to_aoo.lack_security_women == 1 |
        df$reason_to_return_to_aoo.facing_eviction == 1 ~ 1,
      is.na(df$reason_to_return_to_aoo) ~ NA_real_,
      T ~ 0
    )
  df$return_livelihoods_aod <-
    df$reason_to_return_to_aoo.limited_livelihood_opportunities
  df$return_secure_hlp <- df$reason_to_return_to_aoo.secure_house_land
  df$return_limited_services <-
    df$reason_to_return_to_aoo.limited_services
  df$return_available_services_aoo <-
    case_when(
      df$reason_to_return_to_aoo.basic_services == 1 |
        df$reason_to_return_to_aoo.livelihood_availability_there == 1 ~ 1,
      is.na(df$reason_to_return_to_aoo) ~ NA_real_,
      T ~ 0
    )  
  df$return_uxo <- df$reason_to_return_to_aoo.uxo
  df$return_pull_factors <- case_when(df$reason_to_return_to_aoo.emotional_desire == 1 |
                                        df$reason_to_return_to_aoo.fam_released == 1 |
                                        df$reason_to_return_to_aoo.other_members_returned == 1 |
                                        df$reason_to_return_to_aoo.no_integrated_in_area_of_displace == 1 ~ 1,
                                      is.na(df$reason_to_return_to_aoo) ~ NA_real_,
                                      T ~ 0
  )
  
  df$no_return_security_aoo  <-
    case_when(
      df$why_not_return.desicrimination == 1 |
        df$why_not_return.fear_trauma == 1 |
        df$why_not_return.lack_of_security_forces == 1 |
        df$why_not_return.lack_security_women == 1 |
        df$why_not_return.movement_restrictions == 1 ~ 1,
      is.na(df$why_not_return) ~ NA_real_,
      T ~ 0
    )
  df$no_return_lack_livelihoods <-
    case_when(df$why_not_return.lack_livelihood_generating_income == 1 |
                df$why_not_return.no_financial_money_to_return == 1 |
                df$why_not_return.no_transport_to_return == 1 ~ 1,
              is.na(df$why_not_return) ~ NA_real_,
              T ~ 0)
  df$no_return_lack_shelter <-
    case_when(
      df$why_not_return.hh_assets_stolen_damaged == 1 |
        df$why_not_return.house_damaged_destroyed == 1 |
        df$why_not_return.house_land_occupied == 1 ~ 1,
      is.na(df$why_not_return) ~ NA_real_,
      T ~ 0
    )
  df$no_return_lack_docs  <- df$why_not_return.no_personal_id
  df$no_return_lack_services <-
    case_when(
      df$why_not_return.basic_services_not_enough == 1 |
        df$why_not_return.lack_court == 1 |
        df$why_not_return.local_markets_not_working == 1 |
        df$why_not_return.lack_of_education_oppotunities == 1 ~ 1,
      is.na(df$why_not_return) ~ NA_real_,
      T ~ 0
    )
  df$no_return_uxo <- df$why_not_return.presence_of_mines
  df$no_return_pull_factors <-
    case_when(
      df$why_not_return.children_enrolled_in_displacement == 1 |
        df$why_not_return.health_conditions == 1 |
        df$why_not_return.living_conditions_better == 1 |
        df$why_not_return.immediate_family_wont_return == 1 ~ 1,
      is.na(df$why_not_return) ~ NA_real_,
      T ~ 0
    )
  
  df$unemployed_seeking_work <-
    case_when(df$unemployed_seek_work > 0 ~ 1,  TRUE ~ 0)
  
  df$female_unemployed_seeking_work <-
    case_when(df$female_unemployed_seeking_work > 0 ~ 1, 
              df$female_adult == 0 ~ NA_real_,
              TRUE ~ 0)
  
  df$children_working <- case_when(df$children == 0 ~ NA_real_,
                                   df$child_working > 0 ~ 1,
                                   T ~ 0)
  
  df$employment_barrier_competition <-
    df$employment_primary_barriers.increased_competition
  df$employment_barrier_too_far <-
    df$employment_primary_barriers.jobs_far
  df$employment_barrier_only_low_available <-
    df$employment_primary_barriers.only_low_available
  df$employment_barrier_underqualified_for_jobs <-
    df$employment_primary_barriers.underqualified_for_jobs
  df$employment_barrier_no_wasta <-
    df$employment_primary_barriers.lack_of_connections

  
  
  df$insufficient_income <-
    case_when(df$inc_employment < 480000 ~ 1, TRUE ~ 0)
  
  df$too_much_debt <-
    case_when(df$how_much_debt > 505000 ~ 1, TRUE ~ 0)
  
  df$basic_services <- case_when(
    df$reasons_for_debt %in% c("basic_hh_expenditure", "health", "food", "education") ~ 1,
    is.na(df$reasons_for_debt) ~ NA_real_,
    TRUE ~ 0
  )
  
  df$ngo_assistance <- case_when(
    df$primary_livelihood.ngo_charity_assistance == 1 ~ 1,
    is.na(df$primary_livelihood) ~ NA_real_ ,
    TRUE ~ 0
  )
  
  df$health_expenses <-
    case_when(df$medical_exp / df$tot_expenditure >= 0.25 ~ 1, TRUE ~ 0)
  
  df$risk_eviction <-
    case_when(df$hh_risk_eviction == "yes" ~ 1, TRUE ~ 0)
  
  df$movement_restriction <- case_when(
    df$restriction_clearance == "yes" |
      df$restriction_documents == "yes" |
      df$restriction_time == "yes" |
      df$restriction_reason == "yes" |
      df$restriction_physical == "yes" |
      df$restriction_other == "yes" ~ 1 ,
    TRUE ~ 0
  )
  
  df$hh_missing_doc <- case_when(
    df$pds == "no" |
      df$id_card_a18 == "no" |
      df$citizenship_a18 == "no" |
      df$id_card_u18 == "no" |
      df$citizenship_u18 == "no" |
      df$birth_cert_u18 == "no" ~ 1,
    TRUE ~ 0
  )
  
  df$children_missing_doc <- case_when(
    df$id_card_u18 == "no" |
      df$citizenship_u18 == "no" |
      df$birth_cert_u18 == "no" ~ 1,
    TRUE ~ 0
  )
  
  
  df$child_married <-
    case_when(df$child_married > 0 ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)
  
  df$child_distress <-
    case_when(
      df$hh_member_distress == "yes" &
        df$child_distress_number > 0 ~ 1,
      df$children > 0 ~ 0,
      TRUE ~ NA_real_
    )
  df$adult_distress <-
    case_when(df$hh_member_distress == "yes" &
                df$adult_distress_number > 0 ~ 1,
              TRUE ~ 0)
  
  df$lack_secure_tenure <-
    case_when(df$hh_dispute == "yes" ~ 1,
              is.na(df$hh_dispute) ~ NA_real_,
              TRUE ~ 0)
  
  df$shelter_2_improvements <- case_when(
    df$shelter_better.protec_hazards +
      df$shelter_better.improve_safety +
      df$shelter_better.improve_privacy +
      df$shelter_better.protect_climate >= 2 ~ 1,
    TRUE ~ 0
  )
  
  df$water_source <- case_when(
    df$drinking_water_source.bottled_water == 1 |
      df$drinking_water_source.prot_spring == 1 |
      df$drinking_water_source.network_comm == 1 |
      df$drinking_water_source.network_private == 1 |
      df$drinking_water_source.borehole == 1 |
      df$drinking_water_source.prot_tank == 1 |
      df$drinking_water_source.prot_well == 1 ~ "improved",
    T ~ "unimproved"
  )
  
  df$imptoved_water_source <- case_when(df$water_source == "unimproved" ~ 1,
                                        T ~ 0)
  
  df$improved_sanitation <- case_when(df$latrines.vip_pit == 1 |
                                        df$latrines.flush == 1  ~ 1,
                                      TRUE ~ 0)
  
  df$problem_water_quality <-
    case_when(
      df$treat_drink_water_why.not_clear == 1 |
        df$treat_drink_water_why.tastes_unpleasant == 1 |
        df$treat_drink_water_why.smells_unpleasant == 1 ~ 1,
      TRUE ~ 0
    )
  
  df$treat_water <-
    case_when(
      df$treat_drink_water_how != "not_necessary" ~ 1,
      is.na(df$treat_drink_water_how) ~ NA_real_,
      TRUE ~ 0
    )
  
  return(df)
}
