recoding_2018 <- function(df, loop) {
  df$received_aid <-
    ifelse(df$accountability.aid_received == "yes", 1, 0)
  
  df$received_aid_cash <- df$accountability.aid_type.cash
  df$received_aid_food <- df$accountability.aid_type.food
  df$received_aid_water <- df$accountability.aid_type.water
  df$received_aid_fuel <- df$accountability.aid_type.fuel
  df$received_aid_shelter <- df$accountability.aid_type.shelter
  df$received_aid_seasonal_items <- df$accountability.aid_type.seasonal_items
  df$received_aid_other_nfi <- df$accountability.aid_type.other_nfi
  df$received_aid_protection <- df$accountability.aid_type.protection
  
  df$aid_satisfaction <-
    case_when(df$accountability.aid_satisfaction == "yes" ~ 1,
           is.na(df$accountability.aid_satisfaction ) ~ NA_real_, T ~ 0)
  
  df$critical_shelter <-
    case_when(
      df$S_NFI.shelter_type %in% c(
        "Caravan",
        "Container",
        "damaged_building",
        "Makeshift_or_Improvised_Shelter",
        "makeshift_shelter",
        "non_residential",
        "public_building",
        "religious_building",
        "RHU",
        "Semi_permanent_structure",
        "Single_Family_Residential_Unit",
        "tent",
        "Tent",
        "Unfinished_Abandoned_building"
      ) ~ 1,
      is.na(df$S_NFI.shelter_type) ~ NA_real_,
      T ~ 0
    )
  
  df$shelter.shelter_type <- df$S_NFI.shelter_type
  
  loop$not_attending_school <- case_when(loop$Household_Roster.member.attend_formal_ed == "no" ~ 1, T ~ 0)
  loop$school_aged_children <- case_when(loop$Household_Roster.member.age %in% c(6:17) ~ 1, T ~ 0)
  loop$dropped_from_school <- case_when(!is.na(loop$Household_Roster.member.drop_out) ~ 1, T ~ 0)
  loop$health_accessed <- case_when(loop$Household_Roster.member.access_health_service == "yes" ~ 1, T ~ 0)

  loop <- loop %>% mutate(
    age_class = case_when(
      Household_Roster.member.age < 18 ~ "Children",
      Household_Roster.member.age < 60 ~ "Adult",
      Household_Roster.member.age >= 60 ~ "Elderly"
    ),
    not_attending_school = case_when(Household_Roster.member.attend_formal_ed == "no" ~ 1,
                                     TRUE ~ 0),
    female_not_attending_school = case_when(Household_Roster.member.attend_formal_ed == "no" & Household_Roster.member.sex == "female" ~ 1,
                                     TRUE ~ 0),
    male_not_attending_school = case_when(Household_Roster.member.attend_formal_ed == "no" & Household_Roster.member.sex == "male" ~ 1,
                                     TRUE ~ 0),
    child_married = case_when(
      Household_Roster.member.marital_status %in% c("divorced", "married", "separated", "widowed") &
        Household_Roster.member.age < 18 ~ 1,
      TRUE ~ 0
    ),
    child_working = case_when(
      Household_Roster.member.age < 18 &
        Household_Roster.member.work == "yes" ~ 1,
      TRUE ~ 0
    ),
    school_aged_child = case_when(
      Household_Roster.member.age >= 6 &
        Household_Roster.member.age < 18 ~ 1,
      TRUE ~ 0
    ),
    female_school_aged_child = case_when(
      Household_Roster.member.age >= 6 &
        Household_Roster.member.age < 18 &
        Household_Roster.member.sex == "female" ~ 1,
      TRUE ~ 0
    ),
    male_school_aged_child = case_when(
      Household_Roster.member.age >= 6 &
        Household_Roster.member.age < 18 &
        Household_Roster.member.sex == "male" ~ 1,
      TRUE ~ 0
    ),
    single_headed = case_when(
      Household_Roster.member.relationship == "head" &
        Household_Roster.member.marital_status != "married" ~ 1,
      TRUE ~ 0
    ),
    female_headed = case_when(
      Household_Roster.member.relationship == "head" &
        Household_Roster.member.sex == "female" ~ 1,
      TRUE ~ 0
    ),
    unemployed_seek_work = case_when(
      Household_Roster.member.age < 18 ~ NA_real_,
      Household_Roster.member.work == "no" &
        Household_Roster.member.actively_seek_work == "yes" ~ 1,
      TRUE ~ 0
    ),
    female_unemployed_seek_work = case_when(
      Household_Roster.member.age < 18 ~ NA_real_,
      Household_Roster.member.work == "no" &
        Household_Roster.member.actively_seek_work == "yes" &
        Household_Roster.member.sex == "female" ~ 1,
      TRUE ~ 0
    ),
    female_adult = case_when(Household_Roster.member.sex == "female" & Household_Roster.member.age > 17 ~ 1, T ~ 0),
    id_card_u18 = case_when(Household_Roster.member.civil_docs.id_card == 1 & Household_Roster.member.age < 18 ~ 1, T ~ 0),
    nationality_u18 = case_when(Household_Roster.member.civil_docs.citizenship_certificate == 1 & Household_Roster.member.age < 18 ~ 1, T ~ 0),
    birth_cert_u18 = case_when(Household_Roster.member.civil_docs.birth_certificate == 1 & Household_Roster.member.age < 18 ~ 1, T ~ 0)
  )
  
  
  summ <- loop %>% group_by(X_submission__uuid) %>%
    summarise(
      children = sum(age_class == "Children", na.rm = T),
      not_attending_school = sum(not_attending_school, na.rm = T),
      school_aged_child = sum(school_aged_child, na.rm = T),
      child_married = sum(child_married, na.rm = T),    
      child_working = sum(child_working, na.rm = T),
      single_headed = sum(single_headed, na.rm = T),
      female_headed = sum(female_headed, na.rm = T),
      unemployed_seek_work = sum(unemployed_seek_work, na.rm = T),
      female_unemployed_seek_work = sum(female_unemployed_seek_work, na.rm = T),
      female_adult = sum(female_adult, na.rm = T),
      school_aged_children = sum(school_aged_children, na.rm = T),
      female_school_aged_children = sum(female_school_aged_child, na.rm = T),
      male_school_aged_children = sum(male_school_aged_child, na.rm = T),
      not_attending_school = sum(not_attending_school, na.rm = T),
      female_not_attending_school = sum(female_not_attending_school, na.rm = T),
      male_not_attending_school = sum(male_not_attending_school),
      dropped_from_school = sum(dropped_from_school, na.rm = T),
      cannot_afford = sum(
        Household_Roster.member.reasons_not_attend.Cannot_afford_to_pay,
        na.rm = T
      ),
      no_space = sum(
        Household_Roster.member.reasons_not_attend.No_space_inschool,
        na.rm = T
      ),
      bad_condition = sum(
        Household_Roster.member.reasons_not_attend.school_bad_condition,
        na.rm = T
      ),
      lack_curriculum = sum(
        Household_Roster.member.reasons_not_attend.lack_suitable_curriculum,
        na.rm = T
      ),
      lack_teacher = sum(
        Household_Roster.member.reasons_not_attend.lack_trained_teachers,
        na.rm = T
      ),
      lack_gender = sum(
        Household_Roster.member.reasons_not_attend.lacked_gender_appropriate_staff,
        na.rm = T
      ),
      stayed_at_home = sum(
        Household_Roster.member.reasons_not_attend.Children_stay_home,
        na.rm = T
      ),
      participate_remunerative_activities = sum(
        Household_Roster.member.reasons_not_attend.participate_remunerative_activities,
        na.rm = T
      ),
      recent_displacement = sum(
        Household_Roster.member.reasons_not_attend.Recently_displacement,
        na.rm = T
      ),
      not_important = sum(
        Household_Roster.member.reasons_not_attend.education_not_importatnt,
        na.rm = T
      ),
      security = sum(
        Household_Roster.member.reasons_not_attend.Security_situation_Insecurity,
        na.rm = T
      ),
      disability = sum(
        Household_Roster.member.reasons_not_attend.Child_disabled,
        na.rm = T
      ),
      not_interested = sum(
        Household_Roster.member.reasons_not_attend.Child_disinterested,
        na.rm = T
      ),
      missed_too_much = sum(
        Household_Roster.member.reasons_not_attend.Missed_too_much,
        na.rm = T
      ),
      distance = sum(
        Household_Roster.member.reasons_not_attend.school_too_far,
        na.rm = T
      ),
      other_barriers = sum(Household_Roster.member.reasons_not_attend.other, na.rm = T),
      health_accessed = sum(health_accessed, na.rm = T),
      health_barriers.no_issues = sum(Household_Roster.member.healthcare_difficulties.no_issues,
                                      na.rm = T),
      health_barriers.cost = sum(
        Household_Roster.member.healthcare_difficulties.healthcare_cost,
        Household_Roster.member.healthcare_difficulties.medicine_cost_high,
        na.rm = T
      ),
      health_barriers.unqualified_staff = sum(
        Household_Roster.member.healthcare_difficulties.unqualified_staff_phc,
        Household_Roster.member.healthcare_difficulties.unqualified_staff_hosp,
        na.rm = T
      ),
      health_barriers.civ_docs_problems = sum(Household_Roster.member.healthcare_difficulties.civ_docs_problems,
                                              na.rm = T),
      health_barriers.no_referral_phc = sum(Household_Roster.member.healthcare_difficulties.no_referral_phc,
                                            na.rm = T),
      health_barriers.phc_closed = sum(Household_Roster.member.healthcare_difficulties.phc_closed,
                                       na.rm = T),
      health_barriers.distance_to_treatmentcenter = sum(
        Household_Roster.member.healthcare_difficulties.distance_to_treatmentcenter,
        na.rm = T
      ),
      health_barriers.refused_treatment = sum(Household_Roster.member.healthcare_difficulties.refused_treatment,
                                              na.rm = T),
      health_barriers.no_medicine = sum(
        Household_Roster.member.healthcare_difficulties.no_medicine_pharm,
        Household_Roster.member.healthcare_difficulties.no_medicine_phc,
        Household_Roster.member.healthcare_difficulties.no_medicine_hosp,
        na.rm = T
      ),
      health_barriers.no_offered_treatment = sum(
        Household_Roster.member.healthcare_difficulties.no_offered_treatment_phc,
        Household_Roster.member.healthcare_difficulties.no_offered_treatment_hosp,
        na.rm = T
      ),
      health_barriers.other = sum(Household_Roster.member.healthcare_difficulties.language_barrier,
                                  Household_Roster.member.healthcare_difficulties.no_transport,
                                  Household_Roster.member.healthcare_difficulties.gender_descrimination,
                                  Household_Roster.member.healthcare_difficulties.no_support_from_family,
                                  na.rm = T),
      employment_primary_barriers.increased_competition = sum(Household_Roster.member.employment_primary_barriers.Increased_competition, na.rm = T),
      employment_primary_barriers.jobs_far = sum(Household_Roster.member.employment_primary_barriers.jobs_far, na.rm = T),
      employment_primary_barriers.only_low_available = sum(Household_Roster.member.employment_primary_barriers.only_low_available, na.rm = T),
      employment_primary_barriers.underqualified_for_jobs = sum(Household_Roster.member.employment_primary_barriers.Underqualified_for_jobs, na.rm = T),
      employment_primary_barriers.lack_of_connections = sum(Household_Roster.member.employment_primary_barriers.lack_of_connections, na.rm = T),
      id_card_u18 = sum(id_card_u18, na.rm = T),
      nationality_u18 = sum(nationality_u18, na.rm = T),
      birth_cert_u18 = sum(birth_cert_u18, na.rm = T),
      id_card = sum(Household_Roster.member.civil_docs.id_card, na.rm = T),
      nationality = sum(Household_Roster.member.civil_docs.citizenship_certificate)
    )
  
  df <- left_join(df, summ, by = c("X_uuid" = "X_submission__uuid"))
  
  df$female_headed <- case_when(df$female_headed > 0 ~ 1, T ~ 0 )
  df$single_headed <- case_when(df$single_headed > 0 ~ 1, T ~ 0 )
  
  df$not_attending_formal <-
    case_when(df$school_aged_children == 0 ~ NA_real_,
              df$not_attending_school > 0 ~ 1,
              T ~ 0)
  df$male_not_attending_formal <-
    case_when(df$male_school_aged_children == 0 ~ NA_real_,
              df$male_not_attending_school > 0 ~ 1,
              T ~ 0)
  df$female_not_attending_formal <-
    case_when(df$female_school_aged_children == 0 ~ NA_real_,
              df$female_not_attending_school > 0 ~ 1,
              T ~ 0)
  df$dropped_from_school <-
    case_when(df$school_aged_children == 0 ~ NA_real_,
              df$dropped_from_school > 0 ~ 1,
              T ~ 0)
  
  df$education_barriers_safety <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$security > 0 ~ 1,
              T ~ 0)
  df$education_barriers_cost <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$cannot_afford > 0 ~ 1,
              T ~ 0)
  df$education_barriers_unable_to_register <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$no_space > 0 |
                df$missed_too_much > 0 ~ 1,
              T ~ 0)
  df$education_barriers_health_issue <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$disability > 0 ~ 1,
              T ~ 0)
  df$education_barriers_physical_limitation <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$distance > 0 ~ 1,
              T ~ 0)
  df$education_barriers_poor_infrastructure <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$bad_condition > 0 ~ 1,
              T ~ 0)
  df$education_barriers_curriculum_not_adapted <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$lack_curriculum > 0 ~ 1,
              T ~ 0)
  df$education_barriers_parental_refusal <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$not_important > 0 ~ 1,
              T ~ 0)
  df$education_barriers_working <-
    case_when(
      df$school_aged_children == 0 |
        df$dropped_from_school == 0 ~ NA_real_,
      
      df$stayed_at_home > 0 |
        df$participate_remunerative_activities > 0 ~ 1,
      T ~ 0
    )
  df$education_barriers_lack_interest <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$not_interested > 0 ~ 1,
              T ~ 0)
  
  df$education_barriers_overcrowded <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$lack_teacher > 0 ~ 1,
              T ~ 0)
  
  df$education_barriers_other <-
    case_when(df$school_aged_children == 0 | df$dropped_from_school == 0 ~ NA_real_,
              df$other_barriers > 0 |
                df$lack_gender > 0 |
                df$recent_displacement > 0 ~ 1,
              T ~ 0)
  
  df$fcs <-
    (as.numeric(df$food_security.fcs.cereals) * 2) + (as.numeric(df$food_security.fcs.nuts_seed) * 3) + (as.numeric(df$food_security.fcs.milk_dairy) * 4) + (as.numeric(df$food_security.fcs.meat) * 4) +
    as.numeric(df$food_security.fcs.vegetables) + as.numeric(df$food_security.fcs.fruits) + (as.numeric(df$food_security.fcs.oil_fats) * 0.5) + (as.numeric(df$food_security.fcs.sweets) * 0.5)
  
  df$fcs_category <- case_when(df$fcs <= 28.5 ~ "Poor",
                               df$fcs < 42.5 ~ "Borderline",
                               T ~ "Acceptable")
  
  df$stress <-
    ifelse(
      df$food_security.coping_strategies_food2.selling_assets %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.borrow_debt  %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.reduce_spending %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.spent_savings %in% c("no_already_did", "yes")   ,
      1,
      0
    )
  df$crisis <-
    ifelse(
      df$food_security.coping_strategies_food2.selling_transportation_means %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.change_place  %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$emergency <-
    ifelse(
      df$food_security.coping_strategies_food2.child_droput_school %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.male_illigal_acts %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.female_illigal_acts %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.family_migrating %in% c("no_already_did", "yes") |
        df$food_security.coping_strategies_food2.child_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  #FOOD CONSUMPTIONS SCORE
  df$fcs <-
    (as.numeric(df$food_security.fcs.cereals) * 2) + (as.numeric(df$food_security.fcs.nuts_seed) * 3) + (as.numeric(df$food_security.fcs.milk_dairy) * 4) + (as.numeric(df$food_security.fcs.meat) * 4) +
    as.numeric(df$food_security.fcs.vegetables) + as.numeric(df$food_security.fcs.fruits) + (as.numeric(df$food_security.fcs.oil_fats) * 0.5) + (as.numeric(df$food_security.fcs.sweets) * 0.5)
  

  df$fcs_category <-
    case_when(df$fcs <= 28 ~ "Poor", df$fcs <= 42 ~ "Borderline", TRUE ~ "Acceptable")
  
  #FOOD EXPENDITURE SHARE
  df$food_share <-
    df$livelihoods.expenses.food_exp_basic_needs / df$livelihoods.expenses.tot_expenditure
  
  df$food_expenditure_share <- case_when(df$food_share > 0.5 ~ 1, T ~ 0)
  
  
  df$hospital_access_services <- case_when(df$health_group.distance_hospital %in% c("within_2km", "between_2km_5km") &
                                    df$health_group.hospital_emergency_ser == "yes" &
                                    df$health_group.hospital_pediatric_ser == "yes" &
                                    df$health_group.hospital_surgical_ser == "yes" ~ 1,
                                  T ~ 0)
  
  df$clinic_access <- case_when(df$health_group.distance_health_service %in% c("within_2km", "between_2km_5km", "in_camp") ~ 1,
                                T ~ 0)
  
  df$difficulty_access_healthcare <- case_when(df$health_accessed > 0 &
                                                 (df$health_barriers.cost >= 1 |
                                                 df$health_barriers.unqualified_staff >= 1 |
                                                 df$health_barriers.civ_docs_problems >= 1 |
                                                 df$health_barriers.no_referral_phc >= 1 |
                                                 df$health_barriers.phc_closed >= 1 |
                                                 df$health_barriers.distance_to_treatmentcenter >= 1 |
                                                 df$health_barriers.refused_treatment >= 1 |
                                                 df$health_barriers.no_medicine >= 1 |
                                                 df$health_barriers.no_offered_treatment >= 1 |
                                                 df$health_barriers.other >= 1) ~ 1,
                                               is.na(df$health_accessed) | df$health_accessed == 0 ~ NA_real_,
                                               T ~ 0)
  
  df$difficulty_access_healthcare_cost    <- case_when(df$health_barriers.cost > 0 ~ 1,
                                                       df$difficulty_access_healthcare == 1 ~ 0,
                                                       T ~ NA_real_)
  df$difficulty_access_healthcare_lack_qualified_staff   <-
    case_when(
      df$health_barriers.unqualified_staff > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_lack_civil_doc  <-
    case_when(
      df$health_barriers.civ_docs_problems > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_no_referral_phc   <-
    case_when(
      df$health_barriers.no_referral_phc > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_phc_closed    <-
    case_when(
      df$health_barriers.phc_closed > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_too_far   <-
    case_when(
      df$health_barriers.distance_to_treatmentcenter > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_staff_refused_treatment  <-
    case_when(
      df$health_barriers.refused_treatment > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_lack_medicine <-
    case_when(
      df$health_barriers.no_medicine > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  df$difficulty_access_healthcare_no_treatment_phc   <-
    case_when(
      df$health_barriers.no_offered_treatment > 0 ~ 1,
      df$difficulty_access_healthcare == 1 ~ 0,
      T ~ NA_real_
    )
  
  df$difficulty_accessing_female_services <- ifelse(df$health_group.women_specialised_services == "no", 1, 0)
  
  df$no_movement_3 <- case_when(df$intentions.movement_intentions == "remain" ~ 1, is.na(df$intentions.movement_intentions) ~ NA_real_, TRUE ~ 0)
  df$no_movement_12 <- case_when(df$intentions.movement_intentions12 %in% c("current", "remain") ~ 1, is.na(df$intentions.movement_intentions12) ~ NA_real_, TRUE ~ 0)
  
  
  
  
  df$return_security_aoo <- df$intentions.reason_to_return_to_aoo.security_stable
  df$return_security_aod <- case_when(df$intentions.reason_to_return_to_aoo.no_safe_in_area_of_displace == 1 |
                                        df$intentions.reason_to_return_to_aoo.forced_security == 1 |
                                        df$intentions.reason_to_return_to_aoo.facing_eviction == 1 ~ 1,
                                      is.na(df$intentions.reason_to_return_to_aoo) ~ NA_real_,
                                      T ~ 0)
  df$return_livelihoods_aod <- df$intentions.reason_to_return_to_aoo.limited_livelihood_opportunities
  df$return_secure_hlp <- df$intentions.reason_to_return_to_aoo.secure_house_land
  df$return_limited_services <- df$intentions.reason_to_return_to_aoo.limited_services
  df$return_available_services_aoo <- df$intentions.reason_to_return_to_aoo.basic_services_availability
  df$return_uxo <- df$intentions.reason_to_return_to_aoo.uxo
  df$return_pull_factors <- case_when(df$intentions.reason_to_return_to_aoo.emotional_desire == 1 |
                                        df$intentions.reason_to_return_to_aoo.fam_released == 1 |
                                        df$intentions.reason_to_return_to_aoo.other_members_returned == 1 ~ 1,
                                      is.na(df$intentions.reason_to_return_to_aoo) ~ NA_real_,
                                      T ~ 0
  )
  
  df$no_return_security_aoo  <- case_when(df$intentions.why_not_return.desicrimination == 1 |
                                            df$intentions.why_not_return.fear_trauma == 1 |
                                            df$intentions.why_not_return.lack_of_security_forces == 1 |
                                            df$intentions.why_not_return.movement_restrictions == 1 ~ 1,
                                          is.na(df$intentions.why_not_return) ~ NA_real_,
                                          T ~ 0)
  df$no_return_lack_livelihoods <- df$intentions.why_not_return.lack_livelihood_generating_income
  df$no_return_lack_livelihoods <-
    case_when(df$intentions.why_not_return.lack_livelihood_generating_income == 1 |
                df$intentions.why_not_return.no_financial_money_to_return == 1 |
                df$intentions.why_not_return.no_transport_to_return == 1 ~ 1,
              is.na(df$intentions.why_not_return) ~ NA_real_,
              T ~ 0)
  df$no_return_lack_shelter <- case_when(df$intentions.why_not_return.hh_assets_stolen_damaged == 1 |
                                           df$intentions.why_not_return.house_damaged_destroyed == 1 |
                                           df$intentions.why_not_return.house_land_occupied == 1 ~ 1,
                                         is.na(df$intentions.why_not_return) ~ NA_real_,
                                         T ~ 0)
  df$no_return_lack_docs  <- df$intentions.why_not_return.no_personal_id
  df$no_return_lack_services <-
    case_when(
      df$intentions.why_not_return.basic_services_not_enough == 1 |
        df$intentions.why_not_return.local_markets_not_working == 1 |
        df$intentions.why_not_return.lack_of_education_oppotunities == 1 ~ 1,
      is.na(df$intentions.why_not_return) ~ NA_real_,
      T ~ 0
    )
  df$no_return_uxo <- df$intentions.why_not_return.presence_of_mines
  df$no_return_pull_factors <-
    case_when(
      df$intentions.why_not_return.children_enrolled_in_displacement == 1 |
        df$intentions.why_not_return.health_conditions == 1 |
        df$intentions.why_not_return.living_conditions_better == 1 |
        df$intentions.why_not_return.immediate_family_wont_return == 1 ~ 1,
      is.na(df$intentions.why_not_return) ~ NA_real_,
      T ~ 0
    )
  
  df$unemployed_seeking_work <- case_when(df$unemployed_seek_work > 0 ~ 1,  TRUE ~ 0)
  df$female_unemployed_seeking_work <- case_when(df$female_unemployed_seek_work > 0 ~ 1, 
                                                 df$female_adult == 0 ~ NA_real_,
                                                 TRUE ~ 0)
  
  df$children_working <- case_when(df$children == 0 ~ NA_real_,
                                   df$child_working > 0 ~ 1,
                                   T ~ 0)
  
  df$employment_barrier_competition <-
    case_when(
      df$employment_primary_barriers.increased_competition > 0 ~ 1,
      df$unemployed_seeking_work == 1 ~ 0,
      T ~ 0
    )
  df$employment_barrier_too_far <-
    case_when(
      df$employment_primary_barriers.jobs_far > 0 ~ 1,
      df$unemployed_seeking_work == 1 ~ 0,
      T ~ 0
    )
  df$employment_barrier_only_low_available <-
    case_when(
      df$employment_primary_barriers.only_low_available > 0 ~ 1,
      df$unemployed_seeking_work == 1 ~ 0,
      T ~ 0
    )
  df$employment_barrier_underqualified_for_jobs <-
    case_when(
      df$employment_primary_barriers.underqualified_for_jobs > 0 ~ 1,
      df$unemployed_seeking_work == 1 ~ 0,
      T ~ 0
    )
  df$employment_barrier_no_wasta <-
    case_when(
      df$employment_primary_barriers.lack_of_connections > 0 ~ 1,
      df$unemployed_seeking_work == 1 ~ 0,
      T ~ 0
    )
  
  df$insufficient_income <-
    case_when(sum_row(df$livelihoods.income.inc_employment, df$livelihoods.income.inc_pension, na.rm = T) < 480000 ~ 1,
              TRUE ~ 0)
  
  df$too_much_debt <- case_when(df$livelihoods.how_much_debt > 505000 ~ 1, TRUE ~ 0)
  
  df$basic_services <- case_when(
    df$livelihoods.reasons_for_debt %in% c(
      "basic_hh_expenditure",
      "health",
      "health Food",
      "Food",
      "Education"
    ) ~ 1, 
    is.na(df$livelihoods.reasons_for_debt) ~ NA_real_, TRUE ~ 0
  )

  df$ngo_assistance <- case_when(
    df$livelihoods.primary_livelihood.NGO_charity_assistance == 1 ~ 1, is.na(df$livelihoods.primary_livelihood) ~ NA_real_ , TRUE ~ 0
  )
  
  df$health_expenses <- case_when(df$livelihoods.expenses.medical_exp_basic_needs/df$livelihoods.expenses.tot_expenditure >= 0.25 ~ 1, TRUE ~ 0)
  
  df$risk_eviction <- case_when(df$S_NFI.hh_risk_eviction == "yes" ~ 1, TRUE ~ 0)
  
  df$movement_restriction <- case_when(
    df$protection.restriction_type.clearance == 1 |
      df$protection.restriction_type.id_authorities == 1 |
      df$protection.restriction_type.time == 1 |
      df$protection.restriction_type.reason == 1 |
      df$protection.restriction_type.road_blocks == 1 |
      df$protection.restriction_type.other == 1 ~ 1 , TRUE ~ 0
  )
  
  df$hh_missing_doc <- case_when(
    df$food_security.access_to_PDS  == "no" |
      df$id_card > 0 |
      df$nationality > 0 |
      df$birth_cert_u18 > 0 ~ 1, 
    TRUE ~ 0
  )
  
  df$children_missing_doc <- case_when(
    df$children == 0 ~ NA_real_,
    df$birth_cert_u18 > 0 |
      df$nationality_u18 > 0 |
       df$id_card_u18 > 0 ~ 1,
    TRUE ~ 0
  )
  
  
  df$child_married <- case_when(df$child_married > 0 ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)
  
  df$child_distress <- case_when(df$health_group.child_distress == "yes" ~ 1, df$children > 0 ~ 0, TRUE ~ NA_real_)

  df$shelter_2_improvements <- case_when(
    df$S_NFI.shelter_better.protec_hazards +
      df$S_NFI.shelter_better.improve_safety +
      df$S_NFI.shelter_better.improve_privacy +
      df$S_NFI.shelter_better.protect_climate +
      df$S_NFI.shelter_better.improve_infrastructure + 
      df$S_NFI.shelter_better.improve_structure >= 2 ~ 1, 
    TRUE ~ 0
  )
  
  df$water_source <- case_when(
    df$wash.drinking_water_source %in% c("dug_well", "other", "river_spring", "water_trucking") ~ "unimproved",
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
  
  df$treat_water <- case_when(df$wash.treat_drink_water_how.not_necessary == 1 ~ 0, is.na(df$wash.treat_drink_water_how) ~ NA_real_, TRUE ~ 1)
  
  
  return(df)
}

