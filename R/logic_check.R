### Logic Checks
## Duplicate data ----------------------------------------------------------------------------------
duplicate_data <- hf_checklist_approved %>% get_dupes(Site_Visit_ID) 
school_checklist_approved %>% get_dupes(Site_Visit_ID)
wss_observation_approved %>% get_dupes(Site_Visit_ID)

# write.xlsx(duplicate_data, "output/manual_review/HF_Obs_duplicate_site_visits.xlsx")

## HF Checklist ------------------------------------------------------------------------------------
# Same HF but different Types
hf_checklist_approved %>% 
  select(Province, District, Village_Name, HF_Name_based_on_Sample, HF_Type_based_on_sample) %>% unique() %>% 
  janitor::get_dupes(HF_Name_based_on_Sample, HF_Type_based_on_sample)

# Check sums
hf_checklist_approved %>% 
  filter(as.numeric(men_staff_HF)+as.numeric(women_staff_HF)!=as.numeric(total_n_staff_HF_M_F) & total_n_staff_HF_M_F %notin% 9999) %>% 
  select(men_staff_HF, women_staff_HF, total_n_staff_HF_M_F)
hf_checklist_approved %>% 
  filter(as.numeric(men_disabled_HF)+as.numeric(women_disabled_HF)!=as.numeric(total_n_staff_HF_M_F_disabilities) & total_n_staff_HF_M_F_disabilities %notin% 9999)

# Logic checks
hf_logical_issues <- rbind(
  hf_checklist_approved %>%
    mutate(total_time = (time_by_walk_min_HF_H*60)+time_by_walk_min_HF_M) %>% 
    filter(location__physical_distance_km_estimated_HF%in% c(1,2,3) & total_time <= 10) %>%
    mutate(issue="Distance in Hour/Minute doesnt seem realistic compared to distance in KM",
           Questions = "time_by_walk_min_HF_H - time_by_walk_min_HF_M - location__physical_distance_km_estimated_HF",
           Values = paste0(time_by_walk_min_HF_H, " - ", time_by_walk_min_HF_M, " - ", location__physical_distance_km_estimated_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>%
    filter(availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF %in% "Yes" &
             availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF %in% "No") %>% 
    mutate(issue="If water point and soap is available then waterpoint alone is also available",
           Questions = "availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF",
           Values = paste0(availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>%
    filter(availability_of_a_designated_handwashing_place_simple_basic_with_functional_water_point_in_hf_HF %in% "No" & 
             (availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF %in% "Yes" |
             availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF %in% "Yes")) %>% 
    mutate(issue="If water point and soap is available then waterpoint alone is also available",
           Questions = "availability_of_a_designated_handwashing_place_simple_basic_with_functional_water_point_in_hf_HF - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF",
           Values = paste0(availability_of_a_designated_handwashing_place_simple_basic_with_functional_water_point_in_hf_HF, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_hf_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>%
    filter(availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF %in% "Yes" &
             hand_washing_station_place_HF %in% "No") %>% 
    mutate(issue="If handwashing station with soap is available then the following question should also be Yes",
           Questions = "availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF - hand_washing_station_place_HF",
           Values = paste0(availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_hf_HF, " - ", hand_washing_station_place_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Water point
  hf_checklist_approved %>% 
    mutate(merged_responses=paste0(hf_staff_men_HF, "-",	hf_staff_women_HF, "-",	patients_men_HF, "-",	patients_women_HF),
           total_resp=str_count(merged_responses, "Yes")) %>% 
    filter(if_yes_total_number_of_available_drinking_water_points_HF<total_resp) %>% 
    mutate(issue="Separate Water points does not match the total water points",
           Questions = "if_yes_total_number_of_available_drinking_water_points_HF - hf_staff_men_HF - hf_staff_women_HF - patients_men_HF - patients_women_HF",
           Values = paste0(if_yes_total_number_of_available_drinking_water_points_HF, " - ", hf_staff_men_HF, " - ", hf_staff_women_HF, " - ", patients_men_HF, " - ", patients_women_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>% 
    filter(number_of_functional_drinking_water_points_HF+number_of_dysfunctional_drinking_water_points_HF!=if_yes_total_number_of_available_drinking_water_points_HF) %>% 
    mutate(issue="Total functional/disyfunctional water points does not equal to available water points",
           Questions = "if_yes_total_number_of_available_drinking_water_points_HF - number_of_functional_drinking_water_points_HF - number_of_dysfunctional_drinking_water_points_HF",
           Values = paste0(if_yes_total_number_of_available_drinking_water_points_HF, " - ", number_of_functional_drinking_water_points_HF, " - ", number_of_dysfunctional_drinking_water_points_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Latrines
  hf_checklist_approved %>% 
    filter(are_there_latrines_available_in_hf_HF %in% "Yes" &	if_so_total_number_of_available_latrines_HF < 1) %>% 
    mutate(issue="Latrines available but total is 0",
           Questions = "are_there_latrines_available_in_hf_HF - if_so_total_number_of_available_latrines_HF",
           Values = paste0(are_there_latrines_available_in_hf_HF, " - ", if_so_total_number_of_available_latrines_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>% 
    filter(are_there_latrines_available_in_hf_HF %in% "No" &	inside_latrine_HF %in% "Yes") %>% 
    mutate(issue="Soap is available inside latrines but latrine itself is not available in HF",
           Questions = "are_there_latrines_available_in_hf_HF - inside_latrine_HF",
           Values = paste0(are_there_latrines_available_in_hf_HF, " - ", inside_latrine_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>% 
    filter(if_not_number_of_functional_latrines_HF+if_not_number_of_dysfunctional_latrines_HF!=if_so_total_number_of_available_latrines_HF) %>% 
    mutate(issue="Total functional/disyfunctional latrines does not equal to available latrines",
           Questions = "if_so_total_number_of_available_latrines_HF - if_not_number_of_functional_latrines_HF - if_not_number_of_dysfunctional_latrines_HF",
           Values = paste0(if_so_total_number_of_available_latrines_HF, " - ", if_not_number_of_functional_latrines_HF, " - ", if_not_number_of_dysfunctional_latrines_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>% 
    mutate(merged_responses=paste0(hf_staff_men_lat_HF, "-",	hf_staff_women_lat_HF, "-",	patients_men_lat_HF, "-",	patients_women_lat_HF),
           total_resp=str_count(merged_responses, "Yes")) %>% 
    filter(if_so_total_number_of_available_latrines_HF<total_resp) %>% 
    mutate(issue="Separate Latrines does not match the total Latrines",
           Questions = "if_so_total_number_of_available_latrines_HF - hf_staff_men_lat_HF - hf_staff_women_lat_HF - patients_men_lat_HF - patients_women_lat_HF",
           Values = paste0(if_so_total_number_of_available_latrines_HF, " - ", hf_staff_men_lat_HF, " - ", patients_men_lat_HF, " - ", patients_men_HF, " - ", patients_women_lat_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  hf_checklist_approved %>% 
    filter(what_type_of_floor_does_the_latrine_have_HF %in% "Cement/Concrete/Ceramic Tiles (Packka)" & type_of_latrines_HF %in% "Unimproved" | 
             what_type_of_floor_does_the_latrine_have_HF %in% "Bare floor or earth/sand, dung, wood/planks (Katcha)" & type_of_latrines_HF %in% "Improved") %>% 
    mutate(issue="Type does not match, please double-check!",
           Questions = "what_type_of_floor_does_the_latrine_have_HF - type_of_latrines_HF",
           Values = paste0(what_type_of_floor_does_the_latrine_have_HF, " - ", type_of_latrines_HF)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
  # Note: no cases of HFs where Latrines are not available, else we could have compared it with all the features (door, wall ...) 
) %>% mutate(Tool="HF_Checklist")

## School Checklist ------------------------------------------------------------------------------------

# Logic checks
school_logical_issues <- rbind(
  school_checklist_approved %>%
    mutate(total_time = (time_by_walk_hours*60)+time_by_walk_minuts) %>% 
    filter(location__physical_distance_km_estimated_School%in% c(1,2,3, 4) & total_time <= 10) %>%
    mutate(issue="Distance in Hour/Minute doesnt seem realistic compared to distance in KM",
           Questions = "time_by_walk_hours - time_by_walk_minuts - location__physical_distance_km_estimated_School",
           Values = paste0(time_by_walk_hours, " - ", time_by_walk_minuts, " - ", location__physical_distance_km_estimated_School)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>%
    filter(availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school %in% "No" &
             availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school%in% "Yes") %>% 
    mutate(issue="If water point and soap is available then waterpoint alone is also available",
           Questions = "availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school",
           Values = paste0(availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>%
    filter(availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school %in% "No" & 
             (availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_the_school %in% "Yes" |
                availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school %in% "Yes")) %>% 
    mutate(issue="If water point and soap is available then waterpoint alone is also available",
           Questions = "availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_the_school - availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school",
           Values = paste0(availability_of_a_designated_place_for_simple_basic_handwashing_with_functional_water_point_in_the_school, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_in_the_school, " - ", availability_of_a_proper_handwashing_station_with_wash_basin_with_functional_water_point_and_soap_in_the_school)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Water point 
  school_checklist_approved %>% 
    mutate(merged_responses=paste0(teachers_men, "-",	teachers_women, "-",	students_boys, "-",	students_girls),
           total_resp=str_count(merged_responses, "Yes")) %>% 
    filter(if_yes_total_number_of_available_drinking_water_points<total_resp) %>% 
    mutate(issue="Separate Water points does not match the total water points",
           Questions = "if_yes_total_number_of_available_drinking_water_points - teachers_men - teachers_women - students_boys - students_girls",
           Values = paste0(if_yes_total_number_of_available_drinking_water_points, " - ", teachers_men, " - ", teachers_women, " - ", students_boys, " - ", students_girls)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>% 
    filter(if_not_number_of_functional_drinking_water_points+if_not_number_of_dysfunctional_drinking_water_points!=if_yes_total_number_of_available_drinking_water_points) %>% 
    mutate(issue="Total functional/disyfunctional water points does not equal to available water points",
           Questions = "if_yes_total_number_of_available_drinking_water_points - if_not_number_of_functional_drinking_water_points - if_not_number_of_dysfunctional_drinking_water_points",
           Values = paste0(if_yes_total_number_of_available_drinking_water_points, " - ", if_not_number_of_functional_drinking_water_points, " - ", if_not_number_of_dysfunctional_drinking_water_points)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  # Latrines
  school_checklist_approved %>% 
    filter(are_latrines_available_in_the_school %in% "Yes" &	if_yes_total_number_of_available_latrines < 1) %>% 
    mutate(issue="Latrines available but total is 0",
           Questions = "are_latrines_available_in_the_school - if_yes_total_number_of_available_latrines",
           Values = paste0(are_latrines_available_in_the_school, " - ", if_yes_total_number_of_available_latrines)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>% 
    filter(are_latrines_available_in_the_school %in% "No" &	inside_latrine %in% "Yes") %>% 
    mutate(issue="Soap is available inside latrines but latrine itself is not available in HF",
           Questions = "are_latrines_available_in_the_school - inside_latrine",
           Values = paste0(are_latrines_available_in_the_school, " - ", inside_latrine)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>% 
    filter(if_no_number_of_functional_latrines+if_no_number_of_dysfunctional_latrines!=if_yes_total_number_of_available_latrines) %>% 
    mutate(issue="Total functional/disyfunctional latrines does not equal to available latrines",
           Questions = "if_yes_total_number_of_available_latrines - if_no_number_of_functional_latrines - if_no_number_of_dysfunctional_latrines",
           Values = paste0(if_yes_total_number_of_available_latrines, " - ", if_no_number_of_functional_latrines, " - ", if_no_number_of_dysfunctional_latrines)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  school_checklist_approved %>%  			
    mutate(merged_responses=paste0(teachers_men_lat, "-",	teachers_women_lat, "-",	students_boys_lat, "-",	students_girls_lat),
           total_resp=str_count(merged_responses, "Yes")) %>% 
    filter(if_yes_total_number_of_available_latrines<total_resp) %>% 
    mutate(issue="Separate Latrines does not match the total Latrines",
           Questions = "if_yes_total_number_of_available_latrines - teachers_men_lat - teachers_women_lat - students_boys_lat - students_girls_lat",
           Values = paste0(if_yes_total_number_of_available_latrines, " - ", teachers_men_lat, " - ", teachers_women_lat, " - ", students_boys_lat, " - ", students_girls_lat)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
  # Note: no cases of School where Latrines are not available, else we could have compared it with all the features (door, wall ...) 
) %>% mutate(Tool="School_Checklist")

# write.xlsx(school_logical_issues, "output/manual_review/School_logic_check.xlsx")


# Rejected and approved data -----------------------------------------------------------------------
hf_checklist_approved %>% count(review_status, qa_status)
# rejec_approved <- rbind(
#   hf_checklist_approved %>% filter(review_status %in% "REJECTED" & qa_status %in% "Approved") %>%
#     select(KEY, review_status, qa_status) %>% mutate(Tool = "1.2"),
# )

# Export list --------------------------------------------------------------------------------------
logical_issues <- plyr::rbind.fill(
  hf_logical_issues,
  school_logical_issues
)

logical_issues_list <- list(
  logical_issues=logical_issues
)

# Remove extra objects -----------------------------------------------------------------------------
rm()


