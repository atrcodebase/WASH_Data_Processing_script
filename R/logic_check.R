### Logic Checks
## Duplicate data ----------------------------------------------------------------------------------
duplicate_data <- hf_checklist_approved %>% get_dupes(Site_Visit_ID) 
school_checklist_approved %>% get_dupes(Site_Visit_ID)
wss_observation_approved %>% get_dupes(Site_Visit_ID)
# household_dt_approved %>% get_dupes(Site_Visit_ID) # Not applicable

# write.xlsx(duplicate_data, "output/manual_review/HF_Obs_duplicate_site_visits.xlsx")

## Household Survey --------------------------------------------------------------------------------
# Logic checks
hh_logical_issues <- rbind(
  # Respondent Information
  household_dt_approved %>%
    filter(D5 %in% "Bachelor" & Age_Of_Interviewee < 22 | 
             (D5 %in% "Master/PhD" & Age_Of_Interviewee < 25)) %>%
    mutate(issue="Age & Education status of the respondent doesn't seem realistic, please double-check!",
           Questions = "D5 - Age_Of_Interviewee",
           Values = paste0(D5, " - ", Age_Of_Interviewee)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Household income
  household_dt_approved %>%
    filter(D7 < 1 | D7 > 100000) %>%
    mutate(issue="Household income doesnt seem realistic, please double-check!",
           Questions = "D7",
           Values = D7) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Displaced Years
  household_dt_approved %>%
    filter(D8 %in% "Yes" & D8_How_Long_Ago_Years == 0 & D8_How_Long_Ago_Month == 0) %>%
    mutate(issue="Year and month are both 0",
           Questions = "D8 - D8_How_Long_Ago_Years - D8_How_Long_Ago_Month",
           Values = paste0(D8, " - ", D8_How_Long_Ago_Years, " - ", D8_How_Long_Ago_Month)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Household members
  household_dt_approved %>%
    filter(D2_Total_M_F_5_Years_And_Under != (D2_Male_5_Years_And_Under+D2_Female_5_Years_And_Under)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_M_F_5_Years_And_Under - D2_Male_5_Years_And_Under - D2_Female_5_Years_And_Under",
           Values = paste0(D2_Total_M_F_5_Years_And_Under, " - ", D2_Male_5_Years_And_Under, " - ", D2_Female_5_Years_And_Under)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D2_Total_M_F_6_18_Years != (D2_Male_6_18_Years+D2_Female_6_18_Years)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_M_F_6_18_Years - D2_Male_6_18_Years - D2_Female_6_18_Years)",
           Values = paste0(D2_Total_M_F_6_18_Years, " - ", D2_Male_6_18_Years, " - ", D2_Female_6_18_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D2_Total_M_F_Above_18_Years != (D2_Male_Above_18_Years+D2_Female_Above_18_Years)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_M_F_Above_18_Years - D2_Male_Above_18_Years - D2_Female_Above_18_Years)",
           Values = paste0(D2_Total_M_F_Above_18_Years, " - ", D2_Male_Above_18_Years, " - ", D2_Female_Above_18_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D2_Total_HH_members != (D2_Total_M_F_5_Years_And_Under+D2_Total_M_F_6_18_Years+D2_Total_M_F_Above_18_Years)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_HH_members - D2_Total_M_F_5_Years_And_Under - D2_Total_M_F_6_18_Years - D2_Total_M_F_Above_18_Years",
           Values = paste0(D2_Total_HH_members, " - ", D2_Total_M_F_5_Years_And_Under, " - ", D2_Total_M_F_6_18_Years, " - ", D2_Total_M_F_Above_18_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D2_Total_HH_members != D1) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_HH_members - D1",
           Values = paste0(D2_Total_HH_members, " - ", D1)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Household members with Physical disability
  household_dt_approved %>%
    filter(D4_1_Total_M_F_5_Years_And_Under_Disability != (D4_1_Male_5_Years_And_Under_Disability+D4_1_Female_5_Years_And_Under_Disability)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D4_1_Total_M_F_5_Years_And_Under_Disability - D4_1_Male_5_Years_And_Under_Disability - D4_1_Female_5_Years_And_Under_Disability",
           Values = paste0(D4_1_Total_M_F_5_Years_And_Under_Disability, " - ", D4_1_Male_5_Years_And_Under_Disability, " - ", D4_1_Female_5_Years_And_Under_Disability)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D4_1_Total_M_F_6_18_Years_Disability != (D4_1_Male_6_18_Years_Disability+D4_1_Female_6_18_Years_Disability)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D4_1_Total_M_F_6_18_Years_Disability - D4_1_Male_6_18_Years_Disability - D4_1_Female_6_18_Years_Disability)",
           Values = paste0(D4_1_Total_M_F_6_18_Years_Disability, " - ", D4_1_Male_6_18_Years_Disability, " - ", D4_1_Female_6_18_Years_Disability)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D4_1_Total_M_F_Above_18_Years_Disability != (D4_1_Male_Above_18_Years_Disability+D4_1_Female_Above_18_Years_Disability)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D4_1_Total_M_F_Above_18_Years_Disability - D4_1_Male_Above_18_Years_Disability - D4_1_Female_Above_18_Years_Disability)",
           Values = paste0(D4_1_Total_M_F_Above_18_Years_Disability, " - ", D4_1_Male_Above_18_Years_Disability, " - ", D4_1_Female_Above_18_Years_Disability)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D4_1_Total_HH_members_Disability != (D4_1_Total_M_F_5_Years_And_Under_Disability+D4_1_Total_M_F_6_18_Years_Disability+D4_1_Total_M_F_Above_18_Years_Disability)) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_HH_members - D4_1_Total_M_F_5_Years_And_Under_Disability - D4_1_Total_M_F_6_18_Years_Disability - D4_1_Total_M_F_Above_18_Years_Disability",
           Values = paste0(D2_Total_HH_members, " - ", D4_1_Total_M_F_5_Years_And_Under_Disability, " - ", D4_1_Total_M_F_6_18_Years_Disability, " - ", D4_1_Total_M_F_Above_18_Years_Disability)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(D4_1_Total_HH_members_Disability != D4_1_How_Many_Household_Members_Experience_Such_Physical_Disabilities) %>%
    mutate(issue="Total Doesnt match the sum of people",
           Questions = "D2_Total_HH_members - D4_1_How_Many_Household_Members_Experience_Such_Physical_Disabilities",
           Values = paste0(D2_Total_HH_members, " - ", D4_1_How_Many_Household_Members_Experience_Such_Physical_Disabilities)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Water
  household_dt_approved %>%
    filter(grepl("Insufficient WASH facilities in health facilities|Inadequate WASH facilities in schools", R_1) & 
             R_2 %in% "Fully meets community needs.") %>%
    mutate(issue="Insufficient/inadequate WASH facilities are reported as unmet needs and yet they also say they fully meet community needs",
           Questions = "R_1 - R_2",
           Values = paste0(R_1, " - ", R_2)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(W1 %in% "Piped to neighbour" & W2_1 %in% c("16 - 30 minutes walking distance (for one complete round trip)",
                                                      "Between 30 - 60 minutes walking distance (for one complete round trip)",
                                                      "More than one hour walking distance (for one complete round trip")) %>%
    mutate(issue="Main water Source is piped to neighbor and yet the distance is more than where a typical neighbor would be",
           Questions = "W1 - W2_1",
           Values = paste0(W1, " - ", W2_1)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(W3 %in% "Girl (<15 years)" & D2_Female_5_Years_And_Under == 0 & D2_Female_6_18_Years == 0) %>%
    mutate(issue="There are no girls under 18 in HH and yet they reported Girls less than 15 fetch water",
           Questions = "W3 - D2_Female_5_Years_And_Under - D2_Female_6_18_Years",
           Values = paste0(W3, " - ", D2_Female_5_Years_And_Under, " - ", D2_Female_6_18_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(W3 %in% "Boy (<15 years)" & D2_Male_5_Years_And_Under == 0 & D2_Male_6_18_Years == 0) %>%
    mutate(issue="There are no boys under 18 in HH and yet they reported boys less than 15 fetch water",
           Questions = "W3 - D2_Male_5_Years_And_Under - D2_Male_6_18_Years",
           Values = paste0(W3, " - ", D2_Male_5_Years_And_Under, " - ", D2_Male_6_18_Years)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    mutate(W1_new = str_replace_all(W1, "\\/", "\\\\/"),
           W1_new = str_replace_all(W1_new, "\\(", "\\\\("),
           W1_new = str_replace_all(W1_new, "\\)", "\\\\)"),
           W1_new = str_replace_all(W1_new, "\\–", "\\\\–")) %>% 
    rowwise() %>% 
    filter(grepl(W1_new, W6_B)) %>%
    mutate(issue="Main water Source is the same or is included in the other water source",
           Questions = "W1 - W6_B",
           Values = paste0(W1, " - ", W6_B)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter((W1 %in% "Tanker/Truck" | W7 %in% "Tanker/Truck") & WTS_1_A %in% "No") %>%
    mutate(issue="Water source for drinking or bathing is from Tanker Truck and yet they reported never receiving water from truck",
           Questions = "W1 - W7 - WTS_1_A",
           Values = paste0(W1, " - ", W7, " - ", WTS_1_A)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(W1 %in% "Packaged/delivered water (Bottled water)" & W6_A %in% c("During the dry season", "During the rainy season")) %>%
    mutate(issue="The respondent reported not having access to primary water source in rainy/dry season but their source is bottled water",
           Questions = "W1 - W6_A",
           Values = paste0(W1, " - ", W6_A)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Toilet
  household_dt_approved %>%
    filter(S4 %in% "Everyone in the family uses toilet" & S3_B <= D2_Total_HH_members) %>%
    mutate(issue="Everyone in the family uses toilet including other families and yet the number of people using the toilet is less than household members",
           Questions = "S4 - S3_B - D2_Total_HH_members",
           Values = paste0(S4, " - ", S3_B, " - ", D2_Total_HH_members)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(W7 %in% "Packaged/delivered water (Bottled water)") %>%
    mutate(issue="It is unlikely for a household to use bottled water for bathing, please double-check!",
           Questions = "W7",
           Values = W7) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(grepl("Lack of sanitation facilities \\(toilets, latrines\\)", R_1) & 
             S1 %in% "Yes") %>%
    mutate(issue="Lack of toilets reported as unmet needs and yet they also reported having toilets in household",
           Questions = "R_1 - S1",
           Values = paste0(R_1, " - ", S1)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>%
    filter(grepl("Thrown in toilet facility\\/latrine|In the pit latrine \\(unimproved\\)", S12) & 
             S1 %in% "No") %>%
    mutate(issue="Thrown in toilet/pit latrine but household have no toilet",
           Questions = "S12 - S1",
           Values = paste0(S12, " - ", S1)) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  # Information
  household_dt_approved %>%
    filter(grepl("Don’t know", CC_7) & 
             grepl(";", CC_7)) %>%
    mutate(issue="I don't know and other responses shouldn't be selected together",
           Questions = "CC_7",
           Values = paste0(CC_7)) %>% 
    select(Questions, Values, issue, KEY, qa_status)
)

## Other columns
dont_know_resps <- c("99999", "9999", "999", "99", "999o", 0:9)
hh_other_issues <- rbind(
  household_dt_approved %>% 
    filter(S7_Other %in% dont_know_resps) %>%
    mutate(issue="Other not recoded!",
           Questions = "S7_Other",
           Values = S7_Other) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(CC_1_Name %in% c(dont_know_resps, "Im", "Wha", "Ta ta")) %>%
    mutate(issue="Other not recoded!",
           Questions = "CC_1_Name",
           Values = CC_1_Name) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(CC_1A_Other %in% c(dont_know_resps, "Im", "Wha", "Ta ta")) %>%
    mutate(issue="Other not recoded!",
           Questions = "CC_1A_Other",
           Values = CC_1A_Other) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(Message_3 %in% c(dont_know_resps, "000", ".", "...", "....")) %>%
    mutate(issue="Other not recoded!",
           Questions = "Message_3",
           Values = Message_3) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(MS1_Name %in% c(dont_know_resps, "inc")) %>%
    mutate(issue="Other not recoded!",
           Questions = "MS1_Name",
           Values = MS1_Name) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(MS1_A_Other %in% c(dont_know_resps)) %>%
    mutate(issue="Other not recoded!",
           Questions = "MS1_A_Other",
           Values = MS1_A_Other) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(MS2_Name %in% c(dont_know_resps, "Im")) %>%
    mutate(issue="Other not recoded!",
           Questions = "MS2_Name",
           Values = MS2_Name) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(CRM_2_Name %in% c(dont_know_resps, "Im")) %>%
    mutate(issue="Other not recoded!",
           Questions = "CRM_2_Name",
           Values = CRM_2_Name) %>% 
    select(Questions, Values, issue, KEY, qa_status),
  
  household_dt_approved %>% 
    filter(WTS_1_Name %in% c(dont_know_resps, "IM")) %>%
    mutate(issue="Other not recoded!",
           Questions = "WTS_1_Name",
           Values = WTS_1_Name) %>% 
    select(Questions, Values, issue, KEY, qa_status)
)

# write.xlsx(hh_logical_issues, "output/manual_review/HH_logic_check.xlsx")


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
  # hf_logical_issues,
  # school_logical_issues
  hh_logical_issues
)

logical_issues_list <- list(
  logical_issues=logical_issues,
  hh_other_recode=hh_other_issues
)

# Remove extra objects -----------------------------------------------------------------------------
rm()


