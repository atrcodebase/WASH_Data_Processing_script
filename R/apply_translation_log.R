# clean the translation log -----------------------------------------------------------------
tabs <- c("data")
tool_names <- c("Household_Survey", "Observation Checklist HF", "Observation Checklist School", "Observation Water Supply System")
## Filter empty rows
translation_log_filtered <- translation_log %>%
  mutate(Translation=case_when(
    !is.na(`Final Translation`) ~ `Final Translation`,
    TRUE ~ Translation
  ), old_value="") %>% 
  select(KEY, Tool, Tab_Name=Tab, question=Question, old_value, new_value=Translation)

# Identify issues
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    is.na(Tool) & Tool %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    is.na(new_value) ~ "Translation is missing",
    Tool == "Household_Survey" & question %notin% names(household_dt) ~ "question",
    Tool == "Household_Survey" & KEY %notin% household_dt$KEY ~ "KEY",
    Tool == "Observation Checklist HF" & question %notin% names(hf_checklist) ~ "question",
    Tool == "Observation Checklist HF" & KEY %notin% hf_checklist$KEY ~ "KEY",
    Tool == "Observation Checklist School" & question %notin% names(school_checklist) ~ "question",
    Tool == "Observation Checklist School" & KEY %notin% school_checklist$KEY ~ "KEY",
    Tool == "Observation Water Supply System" & question %notin% names(wss_observation) ~ "question",
    Tool == "Observation Water Supply System" & KEY %notin% wss_observation$KEY ~ "KEY"))

translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(translation_log_filtered[, c("KEY", "question")])

# Filter issues
translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question)

# # Join QA_Status with the issues
# translation_log_issues <- translation_log_issues %>% 
#   left_join(select(qa_log_sub, -Tool), by="KEY") %>% 
#   filter(qa_status %in% c("Approved", "Excel Check Approved")) # Filtering issues of approved data only (QA)

translation_log_filtered <- translation_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue))

# apply the Translation log -------------------------------------------
## Household Survey
household_dt_copy <- household_dt
household_dt <- apply_log(data = household_dt, log= filter(translation_log_filtered, Tool == "Household_Survey" & Tab_Name == "data"), 
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))

## Observation Checklist - HF
hf_checklist_copy <- hf_checklist
hf_checklist <- apply_log(data = hf_checklist, log= filter(translation_log_filtered, Tool == "Observation Checklist HF" & Tab_Name == "data"), 
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))


## Observation Checklist - School
school_checklist_copy <- school_checklist
school_checklist <- apply_log(data = school_checklist, log= filter(translation_log_filtered, Tool == "Observation Checklist School" & Tab_Name == "data"), 
                              data_KEY = "KEY",
                              log_columns = c(question = "question",
                                              old_value = "old_value",
                                              new_value = "new_value",
                                              KEY = "KEY"))


## Water Supply System Observation
wss_observation_copy <- wss_observation
wss_observation <- apply_log(data = wss_observation, log= filter(translation_log_filtered, Tool == "Observation Water Supply System" & Tab_Name == "data"), 
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))

# Verify Translation log -------------------------------------------
message("Verifying Correction log, please wait!")
translation_log_discrep <- rbind(
  ## Household_Survey
  compare_dt(df1 = household_dt_copy, df2 = household_dt,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Household_Survey"),
  ## Observation Checklist - HF
  compare_dt(df1 = hf_checklist_copy, df2 = hf_checklist,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "HF_Checklist"),
  ## Observation Checklist - School
  compare_dt(df1 = school_checklist_copy, df2 = school_checklist,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "School_Checklist"),
  ## Water Supply System Observation
  compare_dt(df1 = wss_observation_copy, df2 = wss_observation,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "WSS_Observation")
  ) 

# Removing extra spaces from new_value before joining 
translation_log_discrep <- translation_log_discrep %>%
  anti_join(translation_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(hf_t1_data_copy, hf_injuries_copy, hf_incidents_copy, hf_fatalities_copy, hf_t2_data_copy,
   hf_t3_data_copy, t2_data_copy, t3_data_copy, translation_log_filtered, tabs, t2_income_copy,
   t2_illness_copy, t2_injuries_copy, t2_immunization_copy, t2_other_copy)



