# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
tabs <- c("data")
sm_variables <- read_excel("input/select_multiple_questions.xlsx") %>% pull(questions)
## Filter empty rows
correction_log_filtered <- correction_log %>%
  filter(!(is.na(`Full_ KEY`) & is.na(question) & is.na(old_value))) %>%
  rename(Tab_Name=Sheet_Name, KEY=`Full_ KEY`) %>%
  mutate(new_value = case_when(
    question %in% sm_variables ~ str_replace_all(new_value, "-|,|  | - ", " ") %>% str_squish(),
    TRUE ~ str_squish(new_value)
  ),
  Tab_Name = "data", # No repeat groups in these datasets
  # KEY= case_when(
  #   is.na(KEY) & !is.na(`Full_ KEY`) ~ str_squish(`Full_ KEY`),
  #   TRUE ~ str_squish(KEY)
  # )
  )

# Identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    is.na(Tools) ~ "Tool name is missing",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    Tools == "Household Survey" & question %notin% names(household_dt) ~ "question",
    Tools == "Household Survey" & KEY %notin% household_dt$KEY ~ "KEY",
    Tools == "Observation Checklist HF" & question %notin% names(hf_checklist) ~ "question",
    Tools == "Observation Checklist HF" & KEY %notin% hf_checklist$KEY ~ "KEY",
    Tools == "Observation Checklist School" & question %notin% names(school_checklist) ~ "question",
    Tools == "Observation Checklist School" & KEY %notin% school_checklist$KEY ~ "KEY",
    Tools == "Observation Water Supply System" & question %notin% names(wss_observation) ~ "question",
    Tools == "Observation Water Supply System" & KEY %notin% wss_observation$KEY ~ "KEY")) # Add tool name based on Log names

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(correction_log_filtered[, c("KEY", "question")])

# Filter issues
correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question)

correction_log_filtered <- correction_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue)) 

# apply the correction-log -------------------------------------------
## Household Survey
household_dt_copy <- household_dt
household_dt <- apply_log(data = household_dt, log= filter(correction_log_filtered, Tools == "Household Survey" & Tab_Name == "data"), 
                        data_KEY = "KEY",
                        log_columns = c(question = "question",
                                        old_value = "old_value",
                                        new_value = "new_value",
                                        KEY = "KEY"))

## Observation Checklist - HF
hf_checklist_copy <- hf_checklist
hf_checklist <- apply_log(data = hf_checklist, log= filter(correction_log_filtered, Tools == "Observation Checklist HF" & Tab_Name == "data"), 
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))


## Observation Checklist - School
school_checklist_copy <- school_checklist
school_checklist <- apply_log(data = school_checklist, log= filter(correction_log_filtered, Tools == "Observation Checklist School" & Tab_Name == "data"), 
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))


## Water Supply System Observation
wss_observation_copy <- wss_observation
wss_observation <- apply_log(data = wss_observation, log= filter(correction_log_filtered, Tools == "Observation Water Supply System" & Tab_Name == "data"), 
                          data_KEY = "KEY",
                          log_columns = c(question = "question",
                                          old_value = "old_value",
                                          new_value = "new_value",
                                          KEY = "KEY"))



# Verify correction log -------------------------------------------
message("Verifying Correction log, please wait!")
correction_log_discrep <- rbind(
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
correction_log_discrep <- correction_log_discrep %>%
  anti_join(correction_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(household_dt_copy, hf_checklist_copy, school_checklist_copy, wss_observation_copy, sm_variables,
   tabs, correction_log_filtered)



