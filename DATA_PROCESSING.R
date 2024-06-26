##### Data Processing Script #####
# Install/load required packages -------------------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(janitor)) install.packages("janitor")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Declaring Global Variables -----------------------------------------------------------------------
hh_tool_path <- "input/tools/UNICEF+WASH+-+Household+Survey.xlsx"
hf_obs_tool_path <- "input/tools/UNICEF+Wash-HF+Observation+Checklist.xlsx"
school_tool_path <- "input/tools/UNICEF+Wash-School+Observation+Checklist.xlsx"
wss_tool_path <- "input/tools/UNICEF+Wash-+WSS+Observation.xlsx"

# Read data ----------------------------------------------------------------------------------------
# file.edit("R/read_data.R")
source("R/read_data.R") # read data

# Rename problematic column
household_dt <- household_dt %>% 
  rename(DK.2=DK_2...326, # 0s/1s column
         DK_2=DK_2...336 # Main question
  ) # Rename this in Final Client dataset


# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTF_xbTVE6fH9sBSB4s-s70-4cYm_3aho6DXf3_9ZAVs9u5aLPcq1EK_NacvnevL4EO47WwRl1YzXNM/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=473078450&single=true&output=csv"), col_types = "c")
correction_log <- readr::read_csv(paste0(url, "gid=758688462&single=true&output=csv"), col_types = "c")

# Join QA Status -----------------------------------------------------------------------------------
count(qa_log, Tool, `Final QA Status`)
qa_log_sub <- qa_log %>%
  select(KEY, qa_status=`Final QA Status`, Tool) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status
  )) %>% unique()

## Household Survey
household_dt <- household_dt %>%
  left_join(filter(qa_log_sub, Tool=="Household Survey"), by="KEY") %>% select(-Tool)

## Observation Checklist - HF
hf_checklist <- hf_checklist %>% left_join(filter(qa_log_sub, Tool=="Observation Checklist HF"), by="KEY") %>% select(-Tool)

## Observation Checklist - School
school_checklist <- school_checklist %>% left_join(filter(qa_log_sub, Tool=="Observation Checklist School"), by="KEY") %>% select(-Tool)

## Water Supply System Observation
wss_observation <- wss_observation %>% left_join(filter(qa_log_sub, Tool=="Observation Water Supply System"), by="KEY") %>% select(-Tool)

# apply correction log -----------------------------------------------------------------------------
correction_log %>% count(Tools, Sheet_Name)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R") # Check unique key
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Remove Rejected data ----------------------------------------------------------------------------
# # file.edit("R/remove_rejected_data.R")
source("R/remove_rejected_data.R")

# Relevancy check ----------------------------------------------------------------------------------
# file.edit("R/check_relevancy_rules.R")
source("R/check_relevancy_rules.R") # Figure out the one separator issue HH

## Attach labels -----------------------------------------------------------------------------------
# file.edit("R/attach_labels.R")
source("R/attach_labels.R")

# # apply Translation log ----------------------------------------------------------------------------
# translation_log %>% count(Tool, `Tab Name`)
# # file.edit("R/apply_translation_log.R")
# source("R/apply_translation_log.R")
# if(nrow(translation_log_discrep) !=0){
#   print("Correction Logs not applied -------------------")
#   correction_log_discrep
# }

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R") 

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(Tool, qa_status=`Final QA Status`, KEY)
## Filter
QA_backlog_keys <- rbind(
  left_join(
    household_dt %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="Household_Survey"),
  left_join(
    hf_checklist %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="HF_Checklist"),
  left_join(
    school_checklist %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="School_Checklist"),
  left_join(
    wss_observation %>% select(SubmissionDate, KEY),
    qa_log_sub, by = "KEY") %>% mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="WSS_Observation")) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>%
  filter(qa_status %notin% c("Approved", "Excel Check Approved", "Rejected", "Partial Check Approved")) # Filter Keys not yet QAed
# Count
QA_backlog <- QA_backlog_keys %>%
  group_by(SubmissionDate, Tool) %>% count(qa_status, name = "freq") %>%
  # mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% arrange(SubmissionDate) %>%
  pivot_wider(names_from = "Tool", values_from = "freq")
# Print
print(knitr::kable(QA_backlog, format = "simple"))

## Filter Approved data ----------------------------------------------------------------------------
approved_qa_status <- c("Approved", "Excel Check Approved", "Partial Check Approved") # Double-check in the end
# # file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R")

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
# source("R/logic_check.R")

## Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
# source("R/remove_extra_columns.R")

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/check_missing_translation.R")
source("R/check_missing_translation.R") # Add export # Temorary filter for QA at the end


# Export -------------------------------------------------------------------------------------------
## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)

## export cleaned datasets
check_path("output/cleaned_data") # create the output path
writexl::write_xlsx(list(data=household_dt), paste0("output/cleaned_data/UNICEF_WASH_Household_Survey_", lubridate::today(), ".xlsx"))
writexl::write_xlsx(list(data=hf_checklist), paste0("output/cleaned_data/UNICEF_WASH_Observation_Checklist_HF_", lubridate::today(), ".xlsx"))
writexl::write_xlsx(list(data=school_checklist), paste0("output/cleaned_data/UNICEF_WASH_Observation_Checklist_School_", lubridate::today(), ".xlsx"))
writexl::write_xlsx(list(data=wss_observation), paste0("output/cleaned_data/UNICEF_WASH_WSS_Observation_", lubridate::today(), ".xlsx"))

## export client datasets
check_path("output/client_data") # create the output path
export_datasets(list(data=household_dt_approved), paste0("output/cleaned_data/UNICEF_WASH_Household_Survey_Pilot_", lubridate::today(), ".xlsx"))
export_datasets(list(data=hf_checklist_approved), paste0("output/cleaned_data/UNICEF_WASH_Observation_Checklist_HF_", lubridate::today(), ".xlsx"))
export_datasets(list(data=school_checklist_approved), paste0("output/cleaned_data/UNICEF_WASH_Observation_Checklist_School_", lubridate::today(), ".xlsx"))
export_datasets(list(data=wss_observation_approved), paste0("output/cleaned_data/UNICEF_WASH_WSS_Observation_", lubridate::today(), ".xlsx"))


## export additional files
writexl::write_xlsx(correction_log, "output/correction_log.xlsx", format_headers = F) # correction
writexl::write_xlsx(correction_log_issues, "output/correction_log_issues.xlsx", format_headers = F) # correction log issues
# writexl::write_xlsx(translation_log_issues, "output/translation_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)
writexl::write_xlsx(SM_issues, "output/Select_multiple_issues.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_QA_log, "output/Missing_audio_translation_&_image_QA.xlsx", format_headers = F)
writexl::write_xlsx(qa_backlog_list, "output/QA_backlog.xlsx", format_headers = F)
writexl::write_xlsx(response_log_list, "output/dataset_response_mismatch_with_tool.xlsx", format_headers = F)
# writexl::write_xlsx(logical_issues_list, "output/Logical_issues.xlsx", format_headers = F) # Add later

