# Log missing audio translation and missing image QA -----------------------------------------------
## Household Survey
household_tool <- read_excel(hh_tool_path, "survey", guess_max = 100000)
hh_audio_cols <- household_tool %>% filter(type %in% c("audio") & name %in% names(household_dt)) %>% pull(name)
hh_image_cols <- household_tool %>% filter(type %in% c("image") & name %in% names(household_dt)) %>% pull(name)

hh_missing_log <- rbind(
  # Translation
  log_questions(data=household_dt, columns=hh_audio_cols, suffix="Translation", sheet="data")
  # Image QA
  # log_questions(data=household_dt, columns=hh_image_cols, suffix="QA", sheet="data") # No QA required for non ARTF projects
)

## Observation Checklist - HF
hf_tool <- read_excel(hf_obs_tool_path, "survey", guess_max = 100000)
hf_audio_cols <- hf_tool %>% filter(type %in% c("audio") & name %in% names(hf_checklist)) %>% pull(name)
hf_image_cols <- hf_tool %>% filter(type %in% c("image") & name %in% names(hf_checklist)) %>% pull(name)

hf_missing_log <- rbind(
  # Translation
  log_questions(data=hf_checklist, columns=hf_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=hf_checklist, columns=hf_image_cols, suffix="QA", sheet="data") # No QA required for non ARTF projects
)

## Observation Checklist - School
school_tool <- read_excel(school_tool_path, "survey", guess_max = 100000)
school_audio_cols <- school_tool %>% filter(type %in% c("audio") & name %in% names(school_checklist)) %>% pull(name)
school_image_cols <- school_tool %>% filter(type %in% c("image") & name %in% names(school_checklist)) %>% pull(name)

school_missing_log <- rbind(
  # Translation
  log_questions(data=school_checklist, columns=school_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=school_checklist, columns=school_image_cols, suffix="QA", sheet="data") # No QA required for non ARTF projects
)

## Water Supply System Observation
wss_tool <- read_excel(wss_tool_path, "survey", guess_max = 100000)
wss_audio_cols <- wss_tool %>% filter(type %in% c("audio") & name %in% names(wss_observation)) %>% pull(name)
wss_image_cols <- wss_tool %>% filter(type %in% c("image") & name %in% names(wss_observation)) %>% pull(name)

wss_missing_log <- rbind(
  # Translation
  log_questions(data=wss_observation, columns=wss_audio_cols, suffix="Translation", sheet="data"),
  # Image QA
  log_questions(data=wss_observation, columns=wss_image_cols, suffix="QA", sheet="data") # No QA required for non ARTF projects
)

## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- c()

missing_translation_log <- rbind(
  missing_translation(data = household_dt, KEY = "KEY", excluded_cols) %>% mutate(Tool = "Household_Survey", tab_name="data"),
  missing_translation(data = hf_checklist, KEY = "KEY", excluded_cols) %>% mutate(Tool = "HF_Checklist", tab_name="data"),
  missing_translation(data = school_checklist, KEY = "KEY", excluded_cols) %>% mutate(Tool = "School_Checklist", tab_name="data"),
  missing_translation(data = wss_observation, KEY = "KEY", excluded_cols) %>% mutate(Tool = "WSS_Observation", tab_name="data")
  
)

## Export List -------------------------------------------------------------------------------------
missing_translation_QA_log <- rbind(
  hh_missing_log %>% mutate(Tool = "Household_survey"),
  hf_missing_log %>% mutate(Tool = "HF_Checklist"),
  school_missing_log %>% mutate(Tool = "School_Checklist"),
  wss_missing_log %>% mutate(Tool = "WSS_Observation")
  
) %>% 
  arrange(Tool, KEY)

## Separate translation and image logs
missing_translation_QA_log_sub <- missing_translation_QA_log %>% 
  mutate(key=str_split_fixed(KEY, "/", 2)[,1])

# Export list
missing_translation_QA_log <- list(
  Image_log=filter(missing_translation_QA_log, question_type=="QA"),
  Audio_log=missing_translation_QA_log_sub
)

# remove extra objects -----------------------------------------------------------------------------
rm(hh_audio_cols, hh_image_cols, missing_translation_QA_log_sub)
