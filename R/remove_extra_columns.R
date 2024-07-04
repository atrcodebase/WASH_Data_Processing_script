### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx")
extra_cols %>% count(Tool, Sheet)

## Remove Extra columns ----------------------------------------------------------------------------
# Household Survey
household_dt_approved <- household_dt_approved %>% 
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "household_survey" & extra_cols$Sheet %in% "data"]))

## Observation Checklist - HF
hf_checklist_approved <- hf_checklist_approved %>% 
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "HF_checklist" & extra_cols$Sheet %in% "data"]))

## Observation Checklist - School
school_checklist_approved <- school_checklist_approved %>% 
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "school_checklist" & extra_cols$Sheet %in% "data"]))

## Water Supply System Observation
wss_observation_approved <- wss_observation_approved %>% 
  select(-any_of(extra_cols$questions[extra_cols$Tool %in% "wss_checklist" & extra_cols$Sheet %in% "data"]))


# remove extra objects -----------------------------------------------------------------------------
rm(extra_cols)


