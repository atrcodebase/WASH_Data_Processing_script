# Check Relevancy Rules ----------------------------------------------------------------------------
## Read
hh_tool_relevancy <- read_excel("input/tool_relevancy_rules/Household_relevancy_rules.xlsx")
HF_tool_relevancy <- read_excel("input/tool_relevancy_rules/HF_Obs_relevancy_rules.xlsx")
school_tool_relevancy <- read_excel("input/tool_relevancy_rules/School_Obs_relevancy_rules.xlsx")
wss_tool_relevancy <- read_excel("input/tool_relevancy_rules/WSS_relevancy_rules.xlsx")

### Household Survey
hh_relevancy_issues <- check_relevancy_rules(household_dt, hh_tool_relevancy, sheet_name="data")

### Observation Checklist - HF
hf_relevancy_issues <- check_relevancy_rules(hf_checklist, HF_tool_relevancy, sheet_name="data")

### Observation Checklist - School
school_relevancy_issues <- check_relevancy_rules(school_checklist, school_tool_relevancy, sheet_name="data")

### Water Supply System Observation
wss_relevancy_issues <- check_relevancy_rules(wss_observation, wss_tool_relevancy, sheet_name="data")


# Update Select_multiple series columns ------------------------------------------------------------
### Household Survey
household_dt <- update_series_cols(data=household_dt,
                                 tool_path = hh_tool_path,
                                 question_separator="_",
                                 excluded_col = "DK")
# Problematic column
household_dt <- update_series_cols_1(data=household_dt,
                                      question = "DK",
                                      series_cols = c("DK_1", "DK_3", "DK_4", "DK_5", "DK_6",
                                                      "DK_7", "DK_8", "DK_99", "DK_77"),
                                      question_separator=".")
household_dt <- update_series_cols_1(data=household_dt,
                                     question = "DK",
                                     series_cols = c("DK.2"),
                                     question_separator=".")

# Check if updated correctly
hh_SM_issues <- check_select_multiple(data=household_dt,
                                        tool_path = hh_tool_path,
                                        question_separator="_",
                                      excluded_col = "DK")

### Observation Checklist - HF
hf_checklist <- update_series_cols(data=hf_checklist,
                                 tool_path = hf_obs_tool_path,
                                 question_separator="_")
# Check if updated correctly
hf_SM_issues <- check_select_multiple(data=hf_checklist,
                                        tool_path = hf_obs_tool_path,
                                        question_separator="_")

### Observation Checklist - School
school_checklist <- update_series_cols(data=school_checklist,
                                   tool_path = school_tool_path,
                                   question_separator="_")
# Check if updated correctly
school_SM_issues <- check_select_multiple(data=school_checklist,
                                      tool_path = school_tool_path,
                                      question_separator="_")

### Water Supply System Observation
wss_observation <- update_series_cols(data=wss_observation,
                                       tool_path = wss_tool_path,
                                       question_separator="_")
# Check if updated correctly
wss_SM_issues <- check_select_multiple(data=wss_observation,
                                          tool_path = wss_tool_path,
                                          question_separator="_")


## Export List -------------------------------------------------------------------------------------
# Relevancy
relevancy_issues <- plyr::rbind.fill(
  hh_relevancy_issues %>% mutate(Tool="Household Survey"),
  hf_relevancy_issues %>% mutate(Tool="HF Checklist"),
  school_relevancy_issues %>% mutate(Tool="School Checklist"),
  wss_relevancy_issues %>% mutate(Tool="WSS")
) %>% 
  arrange(Tool, KEY)

## Select Multiple issues
SM_issues <- list(
  hh_SM_issues=hh_SM_issues,
  hf_SM_issues=hf_SM_issues,
  school_SM_issues=school_SM_issues,
  wss_SM_issues=wss_SM_issues
)

# remove extra objects -----------------------------------------------------------------------------
rm(hh_tool_relevancy, HF_tool_relevancy, school_tool_relevancy, wss_tool_relevancy)

