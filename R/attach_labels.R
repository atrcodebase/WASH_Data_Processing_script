# apply the value labels ---------------------------------------------------------------------------
## Household Survey
household_dt <- labeler(data = household_dt,
                        tool = hh_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")

## Observation Checklist - HF
hf_checklist <- labeler(data = hf_checklist,
                        tool = hf_obs_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")

## Observation Checklist - School
school_checklist <- labeler(data = school_checklist,
                        tool = school_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")

## Water Supply System Observation
wss_observation <- labeler(data = wss_observation,
                        tool = wss_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")

# remove extra objects -------------------------------------------
rm()

