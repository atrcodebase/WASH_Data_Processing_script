## Check for any values in the dataset that cannot be found in the tool ---------------------------- 
## Household Survey
household_response_log <- check_responses(data=household_dt_approved, tool_path=hh_tool_path, sheet="data")

## Observation Checklist - HF
hf_response_log <- check_responses(data=hf_checklist_approved, tool_path=hf_obs_tool_path, sheet="data")

## Observation Checklist - School
school_response_log <- check_responses(data=school_checklist_approved, tool_path=school_tool_path, sheet="data")

## Water Supply System Observation
wss_response_log <- check_responses(data=wss_observation_approved, tool_path=wss_tool_path, sheet="data")


# Export List
response_log_list <- rbind(
  "Household Survey"=household_response_log,
  "HF Checklist"=hf_response_log,
  "School Checklist"=school_response_log,
  "WSS Observation"=wss_response_log
  )

