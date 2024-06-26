### Filter Approved data for client
## Household Survey
household_dt_approved <- household_dt %>% filter(qa_status %in% approved_qa_status)

## Observation Checklist - HF
hf_checklist_approved <- hf_checklist %>% filter(qa_status %in% approved_qa_status)

## Observation Checklist - School
school_checklist_approved <- school_checklist %>% filter(qa_status %in% approved_qa_status)

## Water Supply System Observation
wss_observation_approved <- wss_observation %>% filter(qa_status %in% approved_qa_status)

## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

