### Remove Rejected QA status and keys -------------------------------------------------------------
rejected_qa_status <- "Rejected"
scto_rejected <- "REJECTED"

## Household Survey
household_dt <- household_dt %>% 
  filter(qa_status %notin% rejected_qa_status &
           # KEY %notin% rejection_log$KEY_Unique & 
           review_status %notin% scto_rejected)

## Observation Checklist - HF
hf_checklist <- hf_checklist %>% 
  filter(qa_status %notin% rejected_qa_status &
           # KEY %notin% rejection_log$KEY_Unique & 
           review_status %notin% scto_rejected)

## Observation Checklist - School
school_checklist <- school_checklist %>% 
  filter(qa_status %notin% rejected_qa_status &
           # KEY %notin% rejection_log$KEY_Unique & 
           review_status %notin% scto_rejected)

## Water Supply System Observation
wss_observation <- wss_observation %>% 
  filter(qa_status %notin% rejected_qa_status &
           # KEY %notin% rejection_log$KEY_Unique & 
           review_status %notin% scto_rejected)

## Remove extra objects ----------------------------------------------------------------------------
rm(rejected_qa_status, scto_rejected)

