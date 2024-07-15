# Change/Recode variables
remove_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, 19998, "9999", "19998") ~ "I don't know",
    TRUE ~ x
  )}

download_link <- "https://atrconsultingaf.surveycto.com/view/submission-attachment/"
  
## Household Survey --------------------------------------------------------------------------------
household_dt <- household_dt %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
  Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))
  # SubmissionDate= openxlsx::convertToDateTime(SubmissionDate))

# Update links
household_dt <- update_media_links(data=household_dt, 
                                   tool_path = hh_tool_path, 
                                   download_link=download_link) # No need if data is downloaded from SCTO website

## Observation Checklist - HF  ---------------------------------------------------------------------
hf_checklist <- hf_checklist %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S")) 

# Update links
hf_checklist <- update_media_links(data=hf_checklist, 
                                   tool_path = hf_obs_tool_path,
                                   download_link=download_link) # No need if data is downloaded from SCTO website

## Observation Checklist - School  -----------------------------------------------------------------
school_checklist <- school_checklist %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))

# Update links
school_checklist <- update_media_links(data=school_checklist, 
                                       tool_path = school_tool_path,
                                       download_link=download_link) # No need if data is downloaded from SCTO website

## Water Supply System Observation  ----------------------------------------------------------------
wss_observation <- wss_observation %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
         Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"))

# Update links
wss_observation <- update_media_links(data=wss_observation,
                                      tool_path = wss_tool_path,
                                      download_link=download_link) # No need if data is downloaded from SCTO website


# remove extra objects -----------------------------------------------------------------------------
rm()

