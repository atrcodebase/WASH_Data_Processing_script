# Recode Numeric cols
relabel_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, "9999") ~ "Refused to answer",
    TRUE ~ x
  )}
hf_numeric_cols <- c("estimated_no_of_daily_outpatients_HF", "men_staff_HF", "women_staff_HF", 
                     "men_disabled_HF", "women_disabled_HF", "time_by_walk_min_HF_H", 
                     "time_by_walk_min_HF_M", "if_yes_total_number_of_available_drinking_water_points_HF", 
                     "number_of_functional_drinking_water_points_HF", "number_of_dysfunctional_drinking_water_points_HF", 
                     "if_so_total_number_of_available_latrines_HF", "if_not_number_of_functional_latrines_HF", 
                     "if_not_number_of_dysfunctional_latrines_HF")

## Observation Checklist - HF
hf_checklist_approved <- hf_checklist_approved %>%
  # mutate(across(hf_numeric_cols, as.numeric)) 
  mutate(across(all_of(hf_numeric_cols), as.character)) %>% 
  mutate(across(all_of(hf_numeric_cols), relabel_98_99))

# # Recode Client data as per their request ----------------------------------------------------------
# reshape_tool <- function(tool_path, excluded_cols=""){
#   # Read tool
#   tool_survey <- read_excel(tool_path, "survey", guess_max = 100000)
#   tool_choices <- read_excel(tool_path, "choices", guess_max = 100000)
#   
#   # Filter select_one/multiple questions
#   tool_survey <- tool_survey %>% 
#     select(type, Question=name) %>% 
#     filter(grepl("\\bselect_multiple", type) & Question %notin% excluded_cols) %>% 
#     mutate(select_type = str_replace_all(type, " .*", ""),
#            type = str_replace_all(type, "select_one ", "") %>% str_replace_all("select_multiple ", "")) 
#   tool_choices <- tool_choices %>% 
#     filter(!is.na(list_name) & !(list_name %in% label & list_name %in% label)) %>% 
#     select(list_name, response_code=value, response_label=label) %>% 
#     mutate(response_label=str_squish(response_label) %>% str_replace_all(" ", "_"),
#            response_code=as.numeric(response_code)) # New
#   
#   # Join questions & responses
#   tool_survey <- tool_survey %>% 
#     left_join(tool_choices, by=c("type"="list_name"), relationship="many-to-many") %>% 
#     select(-c(type)) %>% 
#     filter(!is.na(response_code) & !is.na(response_label)) %>% 
#     mutate(dataset_col= paste0(Question, "_", response_code),
#            labeled_col = paste0(Question, "/", response_label))
#   # Return
#   return(tool_survey)
# }
# 
# wss_tool <- reshape_tool(wss_tool_path)
# 
# # Update columns
# sm_cols <- unique(wss_tool$Question)
# 
# for(question in sm_cols){
#   # Get all series columns
#   series_cols <- names(wss_observation_approved)[grepl(paste0("^",question, "_", "[0-9]{1,4}$"), names(wss_observation_approved))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
#   
#   # Note: the order of the new names might be different, find a new way
#   names(wss_observation_approved)[names(wss_observation_approved) %in% series_cols] <- wss_tool$labeled_col[wss_tool$Question %in% question]
#   
# }