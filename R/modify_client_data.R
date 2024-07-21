# Recode Numeric cols
relabel_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, "9999") ~ "Refused to answer",
    TRUE ~ x
  )}
hf_numeric_cols <- c("estimated_no_of_daily_outpatients_HF", "men_staff_HF", "women_staff_HF", 
                     "men_disabled_HF", "women_disabled_HF", "time_by_walk_min_HF_H", "total_n_staff_HF_M_F",
                     "time_by_walk_min_HF_M", "if_yes_total_number_of_available_drinking_water_points_HF", 
                     "number_of_functional_drinking_water_points_HF", "number_of_dysfunctional_drinking_water_points_HF", 
                     "if_so_total_number_of_available_latrines_HF", "if_not_number_of_functional_latrines_HF", 
                     "total_n_staff_HF_M_F_disabilities", "if_not_number_of_dysfunctional_latrines_HF")

## Observation Checklist - HF
hf_checklist_approved <- hf_checklist_approved %>%
  # mutate(across(hf_numeric_cols, as.numeric)) 
  mutate(across(all_of(hf_numeric_cols), as.character)) %>% 
  mutate(across(all_of(hf_numeric_cols), relabel_98_99))

## Household Survey
household_dt_approved <- household_dt_approved %>%
  mutate(across(all_of("W4"), as.character)) %>% 
  mutate(across(all_of("W4"), function(x) {
    x = case_when(
      x %in% c(99, "99") ~ "I don't know",
      TRUE ~ x
    )
  }))

# Renaming Datasets ----------------------------------------------------------------------
var_map <- read_excel("input/Column Mapping.xlsx")
hh_vars <- var_map %>% filter(form == "Household_Survey")

#Rename
household_dt_labeled <- household_dt_approved %>% 
  # rename(across(hh_vars$xml, function(x) {x = hh_vars$english[hh_vars$xml %in% x]})) %>% 
  rename_at(vars(hh_vars$xml), function(x) {x = hh_vars$english[hh_vars$xml == x]})


# Recode Client data as per their request ----------------------------------------------------------
reshape_tool <- function(tool_path, excluded_cols=""){
  # Read tool
  tool_survey <- read_excel(tool_path, "survey", guess_max = 100000)
  tool_choices <- read_excel(tool_path, "choices", guess_max = 100000)

  # Filter select_one/multiple questions
  tool_survey <- tool_survey %>%
    select(type, Question=name) %>%
    filter(grepl("\\bselect_multiple", type) & Question %notin% excluded_cols) %>%
    mutate(select_type = str_replace_all(type, " .*", ""),
           type = str_replace_all(type, "select_one ", "") %>% str_replace_all("select_multiple ", ""))
  tool_choices <- tool_choices %>%
    filter(!is.na(list_name) & !(list_name %in% label & list_name %in% label)) %>%
    select(list_name, response_code=value, response_label=label) %>%
    mutate(response_label_org=str_squish(response_label),
           response_label=str_squish(response_label) %>% str_replace_all(" ", "_"),
           response_code=as.numeric(response_code)) # New

  # Join questions & responses
  tool_survey <- tool_survey %>%
    left_join(tool_choices, by=c("type"="list_name"), relationship="many-to-many") %>%
    select(-c(type)) %>%
    filter(!is.na(response_code) & !is.na(response_label)) %>%
    mutate(dataset_col= paste0(Question, "_", response_code),
           labeled_col = paste0(Question, "/", response_label))
  # Return
  return(tool_survey)
}

# Apply SM labels 
apply_SM_Label <- function(client_dt, tool_path, excluded_cols=""){
  tool <- reshape_tool(tool_path)
  
  # Get select multiple columns
  sm_cols <- unique(tool$Question)
  
  for(question in sm_cols){
    # Get all series columns
    series_cols <- names(client_dt)[grepl(paste0("^",question, "_", "[0-9]{1,4}$"), names(client_dt))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    # Exclude main columns from 0s/1s
    series_cols <- series_cols[series_cols %notin% excluded_cols]
    
    # Note: this might not apply accurately, using loop to save time
    # names(client_dt)[names(client_dt) %in% series_cols] <- tool$labeled_col[tool$Question %in% question]
    
    for(col in series_cols){
      names(client_dt)[names(client_dt) %in% col] <- tool$labeled_col[tool$dataset_col %in% col]
    }
    
  }
  
  return(client_dt)
}
# ## test
# client_dt <- household_dt_approved; tool_path <- hh_tool_path; question <- "DK";excluded_cols="DK_2"; col="DK_2"

## Relabel WSS Observation
wss_observation_approved <- apply_SM_Label(wss_observation_approved, wss_tool_path)

## Relabel Household Survey
household_dt_approved <- apply_SM_Label(household_dt_approved, hh_tool_path, excluded_cols="DK_2")
names(household_dt_approved)[names(household_dt_approved) %in% "DK.2"] <- "DK/Use_latrines/dispose_faeces_of_children_in_latrines"

## Remove extra objects ----------
rm(reshape_tool, apply_SM_Label, relabel_98_99, hf_numeric_cols)
