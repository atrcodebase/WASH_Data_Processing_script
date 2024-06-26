### Read Data 
guess_max <- 5000000
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA

# Household Survey
household_dt <- read_excel("input/raw_data/UNICEF WASH - Household Survey Tool.xlsx", sheet = "data", guess_max = guess_max, na = convert_to_na)

# Observation Checklist - HF
hf_checklist <- read_excel("input/raw_data/UNICEF Wash - Observation Checklist - HF.xlsx", sheet = "data", guess_max = guess_max, na = convert_to_na)

# Observation Checklist - School
school_checklist <- read_excel("input/raw_data/UNICEF Wash - Observation Checklist - School.xlsx", sheet = "data", guess_max = guess_max, na = convert_to_na)

# Water Supply System Observation
wss_observation <- read_excel("input/raw_data/UNICEF Wash - WSS Observation.xlsx", sheet = "data", guess_max = guess_max, na = convert_to_na)


# Remove Extra Objects -----------------------------------------------------------------------------
rm()
