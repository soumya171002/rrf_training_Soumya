# Reproducible Research Fundamentals 
# 01. Data processing


data_path <- "C:\\Users\\sagrawal9\\Downloads\\DataWork\\Data"


# Data exploration
View(data)
glimpse(data) ## to view the variables and their type 
n_distinct(data$hhid)
nrow(data)

### Remove duplicates based on hhid
data_dedup <- data %>%
    distinct(hhid, .keep_all = TRUE) # 2 observations are duplicate 
    

### Household (HH) level data ----

#### Tidying data for HH level
data_tidy_hh <- data_dedup %>%
    select(vid, hhid, enid ,floor:n_elder, food_cons:submissionday)

### Data cleaning for Household-member (HH-member) level
data_clean_hh <- data_tidy_hh %>%
    # Convert submissionday to date
    mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    mutate(duration  = as.numeric(duration)) %>%
    # Convert ar_farm_unit to factor (categorical data)
    mutate(ar_unit = as.factor(ar_farm_unit)) %>%
    mutate(ar_unit = na_if(ar_unit, "")) %>% 
    mutate(crop_other = str_to_title(crop_other)) %>% 
    # Replace values in the crop variable based on crop_other using regex for new crops
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut")~ 40,
        str_detect(crop_other, "Sesame")~41,
        TRUE ~ crop
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(where(is.numeric), ~replace(., . == -88,NA))) %>%
    # Add variable labels
    set_variable_labels(
        duration = "Duration",
        submissiondate = "sd",
        ar_unit = "Fram area unit"
    )

# Save the household data
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

### Household member (HH-member) level data ----

#### Tidying data for HH-member level
data_tidy_mem <- data_dedup %>%
    select(vid, hhid, enid, starts_with("gender"), starts_with("age"), starts_with("read"),
           starts_with("clinic_visit"), starts_with("sick"), starts_with("days_sick"),
           starts_with("treat_fin"), starts_with("ill_impact"), starts_with("days_impact"),
           starts_with("treat_cost")) %>%
    pivot_longer(cols = -c(vid, hhid, enid),  # Keep IDs static
                 names_to = c(".value", "member"),
                 names_pattern = "(.*)_(\\d+)")  # Capture the variable and the suffix

### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    filter(!is.na(gender)) %>%
    # Variable labels
    set_variable_labels(
        member = "HH member ID",
        age = "Age of the household member",
        clinic_visit = "number of times visited the clinic",
        days_sick = "number of days in last 4 weeks the member suffered from health problems",
        days_impact = "no. of days not able to perform daily activities due to health reason"
    )

# Save the tidy household-member data
write_dta(data_clean_mem, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = amenity,
                values_from = n,
                names_prefix = "n_")
# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))
