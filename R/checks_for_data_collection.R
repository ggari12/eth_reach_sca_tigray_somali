###############################################################################
# checks for data collection
# read packages
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/ETH2306a_SCA_Tigray_Somali_data.xlsx"

df_tool_data <- readxl::read_excel(data_path) |>  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         `_geopoint_latitude` = as.numeric(`_gps_latitude`),
         `_geopoint_longitude` = as.numeric(`_gps_longitude`),
         enumerator_id = ifelse(is.na(enumerator_id), enum_id, enumerator_id)) |> 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "woreda") 

# tool
loc_tool <- "inputs/ETH2306a_SCA_Tigray_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_sample_data <- sf::st_read("inputs/sca_tigray_somali_facilities_samples.gpkg", quiet = TRUE)

# checks ------------------------------------------------------------------

checks_output <- list()

# testing data ------------------------------------------------------------

df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2024-02-21")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 15
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "woreda",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers ----------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "woreda")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify -----------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data, 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "woreda",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# spatial checks ----------------------------------------------------------

if("status" %in% colnames(df_sample_data)){
sample_pt_nos <- df_sample_data %>% 
         mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
         pull(unique_pt_number) %>% 
         unique()
 }else{
     sample_pt_nos <- df_sample_data %>% 
         mutate(unique_pt_number = Name) %>% 
         pull(unique_pt_number) %>% 
         unique()
 }
 
# duplicate point numbers
df_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data,
                                                   input_enumerator_id_col = "enumerator_id",
                                                   input_location_col = "woreda",
                                                   input_point_id_col = "point_number",
                                                   input_sample_pt_nos_list = sample_pt_nos)
 
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_duplicate_pt_nos")

# point number does not exist in sample
df_pt_number_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data,
                                                              input_enumerator_id_col = "enumerator_id",
                                                              input_location_col = "woreda",
                                                              input_point_id_col = "point_number",
                                                              input_sample_pt_nos_list = sample_pt_nos)
 
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_pt_number_not_in_sample")

# check for exceeded threshold distance
df_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data,
                                                        input_tool_data = df_tool_data, 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "woreda",
                                                        input_point_id_col = "point_number",
                                                        input_threshold_dist = 150)
 
add_checks_data_to_list(input_list_name = "checks", input_df_name = "df_greater_thresh_distance")

# logical checks --------------------------------------------------------------


# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_sca_tigray_somali.csv"), na = "")

###############################################################################

