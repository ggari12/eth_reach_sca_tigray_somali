###############################################################################
# checks for data collection
# read packages
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

source("R/composite_indicators.R")

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
                        input_location_col = "woreda") |> 
  create_composite_indicators() |> 
  rowwise() |> 
  mutate(int.total_enrolled_boys = sum(c_across(c("pre_school_enrolled_boys", "primary_school_enrolled_boys", "post_primary_school_enrolled_boys", "middle_school_enrolled_boys", "high_school_enrolled_boys", "preparatory_school_enrolled_boys")), na.rm = T),
         int.total_enrolled_girls = sum(c_across(c("pre_school_enrolled_girls", "primary_school_enrolled_girls", "post_primary_school_enrolled_girls", "middle_school_enrolled_girls", "high_school_enrolled_girls", "preparatory_school_enrolled_girls")), na.rm = T),        
         #int.total_enrolled_children = sum(c_across(c("pre_school_enrolled_total", "primary_school_enrolled_total", "post_primary_school_enrolled_total", "middle_school_enrolled_total", "high_school_enrolled_total", "preparatory_school_enrolled_total")), na.rm = T)
  )|>
  ungroup()

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
min_time_of_survey <- 7
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
# number of boys with disabilities should be not greater than total enrolled boys
df_logic_c_boys_with_disabilities_not_grthan_enrolled_boys <- df_tool_data |> 
  filter(boys_with_disabilities > int.total_enrolled_boys) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "boys_with_disabilities",
         i.check.current_value = as.character(boys_with_disabilities),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_boys_with_disabilities_not_grthan_enrolled_boys",
         i.check.issue = glue("boys_with_disabilities_not_grthan_enrolled_boys, boys_with_disabilities: {boys_with_disabilities} but int.total_enrolled_boys: {int.total_enrolled_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_boys_with_disabilities_not_grthan_enrolled_boys")

# number of girls with disabilities should be not greater than total enrolled girls
df_logic_c_girls_with_disabilities_not_grthan_enrolled_girls <- df_tool_data |> 
  filter(girls_with_disabilities > int.total_enrolled_girls) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "girls_with_disabilities",
         i.check.current_value = as.character(girls_with_disabilities),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_girls_with_disabilities_not_grthan_enrolled_girls",
         i.check.issue = glue("girls_with_disabilities_not_grthan_enrolled_girls, girls_with_disabilities: {girls_with_disabilities} but int.total_enrolled_girls: {int.total_enrolled_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_girls_with_disabilities_not_grthan_enrolled_girls")

# number of boys dropouts should be not greater than total enrolled boys
df_logic_c_boys_dropped_out_not_grthan_enrolled_boys <- df_tool_data |> 
  filter(boys_dropped_out > int.total_enrolled_boys) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "boys_dropped_out",
         i.check.current_value = as.character(boys_dropped_out),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_boys_dropped_out_not_grthan_enrolled_boys",
         i.check.issue = glue("boys_dropped_out_not_grthan_enrolled_boys, boys_dropped_out: {boys_dropped_out} but int.total_enrolled_boys: {int.total_enrolled_boys}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_boys_dropped_out_not_grthan_enrolled_boys")

# number of girls dropouts should be not greater than total enrolled girls
df_logic_c_girls_dropped_out_not_grthan_enrolled_girls <- df_tool_data |> 
  filter(girls_dropped_out > int.total_enrolled_girls) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "girls_dropped_out",
         i.check.current_value = as.character(girls_dropped_out),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_girls_dropped_out_not_grthan_enrolled_girls",
         i.check.issue = glue("girls_dropped_out_not_grthan_enrolled_girls, girls_dropped_out: {girls_dropped_out} but int.total_enrolled_girls: {int.total_enrolled_girls}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_girls_dropped_out_not_grthan_enrolled_girls")

# number of idp enrolled children should be not greater than total enrolled children
df_logic_c_idp_enrolled_children_not_grthan_total_enrolled_children <- df_tool_data |> 
  filter(idp_enrolled_children > int.total_enrolled_children) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "idp_enrolled_children",
         i.check.current_value = as.character(idp_enrolled_children),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_idp_enrolled_children_not_grthan_enrolled_girls",
         i.check.issue = glue("idp_enrolled_children_not_grthan_total_enrolled_children, idp_enrolled_children: {idp_enrolled_children} but int.total_enrolled_children: {int.total_enrolled_children}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_idp_enrolled_children_not_grthan_total_enrolled_children")

# number of students attending shool now should be not greater than total enrolled children
df_logic_c_attending_shool_now_not_grthan_total_enrolled_children <- df_tool_data |> 
  filter(attending_shool_now > int.total_enrolled_children) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "attending_shool_now",
         i.check.current_value = as.character(attending_shool_now),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_attending_shool_now_not_grthan_enrolled_girls",
         i.check.issue = glue("attending_shool_now_not_grthan_total_enrolled_children, attending_shool_now: {attending_shool_now} but int.total_enrolled_children: {int.total_enrolled_children}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_attending_shool_now_not_grthan_total_enrolled_children")

# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_sca_tigray_somali.csv"), na = "")

###############################################################################

