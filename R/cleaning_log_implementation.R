################################################################################
# Applying the cleaning log to clean the data

rm(list = ls())
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

source("R/composite_indicators.R")

# Read data and checking log 

df_cleaning_log <- read_csv("inputs/combined_checks_eth_sca_tigray_somali_GG.csv", show_col_types = FALSE) |> 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
         sheet = NA,
         index = NA,
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
data_path <- "inputs/ETH2306a_SCA_Tigray_Somali_data.xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime", "endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text", TRUE ~ "guess")

df_raw_data <- readxl::read_excel(path = data_path, col_types = c_types, na = c("NA", "N/A", "n/a"))

# tool
loc_tool <- "inputs/ETH2306a_SCA_Tigray_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

vars_to_remove_from_data = c("deviceid", "audit", "audit_URL", "instance_name", "ki_phone_no", "gps", 
                             "_gps_latitude", "_gps_longitude", "_gps_altitude", "_gps_precision") 

# main dataset ----------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log |> 
  filter(is.na(sheet))

df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data,
                                                input_df_survey = df_survey,
                                                input_df_choices = df_choices,
                                                input_df_cleaning_log = df_cleaning_log_main, 
                                                input_vars_to_remove_from_data = vars_to_remove_from_data) 

df_cleaned_data <- df_cleaning_step 

# Add composite indicators at this stage ----------------------------------



# write final datasets out -----------------------------------------------

df_raw_data_final <- df_raw_data |> 
  mutate(across(.cols = any_of(vars_to_remove_from_data), .fns = ~na_if(., .)))

openxlsx::write.xlsx(x = df_raw_data_final,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_raw_data_eth_sca_tigray_somali.xlsx"))

openxlsx::write.xlsx(x = df_cleaned_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_eth_sca_tigray_somali.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

################################################################################
