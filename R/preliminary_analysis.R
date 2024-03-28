###############################################################################
rm(list = ls())

library(tidyverse)
library(srvyr)
library(supporteR) 

source("R/composite_indicators.R")

# packages to install incase needed
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "inputs/clean_data_eth_sca_tigray_somali.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_os$|_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, col_types = c_types, na = "NA") |> 
  create_composite_indicators() 

# tool
df_survey <- readxl::read_excel("inputs/ETH2306a_SCA_Tigray_Somali_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, `label::English`) |> 
  filter(str_detect(string = type, pattern = "decimal|integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_eth_sca_tigray_somali.csv", show_col_types = FALSE)

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data)

# analysis
# Please wait the following lines of code might takes 3/5 minutes
df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap |> 
                                                     filter(!variable %in% c("other_classroom_type",
                                                                             "water_affordability_charge"))
                                                   )|> 
  mutate(level = "KII")

# formatting the analysis, adding question labels
full_analysis_long <- df_main_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(`label::English`, .after = variable) |> 
  mutate(variable = ifelse(variable %in% c(""), str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% c(""), c("decimal", "integer"), select_type),
         `label::English` = ifelse(is.na(`label::English`), variable, `label::English`),
         #`mean/pct` = ifelse(select_type %in% c("decimal", "integer") & !variable %in% c("") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  mutate(variable = ifelse(variable %in% c(""), str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         `label::English` = ifelse(`label::English` %in% c(""), str_replace(string = `label::English`, pattern = "int.", replacement = "i."), `label::English`)) |> 
  select(`Question`= `label::English`, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val,
         select_type,
         level)

# output analysis
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_eth_sca_tigray_somali.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_eth_sca_tigray_somali.csv"), na="")

###############################################################################