###############################################################################
rm(list = ls())

library(tidyverse)
library(openxlsx)

# global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
#options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.withFilter" = FALSE)

# tool
loc_tool <- "inputs/ETH2306a_SCA_Tigray_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  select(list_name, choice_name = name,   choice_label =`label::English`)

# extract select types
df_tool_select_type <- df_survey |> 
  select(type, qn_name = name) |> 
  filter(str_detect(string = type, pattern = "decimal|integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop")

# extract choice ids and labels
df_choices_support <- df_choices |> 
  left_join(df_tool_select_type) |> 
  unite("survey_choice_id", qn_name, choice_name, sep = "_", remove = FALSE) |> 
  select(survey_choice_id, choice_label)  

# extract groups
df_tool_groups <- df_survey |> 
  mutate(int.group = ifelse(str_detect(string = name, pattern = "^grp_"), name, NA_character_),
         i.group = int.group) |> 
  fill(i.group) |> 
  filter(!str_detect(string = name, pattern = "_other$"),
         !str_detect(string = type, pattern = "group|repeat|text|geopoint|^gps$|^note$"),
         !is.na(i.group)) |> 
  select(type, name, `label::English`, i.group, in_number)

# support composite grps and labels
#df_support_composite_grps <- readxl::read_excel("support_files/support composite grps and labels.xlsx")
#df_support_integer_col_labs <- readxl::read_excel("support_files/support integer column names.xlsx") |> 
#  filter(!is.na(integer_column_label))

# analysis
df_analysis <- read_csv("outputs/full_analysis_eth_sca_tigray_somali.csv") |> 
  mutate(analysis_choice_id = case_when(select_type %in% c("select_multiple", "select multiple") ~ str_replace(string = `choices/options`, 
                                                                                                               pattern = "\\/", replacement = "_"),
                                        select_type %in% c("select_one", "select one") ~ paste0(variable, "_", `choices/options`)))

# identify indicators ---------------------------------------------------------
df_dap_questions <- readxl::read_excel("support_files/ETH2306a_SCA_DAP.xlsx", sheet = "ETH2306a_DAP") |> 
  filter(!is.na(`Indicator / Variable`)) |> 
  janitor::clean_names() |> 
  select(in_number, indicator_group_sector, indicator_variable, questionnaire_question) |> 
  mutate(indicator_group_sector = str_replace_all(string = indicator_group_sector, pattern = "\\/|\\?", replacement = "_"),
         indicator_group_sector = case_when(indicator_group_sector %in% c("Key characteristics", "Key Informant profile") ~ "Respondent Information",
                                            indicator_group_sector %in% c("Education Facility") ~ "Education Facility",
                                            indicator_group_sector %in% c("Health Facility") ~ "Health Facility",
                                            indicator_group_sector %in% c("Water point Facility") ~ "Water points Facility",
                                            TRUE ~ indicator_group_sector))

df_tool_dap_info <- df_tool_groups |> 
  left_join(df_dap_questions) |> 
  mutate(qn_number = row_number())

# format the data -------------------------------------------------------------
df_analysis_dap_info <- df_analysis |> 
  left_join(df_tool_dap_info |> 
              select(name, indicator_group_sector, indicator_variable, qn_number), by = c("variable" = "name")) |> 
  mutate(response_lablel = recode(analysis_choice_id, !!!setNames(df_choices_support$choice_label, df_choices_support$survey_choice_id)),
         choices = ifelse(is.na(response_lablel), `choices/options`, response_lablel),
         subset_1_val_label = recode(subset_1_val, !!!setNames(df_choices$choice_label, df_choices$choice_name)),
         subset_1_val_label =  ifelse(is.na(subset_1_val_label), "Region", subset_1_val_label),
         indicator_group_sector = case_when(indicator_group_sector %in% c("Key characteristics", "Key Informant profile") ~ "Respondent Information",
                                            indicator_group_sector %in% c("Education Facility") ~ "Education Facility",
                                            indicator_group_sector %in% c("Health Facility") ~ "Health Facility",
                                            indicator_group_sector %in% c("Water point Facility") ~ "Water points Facility",
                                            TRUE ~ indicator_group_sector)) |> 
  select(-c(n_unweighted, subset_1_name, subset_1_val)) |> 
  filter(!is.na(indicator_group_sector))

# split data based on groups or sectors
output <- split(df_analysis_dap_info, df_analysis_dap_info$indicator_group_sector)

wb <- createWorkbook()

hs1 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", fontColour = "white", fontSize = 14, wrapText = T)
hs2 <- createStyle(fgFill = "#808080", halign = "CENTER", textDecoration = "Bold", fontColour = "white", wrapText = T)
hs3 <- createStyle(fgFill = "#EE5859", halign = "CENTER", textDecoration = "Bold", border = "Bottom", fontColour = "white")

# numbers
number_2digit_style <- openxlsx::createStyle(numFmt = "0.00")
number_1digit_style <- openxlsx::createStyle(numFmt = "0.0")
number_style <- openxlsx::createStyle(numFmt = "0")

cols_for_special_formatting <- c("Region", "Tigray", "Somali")

for (i in 1:length(output)) {
  addWorksheet(wb, sheetName=names(output[i]))
  
  # add header to sheet
  mergeCells(wb, sheet = names(output[i]), rows = 1, cols = 1:5)
  writeData(wb, sheet = names(output[i]), names(output[i]), startCol = 1, startRow = 1, headerStyle = hs1)
  addStyle(wb, sheet = names(output[i]), hs1, rows = 1, cols = 1:5, gridExpand = TRUE)
  
  setColWidths(wb = wb, sheet = names(output[i]), cols = 2, widths = 90)
  
  # get current data for the group or sector
  current_sheet_data <- output[[i]] |> 
    pivot_wider(names_from = subset_1_val_label, values_from = `Results(mean/percentage)`) |> 
    arrange(qn_number) |> 
    mutate(row_id = row_number())
  
  # split variables to be written in different tables with in a sheet
  sheet_variables_data <- split(current_sheet_data, factor(current_sheet_data$variable, levels = unique(current_sheet_data$variable)))
  
  previous_row_end <- 2
  
  for (j in 1:length(sheet_variables_data)) {
    
    current_variable_data <- sheet_variables_data[[j]]
    
    get_question <- current_variable_data |> select(Question) |> unique() |> pull()
    get_qn_type <- current_variable_data |> select(select_type) |> unique() |> pull()
    
    if(get_qn_type %in% c("select_one", "select one", "select_multiple", "select multiple")){
      class(current_variable_data$Region) <- "percentage"
      class(current_variable_data$Tigray) <- "percentage"
      class(current_variable_data$Somali) <- "percentage"
    }else{
      class(current_variable_data$Region) <- "numeric"
      class(current_variable_data$Tigray) <- "numeric"
      class(current_variable_data$Somali) <- "numeric"
    }
    
    current_row_start <- previous_row_end + 3
    
    print(current_row_start)
    
    # add header for variable
    mergeCells(wb, sheet = names(output[i]), rows = previous_row_end + 2, cols = 1:5)
    writeData(wb, sheet = names(output[i]), get_question, startCol = 1, startRow = previous_row_end + 2)
    addStyle(wb, sheet = names(output[i]), hs2, rows = previous_row_end + 2, cols = 1:5, gridExpand = TRUE)
    
    current_data_length <- max(current_variable_data$row_id) - min(current_variable_data$row_id)
    
    addStyle(wb, sheet = names(output[i]), number_1digit_style, rows = current_row_start + 1 : current_row_start + 1 + current_data_length, cols = 1:6, gridExpand = TRUE)
    
    writeDataTable(wb = wb, 
                   sheet = names(output[i]), 
                   x = current_variable_data |> 
                     select(-c(Question, `choices/options`, 
                               population, analysis_choice_id, 
                               indicator_group_sector,response_lablel,
                               row_id, variable, select_type, indicator_variable, qn_number)), 
                   startRow = current_row_start, 
                   startCol = 1, 
                   tableStyle = "TableStyleLight9", 
                   headerStyle = hs3)
    
    previous_row_end <- current_row_start + 1 + current_data_length
  }
  # hide grid lines
  showGridLines(wb,  names(output[i]), showGridLines = FALSE)  
}

# worksheets order
worksheetOrder(wb) <- c(3, 1, 2, 4)

activeSheet(wb) <- 1

saveWorkbook(wb, paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_sca_tigray_somali.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),"_formatted_analysis_eth_sca_tigray_somali.xlsx"))

###############################################################################