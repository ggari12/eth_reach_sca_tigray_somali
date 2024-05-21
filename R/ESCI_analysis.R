###############################################################################
## Service Capability Assessment, Phase II Scoring Index (ESCI)
## Author: Getu GARI - getu.gari@reach-initiatives.org
## Date: 18/05/2024

rm(list=ls())
library(tidyverse)
library(here)

#import clean data
data <- read_csv('inputs/clean_data_eth_sca_tigray_somali.csv') %>%  
  rename('adm1' = 'region', 
         'adm2' = 'zone', 
         'adm3' = 'woreda')

###################################################

#### QUALITY #### -----------------------------------------

#Quality.1
# Teachers to student ratio. This indicates number of teachers available with regards to number of students

ESCI_quality1 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(teacher_student_ratio = teachers_total / rowSums(.[c("pre_school_enrolled_total",
                                                              "primary_school_enrolled_total",
                                                              "post_primary_school_enrolled_total",
                                                              "middle_school_enrolled_total",
                                                              "high_school_enrolled_total",
                                                              "preparatory_school_enrolled_total")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(teacher_student_ratio = round(sum(teacher_student_ratio)/n()*45, 1)) %>%
  mutate(teacher_student_qual_score = teacher_student_ratio) 

#Quality.2
#calculate '% of dropout in the current year'

ESCI_quality2 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(dropout_rate = (boys_dropped_out + girls_dropped_out) / rowSums(.[c("pre_school_enrolled_total",
                                                              "primary_school_enrolled_total",
                                                              "post_primary_school_enrolled_total",
                                                              "middle_school_enrolled_total",
                                                              "high_school_enrolled_total",
                                                              "preparatory_school_enrolled_total")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(dropout_rate = round(sum(dropout_rate)/n()*1.01, 1)) %>%
  mutate(dropout_rate_qual_score = dropout_rate)

#Quality.3
#Availability of adequate learning materials

ESCI_quality3 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(availability_quality_score = case_when(child_with_adequate_material == "none" | child_with_adequate_material == "not_sure" ~ 0,
                                        child_with_adequate_material == "around_a_quarter" ~ 0.25,
                                        child_with_adequate_material == "around_half" ~ 0.5,
                                        child_with_adequate_material == "around_three_quarters" ~ 0.75,
                                        child_with_adequate_material == "all" ~ 1, TRUE ~ NA_real_  # Keep other values as NA
                                     )) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(availability_quality_score = round(sum(availability_quality_score)/n(), 1)) %>%
  mutate(availability_qual_score = availability_quality_score)

#Quality.4
#calculate % "number of students attending school on the day of data collection"

ESCI_quality4 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(school_attendance_ratio = attending_shool_now / rowSums(.[c("pre_school_enrolled_total",
                                                              "primary_school_enrolled_total",
                                                              "post_primary_school_enrolled_total",
                                                              "middle_school_enrolled_total",
                                                              "high_school_enrolled_total",
                                                              "preparatory_school_enrolled_total")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(school_attendance_ratio = round(sum(school_attendance_ratio)/n(), 1)) %>%
  mutate(school_attendance_qual_score = school_attendance_ratio)  # take the ratio as final score

#Quality.5
# Classroom to student ratio. This indicates a ratio of number of students in one classroom

ESCI_quality5 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(classroom_student_ratio = classroom_totala_functional / rowSums(.[c("pre_school_enrolled_total",
                                                              "primary_school_enrolled_total",
                                                              "post_primary_school_enrolled_total",
                                                              "middle_school_enrolled_total",
                                                              "high_school_enrolled_total",
                                                              "preparatory_school_enrolled_total")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(classroom_student_ratio = round(sum(classroom_student_ratio)/n()*45, 1)) %>%
  mutate(classroom_student_qual_score = classroom_student_ratio) 


#### SAFTEY #### -------------------------------------

#Safety.1
#Presence of risks to children in the school area

ESCI_safety1 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(safety_in_school_safety_score = case_when(child_saftey_risk == "yes" ~ 0,
                                            child_saftey_risk == "no" | child_saftey_risk == "dk" ~ 1
                                          )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(safety_in_school_safety_score = round(sum(safety_in_school_safety_score)/n(), 1)) %>%
  mutate(safety_in_school_safety_score = safety_in_school_safety_score)

#Safety.2
#Presence of risks to children in the school

ESCI_safety2 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(safety_in_school2_safety_score = case_when(child_saftey_risk_school == "yes" ~ 0,
                                             child_saftey_risk_school == "no" | child_saftey_risk == "dk" ~ 1
                                          )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(safety_in_school2_safety_score = round(sum(safety_in_school2_safety_score)/n(), 1)) %>%
  mutate(safety_in_school2_safety_score = safety_in_school2_safety_score)

#### FACILITY/ INFRASTRUCTURE #### -------------------------------------

#Facility.1
#calculate % availability of basicc drinking water services

ESCI_infra_facilities1 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(drinking_water_infra_facil_score = case_when(school_drinking_water_source  == "piped_water_supply" | school_drinking_water_source == "protected_well" |school_drinking_water_source  == "harvested_rainwater" | school_drinking_water_source == "packaged_bottled_water" | school_drinking_water_source  == "truck_or_cart" & school_drinking_water_source_availability == "yes" ~ 1,
                                                      school_drinking_water_source  == "piped_water_supply" | school_drinking_water_source == "protected_well" |school_drinking_water_source  == "harvested_rainwater" | school_drinking_water_source == "packaged_bottled_water" | school_drinking_water_source  == "truck_or_cart" & school_drinking_water_source_availability == "no" ~ 0.5,
                                                      school_drinking_water_source  == "unprotected_well" | school_drinking_water_source == "surface_water" |school_drinking_water_source  == "borehole" & school_drinking_water_source_availability == "no" ~ 0,
                                                  TRUE ~ NA_real_  # Keep other values as NA
                                                )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(drinking_water_infra_facil_score = round(sum(drinking_water_infra_facil_score, na.rm = T)/n(), 1)) %>%
  mutate(drinking_water_infra_facil_score = drinking_water_infra_facil_score)


#Facility.2
#calculate % availability of basic sanitation facilities

ESCI_infra_facilities2 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(availability_of_latrines_infra_facil_score = case_when(school_latrines_type == "flush_pourflush_toilets" | school_latrines_type == "pit_latrines_with_slab" | school_latrines_type == "composting_toilets" ~ 1,
                                                                school_latrines_type == "pit_latrines_without_slab" | school_latrines_type == "hanging_latrines" | school_latrines_type == "bucket_latrines" | school_latrines_type == "none" ~ 0, 
                                                    TRUE ~ NA_real_  # Keep other values as NA
                                                 ))  %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(availability_of_latrines_infra_facil_score = round(sum(availability_of_latrines_infra_facil_score)/n(), 1)) %>%
  mutate(availability_of_latrines_infra_facil_score = availability_of_latrines_infra_facil_score)

#Facility.3
#calculate % functionality of latrines

ESCI_infra_facilities3 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(functionality_of_latrines_ratio = school_latrines_usable / rowSums(.[c("pre_school_enrolled_total",
                                                                             "primary_school_enrolled_total",
                                                                             "post_primary_school_enrolled_total",
                                                                             "middle_school_enrolled_total",
                                                                             "high_school_enrolled_total",
                                                                             "preparatory_school_enrolled_total")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(functionality_of_latrines_ratio = round(sum(functionality_of_latrines_ratio, na.rm = T)/n()*45, 1)) %>%
  mutate(functionality_of_latrines_infra_facil_score = functionality_of_latrines_ratio) 

#Facility.4
#calculate % availabily of separate or Privacy (Gender) availabily of latrines

ESCI_infra_facilities4 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(availability_of_separate_latrines_infra_facil_score = case_when(school_latrines_separate == "yes" ~ 1,
                                                                         school_latrines_separate == "no" ~ 0
                                                                     )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(availability_of_separate_latrines_infra_facil_score = round(sum(availability_of_separate_latrines_infra_facil_score, na.rm = T)/n(), 1)) %>%
  mutate(availability_of_separate_latrines_infra_facil_score = availability_of_separate_latrines_infra_facil_score)

#Facility.5
#calculate % availability of handwashing facility

ESCI_infra_facilities5 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(availability_of_handwashing_facility_infra_facil_score = case_when(school_functional_handwash == "yes" ~ 1,
                                                                            school_functional_handwash == "no" | school_functional_handwash == "dk" ~ 0
                                                                        )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(availability_of_handwashing_facility_infra_facil_score = round(sum(availability_of_handwashing_facility_infra_facil_score)/n(), 1)) %>%
  mutate(availability_of_handwashing_facility_infra_facil_score = availability_of_handwashing_facility_infra_facil_score)

#Facility.6
#calculate % availability of internet for educational purpose

ESCI_infra_facilities6 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(internet_access_infra_facil_score = case_when(school_internet == "yes" ~ 0.5,
                                                       school_internet == "no" | school_internet == "dk" ~ 0
                                                    )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(internet_access_infra_facil_score = round(sum(internet_access_infra_facil_score)/n(), 1)) %>%
  mutate(internet_access_infra_facil_score = internet_access_infra_facil_score)

#Facility.7
#calculate % how many classrooms are functional with regard to the available classrooms

ESCI_infra_facilities7 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(functionality_of_classrooms_ratio = classroom_totala_functional / rowSums(.[c("permanent_classroom_type",
                                                                                "semi_permanent_classroom_type",
                                                                                "tents_classroom_type",
                                                                                "other_classroom_type",
                                                                                "classroom_universal_num")], na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(functionality_of_classrooms_ratio = round(sum(functionality_of_classrooms_ratio, na.rm = T)/n(), 1)) %>%
  mutate(functionality_of_classrooms_infra_facil_score = functionality_of_classrooms_ratio) # take the ratio as final score 
 
#ESCI_functionality_of_classrooms2 <- data %>%
#  filter(facility_assessed == "education") %>%
#  mutate(functionality_of_classrooms2_ratio = classroom_totala_functional / rowSums(.[c("classroom_totala_functional",
#                                                                                       "classroom_total_nonfunctional")], na.rm = TRUE)) %>%
#  group_by(adm1, adm2, adm3) %>% 
# mutate(functionality_of_classrooms2_score = round(functionality_of_classrooms2_ratio, 2)) 

#Facility.8
#calculate % availability of computer for educational purpose

ESCI_infra_facilities8 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(computer_infra_facil_score = case_when(school_computer == "yes" ~ 0.5,
                                                school_computer == "no" | school_computer == "dk" ~ 0
                                            )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(computer_infra_facil_score = round(sum(computer_infra_facil_score)/n(), 1)) %>%
  mutate(computer_infra_facil_score = computer_infra_facil_score)

#Facility.9
#calculate % availability of electricity

ESCI_infra_facilities9 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(electricity_infra_facil_score = case_when(school_electricity == "yes" ~ 1,
                                                        school_electricity == "no" | school_electricity == "dk" ~ 0
                                                     )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(electricity_infra_facil_score = round(sum(electricity_infra_facil_score)/n(), 1)) %>%
  mutate(electricity_infra_facil_score = electricity_infra_facil_score)

#### Inclusivity #### -------------------------------------

#Inclusivity.1
#calculate % availability of service for special needs

ESCI_inclusivity1 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(service_for_special_needs_incl_score = case_when(psychosocial_support_programs == "yes" ~ 1,
                                                                 psychosocial_support_programs == "no" | psychosocial_support_programs == "dk" ~ 0
                                                            )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(service_for_special_needs_incl_score = round(sum(service_for_special_needs_incl_score)/n(), 1)) %>%
  mutate(service_for_special_needs_incl_score = service_for_special_needs_incl_score)

#Inclusivity.2
#calculate % accessibility of classrooms to disabled

ESCI_inclusivity2 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(accessibility_of_classrooms_to_disabled_ratio = (classroom_universal_num) / (classroom_totala_functional), na.rm = TRUE) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(accessibility_of_classrooms_to_disabled_ratio = round(sum(accessibility_of_classrooms_to_disabled_ratio, na.rm = T)/n(), 1)) %>%
  mutate(accessibility_of_classrooms_to_disabled_incl_score = accessibility_of_classrooms_to_disabled_ratio) # take the ratio as final score  

#### RESILIENCE  #### -------------------------------------

#Resilience.1
#calculate % the capacity of the service to withstand shocks

ESCI_resilience1 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(resilience_to_shock_resil_score = case_when(school_inturpted == "yes" ~ 0,
                                                     school_inturpted == "no" | school_inturpted == "dk" ~ 1
                                                  )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(resilience_to_shock_resil_score = round(sum(resilience_to_shock_resil_score)/n(), 1)) %>%
  mutate(resilience_to_shock_resil_score = resilience_to_shock_resil_score)

#Resilience.2
#calculate % the capacity of the service to withstand shocks

ESCI_resilience2 <- data %>%
  filter(facility_assessed == "education") %>% 
  mutate(resilience_to_shock2_resil_score = case_when(school_facility_damaged == "yes" ~ 0,
                                                      school_facility_damaged == "no" | school_facility_damaged == "dk" ~ 1
                                                  )) %>% 
  group_by(adm1, adm2, adm3) %>% 
  summarise(resilience_to_shock2_resil_score = round(sum(resilience_to_shock2_resil_score)/n(), 1)) %>%
  mutate(resilience_to_shock2_resil_score = resilience_to_shock2_resil_score)

#Resilience.3
#calculate % the capacity to withstand future shocks

ESCI_resilience3 <- data %>%
  filter(facility_assessed == "education") %>%
  mutate(resilience_to_shock3_ratio = permanent_classroom_type / rowSums(.[c("classroom_totala_functional")], 
                                                                                 na.rm = TRUE)) %>%
  group_by(adm1, adm2, adm3) %>% 
  summarise(resilience_to_shock3_ratio = round(sum(resilience_to_shock3_ratio, na.rm = T)/n(), 1)) %>%
  mutate(resilience_to_shock3_resil_score = resilience_to_shock3_ratio) # take the ratio as final score  

#======================================== CALCULATE ESCI =============================================

ESCI_data_list <- list(ESCI_quality1,ESCI_quality2,ESCI_quality3,ESCI_quality4,ESCI_quality5, #Quality pillar - max 5
                       ESCI_safety1,ESCI_safety2,                                             #Safety pillar - max (=2) depends on number of items or categories
                       ESCI_infra_facilities1,ESCI_infra_facilities2,ESCI_infra_facilities3,ESCI_infra_facilities4,ESCI_infra_facilities5,ESCI_infra_facilities6,ESCI_infra_facilities7,ESCI_infra_facilities8,ESCI_infra_facilities9, #Facility/Infrastructure pillar - max (=8) depends on number of items
                       ESCI_inclusivity1,ESCI_inclusivity2,                 #Inclusivity pillar - max (=2) depends on number of items or categories
                       ESCI_resilience1,ESCI_resilience2,ESCI_resilience3)  #Resilience  pillar - max 3

#create full dataset
ESCI <- ESCI_data_list %>% reduce(full_join, by=c('adm1','adm2','adm3'))

#calculate final ESCI
ESCI <- ESCI %>% 
  select(adm1, adm2, adm3, contains('_score'))%>% 
  #calculate pillar scores, scale to 0-1 by dividing by the max score for that pillar, then apply weights
  mutate(ESCI_quality_pillar_score = (rowSums(across(contains('qual_score'))) /5)*25,
         ESCI_safety_pillar_score = (rowSums(across(contains('safety_score'))) /2)*10,
         ESCI_infra_facilities_pillar_score = (rowSums(across(contains('infra_facil'))) /9)*40,
         ESCI_inclusivity_pillar_score = (rowSums(across(contains('incl_score'))) /2) *10,
         ESCI_resilience_pillar_score = (rowSums(across(contains('resil_score'))) /3) *15,
         
         #calculate final score
         ESCI_score = rowSums(across(contains('_pillar_score')), na.rm = T)) %>% 
  select(adm1, adm2, adm3, contains(c('_pillar_score', 'ESCI_score')))

#export

write_csv(ESCI, file = paste0("outputs/", butteR::date_file_prefix(),
                              "_ESCI_score.csv"))

###############################################################################

