###############################################################################
# creating composite indicators -----------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df |> 
    dplyr::mutate(int.total_enrolled_boys = sum(c_across(c("pre_school_enrolled_boys", "primary_school_enrolled_boys", "post_primary_school_enrolled_boys", "middle_school_enrolled_boys", "high_school_enrolled_boys", "preparatory_school_enrolled_boys")), na.rm = T),
                  int.total_enrolled_girls = sum(c_across(c("pre_school_enrolled_girls", "primary_school_enrolled_girls", "post_primary_school_enrolled_girls", "middle_school_enrolled_girls", "high_school_enrolled_girls", "preparatory_school_enrolled_girls")), na.rm = T),        
                  int.total_enrolled_children = sum(c_across(c("pre_school_enrolled_boys", "primary_school_enrolled_boys", "post_primary_school_enrolled_boys", "middle_school_enrolled_boys", "high_school_enrolled_boys", "preparatory_school_enrolled_boys", 
                                                               "pre_school_enrolled_girls", "primary_school_enrolled_girls", "post_primary_school_enrolled_girls", "middle_school_enrolled_girls", "high_school_enrolled_girls", "preparatory_school_enrolled_girls")), na.rm = T)
    )|>
    ungroup()
}
###############################################################################  