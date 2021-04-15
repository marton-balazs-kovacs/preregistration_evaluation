#' Function to rename variables of the survey
#' 
#' This function renames variables containing survey respones.
#' 
#' @param var_name name of the variable that contains survey question variable
#' names in a long or tidy format.
vars_rename <- function(var_name) {
  case_when({{var_name}} == "planning" ~ "Consider Preregistration in Future Work",
            {{var_name}} == "recommend" ~ "Recommend Preregistration To Colleagues",
            {{var_name}} == "hypothesis" ~ "Research Hypothesis",
            {{var_name}} == "design" ~ "Experimental Design",
            {{var_name}} == "analysis" ~ "Analysis Plan",
            {{var_name}} == "rdm" ~ "Research Data Management",
            {{var_name}} == "workflow" ~ "Project Workflow",
            {{var_name}} == "collab" ~ "Collaboration in the Team",
            {{var_name}} == "preparatory" ~ "Preparatory Work",
            {{var_name}} == "duration" ~ "Total Project Duration",
            {{var_name}} == "stress" ~ "Work-related Stress",
            {{var_name}} == "qrp" ~ "Preregistration Prevents Questionable Research Practices",
            TRUE ~ NA_character_)
}
