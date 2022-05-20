#' Function to rename groups in the survey
#' 
#' The function takes group names in the survey and renames them.
#' 
#' @param group_name variable containing the groups
groups_rename <- function(group_name) {
  case_when({{group_name}} == "prereg" ~ "Preregistration",
            {{group_name}} == "control" ~ "Control",
            {{group_name}} == "testing" ~ "Testing",
            {{group_name}} == "else" ~ "Else",
            {{group_name}} == "other" ~ "Unpublished",
            TRUE ~ NA_character_)
}
