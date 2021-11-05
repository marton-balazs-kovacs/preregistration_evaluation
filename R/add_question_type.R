#' Function to add question type
#' 
#' For the interval type questions the mean and SD will be computed but for
#' categorical variables the count and percentage of the responses
#' to all level will be calculated separately.
#' 
#' @param var_name variable name
add_question_type <- function(var_name) {
  case_when({{var_name}} %in% c("recommend", "qrp", "planning") ~ "likert",
            {{var_name}} %in% c("hypothesis", "design", "analysis", "rdm", "workflow", "collab", "preparatory", "duration", "stress") ~ "interval",
            TRUE ~ NA_character_)
}
