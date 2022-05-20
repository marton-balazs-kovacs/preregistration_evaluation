#' Function to reorder factor scale variables
#' 
#' This function takes a response variable and reorder the factor levels in it.
#' 
#' @param factor_var variable containing the responses
#' @param group name of the group
#' @param var name of the response variable
#' 
#' @return Returns a reordered factor.
factor_order <- function(factor_var, group, var) {
  if (group == "control" & var %in%  c("analysis", "rdm", "workflow", "design", "hypothesis")) {
    factor(
      {{factor_var}},
      levels = c(
        "1\nWould get less thought-through",
        "2",
        "3",
        "4\nWould not change",
        "5",
        "6",
        "7\nWould get more thought-through",
        "I do not know",
        "Not applicable"))
  } else if (group == "control" & var == "collab") {
    factor(
      {{factor_var}},
      levels = c(
        "1\nWould get worse",
        "2",
        "3",
        "4\nWould not change",
        "5",
        "6",
        "7\nWould get better",
        "I do not know",
        "Not applicable"))
  } else if (group == "control" & var == "preparatory") {
    factor(
      {{factor_var}},
      levels = c(
        "1\nWould get worse",
        "2",
        "3",
        "4\nWould not change",
        "5",
        "6",
        "7\nWould improve",
        "I do not know",
        "Not applicable"))
  } else if (group == "control" & var == "duration") {
    factor(
      {{factor_var}},
      levels = c(
        "1\nWould be longer",
        "2",
        "3",
        "4\nWould not change",
        "5",
        "6",
        "7\nWould be shorter",
        "I do not know",
        "Not applicable"))
  } else if (group == "control" & var == "stress") {
    factor(
      {{factor_var}},
      levels = c(
      "1\nWould be increased",
      "2",
      "3",
      "4\nWould not change",
      "5",
      "6",
      "7\nWould be reduced",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("control", "prereg", "other") & var %in% c("qrp","recommend")) {
    factor(
      {{factor_var}},
      levels = c(
      "Very Strongly Disagree",
      "Strongly Disagree",
      "Disagree",
      "Neither Agree or Disagree",
      "Agree",
      "Strongly Agree",
      "Very Strongly Agree",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("prereg", "other") & var %in% c("hypothesis", "design", "analysis", "rdm", "workflow")) {
    factor(
      {{factor_var}},
      levels = c(
      "1\nGot less thought-through",
      "2",
      "3",
      "4\nDid not change",
      "5",
      "6",
      "7\nGot more thought-through",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("prereg", "other") & var == "collab") {
    factor(
      {{factor_var}},
      levels = c(
      "1\nGot worse",
      "2",
      "3",
      "4\nDid not change",
      "5",
      "6",
      "7\nGot better",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("prereg", "other") & var == "preparatory") {
    factor(
      {{factor_var}},
      levels = c(
      "1\nGot worse",
      "2",
      "3",
      "4\nDid not change",
      "5",
      "6",
      "7\nImproved",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("prereg", "other") & var == "duration") {
    factor(
      {{factor_var}},
      levels = c(
      "1\nWas longer",
      "2",
      "3",
      "4\nDid not change",
      "5",
      "6",
      "7\nWas shorter",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("prereg", "other") & var == "stress") {
    factor(
      {{factor_var}},
      levels = c(
      "1\nWas increased",
      "2",
      "3",
      "4\nDid not change",
      "5",
      "6",
      "7\nWas reduced",
      "I do not know",
      "Not applicable"))
  } else if (group %in% c("control", "prereg", "other") & var == "planning") {
    factor(
      {{factor_var}},
      levels = c(
        "Never",
        "Rarely",
        "Occasionally",
        "Sometimes",
        "Frequently",
        "Usually",
        "Always"))
  } else  {
    return("Variable name not found")
  }
}
  
