#' Function to check whether the level of a scale is present in the data
#' 
#' The function takes a scale in a factor variable and checks whether each
#' level of the factor is present in the dataset at least once in a specific
#' subgroup.
#' 
#' @inheritParams apa_barplot
#' 
#' @return The function returns a dataframe with each level of the factor and
#' with the `present` variable that is 1 if the levels is present in the dataset
#' and 0 if it is not.
level_present <- function(df, group_name, var_name, factor_group, group_var) {
  df %>% 
    dplyr::filter({{group_var}} == group_name) %>% 
    dplyr::distinct(.data[[var_name]]) %>%
    dplyr::mutate(present = 1) %>% 
    dplyr::rename(levels = .data[[var_name]]) %>%
    dplyr::mutate(levels = factor_order(
      factor_var = levels,
      group = factor_group,
      var = var_name)) %>% 
    tidyr::complete(levels, fill = list(present = 0))
}
