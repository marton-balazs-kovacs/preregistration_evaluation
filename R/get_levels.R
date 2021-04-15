#' Function to extract levels of a scale
#' 
#' The function extracts the levels of a scale stored in a factor type variable
#' for a specific subgroup. Even if specific levels are not found in the data,
#' they are complemented by the \code{\link{factor_order}} function.
#' 
#' @inheritParams apa_barplot
#' 
#' @return The function returns a character vector of the levels if the scale
#'   in a specific order.
get_levels <- function(df, group_name, var_name, factor_group, group_var) {
  df %>% 
    dplyr::filter({{group_var}} == group_name) %>% 
    dplyr::distinct(.data[[var_name]]) %>% 
    dplyr::rename(levels = .data[[var_name]]) %>%
    dplyr::mutate(levels = factor_order(
      factor_var = levels,
      group = factor_group,
      var = var_name)) %>% 
    tidyr::complete(levels) %>% 
    dplyr::mutate(levels = as.character(levels)) %>% 
    dplyr::pull(levels)
}
