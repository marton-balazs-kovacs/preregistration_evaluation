#' Count the number of responses for each level of a scale
#' 
#' This function counts the number of responses for each level of a scale variable
#' in the given subgroup. If some levels are not present in the data the function
#' assigns 0 to the given levels count.
#' 
#' @inheritParams apa_barplot
#' 
#' @return The function returns a summarized dataframe of the counts of each
#' scale level for a given subgroup and the order of the levels as an integer
#' in `levels_int`. 
level_count <- function(df, group_name, var_name, group_var, factor_group) {
  df %>%
      dplyr::filter({{group_var}} == group_name) %>%
      dplyr::count(.data[[var_name]]) %>%
      dplyr::rename(levels = .data[[var_name]]) %>%
      dplyr::mutate(levels = factor_order(
        factor_var = levels,
        group = factor_group,
        var = var_name)) %>% 
      tidyr::complete(levels, fill = list(n = 0)) %>% 
      dplyr::mutate(levels_int = as.integer(levels))
}

