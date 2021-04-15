#' Creating an APA formatted barplot
#' 
#' The function creates an APA formatted barplot that depicts the proportion
#' of responses given to a all the levels of scale in a subgroup.
#' 
#' @param df dataframe that contains the processed data
#' @param group_var name of the variable that contains the groups
#' @param group_name the name of the subgroup (has to be in group_var)
#' @param var_name name of the variable that contains the scale as a string
#' @param factor_group the name of the subgroup that will be passed to \code{\link{factor_order}}
#' 
#' @return The function returns a ggplot2 object.
apa_barplot <- function(df, group_name, var_name, group_var, factor_group) {
  df %>% 
    dplyr::filter({{group_var}} == group_name) %>% 
    dplyr::count(.data[[var_name]]) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(N = sum(n),
                  prop = round(n / N * 100, 2)) %>% 
    dplyr::mutate({{var_name}} := factor_order(.data[[var_name]], factor_group, var_name)) %>% 
    tidyr::complete(.data[[var_name]], nesting(N), fill = list(n = 0, prop = 0)) %>% 
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[var_name]], y = prop) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(y = "Proportion",
                  title = group_name) +
    ggplot2::coord_flip() +
    papaja::theme_apa()
}
