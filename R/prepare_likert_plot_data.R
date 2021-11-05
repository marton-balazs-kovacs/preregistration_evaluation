#' The data provided to the function to create the plots should be in a nested list called `var_desc` created with
#' \code{\link{level_count}} in a dataframe where the groups and variable names
#' are stored in `groups` and `vars` variables as a string.
#' 
#' @param data_sum summarised dataframe created with \code{\link{level_count}}
#' @param vars_to_include the name of the variables that store the Likert-type data
prepare_likert_plot_data <- function(data_sum, vars_to_include) {
  data_sum %>% 
    filter(vars %in% vars_to_include) %>% 
    mutate(groups = groups_rename(groups),
           vars = vars_rename(vars)) %>% 
    select(groups, vars, var_desc) %>% 
    unnest(var_desc)
}
