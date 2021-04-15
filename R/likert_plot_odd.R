#' Horizontal stacked barplot for Likert-type data
#' 
#' The function creates a horizontal stacked barpolot for visualizing
#' Likert-type data with an odd number of response levels. The function can
#' take multiple scales and present them in one figure. The data provided to the
#' function to create the plots should be in a nested list called `var_desc` created with
#' \code{\link{level_count}} in a dataframe where the groups and variable names
#' are stored in `groups` and `vars` variables as a string.
#' 
#' @section Source: The function is based on the code from \url{https://gist.github.com/trinker/0260a9dfdd9531f9b90d9fad2f7b4b12}
#' 
#' @param data_sum summarised dataframe created with \code{\link{level_count}}
#' @param vars the name of the variables that store the Likert-type data
#' @param limits the limits of the x axis
#' @param text_push the distance between the x axis limits and the text along the y axis
#' 
#' @return The function returns a ggplot2 object.
likert_plot_odd <- function(data_sum, vars, limits = c(-0.5, 1), text_push = 0.1) {
  # Get the id of the questions
  likert_plot_vars <- vars
  
  # Modify the data for plotting
  likert_plot_data <-
    data_sum %>% 
    filter(vars %in% likert_plot_vars) %>% 
    mutate(groups = groups_rename(groups),
           vars = vars_rename(vars)) %>% 
    select(groups, vars, var_desc) %>% 
    unnest(var_desc) %>% 
    filter(levels %ni% c("I do not know", "Not applicable")) %>%
    group_by(groups, vars) %>% 
    mutate(prop = n / sum(n),
           groups = as.factor(groups),
           support = case_when(levels_int %in% c(1, 2, 3) ~ "negative",
                               levels_int == 4 ~ "neutral",
                               levels_int %in% c(5, 6, 7) ~ "positive",
                               TRUE ~ NA_character_),
           colors = case_when(levels_int == 1 ~ "#CD7F32",
                              levels_int == 2 ~ "#D5A16D",
                              levels_int == 3 ~ "#DDC3A9",
                              levels_int == 4 ~ "grey90",
                              levels_int == 5 ~ "#99D0CD",
                              levels_int == 6 ~ "#4EBCB5",
                              levels_int == 7 ~ "#03A89E"),
           colors = factor(colors, levels = colors)) %>% 
    ungroup()
  
  # Split the data to positive and negative part
  likert_plot_data_pos <-
    likert_plot_data %>% 
    filter(support %in% c("positive", "neutral")) %>% 
    mutate(colors = factor(colors, levels = rev(levels(colors))),
           prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop)) 
  
  likert_plot_data_neg <-
    likert_plot_data %>% 
    filter(support %in% c("negative", "neutral")) %>% 
    mutate(prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop),
           prop = -1 * prop)
  
  # Calculate the text labels for the plot
  label_data <- 
    likert_plot_data %>%
    group_by(vars, groups, support) %>%
    summarise(n_support = sum(n)) %>% 
    group_by(vars, groups) %>%
    mutate(prop_support = n_support / sum(n_support),
           label = paste0(round(prop_support * 100, 0), "%")) %>% 
    ungroup()
  
  divisor <- 
    label_data %>%
    filter(support == "neutral") %>% 
    mutate(divisor = prop_support / 2) %>% 
    select(-c(support, label, n_support, prop_support))
  
  label_data <- 
    label_data %>% 
    left_join(., divisor, by = c("vars", "groups")) %>% 
    mutate(prop_support = case_when(support %in% c("positive", "negative") ~ prop_support + divisor,
                                    support == "neutral" ~ prop_support),
           proploc = case_when(support == "negative" ~ -1 * prop_support,
                               TRUE ~ prop_support)) %>% 
    group_by(support) %>%
    mutate(textloc = case_when(support == "positive" ~ limits[2] - text_push,
                               support == "negative" ~ limits[1] + text_push,
                               support == "neutral" ~ 0)) %>% 
    split(.$support)
  
  breaks <- round(seq(limits[1], limits[2], by = 0.5), 1)
  
  labels <- paste0(as.character(abs(breaks) * 100), "%")
    
  # Create the plot
  likert_plot <-
    ggplot() + 
    geom_bar(
      data = likert_plot_data_pos, 
      aes(x = groups, y = prop, fill = colors),
      position = "stack", 
      stat = "identity"
    )  + 
    geom_bar(
      data = likert_plot_data_neg, 
      aes(x = groups, y = prop, fill = colors),
      position = "stack", 
      stat = "identity"
    ) +
    coord_flip()  +
    geom_hline(yintercept = 0, color = 'white', size = 1) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_identity(
      labels = unique(likert_plot_data$levels), 
      breaks = unique(likert_plot_data$colors), 
      guide = "legend", 
      name = '')
  
  if (length(vars) > 1) {
    likert_plot <- 
      likert_plot +
      facet_wrap(~ vars, ncol = 1)
  } else {
    likert_plot <- 
      likert_plot +
      labs(title = dplyr::distinct(likert_plot_data, vars))
  }
    
  likert_plot <- 
    likert_plot +
    geom_text(
      data = label_data$negative, 
      aes(label = label, x = groups,  y = textloc), 
      hjust = 0, color = "grey50", size = 6) +
    geom_text(
      data = label_data$positive,
      aes(label = label, x = groups,  y = textloc), 
      hjust = 1, color = "grey50", size = 6)  +
    geom_text(
      data = label_data$neutral,
      aes(label = label, x = groups,  y = textloc), 
      hjust = .5, color = "grey50", size = 6) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = limits,
      breaks = breaks,
      labels = labels) +
    theme(
      title = element_text(size = 20),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 25),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = 'bold', size = 25),
      panel.border = element_rect(color = 'gray90', linetype = "dashed", fill = NA),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 15))
  
  # Return output
  return(likert_plot)
}
