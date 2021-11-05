#' Horizontal stacked barplot for Likert-type data
#' 
#' The function creates a horizontal stacked barpolot for visualizing
#' Likert-type data with an odd number of response levels. The function can
#' take multiple scales and present them in one figure.
#' 
#' @section Source: The function is based on the code from \url{https://gist.github.com/trinker/0260a9dfdd9531f9b90d9fad2f7b4b12}
#' 
#' @param data dataframe containing the data for the plot
#' @param title title of the plot
#' @param levels name of the variable containing the levels. this variable should be a factor
#' @param questions name of the variable that contains the questions to which the levels belong (multiple questions can be used)
#' @param n name of the variable containing the number of responses corresponding to each level
#' @param positive_side if `"lower"` lower levels than the middle are the positive side of the scale, while if `"upper"` levels bigger than the middle are the positive side
#' @param levels_to_drop character vector containing the levels that should not be included e.g. "NA" or "I do not know" 
#' @param limits the limits of the x axis
#' @param text_push the distance between the x axis limits and the text along the y axis
#' 
#' @return The function returns a ggplot2 object.
likert_plot_odd <- function(data, title, questions, levels, n, levels_to_drop, positive_side = c("lower", "upper"), limits = c(-0.5, 1), text_push = 0.1) {
  .questions <- rlang::enquo(questions)
  .levels <- rlang::enquo(levels)
  
  `%ni%` <- Negate(`%in%`)
  
  # Save color palette
  color_palette <- 
    data.frame(
      colors = c("#CD7F32", "#D5A16D", "#DDC3A9", "grey90", "#99D0CD", "#4EBCB5", "#03A89E"),
      levels_int = 1:7)
  
  # Filter levels
  likert_plot_data <-
    data %>%
    # Drop unwanted levels
    filter({{levels}} %ni% levels_to_drop) %>% 
    # Get levels as integer
    mutate(levels_int = as.integer({{levels}}))
  
  # The number of levels should be odd
  if (length(unique(likert_plot_data$levels_int)) %ni% c(3, 5, 7)) {
    stop("The number of levels should 3, 5, or 7!")
  }
  
  # Get middle level
  middle_level <- median(likert_plot_data$levels_int)
  
  # Modify data for plotting
  likert_plot_data <-
    likert_plot_data %>% 
    group_by({{questions}}) %>%
    mutate(prop = {{n}} / sum({{n}}),
           {{questions}} := as.factor({{questions}}),
           support = case_when(levels_int < middle_level ~ "negative",
                               levels_int ==  middle_level ~ "neutral",
                               levels_int > middle_level ~ "positive",
                               TRUE ~ NA_character_)) %>%
    ungroup() %>% 
    left_join(., color_palette, by = "levels_int") %>% 
    mutate(colors = forcats::as_factor(colors))

  # Split the data to positive and negative part
  likert_plot_data_pos <-
    likert_plot_data %>%
    filter(support %in% c("positive", "neutral")) %>%
    mutate(
      colors = factor(colors, levels = rev(levels(colors))),
      prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop))

  likert_plot_data_neg <-
    likert_plot_data %>%
    filter(support %in% c("negative", "neutral")) %>%
    mutate(
      prop = case_when(support == 'neutral' ~ prop / 2, TRUE ~ prop),
      prop = -1 * prop)

  # Calculate the text labels for the plot
  label_data <-
    likert_plot_data %>%
    group_by({{questions}}, support) %>%
    summarise(n_support = sum(n)) %>%
    group_by({{questions}}) %>%
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
    left_join(., divisor, by = rlang::as_name(.questions)) %>%
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
    geom_hline(yintercept = 0, color = 'black', size = 1) +
    geom_bar(
      data = likert_plot_data_pos,
      aes(x = {{questions}}, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    )  +
    geom_bar(
      data = likert_plot_data_neg,
      aes(x = {{questions}}, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    ) +
    coord_flip()  +
    labs(y = "Percentage") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_identity(
      labels = unique(likert_plot_data$levels),
      breaks = unique(likert_plot_data$colors),
      guide = "legend",
      name = '')

  likert_plot <-
    likert_plot +
    geom_text(
      data = label_data$negative,
      aes(label = label, x = {{questions}},  y = textloc),
      hjust = 0, color = "black", size = 10) +
    geom_text(
      data = label_data$positive,
      aes(label = label, x = {{questions}},  y = textloc),
      hjust = 1, color = "black", size = 10)  +
    geom_text(
      data = label_data$neutral,
      aes(label = label, x = {{questions}},  y = textloc),
      hjust = .5, color = "black", size = 10) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = limits,
      breaks = breaks,
      labels = labels) +
    labs(title = title) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(
      title = element_text(size = 30),
      axis.text = element_text(size = 30),
      axis.title = element_text(size = 30),
      strip.text = element_text(
        hjust = 0, face = 'bold', 
        size = 30
        ),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(color = 'black', linetype = "solid", fill = NA),
      legend.position = "bottom"
      # legend.spacing.x = unit(1.0, 'cm')
      # legend.box = "vertical",
      )

  # Return output
  return(likert_plot)
}
