#' @section Source: The function is based on the code from \url{https://gist.github.com/trinker/0260a9dfdd9531f9b90d9fad2f7b4b12}
#' 
#' @param dat dataframe containing the data for the plot
#' @param items names of the variables
#' @param labs labels for two time points
#' @param title title of the plot
#' @param levels levels of the likert scale (vector)
#' @param yaxis logical: do you want to show the yaxis (relevant for multipanel plots)
#' @param positive_side if `"lower"` lower levels than the middle are the positive side of the scale, while if `"upper"` levels bigger than the middle are the positive side
#' @param levels_to_drop character vector containing the levels that should not be included e.g. "NA" or "I do not know" 
#' @param limits the limits of the x axis
#' @param text_push the distance between the x axis limits and the text along the y axis
#' @param cols colors for the raincloud and likert bars (vector of 4)
#' @return The function returns a ggplot2 object.
likert_plot_new <- function(data, groups, title, levels, n,
                            levels_to_drop = NA, 
                            yaxis = TRUE, positive_side = c("lower", "upper"), 
                            limits = c(-1, 1.2), text_push = 0.1, textsize = 30, 
                            cols = c("#03A89E","#CD7F32")) {
  .groups <- rlang::enquo(groups)
  .levels <- rlang::enquo(levels)
  
  `%ni%` <- Negate(`%in%`)
  
  # Counts per level of Likert scale

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
  
  # Save color palette 
  pal <- colorRampPalette(c(cols[2],"grey90",cols[1]))
  color_palette <- data.frame(
    colors = pal(max(likert_plot_data$levels_int)),
    levels_int = 1:max(likert_plot_data$levels_int)
  )
  
  # Modify data for plotting
  likert_plot_data <-
    likert_plot_data %>% 
    group_by({{groups}}) %>%
    mutate(prop = {{n}} / sum({{n}}),
           group = as.factor({{groups}}),
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
    group_by(group, support) %>%
    summarise(n_support = sum(n)) %>%
    group_by(group) %>%
    mutate(prop_support = n_support / sum(n_support),
           label = paste0(round_percent(prop_support), "%")) %>%
    ungroup()
  
  divisor <-
    label_data %>%
    filter(support == "neutral") %>%
    mutate(divisor = prop_support / 2) %>%
    select(-c(support, label, n_support, prop_support))
  
  label_data <-
    label_data %>%
    left_join(., divisor, by = "group") %>%
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
  base_plot <- 
    ggplot() +
    geom_hline(yintercept = 0, color = 'black', size = 0.6) +
    geom_bar(
      data = likert_plot_data_pos,
      aes(x = group, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    )  +
    geom_bar(
      data = likert_plot_data_neg,
      aes(x = group, y = prop, fill = colors),
      position = "stack",
      stat = "identity"
    ) +
    coord_flip() +
    #labs(y = "Percentage") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_identity(
      labels = unique(likert_plot_data$levels),
      breaks = unique(likert_plot_data$colors),
      guide = "legend",
      name = '') 
  
  likert_plot <-
    base_plot +
    geom_text(
      data = label_data$negative,
      aes(label = label, x = group,  y = textloc),
      color = "black", size = textsize*1/4) +
    geom_text(
      data = label_data$positive,
      aes(label = label, x = group,  y = textloc),
      color = "black", size = textsize*1/4)  +
    geom_text(
      data = label_data$neutral,
      aes(label = label, x = group,  y = textloc),
      color = "black", size = textsize*1/4) +
    labs(title = title) +
    theme_classic() +
    {if(yaxis)scale_y_continuous(expand = c(0,0), limits = limits, breaks = breaks, labels = labels, name = "Percentage")} +
    {if(!yaxis)scale_y_continuous(expand = c(0,0), limits = limits, breaks = NULL, labels = NULL, name = "")} +
    theme(
      title = element_text(size = textsize),
      axis.text = element_text(size = textsize-2),
      axis.title = element_text(size = textsize-2),
      axis.line=element_blank(),
      strip.text = element_text(hjust = 0, size = textsize),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(color = 'black', linetype = "solid", fill = NA),
      legend.title = element_text(size = textsize*4/5),
      #legend.key.size = unit(1, "cm"),
      legend.position = "none",
      legend.text = element_text(size = textsize*4/5)
    )
  
  # Return output
  return(likert_plot)
}


round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}
