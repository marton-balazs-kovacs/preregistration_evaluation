#' Function to not include a vector in another vector
#' 
#' Retrieved from https://stackoverflow.com/questions/5831794/opposite-of-in
`%ni%` <- Negate(`%in%`)

#' Function to choose one group for ggplot2
#' 
#' Retrieved from https://stackoverflow.com/questions/35806310/ggplot-plotting-layers-only-if-certain-criteria-are-met
pick <- function(condition){
  function(d) d %>% filter(!!enquo(condition))
}
