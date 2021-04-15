#' Function retrieved from the plyr package https://cran.r-project.org/web/packages/plyr/plyr.pdf
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
  }