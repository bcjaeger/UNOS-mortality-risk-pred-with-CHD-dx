#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x

is_01 <- function(x){

  unique_x <- unique(na.omit(x))

  all(unique_x %in% c(0, 1)) & (length(unique_x) == 2)

}
