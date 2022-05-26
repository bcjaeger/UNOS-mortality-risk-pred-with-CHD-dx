

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
mode <- function(x){

  x_tbl <- table(as.character(x))
  names(x_tbl)[which.max(x_tbl)]

}
