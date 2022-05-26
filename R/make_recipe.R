#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_train
make_recipe <- function(data_train) {

  recipe(x = data_train, time + status ~ .) %>%
    step_impute_mean(recipes::all_numeric(), -recipes::all_outcomes()) %>%
    step_impute_mode(recipes::all_nominal())

}
