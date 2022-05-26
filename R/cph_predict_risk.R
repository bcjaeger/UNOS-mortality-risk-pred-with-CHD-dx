#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param object
#' @param new_data
cph_predict_risk <- function(object, new_data, times) {

  new_data_processed <- bake(object$recipe, new_data = new_data)

  new_data_processed$predicted_risk <-
    as.numeric(
      predictRisk(
        object = object$fit,
        newdata = new_data_processed,
        times = times
      )
    )

  as_tibble(new_data_processed) %>%
    select(time, status, predicted_risk)

}
