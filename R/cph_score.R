#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param object
cph_score <- function(object, times, time_scale_factor = 0.95) {

  score_raw <- Score(
    object = list(matrix(object$predicted_risk, ncol = 1)),
    formula = Surv(time, status) ~ 1,
    data = object,
    # AUC doesn't get computed if times = max(time),
    # so scale the times back just a smidge
    times = times*time_scale_factor,
    summary = 'IPA'
  )

  gnd <- suppressWarnings(
    with(object, GND_test(predicted_risk, times, time, status))
  )

  tibble(
    auc = score_raw$AUC$score$AUC,
    ipa = score_raw$Brier$score$IPA[-1]
  ) %>%
    bind_cols(gnd)

}
