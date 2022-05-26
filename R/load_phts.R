#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param ... additional commands for mutate
#' @param date
#' @param fname
#' @param time
#' @param status
#' @param features
#' @param admin_censor_years
load_phts <- function(...,
                      date,
                      fname,
                      time,
                      status,
                      features,
                      admin_censor_years) {

  input <- file.path(date, fname) %>%
    read_csv(guess_max = 15000, na = c("", "NA", "U", "Un")) %>%
    set_names(tolower(names(.))) %>%
    mutate(
      chd_bv_surg = recode(chd_bv_surg,
                           '0' = 'noSurgery',
                           '1' = 'cbvSurgery'),
      chd_sv_rc = recode(chd_sv_rc,
                         '0' = 'biVentricle',
                         '1' = 'singleVentricle'),
      chd_sv_group = str_replace(chd_sv_group, '1VCHD-', '')
    ) %>%
    rename(time = !!time,
           status = !!status) %>%
    select(time, status, any_of(set_names(features, NULL))) %>%
    mutate(
      status = replace(
        x = status,
        list = time > admin_censor_years,
        values = 0
      ),
      time = pmin(time, admin_censor_years),
      ...
    )

  # only the outcome column should be 0/1, other columns should be no/yes
  cols_01 <- map_lgl(input, is_01) %>%
    enframe() %>%
    filter(name != 'status', value == TRUE) %>%
    pull(name)

  output <- input %>%
    mutate(
      across(.cols = all_of(cols_01),
             .fns = factor,
             levels = c(0,1),
             labels = c('No', 'Yes')),
      across(where(is.character), as.factor),
      stage = factor(stage)
    )

  output

}

