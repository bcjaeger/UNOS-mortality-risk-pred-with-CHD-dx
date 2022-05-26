#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param data
#' @param run
#' @param features
#' @param training_index
#'
#' Expanded models were fitted accounting for UNOS variables and,
#' when applicable, incorporated diagnostic CHD variables.
#'
make_mccv_output <- function(data,
                             features_ref,
                             times,
                             run,
                             training_index) {


  str_dots <- function(x){

    x_obs <- !is.na(x)
    x[x_obs] <- paste('..', x[x_obs], sep = '')
    x

  }

  data <- data |>
    mutate(across(where(is.factor), as.character),
           across(where(is.character), str_dots),
           across(where(is.character), as.factor))

  data_train <- data[training_index[[1]], ]

  data_test <- data[-training_index[[1]], ] %>%
    filter(prim_dx_rc %in% c('..Congenital HD','..Cardiomyopathy'))

  # Step 1: fit a model to all training data using reference predictors
  # (this is analogous to the UNOS model)
  mdl_ref <- cph_fit(data_train, features_ref)

  # compute predicted risk
  prisk_ref <- cph_predict_risk(
    object = mdl_ref,
    new_data = data_test,
    times = times
  ) %>%
    mutate(
      dx = case_when(
        data_test$prim_dx_rc == '..Cardiomyopathy' ~ '..Cardiomyopathy',
        data_test$chd_sv_rc == '..singleVentricle' ~ '..Congenital HD; single ventricle',
        data_test$chd_sv_rc == '..biVentricle' ~ '..Congenital HD; bi-ventricle'
      )
    )

  # validate the predicted risk in the testing data
  score_ref <- cph_score(object = prisk_ref, times = times)

  # include baseline risk prediction in the expanded CHD models
  data_train$lp <- predict(mdl_ref$fit,
                           type = 'lp',
                           newdata = juice(mdl_ref$recipe))

  data_test$lp <- predict(mdl_ref$fit,
                          type = 'lp',
                          newdata = bake(mdl_ref$recipe,
                                         new_data = data_test))

  # Cardiomyopathy (preds: sec_dx_rc)
  mdl_cardiomyopathy <-
    cph_fit(
      filter(data_train, prim_dx_rc == '..Cardiomyopathy'),
      features = c('lp', 'sec_dx_rc')
    )


  mdl_cardiomyopathy_2 <-
    cph_fit(
      filter(data_train, prim_dx_rc == '..Cardiomyopathy'),
      features = c(setdiff(features_ref, c('prim_dx_rc', 'rtdial')),
                   "lp",
                   "sec_dx_rc",
                   "age_cat",
                   "blood_type_cat",
                   "weight_cat",
                   "bsa_cat",
                   "race_cat",
                   "egfr_cat"),
      backward_selection = TRUE
    )

  prisk_cardiomyopathy <- cph_predict_risk(
    object = mdl_cardiomyopathy,
    times = times,
    new_data = filter(data_test, prim_dx_rc == '..Cardiomyopathy')
  ) %>%
    mutate(dx = '..Cardiomyopathy')

  prisk_cardiomyopathy_2 <- cph_predict_risk(
    object = mdl_cardiomyopathy_2,
    times = times,
    new_data = filter(data_test, prim_dx_rc == '..Cardiomyopathy')
  ) %>%
    mutate(dx = '..Cardiomyopathy')

  # Congenital HD single ventricle (preds: stage, chd_sv_group)
  # warnings due to small sample size; safe to suppress
  mdl_congenitalHD_sv <- suppressWarnings(
    cph_fit(
      filter(data_train,
             prim_dx_rc == '..Congenital HD',
             chd_sv_rc == '..singleVentricle'),
      features = c('lp', 'stage', 'chd_sv_group')
    )
  )

  mdl_congenitalHD_sv_2 <- suppressWarnings(
    cph_fit(
      filter(data_train,
             prim_dx_rc == '..Congenital HD',
             chd_sv_rc == '..singleVentricle'),
      features = c(setdiff(features_ref, c('prim_dx_rc', 'rtdial')),
                   "lp",
                   'stage',
                   'chd_sv_group',
                   "age_cat",
                   "blood_type_cat",
                   "weight_cat",
                   "bsa_cat",
                   "race_cat",
                   "egfr_cat"),
      backward_selection = TRUE
    )
  )

  prisk_congenitalHD_sv <- cph_predict_risk(
    object = mdl_congenitalHD_sv,
    times = times,
    new_data = filter(data_test,
                      prim_dx_rc == '..Congenital HD',
                      chd_sv_rc == '..singleVentricle')
  ) %>%
    mutate(dx = '..Congenital HD; single ventricle')

  prisk_congenitalHD_sv_2 <- cph_predict_risk(
    object = mdl_congenitalHD_sv_2,
    times = times,
    new_data = filter(data_test,
                      prim_dx_rc == '..Congenital HD',
                      chd_sv_rc == '..singleVentricle')
  ) %>%
    mutate(dx = '..Congenital HD; single ventricle')


  # Congenital HD double ventricle (preds: chd_bv_surg)
  mdl_congenitalHD_bv <-
    cph_fit(
      filter(data_train,
             prim_dx_rc == '..Congenital HD',
             chd_sv_rc == '..biVentricle'),
      features = c('lp', 'chd_bv_surg')
    )

  mdl_congenitalHD_bv_2 <-
    cph_fit(
      filter(data_train,
             prim_dx_rc == '..Congenital HD',
             chd_sv_rc == '..biVentricle'),
      features = c(setdiff(features_ref, c('prim_dx_rc', 'rtdial')),
                   "lp",
                   "chd_bv_surg",
                   "age_cat",
                   "blood_type_cat",
                   "weight_cat",
                   "bsa_cat",
                   "race_cat",
                   "egfr_cat"),
      backward_selection = TRUE
    )

  prisk_congenitalHD_bv <- cph_predict_risk(
    object = mdl_congenitalHD_bv,
    times = times,
    new_data = filter(data_test,
                      prim_dx_rc == '..Congenital HD',
                      chd_sv_rc == '..biVentricle')
  ) %>%
    mutate(dx = '..Congenital HD; bi-ventricle')

  prisk_congenitalHD_bv_2 <- cph_predict_risk(
    object = mdl_congenitalHD_bv_2,
    times = times,
    new_data = filter(data_test,
                      prim_dx_rc == '..Congenital HD',
                      chd_sv_rc == '..biVentricle')
  ) %>%
    mutate(dx = '..Congenital HD; bi-ventricle')

  score_chd <- cph_score(
    object = bind_rows(
      prisk_cardiomyopathy,
      prisk_congenitalHD_sv,
      prisk_congenitalHD_bv
    ),
    times = times
  )

  score_chd_2 <- cph_score(
    object = bind_rows(
      prisk_cardiomyopathy_2,
      prisk_congenitalHD_sv_2,
      prisk_congenitalHD_bv_2
    ),
    times = times
  )

  score_ref_bydx <- prisk_ref %>%
    split(.$dx) %>%
    map_dfr(cph_score, times = times, .id = 'dx') %>%
    mutate(model = 'ref')

  score_chd_bydx <- bind_rows(
    prisk_cardiomyopathy,
    prisk_congenitalHD_sv,
    prisk_congenitalHD_bv
  ) %>%
    split(.$dx) %>%
    map_dfr(cph_score, times = times, .id = 'dx') %>%
    mutate(model = 'chd')

  score_chd_bydx_2 <- bind_rows(
    prisk_cardiomyopathy_2,
    prisk_congenitalHD_sv_2,
    prisk_congenitalHD_bv_2
  ) %>%
    split(.$dx) %>%
    map_dfr(cph_score, times = times, .id = 'dx') %>%
    mutate(model = 'chd_2')

  bind_rows(
    ref = score_ref,
    chd = score_chd,
    chd_2 = score_chd_2,
    .id = 'model'
  ) %>%
    mutate(dx = 'Overall') %>%
    bind_rows(score_ref_bydx,
              score_chd_bydx,
              score_chd_bydx_2) %>%
    mutate(run = run, .after = model)

}
