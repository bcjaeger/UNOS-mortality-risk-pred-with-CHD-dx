#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param
cph_fit <- function(data_train, features, backward_selection = FALSE) {

  formula_rhs <- paste(features, collapse = ' + ')

  model_formula <- formula(glue("Surv(time, status) ~ {formula_rhs}"))

  recipe <- make_recipe(data_train[, c('time', 'status', features)]) %>%
    prep()

  fit <- do.call(
    what = 'coxph',
    args = list(
      formula = model_formula,
      data = juice(recipe),
      x = TRUE)
  )

  while(any(is.na(coef(fit)))){

    fit_coef <- enframe(coef(fit)) |>
      separate(name,
               into = c('variable', 'level'),
               sep = '\\.\\.',
               fill = 'right')

    fit_coef_na <- fit_coef |>
      filter(is.na(value)) |>
      pull(variable)

    features <- setdiff(features, fit_coef_na)

    formula_rhs <- paste(features, collapse = ' + ')

    model_formula <- formula(glue("Surv(time, status) ~ {formula_rhs}"))

    recipe <- make_recipe(data_train[, c('time', 'status', features)]) %>%
      prep()

    fit <- do.call(
      what = 'coxph',
      args = list(
        formula = model_formula,
        data = juice(recipe),
        x = TRUE)
    )

  }

  if(backward_selection){

    step_scope_rhs <- juice(recipe) %>%
      select(-time, -status) %>%
      names() %>%
      glue_collapse(sep = ' + ')

    step_scope <- as.formula(glue("Surv(time, status) ~ {step_scope_rhs}"))

    fit <- stepAIC(object = fit,
                   scope = step_scope,
                   direction = 'both',
                   k = log(nrow(juice(recipe))),
                   trace = 0)

    while(any(is.na(coef(fit)))){

      fit_coef <- enframe(coef(fit)) |>
        separate(name,
                 into = c('variable', 'level'),
                 sep = '\\.\\.',
                 fill = 'right')

      fit_coef_na <- fit_coef |>
        filter(is.na(value)) |>
        pull(variable) |>
        unique()

      features <- setdiff(features, fit_coef_na)

      recipe <- make_recipe(data_train[, c('time', 'status', features)]) %>%
        prep()

      step_scope_rhs <- juice(recipe) %>%
        select(-time, -status) %>%
        names() %>%
        glue_collapse(sep = ' + ')

      fit <- stepAIC(object = fit,
                     scope = step_scope,
                     direction = 'both',
                     trace = 0)

    }


  }

  list(recipe = recipe,
       fit = fit)

}
