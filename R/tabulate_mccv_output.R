#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ...
tabulate_mccv_output <- function(..., rspec,
                                 admin_censor_posttxpl,
                                 admin_censor_waitlist) {

  data <- bind_rows(..., .id = 'period')

  data_smry <- data %>%
    rename(gnd = GND_pvalue) %>%
    select(-GND_chisq) %>%
    mutate(model = recode(model, 'chd_2' = 'chd2')) %>%
    #mutate(gnd = if_else(gnd < 0.05, 0, 1)) %>%
    pivot_wider(names_from = model,
                values_from = c(auc, ipa, gnd)) %>%
    #drop_na() %>%
    # mutate(auc_diff = auc_chd - auc_ref,
    #        ipa_diff = ipa_chd - ipa_ref,
    #        gnd_diff = gnd_chd - gnd_ref) %>%
    pivot_longer(cols = matches("^auc|^ipa|^gnd"),
                 names_to = c('metric', 'model'),
                 names_sep = '_') %>%
    group_by(period, dx, metric, model) %>%
    summarize(
      across(
        .cols = value,
        .fns = list(lwr = ~ 100*quantile(.x, na.rm=T, probs = 0.025),
                    est = ~ 100*quantile(.x, na.rm=T, probs = 0.50),
                    upr = ~ 100*quantile(.x, na.rm=T, probs = 0.975))
      )
    )


  data_table <- data_smry %>%
    mutate(
      value = if_else(
        metric == 'gnd',
        true = table_glue(
          "{value_est}\n({value_lwr}, {value_upr})",
          rspec = rspec
        ),
        false = table_glue(
          "{value_est}\n({value_lwr}, {value_upr})",
          rspec = rspec
        )
      )
    )%>%
    select(-value_lwr, -value_est, -value_upr) %>%
    pivot_wider(names_from = model, values_from = value) %>%
    select(period, dx, metric, ref, chd, chd2) %>%
    ungroup() %>%
    mutate(
      period = recode(
        period,
        'posttxpl' = paste(
          'Predicting death or graft loss', admin_censor_posttxpl,
          'year after transplant'
        ),
        'waitlist' = paste(
          'Predicting death', admin_censor_waitlist, 'years after listing'
        )
      ),
      dx = factor(dx,
                  levels = c('Overall',
                             '..Cardiomyopathy',
                             '..Congenital HD; single ventricle',
                             '..Congenital HD; bi-ventricle'),
                  labels = c('Overall',
                             'Cardiomyopathy',
                             'Congenital HD;\nsingle ventricle',
                             'Congenital HD;\nbi-ventricle'))
    ) %>%
    split(.$metric) %>%
    map(arrange, period, dx)

  tables <- map(
    data_table,
    ~ungroup(.x) %>%
      select(-metric) %>%
      as_grouped_data(groups = 'period') %>%
      flextable::as_flextable(hide_grouplabel = TRUE) %>%
      width(width = 1.4) %>%
      set_header_labels(dx = 'Patient group',
                        ref = 'UNOS variables',
                        chd = 'UNOS + CHD variables',
                        chd2 = 'UNOS + CHD2 variables') %>%
      theme_box() %>%
      align(j = 2:4, align = 'center', part = 'all')
  )

  tables

}
