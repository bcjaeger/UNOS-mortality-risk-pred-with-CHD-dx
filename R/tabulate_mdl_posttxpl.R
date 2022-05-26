#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_posttxpl
#' @param labels
tabulate_mdl_posttxpl <- function(data_posttxpl, labels) {

  data_ref <- recipe(x = data_posttxpl, time + status ~ .) %>%
    step_impute_mean(recipes::all_numeric(), -recipes::all_outcomes()) %>%
    step_impute_mode(recipes::all_nominal()) %>%
    prep() %>%
    juice() %>%
    filter(prim_dx_rc %in% c("Cardiomyopathy", "Congenital HD")) %>%
    droplevels()

  data_chd_1 <- data_ref %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(
      prim_dx_rc = if_else(
        prim_dx_rc == 'Congenital HD',
        true = paste(prim_dx_rc, chd_sv_rc, sep = '..'),
        false = prim_dx_rc
      )
    )

  data_chd_2 <- data_chd_1 %>%
    mutate(
      prim_dx_rc = if_else(
        prim_dx_rc == 'Congenital HD..biVentricle',
        true = paste(prim_dx_rc, chd_bv_surg, sep = '..'),
        false = prim_dx_rc
      ),
      prim_dx_rc = if_else(
        prim_dx_rc == 'Congenital HD..singleVentricle',
        true = paste(prim_dx_rc, chd_sv_group, sep = '..'),
        false = prim_dx_rc
      )
    )

  data_chd_3 <- data_chd_2 %>%
    mutate(
      stage = recode(stage,
                     '0' = 'lt2',
                     '1' = 'lt2',
                     '2' = '2plus',
                     '3' = '2plus'),
      prim_dx_rc = if_else(
        str_detect(prim_dx_rc, 'HLHS$|LVdom$'),
        paste(prim_dx_rc, stage, sep = '..'),
        prim_dx_rc
      )
    )

  mdl_ref_full <- coxph(
    formula = Surv(time, status) ~
    height_ratio +
    rtdial +
    txecmo +
    txbili_t_r +
    prim_dx_rc +
    txvent,
    data = data_ref
  )

  mdl_ref_uni <- update(mdl_ref_full, . ~ prim_dx_rc)

  mdl_chd_1_full <- update(mdl_ref_full,
                           . ~ . + prim_dx_rc,
                           data = data_chd_1)

  mdl_chd_1_uni <- update(mdl_chd_1_full, . ~ prim_dx_rc)

  mdl_chd_2_full <- update(mdl_ref_full,
                           . ~ . + prim_dx_rc,
                           data = data_chd_2)

  mdl_chd_2_uni <- update(mdl_chd_2_full, . ~ prim_dx_rc)

  mdl_chd_3_full <- update(mdl_ref_full,
                           . ~ . + prim_dx_rc,
                           data = data_chd_3)

  mdl_chd_3_uni <- update(mdl_chd_3_full, . ~ prim_dx_rc)

  counts_chd <- list(data_ref,
                     data_chd_1,
                     data_chd_2,
                     data_chd_3) %>%
    map(count, prim_dx_rc, status) %>%
    map(pivot_wider,
        names_from = status,
        values_from = n,
        names_prefix = 'status') %>%
    bind_rows() %>%
    transmute(
      term = prim_dx_rc,
      n_smry = table_glue("{status1} / {status0 + status1}")
    ) %>%
    distinct()

  estimates_chd <-
    list(ref_adj = mdl_ref_full,
         ref_uni = mdl_ref_uni,
         chd1_adj = mdl_chd_1_full,
         chd1_uni = mdl_chd_1_uni,
         chd2_adj = mdl_chd_2_full,
         chd2_uni = mdl_chd_2_uni,
         chd3_adj = mdl_chd_3_full,
         chd3_uni = mdl_chd_3_uni) %>%
    map(broom::tidy, exponentiate = TRUE, conf.int = TRUE) %>%
    map(filter, str_detect(term, '^prim_dx_rc|^stage')) %>%
    map(add_row, term = 'Cardiomyopathy') %>%
    bind_rows(.id = 'split_me') %>%
    mutate(term = str_remove(term, '^prim_dx_rc')) %>%
    separate(split_me, into = c('mdl', 'control')) %>%
    transmute(
      mdl = factor(mdl),
      mdl = fct_relevel(mdl, 'ref'),
      control,
      term,
      hr = if_else(
        term == 'Cardiomyopathy',
        true = '1 (ref)',
        false = table_glue("{estimate} ({conf.low}, {conf.high})")
      ),
      pval = if_else(p.value < 0.001, '<.001', table_value(p.value))
    ) %>%
    arrange(mdl, term) %>%
    pivot_wider(names_from = control,
                values_from = c(hr, pval)) %>%
    select(-pval_uni)

  control_footer <-
    names(labels)[labels %in% c("height_ratio",
                                "rtdial",
                                "txecmo",
                                "txbili_t_r",
                                "prim_dx_rc",
                                "txvent")] %>%
    glue_collapse(sep = ', ', last = ', and ') %>%
    tolower() %>%
    paste("Adjusted for", .) %>%
    str_replace("ecmo", "ECMO")

  tbl_chd <- estimates_chd %>%
    left_join(counts_chd) %>%
    select(mdl, term, n_smry, hr_uni, hr_adj, pval_adj) %>%
    mutate(
      mdl = recode(
        mdl,
        'ref' = 'Model 1',
        'chd1' = 'Model 2',
        'chd2' = 'Model 3',
        'chd3' = 'Model 4'
      ),
      term = str_replace(term, 'biV', 'bi-V'),
      term = str_replace(term, 'singleV', 'single-V'),
      term = str_replace(term, 'cbvS', 'CBV S'),
      term = str_replace(term, 'noS', 'no S'),
      term = str_replace(term, 'lt2', 'Stage <2'),
      term = str_replace(term, '2plus', 'Stage \u22652'),
      term = str_replace_all(term, '\\.\\.', ';\n')
    ) %>%
    as_grouped_data('mdl') %>%
    flextable::as_flextable(hide_grouplabel = TRUE) %>%
    add_header_row(
      values = c("Diagnostic group",
                 "N events / N at risk",
                 "Hazard ratio (95% CI)",
                 "P-value"),
      colwidths = c(1,1,2,1)
    ) %>%
    set_header_labels(
      term = 'Diagnostic group',
      n_smry = 'N events / N at risk',
      hr_uni = 'Unadjusted',
      hr_adj = 'Adjusted',
      pval_adj = 'P-value'
    ) %>%
    merge_v(part = 'header') %>%
    theme_box() %>%
    width(width = 1.5) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    bg(i = ~ !is.na(mdl), bg = 'grey80') %>%
    italic(i = ~ !is.na(mdl), italic = TRUE) %>%
    footnote(
      i = 2, j = 4,
      part = 'header',
      ref_symbols = 'a',
      value = as_paragraph(control_footer)
    ) %>%
    footnote(
      i = ~ term == 'Congenital HD;\nsingle-Ventricle;\nRVdom',
      j = 1,
      ref_symbols = 'b',
      part = 'body',
      value = as_paragraph(
        'Due to smaller sample size, stage was not analyzed in this group'
      )
    )

  data_ref_cmy <- data_ref %>%
    mutate(
      prim_dx_rc = factor(prim_dx_rc),
      prim_dx_rc = fct_relevel(prim_dx_rc, 'Congenital HD')
    )

  data_cmy_1 <- data_ref_cmy %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(
      prim_dx_rc = if_else(
        prim_dx_rc == 'Cardiomyopathy',
        true = paste(prim_dx_rc, sec_dx_rc, sep = '..'),
        false = prim_dx_rc
      ),
      prim_dx_rc = factor(prim_dx_rc),
      prim_dx_rc = fct_relevel(prim_dx_rc, 'Congenital HD')
    )

  mdl_ref_full <- update(mdl_ref_full, data = data_ref_cmy)
  mdl_ref_uni <- update(mdl_ref_uni, data = data_ref_cmy)

  mdl_cmy_1_full <- update(mdl_ref_full,
                           . ~ . + prim_dx_rc,
                           data = data_cmy_1)

  mdl_cmy_1_uni <- update(mdl_cmy_1_full, . ~ prim_dx_rc)

  counts_cmy <- list(data_ref,
                     data_cmy_1) %>%
    map(count, prim_dx_rc, status) %>%
    map(pivot_wider,
        names_from = status,
        values_from = n,
        names_prefix = 'status') %>%
    bind_rows() %>%
    transmute(
      term = prim_dx_rc,
      n_smry = table_glue("{status1} / {status0 + status1}")
    ) %>%
    distinct()

  estimates_cmy <-
    list(ref_adj = mdl_ref_full,
         ref_uni = mdl_ref_uni,
         chd1_adj = mdl_cmy_1_full,
         chd1_uni = mdl_cmy_1_uni) %>%
    map(broom::tidy, exponentiate = TRUE, conf.int = TRUE) %>%
    map(filter, str_detect(term, '^prim_dx_rc')) %>%
    map(add_row, term = 'Congenital HD') %>%
    bind_rows(.id = 'split_me') %>%
    mutate(term = str_remove(term, '^prim_dx_rc')) %>%
    separate(split_me, into = c('mdl', 'control')) %>%
    transmute(
      mdl = factor(mdl),
      mdl = fct_relevel(mdl, 'ref'),
      control,
      term,
      hr = if_else(
        term == 'Congenital HD',
        true = '1 (ref)',
        false = table_glue("{estimate} ({conf.low}, {conf.high})")
      ),
      pval = if_else(p.value < 0.001, '<.001', table_value(p.value))
    ) %>%
    arrange(mdl, term) %>%
    pivot_wider(names_from = control, values_from = c(hr, pval)) %>%
    select(-pval_uni)

  tbl_cmy <- estimates_cmy %>%
    left_join(counts_cmy) %>%
    mutate(
      term = factor(
        term,
        levels = c(
          "Congenital HD",
          "Cardiomyopathy",
          "Cardiomyopathy..RCM",
          "Cardiomyopathy..DCM",
          "Cardiomyopathy..OtherCM"
        )
      )
    ) %>%
    arrange(mdl, term) %>%
    select(mdl, term, n_smry, hr_uni, hr_adj, pval_adj) %>%
    mutate(
      mdl = recode(
        mdl,
        'ref' = 'Model 1',
        'chd1' = 'Model 2',
        'chd2' = 'Model 3'
      ),
      term = str_replace(term, 'biV', 'bi-V'),
      term = str_replace(term, 'singleV', 'single-V'),
      term = str_replace(term, 'cbvS', 'CBV S'),
      term = str_replace(term, 'noS', 'no S'),
      term = str_replace_all(term, '\\.\\.', ';\n')
    ) %>%
    as_grouped_data('mdl') %>%
    flextable::as_flextable(hide_grouplabel = TRUE) %>%
    add_header_row(
      values = c("Diagnostic group",
                 "N events / N at risk",
                 "Hazard ratio (95% CI)",
                 "P-value"),
      colwidths = c(1,1,2,1)
    ) %>%
    set_header_labels(
      term = 'Diagnostic group',
      n_smry = 'N events / N at risk',
      hr_uni = 'Unadjusted',
      hr_adj = 'Adjusted',
      pval_adj = 'P-value'
    ) %>%
    merge_v(part = 'header') %>%
    theme_box() %>%
    width(width = 1.5) %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    bg(i = ~ !is.na(mdl), bg = 'grey80') %>%
    italic(i = ~ !is.na(mdl), italic = TRUE) %>%
    footnote(i = 2, j = 4,
             part = 'header',
             value = as_paragraph(control_footer))

  list(chd = tbl_chd, cmy = tbl_cmy)

}
