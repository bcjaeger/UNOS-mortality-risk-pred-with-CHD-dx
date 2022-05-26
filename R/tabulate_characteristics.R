#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param features
tabulate_characteristics <- function(data,
                                     labels,
                                     status_label) {


  # add labels to data before using gtsummary
  blank_names <- names(labels) == ''
  names(labels)[blank_names] <- labels[blank_names]

  for(f in seq_along(labels)) {
    if(labels[f] %in% names(data)){
      attr(data[[labels[f]]], 'label') <- names(labels[f])
    }

  }

  tbl_gtsum <- data %>%
    mutate(
      status = factor(status, levels = c(0,1), labels = c("No", "Yes")),
    ) %>%
    select(-time) %>%
    tbl_summary(
      by = status,
      missing = 'no',
      statistic = list(all_categorical() ~ "{n} ({p})")
    ) %>%
    add_overall() %>%
    modify_footnote(all_stat_cols() ~ NA)

  tbl_flex <- gtsummary::as_flex_table(tbl_gtsum)  %>%
    add_header_row(
      values = c('Characteristic',
                 '',
                 status_label),
      colwidths = c(1, 1, 2)
    ) %>%
    theme_box() %>%
    delete_part('footer') %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    merge_v(j = c(1), part = 'header') %>%
    fontsize(size = 12, part = 'all') %>%
    width(width = 1.4) %>%
    width(j = 1, width = 1.5) %>%
    footnote(
      i = 1,
      j = 1,
      part = 'header',
      value = as_paragraph(
        'Table values are median (25th percentile, 75th percentile)',
        ' and percent for continuous and categorical',
        ' variables, respectively.'
      )
    )

  tbl_inline <- tbl_gtsum$table_body %>%
    select(variable, label, starts_with('stat')) %>%
    mutate(
      label = if_else(label == variable,
                      true = NA_character_,
                      false = label)
    ) %>%
    rename(overall = stat_0,
           No = stat_1,
           Yes = stat_2) %>%
    as_inline(tbl_variables = c('variable', 'label'),
              tbl_values = c('overall', 'No', 'Yes'))

  list(
    table = tbl_flex,
    inline = tbl_inline
  )

}


