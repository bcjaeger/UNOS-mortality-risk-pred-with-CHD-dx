#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_waitlist
#' @param data_posttxpl
check_outcomes_by_year <- function(data_waitlist, data_posttxpl) {

  data_waitlist_modi <- data_waitlist %>%
    mutate(
      year_grp = case_when(
        list_year < 2005 ~ "before 2005",
        list_year < 2010 ~ "2005-2009",
        list_year < 2015 ~ "2010-2014",
        list_year >= 2015 ~ "2015 or later"
      )
    )

  incidence <- cuminc(ftime   = data_waitlist_modi$time,
                      fstatus = data_waitlist_modi$status,
                      group   = data_waitlist_modi$year_grp,
                      cencode = 0)

  data_gg <- incidence %>%
    map_dfr(~as.data.frame(.x), .id = 'year_grp') %>%
    select(year_grp, time, est, var) %>%
    filter(year_grp != 'Tests') %>%
    mutate(year_grp = str_remove(year_grp, ' \\d$'),
           ci_lwr = est + sqrt(var) * qnorm(0.025),
           ci_upr = est + sqrt(var) * qnorm(0.975),
           year_grp = fct_reorder2(
             .f = factor(year_grp),
             .x = time,
             .y = est,
           )) %>%
    as_tibble()

  fig_waitlist <- ggplot(data_gg, aes(x = time, y = est, col = year_grp)) +
    geom_line() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 9),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.key.size = unit(2/3, 'cm')) +
    labs(y = glue('Cumulative incidence of '),
         x = glue('Time since , years')) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1/2))

  data_posttxpl_modi <- data_posttxpl %>%
    mutate(
      year_grp = case_when(
        txpl_year < 2005 ~ "before 2005",
        txpl_year < 2010 ~ "2005-2009",
        txpl_year < 2015 ~ "2010-2014",
        txpl_year >= 2015 ~ "2015 or later"
      )
    )

  incidence <- cuminc(ftime   = data_posttxpl_modi$time,
                      fstatus = data_posttxpl_modi$status,
                      group   = data_posttxpl_modi$year_grp,
                      cencode = 0)

  data_gg <- incidence %>%
    map_dfr(~as.data.frame(.x), .id = 'year_grp') %>%
    select(year_grp, time, est, var) %>%
    filter(year_grp != 'Tests') %>%
    mutate(year_grp = str_remove(year_grp, ' \\d$'),
           ci_lwr = est + sqrt(var) * qnorm(0.025),
           ci_upr = est + sqrt(var) * qnorm(0.975),
           year_grp = fct_reorder2(
             .f = factor(year_grp),
             .x = time,
             .y = est,
           )) %>%
    as_tibble()

  fig_posttxpl <- ggplot(data_gg, aes(x = time, y = est, col = year_grp)) +
    geom_line() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 9),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.key.size = unit(2/3, 'cm')) +
    labs(y = glue('Cumulative incidence of '),
         x = glue('Time since , years')) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1/2))

  list(waitlist = fig_waitlist,
       posttxpl = fig_posttxpl)

}
