#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_waitlist
visualize_kaplan <- function(data,
                             label_outcome,
                             label_landmark,
                             ymax = 0.4) {

  incidence <- cuminc(ftime   = data$time,
                      fstatus = data$status,
                      cencode = 0)

  data_gg <- incidence %>%
    map_dfr(~as.data.frame(.x[c('time', 'est', 'var')])) %>%
    mutate(ci_lwr = est + sqrt(var) * qnorm(0.025),
           ci_upr = est + sqrt(var) * qnorm(0.975)) %>%
    as_tibble()

  ggplot(data_gg, aes(x = time, y = est, ymin = ci_lwr, ymax = ci_upr)) +
    geom_line() +
    geom_ribbon(alpha = 0.1, linetype = 3) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 9),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.key.size = unit(2/3, 'cm')) +
    labs(y = glue('Cumulative incidence of {label_outcome}'),
         x = glue('Time since {label_landmark}, years')) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, ymax))

}
