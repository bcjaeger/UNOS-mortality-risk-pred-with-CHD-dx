

source("packages.R")

hrdd_bhaz <- read_csv("2021-09-21/hrddpegs1y_202105_baseline_hazard.csv")

hrdd_coef <- read_csv('2021-09-21/hrddpegs1y_202105_coefficients.csv')

tar_load(data_posttxpl)

left_ls <- function(x, cutpoint){
  pmax(0, cutpoint - x)
}

right_ls <- function(x, cutpoint){
  pmax(0, x - cutpoint)
}

haz <- hrdd_bhaz |>
  filter(time == 365) |>
  pull(cumulative_hazard)

hrdd_surv <- data_posttxpl |>
  mutate(
    hrdd = 0,
    hrdd = if_else(
      prim_dx_rc == 'Cardiomyopathy' | is.na(prim_dx_rc),
      true = hrdd - 0.445840357,
      false = hrdd
    ),
    hrdd = if_else(
      is.na(height_ratio),
      true = hrdd - 0.4268012,
      false = hrdd +
        left_ls(height_ratio, 1.3) * (-0.6167795) +
        left_ls(height_ratio, 1.4) * (-0.2481113) +
        right_ls(height_ratio, 0.95) * (0.5931442)
    ),
    hrdd = if_else(
      is.na(txbili_t_r),
      true = hrdd,
      false = hrdd +
        right_ls(txbili_t_r, 1.4) * (0.006712261) +
        right_ls(txbili_t_r, 1.6) * (0.024290914) +
        right_ls(txbili_t_r, 1.8) * (0.159456576)
    ),
    hrdd = if_else(
      is.na(txvent),
      true = hrdd,
      false = hrdd + (txvent == 'Yes') * 0.5774626
    ),
    prob = exp(-exp(hrdd) * (haz)),
    risk = 1-prob,
    risk = if_else(risk == 1, 0.999, risk)
  ) |>
  select(time, status, linpred = hrdd, prob, risk)

mdl <- coxph()

hrdd_surv

library(survival.calib)

cal <- scalib_initiate(list(hrdd = hrdd_surv$risk),
                       pred_horizon = 1,
                       event_status = data_posttxpl$status,
                       event_time = data_posttxpl$time) |>
  scalib_test_gnd() |>
  scalib_slope_hare()

library(riskRegression)

Score(object = list(hrdd = hrdd_surv$risk),
      formula = Surv(time, status) ~ 1,
      data = data_posttxpl,
      times = 0.99)

data_segment <- predrisk_bin_segments(cal$data_inputs$pred_risk,
                                      event_status = data_posttxpl$status,
                                      event_time = data_posttxpl$time,
                                      time_predict = 1,
                                      bin_count = 100,
                                      bin_yintercept = -0.1,
                                      x_min = 0,
                                      x_max = 0.33,
                                      bin_length = 1)

gg_data <- cal |>
  getElement("data_outputs") |>
  select(.pred_risk, hare_data) |>
  unnest(hare_data)

fig_cal_slope <- ggplot(gg_data) +
  aes(x = predicted, y = observed) +
  geom_line(size = 1.2) +
  geom_abline(col = 'grey', linetype = 2, intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0,.33),
                     expand = c(0,0),
                     breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10),"%")) +
  scale_y_continuous(limits = c(-0.21, 0.33),
                     breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10),"%")) +
  geom_segment(data = data_segment,
               inherit.aes = FALSE,
               size = 2.8,
               color = 'grey',
               mapping = aes(x = x,
                             y = y,
                             xend = xend,
                             yend = yend)) +
  theme_bw()




