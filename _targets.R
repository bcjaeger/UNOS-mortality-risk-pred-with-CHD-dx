## Load your packages, e.g. library(targets).
source("./packages.R")

# TODO: -------------------------
# make calibration slope plots
# -------------------------------

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## Map over the UNOS model to the PHTS cohort
## see if adding some diagnostic variables to
## the UNOS model increases its prediction accuracy

## MCCV
## start with the full dataset:
### split it into training/testing
### use the training set to develop 1 or more models
### validate all of the models in the testing set
### repeat 100 times with different training/testing splits
### aggregate the results to see which modeling strategy validates the best
## we can't say one of these models is good, but we can say
## that the procedure used to develop a model is good.

## Referent model contains the standard UNOS variables
## Expanded model contains the referent model + CHD variables

## Compute three things:
### AUC for discrimination
### Mis-calibration p-value; lower values indicate more evidence for bad calibration
### IPA (index of prediction accuracy); analogous to R2 statistics

### (not implemented)
### NRI (net re-classification index); would require a specific probability
### cut-point that would be valid in clinical practice
### Computes positive NRI: P(incorrect positive case -> correct positive case)
### using the new model versus the old model
### Computes negative NRI: P(incorrect negative case -> correct negative case)
### using the new model versus the old model
### Overall NRI is the sum of positive and negative NRIs; anything > 0
### indicates a net benefit, but clinical relevance is hard to gauge

## Modeling procedure
### Fit the UNOS model using all patient data in training set
### get predicted values from UNOS model for all patient data in training set
### Fit the group-specific models as follows:
#### Cardiomyopathy: Outcome ~ prediction from UNOS model + sec_dx_rc

## other modeling procedure?
### Overall model that has interactions without supporting main effects
### Death ~ UNOS variables + Cardiomyopathy * sec_dx_rc

### TODO
### Inference; consider all patients
### Death ~ UNOS + Cardiomyopathy * sec_dx_rc + CHD * (stage + surg)

# meeting with ryan on 9/30/2021 ----

## age categorized as
# -- 0 - < 1 yr,
# -- 1 - < 5 yr,
# -- >= 5 yr

## blood type
# -- Oa if bt == 'O' & age < 2 yr
# -- Ob if bt == 'O' & age >= 2 yr
# -- others

## weight categorized as
# -- 0 to < 5 kg,
# -- 5 to < 20 kg,
# -- >= 20 kg

## bsa categorized as
# -- 0 to < 1/2 m2
# -- >= 1/2

## race
# -- caucasian versus non-caucasian

## eGFR instead of serum creatinine (egfr_txpl)
# 0 - < 30 or on dialysis
# 30 - < 60
# >= 60

# For models, remove serum creatinine and replace it with eGFR
# Insert the variables above if they are not there
# drop weight and height and keep bsa

# Ryan hypothesizes interactions
# -- cardiomyopathy diagnosis interacts with egfr and txvent

# end meeting notes

# start plan ----

tar_plan(

  rspec = round_spec() %>%
    round_using_magnitude(digits = c(2,1,1,0),
                          breaks = c(1, 10, 100, Inf)) %>%
    round_half_even(),

  features_ref_waitlist = c(
    "Age at time of listing, years" = "age_listing",
    "Blood Type" = "rabotype",
    "Medical history at Listing, Neurologic" = "hxneuro",
    "Patient Dialysis" = "dialysis",
    "ECMO at Listing" = "slecmo",
    "Recipient Sex" = "sex",
    "Height at Listing, cm" = "height_listing_cm",
    "Recipient Temporary Device IABP" = "iabp",
    "Recipient on Life Support" = "life_support",
    "Recipient Other Life Support" = "other_life_support",
    "Most Recent Serum Creatinine" = "lcreat_r",
    "Pulmonary Wedge Pressure, mean" = "lsbpcw",
    "Recipient stent or prostaglandin infusion" = "slddpsc",
    "Congenital surgery prior to listing" = "prior_congenital_surgery",
    "Recipient Prior to Listing Pam" = "lsbpam",
    "Recipient Hispanic or Latino" = "hisp",
    "Recipient Race" = "race",
    "Recipient on Ventilator" = "slvent",
    "Weight at Listing, kg" = "weight_listing_kg",
    "Prior Heart Transplant Failure" = "prior_txpl_failure"
  ),

  features_ref_posttxpl = c(
    "Donor to Recipient Height Ratio" = "height_ratio",
    "Recipient Dialysis since Listing" = "rtdial",
    "Recipient ECMO at Transplant" = "txecmo",
    "Recipient Most Recent Total Bilirubin at Transplant" = "txbili_t_r",
    "Recipient Primary Diagnosis" = "prim_dx_rc",
    "Recipient Ventilator at Transplant" = "txvent"
  ),

  features_chd = c(
    "Recipient Primary Diagnosis"  = "prim_dx_rc",
    "Cardiomyopathy Type" = "sec_dx_rc",
    "CHD Single Ventricle" = "chd_sv_rc",
    "CHD Single Ventricle Dominant Ventricle" = "chd_sv_group",
    "CHD Single Ventricle Stage" = "stage",
    "CHD Bi-Ventricle Cardiac Surgery" = "chd_bv_surg"
  ),

  labels = c(features_ref_waitlist,
             features_ref_posttxpl,
             features_chd),

  data_waitlist_init = load_phts(
    lsbpcw = parse_double(lsbpcw, na = c("N")),
    lsbpam = parse_double(lsbpam, na = c("N")),
    sex = factor(sex,
                 levels = c("F","M"),
                 labels = c("Female", "Male")),
    race = case_when(rrace_b == 1 ~ "Black",
                     rrace_w == 1 ~ "White",
                     TRUE ~ "Other"),
    race = factor(race, levels = c("White", "Black", "Other")),
    date = '2021-09-21',
    fname = 'PHTS_waitlist_model.csv',
    time = 'waitlist_outcome_interval',
    status = 'waitlist_outcome_death',
    features = c(
      # reference features
      "age_listing",
      "rabotype",
      "hxneuro",
      "dialysis",
      "slecmo",
      "sex",
      "height_listing_cm",
      "iabp",
      "life_support",
      "other_life_support",
      "lcreat_r",
      "lsbpcw",
      "slddpsc",
      "prior_congenital_surgery",
      "lsbpam",
      "rrace_ai",
      "rrace_a",
      "rrace_b",
      "hisp",
      "rrace_pi",
      "rrace_w",
      "slvent",
      "weight_listing_kg",
      "prior_txpl_failure",
      "prim_dx_rc",
      # chd diagnosis features
      "sec_dx_rc",
      "chd_sv_rc",
      "chd_sv_group",
      "stage",
      "chd_bv_surg",
      "age_cat",
      "blood_type_cat",
      "weight_cat",
      "bsa_cat",
      "race_cat",
      "egfr_listing_cat",
      # listing year (for sanity check)
      "list_year"
    ),
    admin_censor_years = 1
  ) %>%
    select(-starts_with('rrace')) %>%
    rename(egfr_cat = egfr_listing_cat),

  data_posttxpl_init = load_phts(
    date = '2021-09-21',
    fname = 'PHTS_posttxpl_model.csv',
    time = 'int_graft_loss',
    status = 'graft_loss',
    features = c(
      "height_ratio",
      "rtdial",
      "txecmo",
      "txbili_t_r",
      "prim_dx_rc",
      "txvent",
      # chd diagnosis features
      "sec_dx_rc",
      "chd_sv_rc",
      "chd_sv_group",
      "stage",
      "chd_bv_surg",
      "age_txpl_cat",
      "blood_type_cat",
      "weight_txpl_cat",
      "bsa_txpl_cat",
      "race_cat",
      "egfr_txpl_cat",
      # listing year (for sanity check)
      "txpl_year"
    ),
    admin_censor_years = 1
  ) %>%
    rename(age_cat = age_txpl_cat,
           blood_type_cat = blood_type_cat,
           weight_cat = weight_txpl_cat,
           bsa_cat = bsa_txpl_cat,
           egfr_cat = egfr_txpl_cat),

  # make the cohorts more contemporary

  data_waitlist = filter(data_waitlist_init,
                         list_year >= 2010),

  data_posttxpl = filter(data_posttxpl_init,
                         txpl_year >= 2010),

  folds_waitlist = make_mccv_folds(data_waitlist,
                                   train_proportion = 2/3,
                                   n_runs = 250),

  folds_posttxpl = make_mccv_folds(data_posttxpl,
                                   train_proportion = 2/3,
                                   n_runs = 250),

  # count the number of graft loss cases that are re-transplant
  # among post-txpl events, how many were re-transplants?
  # stat_txpl2 = summarize_retransplant(data_posttxpl),

  tar_target(
    mccv_output_waitlist,
    make_mccv_output(
      data = data_waitlist,
      features_ref = features_ref_waitlist,
      times = 1,
      run = folds_waitlist$run,
      training_index = folds_waitlist$training_index
    ),
    pattern = map(folds_waitlist)
  ),

  tar_target(
    mccv_output_posttxpl,
    make_mccv_output(
      data = data_posttxpl,
      features_ref = features_ref_posttxpl,
      times = 1,
      run = folds_posttxpl$run,
      training_index = folds_posttxpl$training_index
    ),
    pattern = map(folds_posttxpl)
  ),

  tbl_mccv = tabulate_mccv_output(waitlist = mccv_output_waitlist,
                                  posttxpl = mccv_output_posttxpl,
                                  rspec = rspec,
                                  admin_censor_posttxpl = 1,
                                  admin_censor_waitlist = 1),

  tbl_mdl_waitlist = tabulate_mdl_waitlist(data_waitlist, labels),

  tbl_mdl_posttxpl = tabulate_mdl_posttxpl(data_posttxpl, labels),

  fig_kaplan_waitlist = visualize_kaplan(data_waitlist,
                                         label_outcome = 'death',
                                         label_landmark = 'listing'),

  fig_kaplan_posttxpl = visualize_kaplan(data_posttxpl,
                                         label_outcome = 'graft loss',
                                         label_landmark = 'transplant'),

  fig_kaplan_by_year = check_outcomes_by_year(data_waitlist,
                                              data_posttxpl),


  tbl_chars_waitlist = tabulate_characteristics(
    data_waitlist,
    labels = labels,
    status_label = "1-year waitlist morality"
  ),

  tbl_chars_posttxpl = tabulate_characteristics(
    data_posttxpl,
    labels = labels,
    status_label = "1-year transplant Mortality or graft loss"
  ),

  tar_render(report, "doc/report.Rmd")

)


