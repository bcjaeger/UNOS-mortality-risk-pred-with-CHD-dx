

#' Diagnosis Variable Parent Child Summary
#' If PRIM_DX_RC = “Cardiomyopathy”
#' o	Value for SEC_DX_RC (type of cardio myopathy)
#' o	If cardiomyopathy and sec_dx_rc is missing, leave it missing
#' o	If not cardio, set sec_dx_rc to ‘None’
#' If PRIM_DX-RC =”Congenital HD”
#' o	value for CHD_SV_RC (1 = single ventricle CHD, 0 = bi-ventricle CHD)
#' o	same logic as above
#' If CHD_SV_RC = 1
#' o	Value for CHD_SV_GROUP (dominate ventricle for CHD single ventricle patients)
#' o	Same logic as above
#' o	Value for STAGE (Staging for CHD Single ventricle patient)
#' o	Same logic as above
#' If CHD_SV_RC = 0
#' o	Value for CHD_BV_SURG
#' o  1 = Congenital Surgery for Biventricular CHD pts; cbvSurgery
#' o  2 = noSurgery
#' o	Same logic as above
#'
#' @title
#' @param data
fill_parent_child_values <- function(data) {

  data
    #     ,
    #   # impute to the mode if bi-ventricle
    #   chd_sv_group = if_else(
    #     is.na(chd_sv_group) & chd_sv_rc == "biVentricle",
    #     mode(chd_sv_group),
    #     false = chd_sv_group
    #   ),
    #   # impute to the mode if bi-ventricle
    #   stage = if_else(
    #     is.na(stage) & chd_sv_rc == 'biVentricle',
    #     true = mode(stage),
    #     false = as.character(stage)
    #   )
    # ) %>%
    # unite(chd_sv_rc, stage, chd_sv_group, chd_bv_surg,
    #       na.rm = TRUE,
    #       col = 'chd_v',
    #       remove = FALSE) %>%
    # mutate(
    #   sec_dx_rc = replace(sec_dx_rc, is.na(sec_dx_rc), ''),
    #   sec_dx = paste(sec_dx_rc, chd_v, sep = ''),
    #   sec_dx = replace(sec_dx, sec_dx == '', "unknown"),
    #   sec_dx = replace(sec_dx, prim_dx_rc == 'Myocarditis', ""),
    #   prim_dx_rc = str_replace(prim_dx_rc, ' ', ''),
    #   prim_dx_expanded = paste(prim_dx_rc, sec_dx, sep = '_'),
    #   prim_dx_expanded = str_remove(prim_dx_expanded, '_$'),
    #   prim_dx_expanded = recode(prim_dx_expanded, 'Other_unknown' = 'otherDx')
    # ) %>%
    # select(-c(sec_dx_rc, chd_sv_group, chd_sv_rc, stage, chd_bv_surg))

}



## the old way
# mutate(
#   data,
#   sec_dx_rc = replace(
#     sec_dx_rc,
#     prim_dx_rc != 'Cardiomyopathy',
#     'None'
#   ),
#   chd_sv_rc = replace(
#     chd_sv_rc,
#     prim_dx_rc != 'Congenital HD',
#     'None'
#   ),
#   chd_sv_rc = recode(
#     chd_sv_rc,
#     "1" = "Single_ventricle_CHD",
#     "0" = "Bi_ventricle_CHD"
#   ),
#   chd_sv_group = replace(
#     chd_sv_group,
#     chd_sv_rc != "Single_ventricle_CHD",
#     'None'
#   ),
#   stage = replace(
#     stage,
#     chd_sv_rc != "Single_ventricle_CHD",
#     "None"
#   ),
#   chd_bv_surg = replace(
#     chd_bv_surg,
#     chd_sv_rc != "Bi_ventricle_CHD",
#     "None"
#   ),
#   chd_bv_surg = recode(
#     chd_bv_surg,
#     '0' = "No",
#     '1' = "Yes"
#   ),
#   chd_bv_surg = factor(chd_bv_surg,
#                        levels = c("No", "Yes", "None")),
#   chd_sv_rc = factor(chd_sv_rc,
#                      levels = c("Single_ventricle_CHD",
#                                 "Bi_ventricle_CHD",
#                                 "None"))
# )
