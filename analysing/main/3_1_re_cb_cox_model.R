#//////////////////////////////////////////////////////////////////////////////
# Project: Fertility in BEACOPP trt. HL survivros
# File: Estimate AG-Cox model for recurrent childbirth
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(tibble)
library(data.table)
library(purrr)
library(lubridate)
library(openxlsx)
library(survival)
library(Epi)
library(broom)

source(paste0("./programs_and_log_files/user_defined_functions/",
              "compare_model_estimates.R"))

source(paste0("./programs_and_log_files/user_defined_functions/",
              "save_to_excel_wb.R"))

# Import analysis data
re_st_data <- readRDS("./data/analysis_data/re_st_data.RData")

# 1. Estimate Cox model -------------------------------------------------------

# Set up surv object

re_cb_surv <- Surv(re_st_data$st_yrs_strt, 
                   re_st_data$st_yrs_end, 
                   re_st_data$exit_event == "CB", 
                   type = "counting")

#   1.1 Unadjusted ============================================================

cox_unadjusted <- coxph(re_cb_surv ~ 
                          female                   +
                          female:ctreg_2_4_beacopp + 
                          female:ctreg_6_8_beacopp,
                        cluster = lopnr,
                        robust  = TRUE,
                        data = re_st_data)

summary(cox_unadjusted)

#   1.2 Adjusted ==============================================================

cox_adjusted <- coxph(re_cb_surv ~ 
                        female                   +
                        female:ctreg_2_4_beacopp + 
                        female:ctreg_6_8_beacopp +
                        age_dx_c3   + 
                        stage_c3    + 
                        year_dx_c5  + 
                        perfwho_c3  +
                        nulliparous,
                      cluster = lopnr,
                      robust  = TRUE,
                      data = re_st_data)

summary(cox_adjusted)

# Test for TVC
cox.zph(cox_adjusted)

#   1.3 Stratified model ======================================================

cox_adjusted_strat <- coxph(re_cb_surv ~ 
                              female                   +
                              female:ctreg_2_4_beacopp + 
                              female:ctreg_6_8_beacopp +
                              perfwho_c3  +
                              strata(age_dx_c3, stage_c3, 
                                     nulliparous, country,
                                     year_dx_c5),
                            cluster = lopnr,
                            robust  = TRUE,
                            data = re_st_data)

summary(cox_adjusted_strat)

# Test for TVC
cox.zph(cox_adjusted_strat)

#   1.4 Test for interaction ==================================================

cox_interaction <- coxph(re_cb_surv ~ 
                           female                     +
                           female * ctreg_2_4_beacopp + 
                           female * ctreg_6_8_beacopp +
                           stage_c3   + 
                           year_dx_c5 + 
                           perfwho_c3 +
                           strata(age_dx_c3, nulliparous, country),
                         cluster = lopnr,
                         robust  = TRUE,
                         data = re_st_data)

# Test for significance of interaction with sex
test_interactions <- Wald(cox_interaction, 
                          subset = "female1:") %>% 
  as.data.frame() %>% 
  rownames_to_column("Estimate")

#   1.5 Compare adjusted and unadjusted estimates =============================

compare_models <- 
  compare_model_estimates(list(cox_unadjusted     = cox_unadjusted,
                               cox_adjusted       = cox_adjusted,
                               cox_adjusted_strat = cox_adjusted_strat))

# 2. Save estimates in excel file ---------------------------------------------

#   2.1 All estimates =========================================================

save_to_excel_wb(compare_models,
                 wb_path = "./tables/re_cox_models.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(test_interactions,
                 wb_path = "./tables/re_cox_models.xlsx",
                 sheet_name = "r_output_test_interactions")

#   2.2 Estimates for table 3 =================================================

save_to_excel_wb(compare_models,
                 wb_path = "./tables/table3_re.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(test_interactions,
                 wb_path = "./tables/table3_re.xlsx",
                 sheet_name = "r_output_test_interactions")

# /////////////////////////////////////////////////////////////////////////////
# END OF R-FILE