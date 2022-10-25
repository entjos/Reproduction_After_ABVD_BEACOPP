#//////////////////////////////////////////////////////////////////////////////
# Project: Fertility in BEACOPP trt. HL survivros
# File: Estimate Cox model for first childbirth
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
fe_st_data <- readRDS("./data/analysis_data/fe_st_data.RData")

# 1. Estimate Cox model -------------------------------------------------------

#   1.1 Unadjusted ============================================================

cox_unadjusted <- coxph(Surv(st_yrs, exit_event == "CB") ~ 
                          female                   +
                          female:ctreg_2_4_beacopp + 
                          female:ctreg_6_8_beacopp,
                        data = fe_st_data)

summary(cox_unadjusted)

#   1.2 Adjusted ==============================================================

cox_adjusted <- coxph(Surv(st_yrs, exit_event == "CB") ~ 
                        female                   +
                        female:ctreg_2_4_beacopp + 
                        female:ctreg_6_8_beacopp +
                        age_dx_c3  + 
                        stage_c3   + 
                        year_dx_c5 + 
                        perfwho_c3 +
                        nulliparous,
                      data = fe_st_data)

summary(cox_adjusted)

# Test for TVC
cox.zph(cox_adjusted)

#   1.3 Stratified model ======================================================

cox_adjusted_strat <- coxph(Surv(st_yrs, exit_event == "CB") ~ 
                              female                   +
                              female:ctreg_2_4_beacopp + 
                              female:ctreg_6_8_beacopp +
                              perfwho_c3 +
                              strata(age_dx_c3, stage_c3, 
                                     nulliparous, country,
                                     year_dx_c5),
                            data = fe_st_data)

summary(cox_adjusted_strat)

# Test for TVC
test_tvc <- cox.zph(cox_adjusted_strat)$table %>%
  as.data.frame() %>% 
  rownames_to_column()

test_tvc

#   1.4 Test for interaction ==================================================

cox_interaction <- coxph(Surv(st_yrs, exit_event == "CB") ~ 
                           female * ctreg_2_4_beacopp + 
                           female * ctreg_6_8_beacopp +
                           perfwho_c3 +
                           strata(age_dx_c3, stage_c3, 
                                  nulliparous, country,
                                  year_dx_c5),
                         data = fe_st_data)

summary(cox_interaction)

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

#   2.1 Save in extra table ===================================================

save_to_excel_wb(compare_models,
                 wb_path = "./tables/fe_cox_models.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(test_interactions,
                 wb_path = "./tables/fe_cox_models.xlsx",
                 sheet_name = "r_output_test_interactions")

#   2.2 Save in table 3 =======================================================

save_to_excel_wb(compare_models,
                 wb_path = "./tables/table3.xlsx",
                 sheet_name = "r_output_cox_model")

save_to_excel_wb(test_interactions,
                 wb_path = "./tables/table3.xlsx",
                 sheet_name = "r_output_test_interactions")

#   2.3 Save Schoenefeld residuals ============================================

save_to_excel_wb(test_tvc,
                 wb_path = "./tables/fe_tvc_test.xlsx",
                 sheet_name = "r_output")

#   2.4 Save Median Follow-up times ===========================================

sink("./tables/fe_mean_fup.txt")

by(fe_st_data$st_yrs,
   fe_st_data$ctreg_c4,
   summary)

sink()

# /////////////////////////////////////////////////////////////////////////////
# END OF R-FILE