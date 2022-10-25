#//////////////////////////////////////////////////////////////////////////////
# Project: Fertility in BEACOPP trt. HL survivros
# File: Create descriptive tables
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(lubridate)
library(openxlsx)
library(dplyr)
source("./programs_and_log_files/user_defined_functions/summary_table.R")
source(paste0("./programs_and_log_files/user_defined_functions/",
              "summary_table_to_workbook.R"))

# Import merged dataset
bcppfert_d <- readRDS("./data/merged_data/bcppfert_d.RData")

# Import survival datasets
fe_st_data <- readRDS("./data/analysis_data/fe_st_data.RData")
re_st_data <- readRDS("./data/analysis_data/re_st_data.RData")

# 2. Prepare datasets ---------------------------------------------------------

#   2.1 Merged dataset ========================================================

bcppfert_d <- bcppfert_d %>% 
  mutate(end_of_fup = case_when(country == "SE"  ~ ymd("2019-12-31"),
                                country == "NOR" ~ ymd("2017-12-31"),
                                country == "DK"  ~ ymd("2021-06-30")),
         exit_dt    = pmin(cb_1_dt, sct_dt, death_dt, end_of_fup,
                           na.rm = TRUE),
         st_yrs     = time_length(dx_dt %--% exit_dt,
                                  unit = "year"),
         exit_event_cb = if_else(!is.na(cb_1_dt) & exit_dt == cb_1_dt, 1, 0),
         female        = as.factor(female))

#   2.2 Data for RC table =====================================================

# Get no. subsequent childbirth and total time of follow-up
re_st_data <- re_st_data %>% 
  group_by(lopnr) %>% 
  summarise(no_exit_event_cb = sum(exit_event == "CB"),
            st_yrs_re        = max(st_yrs_end))

# Add information for FE dataset
fe_st_data <- fe_st_data %>% 
  left_join(re_st_data, by = "lopnr")

# 3. Create tables  -----------------------------------------------------------

#   3.1 Table 1 ===============================================================

table1_by_country <- summary_table(subset(bcppfert_d, case == 1),
                                   vars = c("age_dx_c3", "year_dx_c5", 
                                            "stage_c3", "perfwho_c3",
                                            "ctreg_c4", "no_cb_c5", 
                                            "nulliparous"),
                                   strata = c("country", "female"))

summary_table_to_workbook("./tables/table1_by_country.xlsx",
                          table1_by_country)

table1_overall <- summary_table(subset(bcppfert_d, case == 1),
                                vars = c("age_dx_c3", "year_dx_c5", 
                                         "stage_c3", "perfwho_c3",
                                         "ctreg_c4", "no_cb_c5", 
                                         "nulliparous", "country",
                                         "female"),
                                strata = c("female"),
                                overall = TRUE)

summary_table_to_workbook("./tables/table1_overall.xlsx",
                          table1_overall)

#   3.2 Table 2 ===============================================================

table2 <- summary_table(subset(bcppfert_d, case == 1),
                        vars = c("age_dx_c3", "year_dx_c5", 
                                 "stage_c3", "perfwho_c3",
                                 "no_cb_c5", "country", "female",
                                 "nulliparous", "ctreg_c4"),
                        strata = c("ctreg_c4"),
                        overall = TRUE,
                        get_rates = TRUE,
                        event = "exit_event_cb",
                        st = "st_yrs")

summary_table_to_workbook("./tables/table2.xlsx",
                          table2)

#   3.3 Table 3 ===============================================================

table3 <- summary_table(fe_st_data,
                        vars = c("age_dx_c3", "year_dx_c5", "year_dx_c4",
                                 "stage_c3", "perfwho_c3",
                                 "no_cb_c5", "country", "female",
                                 "nulliparous", "ctreg_c4"),
                        strata = c("ctreg_c4"),
                        overall = TRUE,
                        get_rates = TRUE,
                        event = "exit_event_cb",
                        st = "st_yrs")

summary_table_to_workbook("./tables/table3.xlsx",
                          table3)

#   3.4 Create Table 3 for recurrent event analysis ===========================

table3_re <- summary_table(fe_st_data,
                           vars = c("age_dx_c3", "year_dx_c5", "year_dx_c4",
                                    "stage_c3", "perfwho_c3",
                                    "no_cb_c5", "country", "female",
                                    "nulliparous", "ctreg_c4"),
                           strata = c("ctreg_c4"),
                           overall = TRUE,
                           get_rates = TRUE,
                           event = "no_exit_event_cb",
                           st = "st_yrs_re")

summary_table_to_workbook("./tables/table3_re.xlsx",
                          table3_re)

#   3.4 Table of chemotherapy treatments ======================================

table_ctreg <- lapply(c("SE", "NOR", "DK"), 
                      FUN = function(lvlcountry){
                        
                        df <- bcppfert_d %>% 
                          filter(country == lvlcountry) %>% 
                          count(ctreg, .drop = FALSE) %>% 
                          mutate(prop = n/sum(n))
                        
                      }) %>% 
  setNames(c("SE", "NOR", "DK"))

summary_table_to_workbook("./tables/table_ctreg.xlsx",
                          table_ctreg)

#'/////////////////////////////////////////////////////////////////////////////
#'NOTES
#'
#'/////////////////////////////////////////////////////////////////////////////
#'END OF R-FILE