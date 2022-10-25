#//////////////////////////////////////////////////////////////////////////////
# Project: Fertility in BEACOPP trt. HL survivros
# File: Prepare data for survival analysis
#
#//////////////////////////////////////////////////////////////////////////////

# 1. Prefix -------------------------------------------------------------------

# clear memory
rm(list = ls())

# load packages
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)

# Load exclusion table package
source("./programs_and_log_files/user_defined_functions/exclusion_table.R",
       echo = FALSE)

# Import merged data sets
index_pop <- readRDS("./data/merged_data/index_pop.RData")

# 1. Exclude obs. with missing information ------------------------------------

exclusions <- exclusion_table(index_pop,
                              exclusion_criteria = c("case       == 0        ",
                                                     "nhl_sub    == 'Missing'",
                                                     "age_dx_c3  == 'Missing'",
                                                     "year_dx_c5 == 'Missing'",
                                                     "year_dx_c5 == 'Other'  "))

# Print No excluded
exclusions$table_ex

# Save cleaned dataset
st_data_cases <- exclusions$dataset

# obtain comparators
st_data_comp  <- index_pop[case == 0 & riskset %in% st_data_cases$riskset]
cat("Obtained", nrow(st_data_comp), "comparators from the merged",
    "index population\n")

# Save no. excluded cases and included comparators
wb <- loadWorkbook("./tables/n_excluded_study_sample.xlsx")

writeData(wb    = wb,
          sheet = "r_output_cases",
          x     = exclusions$table_ex)

writeData(wb    = wb,
          sheet = "r_output_comparators",
          x     = data.frame(label = "No. comparators included",
                             n     = nrow(st_data_comp)))

saveWorkbook(wb        = wb,
             file      = "./tables/n_excluded_study_sample.xlsx",
             overwrite = TRUE)

# Combine comparators and cases
st_data <- rbindlist(list(st_data_cases, st_data_comp))

# 1.1 Drop unused factor levels ===============================================

st_data <- droplevels(st_data)

# 2. Define dummy variables ---------------------------------------------------

st_data <- st_data %>%  
  fastDummies::dummy_cols(select_columns = c("nhl_sub_c5", "nhl_sub_c7", 
                                             "year_dx_c5", "age_dx_c3", 
                                             "country", "female", "educ_max_num",
                                             "case")) %>% 
  rename_with(~ gsub("-", "_", .x))

# 3. Define risksets for NHL subtypes -----------------------------------------

st_data <- st_data %>% 
  mutate(nhl_sub_c5_risksets = na_if(nhl_sub_c5, "Comp"),
         nhl_sub_c7_risksets = na_if(nhl_sub_c7, "Comp")) %>% 
  group_by(riskset) %>% 
  arrange(desc(case)) %>% 
  fill(nhl_sub_c5_risksets, nhl_sub_c7_risksets, .direction = "down") %>% 
  ungroup() %>% 
  droplevels()

# 4. Define factor variables --------------------------------------------------

st_data$female     <- factor(st_data$female,
                             labels = c("male", "female"))
st_data$case       <- as.factor(st_data$case)

# 5. Create interaction term for female * case --------------------------------

st_data$female_1_case_1 <- st_data$female_1 * st_data$case_1

# 6. Export data --------------------------------------------------------------

saveRDS(st_data, "./data/analysis_data/analysis_data.RData")

#'/////////////////////////////////////////////////////////////////////////////
#' END OF R-FILE