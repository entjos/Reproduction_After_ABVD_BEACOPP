#//////////////////////////////////////////////////////////////////////////////
# Project: Fertility in BEACOPP trt. HL survivros
# File: Create table of standardised cause-specific CIF of childbirth 
#       at 2, 5 and 10 years after HL diagnosis.
#
#//////////////////////////////////////////////////////////////////////////////

# PREFIX ----------------------------------------------------------------------

# clear memory
rm(list = ls())

# Load packages
library(openxlsx)
library(haven)
library(dplyr)

# Import data from Stata
std_cif <- read_dta(paste0("./data/analysis_data/",
                           "fe_std_cause_specific_hazard_cis_stata.dta"))

# 1. Get CID for 2, 5, 10 years -----------------------------------------------

# Define function to get closesed value
closest <- function(x, value){
  
  x[which(abs(x - value) == min(abs(x - value)))]
  
}

# Get estimates
cif_selection <- std_cif %>% 
  filter(timevar %in% c(closest(std_cif$timevar, 2),
                        closest(std_cif$timevar, 5),
                        closest(std_cif$timevar, 10))) %>% 
  mutate(timevar = round(timevar, 1))

# 2. Export estimates to excel workbook ---------------------------------------

wb <- loadWorkbook("./tables/fe_std_cif.xlsx")

writeData(wb,
          "r_output",
          cif_selection)

saveWorkbook(wb, 
             "./tables/fe_std_cif.xlsx",
             overwrite = TRUE)

# /////////////////////////////////////////////////////////////////////////////
# END OF R-SCRIPT