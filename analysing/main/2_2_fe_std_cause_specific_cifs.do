/*#############################################################################
# Project: Fertility in BEACOPP trt. HL survivros
# File: Estimate standardised cumulative incidence functions for the total
#       effect of chemotherapy treatment on childbearing
#
#############################################################################*/

* PREFIX ----------------------------------------------------------------------
cd "PATH"

* Load data
use "./data/analysis_data/fe_st_data.dta", clear

* Drop HL patients treated with 2-4 BEACOPP
drop if ctreg_2_4_beacopp == 1

* 1. Data preparation ---------------------------------------------------------

* Generate dummies for categorical variables
tab age_dx_c3, gen(age_cat)
tab stage_c3, gen(stage_cat)
tab year_dx_c5, gen(year_cat)
tab perfwho_c3, gen(perfwho_cat)

* Remove labels
label values ctreg_2_4_beacopp ctreg_6_8_beacopp female .

* Recode female variable
recode female ///
	(1 = 0)   ///
	(2 = 1)
	
* Generate interaction for female and trt.
gen ctreg_6_8_beacopp_fem = ctreg_6_8_beacopp * female

* 2. Fit FPMs -----------------------------------------------------------------

** 2.1 Model for childbirth ===================================================

* Generate indicator for competing events
stset st_yrs, failure(exit_event_cb == 1)

stpm2 ctreg_6_8_beacopp     ///
	  ctreg_6_8_beacopp_fem ///
	  female           ///
      age_cat2         ///
      stage_cat2       ///
      year_cat2        ///
	  year_cat3        ///
	  year_cat4        ///
	  year_cat5        ///
      perfwho_cat2     ///
	  country_DK       ///
	  country_NOR      ///
	  nulliparous_yes, ///
	  scale(hazard)    ///
	  df(3)            ///
	  tvc(age_cat2 stage_cat2 nulliparous_yes) ///
	  dftvc(2)
	  
estimates store cb

** 2.2 Model for competing events =============================================

gen compet_events = inlist(exit_event, "SCT", "death", "relapse")

stset st_yrs, failure(compet_events)

stpm2 ctreg_6_8_beacopp     ///
	  ctreg_6_8_beacopp_fem ///
	  female           ///
      age_cat2         ///
      stage_cat2       ///
      year_dx          ///
      perfwho_cat2     ///
	  country_DK       ///
	  country_NOR,     ///
	  scale(hazard)    ///
	  df(3)            
	  
estimates store compet

* 3. Estimate CIFs ------------------------------------------------------------

range timevar 0.75 10 298

standsurv, ///
	crmodels(cb compet) ///
	cif at1(ctreg_6_8_beacopp 0 ctreg_6_8_beacopp_fem 0 female 1) ///
	    at2(ctreg_6_8_beacopp 1 ctreg_6_8_beacopp_fem 1 female 1) ///
		at3(ctreg_6_8_beacopp 0 ctreg_6_8_beacopp_fem 0 female 0) ///
	    at4(ctreg_6_8_beacopp 1 ctreg_6_8_beacopp_fem 0 female 0) ///
	timevar(timevar) ///
	ci
	
* 4. Export CIFs with CIs -----------------------------------------------------

* Keep only standsurv variables
keep timevar _at*
drop *compet*
drop if _at1_cb == .

* Save dataset
save "./data/analysis_data/fe_std_cause_specific_hazard_cis_stata.dta", replace

///////////////////////////////////////////////////////////////////////////////
// END OF STATA FILE