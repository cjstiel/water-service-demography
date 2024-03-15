/*******************************************************************************
		  Article: Cost and Productivity Effects of Demographic Changes on 
					Local Water Service. 
							
published in: Utilities Policy, 2022, 79(101435).
authors: Astrid Cullmann, Caroline Stiel	
affiliations: Technische Universitaet Berlin, DIW Berlin	
				
********************************************************************************
													                 
																	
						COSTS ANALYSIS
				
		CONTENT: Analyzes the water utilities' cost structure	
		
		OUTLINE:	PART 1: Generate variables
							1.1: Marginal costs of production
							1.2: Fixed costs
							1.3: Age structure
							1.4: East Germany
							
					PART 2: Sample definition
							
					PART 3: Marginal costs of production
							3.1: Marginal costs distribution over the years
							3.2: Marginal costs distribution by population structure
							3.3: Changes in marginal costs and demographic changes
							
					PART 4: Fixed costs
							3.1: Fixed costs distribution over the years
							3.2: Fixed costs distribution by population structure
							

							
					
--------------------------------------------------------------------------------
code author: Caroline Stiel (DIW Berlin)
version: 19-Aug-2020 (v137)
--------------------------------------------------------------------------------					 

input: data_wa_acf_v17_cs.dta

*******************************************************************************/
set more off


* define dates and time etc.
* --------------------------
local date=ltrim("$S_DATE")
local date=subinstr("`date'"," ","_",2)

* start logfile
* -------------
cap log close
log using "$LOGDIR\Log_results_prices_costs_v137_cs_`date'.log", replace


* load data set
* -------------
use "$ARBEITSDIR\data_wa_acf_v17_cs.dta", clear


********************************************************************************
* 1) Generate variables
********************************************************************************

*===============================================================================
* 1.1 Marginal costs of production								
*===============================================================================

* Define mc_it = (1/theta_m)*(M_it*exp_u_it/Q_it) [EUR/m3]
* --------------------------------------------------------
gen mc = (intermediates*exp_u_it)/(elasticity_vorl*y)
label variable mc "marginal costs of production [EUR/m3]"

* summary statistics
fdztabstat mc, stat(p1 p5 p25 p50 mean p75 p95 p99 sd N) format(%6.3g)

* Set negative values to missing
* ------------------------------
replace mc =. if mc<0

* summary statistics
fdztabstat mc, stat(p1 p5 p25 p50 mean p75 p95 p99 sd N) format(%6.3g)

* in logs
gen ln_mc = log(mc)
label variable ln_mc "marginal costs of production (logs) [EUR/m3]"

* scale in "Euro cent" to avoid mean around 1 leading to negative logs
gen ln_mc_cent = log(mc*100)
label variable ln_mc_cent "marginal costs of production (logs) [ct/m3]"


*===============================================================================
* 1.2 Fixed costs								
*===============================================================================

* total fixed costs (deflated)
* ----------------------------
gen fc = UK_Code6901/defl_wa


* fixed costs per m3 sold
* -----------------------
gen fc_q = (fc - steuern)/y
label variable fc_q "fixed costs per m3 sold in EUR"

gen ln_fcq = log(fc_q)
label variable fc_q "fixed costs per m3 sold (logs) in EUR"



*===============================================================================
* 1.3 Age structure								
*===============================================================================

* rescale age structure
* ---------------------
replace g_young = g_young*100
replace s_young = s_young*100
replace g_old = g_old*100
replace s_old = s_old*100


*===============================================================================
* 1.4 East Germany			
*===============================================================================

* east dummy
* ----------
gen east = 0
replace east = 1 if bl>11




********************************************************************************
* 2) Sample definition
********************************************************************************

* number of observations per year
* -------------------------------
tab Jahr, mi

* drop observations without TFP estimate
* --------------------------------------
drop if lag_Phi == .

* number of obs
* -------------
tab Jahr, mi


* drop obs with missings in costs or population data
* --------------------------------------------------
drop if mc==. | pd_kum_l_corr==.
tab Jahr, mi


* drop outliers in marginal costs (p99)
* -------------------------------------
_pctile mc, p(99)
gen mc99 = r(r1)
keep if mc<=mc99
tab Jahr, mi


* final data set (full)
* ---------------------
save "$ARBEITSDIR\TEMP_FULL.dta", replace


* subset growing regions
* ----------------------
use "$ARBEITSDIR\TEMP_FULL.dta", clear

keep if pd_kum_l_corr>0
tab Jahr, mi

save "$ARBEITSDIR\TEMP_Wachstum.dta", replace


* subset shrinking regions
* ------------------------
use "$ARBEITSDIR\TEMP_FULL.dta", clear

keep if pd_kum_l_corr<=0
tab Jahr, mi

save "$ARBEITSDIR\TEMP_Schrumpfung.dta", replace



********************************************************************************
* 3) Marginal costs of production
********************************************************************************

*===============================================================================
* 3.1 Marginal costs distribution [EUR/m3] over the years
*===============================================================================

* --------------------------------------------------
* Figure 4: Evolution of costs between 2004 and 2014
* --------------------------------------------------

* full sample
* -----------
use "$ARBEITSDIR\TEMP_FULL.dta", clear
fdztabstat mc, by(Jahr) stat(N p50 mean sd) format(%6.3g)


* growing regions
* ------------------
use "$ARBEITSDIR\TEMP_Wachstum.dta", clear
fdztabstat mc, by(Jahr) stat(N p50 mean sd) format(%6.3g)


* shrinking regions
* ---------------------
use "$ARBEITSDIR\TEMP_Schrumpfung.dta", clear
fdztabstat mc, by(Jahr) stat(N p50 mean sd) format(%6.3g)


*===============================================================================
* 3.2 Marginal costs distribution [EUR/m3] by population structure
*===============================================================================

* ----------------------------------------------------------
* Figure 5: Costs levels and total population density growth
* ----------------------------------------------------------

* loop through subsets
* --------------------
local SETNAME "Wachstum Schrumpfung"

foreach d of local SETNAME{

	use "$ARBEITSDIR\TEMP_`d'.dta", clear

    display " ---------------------------"
	display " data set: `d'"
	display "----------------------------"
	display ""

	* distribution of aggregates population growth (pd_kum_l)
	* ------------------------------------------------------------
	fdztabstat pd_kum_l_corr, stat(N p1 p5 p25 p50 mean p75 p95 p99 sd) format(%6.3g)


	* MC distribution by quantiles of population growth
	* -------------------------------------------------
	_pctile pd_kum_l_corr, p(25,50,75)
	gen pd25 = r(r1)
	gen pd50 = r(r2)
	gen pd75 = r(r3)

	fdztabstat mc if pd_kum_l_corr<=pd25, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat mc if pd_kum_l_corr>pd25 & pd_kum_l_corr<=pd50, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat mc if pd_kum_l_corr>pd50 & pd_kum_l_corr<=pd75, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat mc if pd_kum_l_corr>pd75, stat(N p25 p50 mean p75 sd) format(%6.3g)

	save "$ARBEITSDIR\TEMP_`d'.dta", replace
}


*===============================================================================
* 3.3 Changes in marginal costs and demographic changes
*===============================================================================

	* ---------------------------------------------------------
	* Table 2: Characteristics of growing and shrinking regions
	* ---------------------------------------------------------

* loop through subsets
* --------------------
local SETNAME "FULL Wachstum Schrumpfung"

foreach d of local SETNAME{

	use "$ARBEITSDIR\TEMP_`d'.dta", clear

    display " ---------------------------"
	display " data set: `d'"
	display " distributions "
	display "----------------------------"
	display ""
	
	
	* distribution annual population growth
	* -------------------------------------
	fdztabstat pd_t1_corr, stat(N p1 p25 p50 mean p75 p99 sd) format(%6.3g)

	* distribution annual growth proportion of young persons
	* -------------------------------------------------------
	fdztabstat g_young, stat(N p1 p25 p50 mean p75 p99 sd) format(%6.3g)

	* distribution annual growth proportion of old persons
	* -------------------------------------------------------
	fdztabstat g_old, stat(N p1 p25 p50 mean p75 p99 sd) format(%6.3g)


	* distribution settlement structure
	* ---------------------------------
	tab Siedlung,mi

	* share East
	* ----------
	tab east, mi


	*===========================================================================
	* 3.2.1 Mean regression
	*===========================================================================

	* -----------------------------------------------
	* Table 9: Marginal costs and demographic changes
	* -----------------------------------------------

	display " ---------------------------"
	display " data set: `d'"
	display " mean regression "
	display "----------------------------"
	display ""
	 
	* regress mc [logs(cent/m3)] on population structure (FE)
	* ---------------------------------------------------------
	* without time effects
	xtreg ln_mc_cent pd_t1_corr g_young g_old, fe vce(cluster id)
	* with time effects
	xtreg ln_mc_cent pd_t1_corr g_young g_old i.Jahr, fe vce(cluster id)


	*===========================================================================
	* 3.2.2 Quantile regression
	*===========================================================================

	* ------------------------------------------------------------------------------
	* Table 10: Regression for different quantiles of the marginal-cost distribution
	* Table 13: Detailed regression for different quantiles of marginal costs
	* ------------------------------------------------------------------------------

	display " ---------------------------"
	display " data set: `d'"
	display " quantile regression "
	display "----------------------------"
	display ""
	

	* quantile regression of MC distribution with FE / bootstrapped SE
	* ----------------------------------------------------------------
	foreach j of numlist 0.1 0.25 0.3 0.5 0.7 0.75 0.9 {
		cap noi bootstrap, cluster(id) idcluster(newid) group(id) reps(2000): xtqreg ln_mc_cent pd_t1_corr g_young g_old i.Jahr, i(id) quantile(`j')
	}
}

********************************************************************************
* 4) Fixed costs per m3
********************************************************************************

*===============================================================================
* 4.1 Fixed costs distribution [EUR/m3] over the years
*===============================================================================

* --------------------------------------------------
* Figure 4: Evolution of costs between 2004 and 2014
* --------------------------------------------------

* full sample
* -----------
use "$ARBEITSDIR\TEMP_FULL.dta", clear
fdztabstat fc_q, by(Jahr) stat(N p50 mean sd) format(%6.3g)


* growing regions
* ---------------
use "$ARBEITSDIR\TEMP_Wachstum.dta", clear
fdztabstat fc_q, by(Jahr) stat(N p50 mean sd) format(%6.3g)


* shrinking regions
* -----------------
use "$ARBEITSDIR\TEMP_Schrumpfung.dta", clear
fdztabstat fc_q, by(Jahr) stat(N p50 mean sd) format(%6.3g)


*===============================================================================
* 4.2 Fixed costs distribution [EUR/m3] by population structure
*===============================================================================

* ----------------------------------------------------------
* Figure 5: Costs levels and total population density growth
* ----------------------------------------------------------

* loop through subsets
* --------------------
local SETNAME "Wachstum Schrumpfung"

foreach d of local SETNAME{

	use "$ARBEITSDIR\TEMP_`d'.dta", clear

    display " ---------------------------"
	display " data set: `d'"
	display "----------------------------"
	display ""
	
	* FC distribution by quantiles of population growth
	* -------------------------------------------------
	fdztabstat fc_q if pd_kum_l_corr<=pd25, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat fc_q if pd_kum_l_corr>pd25 & pd_kum_l_corr<=pd50, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat fc_q if pd_kum_l_corr>pd50 & pd_kum_l_corr<=pd75, stat(N p25 p50 mean p75 sd) format(%6.3g)
	fdztabstat fc_q if pd_kum_l_corr>pd75, stat(N p25 p50 mean p75 sd) format(%6.3g)
}



********************************************************************************
* Clean
********************************************************************************

cap noi erase "$ARBEITSDIR\TEMP_FULL.dta"
cap noi erase "$ARBEITSDIR\TEMP_Wachstum.dta"
cap noi erase "$ARBEITSDIR\TEMP_Schrumpfung.dta"

********************************************************************************
cap log close 
* END OF FILE
********************************************************************************

