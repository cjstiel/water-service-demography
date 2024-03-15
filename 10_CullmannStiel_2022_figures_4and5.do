/*******************************************************************************
		  Article: Cost and Productivity Effects of Demographic Changes on 
					Local Water Service. 
							
published in: Utilities Policy, 2022, 79(101435).
authors: Astrid Cullmann, Caroline Stiel	
affiliations: Technische Universitaet Berlin, DIW Berlin	
				
********************************************************************************
													                 
																
						FIGURES 4 and 5
		
		
--------------------------------------------------------------------------------
code author: Caroline Stiel (DIW Berlin)
version: 18-Aug-2020 (v03)
--------------------------------------------------------------------------------					 

input: water_results.xlsx based on <08_CullmannStiel_2022_costs_analysis.do>

*******************************************************************************/

 set more off

********************************************************************************
* Import results
********************************************************************************

* import results from excel dataset (mc-pdkum)
* --------------------------------------------
* MC distribution by quantiles of population growth
import excel "$ARBEITSDIR\water_results.xlsx", sheet("mc_pdkum") firstrow clear
destring _all, replace

save "$ARBEITSDIR\mc_pdkum_boxplots.dta", replace


* import results from excel dataset (mc levels)
* ---------------------------------------------
* MC levels and growth 2004 to 2014
import excel "$ARBEITSDIR\water_results.xlsx", sheet("mc_levels") firstrow clear
destring _all, replace

save "$ARBEITSDIR\mc_levels.dta", replace


* import results from excel dataset (fcq-pdkum)
* ---------------------------------------------
* FC per m3 distribution by quantiles of population growth
import excel "$ARBEITSDIR\water_results.xlsx", sheet("fcm3_pdkum") firstrow clear
destring _all, replace

save "$ARBEITSDIR\fcm3_pdkum_boxplots.dta", replace


* import results from excel dataset (fcq levels)
* ----------------------------------------------
* FC per m3 levels and growth 2004 to 2014
import excel "$ARBEITSDIR\water_results.xlsx", sheet("fcm3_levels") firstrow clear
destring _all, replace

save "$ARBEITSDIR\fcm3_levels.dta", replace


*==============================================================================*
* FIGURE 4: Evolution of costs between 2004 and 2014
*==============================================================================*

* Figure 4a: Marginal costs of production
* ---------------------------------------
use "$ARBEITSDIR\mc_levels.dta", clear

replace mean_growth = mean_growth*100
replace median_growth = median_growth*100

sort year
twoway (line median_growth year if region=="schrumpfung", lwidth(thick) color("black")) ///
		(line median_growth year if region=="wachstum", lwidth(thick) color("black") lpattern(dash)), ///
		legend(order(1 "shrinking regions" 2 "growing regions") cols(2)) ///
		xtitle(" ", size(vlarge) margin(0 0 0 2)) ///
		ytitle("median increase" "in %", size(vlarge) margin(0 5 0 0)) graphregion(fcolor(white)) ///
		title("marginal costs of production", size(huge) margin(0 0 10 0) ) ///
		xlabel(2004(2)2015,labsize(vlarge)) ylabel(0(10)20,labsize(vlarge) format(%9.0f)) ///

graph export "$ARBEITSDIR\Figures\MC_levelssince2004.pdf", replace



* Figure 4b: Fixed costs per m3
* -----------------------------
use "$ARBEITSDIR\fcm3_levels.dta", clear

replace mean_growth = mean_growth*100
replace median_growth = median_growth*100

sort year
twoway (line median_growth year if region=="schrumpfung", lwidth(thick) color("black")) ///
		(line median_growth year if region=="wachstum", lwidth(thick) color("black") lpattern(dash)), ///
		legend(order(1 "shrinking regions" 2 "growing regions") cols(2)) ///
		xtitle(" ", size(vlarge) margin(0 0 0 2)) ///
		ytitle("median increase" "in %", size(vlarge) margin(0 5 0 0)) graphregion(fcolor(white)) ///
		title("fixed costs per m3", size(huge) margin(0 0 10 0) ) ///
		xlabel(2004(2)2015,labsize(vlarge)) ylabel(0(10)20,labsize(vlarge) format(%9.0f)) ///

graph export "$ARBEITSDIR\Figures\FCm3_levelssince2004.pdf", replace



*==============================================================================
* FIGURE 5: Costs levels and total population density growth 
*==============================================================================


* Figure 5a: Marginal costs of production
* ---------------------------------------
use "$ARBEITSDIR\mc_pdkum_boxplots.dta", clear

sort pdkum_quantiles
twoway 	 (line p50 pdkum_quantiles, lwidth(thick) color("black")), ///
		xline(4.5) ///
		text(1.35 2.5 "shrinking regions", size(vlarge)) ///
		text(1.55 6.5 "growing regions", size(vlarge)) ///
		xtitle("total population density growth (2003-2014)", size(vlarge) margin(0 0 0 5)) ///
		ytitle("EUR/m3", size(vlarge) margin(0 5 0 0)) graphregion(fcolor(white)) ///
		title("(median) marginal costs of production", size(huge)) ///
		xsize(5) ysize(3.3) ///
		xlabel(1.5 "-0.07" 2.5 "-0.04" 3.5  "-0.02" 4.5 "0" 5.5 "0.02" ///
		6.5 "0.04" 7.5 "0.07" ,labsize(large)) ylabel(1.3(0.1)1.7,labsize(large))
		
graph export "$ARBEITSDIR\Figures\MC_pdkum_all_lines.pdf", replace



* Figure 5b: Fixed costs per m3
* -----------------------------
use "$ARBEITSDIR\fcm3_pdkum_boxplots.dta", clear

sort pdkum_quantiles
twoway 	 (line p50 pdkum_quantiles, lwidth(thick) color("black")), ///
		xline(4.5) ///
		text(1.7 3.15 "shrinking regions", size(vlarge)) ///
		text(1.7 5.8 "growing regions", size(vlarge)) ///
		xtitle("total population density growth (2003-2014)", size(vlarge) margin(0 0 0 5)) ///
		ytitle("EUR/m3", size(vlarge) margin(0 5 0 0)) graphregion(fcolor(white)) ///
		title("(median) fixed costs per m3", size(huge)) ///
		xsize(5) ysize(3.3) ///
		xlabel(1.5 "-0.07" 2.5 "-0.04" 3.5  "-0.02" 4.5 "0" 5.5 "0.02" ///
		6.5 "0.04" 7.5 "0.07",labsize(large)) ylabel(1(0.2)2,labsize(large))
		
graph export "$ARBEITSDIR\Figures\FCm3_pdkum_all_lines.pdf", replace



*==============================================================================*
* Clean
*==============================================================================*

erase "$ARBEITSDIR\mc_pdkum_boxplots.dta"
erase "$ARBEITSDIR\mc_levels.dta"
erase "$ARBEITSDIR\fcm3_pdkum_boxplots.dta"
erase "$ARBEITSDIR\fcm3_levels.dta"

*==============================================================================*
* 						End of file
*==============================================================================*
