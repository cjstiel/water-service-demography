/*******************************************************************************
		  Article: Cost and Productivity Effects of Demographic Changes on 
					Local Water Service. 
							
published in: Utilities Policy, 2022, 79(101435).
authors: Astrid Cullmann, Caroline Stiel	
affiliations: Technische Universitaet Berlin, DIW Berlin
				
********************************************************************************
													                 
																	
							MASTER FILE
									
		
							
--------------------------------------------------------------------------------
code author: Caroline Stiel (DIW Berlin)
--------------------------------------------------------------------------------					 

********************************************************************************
*								START										   *
*******************************************************************************/

* Prepare workspace
* -----------------
clear
set more off
capture log close


* Variable 'RDC' defines the working environment
*-----------------------------------------------
* 0: own computer
* 1: remote access to original data set at research data centre (RDC)

global RDC 0

* Paths
* -----

if $RDC==1{
	global DATADIR 		"" /*where original data is stored*/
	global LOGDIR 		"" /*where output and logfiles are saved*/
	global ARBEITSDIR 	"" /*where working data is saved*/
	global PROGRAMS		$LOGDIR
	adopath				+ 	"" /*ado path at RDC*/
	}


if $RDC==0{
	global DATADIR 		"" /*where original data is stored*/
	global LOGDIR 		"" /*where output and logfiles are saved*/
	global ARBEITSDIR 	"" /*where working data is saved*/
	global PROGRAMS		$LOGDIR
	adopath				+ 	"" /*ado path at local computer*/
	}


********************************************************************************


* run costs analysis
* ------------------
do "$PROGRAMS\08_CullmannStiel_2022_costs_analysis.do"

* figures 1 to 3 (maps)
* ---------------------
do "$PROGRAMS\09_CullmannStiel_2022_figures_1to3_maps.do"

* figures 4 and 5
* ---------------
do "$PROGRAMS\10_CullmannStiel_2022_figures_4and5.do"





********************************************************************************
* 										END									   *
********************************************************************************
