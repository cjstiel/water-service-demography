/*******************************************************************************
		  Article: Cost and Productivity Effects of Demographic Changes on 
					Local Water Service. 
							
published in: Utilities Policy, 2022, 79(101435).
authors: Astrid Cullmann, Caroline Stiel	
affiliations: Technische Universitaet Berlin, DIW Berlin
				
********************************************************************************
													                 
																	
							FIGURES 1 to 3 (maps)
							
		Maps of growing and shrinking municipalities in Germany (2003-2014)
		-------------------------------------------------------------------
		
		OUTLINE: PART 1: Prepare the shapefiles
						1.1 VZ250_GEM (Ebene Gemeinden)
						1.2 VG250_STA (Ebene STAAT)
						1.3 VG250_LAN (Ebene Bundesland)
						1.4 VG250_PK (Ebene Staedte)
				 PART 2: Prepare population data on growing/shrinking regions
						2.1 Change in population age structure
						2.2 Data cleaning
						2.3 Define variable for growing and shrinking regions
				 PART 3: Merge population data to shapefiles
				 PART 4: Draw maps
						4.1 Total population growth
						4.2 Total growth in the share of the old population
						4.3 Total growth in the share of the young population
		
		
							
--------------------------------------------------------------------------------
code author: Caroline Stiel (DIW Berlin)
version: 20-Aug-2020 (v02)
--------------------------------------------------------------------------------					 

Source shapefiles
-----------------
Verwaltungszuordnungen 1: 250 000 (VZ250) Stand 31.12.2018
© GeoBasis-DE / BKG 2018 (DIW Berlin)

Help
----
https://www.stata.com/support/faqs/graphics/spmap-and-maps/

*******************************************************************************/


*===============================================================================
* 1) Prepare the shapefiles	
*===============================================================================

*===============================================================================
* 1.1 VZ250_GEM (Ebene Gemeinden)
*===============================================================================

* Import the shapefiles into STATA
* -----------------------------------
shp2dta using "$DATADIR\vz250_12-31.gk3.shape\vz250\VZ250_GEM.shp", database(vz250) coordinates(vz250coord) genid(id) replace

/* Notes:
We only need the .dfb and .shp files.
- 'database' determines the name of the .dta file
- 'coordinates' determines the names of the file with the coordinates
- 'genid' determines the name of the id (fortlaufende Nummer)

The command 'shp2dta' creates a vz250.dta file which contains the infos on the 
municipalities such as names etc., and another vz250coord.dta-file which 
contains the coordinates for each municipality. The coordinates are matched to
the municipalities based on the _ID variable.
*/

* Work with the new dataset that STATA just created
* ------------------------------------------------
use "$DATADIR\vz250.dta", clear
display _N

* Are there duplicated entries?
* -----------------------------
duplicates report AGS_G
*duplicates list AGS_G, nolabel sepby(AGS_G)

/*
The duplicates belong to regions at the coast who own some territory in the seas
(North Sea/ Baltic Sea / Lake Constance). They have two entries, one for 
their land area with GF=4 and one for the water territoy with GF = 2.
Keep only the land area (the coastal form is preserved).
Source: BKG (2018): Documentation for VZ250.
*/

drop if GF==2
display _N


* Keep relevant variables at municipality level
* ---------------------------------------------
keep AGS_G GEN_G BEZ_G id

label variable AGS_G "Allgemeiner Gemeindeschlüssel"
label variable GEN_G "Name der Gemeinde"
label variable BEZ_G "Art der Verwaltungseinheit"
label variable id "ID"

* Analyse coding of AGS_G
* -----------------------
gen HELP = length(AGS_G)
tab HELP, mi
drop HELP

duplicates report AGS_G

save "$ARBEITSDIR\TEMP1.dta", replace



*===============================================================================
* 1.2 VG250_STA (Ebene STAAT)
*===============================================================================

* Import the shapefiles into STATA (Staat)
* ------------------------------------------
shp2dta using "$DATADIR\vg250_12-31.gk3.shape.ebenen\vg250_ebenen_3112\VG250_STA.shp", database(vg250STAAT) coordinates(vg250STAATcoord) genid(id) replace

* Work with the new dataset that STATA just created
* ------------------------------------------------
use "$DATADIR\vg250STAAT.dta", clear
display _N

* Polygon-IDs defining coastal line (Seegrenzen)
* ------------------------------------------
tab id if GF!=4
* All _IDs >1 define coastal lines



*===============================================================================
* 1.3 VG250_LAN (Ebene BUNDESLAND)
*===============================================================================


* Import the shapefiles into STATA (Staat)
* ------------------------------------------
shp2dta using "$DATADIR\vg250_12-31.gk3.shape.ebenen\vg250_ebenen_3112\VG250_LAN.shp", database(vg250BL) coordinates(vg250BLcoord) genid(id) replace

* Work with the new dataset that STATA just created
* ------------------------------------------------
use "$DATADIR\vg250BL.dta", clear
display _N

* Polygon-IDs defining the coastal line (Seegrenzen)
* ------------------------------------------------
tab id if GF!=4
* All _IDs >=17 define coastal lines.



*===============================================================================
* 1.4 VG250_PK (Ebene Punkte - Gemeindenamen/Staedtenamen)
*===============================================================================

* Import the shapefiles into STATA (Staat)
* ------------------------------------------
shp2dta using "$DATADIR\vg250_12-31.gk3.shape.ebenen\vg250_ebenen_3112\VG250_PK.shp", database(vg250PK) coordinates(vg250PKcoord) genid(id) replace

* Work with the new dataset that STATA just created
* ------------------------------------------------
use "$DATADIR\vg250PK.dta", clear
display _N

* Select Berlin
* -------------
tab id if GEN=="Berlin"
* id=8593

keep if GEN=="Berlin"
rename id _ID

* retrieve coordinates
* ---------------------
merge 1:1 _ID using "$DATADIR\vg250PKcoord.dta"
keep if _merge==3

save "$DATADIR\vg250_Berlincoord.dta", replace



*===============================================================================
* 2) Prepare population data on growing/shrinking regions
*===============================================================================

use "$DATADIR\Originaldaten\data_gemeinden_all_v01_cs.dta", clear

* Select relevant variables
* -------------------------
keep AGS_n2 pd_kum_l pd_kum_l_corr Basisjahr Austrittsjahr_ew Jahr ///
s_young s_old

* generate federal state variable
* -------------------------------
gen BL = substr(AGS_n2,1,2)
label variable BL "Bundesland"


*===============================================================================
* 2.1 Change in population age structure
*===============================================================================

* Note territorial reform in Saxony 2008, Saxony-Anhalt 2007 and MeckPomm 2011.

* Share children under 18 years
* ------------------------------
gen young_start = s_young if Jahr==Basisjahr
gen young_2008 = s_young if Jahr==2008
gen young_2009 = s_young if Jahr==2009
gen young_2010 = s_young if Jahr==2010
gen young_2011 = s_young if Jahr==2011
gen young_end = s_young if Jahr==Austrittsjahr_ew

bysort AGS_n2: egen young_first = mean(young_start)
bysort AGS_n2: egen young2008 = mean(young_2008)
bysort AGS_n2: egen young2009 = mean(young_2009)
bysort AGS_n2: egen young2010 = mean(young_2010)
bysort AGS_n2: egen young2011 = mean(young_2011)
bysort AGS_n2: egen young_last = mean(young_end)

* changes in young people's share
gen g0314young = young_last-young_first
gen g0814young = young_last-young2008
gen g0914young = young_last-young2009
gen g1014young = young_last-young2010
gen g1114young = young_last-young2011

* Correct for MeckPomm territorial reform 2011
replace g0814young = g1114young if BL=="13"

* Correct for Saxony-Anhalt territorial reform 2008 (missing data)
replace g0814young = g0914young if BL=="15" & g0814young ==.
replace g0814young = g1014young if BL=="15" & g0814young ==.


* Share elderly above 60 years
* -----------------------------
gen old_start = s_old if Jahr==Basisjahr
gen old_2008 = s_old if Jahr==2008
gen old_2009 = s_old if Jahr==2009
gen old_2010 = s_old if Jahr==2010
gen old_2011 = s_old if Jahr==2011
gen old_end = s_old if Jahr==Austrittsjahr_ew

bysort AGS_n2: egen old_first = mean(old_start)
bysort AGS_n2: egen old2008 = mean(old_2008)
bysort AGS_n2: egen old2009 = mean(old_2009)
bysort AGS_n2: egen old2010 = mean(old_2010)
bysort AGS_n2: egen old2011 = mean(old_2011)
bysort AGS_n2: egen old_last = mean(old_end)

* changes in old people's share
* -----------------------------
gen g0314old = old_last-old_first
gen g0814old = old_last-old2008
gen g0914old = old_last-old2009
gen g1014old = old_last-old2010
gen g1114old = old_last-old2011

* Correct for MeckPomm territorial reform 2011
replace g0814old = g1114old if BL=="13"

* Correct for Saxony-Anhalt territorial reform 2008 (missing data)
replace g0814old = g0914old if BL=="15" & g0814old ==.
replace g0814old = g1014old if BL=="15" & g0814old ==.

* Clean
* -----
drop young_* old_* s_young s_old Jahr



*===============================================================================
* 2.2 Data cleaning
*===============================================================================

* Drop duplicates
* ---------------
* Total population growth between 2003 and 2014 is identical across years, 
* therefore, keep only one observations per municipality (only matters if age
* structure is not considered)
duplicates drop

* For how many municipalities/areas is data available?
* ----------------------------------------------------
tabstat pd_kum_l_corr, stat(min max n)

* How many municipalities do we observe exactly between 2003 and 2014?
* ----------------------------------------------------------------------
gen HELP = 0
replace HELP = 1 if Basisjahr!=2003 & Austrittsjahr_ew!=2014
tab HELP
tab BL if HELP ==1
tab BL if pd_kum_l_corr ==.
tab BL if pd_kum_l ==.

* Drop municipalities with other time spans
* ------------------------------------------
* drop if  HELP == 1

* Set pd_kum_l_corr = pd_kum_l where pd_kum_l_corr is not available
* -----------------------------------------------------------------
/* Background: The 2011 Census led to a break in the population figures, which 
is why the population figures prior to 2011 are recursively corrected in the 
original code. However, the recursion does not work for municipalities that are
not observed at all after 2010. These have pd_kum_l_corr =. although pd_kum_l
exists. Furthermore, all municipalities have pd_kum_l_corr=. if one annual
observation is missing, as recursive replacement is not possible in this case.*/
replace pd_kum_l_corr = pd_kum_l if pd_kum_l_corr == .
tab BL if pd_kum_l_corr ==.


*===============================================================================
* 2.3 Define variable for growing and shrinking regions
*===============================================================================

* dummy growing/shrinking regions
* -------------------------------
gen wa = .
replace wa = 1 if pd_kum_l_corr >= 0
replace wa = 0 if pd_kum_l_corr < 0
tab wa

save "$ARBEITSDIR\TEMP2.dta", replace



*===============================================================================
* 3) Merge population data to shapefiles
*===============================================================================

* load population data
* --------------------
use "$ARBEITSDIR\TEMP2.dta", clear

rename AGS_n2 AGS_G

* merge to shapefiles via AGS
* ---------------------------
merge 1:1 AGS_G using "$ARBEITSDIR\TEMP1.dta"

* Delete municipalities in population data without match in shapefiles
* ---------------------------------------------------------------------
drop if _merge==1

* List areas in shapefile with missing population data
* ----------------------------------------------------
tab BEZ_G if _merge==2, mi
/* In 75% of the cases, the areas are "gemeindefreie Gebiete", i.e., they do not
belong to a municipality. Only for 62 municipalities data is missing.*/

* List municipalities with missing population data
* -------------------------------------------------
list GEN_G if BEZ_G!="Gemeindefreies Gebiet" & _merge==2

* How many regions are shrinking regions and how many are growing regions?
* ------------------------------------------------------------------------
tab wa if _merge==3

* save data
* ---------
save "$ARBEITSDIR\TEMP3.dta", replace



*===============================================================================
* 4) Draw maps
*===============================================================================

use "$ARBEITSDIR\TEMP3.dta", clear

*===============================================================================
* 4.1) Total population growth
*===============================================================================

* Summary statistics to determine cutoff points
* ---------------------------------------------
replace pd_kum_l_corr = pd_kum_l_corr * 100
tabstat pd_kum_l_corr, stat(min p1 p5 p10 p25 p50 p75 p90 p95 p99 max n)


* Draw a map of all municipalities (red/green)
* --------------------------------------------
* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap pd_kum_l_corr using vz250coord, id(id) fcolor(RdYlGn) ocolor(none ..) ///
clmethod(custom) clbreaks (-90 -6 -3 0 3 6 170) ///
title("Population density growth 2003-2014") ///
note(`"Population data: Destatis 2003-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
che ich ecih
graph export "pdkum_l_map_v1c.png", as(png) width(1200) replace


* Draw a map of all municipalities (Blues)
* ----------------------------------------

* ------------------------------------------------------------------------------
* Figure 1: Population density growth in German municipalities between 2003 and 2014
* ------------------------------------------------------------------------------

* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap pd_kum_l_corr using vz250coord, id(id) fcolor(Blues) ocolor(none ..) ///
clmethod(custom) clbreaks (-90 -6 -3 0 3 6 170) ndfcolor(gs9) ///
title("Population density growth 2003-2014") ///
note(`"Population data: Destatis 2003-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
 
graph export "pdkum_l_map_v2c.png", as(png) width(1200) replace


*===============================================================================
* 4.2 Total growth in the share of the old population
*===============================================================================

* Summary statistics to determine cutoff points 2003-2014
* --------------------------------------------------------
replace g0314old = g0314old * 100
tabstat g0314old, stat(min p1 p5 p10 p25 p50 p75 p90 p95 p99 max n)

* Draw a map of all municipalities: 2003-2014
* --------------------------------------------
* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap g0314old using vz250coord, id(id) fcolor(Greens) ocolor(none ..) ///
clmethod(custom) clbreaks (-23 0 2 4 6 8 42) ndfcolor(gs9) ///
title("Total growth in the share of the population" "abover 60 years between 2003 and 2014") ///
note(`"Population data: Destatis 2003-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
 
graph export "g0314old_map_v1.png", as(png) width(1200) replace


* Summary statistics to determine cutoff points for 2008-2014
* ------------------------------------------------------------
replace g0814old = g0814old * 100
tabstat g0814old, stat(min p1 p5 p10 p25 p50 p75 p90 p95 p99 max n)

* Draw a map of all municipalities: 2008-2014
* --------------------------------------------

* ------------------------------------------------------------------------------
* Figure 2: Changes in the share of the population older than 60 years in German municipalities between 2008 and 2014.
* ------------------------------------------------------------------------------

* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap g0814old using vz250coord, id(id) fcolor(Greens) ocolor(none ..) ///
clmethod(custom) clbreaks (-23 0 1.5 3 4.5 6 30) ndfcolor(gs9) ///
title("Total growth in the share of the population" "abover 60 years between 2008 and 2014") ///
note(`"Population data: Destatis 2008-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
 
graph export "g0814old_map_v1.png", as(png) width(1200) replace


*===============================================================================
* 4.3 Total growth in the share of the young population
*===============================================================================

* Summary statistics to determine cutoff points 2003-2014
* -------------------------------------------------------
replace g0314young = g0314young * 100
tabstat g0314young, stat(min p1 p5 p10 p25 p50 p75 p90 p95 p99 max n)

* Draw a map of all municipalities for the years 2003-2014
* --------------------------------------------------------
* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap g0314young using vz250coord, id(id) fcolor(Reds) ocolor(none ..) ///
clmethod(custom) clbreaks (-32 -6 -3 -1 0 1 15) ndfcolor(gs9) ///
title("Total growth in the share of the population" "under 18 years between 2003 and 2014") ///
note(`"Population data: Destatis 2003-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
 
graph export "g0314young_map_v1.png", as(png) width(1200) replace


* Summary statistics to determine cutoff points for 2008-2014
* ------------------------------------------------------------
replace g0814young = g0814young * 100
tabstat g0814young, stat(min p1 p5 p10 p25 p50 p75 p90 p95 p99 max n)

* Draw a map of all municipalities for the years 2008-2014
* --------------------------------------------------------

* ------------------------------------------------------------------------------
* Figure 3: Changes in the share of the population younger than 18 years in German municipalities between 2008 and 2014
* ------------------------------------------------------------------------------

* Drop coastal lines when drawing federal states and national states (_ID>17)
spmap g0814young using vz250coord, id(id) fcolor(Reds) ocolor(none ..) ///
clmethod(custom) clbreaks (-32 -6 -3 -1 0 1 15) ndfcolor(gs9) ///
title("Total growth in the share of the population" "under 18 years between 2008 and 2014") ///
note(`"Population data: Destatis 2008-2014 (own calculations)"' `"Geographical data: © GeoBasis-DE / BKG 2018"') ///
legtitle("in %") ///
legstyle(2) legjunction(" to ") ///
polygon(data("$DATADIR\vg250BLcoord.dta") select(drop if _ID >=17) ocolor(black)) ///
point(data("$DATADIR\vg250PKcoord.dta") xcoord(_X) ycoord(_Y) size(tiny) ///
select(keep if _ID==8593) ocolor(black)) ///
label(data("$DATADIR\vg250_Berlincoord.dta") xcoord(_X) ycoord(_Y) label(GEN) ///
position(2) size(small) gap(0.25cm))
 
graph export "g0814young_map_v1.png", as(png) width(1200) replace



*==============================================================================*
* Clean
*==============================================================================*

forvalues p = 1/3 {
	erase "$ARBEITSDIR\TEMP`p'.dta"
}


*==============================================================================*
* 						End of file
*==============================================================================*