/*********************************************************************+*********
***********       			    SERNAM: DE 4 A 7      			     ***********
********************************************************************************

	AUTHORS	     : Marcela Perticara/Claudia Martinez
	PURPOSE		 : All tables in appendix
	
	Create a directory that includes the folowing folders and files:
		- /dta (copy in it, muestra_validez_ext.dta; data_analysis.dta)
		- /do
		- /appendix
		
		
																	  		  */		
********************************************************************************
******* 	PREAMBLE								 	     			 *******
****************************************************out*************************

clear all
set more off
version 11.2



/*SET WORKING DIRECTORY*/

global dropbox "C:\Users\mperticara\Dropbox\4 a 7 _2011\2011-2012 (orden estilo J-PAL)\16. Paper Mujeres\JDE\R&R2\sent\analisis"

cd "$dropbox"


/*APPENDIX 1: EXTERNAL VALIDITY*/

*****VALIDEZ EXTERNA: ESTADISTICAS DE LAS ESCUELAS
**(BASE MINEDUC)

use "dta\muestra_validez_ext.dta", clear

reg muestra mat_ens_2 n_prio if comparable==1
outreg2 using appendix/table_A1 , replace lab word excel bdec(4) nocon

reg muestra mat_ens_2 n_prio mama_w mama_ft if comparable==1
outreg2 using appendix/table_A1, append lab word excel bdec(4) nocon

reg muestra mat_ens_2 n_prio mama_w mama_ft share_capac if comparable==1
outreg2 using appendix/table_A1, append lab word excel bdec(4) nocon



/*APPENDIX 4: RESULTS WITHOUT CLUSTER*/

/*GET DATA FINAL USED IN ANALYSIS*/

use "dta\data_analysis.dta", replace


gen t_bebe=trabybebe+notrabybebe
gen t_nobebe=trabynobebe+notrabynobebe



********************************************************************************
******* 	ANALYSIS											  		 *******
********************************************************************************

*Keep useful observations
global keep "seguimiento==1 & (entrevista_distinta==9 | entrevista_distinta==1)"
global X "LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post"
global estratoa "treatment"
global estratob1 "trabybebe notrabybebe trabynobebe notrabynobebe"
global estratob2 "trabybebe notrabybebe"

global cluster "vce(robust) absorb(est_rbd)"
global outrega  "word excel lab adec(3) bdec(3) keep(treatment) nocon slow(1000)" 
global outreg  "word excel lab bdec(5) slow(1000)"
global outreg2  "word excel lab bdec(3) slow(1000)"
global outregb1  "word excel lab adec(3) bdec(3) keep(trabynobebe trabybebe notrabybebe notrabynobebe) nocon slow(1000)" 
global outregb2  "word excel lab adec(3) bdec(3) keep(trabybebe notrabybebe) nocon slow(1000)" 






keep if $keep


/*all the tables with the same structure: panel A, all sample; panel B, by strata*/


********************************************************************************
******* CHILDCARE USE. SCHOOL AGE CHILDREN (AGE>5)	& PRE-SCH CHILD ****
********************************************************************************


local cuida_big "part_f cuida_infantil cuida_infantil_f"
local cuida_baby "cuida_bebe_f cuida_bebe_g"

/*PANEL A*/

cap: erase appendix/tabA4_1a.xml
cap: erase appendix/tabA4_1a.rtf

foreach var of local cuida_big {
global y = "`var'"
areg `var' $estratoa $X 			,  $cluster 
sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using appendix/tabA4_1a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
}



/*Agregamos las columnas de toddlers*/


foreach var of local cuida_baby {

areg `var' $estratoa $X	if est1==1 | est2==1	,  $cluster 
sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using appendix/tabA4_1a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
}

cap: erase appendix/tabA4_1a.txt


/*PANEL B*/

cap: erase appendix/tabA4_1b.xml
cap: erase appendix/tabA4_1b.rtf

foreach var of local cuida_big {
global y = "`var'"
areg `var' $estrato_c1 $X 			,  $cluster /*only for testing if it is better to group strata in two*/
test $test_c
local p_Fc= r(p)
areg `var' $estratob1 $X 			,  $cluster
test $test_b11
local p_F11= r(p)
test $test_b22
local p_F22= r(p)
forvalues x=1/4{
	sum `var' if treatment==0 & est`x'==1 & e(sample)
	local cons_est`x'=r(mean)
	}
local lab : var label `var'
outreg2 using appendix/tabA4_1b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
*/ ctitle(`lab') $outregb1	
}


/*Agregamos las columnas de toddlers*/


foreach var of local cuida_baby {
areg `var' $estratob2 $X		if est1==1 | est2==1	,  $cluster
test $test_b2
local p_F2= r(p)
test $test_b22
local p_F22= r(p)
test $test_b21
local p_F21= r(p)
forvalues x=1/2{
	sum `var' if treatment==0 & est`x'==1 & e(sample)
	local cons_est`x'=r(mean)
	}
local lab : var label `var'
outreg2 using appendix/tabA4_1b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2',/*
*/ Test F (baby coeff equal):, `p_F22')/*
*/ctitle(`lab') $outregb2	
}

cap: erase appendix/tabA4_1b.txt



********************************************************************************
******* 	LABOR MARKET OUTCOMES				 	     		 *******
********************************************************************************



	// Labor Force Participation y empleo

preserve

drop if meses_10!=10 

cap: erase appendix/tabA4_2a.xml
cap: erase appendix/tabA4_2a.rtf


foreach var in participa_unmes_mp participasiempre_mp mp_meses_activ trabaja_unmes_mp trabajasiempre_mp mes_trabaja {
	global y = "`var'"
	areg `var' $estratoa $X 			,  $cluster 
	sum `var' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `var'
	outreg2 using appendix/tabA4_2a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
	}

cap: erase appendix/tabA4_2a.txt

cap: erase appendix/tabA4_2b.xml
cap: erase appendix/tabA4_2b.rtf

foreach var in participa_unmes_mp participasiempre_mp mp_meses_activ trabaja_unmes_mp trabajasiempre_mp mes_trabaja {
	global y = "`var'"
	areg `var' $estrato_c1 $X 			,  $cluster /*only for testing if it is better to group strata in two*/
	test $test_c
	local p_Fc= r(p)
	areg `var' $estratob1 $X 			,  $cluster 
	test $test_b11
	local p_F11= r(p)
	test $test_b22
	local p_F22= r(p)
	forvalues x=1/4{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `var'
	outreg2 using appendix/tabA4_2b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4',/*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
	*/ ctitle(`lab') $outregb1	
}

cap: erase appendix/tabA4_2b.txt

	// Worked Hours, income

cap: erase appendix/tabA4_3a.xml
cap: erase appendix/tabA4_3a.rtf

local out_list "hrs ing_laboral ing_porhora stress"
	
foreach out of local out_list {
	global y = "`out'"
	areg `out' $estratoa $X 			,  $cluster
	local p_F1= r(p)
	sum `out' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `out'
	outreg2 using appendix/tabA4_3a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
	}

cap: erase appendix/tabA4_3a.txt

cap: erase appendix/tabA4_3b.xml
cap: erase appendix/tabA4_3b.rtf

foreach out of local out_list {
	global y = "`out'"
	areg `out' $estrato_c1 $X 			,  $cluster /*only for testing if it is better to group strata in two*/
	test $test_c
	local p_Fc= r(p)
	areg `out' $estratob1 $X 			,  $cluster
	test $test_b11
	local p_F11= r(p)
	test $test_b22
	local p_F22= r(p)
	test notrabynobebe=trabybebe
	local p_F_41=r(p)
	test notrabynobebe=notrabybebe
	local p_F_42=r(p)
	test notrabynobebe=trabynobebe
	local p_F_43=r(p)
	forvalues x=1/4{
		sum `out' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `out'
	outreg2 using appendix/tabA4_3b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc',/*
	*/ Strata4vs1:, `p_F_41', Strato4vs2:, `p_F_42', Strato4vs3:, `p_F_43' ) /*	
	*/ ctitle(`lab') $outregb1	
}

cap: erase appendix/tabA4_3b.txt


restore


local out_list "G2t G3_1 G3_2 G3_3 G3_4 G3_other"

	// Expenses

cap: erase appendix/tabA4_4a.xml
cap: erase appendix/tabA4_4a.rtf


foreach out of local out_list {
	global y = "`out'"
	areg `out' $estratoa $X 			,  $cluster 
	sum `out' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `out'
	outreg2 using appendix/tabA4_4a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
	}

cap: erase appendix/tabA4_4a.txt

cap: erase appendix/tabA4_4b.xml
cap: erase appendix/tabA4_4b.rtf

foreach out of local out_list {
	global y = "`out'"
	areg `out' $estrato_c1 $X 			,  $cluster /*only for testing if it is better to group strata in two*/
	test $test_c
	local p_Fc= r(p)
	areg `out' $estratob1 $X 			,  $cluster
	test $test_b11
	local p_F11= r(p)
	test $test_b22
	local p_F22= r(p)	
	test $test_b23
	local p_F23= r(p)
	forvalues x=1/4{
		sum `out' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `out'
	outreg2 using appendix/tabA4_4b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4',/*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
	*/ ctitle(`lab') $outregb1	
}

cap: erase appendix/tabA4_4b.txt


/*APPENDIX 5: IV ESTIMATION*/



use "dta\data_analysis.dta", replace





********************************************************************************
******* 	ANALYSIS											  		 *******
********************************************************************************


*para IV

gen trabybebe_iv    =part_f*est1
gen notrabybebe_iv  =part_f*est2
gen trabynobebe_iv  =part_f*est3
gen notrabynobebe_iv=part_f*est4



********************************************************************************
******* 	ANALYSIS											  		 *******
********************************************************************************

*Keep useful observations
global keep "seguimiento==1 & (entrevista_distinta==9 | entrevista_distinta==1)"
global X "LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post  i.est_rbd"
global estratoa "treatment"
global estratoa_iv "part_f"
global estratob1 "trabybebe notrabybebe trabynobebe notrabynobebe"
global estratob1_iv "trabybebe_iv notrabybebe_iv trabynobebe_iv notrabynobebe_iv"
global estratob2 "trabybebe notrabybebe"
global estratob2_iv "trabybebe_iv notrabybebe_iv"

global cluster "cl(comuna_esc)"
global outrega  "word excel lab adec(3) bdec(3) keep(part_f) nocon slow(1000)" 
global outreg  "word excel lab bdec(5) slow(1000)"
global outreg2  "word excel lab bdec(3) slow(1000)"
global outregb1  "word excel lab adec(3) bdec(3) keep(trabybebe_iv notrabybebe_iv trabynobebe_iv notrabynobebe_iv) nocon slow(1000)" 
global outregb2  "word excel lab adec(3) bdec(3) keep(trabybebe_iv notrabybebe_iv) nocon slow(1000)" 

keep if $keep


/*all the tables with the same structure: panel A, all sample; panel B, by strata*/


********************************************************************************
******* CHILDCARE USE. SCHOOL AGE CHILDREN (AGE>5)	& PRE-SCH CHILD ****
********************************************************************************


local cuida_big "cuida_infantil cuida_infantil_f"
local cuida_baby "cuida_bebe_f cuida_bebe_g"

/*PANEL A*/

cap: erase appendix/tabA5_1a.xml
cap: erase appendix/tabA5_1a.rtf

foreach var of local cuida_big {
global y = "`var'"
ivregress 2sls `var' ($estratoa_iv=$estratoa) $X 			,  $cluster
estat firststage
matrix fstat = r(singleresults)
scalar fstat = fstat[1,4]
local fstat: di %5.2f fstat
sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using appendix/tabA5_1a, addstat(Control Mean, `cons_', First Stage F, `fstat') ctitle(`lab') $outrega	
}



/*Agregamos las columnas de toddlers*/


foreach var of local cuida_baby {
global y = "`var'"
ivregress 2sls `var' ($estratoa_iv=$estratoa) $X if est1==1 | est2==1	,  $cluster
estat firststage
matrix fstat = r(singleresults)
scalar fstat = fstat[1,4]
local fstat: di %5.2f fstat
sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using appendix/tabA5_1a, addstat(Control Mean, `cons_', First Stage F, `fstat') ctitle(`lab') $outrega	
}

cap: erase appendix/tabA5_1a.txt


/*PANEL B*/

cap: erase appendix/tabA5_1b.xml
cap: erase appendix/tabA5_1b.rtf

foreach var of local cuida_big {
	global y = "`var'"
	ivregress 2sls `var' ($estratob1_iv=$estratob1) $X 			,  $cluster
	forvalues x=1/4{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
	}
	local lab : var label `var'
	outreg2 using appendix/tabA5_1b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4')/*
	*/ ctitle(`lab') $outregb1	
	}


/*Agregamos las columnas de toddlers*/


foreach var of local cuida_baby {
	global y = "`var'"
	ivregress 2sls `var' ($estratob2_iv=$estratob2) $X 		if est1==1 | est2==1		,  $cluster
	forvalues x=1/2{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
	}
	local lab : var label `var'
	outreg2 using appendix/tabA5_1b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2')/*
	*/ ctitle(`lab') $outregb2	
}

cap: erase appendix/tabA5_1b.txt



********************************************************************************
******* 	LABOR MARKET OUTCOMES				 	     		 *******
********************************************************************************



	// Labor Force Participation y empleo

preserve

drop if meses_10!=10 

cap: erase appendix/tabA5_2a.xml
cap: erase appendix/tabA5_2a.rtf


foreach var in participa_unmes_mp participasiempre_mp mp_meses_activ trabaja_unmes_mp trabajasiempre_mp mes_trabaja {
	global y = "`var'"
	ivregress 2sls `var' ($estratoa_iv=$estratoa) $X 			,  $cluster
	estat firststage
	matrix fstat = r(singleresults)
	scalar fstat = fstat[1,4]
	local fstat: di %5.2f fstat
	sum `var' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `var'
	outreg2 using appendix/tabA5_2a, addstat(Control Mean, `cons_', First Stage F, `fstat') ctitle(`lab') $outrega	
	}

cap: erase appendix/tabA5_2a.txt

cap: erase appendix/tabA5_2b.xml
cap: erase appendix/tabA5_2b.rtf

foreach var in participa_unmes_mp participasiempre_mp mp_meses_activ trabaja_unmes_mp trabajasiempre_mp mes_trabaja {
	global y = "`var'"
	ivregress 2sls `var' ($estratob1_iv=$estratob1) $X 			,  $cluster
	forvalues x=1/4{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
	}
	local lab : var label `var'
	outreg2 using appendix/tabA5_2b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4'/*
	*/) ctitle(`lab') $outregb1	
	}

cap: erase appendix/tabA5_2b.txt

	// Worked Hours, income

cap: erase appendix/tabA5_3a.xml
cap: erase appendix/tabA5_3a.rtf

local out_list "hrs ing_laboral ing_porhora stress"
	
foreach var in hrs ing_laboral ing_porhora  stress {
	global y = "`var'"
	ivregress 2sls `var' ($estratoa_iv=$estratoa) $X 			,  $cluster
	estat firststage
	matrix fstat = r(singleresults)
	scalar fstat = fstat[1,4]
	local fstat: di %5.2f fstat
	sum `var' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `var'
	outreg2 using appendix/tabA5_3a, addstat(Control Mean, `cons_', First Stage F, `fstat') ctitle(`lab') $outrega		
	}

cap: erase appendix/tabA5_3a.txt

cap: erase appendix/tabA5_3b.xml
cap: erase appendix/tabA5_3b.rtf

foreach var in hrs ing_laboral ing_porhora stress {
	global y = "`var'"
	ivregress 2sls `var' ($estratob1_iv=$estratob1) $X 			,  $cluster
	forvalues x=1/4{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
	}
	local lab : var label `var'
	outreg2 using appendix/tabA5_3b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4'/*
	*/ ) ctitle(`lab') $outregb1		
}

cap: erase appendix/tabA5_3b.txt


restore


local out_list "G2t G3_1 G3_2 G3_3 G3_4 G3_5"

	// Expenses

cap: erase appendix/tabA5_4a.xml
cap: erase appendix/tabA5_4a.rtf


foreach var in G2t G3_1 G3_2 G3_3 G3_4 G3_other {
	global y = "`var'"
	ivregress 2sls `var' ($estratoa_iv=$estratoa) $X 			,  $cluster
	estat firststage
	matrix fstat = r(singleresults)
	scalar fstat = fstat[1,4]
	local fstat: di %5.2f fstat
	sum `var' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `var'
	outreg2 using appendix/tabA5_4a, addstat(Control Mean, `cons_', First Stage F, `fstat') ctitle(`lab') $outrega	
	}

cap: erase appendix/tabA5_4a.txt

cap: erase appendix/tabA5_4b.xml
cap: erase appendix/tabA5_4b.rtf

foreach var in G2t G3_1 G3_2 G3_3 G3_4 G3_other {
	global y = "`var'"
	ivregress 2sls `var' ($estratob1_iv=$estratob1) $X 			,  $cluster
	forvalues x=1/4{
		sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
	}
	local lab : var label `var'
	outreg2 using appendix/tabA5_4b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4'/*
	*/) ctitle(`lab') $outregb1			
}

cap: erase appendix/tabA5_4b.txt



