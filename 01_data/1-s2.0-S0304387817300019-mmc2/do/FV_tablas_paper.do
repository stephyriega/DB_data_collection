/*********************************************************************+*********
***********       			    SERNAM: DE 4 A 7      			     ***********
********************************************************************************

	AUTHORS	     : Marcela Perticara/Claudia Martinez
	PURPOSE		 : All tables in paper
	
	Create a directory that includes the folowing folders and files:
		- /dta (copy in it data_analysis.dta)
		- /do
		- /tab_paper
		
		
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

/*GET DATA FINAL USED IN ANALYSIS*/

use "dta\data_analysis.dta", replace

																	  		  */		
********************************************************************************
******* 	ANALYSIS											  		 *******
********************************************************************************


********************************************************************************
*SOME MACRO DEFINITIONS



*Keep useful observations
global keep "seguimiento==1 & (entrevista_distinta==9 | entrevista_distinta==1)" 
global X "LB_jefa_hog LB_edu_anos LB_educ_i LB_cant_nino LB_edad_post"
global estratoa "treatment"
global estratob1 "trabybebe notrabybebe trabynobebe notrabynobebe"
global estratob2 "trabybebe notrabybebe"
global estrato_c1 "t_bebe treatment"
global test_b11 "trabybebe=notrabybebe=trabynobebe=notrabynobebe"
global test_b22 "trabybebe=notrabybebe"
global test_c "t_bebe==0"
global test b23 "trabybebe=notrabybebe trabynobebe=notrabynobebe"


global cluster "cl(comuna_esc) absorb(est_rbd)"
global outreg  "word excel lab bdec(3)"
global outreg2  "word excel lab bdec(3)"
global outrega  "word excel lab adec(3) bdec(3) keep(treatment) nocon" 
global outrega_exp  "word excel lab adec(2) bdec(2) sdec(2) keep(treatment) nocon" 
global outregb1  "word excel lab adec(3) bdec(3) keep(trabynobebe trabybebe notrabybebe notrabynobebe) nocon" 
global outregb2  "word excel lab adec(3) bdec(3) keep(trabybebe notrabybebe) nocon" 
global outregb1_exp  "word excel lab adec(2) bdec(2) sdec(2) keep(trabynobebe trabybebe notrabybebe notrabynobebe) nocon" 


gen t_bebe=trabybebe+notrabybebe
gen t_nobebe=trabynobebe+notrabynobebe



********************************************************************************
******* 	Table 1.	Compliance Rate					 	     		 *******
********************************************************************************
preserve
table treatment, c(freq count seg sum part_f) row replace

sort treatment


ren table1 baseline
ren table2 followup
ren table3 participation

gen take_up=participation/followup if _n<3

order treatment

export excel using tab_paper\table1.xls, firstrow(variables) replace

restore



********************************************************************************
******* 	Table 2.									 	     		 *******
********************************************************************************

preserve

keep if $keep

*2.2 Define relevant variables

lab var est1 "Works and children $<5$"
lab var est2 "Does not work and children $<5$"
lab var est3 "Works and children $>5$"
lab var est4 "Does not work and children $>5$"


global yvar LB_edad_post LB_ingpercap_post LB_ing_laboral LB_participa 		///
		   	LB_desempleo LB_ocupadas LB_hrs_trabaja LB_mes_trabaja 			///
			LB_contrato LB_edu_anos LB_edu_max1 LB_edu_max2 LB_edu_max3 	///
			LB_edu_max4 LB_asiste_post LB_stress LB_bebe LB_n_kids 			///
			LB_cuida_infantil est1 est2 est3 est4 


sum est*

foreach var of varlist est1 est2 est3 est4 {
	count if `var'==1
	global `var'=r(N)
}
			
			
local nyvar : word count $yvar
token $yvar

forvalues n = 1/`nyvar'{
cap: ren ``n'' yvar`n'
qui: ds yvar* 
global yvar `r(varlist)'
}


forvalues y=1/`nyvar'{
	qui: sum yvar`y'
	
	local mean= r(mean)
	if `mean'>10000{
	global m_`y': di %9.0fc r(mean)
	global s_`y': di %9.0fc r(sd)
	global n_`y': di %9.0fc r(N)
	
	qui: sum yvar`y' if treatment==1
	global t_`y': di %9.0fc r(mean)

	qui: sum yvar`y' if treatment==0
	global c_`y': di %9.0fc r(mean)
	}
	else {
	global m_`y': di %5.3fc r(mean)
	global s_`y': di %5.3fc r(sd)
	global n_`y': di %5.0fc r(N)
	
	qui: sum yvar`y' if treatment==1
	global t_`y': di %5.3fc r(mean)

	qui: sum yvar`y' if treatment==0
	global c_`y': di %5.3fc r(mean)
	}
	
		qui: reg yvar`y' treatment
		mat B_`y'=e(b)
		scalar b_`y' = B_`y'[1,1]
		global b_`y' : di %5.1fc b_`y'
		qui: test treatment=0
	    global p_`y' : di %5.3fc r(p)	 
}


texdoc init tab_paper/table2.tex, replace
tex \begin{tabular}{lccccccc}  \hline \hline
tex 						& \multicolumn{3}{c}{All observations}			&& Control	  	& Treatment		& p-value 		\\ \cline{2-4}
tex 	 					& Mean			& SD 			& Obs 			&& Mean 		& Mean 			& T=C			\\ 
tex 	 					& [1]			& [2]		 	& [3] 			&& [4]			& [5]			& [6]			\\ \hline
tex 																														\\								
	forvalues y=1/`nyvar'{
tex `: var label yvar`y''	& ${m_`y'}		& ${s_`y'}		&${n_`y'}		&&${t_`y'}		&${c_`y'}		&${p_`y'}		\\	
}
tex $est1 \\
tex $est2 \\
tex $est3 \\
tex $est4 \\

tex \hline \hline 
tex \end{tabular} 


restore



********************************************************************************
******* 	Table 3.									 	     		 *******
********************************************************************************

*3.1 Define variables to explain attrition

global attrition LB_ingpercap_post LB_ing_laboral LB_desempleo LB_hrs_trabaja 	///
                  LB_mes_trabaja LB_contrato LB_asiste_post LB_stress LB_n_kids ///
                  est1 est2 est3

label var LB_desempleo "Unemployed"	  


*3.3 Regressions


reg seg_misma treatment if treatment!=. & LB_consentimiento==1 
outreg2 using tab_paper/ltable3, $outreg replace
reg seg_misma treatment $attrition if treatment!=. & LB_consentimiento==1 
outreg2 using tab_paper/ltable3, $outreg


cap: erase tab_paper/ltable3.txt




keep if $keep


/*all the tables with the same structure: panel A, all sample; panel B, by strata*/


********************************************************************************
******* Table 4: CHILDCARE USE. SCHOOL AGE CHILDREN (AGE>5)	& PRE-SCH CHILD ****
********************************************************************************


local cuida_big "part_f cuida_infantil cuida_infantil_f"
local cuida_baby "cuida_bebe_f cuida_bebe_g"

/*PANEL A*/

cap: erase tab_paper/tab4a.xml
cap: erase tab_paper/tab4a.rtf

foreach var of local cuida_big {
global y = "`var'"
areg `var' $estratoa $X 			,  $cluster 
qui sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using tab_paper/tab4a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
}



/*We add toddles columns*/


foreach var of local cuida_baby {

areg `var' $estratoa $X	if est1==1 | est2==1	,  $cluster 
qui sum `var' if treatment==0 & e(sample)
local cons_=r(mean)
local lab : var label `var'
outreg2 using tab_paper/tab4a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
}

cap: erase tab_paper/tab4a.txt


/*PANEL B*/

cap: erase tab_paper/tab4b.xml
cap: erase tab_paper/tab4b.rtf

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
	qui sum `var' if treatment==0 & est`x'==1 & e(sample)
	local cons_est`x'=r(mean)
	}
local lab : var label `var'
outreg2 using tab_paper/tab4b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
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
	qui sum `var' if treatment==0 & est`x'==1 & e(sample)
	local cons_est`x'=r(mean)
	}
local lab : var label `var'
outreg2 using tab_paper/tab4b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2',/*
*/ Test F (baby coeff equal):, `p_F22')/*
*/ ctitle(`lab') $outregb2	
}

cap: erase tab_paper/tab4b.txt



********************************************************************************
******* 	Table 5: LABOR MARKET OUTCOMES				 	     		 *******
********************************************************************************



	// Labor Force Participation y empleo

preserve

drop if meses_10!=10 

cap: erase tab_paper/tab5a.xml
cap: erase tab_paper/tab5a.rtf


foreach var in participa_unmes_mp participasiempre_mp mp_meses_activ trabaja_unmes_mp trabajasiempre_mp mes_trabaja {
	global y = "`var'"
	areg `var' $estratoa $X 			,  $cluster 
	qui sum `var' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `var'
	outreg2 using tab_paper/tab5a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
	}

cap: erase tab_paper/tab5a.txt

cap: erase tab_paper/tab5b.xml
cap: erase tab_paper/tab5b.rtf

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
		qui sum `var' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `var'
	outreg2 using tab_paper/tab5b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
	*/ ctitle(`lab') $outregb1	
}

cap: erase tab_paper/tab5b.txt

	// Worked Hours, income

cap: erase tab_paper/tab6a.xml
cap: erase tab_paper/tab6a.rtf

local out_list "hrs ing_laboral ing_porhora stress"
	
foreach out of local out_list {
	global y = "`out'"
	areg `out' $estratoa $X 			,  $cluster
	local p_F1= r(p)
	qui sum `out' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `out'
	outreg2 using tab_paper/tab6a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega	
	}

cap: erase tab_paper/tab6a.txt

cap: erase tab_paper/tab6b.xml
cap: erase tab_paper/tab6b.rtf

foreach out of local out_list {
	global y = "`var'"
	areg `out' $estrato_c1 $X 			,  $cluster /*only for testing if it is better to group strata in two*/
	test $test_c
	local p_Fc= r(p)
	areg `out' $estratob1 $X 			,  $cluster
	test $test_b11
	local p_F11= r(p)
	test $test_b22
	local p_F22= r(p)	
	forvalues x=1/4{
		qui sum `out' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `out'
	outreg2 using tab_paper/tab6b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
	*/ ctitle(`lab') $outregb1	
}

cap: erase tab_paper/tab6b.txt


restore


local out_list "G2t G3_1 G3_2 G3_3 G3_4 G3_other"

	// Expenses

cap: erase tab_paper/tab7a.xml
cap: erase tab_paper/tab7a.rtf


foreach out of local out_list {
	global y = "`out'"
	areg `out' $estratoa $X 			,  $cluster 
	qui sum `out' if treatment==0 & e(sample)
	local cons_=r(mean)
	local lab : var label `out'
	outreg2 using tab_paper/tab7a, addstat(Control Mean, `cons_') ctitle(`lab') $outrega_exp	
	}

cap: erase tab_paper/tab7a.txt

cap: erase tab_paper/tab7b.xml
cap: erase tab_paper/tab7b.rtf

foreach out of local out_list {
	global y = "`var'"
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
		qui sum `out' if treatment==0 & est`x'==1 & e(sample)
		local cons_est`x'=r(mean)
		}
	local lab : var label `out'
	outreg2 using tab_paper/tab7b, addstat(Works LB and <= 5, `cons_est1', Does not Wo LB and <= 5, `cons_est2', Works LB and > 5, `cons_est3', Does not Wo LB and > 5, `cons_est4', /*
	*/Test F (all equal):, `p_F11', Test F (baby coeff equal):, `p_F22', Test F (H0: strata1_2 different strata 3_4):, `p_Fc') /*
	*/ ctitle(`lab') $outregb1_exp	
}


cap: erase tab_paper/tab7b.txt



***LEE BOUNDS


local ing_var "ing_laboral ing_porhora"

	tab LB_estrato, gen(_covariates)

*PANEL A TABLE

cap: erase tab_paper/tab6a_lee.xml
cap: erase tab_paper/tab6a_lee.rtf

foreach var of local ing_var {
	preserve
	replace `var'=. if `var'==0
	sort est_rbd treatment
	by est_rbd treatment: egen kk=mean(`var')
	gen bad=kk==.
	drop if bad==1
	drop bad kk
	sort est_rbd
	by est_rbd: egen meant=mean(treatment)
	gen bad=treatment==meant
	drop if bad==1
	drop bad meant
	leebounds `var' treatment, tight(_covariates*) cieffect
	local lab : var label `var'
	outreg2 using tab_paper/tab6a_lee, addstat(CI low, e(cilower), CI Up, e(ciupper)) ctitle(`lab') $outreg2
	restore
	}

	cap: erase tab_paper/tab6a_lee.txt
	
	
	*PANEL B TABLE

cap: erase tab_paper/tab6b_lee.xml
cap: erase tab_paper/tab6b_lee.rtf

foreach var of local ing_var {
	foreach num of numlist 1(1)4 {
	preserve
	keep if est`num'==1
	replace `var'=. if `var'==0
	sort LB_rbd treatment
	by LB_rbd treatment: egen kk=mean(`var')
	gen bad=kk==.
	drop if bad==1
	drop bad kk
	sort LB_rbd
	by LB_rbd: egen meant=mean(treatment)
	gen bad=treatment==meant
	drop if bad==1
	drop bad meant
	leebounds `var' treatment, cieffect
	local lab : var label `var'
	outreg2 using tab_paper/tab6b_lee, addstat(CI low, e(cilower), CI Up, e(ciupper)) ctitle(`lab') $outreg2
	restore
	}
	}

	cap: erase tab_paper/tab6b_lee.txt

