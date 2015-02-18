* PROGRAM: afla_genvar_v4.do
* PROGRAMMER: Yue Wang
* Edited by Ellen Degnan
* Double Check Comment added by Yue Wang
* DATE CREATED: 6/27/2013
* DATE MODIFIED: 2/18/2014
* PURPOSE: create analysis variables 
* USES DATA: ..afla\dtafiles\1_merged_student
* CREATES DATA: ..afla\dtafiles\2_analysis_data.dta
/* -------------------------------------------------------------------------- */


clear
set mem 500m
set more off


use ..\dtafiles\1_merged_student.dta

/* -------------------------------------------------------------------------- */

* Treatment variables 
* samp is non-missing for all observations

la def sample_assignment 0 "Control" 1 "Aflatoun" 2 "SFE"
la val samp sample_assignment

gen treatment 	= samp!=0
gen aflatoun	= samp==1   
gen sfe			= samp==2
gen control		= samp==0

gen noattrit= 1 - attrit
la var noattrit "Follow-up survey completed"

/* -------------------------------------------------------------------------- */

* Region variables

*Fix missing values in baseline/endline region variable
* 1. Replace endline with baseline if missing in endline
* 2. Replace baseline with endline for missing in baseline
	*For the 4 contradicting values that remain, replace baseline value with endline value 
	*I checked the raw baseline data, and the mistakes are certainly in baseline, not endline


replace end_region=region if mi(end_region) 
replace region=end_region if end_region!=region 
replace end_region_q2=end_region 

*5 remaining missing values in region and end_region: 1 school in GA, and 4 schools in Nkwanta
replace region=2 if schid==216 & mi(region)
replace region=3 if inlist(schid,314,320,333,335)
replace end_region=2 if schid==216 & mi(end_region) 
replace end_region=3 if inlist(schid,314,320,333,335) & mi(end_region)

/*
Request for Loic on 7/30/14: urban/rural breakdown

egen schidtag=tag(schid)
merge m:1 schid using ..\dtafiles\bl_school_deidentified1.dta, keepusing(schid urban)
drop _m
preserve
use ..\dtafiles\el_school_deidentified1.dta, clear
tempfile end_school
rename urban urban_end
save `end_school'
restore
merge m:1 schid using `end_school', keepusing(schid urban_end) 

replace urban=urban_end if urban==-777
replace urban_end=urban if urban_end==-777
*/

/* -------------------------------------------------------------------------- */
* Impute baseline class based on endline, where baseline is missing
ta class, mi
replace class=end_class if mi(class) & !mi(end_class)
replace class=end_class if class==8 & !mi(end_class)
replace end_class=class if mi(end_class) & !mi(class) 

la val class class_othr

/* -------------------------------------------------------------------------- */

*Change Likert variables so that these a higher number = agree more

	foreach var of varlist end_regret_* end_prfsav* end_prfhappy - end_prfdiscrgcsh {
		recode `var' (3=0) (2=1) (1=2) (0=3) (4=.) (5=.)
	}	
	foreach var of varlist  prfspkopsch - prfsavprntonly {
		recode `var' (4=0) (3=1) (2=2) (1=3)	
	}	
				
	label def pref 3 "Strongly Agree" 2 "Agree" 1 "Disagree" 0 "Strongly Disagree", replace
	local prefvar "end_regret_* end_prfsav* end_prfhappy - end_prfdiscrgcsh prfspkopsch - prfsavprntonly"
	foreach var of varlist `prefvar' {
		label values `var' pref
	}
/* -------------------------------------------------------------------------- */
				/* Aptitude test vars */
				
foreach var in ptest jhstest {
	g `var'dif = (end_`var' - `var')
	g `var'dif_pct = (end_`var' - `var')/`var'
	egen `var'_std = std(`var')
	egen end_`var'_std = std(end_`var')
}

egen test_score = rowtotal(ptest_std jhstest_std)
egen end_test_score = rowtotal(end_ptest_std end_jhstest_std)

/* -------------------------------------------------------------------------- */
				/*  Spending activity variables 		*/ 
				
forv i=1/7 {
	order end_spndactamt`i', a(end_spndact`i')
	}


*Recode amounts when easy to infer a data entry error occurred
egen totalspend=rowtotal(spndactamt*)
egen end_totalspend=rowtotal(end_spndactamt1 end_spndactamt2 end_spndactamt3 end_spndactamt4 end_spndactamt5 end_spndactamt6 end_spndactamt7)

*e.g., one observation allocates 30 cedis to good 1 and 2 cedis to good 2; 	30 was probably meant to be 3
recode end_spndactamt1 (30=3) (20=2) 
recode end_spndactamt2 13.5=1.5 if unique=="22322"
recode end_spndactamt2 20=2 if unique=="33331"
recode end_spndactamt2 10=1 if unique=="2114"
recode end_spndactamt3 5=.5 if unique=="3073"
recode spndactamt1 35 = 3.5 if unique=="14710"
recode spndactamt1 5=.5 if unique=="15125"
drop totalspend end_totalspend*

egen totalspend=rowtotal(spndactamt*), mi
egen end_totalspend=rowtotal(end_spndactamt1 end_spndactamt2 end_spndactamt3 end_spndactamt4 end_spndactamt5 end_spndactamt6 end_spndactamt7), mi

local num 0 
foreach suf in edu nec health fun gift church shortsave emersave longsave {
	local ++num
	gen spend_`suf'=0
	gen end_spend_`suf'=0
	
	forv i=1/5 {
		replace spend_`suf' = spndactamt`i' if spndact`i'==`num'
		}
	forv i=1/7 {
		replace end_spend_`suf' = end_spndactamt`i' if end_spndact`i'==`num'
		}
}
*code "other" responses that correspond to saving; no recodes appear necessary in endline vars but create var for sake of symmetry
gen end_spend_othrsave = .
gen spend_othrsave=spndactamt1 if spndact1==10 & inlist(spndact1_sp, ///
	"GIVE TO MY MOTHER TO KEEP FOR ME", ///
	"GIVE TO MY FRIEND TO SAVE FOR ME", ///
	"WILL NOT USE IT ON ANYTHING", ///
	"SAVE SOME IN MY MONEY BOX")

replace spend_othrsave=spndactamt2 if spndact2==10 & inlist(spndact2_sp, ///
	"SAVE THE REST", ///
	"GIVE TO MOTHER TO KEEP FOR ME", ///
	"GIVE THE REST TO MY MOTHER TO SAVE FOR ME", ///
	"SAVE AS SUSU", ///
	"GIVE TO MY MOTHER TO KEEP FOR ME", ///
	"GIVE TO MOTHER TO SAVE") 


* Recode spending activity and spending vars to 0 if respondent allocated some money to at least 1 good
foreach var in spend_edu spend_nec spend_health spend_fun spend_gift spend_church spend_shortsave spend_emersave spend_longsave spend_othrsave{
	recode `var' .=0 if !mi(totalspend)
	recode end_`var' .=0 if !mi(end_totalspend)
	recode `var' 0=. if mi(totalspend)
	recode end_`var' 0=. if mi(end_totalspend)
	}

egen spend_save = rowtotal(spend_shortsave spend_emersave spend_longsave spend_othrsave)
egen end_spend_save = rowtotal(end_spend_shortsave end_spend_emersave end_spend_longsave end_spend_othrsave)
recode spend_save .=0 if !mi(totalspend)
recode end_spend_save .=0 if !mi(end_totalspend)
recode spend_save 0=. if mi(totalspend)
recode end_spend_save 0=. if mi(end_totalspend)

gen pr_spend_save = spend_save/totalspend
gen end_pr_spend_save = end_spend_save/end_totalspend


foreach var of varlist saveloc1st saveloc2nd saveloc3rd saveloc4th saveloc5th {
	replace end_`var'=. if end_`var'==0
}
	
*Ranking safety of different saving locations 
*1st is safest
	
local i 0
local num 0
foreach var in wfrnds susu bank mnybox wfam {
	local ++i
	gen saveloc`var'=.
	gen end_saveloc`var'=.
	foreach pre in saveloc end_saveloc {
		foreach rank in 1st 2nd 3rd 4th 5th {
			loc ++num
			replace `pre'`var'	=`num' if `pre'`rank'==`i'
		}
		loc num 0
	}
}
*Recode so safer is higher

foreach var in wfrnds susu bank mnybox wfam {
	foreach pre in saveloc end_saveloc {
		recode `pre'`var' (5=0) (4=1) (3=2) (2=3) (1=4)
		}
	}

		

/* -------------------------------------------------------------------------- */
			/* Create variables for indices		*/
/*
index 
end_saving_behavior 
end_saving_attitude 
end_saving_risk_perc 
end_saving_env 
end_work 
end_riskpref 
end_timepref 
end_fin_lit 
end_tempt 
end_expend 
end_confidence
*/

/* -------------------------------------------------------------------------- */
			/* 1. Saving behavior index 	*/ 

*Amount saved vars
#d ;
loc amtvars "
save
savelastwk
end_save
end_savemnybox_end
end_saveschl 
end_savefam
end_savehome 
end_savesusu
end_savewfrnd 
end_savewfrnds
end_saveother" ; 
#d cr

*Assume missing value means 0 savings
foreach var of local amtvars   {
	replace `var'amt=0 if (`var'==0 | `var'==.) 
	ta `var'amt, mi
}

*Weekly savings vars 
replace savelastwkamt=0 	if savewkly==0 | savewkly==.
replace end_savelastwkamt=0 if end_savewkly==0 | end_savewkly==. 

*Total savings and amount saved last week	
foreach var in saveamt savelastwkamt {	
	replace `var'=0 	if `var'==.
	replace end_`var'=0 if end_`var'==. 
}

*Saving outside school
	*saving in moneybox is coded as outside of school
	*missing if all values are missing
	
gen end_saveoutside=1 if inlist(1,end_savemnybox_end,end_savewfrnd,end_savefam,end_savehome,end_savesusu,end_savewfrnds)
replace end_saveoutside=0 if end_saveoutside==. & ///
	(end_savemnybox_end!=.|end_savewfrnd!=.|end_savefam!=.|end_savehome!=.|end_savesusu!=.| end_savewfrnds!=.|end_saveother!=.)
replace end_saveoutside=1 if end_saveother==1 & ///
	end_saveother_sp!="Aflatoun" & end_saveother_sp!="Aflatoun box" & end_saveother_sp!="Aflatoun- bank" & end_saveother_sp!="School"

egen end_saveoutsideamt = rowtotal(end_savemnybox_endamt end_savewfrndamt end_savefamamt ///
	end_savehomeamt end_savesusuamt end_savewfrndsamt end_saveotheramt), missing
replace end_saveoutsideamt=end_saveoutsideamt-end_saveotheramt if ///
	end_saveother_sp=="Aflatoun" | end_saveother_sp=="Aflatoun box" | end_saveother_sp=="Aflatoun- bank" | end_saveother_sp=="School"

replace end_saveschl=1 if end_saveother==1 & ///
	(end_saveother_sp=="Aflatoun" | end_saveother_sp=="Aflatoun box" | end_saveother_sp=="Aflatoun- bank" | end_saveother_sp=="School")
replace end_saveschlamt=end_saveotheramt if end_saveother==1 & end_saveschlamt==0 & ///
	(end_saveother_sp=="Aflatoun" | end_saveother_sp=="Aflatoun box" | end_saveother_sp=="Aflatoun- bank" | end_saveother_sp=="School")

* Create alternate, inferred version of aggregate savings vars: sum of component savings vars 
	*(as opposed to the raw variable taken straight from the survey instrument)
	*Baseline does not have corresponding savings location component questions, so re-use existing var (saveamt)
egen end_saveamt_v2 = rowtotal(end_savemnybox_endamt end_savewfrndamt end_savefamamt ///
	end_savehomeamt end_savesusuamt end_savewfrndsamt end_saveotheramt)
gen saveamt_v2 = saveamt


*For index, use savings dummy that takes 1 if a) savings reported overall or b) savings reported in specific locations
replace end_save=1 if end_saveamt_v2>0 & !mi(end_saveamt_v2)


#delimit ;
	global end_saving_behavior "
	end_save
	end_saveamt
	end_saveschl
	end_saveschlamt
	end_saveoutside
	end_saveoutsideamt
	end_savewkly
	end_savelastwkamt" ;
	
	global saving_behavior "
	save
	saveamt
	savewkly
	savelastwkamt" ;
		
#delimit cr


/* -------------------------------------------------------------------------- */
				/* 2. Saving Attitude Index 	*/

#delimit ;

global saving_attitude "
neg_prfsavprntonly 
pr_spend_save" ;

global end_saving_attitude "
end_prfsavgood 
end_prfsavhappy 
neg_end_prfsavspndbetter 
end_prfsavevrytime 
neg_end_prfsavadultonly 
neg_end_prfsavprntonly 
neg_end_prfsavprntbuy 
neg_end_prfsavlvhm 
end_pr_spend_save" ;

#delimit cr

/* -------------------------------------------------------------------------- */
				/* 3. Saving Environment Index 		*/  

global saving_env "neg_savehhangry savelocwfam nhhbank"
global end_saving_env "end_savemnytalk neg_end_savehhangry end_prfsavprntprd end_savelocwfam end_nhhbank"

/* -------------------------------------------------------------------------- */
				/* 4. Work Index 		*/  
	
loc base 
loc end end_
foreach round in base end {
	*Some apparent mistakes in rspwork var; missing even though work detail vars answers. Rule: if >2 work detail vars exist, assume respondent worked 
	egen ``round''worknonmiss = rownonmiss(``round''rspwork1 ``round''rspwork1wage30dys ``round''rspwork1loc ``round''rspwork1amt)
	recode ``round''rspwork .=1 if ``round''worknonmiss>2 & ``round''worknonmiss!=. 
				
*Total wages in the last 30 days (across all activities): UNCONDITIONAL on working (i.e. no missing)
*Coded as missing if respondent worked but didn't answer any wage question
	egen ``round''rspworkwage30dys 	=	rowtotal(``round''rspwork1wage30dys ``round''rspwork2wage30dys ``round''rspwork3wage30dys)
	replace ``round''rspworkwage30dys = . if ``round''rspwork==1 & mi(``round''rspwork1wage30dys) & mi(``round''rspwork2wage30dys) & mi(``round''rspwork3wage30dys)
	
	*Winsorize wage amount at 95%
	winsor ``round''rspworkwage30dys, 		high p(.05) g(``round''rspworkwage30dys_w)

*Total days worked in the last 30 days (summing days worked across all activities): UNCONDITIONAL on working  (i.e. no missing)
*Coded as missing if respondent worked but didn't answer any #days worked question	
	egen ``round''rspworkdys =			rowtotal(``round''rspwork1dys ``round''rspwork2dys ``round''rspwork3dys)
	replace ``round''rspworkdys = . 	if ``round''rspwork==1 & mi(``round''rspwork1dys) & mi(``round''rspwork2dys) & mi(``round''rspwork3dys) 

*Worked inside the home (coded as 1 if any activity has any work inside the home)
*Coded as missing if respondent worked but didn't answer any location question
	gen 	``round''rspwork_home =		(``round''rspwork1loc==1|``round''rspwork1loc==3) | (``round''rspwork2loc==1|``round''rspwork2loc==3) |(``round''rspwork3loc==1|``round''rspwork3loc==3)
	replace ``round''rspwork_home = . 	if ``round''rspwork==1 & mi(``round''rspwork1loc) & mi(``round''rspwork2loc) & mi(``round''rspwork3loc) 

*Worked outside the home (coded as 1 if any activity has any work outside the home)
*Coded as missing if respondent worked but didn't answer any location question
	gen 	``round''rspwork_out =		(``round''rspwork1loc==2|``round''rspwork1loc==3)| (``round''rspwork2loc==2|``round''rspwork2loc==3) | (``round''rspwork3loc==2|``round''rspwork3loc==3)
	replace ``round''rspwork_out = . 	if ``round''rspwork==1  & mi(``round''rspwork1loc) & mi(``round''rspwork2loc) & mi(``round''rspwork3loc) 
	
*Worked on an activity a lot during the school term (coded as 1 if did any activity a lot)
*Coded as missing if the respondent worked but didn't answer any of the amount questions
	
	gen 	``round''rspworkamt_alot = 	``round''rspwork1amt==1|``round''rspwork2amt==1|``round''rspwork3amt==1
	replace ``round''rspworkamt_alot = . if ``round''rspwork==1 & mi(``round''rspwork1amt) & mi(``round''rspwork2amt) & mi(``round''rspwork3amt)
}

*Worked on an activity in a given month
*Coded as missing if the respondent worked but didn't answer the month question for any activity

	foreach month in feb mar apr may {
		gen 	end_rspwork`month'= end_rspwork1`month'==1 | end_rspwork2`month'==1 | end_rspwork3`month'==1
		replace end_rspwork`month'= . if end_rspwork==1 & mi(end_rspwork1`month') & mi(end_rspwork2`month') & mi(end_rspwork3`month') 
	}

#d ;
global work "
rspwork 
rspworkdys 
rspworkwage30dys 
rspworkwage30dys_w 
rspwork_home 
rspwork_out 
rspworkamt_alot" ;

global end_work "
end_rspwork 
end_rspworkdys 
end_rspworkwage30dys 
end_rspworkwage30dys_w 
end_rspworkfeb 
end_rspworkmar 
end_rspworkapr 
end_rspworkmay 
end_rspwork_home 
end_rspwork_out 
end_rspworkamt_alot" ;

#d cr

/* -------------------------------------------------------------------------- */
				/* 5. Risk Index 		*/


replace end_risk_averse=. if end_risk_averse>11
*Transform the existing 1-11 scale to 0 (low risk) to 10 (high risk), then 0 (low) to 1 (high)
replace end_risk_averse=end_risk_averse-1
g end_risk_averse_dum = end_risk_averse/10

*recode 0 for low risk less profit, 1 for high risk more profit, .for no preference and don't know
recode gabiz end_gabiz (1=1) (2=0) (3=.)(4=.)
recode end_gatime36 end_gatime26 end_gatime16  (1=0) (2=1) (3=.)(4=.)

*Note to Yue from Ellen: I didn't write the following note about gabiz (maybe you or Carl did) 
	*but for some reason I didn't delete it, either, so might be good to double check
	*the recoding here.
	
*Note to Ellen from Yue: I changed Carl's recode here last time. My note is to explain why for gabiz and end_gabiz it's (1=1) (2=0), 
*while for end_gatime36 end_gatime26 end_gatime16 it's  (1=0)(2=1):
    //In baseline survey Q86 and endline survey Q79, 1=option A (high risk), 2=option B (low risk).
    //In endline survey Q81, end_gatime36 end_gatime26 end_gatime16 1=GameA (low risk),2=GameB(high risk).
*I have double checked this with Jim and we think the current code is fine.
global riskpref "gabiz"

#d ;
global end_riskpref "
	end_gatime36
	end_gatime26
	end_gatime16
	end_risk_averse_dum
	end_gabiz" ;

#d cr

/* -------------------------------------------------------------------------- */
				/* 6. Time Index 		*/ 
				
*Recode gasick end_gasick 
	*0 for medicine today to feel somewhat better, 
	*1 for wait longer but feel entirely better, 
	*. for no preference
	
	recode gasick end_gasick (2=1) (1=0) (3=.)
	*Redefine label
	label def gasick 0 "Meds Now, Small Sick" 1 "Sick One Week, Then Cure", replace 
		label values gasick gasick
		label values end_gasick gasick

*Recode prfprocras end_prfprocras 1 for same day, 0 for tomorrow
	recode prfprocras end_prfprocras (2=0) (4=.)
	*Redefine label
	label def prfprocras 1 "Same Day" 0 "Tomorrow", replace
		label values prfprocras prfprocras
		label values end_prfprocras prfprocras
	
*Recode end_gatime69onewk 
	*0 for 6 GHC today, 
	*1 for 9 tomorrow; 
*Recode end_gatime69fowk 
	*0 for 6 GHC 4 weeks from today, 
	*1 for 9 GHC 5 weeks from today.
	recode end_gatime69onewk end_gatime69fowk (1=0) (2=1)(4=.)

global timepref "gasick"
#d ;
	global end_timepref "
	end_gatime69onewk 
	end_gatime69fowk 
	end_gasick" ;
#d cr


/* -------------------------------------------------------------------------- */

				/* 7. Financial Literacy Index 		*/ 

#d ;
global fin_lit "
	neg_shoplftovr1_final 
	shopcrrct1_final 
	neg_shopsec1 
	neg_shoplftovr2_final 
	shopcrrct2_final 
	neg_shopsec2 
	wklymnyplan" ;

global end_fin_lit "
	neg_end_shoplftovr1_final 
	end_shopcrrct1_final 
	neg_end_shopsec1 
	neg_end_shoplftovr2_final 
	end_shopcrrct2_final 
	neg_end_shopsec2 
	end_wklymnyplan" ;
#d cr

/* -------------------------------------------------------------------------- */
				/* 8. Temptation Goods Index 		*/ 

foreach var of varlist end_spntmny7dys_snck end_spntmny7dys_fun {
	replace `var'=. if end_spntmny7dys==.
}
foreach var of varlist  spend_fun spntmny7dys_snck spntmny7dys_fun {
	replace `var'=0 if `var'==.
	replace end_`var'=0 if end_`var'==.
}
#d ;
global tempt "
	spntmny7dys_snck 
	spntmny7dys_fun 
	spend_fun" ;

global end_tempt "
	end_spntmny7dys_snck 
	end_spntmny7dys_fun 
	end_spend_fun" ;
#d cr

/* -------------------------------------------------------------------------- */
				/* 9. Expenditure Index 		*/  
recode end_expspnd7dys (-77.77=.)
replace end_meat3dys=4 if end_meat3dys==40
replace end_nunif=1 if end_nunif==11
replace end_nunif=2 if end_nunif==22

#d ;
global expend "
	spntmny7dys 
	expspnd7dys" ;

global end_expend "
	end_spntmny7dys 
	end_expspnd7dys" ;
#d cr


/* -------------------------------------------------------------------------- */
				/*  10. Confidence Index 		*/ 

*Recode prluck 
	*1 for own fault, 
	*0 for badluck; 
*Recode prfwork 
	*1 for hardwork, 
	*0 for goodluck; 
	*. for don't know and refuse

foreach var in prfwork prfluck {
	recode `var' (3=.) (2=0)
	recode end_`var' (3=.) (2=0)
}
	*Redefine labels
	label def prfluck 1 "Own Fault" 0 "Bad Luck", replace
		label values end_prfluck prfluck
		label values prfluck prfluck
	label def prfwork 1 "Hard Work" 0 "Good Luck", replace
		label values end_prfwork prfwork
		label values prfwork prfwork
		
*Confidence index excluding neg_end_prfpplbetter, end_prfluck and end_prfwork

#d ;

global end_confidence "
	end_prfconfexmsch 
	neg_end_prflowopn 
	neg_end_prfupstsch 
	neg_end_prftchfeelbad 
	neg_end_prfdiscrgcsh" ;
#d cr

/* -------------------------------------------------------------------------- */
				/*  10. Academic Index 		*/ 
				
recode daysattend 50=5 //data entry: probably meant 5
recode daysattend 9=0  //data entry: probably meant 0

replace end_daysattend = end_daysattend-1 //looks like this var is scaled up 1
recode end_daysattend 6=5 //dataentry: probably meant 5

#d ;
global academic "
	daysattend
	test_score" ;

global end_academic "
	end_daysattend
	end_test_score" ;

#d cr	
/* -------------------------------------------------------------------------- */
				/* XX. Savings Risk Perception Index 	*/ 
	
	
global saving_risk_perc "savelocsusu savelocmnybox savelocwfrnds savelocbank savelocwfam"
global end_saving_risk_perc "end_savelocsusu end_savelocmnybox end_savelocwfrnds end_savelocbank end_savelocwfam" 

/* -------------------------------------------------------------------------- */
				/* Remove endline values created for attriters		*/
	
*Respondents that did NOT take the follow-up survey (noattrit=0) may have values in endline
	*variables do to the blanket nature of var construction in this dofile. Replace all
	*endline vars to 0 for attriters.
	
ds end_*, has(type numeric)
loc nums `r(varlist)'
ds end_*, has(type string)
loc strings `r(varlist)'
foreach var in `nums'{
	replace `var'=. if  noattrit==0
	}
foreach var in `strings'{
	replace `var'="" if noattrit==0
	}


/* -------------------------------------------------------------------------- */
				/* Create Indices: 		*/

			
*Three set of indices created:
	*1) "all" ignores missing, effectively treating them as 0
	*2) "app" creates missing value for index if any component variable is missing
	*3) "fs" represents subsample of 5th and 7th graders

* Create global for each set of indices
foreach suf in all app fs{
	#d ;
	global `suf'_index "end_saving_behavior_`suf'_index end_saving_attitude_`suf'_index 
			end_saving_env_`suf'_index end_work_`suf'_index end_riskpref_`suf'_index end_timepref_`suf'_index end_fin_lit_`suf'_index 
			end_tempt_`suf'_index end_expend_`suf'_index end_confidence_`suf'_index end_academic_`suf'_index" ;
	#de cr	
	*display "${`suf'_index}"
}	

*** Create indices: Standardize vars on control mean/sd

loc all 1
loc fs "(class==3 | class==5)"

*Note that the confidence index exists only for endline
*Note that the local `macro' copies the name of each index global created throughout this dofile,
*which permits us to call on and standardize each component variable in a given index 
foreach macro in saving_behavior end_saving_behavior ///
				saving_attitude end_saving_attitude  ///
				saving_env end_saving_env ///
				work end_work ///
				riskpref end_riskpref /// 
				timepref end_timepref /// 
				fin_lit end_fin_lit ///
				tempt end_tempt ///
				expend end_expend /// 
				end_confidence ///
				academic end_academic {
		
	foreach samp in all fs {	
	* Create empty global index
	global `macro'_`samp'_index=""
 
		* Standardize individual component variables for full sample and subsample of fifth and seventh graders (if ``samp'')
		foreach var in $`macro' {
			*Strip prefix "neg_" from variable name stored in local
			*"neg_" is not actually part of the varibale name; instead, it's tacked on to the variable name in order to identify
			*the var as entering the index negatively
			loc sans = regexr("`var'", "^neg_", "")
			qui sum `sans' if treatment==0 & ``samp''
				global `sans'meanc r(mean)
				global `sans'sdc r(sd)
			capture confirm variable `var'_n_`samp'
			if _rc!=0 {
				* First condition: if variable does not have a "neg" prefix
				qui if "`sans'"=="`var'" {
					g `var'_n_`samp' = (`sans'-$`sans'meanc)/$`sans'sdc if ``samp''
					}
				* Second condition: if variable has a "neg" prefix
				qui else {
					g `var'_n_`samp' = -(`sans'-$`sans'meanc)/$`sans'sdc if ``samp''
					}
				* Iteratively populate global index with standardized variables
				global `macro'_`samp'_index  "${`macro'_`samp'_index} `var'_n_`samp'"
					}
				}
		 }
 		* Find mean of standardized vars for full sample, ignoring missing values in individual vars
		tempvar index_mean_all index_mean_app index_mean_fs index_mi 
		egen `index_mean_all'=rowmean($`macro'_all_index)

		
		* Replace mean to missing if one or more standardized vars missing (main table)
		qui egen `index_mi' = rowmiss($`macro'_all_index)
		gen `index_mean_app' = `index_mean_all'
		replace `index_mean_app'=. if `index_mi'!=0
		}
		 
		* Find mean of standardized vars for 5th and 7th grade sample, replace mean to missing if one or more standardized vars missing (appendix)
		qui egen `index_mean_fs'=rowmean($`macro'_fs_index) if `fs'
		
		* Standardize MAIN INDEX
		qui sum `index_mean_all' if treatment==0	
			loc index_meanc r(mean)
			loc index_sdc r(sd)
		qui gen `macro'_all_index 	= (`index_mean_all'-`index_meanc')/`index_sdc'
		sum `macro'_all_index 	if treatment==0

			* Global for individual index appendix apps
			global `macro'_all	"`macro'_all_index $`macro'"
			display "$`macro'_all"
		
		/*
		* Standardize APPENDIX INDEX 1 (missing if 1 or more individual vars missing))
			qui sum `index_mean_app' if treatment==0 
				loc index_meanc r(mean)
				loc index_sdc r(sd)
			qui gen `macro'_app_index	= (`index_mean_app'-`index_meanc')/`index_sdc'
			sum `macro'_app_index 	if treatment==0
			* Global for individual index appendix tables - not needed for this version of tables
			global `macro'_app 	"`macro'_app_index $`macro'"
			display "$`macro'_app"
		*/
		
		* Standardize APPENDIX INDEX 2 (5th and 7th grade subsample)
		qui sum `index_mean_fs' if treatment==0 & `fs'	
			loc index_meanc r(mean)
			loc index_sdc r(sd)
		qui gen `macro'_fs_index = (`index_mean_fs'-`index_meanc')/`index_sdc'
		sum `macro'_fs_index  	if treatment==0
		
		* Global for individual index appendix apps; would still need to restrict sample to 5th and 7th in regression
		global `macro'_fs	"`macro'_fs_index $`macro'"
		display "$`macro'_fs"

			
		drop __*
	}

	
/* -------------------------------------------------------------------------- */
	
	
save ..\dtafiles\analysis_data_v4.dta, replace

