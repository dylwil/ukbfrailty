
/*
Script accompanying manuscript: A frailty index for UK Biobank participants

Dylan Williams, Karolinska Institutet, February 2018 
dylan.williams@ki.se

Script for deriving the standard Frailty Index from UKB baseline assessment variables (self-reported or from interview)

A 49-item FI is derived, and then the code used to impute missing data and run survival models are listed.

Notes: 
- 	Several of the items are derived from data field 20002, which includes reports of various non-cancer illnesses
	Please refer to the 'Coding 6' file downloaded from UKB data showcase to identify the diseases of relevance from codes in the 20002 data field.

-	All variables are based on instance one measures (data recorded at the cohort's baseline assessment), 
	but some array variables (repeated measures or extra information) are necessary to use.

-	Variables required from UKB data (these represent data field numbers, along with the Stata prefix for each variable 
	('s_' or 'n_' for string or numeric vars, respectively):
n_eid  		-- participant identifier
n_31_0* 	-- Sex
n_34_* 		-- Year of birth
n_52_* 		-- Month of birth
n_21003_0*  -- Age when attended assessment centre
n_21000* 	-- Ethnicity
n_1200_0* 	-- Sleeplessness / insomnia
n_6148_0* 	-- Eye problems / disorders
n_2227_0*  	-- Other eye problems
n_2020_0* 	-- Loneliness / isolation
n_2247_0* 	-- Hearing difficulties
n_2188_0* 	-- Long-standing illness, disability or infirmity
n_2178_0* 	-- Overall self-rated health
n_2296_0* 	-- Falls in last year
n_2316_0* 	-- Chest wheeze or whistling
n_2080_0* 	-- Tiredness / Lethargy
n_6150_0* 	-- Vascular problems	
n_20002_0* 	-- Non-cancer illness codes (all array variables are necessary to keep)
n_6149_0* 	-- Mouth / dental problems
n_2335_0* 	-- Chest pain
n_2443_0* 	-- Diabetes
n_2453_0* 	-- Cancer diagnosis
n_134_0* 	-- Number of self-reported cancers
n_2463_0* 	-- fractured/broken bones
n_6152_0*	-- Multiple medical conditions reported - e.g. DVT, asthma, eczema
n_3786_0*	-- Age of asthma diagnosis
n_2473_0* 	-- Other serious medical conditions reported
n_2050_0*  	-- Depressed moods
n_1970_0* 	-- Nervous feelings
n_1930_0* 	-- Miserableness
n_6159_0* 	-- Pain in last month
n_136_0* 	-- Number of operations
n_6177_0* 	-- Medications used for cholesterol, blood pressure or diabetes (male)
n_6153_0* 	-- Medications used for cholesterol, blood pressure or diabetes (female)
n_137_0* 	-- Number of medications used

n_40007_* 	-- Age at death for deceased participants

*/

clear
set more off, perm

use "UKB data file containing UKB vars outlined above" // Edit to open appropriate dataset

* Adds code and labels of variables together to make variable table displays easier to intrpret:

label dir
numlabel `r(names)', add



* 1 - Glaucoma: array item so need to use multiple vars
* Code any glaucoma mention across array vars as 1:

gen i1_glaucoma = 0 if n_2227==0
replace i1_glaucoma = 0 if n_6148_0_0==-7

	* glaucoma mentioned in two array vars:
replace i1_glaucoma= 1 if n_6148_0_0==2
replace i1_glaucoma = 1 if n_6148_0_1==2 // no more glaucoma cases from 3rd array var onwards

	* step to include others that didn't report any glaucoma, but didn't respond with 'prefer not to answer' / 'do not know':
replace i1_glaucoma = 0 if i1_g!=1 & n_6148_0_0!=-3 & n_2227!=-3

	* coding 'prefer not to answer' / 'do not know' to missing:
replace i1_glaucoma = . if n_6148_0_0==-3 | n_6148_0_0==-1

		* recoding glaucoma reported as '1277' in field 20002
forvalues n =  0(1)28 {
replace i1_glaucoma = 1 if n_20002_0_`n'==1277 
}  

	* recoding 6148 'do not know' respondents to 0 if they were interviewed and still had no record of glaucoma:
replace i1_glaucoma = 0 if i1_!=1 & n_20002_0_0!=. & n_6148_0_0==-1



* 2 - Cataracts: procedure as for glaucoma

gen i2_cataracts = 0 if n_2227==0
replace i2_cataracts = 0 if n_6148_0_0==-7

	* capturing all cataract reportings in array vars (where individuals reported more than one eye problem):
replace i2_cataracts = 1 if n_6148_0_0==4
replace i2_cataracts = 1 if n_6148_0_1==4
replace i2_cataracts = 1 if n_6148_0_2==4
replace i2_cataracts = 1 if n_6148_0_3==4

replace i2_cataracts = 0 if i2_c!=1 & n_6148_0_0!=-3 & n_2227!=-3

	* coding 'prefer not to answer' / 'do not know' to missing:
replace i2_cataracts = . if n_6148_0_0==-3 | n_6148_0_0==-1

	* recoding cataracts reported as '1278' in field 20002

forvalues n =  0(1)28 {
replace i2_cataracts = 1 if n_20002_0_`n'==1278 
}  

		* recoding 6148 'do not know' respondents to 0 if they were interviewed and still had no record of cataracts:

replace i2_cataracts = 0 if i2_c!=1 & n_20002_0_0!=. & n_6148_0_0==-1

	

* 3 - Hearing: dichotomous on any/no hearing problems; deaf also coded to 1

gen i3_hearing = 1 if n_2247==1
replace i3 = 1 if n_2247==99
replace i3 =0 if n_2247==0

	* NB: those answering 'prefer not to answer' or 'do not know' are coded as missing 
	* Same applies to item variables below that are coded from single data fields
	

* 4 - infirmity 

gen i4_infirmity = 1 if n_2188==1
replace i4 = 0 if n_2188==0


* 5 - self-rated health
gen i5_self_health = 0 if n_2178==1
replace i5 = 0.25 if n_2178==2
replace i5 = 0.5 if n_2178==3
replace i5 = 1 if n_2178==4


* 6 - Falls
 
gen i6_falls = 0 if n_2296==1
replace i6_falls = 0.5 if n_2296==2
replace i6_falls = 1 if n_2296==3


* 7 - Breathing/wheeze

gen i7_wheeze = 1 if n_2316==1
replace i7 = 0 if n_2316==0

	
* 8 - Fatigue

gen i8_fatigue = 0 if n_2080==1 
replace i8 = 0.25 if n_2080==2
replace i8 = 0.75 if n_2080==3
replace i8 = 1 if n_2080==4


* 9 - myocardial infarction

gen i9_chd = 0 if  n_6150_0_0==-7
replace i9 = 1 if  n_6150_0_0==1
replace i9 = 0 if n_6150_0_0==2 | n_6150_0_0==3 | n_6150_0_0==4

		*NB: no heart attack values in later array variables for question
		* recoding heart attack reported as '1075' in field 20002
		
forvalues n =  0(1)28 {
replace i9 = 1 if n_20002_0_`n'==1075
}
		* NB: no one coded as 'do not know' from field 6150 to recode here



* 10 - Angina 

gen i10_angina = 0 if n_6150_0_0==-7
replace i10 = 1 if n_6150_0_0==2
replace i10 = 0 if n_6150_0_0==1 | n_6150_0_0==3 | n_6150_0_0==4

	* Angina values in later array variables:
replace i10 = 1 if n_6150_0_1==2
replace i10 = 1 if n_6150_0_2==2
replace i10 = 1 if n_6150_0_3==2

	* recoding angina reported as '1075' in field 20002

forvalues n =  0(1)28 {
replace i10 = 1 if n_20002_0_`n'==1074
}

	* NB: no one coded as 'do not know' from field 6150 to recode here


* 11 - Stroke

gen i11_stroke = 0 if n_6150_0_0==-7
replace i11 = 1 if n_6150_0_0==3
replace i11 = 0 if n_6150_0_0==1 | n_6150_0_0==2 | n_6150_0_0==4

replace i11 = 1 if n_6150_0_1==3

replace i11 = 1 if n_6150_0_2==3

	* recoding stroke reported as '1081' (ambiguous stroke) or '1583' (ischaemic) in field 20002

forvalues n =  0(1)28 {
replace i11 = 1 if n_20002_0_`n'==1081 | n_20002_0_`n'==1583
}

		* NB: no one coded as 'do not know' from field 6150 to recode here


* 12 - High blood pressure

gen i12_hypertension = 0 if n_6150_0_0==-7

replace i12 =1 if n_6150_0_0==4
replace i12 =0 if n_6150_0_0==1 | n_6150_0_0==2 | n_6150_0_0==3

replace i12 = 1 if n_6150_0_1==4
replace i12 = 1 if n_6150_0_2==4
replace i12 = 1 if n_6150_0_3==4

	* recoding hypertension reported as '1065' in field 20002

forvalues n =  0(1)28 {
replace i12 = 1 if n_20002_0_`n'==1065
}

			* NB: no one coded as 'do not know' from field 6150 to recode here


			
			
* 13 - Rheumatoid arthritis - disease code 1464 in field 20002

gen i13_rh_arth = 0
replace i13 =. if n_2473_0_0==-3 

forvalues n =  0(1)28 {
replace i13_rh_arth = 1 if n_20002_0_`n'==1464
}
		
		
* 14 - Osteoarthritis - disease code 1465 in field 20002

gen i14_osteoarth = 0
replace i14 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i14 = 1 if n_20002_0_`n'==1465
}



* 15 - Gout - disease code 1466 in field 20002

gen i15_gout = 0
replace i15 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i15 = 1 if n_20002_0_`n'==1466
}



* 16 - Dental problems

gen i16_dental = 0 if n_6149_0_0==-7
replace i16 = 1 if n_6149_0_0!=-7 & n_6149_0_0!=-3 & n_6149_0_0!=.



* 17 - Chest pain

gen i17_chest_pain = 0 if n_2335==0
replace i17= 1 if  n_2335==1
  

* 18 - Sciatica - disease code 1476 in field 20002 

gen i18_sciatica = 0
replace i18 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i18 = 1 if n_20002_0_`n'==1476
}


* 19 - Diabetes

gen i19_diabetes = 0 if n_2443==0
replace i19 = 1 if n_2443==1
	
	* 20002 codes 1220 and 1223 refer to 'diabetes' and 'type 2 diabetes' respectively 

forvalues n =  0(1)28 {
replace i19 = 1 if n_20002_0_`n'==1220 |  n_20002_0_`n'==1223
}

replace i19 = 0 if i19!=1 & n_20002_0_0!=. & n_2443==-1



* 20 - Cancer
gen i20_cancer = 0 if n_2453==0
replace i20_cancer = 1 if n_2453==1

	* more people replied on the multi cancer Q, so that can be used to fill in missing data on i20

replace i20 = 0 if n_134==0
replace i20 = 1 if n_134>=1 & n_134!=. // this recodes several thousand more as having 1 for cancer deficit that had not reported cancer in the touchscreen questionnaire


* 21 - Multiple cancers

gen i21_multi_cancer = 0 if n_134==0 | n_134==1
replace i21 = 1 if n_134>1 & n_134!=.


* 22 - Fractures

gen i22_fractures = 0 if n_2463==0
replace i22 = 1 if n_2463==1


* 23 - deep vein thrombosis

gen i23_dvt = 0 if n_6152_0_0==-7

replace i23 = 1 if n_6152_0_0==5
replace i23 =0 if n_6152_0_0!=5 & n_6152_0_0!=-3 & n_6152_0_0!=.

replace i23 = 1 if n_6152_0_1==5
replace i23 = 1 if n_6152_0_2==5
replace i23 = 1 if n_6152_0_3==5
replace i23 = 1 if n_6152_0_4==5


forvalues n =  0(1)28 {
replace i23 = 1 if n_20002_0_`n'==1094
}

	*NB: no 'do not know' values in 6152 to recode
	




* 24 - Emphysema/chronic bronchitis

gen i24_emphysema_cbronc = 0 if n_6152_0_0==-7

replace i24 = 1 if n_6152_0_0==6

replace i24 =0 if n_6152_0_0!=6 & n_6152_0_0!=. & n_6152_0_0!=-3

replace i24 = 1 if n_6152_0_1==6
replace i24 = 1 if n_6152_0_2==6

forvalues n =  0(1)28 {
replace i24 = 1 if n_20002_0_`n'==1113
}

	*NB: no 'do not know' values in 6152 to recode



* 25 - Asthma - two vars to use, reporting in n_6152_0_0 and reporting in 20002 (code 1111)


gen i25_asthma = 0 if n_6152_0_0==-7
replace i25 = 1 if n_6152_0_0==8

replace i25 =0 if n_6152_0_0!=8 & n_6152_0_0!=. & n_6152_0_0!=-3

replace i25 = 1 if n_6152_0_1==8
replace i25 = 1 if n_6152_0_2==8
replace i25 = 1 if n_6152_0_3==8

forvalues n =  0(1)28 {
replace i25 = 1 if n_20002_0_`n'==1111
}

	*NB: no 'do not know' values in 6152 to recode



* 26 - Allergies

gen i26_allergies = 0 if n_6152_0_0==-7 

replace i26 = 1 if n_6152_0_0==9 
replace i26 =0 if n_6152_0_0!=9 & n_6152_0_0!=. & n_6152_0_0!=-3

replace i26 = 1 if n_6152_0_1==9
replace i26 = 1 if n_6152_0_2==9
replace i26 = 1 if n_6152_0_3==9
replace i26 = 1 if n_6152_0_4==9

forvalues n =  0(1)28 {
replace i26 = 1 if n_20002_0_`n'==1387 // hayfever/rhinitis code
}

forvalues n =  0(1)28 {
replace i26 = 1 if n_20002_0_`n'==1452 // eczema code
}
	
		*NB: no 'do not know' values in 6152 to recode

		

* 27 - Hypothyroidism - code 1226 in 20002

gen i27_hypothyroidism = 0
replace i27 =. if n_2473_0_0==-3 // this recodes anyone to missing if they did not want to divulge medical information on 'other conditions' in the touchscreen Q AND then did not report the condition in 20002


forvalues n =  0(1)28 {
replace i27 = 1 if n_20002_0_`n'==1226
}


* 28 - Recent depressed mood 

gen i28_depression = 0 if n_2050==1
replace i28 = 0.5 if n_2050==2
replace i28 = 0.75 if n_2050==3
replace i28 = 1 if n_2050==4


* 29 - Anxiousness 

gen i29_anxiousness = 1 if n_1970==1
replace i29_anxiousness = 0 if n_1970==0


* 30 - Severe anxiety 

gen i30_severe_anxiety = 0
replace i30 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i30 = 1 if n_20002_0_`n'==1287
}

* 31 - Mood/misery

gen i31_misery = 1 if n_1930==1
replace i31 = 0 if n_1930==0



* 32 - Loneliness

gen i32_loneliness= 0 if n_2020==0
replace i32 = 1 if n_2020==1



* 33 - Head and/or neck pain 

gen i33_headneck_pain = 0 if n_6159_0_0!=.
replace i33 = 1 if n_6159_0_0== 1 | n_6159_0_0== 3
replace i33 = . if n_6159_0_0==-3

replace i33 = 1 if n_6159_0_1 == 1 | n_6159_0_1== 3
replace i33 = 1 if n_6159_0_2 == 1 | n_6159_0_2== 3
	
	* no values for head/neck in array 3 or 4



* 34 - back pain


gen i34_back_pain = 0 if n_6159_0_0!=.
replace i34 =1 if n_6159_0_0==4
replace i34=. if n_6159_0_0==-3

replace i34 = 1 if n_6159_0_1==4
replace i34 = 1 if n_6159_0_2==4
replace i34 = 1 if n_6159_0_3==4


* 35 - stomach/abdominal pain

gen i35_stomach_pain = 0 if n_6159_0_0!=.
replace i35 =1 if n_6159_0_0==5
replace i35=. if n_6159_0_0==-3

replace i35 = 1 if n_6159_0_1==5
replace i35 = 1 if n_6159_0_2==5
replace i35 = 1 if n_6159_0_3==5
replace i35 = 1 if n_6159_0_4==5




* 36 - Hip pain

gen i36_hip_pain = 0 if n_6159_0_0!=.
replace i36 =1 if n_6159_0_0==6
replace i36=. if n_6159_0_0==-3

replace i36 = 1 if n_6159_0_1==6
replace i36 = 1 if n_6159_0_2==6
replace i36 = 1 if n_6159_0_3==6
replace i36 = 1 if n_6159_0_4==6
replace i36 = 1 if n_6159_0_5==6


* 37 - knee pain 

gen i37_knee_pain = 0 if n_6159_0_0!=.
replace i37 = 1 if n_6159_0_0==7
replace i37 = . if n_6159_0_0==-3

replace i37 = 1 if n_6159_0_1==7
replace i37 = 1 if n_6159_0_2==7
replace i37 = 1 if n_6159_0_3==7
replace i37 = 1 if n_6159_0_4==7
replace i37 = 1 if n_6159_0_5==7
replace i37 = 1 if n_6159_0_6==7



* 38 - whole-body pain

gen i38_wholebody_pain = 0 if n_6159_0_0!=.
replace i38 = 1 if n_6159_0_0==8
replace i38 = . if n_6159_0_0==-3

	* no values in other array variables

	
* 39 - facial pain

gen i39_facial_pain = 0 if n_6159_0_0==-7
replace i39 = 1 if n_6159_0_0==2
replace i39 = 0 if n_6159_0_0!=2 & n_6159_0_0!=.
replace i39 = . if n_6159_0_0==-3

replace i39 = 1 if n_6159_0_1==2
replace i39 = 1 if n_6159_0_2==2
replace i39 = 1 if n_6159_0_3==2
replace i39 = 1 if n_6159_0_4==2
replace i39 = 1 if n_6159_0_5==2
replace i39 = 1 if n_6159_0_6==2


* 40 - Sleep

gen i40_sleep = 0 if n_1200==1
replace i40 = 0.5 if n_1200==2
replace i40 = 1 if n_1200==3


* 41 - High cholesterol (need to combine separate vars for male and female participants)

	* female vars n_6153_0; male n_6177_0

gen i41_cholesterol =  0 if n_6153_0_0!=1 & n_6177_0_0!=1
replace i41 = 1 if n_6153_0_0==1
replace i41 = 1 if n_6177_0_0==1

	* no cholesterol therapeutic uses recorded in other array vars 

replace i41= . if n_6153_0_0==-3 |  n_6177_0_0==-3
replace i41= . if n_6153_0_0==-1 |  n_6177_0_0==-1



* 42 - Pneumonia - code 1398 in 20002


gen i42_pneumonia = 0
replace i42 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i42 = 1 if n_20002_0_`n'==1398
}


* 43 - Gastric reflux - code 1138 in 20002

gen i43_gastric_reflux = 0
replace i43 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i43 = 1 if n_20002_0_`n'==1138
}


* 44 - Hiatus hernia - code 1474 in 20002

gen i44_hernia = 0
replace i44 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i44 = 1 if n_20002_0_`n'==1474
}


* 45 - diverticulitis - code 1458 in 20002


gen i45_diverticulitis = 0
replace i45 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i45 = 1 if n_20002_0_`n'==1458
}


* 46 - Gall stones - code 1162 in 20002

gen i46_gall_stones = 0
replace i46 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i46 = 1 if n_20002_0_`n'==1162
}


* 47 - Psoriasis - code 1453 in 20002

gen i47_psoriasis = 0
replace i47 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i47 = 1 if n_20002_0_`n'==1453
}


* 48 - Osteoporosis - code 1309 in 20002

gen i48_osteoporosis = 0
replace i48 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i48 = 1 if n_20002_0_`n'==1309
}


* 49 - Migraine - code 1265 in 20002 

gen i49_migraine = 0
replace i49 =. if n_2473_0_0==-3

forvalues n =  0(1)28 {
replace i49 = 1 if n_20002_0_`n'==1265
}



************************************

* Deriving FI - sum up items, divide by 49, and multiply by 100 (to display as percentages) or 10 (for results displayed by 10% increases)

mark all_items
markout all_items i*

egen total_points = rowtotal(i1_-i49_) if all_items

gen fi_49item = (total_points/49)

lab var fi "Frailty Index with 49 items - complete case set"


* Deriving missing data count for items in those with incomplete data:

egen missing_item_count = rowmiss(i1_-i49_) 

count if  missing_item_count>=10
		
		*   N=2,292 with data missing on 10 or more items - to be dropped

drop if missing>=10 // Leaves N= 500339 


	* Scaling FI to be on 1 and 10 percent increases (0.01 and 0.1 unit increments on FI scale)
gen fi_percent = fi_49item * 100

gen fi_10percent = fi_49item * 10


* categorised FI, 0.1 increments but combining all individuals with FI over 40:

egen ficat_10percent = cut(fi_percent), at(0,10,20,30,40,60)


/* Extra script for deriving a 'denominator-varying' FI - 
 * A commonly used approach to provide an FI on full samples where missing item data are present. 
 * This calculation divides summed deficits for items with available data by denominators that vary depending on the available item number

egen total_points_dvFI = rowtotal(i1_-i49_) 

gen fi_dv = total_points_dvFI/(49-missing_item_count)

lab var fi_dv "Frailty Index for whole cohort using total-varying denominators"

*/


******* Preparing ethnicity into a variable with limited number of categories (as per groupings on UKB Data Showcase)

gen ethnicity = . if n_21000_0_0 < 0

	* White category:
replace ethnicity = 1 if n_21000_0_0 == 1  
replace ethnicity = 1 if n_21000_0_0 >= 100 &  n_21000_0_0 < 2000

	* Asian category:
replace ethnic = 2 if n_21000_0_0 >2004 & n_21000_0_0<4000
replace ethnic = 2 if n_21000_0_0 == 3

	* Black category:
replace ethnic = 3 if n_21000_0_0>4000 & n_21000_0_0!=.
replace ethnic = 3 if n_21000_0_0 == 4

	* Mixed:
replace ethnic = 4 if n_21000_0_0>2000 & n_21000_0_0<3000
replace ethnic = 4 if n_21000_0_0==2

	* Chinese
replace ethnic = 5 if n_21000_0_0==5

	* Other
replace ethnic = 6 if n_21000_0_0==6

label define ethnic_groups 1 "White" 2 "Asian or Asian British" 3 "Black or Black British" 4 "Mixed" 5 "Chinese" 6 "Other"
label values ethnicity ethnic_groups



*** Death indicator:

gen died = 1 if n_40007_0_0!=.
replace died = 0 if n_40007_0_0==.

	* Recoding indicator for those that died after overall censorship date....
		/*Current UKB censoring ends Nov 2015 - some deaths are recorded from particular death stat sources after that, 
		but coverage isn't complete for all participants. Thus, any deaths at that date need replacing to 0 as if censored
		NB - more info on UKB censoring dates here: http://biobank.ctsu.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates */

di date("20151130","YMD") 
replace died = 0 if ts_40000_0_0>20422 & ts_40000_0_0!=.


*** Coding a time variable based on attained age = age at death or censorship - age at baseline


* Coding age at censorship for survivors:

gen censor_date = date("30/11/2015", "DMY") // Current overall censorship date - 30th Nov, 2015
replace censor_date = ts_40000_0_0 if died==1
format censor_date %d 

gen time_to_censor = (censor_date - ts_53_0_0) 

/* NB: returns three negative values - these individuals have death dates prior to their attendance dates;
	 ID numbers: 3077398 5373093 3448373
	One participant (5373093) has an alternate instance for death date (presumably where a second death certificate was issued, perhaps after post-mortem, as suggested in UKB death stats doc)
	HOWEVER - email from UKB Data Access team confirmed that all three should be dropped from analyses
*/


* Dropping the individuals with death dates before attendance dates:
drop if time_to_censor<0
	
	

* Variable that indicates the age at either censorship or death:

gen time_to_censor_years = (time_to_censor)/365.25
gen age_censor = n_21003_0_0 + time_to_censor_years


* coding a date of birth variable from month and year vars. All individuals assigned 15 as day of month born. 

gen day_born = 15
gen birth_date = mdy(n_52_0_0, day_born, n_34_0_0) 
format birth_date %d 


/* Setting stata into survival modelling mode,

- Time variable - Date of death or censoring
- Origin point - birth date (NB: approx b-date coded below from only month and year of birth vars available)
- Entry point: date at baseline assessment 
- Exit: death or censoring date, 30th Nov 2015

*/

stset censor_date, id(n_eid) failure(died==1) origin(birth_date) entry(ts_53_0_0) exit(time mdy(11,30,2015)) scale(365.25)



* Survival analyses on complete-case data:
	* Model 1 (adjusted for age only):
stcox fi_percent
stcox fi_10percent

	* Model 2:
stcox fi_percent n_31 
stcox fi_10percent n_31 

	* Model 3:
xi: stcox fi_percent n_31 i.ethnic
xi: stcox fi_10percent n_31 i.ethnic

* Checking PH assumption by log-log plot
stphplot, by(ficat_10percent)

* Checking PH by Schoenfeld: 
xi: stcox fi_10percent n_31 i.ethnic
estat phtest, detail
estat phtest, plot(fi_10percent) lineopts(lc(red))


* Hazards plot by FI category:
sts graph, hazard by(ficat_10percent) 

* Unadjusted survival curve by FI category
sts graph, by(ficat_10percent)  noorigin yla(.5(.1)1)

	
 * N-A cumulative hazards plots:
sts graph, cumhaz by(ficat_10percent)

 
* Generating the cumulative hazards function (nelson-aalen estimator) for including in MI input varlist:
sts gen cumul_haz = na




********************* Imputation ****************************

set seed 1066

* Test var set:

keep n_eid i* ///
cumul_haz died birth_date ts_53_0_0 censor_date n_21003_0_0 ethnicity n_31_0_0 _st _d _origin _t _t0

* Setting program for MI:
mi set mlong 


* Registering variables to be imputed:

	* Any with missing data:
mi register imputed i1_glaucoma i2_cataracts i3_hearing i4_infirmity i5_self_health ///
i6_falls i7_wheeze i8_fatigue i9_chd i10_angina i11_stroke i12_hypertension  ///
 i16_dental i17_chest_pain  i19_diabetes i20_cancer i21_multi_cancer i22_fractures /// 
i23_dvt i24_emphysema_cbronc i25_asthma i26_allergies  i28_depression i29_anxiousness ///
i31_misery i32_loneliness i33_headneck_pain i34_back_pain i35_stomach_pain ///
i36_hip_pain i37_knee_pain i38_wholebody_pain i39_facial_pain i40_sleep i41_cholesterol ethnicity


	* Vars/items with complete data
mi register regular n_eid cumul_haz died birth_date ts_53_0_0 censor_date n_21003_0_0 n_31_0_0 _st _d _origin _t _t0 ///
i13_rh_arth i14_osteoarth i15_gout i18_sciatica i27_hypothyroidism i30_severe_anxiety i42_pneumonia i43_gastric_reflux ///
i44_hernia i45_diverticulitis i46_gall_stones i47_psoriasis i48_osteoporosis i49_migraine



/* Multivariate imputation settings for vars of multiple types:

* List of binary vars to impute with logit:
i1_ i2_ i3_ i4_  i7 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 ///
i19 i20 i21 i22 i23 i24 i25 i26 i27 i29 i30 i31 i32 i33 i34 i35 ///
i36 i37 i38 i39 i41 i42 i34 i44 i45  i46 i47 i48 i49


* List of categorical vars (with uneven spacings) to impute with PMM:
i5 i6 i8 i28 i40 ethnicity


*/



***** MI command (NB: recommend running in parallel via Stata-MP with high-performance computing):

mi impute chained (logit, augment) i1_ i2_ i3_ i4_  i7 i9 i10 i11 i12  i16 i17 ///
i19 i20 i21 i22 i23 i24 i25 i26 i29 i31 i32 i33 i34 i35 i36 i37 i38 i39  i41 ///
(pmm, knn(5)) ethnicity i5 i6 i8 i28 i40 = _t0 _t _d cumul_haz n_31_0_0 i13 i14 i15 i18 ///
 i27 i30 i42 i43 i44 i45 i46 i47 i48 i49, add(5) noisily 


********* Analysis in full MI dataset with containing 5 imputations:


* Coding of FI within imputed data..... ran these commands and then saved a new set to avoid waiting for long variable generation procedures every time the set is loaded:


sort n_e _mi_m


* NB: data should be set in flong so that each imputation set has complete values for all, not just for those with missing values as in mlong style
mi convert flong

* creating FI in imputed data:

mi passive: egen miss_items = rowmiss(i*)
mi passive: egen total_points = rowtotal(i*) if miss_items==0 

	* it is necessary generate fi for all individuals with passive commands, rather than include FI var in imputation data and then use standard coding (the FI vals in imputed data get recoded to . in subsequent estimate commands)

mi passive: gen fi_49item = (total_points / 49)
mi passive: gen fi_percent = fi_49item*100
mi passive: gen fi_10percent = fi_49item*10
mi passive: egen ficat = cut(fi_percent), at(0,10,20,30,40,60)


* Age categories for examining FI distribution:
mi passive: egen agecats = cut(n_21003_0_0), at(35,50,60,65,75) icodes label


* Frac poly of FI on age:

fp <n_21003_0_0>: regress fi_percent <n_21003_0_0> if _mi_m==2

* FP Plot:
twoway scatter fi_49item _t0 if _mi_m==1, mfc(none) msiz(vsmall) mlc(gs8%20) || fpfit fi_49item _t0 if _mi_m==1, lcolor(gs0) lwidth(medium) ///
legend(label(1 "Observations") label(2 "Best fitting trend")) xlabel(40(10)75)  ///
xtitle("Age at baseline assessment") ytitle("Frailty Index values")


/* Testing frac poly models on other imputation sets and complete-case:
fp <n_21003_0_0>, replace: regress fi_percent <n_21003_0_0> if _mi_m==2
fp <n_21003_0_0>, replace: regress fi_percent <n_21003_0_0> if _mi_m==3
fp <n_21003_0_0>, replace: regress fi_percent <n_21003_0_0> if _mi_m==4
fp <n_21003_0_0>, replace: regress fi_percent <n_21003_0_0> if _mi_m==5

fp <n_21003_0_0>, replace: regress fi_percent <n_21003_0_0> if _mi_m==0

*/


** Survival models in MI:
	
	
****** Full set
mi stset censor_date, id(n_eid) failure(died==1) origin(birth_date) entry(ts_53_0_0) exit(time mdy(11,30,2015)) scale(365.25)


	* Model 1:
mi estimate: stcox fi_10percent
mi estimate, hr // need to add this post-estimate command to convert coeffs to HRs 

	* Model 2:
mi estimate:  stcox fi_10percent n_31 
mi estimate, hr 

	* Model 3:
mi estimate:  stcox fi_10percent n_31 i.ethnic
mi estimate, hr 



******* By sex:
	
xi: stcox i.n_31*fi_10percent if _mi_m==0 // interaction tested in complete-case data - P<0.001


 * Model 1: 
mi estimate: stcox fi_10percent if n_31==0
mi estimate, hr 

mi estimate: stcox fi_10percent if n_31==1
mi estimate, hr 

 * Model 3 (age and ethnicity)
mi estimate: stcox fi_10percent i.ethnic if n_31==0
mi estimate, hr 

mi estimate: stcox fi_10percent i.ethnic if n_31==1
mi estimate, hr 

	

* FI vs survival by baseline age groups:


foreach n of numlist 0(1)3 {

mi estimate: stcox fi_10percent if agecats==`n' 
mi estimate, hr 

mi estimate: stcox fi_10percent n_31 if agecats==`n' 
mi estimate, hr 

mi estimate: stcox fi_10percent n_31 i.ethnic if agecats==`n' 
mi estimate, hr 

}

	* repeat in complete-case:
foreach n of numlist 0(1)3 {

stcox fi_10percent if agecats==`n' & _mi_m==0

 stcox fi_10percent n_31 if agecats==`n' & _mi_m==0

xi: stcox fi_10percent n_31 i.ethnic if agecats==`n' & _mi_m==0


}


**** Averaging K-M curves from separate imputed sets (no official command for this):

	* Plotting based on FI averaged over five imputaitons:
	
mi convert wide, clear

gen fi_average = (_1_fi_49item+_2_fi_49item+_3_fi_49item+_4_fi_49item+_5_fi_49item)/5
gen fi_av_percent = fi_ave * 100
egen fi_av_cat = cut( fi_av_percent), at(0,10,20,30,40,60)
	
sts graph if _mi_m==0, by(fi_av_cat)  noorigin yla(.5(.1)1) title("") ///
ylab(0.55(0.1)1, format(%12.1fc)) ///
	plot1opts(lpattern(solid) lcolor(black)) ///
	plot2opts(lpattern(longdash_dot) lcolor(black)) ///
	plot3opts(lpattern(shortdash) lcolor(black)) ///
	plot4opts(lpattern(dash_dot) lcolor(black)) ///
	plot5opts(lpattern(dash) lcolor(black)) ///
legend( subtitle("FI categories") label(1 "< 0.1") label(2 "0.1 to < 0.3") label(3 "0.2 to < 0.3") label(4 "0.3 to < 0.4") label(5 "≥ 0.4")) ///
xtitle("Attained age") ytitle("Survival probability")


* Histogram based on averaged FI:

hist fi_average, color(gs13) lcolor(black) percent

lab var fi_average "Frailty Index score - proportion of deficits accrued"
twoway (histogram fi_average if n_31==0, bin(20)  percent color(gs13)) ///
       (histogram fi_average if n_31==1, bin(20) percent   ///
	   fcolor(none) lcolor(black)), legend(order(1 "Female" 2 "Male" ))
	   

