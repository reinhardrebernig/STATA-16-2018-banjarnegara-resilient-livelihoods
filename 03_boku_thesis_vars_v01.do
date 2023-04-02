

/////////////////////////////////////////////////////////
////*************************************************////
////*Construction of additionally needed variables **////
////*************************************************////
/////////////////////////////////////////////////////////


********************
*Crop diversity ****
********************

*Value of perennial crops (the larger value of value of production and gross revenue was considered to make it comparable to seasonal crops gross revenue)
local perenn_val ///
c4_q2_percrop_1 c4_q2_percrop_2 c4_q2_percrop_3 c4_q2_percrop_4 c4_q2_percrop_5 c4_q2_percrop_6 c4_q2_percrop_7 c4_q2_percrop_8 c4_q2_percrop_9 c4_q2_percrop_10 c4_q2_percrop_11 c4_q2_percrop_12 c4_q2_percrop_13 c4_q2_percrop_14 c4_q2_percrop_15 c4_q2_percrop_16 c4_q2_percrop_17 c4_q2_percrop_18 c4_q2_percrop_19 // perennial crops value of production
local perenn_rev ///
c4_q6_percrop_1 c4_q6_percrop_2 c4_q6_percrop_3 c4_q6_percrop_4 c4_q6_percrop_5 c4_q6_percrop_6 c4_q6_percrop_7 c4_q6_percrop_8 c4_q6_percrop_9 c4_q6_percrop_10 c4_q6_percrop_11 c4_q6_percrop_12 c4_q6_percrop_13 c4_q6_percrop_14 c4_q6_percrop_15 c4_q6_percrop_16 c4_q6_percrop_17 c4_q6_percrop_18 c4_q6_percrop_19
local n : word count `perenn_val'
forvalues i = 1/`n' {
	local a : word `i' of `perenn_val'
	local b : word `i' of `perenn_rev'
	gen 	sum_`a' = 0
	replace sum_`a' = `b' if `b'<.
	replace sum_`a' = `a' if `a'>`b' & `a'<. 
}

*Perennial crops - total gross value or gross revenue
egen perenn_rev = rowtotal(sum_c4_q2_percrop_1 sum_c4_q2_percrop_2 sum_c4_q2_percrop_3 sum_c4_q2_percrop_4 sum_c4_q2_percrop_5 sum_c4_q2_percrop_6 sum_c4_q2_percrop_7 sum_c4_q2_percrop_8 sum_c4_q2_percrop_9 sum_c4_q2_percrop_10 sum_c4_q2_percrop_11 sum_c4_q2_percrop_12 sum_c4_q2_percrop_13 sum_c4_q2_percrop_14 sum_c4_q2_percrop_15 sum_c4_q2_percrop_16 sum_c4_q2_percrop_17 sum_c4_q2_percrop_18 sum_c4_q2_percrop_19)
g crop_totval= perenn_rev + f_rev_seasc

*g simpson index
local crop_rev ///
sum_c4_q2_percrop_1 sum_c4_q2_percrop_2 sum_c4_q2_percrop_3 sum_c4_q2_percrop_4 sum_c4_q2_percrop_5 sum_c4_q2_percrop_6 sum_c4_q2_percrop_7 sum_c4_q2_percrop_8 sum_c4_q2_percrop_9 sum_c4_q2_percrop_10 sum_c4_q2_percrop_11 sum_c4_q2_percrop_12 sum_c4_q2_percrop_13 sum_c4_q2_percrop_14 sum_c4_q2_percrop_15 sum_c4_q2_percrop_16 sum_c4_q2_percrop_17 sum_c4_q2_percrop_18 sum_c4_q2_percrop_19 /// perennial crops value of production
c1_revenue_crop_1 c1_revenue_crop_2 c1_revenue_crop_3 c1_revenue_crop_4 c1_revenue_crop_5 c1_revenue_crop_6 c1_revenue_crop_7 c1_revenue_crop_8 c1_revenue_crop_9 c1_revenue_crop_10 c1_revenue_crop_11 c1_revenue_crop_12 c1_revenue_crop_13 c1_revenue_crop_14 // seasonal crops
foreach var of local crop_rev {
	g crops`var' = `var'/crop_totval
	replace crops`var' = 0 if crops`var'==.
	g sqcrp`var' = (crops`var')^2
	replace sqcrp`var' = 0 if sqcrp`var'==.
}

egen crop_div_straight = rowtotal(sqcrpsum_c4_q2_percrop_1 sqcrpsum_c4_q2_percrop_2 sqcrpsum_c4_q2_percrop_3 sqcrpsum_c4_q2_percrop_4 sqcrpsum_c4_q2_percrop_5 sqcrpsum_c4_q2_percrop_6 sqcrpsum_c4_q2_percrop_7 sqcrpsum_c4_q2_percrop_8 sqcrpsum_c4_q2_percrop_9 sqcrpsum_c4_q2_percrop_10 sqcrpsum_c4_q2_percrop_11 sqcrpsum_c4_q2_percrop_12 sqcrpsum_c4_q2_percrop_13 sqcrpsum_c4_q2_percrop_14 sqcrpsum_c4_q2_percrop_15 sqcrpsum_c4_q2_percrop_16 sqcrpsum_c4_q2_percrop_17 sqcrpsum_c4_q2_percrop_18 sqcrpsum_c4_q2_percrop_19 sqcrpc1_revenue_crop_1 sqcrpc1_revenue_crop_2 sqcrpc1_revenue_crop_3 sqcrpc1_revenue_crop_4 sqcrpc1_revenue_crop_5 sqcrpc1_revenue_crop_6 sqcrpc1_revenue_crop_7 sqcrpc1_revenue_crop_8 sqcrpc1_revenue_crop_9 sqcrpc1_revenue_crop_10 sqcrpc1_revenue_crop_11 sqcrpc1_revenue_crop_12 sqcrpc1_revenue_crop_13 sqcrpc1_revenue_crop_14)
g crop_div_inv= 1/(crop_div_straight)
replace crop_div_inv=0 if crop_div_inv == . 

*graph box crop_div_inv, over(village, total)
ranksum crop_div_inv, by(village)
ttest crop_div_inv, by(village)
extremes crop_div_inv


****************************************************
*Livestock - TLU and livestock diversity index *****
****************************************************
* http://www.fao.org/3/ca6400en/ca6400en.pdf 
*https://www.agrecol.de/files2/Trends%20in%20livestock%20production.pdf

g tlu_cow		= c5_cow_n	   * 0.7
g tlu_goat		= c5_goat_n	   * 0.1
g tlu_chick 	= c5_chick_n   * 0.01
g tlu_duck 		= c5_duck_n    * 0.03
g tlu_menthok	= c5_menthok_n * 0.03
g tlu_rabbit 	= c5_rabbit_n  * 0.02

replace tlu_cow			= 0 if tlu_cow 	==.
replace tlu_goat		= 0 if tlu_goat	==.
replace tlu_chick		= 0 if tlu_chick ==.
replace tlu_duck		= 0 if tlu_duck ==.
replace tlu_menthok		= 0 if tlu_menthok ==.
replace tlu_rabbit		= 0 if tlu_rabbit ==.

egen tlu_tot = rowtotal(tlu_cow tlu_goat tlu_chick tlu_duck tlu_menthok tlu_rabbit)

*Simpson index for livestock
local lvstck tlu_cow tlu_goat tlu_chick tlu_duck tlu_menthok tlu_rabbit
foreach var of local lvstck {
	g lvstck`var' = `var'/tlu_tot
	replace lvstck`var' = 0 if lvstck`var'==.
	g sq`var' = (lvstck`var')^2
	replace sq`var' = 0 if sq`var'==.
}

egen lvstck_div_straight = rowtotal(sqtlu_cow sqtlu_goat sqtlu_chick sqtlu_duck sqtlu_menthok sqtlu_rabbit)
g lvstck_div_inv= 1/(lvstck_div_straight)
replace lvstck_div_inv=0 if lvstck_div_inv == . 

***
*graph box lvstck_div_inv, over(village, total)
ranksum lvstck_div_inv, by(village)
ttest lvstck_div_inv, by(village)
extremes lvstck_div_inv




**********************
*Income diversity  ***
**********************

*Simpson index for income diversity
local ydiv f_y_seasc f_y_perennc f_y_ah f_y_fish f_y_farm f_y_offnonf
foreach var of local ydiv {
	g div_`var' = `var'/f_y_tot
	replace div_`var' = 0 if div_`var'==.
	g sq_`var' = (div_`var')^2
	replace sq_`var' = 0 if sq_`var'==.
}

egen y_div_straight = rowtotal(sq_f_y_seasc sq_f_y_perennc sq_f_y_ah sq_f_y_fish sq_f_y_farm sq_f_y_offnonf)
g y_div_inv= 1/(y_div_straight)
replace y_div_inv=0 if y_div_inv == . 

***
*graph box y_div_inv, over(village, total)
*graph box f_y_seasc f_y_perennc f_y_ah f_y_fish f_y_farm f_y_offnonf f_y_tot, over(village)
ranksum y_div_inv, by(village)
ttest y_div_inv, by(village)
extremes y_div_inv


******************
*Max education  **
******************
*replacing 
replace a1_edu_m4 = "2" if a1_edu_m4 == "TK"
replace a1_edu_m5 = "2" if a1_edu_m5 == "TK"
replace a1_edu_m6 = "2" if a1_edu_m6 == "TK"

destring a1_edu_m4 a1_edu_m5 a1_edu_m6, replace
egen h_edu_max = rowmax(a1_edu_m1 a1_edu_m2 a1_edu_m3 a1_edu_m4 a1_edu_m5 a1_edu_m6 a1_edu_m7 a1_edu_m8 a1_edu_m9 a1_edu_m10)



******************************** 
*Per capita household income ***
********************************
*create age groups
egen h_members_old = rowtotal(h_nm_male_age_3 h_nm_fem_age_3)
replace h_members_old = h_members_old+1 if hh_age>64

egen h_members_young = rowtotal(h_nm_male_age_1 h_nm_fem_age_1)
replace h_members_young = h_members_young+1 if hh_age<15

egen h_members_working = rowtotal(h_nm_male_age_2 h_nm_fem_age_2)
replace h_members_working = h_members_working +1 if hh_age>14 & hh_age<64

egen h_size = rowtotal(h_members_working h_members_young h_members_old)
*graph box h_size, over(village, total)

*create indicators
g h_depratio = (h_members_old + h_members_young) / h_members_working // 2 missing
g h_depshare = (h_members_old + h_members_young) / (h_members_old + h_members_young + h_members_working)
*graph box h_depratio h_depshare, over(village, total)

*create pc y
g f_y_pc = f_y_tot / h_members_working

*create monthly y
g f_y_tot_mon = f_y_tot/12
g f_y_pc_mon = f_y_pc/12
g f_y_pc_mon_mio = f_y_pc_mon/1000000





***********************************************
***	CHAID: Diversification Variables	*******
***********************************************

*preperations 
g b1_land_op_ha = b1_land_op/10000
g f_y_farm_perc = 1-f_y_offnonf_perc

****
*total cost of fertilizer and pesticides per ha

*perennial crop cost
g c4_fert_totcost =0 
lab var c4_fert_totcost "Fertilizer cost perennial crops (IDR)"
replace c4_fert_totcost= 115000 if adm_code_ugm_1=="KAR1VIN"
replace c4_fert_totcost= 190000 if adm_code_ugm_1=="KAR6VIN"
replace c4_fert_totcost= 125000 if adm_code_ugm_1=="KAR7VIN"
replace c4_fert_totcost= 240000 if adm_code_ugm_1=="KAR8VIN"
replace c4_fert_totcost= 30000  if adm_code_ugm_1=="KAR33SAR"
replace c4_fert_totcost= 125000 if adm_code_ugm_1=="KAR39NAD"

*total fertilizer cost
g f_cost_fert=c4_fert_totcost + c3_fert_totcost
*fertilizer cost per hectare 
g f_cost_fert_ha = f_cost_fert / b1_land_op_ha
*fert cost in mio
g mio_f_cost_fert_ha = f_cost_fert_ha/1000000
*fert cost in mio per month
g mo_mio_f_cost_fert_ha = mio_f_cost_fert_ha/12


*pesticide cost 
clonevar f_cost_pest = c3_pest_totval
*pest cost per hectare
g f_cost_pest_ha = c3_pest_totval / b1_land_op_ha
*pest cost in mio 
g mio_f_cost_pest_ha = f_cost_pest_ha/1000000
*pest cost in mio per month
g mo_mio_f_cost_pest_ha = mio_f_cost_pest_ha/12



*labour cost
*perennial labour cost
g f_cost_lab_perennc =0 
lab var f_cost_lab_perennc "Labour costs perennial crops (IDR)"
replace f_cost_lab_perennc= 200000 if adm_code_ugm_1=="KAR21DIL"
*total labour cost
g f_cost_lab = c2_lab_cost + f_cost_lab_perennc
lab var f_cost_lab "Total labour cost (IDR)"
*lab cost per hectare
g f_cost_lab_ha = f_cost_lab / b1_land_op_ha
*lab cost in mio 
g mio_f_cost_lab_ha = f_cost_lab_ha/1000000
*lab cost in mio per month
g mo_mio_f_cost_lab_ha = mio_f_cost_lab_ha/12

g mo_lab_size=c2_lab_famhrs/(12*h_members_working)


*seed cost
egen f_cost_seed = rowtotal(c3_seed_value c4_q8_percrop_1 c4_q8_percrop_2 c4_q8_percrop_3 c4_q8_percrop_4 c4_q8_percrop_5 c4_q8_percrop_6 c4_q8_percrop_7 c4_q8_percrop_8 c4_q8_percrop_9 c4_q8_percrop_10 c4_q8_percrop_11 c4_q8_percrop_12 c4_q8_percrop_13 c4_q8_percrop_14 c4_q8_percrop_15 c4_q8_percrop_16 c4_q8_percrop_17 c4_q8_percrop_18 c4_q8_percrop_19)
*seed cost per hectare
g f_cost_seed_ha = f_cost_seed / b1_land_op_ha
*seed cost in mio 
g mio_f_cost_seed_ha = f_cost_seed_ha/1000000
*seed cost in mio per month
g mo_mio_f_cost_seed_ha = mio_f_cost_seed_ha/12

egen r_cost_seed_perennc = rowtotal(c4_q8_percrop_1 c4_q8_percrop_2 c4_q8_percrop_3 c4_q8_percrop_4 c4_q8_percrop_5 c4_q8_percrop_6 c4_q8_percrop_7 c4_q8_percrop_8 c4_q8_percrop_9 c4_q8_percrop_10 c4_q8_percrop_11 c4_q8_percrop_12 c4_q8_percrop_13 c4_q8_percrop_14 c4_q8_percrop_15 c4_q8_percrop_16 c4_q8_percrop_17 c4_q8_percrop_18 c4_q8_percrop_19)

g seed_perc_perennc= r_cost_seed_perennc/f_cost_seed
g seed_perc_seasc= c3_seed_value/f_cost_seed

tabstat seed_perc_perennc seed_perc_seasc, by(strategy) statistics(mean sd median iqr) column(statistics) longstub nototal

****
*income to scale
g f_y_farm_100 = f_y_farm/100000
g f_y_tot_100 = f_y_tot/100000

local mio f_y_seasc_100 f_y_perennc_100 f_y_ah_100 f_y_fish_100 f_y_offnonf_100 f_y_farm_100 f_y_tot_100
foreach var of local mio {
	g mio_`var' = `var'/10
}

****
*income per month
local mio mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah_100 mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 mio_f_y_tot_100
foreach var of local mio {
	g mo_`var' = `var'/12
}

*prep
g f_y_tot_pos = f_y_tot
replace f_y_tot_pos = 0 if f_y_tot < 0 
g ln_y_tot= ln(f_y_tot_pos)
replace ln_y_tot = 0 if f_y_tot < 0 



////////////////////////////////////////////
*benefit-cost ratio
g bc_ratio_seasc = f_y_seasc / f_cost_seasc
g bc_ratio_farm  = f_y_farm  / f_cost_farm
g bc_ratio_perennc = f_y_perennc / f_cost_perennc
g bc_ratio_ah = f_y_ah / f_cost_ah
g bc_ratio_fish = f_y_fish / f_cost_fish
g bc_ratio_seasc_perennc = (f_y_seasc + f_y_perennc) / (f_cost_seasc + f_cost_perennc)


////////////////////////////////////////////
*profitability analysis
g crop_gm = ((f_rev_seasc + f_rev_perennc) - (f_cost_seasc +f_cost_perennc)) / b1_land_op_ha //calculating crop gross margin 
g crop_gm_mio= crop_gm / 1000000
g mo_crop_gm_mio = crop_gm_mio/12

encode village, g(vill)


****
*number of crops 
g cd_n_totperenn = cd_n_perenn + cd_n_valuetrees

/*
correlation crop revenue with over all costs, individual costs, combined costs


*/

*********************************
*** hazards					*****
*********************************

encode f2_shock_1, generate(shock1)
encode f2_shock_2, generate(shock2)

*level 1
g shock_ideosyncratic =1 if  shock1 == 8 | shock1 == 14 | shock1 ==15 | shock2 == 2
g shock_drought = 1 if shock1==2 | shock2==3
g shock_wet = 1 if shock1==5 | shock1==6 | shock2==4 | shock2==5
g shock_harvestf = 1 if shock1==7
g shock_landslide =1 if shock1==9 
g shock_none=1 if shock1==10
g shock_market=1 if shock1==11 | shock2==6
g shock_seedqual=1 if shock1==12 
g shock_storm=1 if shock1==13 | shock2==7
g shock_traderdefault=1 if shock1==16
g shock_animals = 1 if shock2==1 



*********************************
*** coping strategies 		*****
*********************************

egen f3_cope_tot = rowtotal(f3_cope_plusinp f3_cope_irr_ws f3_cope_redinv f3_cope_fallow f3_cope_sell_assets f3_cope_selltrees f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork fr_cope_savings f3_cope_selllvstck f3_cope_formcred f3_cope_infcred f3_cope_earlharv f3_cope_fencingoff f3_cope_traders)






*******************************************************
***OTHER	(mostly for regression)				*******
*******************************************************

*agroforestry share in unit
g b_cd2_100 = b_cd2 *100
clonevar agroforestry_share_1 = b_cd2_100
replace agroforestry_share_1 = b_cd2_100/100



*irrigation system
g irrig_sys = d5_irrigs_highest
label var irrig_sys "Irrigation system type"
label define irrig_sys 1 "Rainfed" 2 "Simple water supply" 3 "Semi-technical" 4 "Technical" 






*occupations
egen work_occ_n_tot_main = anycount (hh_mainocc_1 hh_mainocc_2 hh_mainocc_3 hh_mainocc_4 hh_mainocc_5 hh_mainocc_6 hh_mainocc_7 hh_mainocc_8 hh_mainocc_9), v(1)
egen work_occ_n_tot_side = anycount (hh_sideocc_1 hh_sideocc_2 hh_sideocc_3 hh_sideocc_4 hh_sideocc_5 hh_sideocc_6 hh_sideocc_7 hh_sideocc_8 hh_sideocc_9 hh_sideocc_10 hh_sideocc_11 hh_sideocc_12), v(1)

local work hh_mainocc_1 hh_mainocc_2 hh_mainocc_3 hh_mainocc_4 hh_mainocc_5 hh_mainocc_6 hh_mainocc_7 hh_mainocc_8 hh_mainocc_9  hh_sideocc_1 hh_sideocc_2 hh_sideocc_3 hh_sideocc_4 hh_sideocc_5 hh_sideocc_6 hh_sideocc_7 hh_sideocc_8 hh_sideocc_9 hh_sideocc_10 hh_sideocc_11 hh_sideocc_12 h_nm_mainocc_1 h_nm_mainocc_2 h_nm_mainocc_3 h_nm_mainocc_4 h_nm_mainocc_5 h_nm_mainocc_6 h_nm_mainocc_7 h_nm_mainocc_8 h_nm_sideocc_1 h_nm_sideocc_2 h_nm_sideocc_3 h_nm_sideocc_4 h_nm_sideocc_5 h_nm_sideocc_6
foreach var of local work {
	g work_`var' = `var' 
	replace work_`var' = 0 if `var'==. 
}

egen work_occ_n_tot = anycount(hh_mainocc_1 hh_mainocc_2 hh_mainocc_3 hh_mainocc_4 hh_mainocc_5 hh_mainocc_6 hh_mainocc_7 hh_mainocc_8 hh_mainocc_9  hh_sideocc_1 hh_sideocc_2 hh_sideocc_3 hh_sideocc_4 hh_sideocc_5 hh_sideocc_6 hh_sideocc_7), v(1) 
replace f_cost_lab_perennc= 200000 if adm_code_ugm_1=="KAR27IND"



*per capita monthly income
g ln_f_y_pc_mon_mio 		= ln(f_y_pc_mon_mio)
g ln_f_cost_fert_mon_mio	= ln(mo_mio_f_cost_fert_ha)
g ln_f_cost_pest_mon_mio	= ln(mo_mio_f_cost_pest_ha)

lab var ln_f_y_pc_mon_mio "Log_n of monthly per capita income"
lab var ln_f_cost_fert_mon_mio "Log_n of monthly per hectare fertilizer spending"
lab var ln_f_cost_pest_mon_mio "Log_n of monthly per hectare pesticide spending"
 
 


 
 