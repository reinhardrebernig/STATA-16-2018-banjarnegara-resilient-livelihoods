


***********************************************
****				testing				*******
***********************************************

signtest f_y_pc_mon_mio=2.692 //average income
signtest f_y_pc_mon_mio=0.392154 //powervty liine

*income

median  mo_mio_f_y_tot_100, by(village)
ranksum mo_mio_f_y_tot_100, by(village) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

*association of strategies with villages
tab village strategy, chi2 V
median  b_cd2_100, by(village)
ranksum b_cd2_100, by(village) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

median  f_y_ah_perc, by(village)
ranksum f_y_ah_perc, by(village)
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

tabstat mo_mio_f_y_tot_100, by(village) statistics(mean sd p50 iqr)
qreg2 mo_mio_f_y_tot_100 i.vill

ranksum mo_mio_f_y_tot_100, by(village) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

ranksum f_y_tot, by(strategy)

ranksum f_y_seasc, by(village)
ranksum f_y_seasc, by(strategy)

ranksum f_cost_fert_ha, by(village)
ranksum f_cost_fert_ha, by(strategy)

ranksum f_cost_pest_ha, by(village)
ranksum f_cost_pest_ha, by(strategy)

*test for normality
swilk hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket
//only hh_age hh_edu h_size crop_div_inv are normal

set seed 12345
cls
foreach var of varlist hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees  cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_h {  
	di "MEDIAN DIFFERENCE `var'"
	qreg2 `var' i.vill
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}

cls
foreach var of varlist mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_seed_ha {  
	di "MEDIAN DIFFERENCE `var'"
	qreg2 `var' i.vill
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}

*bsqreg mo_mio_f_y_farm_100 i.vill, reps(1000)
*bsqreg f_y_perennc_perc i.vill, reps(1000) 

prtest e1_creditinf, by(vill)
prtest e1_creditform, by(vill)

median k1_15_accmarket, by(village)
ranksum k1_15_accmarket, by(village) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

graph box k1_15_accmarket, by(village)
*bsqreg f_y_pc_mon_mio i.vill, reps(1000)
qreg f_y_pc_mon_mio i.vill, vce(r)
qreg2 f_y_pc_mon_mio i.vill

median f_y_pc_mon_mio, by(village)
ranksum f_y_pc_mon_mio, by(village) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)

*median- and ranksum- testing
cls
local chaid hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket
foreach var of local chaid {
	di "RANKSUM `var'"
	ranksum `var', by(village) exact porder
		scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
		scalar define U2 = r(N_1)*r(N_2)-U1
		display _newline ///
		in smcl as text "U1 = " as result U1 _newline ///
		in smcl as text "U2 = " as result U2 _newline ///
		in smcl as text "U  = " as result min(U1,U2)
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}

cls
foreach var of varlist hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_seed_ha e1_creditinf e1_creditform k1_15_accmarket {  
	di "hodges lehman difference"
	cendif `var', by(vill)
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}



cls
local chaid mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_seed_ha
foreach var of local chaid {
	di "RANKSUM `var'"
	ranksum `var', by(village) exact porder
		scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
		scalar define U2 = r(N_1)*r(N_2)-U1
		display _newline ///
		in smcl as text "U1 = " as result U1 _newline ///
		in smcl as text "U2 = " as result U2 _newline ///
		in smcl as text "U  = " as result min(U1,U2)
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}

cls
foreach var of varlist mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_seed_ha {
	di "qreg2 `var'"
	qreg2 `var' i.vill
	di "////////////////////////////"
	di "////////////////////////////"
	di "////////////////////////////"
}



mean f_y_tot if village=="Leksana"
mean f_y_tot if village=="Penanggungan"

ranksum f_y_pc_mon, by(village)




*coping
mean f3_cope_tot if strategy==1
mean f3_cope_tot if strategy==2
ttest f3_cope_tot, by(strategy)

univar f3_cope_tot, by(strategy)
power twomeans .55 .9210526, n1(40) n2(38) sd1(0.60 ) sd2(0.67) alpha(0.10) //power =    0.7186


*testing binary data e1_creditinf e1_creditform k1_15_accmarket
*CHI2 test of independence: H0: There is no relationship between x and y vars 
*
tab e1_creditinf village, col row chi2 V //  Pearson chi2(1) =   3.4703   Pr = 0.062  Cramér's V =   0.2137
tab e1_creditform village, col row chi2 V // Pearson chi2(1) =   1.5375   Pr = 0.215 Cramér's V =   0.1451
tab k1_15_accmarket village, col row chi2 V // Pearson chi2(4) =   5.4657   Pr = 0.243     Cramér's V =   0.2647

*Due to small sample size and non-normal distribution of most variables the proportion test was not considered. 
prtest e1_creditinf, by(village)
prtest e1_creditform, by(village)


*INCOME TESTING
tabstat f_y_pc_mon, statistics(mean sd median)
tabstat f_y_pc_mon, by(village) statistics(mean sd median)
tabstat f_y_pc_mon, by(strategy) statistics(mean sd median)

signtest f_y_pc_mon = 1490000

ttest f_y_pc_mon == 1490000 if village=="Leksana"
ttest f_y_pc_mon == 1490000 if strategy==1
signtest f_y_pc_mon = 1490000 if village=="Leksana"
signtest f_y_pc_mon = 1490000 if strategy==1

ttest f_y_pc_mon == 1490000 if village=="Penanggungan"
ttest f_y_pc_mon == 1490000 if strategy==2
signtest f_y_pc_mon = 1490000 if village=="Penanggungan"
signtest f_y_pc_mon = 1490000 if strategy==2

mean f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc if village=="Leksana"
mean f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc if village=="Penanggungan"

tabstat mo_mio_f_y_tot_100, statistics(mean sd median iqr) by(village)
ranksum mo_mio_f_y_tot_100, by(village)

tabstat mo_mio_f_y_seasc_100, statistics(mean sd median iqr) by(village)
ranksum mo_mio_f_y_seasc_100, by(village)

*CROPPING AND INPUT
swilk y_div_inv crop_div_inv lvstck_div_inv

****
ranksum immx_b_farmdiv_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 immx_b_farmdiv_index i.strategy
cendif immx_b_farmdiv_index, by(strategy)




*MARKET ACCESS AND CREDITS

*correlations between 


*testing income by strategy
swilk f_y_pc_mon_mio mo_mio_f_y_tot_100

ranksum f_y_pc_mon_mio, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 f_y_pc_mon_mio i.strategy
cendif f_y_pc_mon_mio, by(strategy)


ranksum mo_mio_f_y_tot_100, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 mo_mio_f_y_tot_100 i.strategy
cendif mo_mio_f_y_tot_100, by(strategy)
	
tabstat f_y_pc_mon_mio mo_mio_f_y_tot_100, by(strategy) stats(mean sd median iqr) nototal column(statistics)

graph box ///
	f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, ///
	by(strategy, compact title("Income sources of livelihood strategies") note("Agroforestry n=40, Conventional n=38")) ///
	scheme(plottigblind) intensity(50) ytitle("Percent of total income") ///
	legend(order(1 "Seasonal crops" 2 "Perennial crops" 3 "Livestock" 4 "Fisheries" 5 "Off-farm income") rows(1) position(6)) ///
	ylabel(0(0.1)1) yscale(range(0 1)) yline(0(0.1)1, lstyle(grid) lcolor(white) lwidth(thin))

graph bar f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, by(strategy, compact title("Income sources of livelihood strategies") note("Agroforestry n=40, Conventional n=38")) scheme(plottigblind) intensity(50) stack

tabplot f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, by(strategy) stack

graph box f_y_pc_mon_mio mo_mio_f_y_tot_100 if mo_mio_f_y_tot_100<12.5, by(strategy, compact title("Monthly household and per capita income") note("Boxplots exclude major outliers""Agroforestry n=40, Conventional n=38")) ///
	scheme(plottigblind) intensity(50) ytitle("Mio IDR/month") ///
	legend(order(1 "Per capita income" 2 "Household income") rows(1) position(6)) ///
	ylabel(-6(2)13) yscale(range(-6 13)) yline(-6(1)13, lstyle(grid) lcolor(white) lwidth(thin)) 


tabstat f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, by(strategy) statistics(mean sd median iqr) nototal longstub column(statistics)

tabstat mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100, by(strategy) statistics(mean sd median iqr) nototal longstub column(statistics)

swilk bc_ratio_seasc bc_ratio_seasc_perennc mo_crop_gm_mio

tabstat bc_ratio_seasc bc_ratio_seasc_perennc mo_crop_gm_mio, by(strategy) statistics(mean sd median iqr) nototal longstub column(statistics)

ranksum bc_ratio_seasc, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 bc_ratio_seasc i.strategy
cendif bc_ratio_seasc, by(strategy)

ranksum bc_ratio_seasc_perennc, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 bc_ratio_seasc_perennc i.strategy
cendif bc_ratio_seasc_perennc, by(strategy)

ranksum bc_ratio_farm, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 bc_ratio_farm i.strategy
cendif bc_ratio_farm, by(strategy)

ranksum mo_crop_gm_mio, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 mo_crop_gm_mio i.strategy
cendif mo_crop_gm_mio, by(strategy)



*inputs 
*seeds
univar mo_mio_f_cost_seed_ha, by(strategy)
swilk mo_mio_f_cost_seed_ha //no

ranksum mo_mio_f_cost_seed_ha, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 mo_mio_f_cost_seed_ha i.strategy
cendif mo_mio_f_cost_seed_ha, by(strategy)

*labour
univar c2_lab_famhrs, by(strategy)
swilk c2_lab_famhrs //no
ranksum c2_lab_famhrs, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 c2_lab_famhrs i.strategy
cendif c2_lab_famhrs, by(strategy)

*highred labour ratio 
univar c2_lab_hlr, by(strategy)
ranksum c2_lab_hlr, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 c2_lab_hlr i.strategy
cendif c2_lab_hlr, by(strategy)

*fertilizer
univar mo_mio_f_cost_fert_ha, by(strategy)
ranksum mo_mio_f_cost_fert_ha, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 mo_mio_f_cost_fert_ha i.strategy
cendif mo_mio_f_cost_fert_ha, by(strategy)

*manure per hectar
foreach var of varlist c3_fert_urea_kg c3_fert_kcl_kg  c3_fert_tsp_kg  c3_fert_za_kg  c3_fert_manure_kg  c3_fert_npk_kg  c3_fert_phonska_kg c3_fert_convet_kg  c3_fert_orgcm_kg  c3_fert_orgmes_kg  c3_fert_sp36_kg  c3_fert_hnopb_kg {
    g perha_`var'=`var'/(b1_land_op_ha)
	g perha_mon_`var'=perha_`var'/12
}

graph box perha_c3_fert_urea_kg perha_c3_fert_kcl_kg perha_c3_fert_tsp_kg perha_c3_fert_za_kg perha_c3_fert_manure_kg perha_c3_fert_npk_kg perha_c3_fert_phonska_kg perha_c3_fert_convet_kg perha_c3_fert_orgcm_kg perha_c3_fert_orgmes_kg perha_c3_fert_sp36_kg perha_c3_fert_hnopb_kg, by(strategy)

egen fert_tot_kg=rowtotal(perha_c3_fert_urea_kg perha_c3_fert_kcl_kg perha_c3_fert_tsp_kg perha_c3_fert_za_kg perha_c3_fert_manure_kg perha_c3_fert_npk_kg perha_c3_fert_phonska_kg perha_c3_fert_convet_kg perha_c3_fert_orgcm_kg perha_c3_fert_orgmes_kg perha_c3_fert_sp36_kg perha_c3_fert_hnopb_kg)

foreach var of varlist perha_c3_fert_urea_kg perha_c3_fert_kcl_kg perha_c3_fert_tsp_kg perha_c3_fert_za_kg perha_c3_fert_manure_kg perha_c3_fert_npk_kg perha_c3_fert_phonska_kg perha_c3_fert_convet_kg perha_c3_fert_orgcm_kg perha_c3_fert_orgmes_kg perha_c3_fert_sp36_kg perha_c3_fert_hnopb_kg {
    g perc_`var'= `var'/fert_tot_kg
}

graph box perc_perha_c3_fert_urea_kg perc_perha_c3_fert_kcl_kg perc_perha_c3_fert_tsp_kg perc_perha_c3_fert_za_kg perc_perha_c3_fert_manure_kg perc_perha_c3_fert_npk_kg perc_perha_c3_fert_phonska_kg perc_perha_c3_fert_convet_kg perc_perha_c3_fert_orgcm_kg perc_perha_c3_fert_orgmes_kg perc_perha_c3_fert_sp36_kg perc_perha_c3_fert_hnopb_kg, by(strategy)

tabstat perc_perha_c3_fert_urea_kg perc_perha_c3_fert_kcl_kg perc_perha_c3_fert_tsp_kg perc_perha_c3_fert_za_kg perc_perha_c3_fert_manure_kg perc_perha_c3_fert_npk_kg perc_perha_c3_fert_phonska_kg perc_perha_c3_fert_convet_kg perc_perha_c3_fert_orgcm_kg perc_perha_c3_fert_orgmes_kg perc_perha_c3_fert_sp36_kg perc_perha_c3_fert_hnopb_kg, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal

tabstat perha_mon_c3_fert_urea_kg perha_mon_c3_fert_tsp_kg  perha_mon_c3_fert_manure_kg perha_mon_c3_fert_npk_kg perha_mon_c3_fert_phonska_kg perha_mon_c3_fert_orgcm_kg perha_mon_c3_fert_orgmes_kg, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal

foreach var of varlist perha_mon_c3_fert_urea_kg perha_mon_c3_fert_tsp_kg  perha_mon_c3_fert_manure_kg perha_mon_c3_fert_npk_kg perha_mon_c3_fert_phonska_kg perha_mon_c3_fert_orgcm_kg perha_mon_c3_fert_orgmes_kg {
    ranksum `var', by(strategy) porder
	scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
	scalar define U2 = r(N_1)*r(N_2)-U1
	display _newline ///
	in smcl as text "U1 = " as result U1 _newline ///
	in smcl as text "U2 = " as result U2 _newline ///
	in smcl as text "U  = " as result min(U1,U2)
	bsqreg `var' i.strategy
}

*c3_fert_urea_valuec3_fert_kcl_valuec3_fert_tsp_valuec3_fert_za_valuec3_fert_manure_valuec3_fert_npk_valuec3_fert_phonska_valuec3_fert_convet_valuec3_fert_orgcm_valuec3_fert_orgmes_valuec3_fert_sp36_valuec3_fert_hnopb_valuec3_fert_totcost


*total cost of farm
univar mo_mio_f_cost_farm, by(strategy)
g mo_mio_f_cost_farm= f_cost_farm/(12*1000000)

ranksum `var', by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)




***************************
*****Seasonal cropping ****
***************************

*prices //finished
swilk c1_price_crop_1 c1_price_crop_2 c1_price_crop_3 c1_price_crop_4 c1_price_crop_8 
tabstat c1_price_crop_1 c1_price_crop_2 c1_price_crop_3 c1_price_crop_4 c1_price_crop_8, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal
cls
foreach var of varlist c1_price_crop_1 c1_price_crop_2 c1_price_crop_3 c1_price_crop_4 c1_price_crop_8 {	
	capture noisily ranksum `var', by(strategy) porder
	scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
	scalar define U2 = r(N_1)*r(N_2)-U1
	display _newline ///
	in smcl as text "U1 = " as result U1 _newline ///
	in smcl as text "U2 = " as result U2 _newline ///
	in smcl as text "U  = " as result min(U1,U2)
	qreg2 `var' i.strategy, wls(100)
} //



*yields
foreach var of varlist c1_yieldkg_crop_1 c1_yieldkg_crop_2 c1_yieldkg_crop_3 c1_yieldkg_crop_4 c1_yieldkg_crop_5 c1_yieldkg_crop_6 c1_yieldkg_crop_7 c1_yieldkg_crop_8 c1_yieldkg_crop_9 c1_yieldkg_crop_10 c1_yieldkg_crop_11 c1_yieldkg_crop_12 c1_yieldkg_crop_13 c1_yieldkg_crop_14 {
	g per1ha_`var'=`var'/b1_land_op_ha
	g per1ha_per12m_`var'=per1ha_`var'/12
}

tabstat per1ha_c1_yieldkg_crop_1 per1ha_c1_yieldkg_crop_2 per1ha_c1_yieldkg_crop_3 per1ha_c1_yieldkg_crop_4 per1ha_c1_yieldkg_crop_8, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal

swilk per1ha_per12m_c1_yieldkg_crop_1 per1ha_per12m_c1_yieldkg_crop_2 per1ha_per12m_c1_yieldkg_crop_3 per1ha_per12m_c1_yieldkg_crop_4 per1ha_per12m_c1_yieldkg_crop_8

cls
foreach var of varlist per1ha_c1_yieldkg_crop_1 per1ha_c1_yieldkg_crop_2 per1ha_c1_yieldkg_crop_3 per1ha_c1_yieldkg_crop_4 per1ha_c1_yieldkg_crop_8 {
	capture noisily median `var', by(strategy)
	capture noisily ranksum `var', by(strategy) porder
		scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
		scalar define U2 = r(N_1)*r(N_2)-U1
		display _newline ///
		in smcl as text "U1 = " as result U1 _newline ///
		in smcl as text "U2 = " as result U2 _newline ///
		in smcl as text "U  = " as result min(U1,U2)
	capture noisily qreg2 `var' i.strategy, wls(100)
}


*loop without inflated zeros 
cls
preserve 
	*
	foreach var of varlist per1ha_c1_yieldkg_crop_1 per1ha_c1_yieldkg_crop_2 per1ha_c1_yieldkg_crop_3 per1ha_c1_yieldkg_crop_4 per1ha_c1_yieldkg_crop_8 {
			replace `var'=. if `var'==0
		}
	*	
	tabstat per1ha_c1_yieldkg_crop_1 per1ha_c1_yieldkg_crop_2 per1ha_c1_yieldkg_crop_3 per1ha_c1_yieldkg_crop_4 per1ha_c1_yieldkg_crop_8, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal
	*
	foreach var of varlist per1ha_c1_yieldkg_crop_1 per1ha_c1_yieldkg_crop_2 per1ha_c1_yieldkg_crop_3 per1ha_c1_yieldkg_crop_4 per1ha_c1_yieldkg_crop_8 {
		capture noisily median `var', by(strategy)
		capture noisily ranksum `var', by(strategy) porder
			scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
			scalar define U2 = r(N_1)*r(N_2)-U1
			display _newline ///
			in smcl as text "U1 = " as result U1 _newline ///
			in smcl as text "U2 = " as result U2 _newline ///
			in smcl as text "U  = " as result min(U1,U2)
		capture noisily qreg2 `var' i.strategy, wls(100)
	}
	*
restore 


*revenue
swilk c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha 
tabstat c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha , by(strategy) nototal longstub statistics(n mean sd median iqr) column(statistics) missing





cls
foreach var of varlist ///
	c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha  {
	capture noisily median `var', by(strategy)
	capture noisily ranksum `var', by(strategy) porder
		scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
		scalar define U2 = r(N_1)*r(N_2)-U1
		display _newline ///
		in smcl as text "U1 = " as result U1 _newline ///
		in smcl as text "U2 = " as result U2 _newline ///
		in smcl as text "U  = " as result min(U1,U2)
	capture noisily qreg2 `var' i.strategy, wls(100)
}


*loop without inflated zeros 
cls
preserve 
	*
	foreach var of varlist c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha {
			replace `var'=. if `var'==0
		}
	*	
	tabstat c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha, by(strategy) statistics(n mean sd median iqr) longstub column(statistics) nototal
	*
	foreach var of varlist c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_4_miooo_ha  c1_revenue_crop_8_miooo_ha {
		capture noisily median `var', by(strategy)
		capture noisily ranksum `var', by(strategy) porder
			scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
			scalar define U2 = r(N_1)*r(N_2)-U1
			display _newline ///
			in smcl as text "U1 = " as result U1 _newline ///
			in smcl as text "U2 = " as result U2 _newline ///
			in smcl as text "U  = " as result min(U1,U2)
		capture noisily qreg2 `var' i.strategy, wls(100)
	}
	*
restore 



*yieldlist
*per1ha_per12m_c1_yieldkg_crop_1 per1ha_per12m_c1_yieldkg_crop_2 per1ha_per12m_c1_yieldkg_crop_3 per1ha_per12m_c1_yieldkg_crop_4 per1ha_per12m_c1_yieldkg_crop_5 per1ha_per12m_c1_yieldkg_crop_6 per1ha_per12m_c1_yieldkg_crop_7 per1ha_per12m_c1_yieldkg_crop_8 per1ha_per12m_c1_yieldkg_crop_9 per1ha_per12m_c1_yieldkg_crop_10 per1ha_per12m_c1_yieldkg_crop_11 per1ha_per12m_c1_yieldkg_crop_12 per1ha_per12m_c1_yieldkg_crop_13 per1ha_per12m_c1_yieldkg_crop_14

