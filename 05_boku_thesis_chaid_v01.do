

***********************************************
***	CHAID: Diversification Variables	*******
***********************************************


****
*variables of choice
*hh_edu hh_age h_depratio c2_lab_hlr h_edu_max 
label var hh_edu 				"Household-head education (years)"
label var hh_age 				"Household-head age (years)"
label var h_size				"Household size (n)"
label var h_depratio 			"Dependency ratio"
label var h_edu_max				"Highest level of education in household (years)"

*f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc
label var f_y_seasc_perc 		"Income: seasonal crops (%)"
label var f_y_perennc_perc 		"Income: perennial crops (%)"
label var f_y_ah_perc 			"Income: animal husbandry (%)"
label var f_y_fish_perc 		"Income: fisheries (%)"
label var f_y_offnonf_perc 		"Income: off- and non-farm activities (%)"
label var f_y_farm_perc 		"Income: total farm activities (%)"

*mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100
label var mio_f_y_seasc_100 	"Income: seasonal crops (10^6 IDR)"
label var mio_f_y_perennc_100 	"Income: perennial crops (10^6 IDR)"
label var mio_f_y_ah 			"Income: animal husbandry (10^6 IDR)"
label var mio_f_y_fish_100 		"Income: fisheries (10^6 IDR)"
label var mio_f_y_offnonf_100 	"Income: off- and non-farm activities (10^6 IDR)"
label var mio_f_y_farm_100 		"Income: total farm (10^6 IDR)"

*b1_land_op_ha tlu_tot 
label var b1_land_op_ha 		"Operated land (ha)"
label var tlu_tot 				"Livestock ownership (TLU)"

*y_div_inv crop_div_inv lvstck_div_inv
label var y_div_inv 			"Diversification of income (1/Simpson)"
label var crop_div_inv 			"Diversification of crops (1/Simpson)"
label var lvstck_div_inv 		"Diversification of livestock (1/Simpson)"
label var b_cd2_100				"Agroforestry share of cropping area (%)"

* cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_total 
label var cd_n_seas 			"Seasonal crops (n of types)"
label var cd_n_perenn 			"Perennial crops (n of types)"
label var cd_n_valuetrees 		"Value trees crops (n of types)"
label var cd_n_totperenn 		"Subtotal: perennial crops and value trees (n of types)"
label var cd_n_total 			"Total crops and value trees (n of types)"

*f_cost_fert_ha f_cost_pest_ha 
label var mio_f_cost_fert_ha 	"Input: fertilizer (10^6 IDR/ha)"
label var mio_f_cost_pest_ha 	"Input: pesticides (10^6 IDR/ha)"

*e1_creditinf e1_creditform k1_15_accmarket
label var e1_creditinf 			"Easy access to informal credit (yes=1)"
label var e1_creditform 		"Easy access to formal credit (yes=1)"
label var k1_15_accmarket 		"Market access (Likert 1-6, 1=best)"

g lhd_strat=""
label var lhd_strat "Livelihood strategy"
replace lhd_strat="Agroforestry" if b_cd2_100 >15
replace lhd_strat="Conventional" if b_cd2_100 <=15
encode lhd_strat, g(strategy)

***
save "1 Input\boku_ugm_combined_edit_003_01.dta", replace
use "1 Input\boku_ugm_combined_edit_003_01.dta", clear 


***********************************************
****	descriptives for CHAID			*******
***********************************************

tabstat hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket if village=="Leksana", statistics(mean sd q iqr min max var) columns(statistics) format(%9.3f)

*cluster kmedian hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket, name(t1) k(5)

tabstat hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket if village=="Penanggungan", statistics(mean sd q iqr min max var) columns(statistics) format(%9.3f)

tabstat hh_age hh_edu h_size h_depratio c2_lab_hlr h_edu_max f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc f_y_farm_perc mio_f_y_seasc_100 mio_f_y_perennc_100 mio_f_y_ah mio_f_y_fish_100 mio_f_y_offnonf_100 mio_f_y_farm_100 b1_land_op_ha tlu_tot y_div_inv crop_div_inv lvstck_div_inv b_cd2_100 cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_totperenn cd_n_total mio_f_cost_fert_ha mio_f_cost_pest_ha e1_creditinf e1_creditform k1_15_accmarket, statistics(mean sd q iqr min max var) columns(statistics) format(%9.3f)


tabstat mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_y_tot_100 mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_seed_ha, by(village) statistics(mean sd p50 iqr) columns(statistics) format(%9.3f) nototal lo

tabstat f_y_pc_mon_mio, by(village) statistics(mean sd p50 iqr) columns(statistics) format(%9.3f) nototal lo