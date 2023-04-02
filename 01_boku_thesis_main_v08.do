

clear all
snapshot erase _all

global PATH C:\Users\Reinhard Rebernig\OneDrive\2020 BOKU DI\01 Analysis
cd "$PATH"
 

*cap log close
*log using "5 Log\thesislog001.log", replace text





do "2 Code\02_boku_thesis_setup_v01"
save "1 Input\boku_ugm_combined_edit_002.dta", replace 

do "2 Code\03_boku_thesis_vars_v01"
save "1 Input\boku_ugm_combined_edit_003.dta", replace 


do "2 Code\04_boku_thesis_ind_v01"
save "1 Input\boku_ugm_combined_edit_004.dta", replace 


do "2 Code\05_boku_thesis_chaid_v01"
save "1 Input\boku_ugm_combined_edit_005.dta", replace 


do "2 Code\06_boku_thesis_descrchaid_v01"
save "1 Input\boku_ugm_combined_edit_006.dta", replace 


do "2 Code\07_boku_thesis_testing_v01"
save "1 Input\boku_ugm_combined_edit_007.dta", replace 
*/

use "1 Input\boku_ugm_combined_edit_007.dta", clear 



stop 
*do "2 Code\08_boku_thesis_graphing_v01"
*save "1 Input\boku_ugm_combined_edit_008.dta", replace 

do "2 Code\09_boku_thesis_regression_v01"
save "1 Input\boku_ugm_combined_edit_009.dta", replace 


use "1 Input\boku_ugm_combined_edit_008.dta", clear 




two ///
	scatter res2 mmx_f5_wellbeing_rev, yaxis(1)			///
||	lfit res2 mmx_f5_wellbeing_rev, yaxis(1)	 				///
||	scatter res2 mmx_f4_resilience, yaxis(1)	 				///
||	lfit res2 mmx_f4_resilience, yaxis(1) ytitle("Resilience score")	 				///
||	lfit mmx_f5_wellbeing_rev mmx_f4_resilience, yaxis(2)	 	///
	title("Comparing wellbeing and resilience") note("Agroforestry n=40, Conventional n=38") scheme(plotplainblind) ///
	legend(cols(1) position(6) order(1 "Q: Living conditions and wellbeing compared to neighbours?" 2 "Fitted values for wellbeing and calculated resilience" 3 "Q: Capability of household to withstand future hazard or shock?" 4 "Fitted values for stated and calculated resilience" 5 "Fitted values for stated wellbeing and resilience")) ///
	 ytitle("Inquired resilience and wellbeing fit", axis(2)) xtitle("Q:Wellbeing [1-6] | Q:Resilience [1-10]") xtick(0(.1)1)




ktau f_y_pc res2 res_cap_buffer res_cap_learnadapt res_cap_selforg immx_b_sc_index immx_s_cn_index, stats(taub p) print(0.1)


ktau f_y_pc res2 res_cap_buffer res_cap_learnadapt res_cap_selforg immx_b_sc_index immx_b_hc_index immx_s_cn_index, stats(taub p) print(0.1)

scatter f_y_pc_mon_mio immx_b_hc_index
scatter f_y_pc_mon_mio immx_b_sc_index

ktau f_y_pc_mon_mio immx_b_sc_index
spearman f_y_pc_mon_mio immx_b_sc_index
pwcorr f_y_pc_mon_mio immx_b_sc_index, sig 

ktau f_y_pc_mon_mio immx_b_sc_index immx_b_pc_index immx_s_opp_index, stats(taub p) print(0.1) 
0.1834	0.0188 


drop socialc_total
egen socialc_total= rowmean(immx_b_sc_index mmx_io_s_opp immx_s_cn_index)


cls
*household
ktau mo_mio_f_y_tot_100 immx_b_hc_index socialc_total immx_b_sc_index  immx_s_opp_index immx_s_cn_index, stats(taub p) print(0.1)


*per capita
ktau f_y_pc_mon_mio immx_b_hc_index socialc_total immx_b_sc_index  immx_s_opp_index immx_s_cn_index, stats(taub p) print(0.1)


ktau mo_mio_f_y_tot_100 k1_15_accmarket k1_19_roadsdam, stats(taub p) print(0.1)

g y_farm_mo_mio_pha = mo_mio_f_y_farm_100/b1_land_op_ha


mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_y_tot_100 y_farm_mo_mio_pha

g y_seasandper_mo_mio_pha = (mo_mio_f_y_perennc_100 + mo_mio_f_y_seasc_100)/b1_land_op_ha


graph two ///
	scatter y_seasandper_mo_mio_pha bc_ratio_seasc_perennc if strategy==1 ///
||	scatter y_seasandper_mo_mio_pha bc_ratio_seasc_perennc if strategy==2, ///
	scheme(plotplainblind) ytitle("Monthly income Mio IDR/ha") xtitle("Benefit-Cost Ratio (seasonal & perennial crops)") ///
	legend(order(1 "Agroforestry" 2 "Conventional"))
	
graph two ///
	scatter y_seasandper_mo_mio_pha mo_crop_gm_mio if strategy==1, ms mcol(green) msize(vsmall) ///
||	scatter y_seasandper_mo_mio_pha mo_crop_gm_mio if strategy==2, mcol(orange) msize(vsmall) ///
	scheme(plotplainblind) ytitle("Monthly income Mio IDR/ha") xtitle("Crop gross margin (seasonal & perennial crops)") ///
	legend(order(1 "Agroforestry" 2 "Conventional"))	


*correlations of resilience with 
ktau res2 res_cap_buffer res_cap_learnadapt res_cap_selforg y_seasandper_mo_mio_pha mo_crop_gm_mio bc_ratio_seasc_perennc y_farm_mo_mio_pha, stats(taub p) print(0.1)

ktau res2 bc_ratio_seasc bc_ratio_farm bc_ratio_perennc bc_ratio_ah bc_ratio_fish bc_ratio_seasc_perennc, stats(taub p) print(0.1) star(0.05)

*
graph two ///
	scatter bc_ratio_seasc_perennc res2 if strategy==1, mcol(green) msize(small) ///
||	qfit bc_ratio_seasc_perennc res2 if strategy==1, lc(green) ///
||	scatter bc_ratio_seasc_perennc res2 if strategy==2, mcol(orange) msize(small) ///
||	qfit bc_ratio_seasc_perennc res2 if strategy==2, lc(orange) lp(dash_dot) ///
||	lfitci bc_ratio_seasc_perennc res2, lc(grey) ///
	scheme(plotplainblind) ytitle("Monthly income Mio IDR/ha") xtitle("Resilience index [0-1]") ///
	legend(order(1 "Agroforestry" 2 "Agroforestry (fitted)" 3 "Conventional" 4 "Conventional (fitted)" 5 "Sample (fitted)"))	

ttest bc_ratio_seasc_perennc, by(strategy) welch une

graph two ///
	lfit bc_ratio_seasc_perennc res2, lc(grey) lp(dash)  ///
||	scatter bc_ratio_seasc_perennc res2 if strategy==1, mcol(green) msize(small) ///
||	scatter bc_ratio_seasc_perennc res2 if strategy==2, mcol(orange) msize(small) ///
	scheme(plotplainblind) ytitle("Benefit-Cost Ratio (seasonal & perennial crops)") xtitle("Resilience index [0-1]") ///
	legend(order(1 "Sample(fitted)" 2 "Agroforestry" 3 "Conventional")) note("n=78, Spearman's {&rho}=0.327, p<0.01") ///
	xlabel(0.3(0.1)0.75) xscale(range(0.3 0.75)) xline(0.3(0.05)0.75, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	ylabel(0(10)30) yscale(range(0 30)) yline(0(5)30, lstyle(grid) lcolor(gs10) lwidth(thin))

spearman bc_ratio_seasc_perennc res2
	
tabstat hh_age hh_edu mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_farm_100 mo_mio_f_y_tot_100, by(strategy) statistics(mean) column(statistics) longstub


ktau immx_c_rsl_index immx_b_sc_index
