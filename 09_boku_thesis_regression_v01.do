
/*
f_rev_seasc		double	%10.0g	Gross	revenue of seasonal crops (IDR)
f_rev_perennc	long	%10.0g	Gross	revenue of annual crops (IDR)
f_rev_ah		long	%10.0g	Gross	revenue of animal husbandry (IDR)
f_rev_fish		long	%10.0g	Gross	revenue of fisheries (IDR)
f_rev_farm		double	%10.0g	Total	gross revenue of farm (IDR)
*/


*correlations between input and income
ktau mo_mio_f_cost_seed_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_fert_ha f_rev_seasc f_rev_perennc, stats(taub p) print(0.1) star(0.01)

graph box mo_crop_gm_mio, ///
 by(strategy,compact title("Profitability: Gross crop margin") note("Gross crop margin: (Seas. + perenn. revenue - costs) / operated area, Boxplots exclude major outliers" "Agroforestry n=40, Conventional n=38" )) scheme(plottigblind) ///
	legend(off) noout ///
	ytitle("Mio IDR/month") ///
	ylabel(-10(5)30) yscale(range(-10 30)) yline(-10(2.5)30, lstyle(grid) lcolor(white) lwidth(thin)) intensity(50)

graph box bc_ratio_seasc bc_ratio_seasc_perennc, ///
 by(strategy,compact title("Benefit cost ratios") note("Boxplots exclude major outliers" "Agroforestry n=40, Conventional n=38" )) scheme(plottigblind) ///
	legend(order( 1 "Seasonal crops" 2 "Seasonal and perennial crops") rows(1) position(6))  ///
	ytitle("BCR") noout ///
	ylabel(-1 0(5)15) yscale(range(-1 15)) yline(-1(1)15, lstyle(grid) lcolor(white) lwidth(thin)) intensity(50)



*outlier detection 
bacon f_y_seasc f_y_perennc f_y_ah f_y_farm f_y_offnonf f_y_tot tlu_tot b1_land_op_ha f_cost_seasc f_cost_perennc f_cost_ah f_cost_fert f_cost_pest, gen(out1) //individual costs for income
bacon f_y_seasc f_y_perennc f_y_ah f_y_farm f_y_offnonf f_y_tot tlu_tot b1_land_op_ha f_cost_lab f_cost_seed f_cost_fert f_cost_pest, generate(out2) //individual costs
bacon f_y_seasc f_y_perennc f_y_ah f_y_farm f_y_offnonf f_y_tot tlu_tot b1_land_op_ha f_cost_fert_ha f_cost_pest_ha f_cost_lab_ha f_cost_seed_ha, generate(out3) //per hectare
bacon f_y_seasc f_y_perennc f_y_ah f_y_farm f_y_offnonf f_y_tot tlu_tot b1_land_op_ha f_cost_lab f_cost_seed f_cost_fert f_cost_pest, generate(out4) // most valid probs
bacon f_y_seasc f_y_perennc f_y_ah f_y_farm f_y_offnonf f_y_tot tlu_tot b1_land_op_ha f_cost_lab f_cost_seed f_cost_fert f_cost_pest crop_gm  bc_ratio_seasc_perennc, generate(out5) // most valid probs


local regvar f_y_tot c1_yieldkg_crop_1 c1_yieldkg_crop_2 c1_yieldkg_crop_3 c1_yieldkg_crop_4 c1_yieldkg_crop_5 c1_yieldkg_crop_6 c1_yieldkg_crop_7 c1_yieldkg_crop_8 c1_yieldkg_crop_9 c1_yieldkg_crop_10 c1_yieldkg_crop_11 c1_yieldkg_crop_12 c1_yieldkg_crop_13 c1_yieldkg_crop_14 hh_age hh_edu h_depratio h_edu_max b_cd2_100 y_div_inv crop_div_inv lvstck_div_inv f_cost_lab f_cost_seed h_size tlu_tot b1_land_op f_cost_fert f_cost_pest
foreach var of local regvar {
	gen 	reg_`var' = `var'
	replace reg_`var' = 0 if reg_`var'==. 
}

*vselect gvselect gsreg

vselect f_y_tot ///
	c1_yieldkg_crop_1 c1_yieldkg_crop_2 c1_yieldkg_crop_3 c1_yieldkg_crop_4 c1_yieldkg_crop_8  /// yields
	hh_age hh_edu h_depratio h_edu_max /// hh chars
	b_cd2_100 /// assets
	y_div_inv crop_div_inv lvstck_div_inv  /// diversity
	f_cost_lab f_cost_seed  ///
	if out5==0, best fix(h_size i.vill tlu_tot b1_land_op f_cost_fert f_cost_pest)
	
/*best varlist best at bottom
4  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv
5  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv h_depratio
6  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv h_depratio c1_yieldkg_crop_2
7  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv h_depratio y_div_inv c1_yieldkg_crop_1
8  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv h_depratio y_div_inv c1_yieldkg_crop_1 c1_yieldkg_crop_2
9  :  c1_yieldkg_crop_4 f_cost_seed c1_yieldkg_crop_3 crop_div_inv h_depratio y_div_inv c1_yieldkg_crop_1 hh_age c1_yieldkg_crop_2
*/

/*	
gsreg f_y_tot ///
	c1_yieldkg_crop_1 c1_yieldkg_crop_2 c1_yieldkg_crop_3 c1_yieldkg_crop_4 c1_yieldkg_crop_8  /// yields
	hh_age hh_edu h_depratio h_edu_max /// hh chars
	b_cd2_100 /// assets
	y_div_inv crop_div_inv lvstck_div_inv  /// diversity
	f_cost_lab f_cost_seed  ///
	,fixvar(h_size vill tlu_tot b1_land_op f_cost_fert f_cost_pest) ncomb(1, 12) replace hettest
	
gvselect  varlist, nmodels(#): est_cmd
out2 all income sources tlu_tot b1_land_op_ha all inputs
*/


*c1_yieldkg_crop_1 c1_yieldkg_crop_2 c1_yieldkg_crop_3 c1_yieldkg_crop_4 c1_yieldkg_crop_5 c1_yieldkg_crop_6 c1_yieldkg_crop_7 c1_yieldkg_crop_8 c1_yieldkg_crop_9 c1_yieldkg_crop_10 c1_yieldkg_crop_11 c1_yieldkg_crop_12 c1_yieldkg_crop_13 c1_yieldkg_crop_14 //yieldlist
*c1_price_crop_1 c1_price_crop_2 c1_price_crop_3 c1_price_crop_4 c1_price_crop_5 c1_price_crop_6 c1_price_crop_7 c1_price_crop_8 c1_price_crop_9 c1_price_crop_10 c1_price_crop_11 c1_price_crop_12 c1_price_crop_13 c1_price_crop_14 //pricelist

*perennc physical units of production
*c4_q1_percrop_1 c4_q1_percrop_2 c4_q1_percrop_3 c4_q1_percrop_4 c4_q1_percrop_5 c4_q1_percrop_6 c4_q1_percrop_7 c4_q1_percrop_8 c4_q1_percrop_9 c4_q1_percrop_10 c4_q1_percrop_11 c4_q1_percrop_12 c4_q1_percrop_13 c4_q1_percrop_14 c4_q1_percrop_15 c4_q1_percrop_16 c4_q1_percrop_17 c4_q1_percrop_18 c4_q1_percrop_19
*value of production
*c4_q2_percrop_1 c4_q2_percrop_2 c4_q2_percrop_3 c4_q2_percrop_4 c4_q2_percrop_5 c4_q2_percrop_6 c4_q2_percrop_7 c4_q2_percrop_8 c4_q2_percrop_9 c4_q2_percrop_10 c4_q2_percrop_11 c4_q2_percrop_12 c4_q2_percrop_13 c4_q2_percrop_14 c4_q2_percrop_15 c4_q2_percrop_16 c4_q2_percrop_17 c4_q2_percrop_18 c4_q2_percrop_19 

*revenue
1 2 3 4 8

*yield
1 2 3 4 8


*revenue seasonal crops
egen f_rev_seasc_tot= rowtotal(c1_revenue_crop_1 c1_revenue_crop_2 c1_revenue_crop_3 c1_revenue_crop_4 c1_revenue_crop_5 c1_revenue_crop_6 c1_revenue_crop_7 c1_revenue_crop_8 c1_revenue_crop_9 c1_revenue_crop_10 c1_revenue_crop_11 c1_revenue_crop_12 c1_revenue_crop_13 c1_revenue_crop_14)


foreach var of varlist c1_revenue_crop_1 c1_revenue_crop_2 c1_revenue_crop_3 c1_revenue_crop_4 c1_revenue_crop_5 c1_revenue_crop_6 c1_revenue_crop_7 c1_revenue_crop_8 c1_revenue_crop_9 c1_revenue_crop_10 c1_revenue_crop_11 c1_revenue_crop_12 c1_revenue_crop_13 c1_revenue_crop_14 {
	g `var'_perc_og = `var'/f_rev_seasc_tot
}

*relative
graph bar (mean) c1_revenue_crop_1_perc_og c1_revenue_crop_2_perc_og c1_revenue_crop_3_perc_og c1_revenue_crop_4_perc_og c1_revenue_crop_5_perc_og c1_revenue_crop_6_perc_og c1_revenue_crop_7_perc_og c1_revenue_crop_8_perc_og c1_revenue_crop_9_perc_og c1_revenue_crop_10_perc_og c1_revenue_crop_11_perc_og c1_revenue_crop_12_perc_og c1_revenue_crop_13_perc_og c1_revenue_crop_14_perc_og, by(strategy) stack //
*Agroforestry 1 2 3 4 10
*Conventional 2 3 4 8 9  

tabstat c1_revenue_crop_1_perc_og c1_revenue_crop_2_perc_og c1_revenue_crop_3_perc_og c1_revenue_crop_4_perc_og c1_revenue_crop_5_perc_og c1_revenue_crop_6_perc_og c1_revenue_crop_7_perc_og c1_revenue_crop_8_perc_og c1_revenue_crop_9_perc_og c1_revenue_crop_10_perc_og c1_revenue_crop_11_perc_og c1_revenue_crop_12_perc_og c1_revenue_crop_13_perc_og c1_revenue_crop_14_perc_og, by(strategy) statistics(mean p50) column(statistics) nototal longstub

//1 2 3 

*absolute
foreach var of varlist c1_revenue_crop_1 c1_revenue_crop_2 c1_revenue_crop_3 c1_revenue_crop_4 c1_revenue_crop_5 c1_revenue_crop_6 c1_revenue_crop_7 c1_revenue_crop_8 c1_revenue_crop_9 c1_revenue_crop_10 c1_revenue_crop_11 c1_revenue_crop_12 c1_revenue_crop_13 c1_revenue_crop_14 {
	g `var'_miooo = `var'/1000000
}

foreach var of varlist c1_revenue_crop_1_miooo c1_revenue_crop_2_miooo c1_revenue_crop_3_miooo c1_revenue_crop_4_miooo c1_revenue_crop_5_miooo c1_revenue_crop_6_miooo c1_revenue_crop_7_miooo c1_revenue_crop_8_miooo c1_revenue_crop_9_miooo c1_revenue_crop_10_miooo c1_revenue_crop_11_miooo c1_revenue_crop_12_miooo c1_revenue_crop_13_miooo c1_revenue_crop_14_miooo {
	g `var'_ha = `var'/b1_land_op_ha
}

graph box c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha c1_revenue_crop_5_miooo_ha c1_revenue_crop_6_miooo_ha c1_revenue_crop_7_miooo_ha c1_revenue_crop_8_miooo_ha c1_revenue_crop_9_miooo_ha c1_revenue_crop_10_miooo_ha c1_revenue_crop_11_miooo_ha c1_revenue_crop_12_miooo_ha c1_revenue_crop_13_miooo_ha c1_revenue_crop_14_miooo_ha, by(strategy) noout
*Agroforestry 1 2 3 6 (maybe 4 6 and 10)
*conventional 2 3 8 7 (maybe 7 and 9)


*final for seasonal crops 
graph box  ///
	c1_revenue_crop_1_miooo_ha c1_revenue_crop_2_miooo_ha c1_revenue_crop_3_miooo_ha  c1_revenue_crop_8_miooo_ha, ///
	by(strategy,compact title("Revenue from major seasonal crops") note("Boxplots exclude major outliers" "Agroforestry n=40, Conventional n=38" )) scheme(plottigblind) ///
	legend(order( 1 "Maize" 2 "Cabbage" 3 "Chilli" 4 "Carrot") rows(1) position(6))  ///
	ytitle("Mio IDR / ha*year") noout  intensity(20) ///
	ylabel(0(5)30) yscale(range(0 30))

graph box  ///
	c1_revenue_crop_4_miooo_ha, ///
	by(strategy,compact title("Revenue from potatos") note("Boxplots exclude major outliers" "Agroforestry n=40, Conventional n=38" )) scheme(plottigblind) ///
	legend(off)  ///
	ytitle("Mio IDR / ha*year")  intensity(20) ///
	ylabel(0(100)700) yscale(range(0 700)) 

des c1_revenue_crop_1 c1_revenue_crop_2 c1_revenue_crop_3  c1_revenue_crop_8 c1_revenue_crop_10
	

	
	
	
	
*perennial crops
egen f_rev_perennc_tot_2=rowtotal(c4_q6_percrop_1 c4_q6_percrop_2 c4_q6_percrop_3 c4_q6_percrop_4 c4_q6_percrop_5 c4_q6_percrop_6 c4_q6_percrop_7 c4_q6_percrop_8 c4_q6_percrop_9 c4_q6_percrop_10 c4_q6_percrop_11 c4_q6_percrop_12 c4_q6_percrop_13 c4_q6_percrop_14 c4_q6_percrop_15 c4_q6_percrop_16 c4_q6_percrop_17 c4_q6_percrop_18 c4_q6_percrop_19)
replace f_rev_perennc_tot_2=0 if f_rev_perennc_tot_2==. 

foreach var of varlist c4_q6_percrop_1 c4_q6_percrop_2 c4_q6_percrop_3 c4_q6_percrop_4 c4_q6_percrop_5 c4_q6_percrop_6 c4_q6_percrop_7 c4_q6_percrop_8 c4_q6_percrop_9 c4_q6_percrop_10 c4_q6_percrop_11 c4_q6_percrop_12 c4_q6_percrop_13 c4_q6_percrop_14 c4_q6_percrop_15 c4_q6_percrop_16 c4_q6_percrop_17 c4_q6_percrop_18 c4_q6_percrop_19 {
	g `var'_perc_og2 = `var'
	replace `var'_perc_og2=0 if `var'==. 
	replace `var'_perc_og2= `var'_perc_og2/f_rev_perennc_tot_2
}

*relative
graph bar (mean) c4_q6_percrop_1_perc_og c4_q6_percrop_2_perc_og c4_q6_percrop_3_perc_og c4_q6_percrop_4_perc_og c4_q6_percrop_5_perc_og c4_q6_percrop_6_perc_og c4_q6_percrop_7_perc_og c4_q6_percrop_8_perc_og c4_q6_percrop_9_perc_og c4_q6_percrop_10_perc_og c4_q6_percrop_11_perc_og c4_q6_percrop_12_perc_og c4_q6_percrop_13_perc_og c4_q6_percrop_14_perc_og c4_q6_percrop_15_perc_og c4_q6_percrop_16_perc_og c4_q6_percrop_17_perc_og c4_q6_percrop_18_perc_og c4_q6_percrop_19_perc_og, by(strategy) stack percent
*Agroforestry 1 2 3 4 5 6
*Conventional 6 4 7 8 1

graph bar (mean) c4_q6_percrop_1_perc_og c4_q6_percrop_2_perc_og c4_q6_percrop_3_perc_og c4_q6_percrop_4_perc_og c4_q6_percrop_5_perc_og c4_q6_percrop_6_perc_og c4_q6_percrop_7_perc_og c4_q6_percrop_8_perc_og c4_q6_percrop_9_perc_og c4_q6_percrop_10_perc_og c4_q6_percrop_11_perc_og c4_q6_percrop_12_perc_og c4_q6_percrop_13_perc_og c4_q6_percrop_14_perc_og c4_q6_percrop_15_perc_og c4_q6_percrop_16_perc_og c4_q6_percrop_17_perc_og c4_q6_percrop_18_perc_og c4_q6_percrop_19_perc_og, by(strategy) stack percent

tabstat c4_q6_percrop_1_perc_og2 c4_q6_percrop_2_perc_og2 c4_q6_percrop_3_perc_og2 c4_q6_percrop_4_perc_og2 c4_q6_percrop_5_perc_og2 c4_q6_percrop_6_perc_og2 c4_q6_percrop_7_perc_og2 c4_q6_percrop_8_perc_og2 c4_q6_percrop_9_perc_og2 c4_q6_percrop_10_perc_og2 c4_q6_percrop_11_perc_og2 c4_q6_percrop_12_perc_og2 c4_q6_percrop_13_perc_og2 c4_q6_percrop_14_perc_og2 c4_q6_percrop_15_perc_og2 c4_q6_percrop_16_perc_og2 c4_q6_percrop_17_perc_og2 c4_q6_percrop_18_perc_og2 c4_q6_percrop_19_perc_og2, by(strategy) statistics(mean p50) column(statistics) nototal longstub

foreach var of varlist c4_q6_percrop_1 c4_q6_percrop_2 c4_q6_percrop_3 c4_q6_percrop_4 c4_q6_percrop_5 c4_q6_percrop_6  c4_q6_percrop_8 c4_q6_percrop_9 c4_q6_percrop_14 c4_q6_percrop_15 {
	g `var'_miooop = `var'/1000000 
	g `var'_miooop_ha = `var'_miooop/b1_land_op_ha
	replace `var'_miooop_ha=0 if `var'_miooop_ha==. 
}

/*
c4_q6_percrop_1	long	%10.0g	Gross	revenue	(Rp)	Tanaman SENGON
c4_q6_percrop_2	long	%10.0g	Gross	revenue	(Rp)	Tanaman KALIANDRA
c4_q6_percrop_3	long	%10.0g	Gross	revenue	(Rp)	BANANA
c4_q6_percrop_4	long	%10.0g	Gross	revenue	(Rp)	BAMBOO
c4_q6_percrop_5	long	%10.0g	Gross	revenue	(Rp)	COFFEE
c4_q6_percrop_6	long	%10.0g	Gross	revenue	(Rp)	Tanaman KALBI
c4_q6_percrop_8	long	%10.0g	Gross	revenue	(Rp)	Tanaman PUSPA
c4_q6_percrop_9	long	%10.0g	Gross	revenue	(Rp)	TEA
c4_q6_percro~14	long	%10.0g	Gross	revenue	(Rp)	GUAVA
c4_q6_percro~15	long	%10.0g	Gross	revenue	(Rp)	SNAKE FRUIT
*/



graph box ///
	 c4_q6_percrop_1_miooop_ha c4_q6_percrop_2_miooop_ha c4_q6_percrop_3_miooop_ha c4_q6_percrop_4_miooop_ha c4_q6_percrop_5_miooop_ha c4_q6_percrop_6_miooop_ha c4_q6_percrop_9_miooop_ha c4_q6_percrop_14_miooop_ha  ///
	 if c4_q6_percrop_6_miooop_ha<10 & c4_q6_percrop_1_miooop_ha<10 & c4_q6_percrop_5_miooop_ha<10, ///
	 by(strategy, compact title("Revenue from perennial crops") note("Boxplots exclude major outliers of Tanaman Sengon, Coffee and Tanaman Kalbi""Agroforestry n=40, Conventional n=38")) ///
	 legend(order(1 "Tanaman Sengon" 2 "Tanaman Kaliandra" 3 "Banana" 4 "Bamboo" 5 "Coffee" 6 "Tanaman Kalbi" 7 "Tea" 8 "Guava" ) rows(2) position(6)) scheme(plottigblind) intensity(70) ///
	ytitle("Mio IDR / ha*year") ///
	ylabel(0(2)9) yscale(range(0 9)) ///
	yline(0(1)9, lstyle(grid) lcol(white) lwidth(thin))
	
tabstat c4_q6_percrop_1_miooop_ha c4_q6_percrop_2_miooop_ha c4_q6_percrop_3_miooop_ha c4_q6_percrop_4_miooop_ha c4_q6_percrop_5_miooop_ha c4_q6_percrop_6_miooop_ha c4_q6_percrop_9_miooop_ha c4_q6_percrop_14_miooop_ha ///
	,by(strategy) statistics(n mean sd median iqr) longstub nototal column(statistics)

foreach var of varlist sum_c4_q2_percrop_1 sum_c4_q2_percrop_2 sum_c4_q2_percrop_3 sum_c4_q2_percrop_4 sum_c4_q2_percrop_5 sum_c4_q2_percrop_6 sum_c4_q2_percrop_7 sum_c4_q2_percrop_8 sum_c4_q2_percrop_9 sum_c4_q2_percrop_10 sum_c4_q2_percrop_11 sum_c4_q2_percrop_12 sum_c4_q2_percrop_13 sum_c4_q2_percrop_14 sum_c4_q2_percrop_15 sum_c4_q2_percrop_16 sum_c4_q2_percrop_17 sum_c4_q2_percrop_18 sum_c4_q2_percrop_19 {
    g new_`var'=(`var'/1000000)/b1_land_op_ha
}

	

tabstat new_sum_c4_q2_percrop_1 new_sum_c4_q2_percrop_2 new_sum_c4_q2_percrop_3 new_sum_c4_q2_percrop_4 new_sum_c4_q2_percrop_5 new_sum_c4_q2_percrop_6 new_sum_c4_q2_percrop_9 new_sum_c4_q2_percrop_14 ///
	,by(strategy) statistics(n mean sd median iqr) longstub nototal column(statistics)

	 
ktau ///
c4_q6_percrop_1_miooop_ha /// 1
c4_q6_percrop_2_miooop_ha /// 2
c4_q6_percrop_3_miooop_ha /// 3 explains bc seasc Banana (does not make sense)
c4_q6_percrop_4_miooop_ha /// 4
c4_q6_percrop_5_miooop_ha /// 5
c4_q6_percrop_6_miooop_ha /// 6 explains bc seas perennc and crop gm (Tanaman kalbi makes sense)
c4_q6_percrop_9_miooop_ha /// 7
c4_q6_percrop_14_miooop_ha /// 8
c1_revenue_crop_1_miooo_ha /// 9 explains crop gm (maize)
c1_revenue_crop_2_miooo_ha /// 10
c1_revenue_crop_3_miooo_ha /// 11
c1_revenue_crop_4_miooo_ha /// 12 explains crop gm best and bc seasc a bit (potato)
c1_revenue_crop_8_miooo_ha /// 13
c1_revenue_crop_10_miooo_ha /// 14
bc_ratio_seasc bc_ratio_seasc_perennc mo_crop_gm_mio, stats(taub p) print(0.1) star(0.01)

/*
cont here
graph box c4_q6_percrop_2 c4_q6_percrop_3 c4_q6_percrop_4 c4_q6_percrop_5 c4_q6_percrop_6 c4_q6_percrop_7 c4_q6_percrop_8 c4_q6_percrop_9 c4_q6_percrop_10 c4_q6_percrop_11 c4_q6_percrop_12 c4_q6_percrop_13 c4_q6_percrop_14 c4_q6_percrop_15 c4_q6_percrop_16 c4_q6_percrop_17 c4_q6_percrop_18 c4_q6_percrop_19

*sengon (Albizia chinensis) coffee (arabica and robusta) kalbi (Paraserianthes falcataria (L.) Nielsen)
c4_q6_percrop_1
c4_q6_percrop_5
c4_q6_percrop_6
*physical quantitites
c4_q1_percrop_1 c4_q1_percrop_5 c4_q1_percrop_6

*/

*********************************
*** Regressions				*****
*********************************

*household income
reg f_y_tot ///
	hh_age hh_edu h_size h_depratio h_edu_max //hh chars
	tlu_tot b1_land_op_ha b_cd2_100 //assets
	y_div_inv crop_div_inv lvstck_div_inv  /// diversity
	ln_f_cost_fert_mon_mio ln_f_cost_pest_mon_mio i.irrig_sys f6_wa_irr f6_wa_hh  work_occ_n_tot_main work_occ_n_tot_side  if out1==0, vce(cluster vill)




reg f_y_pc_mon_mio b_cd2_100 h_size c2_lab_hlr tlu_tot b1_land_op_ha  mo_mio_f_cost_seed_ha mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha c2_lab_cost 4.irrig_sys f6_wa_irr f6_wa_hh if out1==0, vce(cluster vill) 

* side occupation fertilizer input pesticides input 

su ln_f_y_pc_mon_mio, d (-2.)
su f_y_pc_mon_mio, d