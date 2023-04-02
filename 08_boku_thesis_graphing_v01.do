

***********************************************
***	graphing				*******
***********************************************


twoway ///
	scatter f_y_farm f_cost_tot
||	qfit 


*absolute income of villages
tempfile g1 g2
***
graph twoway ///
	hist mo_mio_f_y_tot_100, scheme(plotplainblind) title("Distribution") /// histogram
|| 	kdensity mo_mio_f_y_tot_100 if village=="Leksana", lc(plg2*0.7) lp(dash) lw(medthick)  /// Leksana		
|| 	kdensity mo_mio_f_y_tot_100 if village=="Penanggungan", lp(solid) lc(plr1*0.7) lw(medthick) /// Penanggungan
	legend(rows(2) label(1 "Sample") label(2 "Leksana") label(3 "Penanggungan") position(6)) ///
	saving(`g1') nodraw ytitle("Density") 
***
graph hbox mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_offnonf_100 if mo_mio_f_y_seasc_100<20, ///
	by(village, total compact cols(1) note("") title("Sources")) legend(rows(2) label(1 "Seasonal crops") label(2 "Perennial crops") label(3 "Animal husbandry") label(4 "Off-farm activities") position(6)) ///
	scheme(plotplainblind) asyvars ///
	ylabel(-6(1)20) yscale(range(-6 20)) yline(-6(0.5)20, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	subtitle(, position(8) ring(0) nobexpand bcolor(none) placement(sw)) note("") ///
	saving(`g2') nodraw note("") intensity(70)
***
graph combine "`g1'" "`g2'", scheme(plotplainblind) title("Distribution of monthly household income in sample") altshrink xcomm b2title("Monthly household income [Mio IDR]") note("Leksana n=38, Penanggungan n=40" "Boxplots exclude major outliers in Pengunggan at 22.72, 26.12 and 51.87")
***



* Pen Parade per Area for total annual consumption 
bysort village: cumul f_y_pc_mon_mio, gen(cdf_pc) 
lab var cdf_pc "Cumulative population" 
sort village cdf_pc

* Pen Parade with per capita  incomes
tempfile g1 g2 
graph twoway  /// 
	connect f_y_pc_mon_mio cdf_pc if vill == 1, msize(vsmall) scheme(plotplainblind)  /// 
||	connect f_y_pc_mon_mio cdf_pc if vill == 2, msize(vsmall) ///
||	scatteri 2.692 0 2.692 1, c(l) m(i) lc(black*0.7) lp(longdash) lw(thin) /// mean wage dec 2018
||	scatteri 1.689 0 1.689 1, c(l) m(i) lc(black*0.7) lp(shortdash_dot) lw(thin) /// mean minimum wage 2017
	ytick(-2(1)15) yscale(range(-2 14)) xlab(0(0.1)1) xtick(0(0.025)1) ///
	title("Population share below regional wages") yla(, ang(h)) ///
	legend(rows(4) size(3) order(1 "Observations in Leksana n=38" 2 "Observations in Penanggungan n=40" 3 "Avg. wage, Central Java 2018 (2.692 Mio IDR/m)" 4 "Avg. min. wage, Central Java 2017 (1.689 Mio IDR/m)") position(6)) ///
	ytitle("Mio IDR/capita*month" " ") xtitle("Cumulative Population") ///
	saving(`g1') nodraw
	
*text(3 0.172 "Mean wage Dec 2018 (Central Java)", size(vsmall)) ///
*text(2 0.195 "Mean minimum wage 2017 (Central Java)", size(vsmall)) ///

* Pen Parade with poverty lines
graph twoway  /// 
	connect f_y_pc_mon_mio cdf_pc if vill == 1 & cdf_pc<0.4, msize(vsmall) /// 
||	connect f_y_pc_mon_mio cdf_pc if vill == 2 & cdf_pc<0.4, msize(vsmall) ///
||	scatteri 0.32154 0 0.32154 0.4, c(l) m(i) lc(black*0.7) lp(shortdash_dot) lw(thin)  /// inter
||	scatteri 0.39215 0 0.39215 0.4, c(l) m(i) lc(black*0.7) lp(longdash) lw(thin)   /// nat rur
	title("Population share (0-40%) below poverty lines") ///
	legend(rows(4) size(3) order(1 "Observations in Leksana n=38" 2 "Observations in Penanggungan n=40" 3 "International poverty line (USD 1.90/d - 2011 PPP)" 4 "National Rural Poverty Line (IDR 392,154/m)") position(6))  ///
	scheme(plotplainblind) xtitle("Cumulative Population")  ///
	ytick(-1.25(0.25)1.25) yscale(range(-1.25 1.25)) yla(, ang(h)) xtick(0(0.025)0.4) ///
	saving(`g2') nodraw
	
graph combine "`g1'" "`g2'", scheme(plotplainblind) title("Monthly per capita income comparison") altshrink 

*Getting numbers
tab cdf_pc village if f_y_pc_mon_mio<1.689
tab cdf_pc village if f_y_pc_mon_mio<2.692
tab cdf_pc village if f_y_pc_mon_mio<0.392154


tempfile g1 g2 g3
*ha and tlu of villages
graph box ///
	b1_land_op_ha tlu_tot, ///
	by(village, compact rows(1) title("Farm assets") note("")) ///
	ylabel(0(1)5) yline(-0(0.5)5, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	scheme(plotplainblind)  ytitle("Scale [ha] | [TLU]") legend(rows(1)) nodraw saving(`g1') intensity(50)
	
*diversification by village
graph box ///
	y_div_inv crop_div_inv lvstck_div_inv, ///
	by(village, compact title("Diversification of income, crops and livestock") note("")) ///
	ylabel(0(1)5) yline(-0(0.5)5, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	scheme(plotplainblind)  ytitle("Scale (1/Simpson diversity index)") legend(rows(3) position(6))	nodraw saving(`g2') intensity(50)
	
*agroforestry
graph box ///
	b_cd2_100, ///
	by(village, compact title("Agroforestry share of operated land (%)") note("")) ///
	ylabel(0(10)100) yline(0(5)100, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	scheme(plotplainblind)  ytitle("percent") legend(off) nodraw saving(`g3') intensity(50)
	
graph combine "`g1'" "`g2'" "`g3'", scheme(plotplainblind) xcomm title("Assets and diversification") altshrink rows(1) note("Leksana n=38, Penanggungan n=40")

	
*input analysis
graph box ///
	mo_mio_f_cost_seed_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_fert_ha, ///
	ylabel(0(1)3) yscale(range(0 3)) yline(0(0.25)3, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	by(village, compact rows(1) note("Leksana n=38, Penanggungan n=40; boxplots exclude major outliers") title("Farm inputs")) noout scheme(plotplainblind) note("") ytitle("Mio IDR/month*ha") legend(rows(1) order(1 "Seed input" 2 "Labour cost" 3 "Pesticide input" 4 "Fertilizer input")) intensity(30)

*bystrategy
graph box ///
	mo_mio_f_cost_seed_ha mo_mio_f_cost_lab_ha mo_mio_f_cost_pest_ha mo_mio_f_cost_fert_ha, ///
	ylabel(0.0(0.5)3.0, format(%9.2fc)) yscale(range(0 3)) yline(0(0.25)3, lstyle(grid) lcolor(white) lwidth(thin)) ///
	by(strategy, compact rows(1) note("Agroforestry n=40, Conventional n=38; boxplots exclude major outliers") title("Farm inputs")) noout scheme(plottigblind) note("") ytitle("Mio IDR/month*ha") legend(rows(1) order(1 "Seed input" 2 "Labour cost" 3 "Pesticide input" 4 "Fertilizer input")) intensity(50)


/*
tempfile g1 g2
***
graph twoway ///
	hist rsk_dr if wave==1, scheme(plottig) xline(19.25939, lc(black) lw(medthick)) /// histogram
|| 	scatteri 0 19.25939 0.2 19.25939, c(l) m(i) lc(black) lp(dash_dot) lw(medthick) /// sample median
|| 	kdensity rsk_dr if wave==1 & T==1 [aw=weight], lc(plr1) lp(dash) lw(medthick) /// treatment		
|| 	kdensity rsk_dr if wave==1 & T==0 [aw=weight], lp(solid) lc(plb1) lw(medthick) /// control
	legend(cols(2) label(1 "Sample") label(2 "Sample median") label(3 "Treatment") label(4 "Control")) saving(`g1') nodraw
***
graph hbox rsk_dr if wave==1 [aw=weight], ///
	over(T, label(labsize(small))) by(h0q1, total compact cols(1) note("") legend(off)) ///
	scheme(plotplain) asyvars  ///
	box(1,fc(plb1) lc(plb1) lp(solid) lw(medthick)) m(1, m(circle_hollow) mc(plb1)) /// control
	box(2,fc(plr1) lc(plr1) lp(dash) lw(medthick)) m(2, m(circle_hollow) mc(plr1)) /// treatment
	ytitle("") yline(19.25939, lcol(black) lp(dash_dot) lw(medthick)) legend(off) ///
	yline(10(1)30, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	subtitle(, position(8) ring(0) nobexpand bcolor(none) placement(sw)) saving(`g2') nodraw
***
grc1leg "`g1'" "`g2'", scheme(plottig) title("Drought risk distribution 2010/11") altshrink legendfrom(`g1') xcomm ring(2) b2title("Drought risk [HazardExposure x Vulnerability]")  position(5) note("Total (n=1,109), Maseru (n=203), Leribe (n=271)," "Berea (n=281), Mafeteng (n=302), Qacha's Nek (n=52)")
***
graph export "4 Graphs\rsk_cb_dist_bl.svg", replace
*/





*INCOME SHARES
tempfile g1 g2
*per village
graph bar ///
	f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, ///
	over(village) stack scheme(plottigblind) /// 
	ytitle("Share of income source (%)") legend(cols(3) label(1 "Seasonal crops") label(2 "Perennial crops") label(3 "Animal husbandry") label(4 "Fisheries") label(5 "Off farm activities")) ///
	saving(`g1') nodraw ytitle("Income source share (%)")
	
*per strategy
graph bar ///
	f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc, ///
	over(strategy) stack scheme(plottigblind) /// 
	legend(off) saving(`g2') nodraw legend(off)

grc1leg "`g1'" "`g2'", scheme(plottigblind) legendfrom(`g1') note("Leksana n=38, Penanggungan n=40" "Agroforestry n=40, Conventional n=38" "Losses in shares not included (Penanggungan / Conventional, n=1)") title("Income diversification per village and strategy") ycomm

*INCOME IDR
*per village
graph box ///
	mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_tot_100, /// 
	by(village,compact title("Monthly income by sources and village") note("Leksana n=38, Penanggungan n=40")) scheme(plottigblind) ///
	legend(cols(3) label(1 "Seasonal crops") label(2 "Perennial crops") label(3 "Animal husbandry") label(4 "Fisheries") label(5 "Off farm activities") label(6 "Total household income") position(6)) ///
	ytitle("Mio IDR monthly") ///
	


*per strategy
graph box ///
	mo_mio_f_y_seasc_100 mo_mio_f_y_perennc_100 mo_mio_f_y_ah_100 mo_mio_f_y_fish_100 mo_mio_f_y_offnonf_100 mo_mio_f_y_tot_100 if mo_mio_f_y_seasc_100<12.5, /// 
	by(strategy,compact title("Income sources of livelihodor strategies") note("Boxplots exclude major outliers" "Agroforestry n=40, Conventional n=38" )) scheme(plottigblind) ///
	legend(cols(3) label(1 "Seasonal crops") label(2 "Perennial crops") label(3 "Livestock") label(4 "Fisheries") label(5 "Off farm activities") label(6 "Total household income") position(6)) ///
	ytitle("Mio IDR/month") ///
	ylabel(-6(2)13) yscale(range(-6 13)) yline(-6(1)13, lstyle(grid) lcolor(white) lwidth(thin)) intensity(50)


	
*DIVERSIFICATION
tempfile g1 g2
*per village
graph box ///
	y_div_inv crop_div_inv lvstck_div_inv, ///
	over(village) scheme(plottigblind) /// 
	ytitle("Diversity index (1/Simpson)") legend(rows(1) label(1 "Income diversification") label(2 "Crop diversification") label(3 "Livestock diversification")) ///
	saving(`g1') nodraw
	
*per strategy
graph box ///
	y_div_inv crop_div_inv lvstck_div_inv, ///
	over(strategy) scheme(plottigblind) ///  
	legend(off) saving(`g2') nodraw legend(off)

grc1leg "`g1'" "`g2'", scheme(plottigblind) legendfrom(`g1') note("Leksana n=38, Penanggungan n=40; Agroforestry n=40, Conventional n=38") title("Measures of diversification per village and strategy") ycomm




*fertilizer and pesticides
tempfile g1 g2
*per village
graph box ///
	mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha, ///
	over(village) scheme(plottigblind) /// 
	ytitle("10^6 IDR/month*ha") legend(rows(1) label(1 "Fertilizer") label(2 "Pesticides")) ///
	saving(`g1') nodraw
	
*per strategy
graph box ///
	mo_mio_f_cost_fert_ha mo_mio_f_cost_pest_ha, ///
	over(village) scheme(plottigblind) ///
	saving(`g2') nodraw legend(off)

grc1leg "`g1'" "`g2'", scheme(plottigblind) legendfrom(`g1') note("Leksana n=38, Penanggungan n=40; Agroforestry n=40, Conventional n=38") title("Fertilizer and pesticide expenditure") ycomm





*benefit-cost analysis and crop gross margin
*by village
graph box ///
	bc_ratio_seasc bc_ratio_seasc_perennc bc_ratio_farm, ///
	by(strategy, compact title("Benefit-cost ratio by village") note("Excludes major outliers" "Agroforestry n=40, Conventional n=38")) legend(cols(3) label(1 "Seasonal crops") label(2 "Seasonal and perennial crops") label(3 "Total farm") position(6)) scheme(plottigblind) /// 
	ytitle("Benefit-cost ratio") noout  intensity(50) note(" ")
	
swilk bc_ratio_farm



/*by strategy
graph box ///
	bc_ratio_seasc bc_ratio_farm, ///
	by(strategy, compact title("Benefit-cost ratio by strategy")) legend(cols(3) label(1 "Seasonal crops") label(2 "Total farm") position(6)) scheme(plottigblind) /// 
	ytitle("Benefit-cost ratio")
	*/
	

	
	
*graph crop gross margin by village
graph box ///
	mo_crop_gm_mio, ///
	by(village, compact title("Seasonal and perennial crop gross margin (monthly) by village") note("Leksana n=38n, Penanggungan n=40")) legend(label(1 "Crop gross margin (monthly)") position(6)) scheme(plottigblind) /// 
	ytitle("Gross margin (monthly)")


g agrof_major=.
su b2_agrof_perc if strategy==1, d
replace agrof_major=1 if b2_agrof_perc>=r(p50)
replace agrof_major=0 if b2_agrof_perc<r(p50)


cls
preserve
replace shock_drought=0 if shock_drought==. 
tab shock_drought agrof_major if strategy==1, chi2 V exact
tab shock_drought b2_agrof_yn if strategy==2, chi2 V exact
prtest shock_drought if strategy==1, by(agrof_major)
prtest shock_drought if strategy==1, by(agrof_major) level(90)
restore

cls
preserve
replace shock_wet=0 if shock_wet==.
tab shock_wet strategy, chi2 V exact
tab shock_wet b2_agrof_yn if strategy==2, chi2 V exact
prtest shock_wet if strategy==1, by(agrof_major)
tab shock_wet agrof_major if strategy==1, chi2 V exact
////////////////////////////////////////////////
prtest shock_wet if strategy==2, by(b2_agrof_yn)
tab shock_wet b2_agrof_yn if strategy==2, chi2 V exact
restore

*other way around
cls
preserve
replace shock_drought=0 if shock_drought==. 
swilk b2_agrof_perc if strategy==1
robvar b2_agrof_perc if strategy==1, by(shock_drought)
ttest b2_agrof_perc if strategy==1, by(shock_drought) une welch
ranksum b2_agrof_perc, by(shock_drought) porder 
	scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
	scalar define U2 = r(N_1)*r(N_2)-U1
	display _newline ///
	in smcl as text "U1 = " as result U1 _newline ///
	in smcl as text "U2 = " as result U2 _newline ///
	in smcl as text "U  = " as result min(U1,U2)
restore

cls
preserve
replace shock_wet=0 if shock_wet==.
tab shock_wet strategy, chi2 V exact
tab shock_wet b2_agrof_yn if strategy==2, chi2 V exact
prtest shock_wet if strategy==1, by(agrof_major)
tab shock_wet agrof_major if strategy==1, chi2 V exact
////////////////////////////////////////////////
prtest shock_wet if strategy==2, by(b2_agrof_yn)
tab shock_wet b2_agrof_yn if strategy==2, chi2 V exact
restore

*nick cox trial
*https://journals.sagepub.com/doi/pdf/10.1177/1536867X0400400209 

cls
clonevar shock_drought2= shock_drought
replace shock_drought2=0 if shock_drought2==.
clonevar shock_wet2= shock_wet 
replace shock_wet2=0 if shock_wet==. 

graph two ///
	hist b2_agrof_perc if strategy ==1 & shock_drought2 ==1, 			///
		bcol(red%30) scheme(plottigblind) fraction  bin(20) 				///
||	hist b2_agrof_perc if strategy ==1 & shock_drought2 ==0, 			///
		bcol(green%30) fraction  bin(20) 									///
		legend(rows(1) order(1 "Experienced drought shock" 2 "Did not experience drought shock") position(6)) ///
		ylabel(0(0.1)0.6)
				
stripplot b2_agrof_perc if strategy==1, ///
	over(shock_drought2) stack vertical width(0.1) height(0.3) ms(sh sh) mc(plg2 plr2) ///
	box(barw(0.08)) boffset(-0.12) pctile(5) aspect(1) yla(, ang(h)) scheme(plottigblind) ///
	ytitle("Share of Agroforestry [%]") ///
	ytick(25(5)100) separate(shock_drought2) legend(off) xlab(0 `""No drought" "n=26""' 1 `""Drought" "n=14""')
	
stripplot b2_agrof_perc, ///
	over(shock_drought2) stack vertical width(0.1) height(0.3) ms(sh sh) mc(plg2 plr2) ///
	box(barw(0.08)) boffset(-0.12) pctile(5) aspect(1) yla(, ang(h)) scheme(plottigblind) ///
	ytitle("Share of Agroforestry [%]") ///
	ytick(0(5)100) separate(shock_drought2) 	
	
stripplot b2_agrof_perc if strategy==1, ///
	over(shock_wet2) stack vertical width(0.1) height(0.3) ms(sh sh) mc(plg2 plb2) ///
	box(barw(0.08)) boffset(-0.12) pctile(5) aspect(1) yla(, ang(h)) scheme(plottigblind) ///
	ytitle("Share of Agroforestry [%]") ///
	ytick(20(5)100) separate(shock_wet2) legend(off) xlab(0 `""No excess wetness" "n=33""' 1 `""Excess wetness" "n=7""')
	
stripplot b2_agrof_perc, ///
	over(shock_wet2) stack vertical width(0.1) height(0.3) ms(sh sh) mc(plg2 plb2) ///
	box(barw(0.08)) boffset(-0.12) pctile(5) aspect(1) yla(, ang(h)) scheme(plottigblind) ///
	ytitle("Share of Agroforestry [%]") ///
	ytick(0(5)100) separate(shock_wet2) 
	
	
	legend(off) xlab(0 `""No excess wetness" "n=33""' 1 `""Excess wetness" "n=7""')
	
	
	yline(20(5)100, lstyle(grid) lcol(white) lwidth(thin))
	
su  b2_agrof_perc if strategy==1 & shock_drought2==0, d


graph two ///
	kdensity b2_agrof_perc if strategy ==1 & shock_drought2 ==1, lcol(red%30*0.3) scheme(plottigblind) ///
||	kdensity b2_agrof_perc if strategy ==1 & shock_drought2 ==0, lcol(green%*0.3)

*********************************
*** hazards					*****
*********************************

graph bar (count) ///
	shock_ideosyncratic shock_drought shock_wet shock_harvestf shock_landslide shock_none shock_market shock_seedqual shock_storm shock_traderdefault shock_animals ///
	, by(strategy, compact title("Shocks with highest impact (last 5 years)") note("Agroforestry n=40, Conventional n=38")) ///
	blabel(total, position(base) size(medium)) ///
	legend(cols(3) label(1 "Idiosyncratic: family health") label(2 "Water scarcity") label(3 "Excess wetness") label(4 "Undefined harvest failure") label(5 "Landslides") label(6 "None") label(7 "Market/price shocks") label(8 "Low seed quality") label(9 "Storm/fierce winds") label(10 "Trader default") label(11 "Wild animals destroyed crops")) scheme(plotplainblind)
	
label var shock_ideosyncratic "Ideosyncratic shock (family health)"
label var shock_drought "Drought"
label var shock_wet "Excess wetness"
label var shock_harvestf "Harvest failure (unspecified)"
label var shock_landslide "Landslide"
label var shock_none "No major event"
label var shock_market "Market shock / price volatility"
label var shock_seedqual "Seed quality"
label var shock_storm "Storm / fierce winds"
label var shock_traderdefault "Trader default"
label var shock_animals "Wild animals damaged crops"

replace shock_market =1 if shock_seedqual==1
replace shock_market =1 if shock_traderdefault==1
replace shock_harvestf =1 if shock_animals==1
	
mrtab 	shock_ideosyncratic shock_drought shock_wet shock_harvestf shock_landslide shock_none shock_market  shock_storm  ///
	, by(strategy) include col rcol
	
mrgraph hbar ///
	shock_ideosyncratic shock_drought shock_wet shock_harvestf shock_landslide shock_none shock_market  shock_storm  ///
	, by(strategy)  sort stat(col) scheme(plotplainblind) ///
	bargap(20) blabel(total, format(%9.2fc)) legend(rows(1) position(6)) ///
	includemissing ytitle("Percent of group") title("Major shocks (last 5 years)") note("Multiple answers possible" "Agroforestry n=40, Conventional n=38")
	

/*
mrgraph hbar d2_soilpt_landsl d2_soilpt_eros d2_soilpt_infert d2_soilpt_dryness d2_soilpt_other, ///
	by(strategy) scheme(plotplain) stat(col) title("Major soil related problems") note("Multiple answers possible" "Agroforestry n=40, Conventional n=38") legend(rows(1) position(6)) blabel(total, format(%9.2fc)) bargap(20)
*/




*********************************
*** coping strategies 		*****
*********************************

mrtab f3_cope_plusinp f3_cope_irr_ws f3_cope_redinv f3_cope_fallow f3_cope_sell_assets f3_cope_selltrees f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork fr_cope_savings f3_cope_selllvstck f3_cope_formcred f3_cope_infcred f3_cope_earlharv f3_cope_fencingoff f3_cope_traders, by(strategy) sort include

g f3_cope_other =.
replace f3_cope_other=1 if f3_cope_traders==1
replace f3_cope_other=1 if f3_cope_fencingoff==1
g f3_cope_sellasslvsttrees =1 if f3_cope_selllvstck ==1 | f3_cope_sell_assets==1 | f3_cope_selltrees==1
replace f3_cope_redinv=1 if f3_cope_fallow==1
replace f3_cope_plusinp=1 if f3_cope_irr_ws==1



g f3_cope_positive = 0
replace f3_cope_positive=1 if f3_cope_plusinp==1 | f3_cope_cd ==1 | f3_cope_cropcha ==1 | f3_cope_offfarmwork  ==1 | fr_cope_savings ==1 | f3_cope_sellasslvsttrees  ==1
g f3_cope_negative = 0 
replace f3_cope_negative=1 if f3_cope_redinv ==1 | f3_cope_formcred ==1 | f3_cope_infcred == 1 | f3_cope_earlharv==1 

tab f3_cope_positive f3_cope_negative // 7 overlap
prtest f3_cope_negative, by(strategy) //
ttest res2, by(f3_cope_negative)
	




g f3_cope_combinedpos1 =. 
replace f3_cope_combinedpos1=1 if f3_cope_positive==1
replace f3_cope_combinedpos1=0 if f3_cope_negative==1

swilk res2
robvar res2, by(f3_cope_combinedpos1) //equal
swilk res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos1<. //all equal vars
robvar res_cap_buffer, by(f3_cope_combinedpos1) //equal
robvar res_cap_learnadapt, by(f3_cope_combinedpos1) //unequal
robvar  res_cap_selforg, by(f3_cope_combinedpos1) // equal

swilk res2 if f3_cope_combinedpos1<.
ttest res2, by(f3_cope_combinedpos1)

mvtest normality res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos1<. //not 




clonevar cope = f3_coping 
replace cope="No event" if cope=="None necessary"

replace cope="Resolve it on our own" if cope=="a"
replace cope="Help from community" if cope=="b"
replace cope="Help from outside community" if cope=="c"
replace cope="Help from government" if cope=="d"
replace cope="Access credit (formal and informal)" if cope=="e"
replace cope="Sitting it out" if cope=="f"



mrgraph hbar cope,by(strategy) scheme(plotplain) stat(col) title("Coping strategies applied") note("Multiple answers possible" "Agroforestry n=40, Conventional n=38") ytitle("Percent of group") legend(rows(1) position(6)) blabel(total, format(%9.2fc)) bargap(20) includemissing ytitle("Percent of observations") sort descending 

g f3_cope_combinedpos2=f3_cope_combinedpos1
label var f3_cope_combinedpos2 "Household applies positive coping strategy"
replace f3_cope_combinedpos2=1 if cope=="Help from community" & f3_cope_combinedpos2==.
replace f3_cope_combinedpos2=1 if cope=="Resolve it on our own" & f3_cope_combinedpos2==.
replace f3_cope_combinedpos2=0 if cope=="Sitting it out" & f3_cope_combinedpos2==.
tab3way cope f3_cope_combinedpos2 strategy, usemiss total

tab f3_cope_combinedpos2 strategy, all
tab f3_cope_combinedpos2 village, all


omninorm res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<., by(f3_cope_combinedpos2) //both normal (also by strategy)
mvtest covariances res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<., by(f3_cope_combinedpos2) //both normal (also by strategy)

foreach var of varlist res_cap_buffer res_cap_learnadapt res_cap_selforg {
    g `var'_100=`var'*100
	g `var'_10=`var'*10
}

manova res2 res_cap_buffer res_cap_learnadapt res_cap_selforg = f3_cope_combinedpos2
logit f3_cope_combinedpos2 res_cap_buffer_10 res_cap_learnadapt_10 res_cap_selforg_10 i.strategy, or 
logit f3_cope_combinedpos2 res_cap_buffer_10 res_cap_learnadapt_10 res_cap_selforg_10 i.strategy, or vce(cluster village)
*continue here
*ordered logistic regression for motivations!!!


swilk res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<. //all normal 
robvar res2, by(f3_cope_combinedpos2)
ttest res2, by(f3_cope_combinedpos2)
robvar res_cap_buffer, by(f3_cope_combinedpos2)
ttest res_cap_buffer, by(f3_cope_combinedpos2)
robvar res_cap_learnadapt, by(f3_cope_combinedpos2)
ttest res_cap_learnadapt, by(f3_cope_combinedpos2)
robvar res_cap_selforg, by(f3_cope_combinedpos2)
ttest res_cap_selforg, by(f3_cope_combinedpos2)

tabplot f3_cope_combinedpos2 strategy, showval percent yasis scheme(s1mono) bfcolor(ltblue) blcolor(blue) title("Livelihood strategies and coping strategies") note("Sample n=71, Agroforestry n=36, Conventional n=35")

g f3_cope_combinedpneg2=.
replace f3_cope_combinedpneg2=1 if f3_cope_combinedpos2==0
replace f3_cope_combinedpneg2=0 if f3_cope_combinedpos2==1


graph hbar f3_cope_combinedpos2 f3_cope_combinedpneg2, over(strategy) stack scheme(plottigblind) percent legend(rows(1) order(1 "Positive coping strategy" 2 "Negative coping strategy") position(6))  blabel(bar, size(medsmall) position(center) format(%9.2fc)) bar(1, col(green%30)) bar(2, col(red%30)) ytitle("percent") note("Agroforestry n=36, Conventional n=35")

*create vars for help from community resolve it on our own and sitting it out without the categories that already exist
*put them in the graph
g f3_cope_comhelp=1 if cope=="Help from community" & f3_cope_combinedpos1==. & f3_cope_plusinp!=1 & f3_cope_redinv != 1 & f3_cope_cd !=1 & f3_cope_cropcha != 1 & f3_cope_offfarmwork != 1 & fr_cope_savings != 1 & f3_cope_formcred != 1 & f3_cope_infcred  != 1 & f3_cope_earlharv != 1
g f3_cope_ownresolve=1 if cope=="Resolve it on our own" & f3_cope_combinedpos1==. & f3_cope_plusinp!=1 & f3_cope_redinv != 1 & f3_cope_cd !=1 & f3_cope_cropcha != 1 & f3_cope_offfarmwork != 1 & fr_cope_savings != 1 & f3_cope_formcred != 1 & f3_cope_infcred  != 1 & f3_cope_earlharv != 1
g f3_cope_sittingitout=1 if cope=="Sitting it out" & f3_cope_combinedpos1==. & f3_cope_plusinp!=1 & f3_cope_redinv != 1 & f3_cope_cd !=1 & f3_cope_cropcha != 1 & f3_cope_offfarmwork != 1 & fr_cope_savings != 1 & f3_cope_formcred != 1 & f3_cope_infcred  != 1 & f3_cope_earlharv != 1

label var f3_cope_plusinp "Increase input (+)"
label var f3_cope_redinv "Reduce investment (-)"
label var f3_cope_cd "Diversify crops (+)"
label var f3_cope_cropcha "Change main crop (+)"
label var f3_cope_offfarmwork "Off-farm work (+)"
label var fr_cope_savings "Use savings (+)"
label var f3_cope_formcred "Formal credit (-)"
label var f3_cope_infcred "Informal credit (-)"
label var f3_cope_earlharv "Early harvest (-)"
label var f3_cope_sellasslvsttrees "Sell assets: Trees / livestock (+)"
label var f3_cope_comhelp "Community help (+)"
label var f3_cope_ownresolve "Resolve it on our own (+)"
label var f3_cope_sittingitout "Sitting it out (-)"
label var f3_cope_other "Other"

mrgraph hbar ///
	f3_cope_plusinp f3_cope_redinv  f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork fr_cope_savings  f3_cope_formcred f3_cope_infcred f3_cope_earlharv  f3_cope_sellasslvsttrees f3_cope_comhelp f3_cope_ownresolve f3_cope_sittingitout, ///
	by(strategy) scheme(plotplain) title("Coping strategies applied") ///
	note("Multiple answers possible; Neg. (-) and pos. (+) coping strategies" "Agroforestry n=40, Conventional n=38") ytitle("Percent of group") ///
	legend(rows(1) position(6)) sort descending blabel(total, size(vsmall) format(%9.2fc)) bargap(20) includemissing stat(col) 

mrtab f3_cope_plusinp f3_cope_redinv  f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork fr_cope_savings  f3_cope_formcred f3_cope_infcred f3_cope_earlharv  f3_cope_sellasslvsttrees f3_cope_comhelp f3_cope_ownresolve f3_cope_sittingitout, sort

swilk res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<. //all normal
robvar res2, by(f3_cope_combinedpos2) //unequal
robvar res_cap_buffer, by(f3_cope_combinedpos2) //unequal
robvar res_cap_learnadapt, by(f3_cope_combinedpos2) //unequal
robvar res_cap_selforg, by(f3_cope_combinedpos2) //equal

mvtest normality  res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<. //not multivariate normal
mvtest covariances  res2 res_cap_buffer res_cap_learnadapt res_cap_selforg if f3_cope_combinedpos2<. //not multivariate normal

ttest res2, by(f3_cope_combinedpos2) une welch //yes 
ttest res_cap_buffer, by(f3_cope_combinedpos2) une welch
ttest res_cap_learnadapt

/*
graph bar (count) ///
	 f3_cope_plusinp f3_cope_irr_ws f3_cope_redinv f3_cope_fallow f3_cope_sell_assets f3_cope_selltrees f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork fr_cope_savings f3_cope_selllvstck f3_cope_formcred f3_cope_infcred f3_cope_earlharv f3_cope_fencingoff f3_cope_traders ///
	,by(strategy, compact title("Coping strategies applied")) ///
	blabel(total, position(base) gap(0) size(medium)) ///
	scheme(plotplainblind) ///
	legend(cols(3) label(1 "Increase input") label(2 "Increase irrigation") label(3 "Reduce investment in cropping") label(4 "Fallow land") label(5 "Sell household assets") label(6 "Sell trees") label(7 "Diversify crops") label(8 "Change main crops") label(9 "Off-farm work") label(10 "Use savings") label(11 "Sell livestock") label(12 "Access formal credit") label(13 "Access informal credit") label(14 "Early harvest") label(15 "Fencing off") label(16 "Change traders"))
	*/





/*
encode cope, generate(cope2)
label define copecope 8 "Resolve it on our own" 9 "Help from community" 10 "Help from outside community" 11 "Help from government" 12 "Access credit (formal and informal)" 13 "Sitting it out" 14 "No event", modify
label values cope2 copecope

catplot cope2, over(strategy) scheme(plotplainblind) percent blabel(bar, position(base) format(%9.2fc))

label define io_s_i1_label 1 "Direct contract (as individ.)" 2 "Traders (as individ.)" 4 "Traders (as farmer group)", modify

label values io_s_i1 io_s_i1_label

catplot ///
	io_s_i1 ///
	,over(strategy) scheme(plotplainblind) ytitle("% of group") ///
	title("Marketing methods") note("Agroforestry n=40 Conventional n=38") ///
	 percent(strategy) blabel(bar, position(base) format(%9.2fc)) var1opts(gap(1)) ///
	 legend(rows(1) position(6)) ///
	 ylabel(0(10)90) yscale(range(0 90)) yline(0(5)90, lstyle(grid) lcolor(gs10) lwidth(thin)) 
	 
mrgraph hbar io_s_i1 ///
	,by(strategy) scheme(plotplainblind) ytitle("% of group") ///
	title("Marketing methods") note("Agroforestry n=40 Conventional n=38")
	includemissing stat(col) poly
	 
graph bar (count) f3_cope_plusinp f3_cope_irr_ws f3_cope_redinv f3_cope_fallow f3_cope_sell_assets f3_cope_selltrees f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork f3_cope_selllvstck f3_cope_formcred f3_cope_infcred f3_cope_earlharv f3_cope_fencingoff f3_cope_traders,over(f3_a_cope_resolve) over(strategy)

dummies cope2
rename (cope21 cope22 cope23 cope24 cope25 cope26 cope27) (f3_a_cope_resolve f3_a_cope_helpincomm f3_a_cope_helpoutside f3_a_cope_helpgov f3_a_cope_credit f3_a_cope_sittingout f3_a_cope_noevent)
label var f3_a_cope_resolve "Resolve it on our own"
label var f3_a_cope_helpincomm "Help from relatives/neighbours/friends in community"
label var f3_a_cope_helpoutside "Help from relatives/neighbours/friends outside community"
label var f3_a_cope_helpgov "Help from local government"
label var f3_a_cope_credit "Access credit (formal and informal)"
label var f3_a_cope_sittingout "Sitting it out"
label var f3_a_cope_noevent "No event"

replace f3_a_cope_resolve=0 if f3_a_cope_resolve==. 
replace f3_a_cope_helpincomm=. if f3_a_cope_helpincomm==0 
replace f3_a_cope_helpoutside=. if f3_a_cope_helpoutside==0 
replace f3_a_cope_helpgov=. if f3_a_cope_helpgov==0 
replace f3_a_cope_credit=. if f3_a_cope_credit==0 
replace f3_a_cope_sittingout=. if f3_a_cope_sittingout==0 
replace f3_a_cope_noevent=. if f3_a_cope_noevent==0 


pwcorr f3_cope_plusinp f3_cope_irr_ws f3_cope_redinv f3_cope_fallow f3_cope_sell_assets f3_cope_selltrees f3_cope_cd f3_cope_cropcha f3_cope_offfarmwork f3_cope_selllvstck f3_cope_formcred f3_cope_infcred f3_cope_earlharv f3_cope_fencingoff f3_cope_traders f3_a_cope_resolve f3_a_cope_helpincomm f3_a_cope_helpoutside f3_a_cope_helpgov f3_a_cope_credit f3_a_cope_sittingout f3_a_cope_noevent, sig print(0.1) star(0.01)

graph bar (mean) f3_a_cope_resolve f3_a_cope_helpincomm f3_a_cope_helpoutside f3_a_cope_helpgov f3_a_cope_credit f3_a_cope_sittingout f3_a_cope_noevent, by(strategy) scheme(plotplainblind)

br f3_a_cope_resolve f3_a_cope_helpincomm f3_a_cope_helpoutside f3_a_cope_helpgov f3_a_cope_credit f3_a_cope_sittingout f3_a_cope_noevent
*/

*********************************
*** agroforestry why		*****
*********************************

replace b3_agrofwhy_pos1 = "1" if b3_agrofwhy_pos1 == "Substitution of rice, price for vegetables, trees as assets (Albizia), food provision for family"
destring b3_agrofwhy_pos1, replace
replace b3_agrofwhy_pos5=1 if b3_agrofwhy_pos8==1
replace b3_agrofwhy_pos5=1 if b3_agrofwhy_pos10==1
replace b3_agrofwhy_pos2=1 if adm_code_boku_1=="HZ15L"
replace b3_agrofwhy_pos5=1 if b3_agrofwhy_pos6==1

local motvars b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos6 b3_agrofwhy_pos7 b3_agrofwhy_pos8 b3_agrofwhy_pos9 b3_agrofwhy_pos10 b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5 
foreach var of local motvars {
	replace `var' = 0 if `var'==. 
}

local motvars b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5 
foreach var of local motvars {
	replace `var' = 0 if `var'==. 
}

g stratevar=0
replace stratevar=1 if strategy==1
*logit stratevar b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos7 b3_agrofwhy_pos9 b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5 i.vill, or vce(cluster vill) cformat(%9.3fc) pformat(%9.3fc) sformat(%9.3fc)

pwcorr stratevar b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos7 b3_agrofwhy_pos9, sig 

pwcorr stratevar b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos7 b3_agrofwhy_pos9 b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5, sig print(0.1) star(0.01)
	
*final tab
mrtab b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos7 b3_agrofwhy_pos9, by(strategy) title("Reasons to implement agroforestry") width(24) sort column rcolumn

*column percent of responses
*column percent of cases

mrtab b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5, by(strategy) include title("Reasons to implement agroforestry") width(24) sort column rcolumn 

mrgraph dot b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos6 b3_agrofwhy_pos7 b3_agrofwhy_pos9, by(strategy) include title("Reasons to implement agroforestry")


*positive
graph bar (count) /// 
	b3_agrofwhy_pos1 b3_agrofwhy_pos2 b3_agrofwhy_pos3 b3_agrofwhy_pos4 b3_agrofwhy_pos5 b3_agrofwhy_pos6 b3_agrofwhy_pos7 b3_agrofwhy_pos8 b3_agrofwhy_pos9 b3_agrofwhy_pos10 ///
	,by(strategy, compact title("Reasons for agroforestry") ///
	note("Multiple answers possible. Total reasons mentioned:62" "For households with agroforestry > 0% (n=49/78")) ///
	scheme(plotplainblind) legend(rows(5) label(1 "Economic: Asset for saving / Safety Net") label(2 " Local convention or tradition of agroforestry") label(3 "Economic: Additional income") label(4 "Convenience: less treatment and effort") label(5 "SWC & Reduction of landslide hazard") label(6 "Shade: additional and benficial") label(7 "Construction material") label(8 "Landslide prevention") label(9 "Food for livestock") label(10 "Soil fertility increase")) ///
	blabel(total, position(base) gap(0)) intensity(30)

	
*negative
graph bar (count) /// 
	b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5 ///
	if strategy==2, by(strategy, compact title("Reasons against agroforestry") note("Multiple answers possible. Total reasons mentioned:31" "For households with 0% agroforestry (n=29/78)")) scheme(plotplainblind) ///
	legend(cols(2)label(1 "Soil fertility decrease") label(2 "Economic reasons / profitability  considerations") label(3 "Negative impact on crops (shade, water retention)") label(4 "Local convention/tradition of not having agroforestry") label(5 "Difficulty of change (legally/technically)")) ///
	blabel(bar, position(base) size(small) format(%9.2fc)) intensity(15)
	
graph hbar /// 
	b3_agrofwhy_neg1 b3_agrofwhy_neg2 b3_agrofwhy_neg3 b3_agrofwhy_neg4 b3_agrofwhy_neg5 ///
	if strategy==2, over(strategy, label(labsize(small))) title("Reasons against agroforestry") note("Multiple answers possible. Total reasons mentioned:31" "For households with 0% agroforestry (n=29/78)") scheme(plotplainblind) ///
	legend(cols(2)label(1 "Soil fertility decrease") label(2 "Economic reasons / profitability  considerations") label(3 "Negative impact on crops (shade, water retention)") label(4 "Local convention/tradition of not having agroforestry") label(5 "Difficulty of change (legally/technically)")) ///
	blabel(bar, position(base) size(small) format(%9.2fc))

	
*agroforestry share
graph box b_cd2_100, over(village, total) title("Share of agroforestry on farmland") ytitle("Share (%)") note("Estimation of household head") scheme(plottigblind) saving(`g2') nodraw
	

	
graph two  ///
	scatter f_y_seasc f_cost_fert, col(green) ///
||	lfit f_y_seasc f_cost_fert, lc(green) ///
||	scatter f_y_seasc c3_pest_totval, col(red) ///
||	lfit f_y_seasc c3_pest_totval if village=="Penanggungan", lcol(red) scheme(plottigblind)


*********************************
*** swc 					*****
*********************************

d4_swc_og d4_swc_1 d4_swc_2 d4_swc_3 d4_swc_4 d4_swc_5 d4_swc_6 d4_swc_7 d4_swc_8 d4_swc_9 d4_swc_10 d4_swc_ot d4_swc_sum

graph bar (count) d4_swc_1 d4_swc_2 d4_swc_3 d4_swc_4 d4_swc_5 d4_swc_6 d4_swc_7 d4_swc_8 d4_swc_9 d4_swc_10, by(strategy, compact title("Soil and water conservation measures")) scheme(plottigblind) legend(cols(3) label(1 "Terracing") label(2 "Contour farming") label(3 "Mixed cropping system") label(4 "Crop rotation") label(5 "Adding manure") label(6 "Natural mulching") label(7 "Plastic covering") label(8 "Agroforestry") label(9 "Soil bounds (trees, stones)") label(10 "Other")) blabel(total, position(base) size(medium))

tab d4_swc_sum strategy, row col

mean d4_swc_sum if strategy==1
mean d4_swc_sum if strategy==2

ttest d4_swc_sum, by(strategy)


*d4_swc_ot d4_swc_sum



*********************************
*** indicator 				*****
*********************************

export excel ///
	adm_code_ugm_1 adm_code_boku_1 adm_1 village strategy ///
	immx_b_nc_index immx_b_pc_index immx_b_sc_index immx_b_hc_index immx_b_fc_index immx_b_farmdiv_index res_cap_buffer ///
	immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index res_cap_learnadapt ///
	immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp res_cap_selforg ///
	res_cap_buffer res_cap_learnadapt res_cap_selforg res2 ///
	using "6 Material\resilience2.xlsx", ///
	firstrow(var) replace
	
swilk immx_b_nc_index immx_b_pc_index immx_b_sc_index immx_b_hc_index immx_b_fc_index immx_b_farmdiv_index res_cap_buffer ///
	immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index res_cap_learnadapt ///
	immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp res_cap_selforg ///
	res_cap_buffer res_cap_learnadapt res_cap_selforg res2

	
mean immx_b_nc_index, over(strategy) vce(bootstrap)
mean immx_b_pc_index, over(strategy) vce(bootstrap)
mean immx_b_sc_index, over(strategy) vce(bootstrap)
mean immx_b_hc_index, over(strategy) vce(bootstrap)
mean immx_b_fc_index, over(strategy) vce(bootstrap)
mean immx_b_farmdiv_index, over(strategy) vce(bootstrap)
mean res_cap_buffer, over(strategy) vce(bootstrap)

mean immx_c_kto_index, over(strategy) vce(bootstrap)
mean immx_c_rsl_index, over(strategy) vce(bootstrap)
mean immx_c_op_index, over(strategy) vce(bootstrap)
mean immx_c_ffm_index, over(strategy) vce(bootstrap)
mean immx_c_tek_index, over(strategy) vce(bootstrap)
mean immx_c_tek_index, over(strategy) vce(bootstrap)
mean immx_c_sv_index, over(strategy) vce(bootstrap)
mean res_cap_learnadapt, over(strategy) vce(bootstrap)

mean immx_s_inst_index, over(strategy) vce(bootstrap)
mean immx_s_cn_index, over(strategy) vce(bootstrap)
mean immx_s_ss_index, over(strategy) vce(bootstrap)
mean res_cap_selforg, over(strategy) vce(bootstrap)

mean res_cap_buffer, over(strategy) vce(bootstrap)
mean immx_c_kto_index, over(strategy) vce(bootstrap)
mean res_cap_learnadapt, over(strategy) vce(bootstrap)
mean res_cap_selforg, over(strategy) vce(bootstrap)



*buffer capacity boxplot
graph box ///
	immx_b_nc_index immx_b_pc_index immx_b_sc_index immx_b_hc_index immx_b_fc_index immx_b_farmdiv_index, ///
	by(strategy, compact title("Resilience: Buffer capacity") note("Agroforestry n=40, Conventional n=38")) asyvars scheme(plotplainblind) legend(rows(2)) ytitle("Indexed scale") intensity(50)

*resilience - capacity for learning and adaptation
graph box ///
	immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index, ///
	 by(strategy, compact title("Resilience: Capacity for learning and adaptation") note("Agroforestry n=40, Conventional n=38")) scheme(plottigblind) legend(rows(4)) ytitle("Indexed scale") intensity(50)

*capacity for self-organisation
graph box ///
	immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp, ///
	by(strategy, compact title("Resilience: Capacity for self-organisation") note("Agroforestry n=40, Conventional n=38")) scheme(plotplainblind) legend(rows(2)) ytitle("Indexed scale") intensity(50)

*boxgraph
graph box ///
	res_cap_buffer res_cap_learnadapt res_cap_selforg res2, by(strategy, compact title("Resilience dimensions") note("Agroforestry n=40, Conventional n=38")) scheme(plotplainblind) legend(rows(2)) ytitle("Indexed scale") intensity(50)


	
	
	

*correlation of wellbeing and resilience with wellbeing and resilience


*for comparison with the not rescaled variables. 
replace f4_resilience = 7 if f4_resilience==6.5
replace f4_resilience = 6 if f4_resilience==5.5
ktau f5_wellbeing_rev f4_resilience

lab var f5_wellbeing_rev "Q: Wellbeing compared to neighbours [1=worst to 6=best]"
graph two ///
	scatter f4_resilience f5_wellbeing_rev, ms(Th) yaxis(1) jitter(2) ///
||	lfit f4_resilience f5_wellbeing_rev, yaxis(1) ///
	by(strategy, total rows(1) title("Stated resilience and wellbeing") note("Agroforestry n=40, Conventional n=38" "Sample: Kendall's {&tau}{subscript:b}=0.191, p<0.05")) scheme(plottigblind) ///
	legend(rows(1) order(1 "Households (jitter 2)" 2 "Fitted values")) ytitle("Q: Resilience to withstand shocks [1=worst - 10=best]") ///
	yline(0(1)10, lstyle(grid) lcolor(white) lwidth(thin)) ylabel(0(1)10)
	
graph two ///
	sunflower f4_resilience f5_wellbeing_rev, msymbol(circle_hollow) binar(1.3) lbstyle(background) lflc(blue) dflc(blue) dbstyle(background) yaxis(1) ///
||	lfit f4_resilience f5_wellbeing_rev, yaxis(1) ///
	by(strategy, total rows(1) title("Stated resilience and wellbeing") note("Agroforestry n=40, Conventional n=38" "Sample: Kendall's {&tau}{subscript:b}=0.193, p<0.05")) scheme(plottigblind) ///
	legend(rows(1) order(1 "Single obs." 3 "1 petal = 1 obs." 4 "Fitted values")) ///
	ytitle("Q: Resilience to withstand shocks [1=worst - 10=best]") ///
	yline(1(1)10, lstyle(grid) lcolor(white) lwidth(thin)) ylabel(1(1)10) 
	
	
graph two ///
		sunflower res2 mmx_f5_wellbeing_rev ///
			,msymbol(circle_hollow) binar(1) lbstyle(background) lflc(blue) dflc(blue) dbstyle(background) ///
||		lfit res2 mmx_f5_wellbeing_rev, yaxis(1) ///
	by(strategy, total rows(1) title("Calculated resilience and wellbeing") note("Agroforestry n=40, Conventional n=38" "Sample: Kendall's {&tau}{subscript:b}=0.191, p<0.05")) ///
		scheme(plottigblind) ///
		legend(rows(1) order(1 "Single obs." 3 "1 petal = 1 obs." 4 "Fitted values")) ///
		ytitle("Q: Resilience to withstand shocks [1=worst - 10=best]") ///
		yline(0.2(0.1)0.8, lstyle(grid) lcolor(white) lwidth(thin)) ylabel(0.2(0.2)0.8) 
	


two ///
	sunflower res2 mmx_f5_wellbeing_rev, m(circle_hollow) yaxis(1)			///
||	lfit res2 mmx_f5_wellbeing_rev, yaxis(1) ytitle("Resilience score")	 				///
	by(strategy, total rows(1) title("Comparing wellbeing and resilience") note("Agroforestry n=40, Conventional n=38")) scheme(plottigblind) 
	
	///
	legend(rows(4)order(1 "Stated wellbeing" 2 "Fitted values for wellbeing"  3 "Q: Capability of household to withstand future hazard or shock?" 4 "Fitted values for sesilience" 5 "Fitted values for wellbeing and  inquiredresilience")) ///
	 xtitle("Q:Wellbeing [1-6] | Q:Resilience [1-10]") 
	
ktau 	res_cap_buffer res_cap_learnadapt res_cap_selforg res2, stats(taub p) print(0.1) star(0.01) //resilience not corerlation 

ktau res2 res_cap_buffer res_cap_learnadapt res_cap_selforg res2 mmx_f5_wellbeing_rev mmx_f4_resilience if strategy==2, stats(taub p) //resilience not corerlation 



	
new_f6_res new_f5_wellb_rev

two ///
	scatter res2 new_f5_wellb_rev, yaxis(1)		///
||	lfit res2 new_f5_wellb_rev, yaxis(1)	 				///
||	scatter res2 new_f6_res, yaxis(1)	 				///
||	lfit res2 new_f6_res, lp(dash_dot) yaxis(1) ytitle("Resilience score")	 				///
	by(strategy,  total rows(1) title("Comparing wellbeing and resilience") note("Agroforestry n=40, Conventional n=38")) scheme(plottigblind) ///
	legend(rows(2) position(6) order(1 "Stated wellbeing" 2 "Fitted values for wellbeing and calculated resilience" 3 "Stated resilience" 4 "Fitted values for stated and calculated resilience")) ///
	xtitle("Q:Wellbeing [1-6=best] | Q:Resilience [1-10=b]") xtick(0(.1)1)
	
*mmx_f4_resilience mmx_f5_wellbeing_rev
	
two ///
	scatter res2 mmx_f5_wellbeing_rev, yaxis(1)		///
||	lfit res2 mmx_f5_wellbeing_rev, yaxis(1)	 				///
||	scatter res2 mmx_f4_resilience, yaxis(1)	 				///
||	lfit res2 mmx_f4_resilience, lp(dash_dot) yaxis(1) ytitle("Resilience score")	 				///
	by(strategy,  total rows(1) title("Comparing wellbeing and resilience") note("Agroforestry n=40, Conventional n=38")) scheme(plottigblind) ///
	legend(rows(2) position(6) order(1 "Stated wellbeing" 2 "Fitted values for wellbeing and calculated resilience" 3 "Stated resilience" 4 "Fitted values for stated and calculated resilience")) ///
	xtitle("Q:Wellbeing [1-6=best] | Q:Resilience [1-10=b]") xtick(0(.1)1)
	
	
	
	
	
	
two ///
	scatter mmx_f4_resilience mmx_f5_wellbeing_rev, yaxis(1)		///
||	lfit mmx_f4_resilience mmx_f5_wellbeing_rev, yaxis(1)	 				/// 				///
	title("Comparing wellbeing and resilience") note("Agroforestry n=40, Conventional n=38") scheme(plottigblind) ///
	legend(rows(2) position(6) order(1 "Stated wellbeing" 2 "Fitted values for wellbeing and calculated resilience" 3 "Stated resilience" 4 "Fitted values for stated and calculated resilience")) ///
	xtitle("Q:Wellbeing [1-6] | Q:Resilience [1-10]") xtick(0(.1)1)
	
	
	
	
	