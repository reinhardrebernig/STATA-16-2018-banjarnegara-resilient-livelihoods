


***************************
***		INDEX		*******
***************************

***********************************
***		Buffer Capacity		*******
***********************************

////////////////////////////////////////////////
***natural capital
des io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 
/*
io_b_nc1        byte    %10.0g                Land ownership [yes/no]
io_b_nc2        byte    %10.0g                Soil condition [scale, 1-6] (to inverse)
io_b_nc3        byte    %10.0g                Soil physical condition [scale, 1-6] (to inverse)
io_b_nc4        double  %10.0g                Soil on slopes [%] (to inverse)
io_b_nc5        byte    %10.0g                Soil related problems [0-4] (to inverse)
*/



*final var immx_b_nc_index
rename immx_b_nc_index immx_b_nc_index_old
egen immx_b_nc_index = rowmean(immx_b_nc1 immx_b_nc2 immx_b_nc3 immx_b_nc4 immx_b_nc5) //all inversed
label var immx_b_nc_index "Natural capital"

*testing natural capital
swilk io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5
tabstat io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5, by(strategy) statistics(mean sd median iqr) nototal format(%9.3fc) columns(statistics) longstub

ktau io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5, stats(taub p)
spearman io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5, stats(rho p)

prtest io_b_nc1, by(village)

ranksum io_b_nc2, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 io_b_nc2 i.strategy
cendif io_b_nc2, by(strategy)

ranksum io_b_nc3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)
qreg2 io_b_nc3 i.strategy
cendif io_b_nc3, by(strategy)

robvar io_b_nc4, by(strategy)
ttest io_b_nc4, by(strategy)

median io_b_nc5, by(strategy)
ranksum io_b_nc5, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_nc5 i.strategy
cendif io_b_nc5, by(strategy)


*****
*graphics
*****
g d2_soilpt_dryness = .
replace d2_soilpt_dryness=1 if d2_soilpt_ot=="Dry Season" | d2_soilpt_ot=="Dry soil" | d2_soilpt_ot=="Dry soil now in this season" | d2_soilpt_ot=="hard to plow" | d2_soilpt_ot=="problems during dry season"

g d2_soilpt_other =. 
replace d2_soilpt_other= 1 if d2_soilpt_dryness != 1 & d2_soilpt_otyn==1

egen d2_soilpt_final=rowtotal(d2_soilpt_landsl d2_soilpt_eros d2_soilpt_infert d2_soilpt_dryness d2_soilpt_other)
median d2_soilpt_final, by(strategy)
ranksum d2_soilpt_final, by(strategy)

graph bar (count) ///
	d2_soilpt_landsl d2_soilpt_eros d2_soilpt_infert d2_soilpt_dryness d2_soilpt_other, ///
	by(strategy, title("Major soil related problems") note("Agroforestry n=38, Conventional n=40" "Multiple answers possible; total=49")) scheme(plotplainblind) blabel(total) legend(rows(2) order(1 "Landslides" 2 "Erosion" 3 "Infertility" 4 "Soil unworkable during dry season" 5 "Other"))

	
/*	
mrtab d2_soilpt_landsl d2_soilpt_eros d2_soilpt_infert d2_soilpt_dryness d2_soilpt_other, by(strategy) include sort
rename (Landslides Erosion Infertility Dryness Other) () 
lab var d2_soilpt_landsl "Landslides"
lab var d2_soilpt_eros "Erosion"
lab var d2_soilpt_infert "Unfertile soils"
lab var d2_soilpt_dryness "Other: dryness (unsp.)"
lab var d2_soilpt_other "Other"

mrgraph hbar d2_soilpt_landsl d2_soilpt_eros d2_soilpt_infert d2_soilpt_dryness d2_soilpt_other, ///
	by(strategy) scheme(plotplain) stat(col) title("Major soil related problems") note("Multiple answers possible" "Agroforestry n=40, Conventional n=38") legend(rows(1) position(6)) blabel(total, format(%9.2fc)) bargap(20) ytitle("Percent of group")
*/
	


	
	
//////////////////////////////////////////////////	
*physical capital
des io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 
/*
io_b_pc1        double  %10.0g                Road system connection to infrastructure [scale 1-6] (to inverse)
io_b_pc2        byte    %10.0g                Road conditions during wet season [yes/no] (to inverse)
io_b_pc3        byte    %10.0g                Easy access to irrigation water [yes/no]
io_b_pc4        byte    %10.0g                Easy access to household water [yes/no]
io_b_pc5        byte    %10.0g                Technical level of irrigation system [1-4]
*/
*done 
*final var immx_b_pc_index

des io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5
swilk io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5
tabstat io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5, by(strategy) statistics(mean sd median iqr) format(%9.3f) nototal columns(statistics) longstub

ktau io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5, stats(taub p)
spearman io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5, stats(rho p) //no correlation above 0.7





ranksum io_b_pc1,by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_pc1 i.strategy
cendif io_b_pc2, by(strategy)


ranksum io_b_pc2,by(strategy)porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_pc2 i.strategy
cendif io_b_pc2, by(strategy)


prtest io_b_pc3,by(strategy) 
prtest io_b_pc4,by(strategy)
robvar io_b_pc5, by(strategy) 
ttest io_b_pc5, by(strategy) 










////////////////////////////////////////////////////
*social capital
des io_b_sc1 io_b_sc2 io_b_sc3 
/*
io_b_sc1        byte    %10.0g                Community group support [rating, 0-4]
io_b_sc2        byte    %10.0g                Community groups - excluding farmers group and savings group [count]
io_b_sc3        double  %10.0g                Participation in community groups [percentage]
*/


*recalc and couble check
egen io_b_sc1_new = rowtotal(g2_reliance_moral g2_reliance_cash g2_reliance_inkind g2_reliance_labour) //commgroup support
g io_b_sc2_new = io_b_sc2
replace io_b_sc2_new = 0 if io_b_sc2==. 
g io_b_sc3_new = io_b_sc3
replace io_b_sc3_new = 0 if io_b_sc3==.
replace io_b_sc3_new = 1 if io_b_sc3_new>1



*normalization and aggregation
norm io_b_sc1_new io_b_sc2_new io_b_sc3_new, method(mmx)
rename immx_b_sc_index immx_b_sc_index_old
egen immx_b_sc_index = rowmean(mmx_io_b_sc1_new mmx_io_b_sc2_new mmx_io_b_sc3_new)
label var immx_b_sc_index "Social capital"

*descr corr and distri	
tabstat io_b_sc1_new io_b_sc2_new io_b_sc3_new, by(strategy) statistics(mean sd median iqr) format(%9.3f) longstub nototal columns(statistics)
ktau io_b_sc1_new io_b_sc2_new io_b_sc3_new, stats(taub p)
spearman io_b_sc1_new io_b_sc2_new io_b_sc3_new, stats(rho p)
swilk io_b_sc1_new io_b_sc2_new io_b_sc3_new
robvar io_b_sc1_new, by(strategy)

*testing
ttest io_b_sc1_new, by(strategy)
robvar io_b_sc2_new, by(strategy)
ttest io_b_sc2_new, by(strategy)

ranksum io_b_sc3_new, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_sc3_new i.strategy
cendif io_b_sc3_new, by(strategy)

*graphs 
 /*
mrtab g2_reliance_moral g2_reliance_cash g2_reliance_inkind g2_reliance_labour, by(strategy) include sort col rcol

mrgraph hbar ///
	g2_reliance_moral g2_reliance_cash g2_reliance_inkind g2_reliance_labour, ///
	by(strategy) stat(col) title("Community group support during crises") note("Agroforestry n=38, Conventional n=40" "Multiple answers possible") scheme(plotplain) includemissing sort descending
	
	legend(rows(1) position(6)) 

graph hbar (mean) ///
	g2_reliance_moral g2_reliance_cash g2_reliance_inkind g2_reliance_labour ///
	, over(strategy, sort(#)  descending) scheme(plotplainblind) ///
	blabel(total, position(outside) format(%9.2fc)) ///
	title("Community group support during crises") note("Agroforestry n=40, Conventional n=38" "Multiple answers possible") ytitle("Fraction of group") ///
	legend(order(1 "Moral support" 2 "Cash donations" 3 "In-kind donations" 4 "Labour support") rows(1) position(6)) ///
	 ylabel(0(0.1)1) yscale(range(0 1)) yline(0(0.1)1, lstyle(grid) lcolor(gs10) lwidth(thin)) ///
	 bargap(20) asyvars intensity(50)
	
	

replace g2_reliance_moral=0 if g2_reliance_moral==.
replace g2_reliance_cash=0 if g2_reliance_cash=0==.
replace g2_reliance_inkind=0 if g2_reliance_inkind==.
replace g2_reliance_labour=0 if g2_reliance_labour==.

rename g2_reliance g2_reliance_tot
*/
	
//////////////////////////////////////////////
*financial capital
***financial capital 
des io_b_fc1 io_b_fc2 io_b_fc3 io_b_fc4 io_b_fc5 
/*
io_b_fc1        byte    %10.0g                Access to formal and informal credits [yes/no]
io_b_fc2        double  %10.0g                Livestock [TLU]
io_b_fc3        double  %10.0g                Size of owned farmland [ha]
io_b_fc4        byte    %10.0g                Farm equipment [count]
io_b_fc5        byte    %10.0g                Side occupations in family [count]
y_div_inv
f_y_pc
*/
*add per capita income
*add income diversity
*final var immx_b_fc_index_old


rename immx_b_fc_index immx_b_fc_index_old
lab var immx_b_fc_index_old "Financial capital OLD"

norm y_div_inv f_y_pc tlu_tot b1_land_op_ha, method(mmx)
egen immx_b_fc_index = rowmean(immx_b_fc1 mmx_tlu_tot mmx_b1_land_op_ha immx_b_fc4 immx_b_fc5 mmx_y_div_inv mmx_f_y_pc) // immx_b_fc2
lab var immx_b_fc_index "Financial capital"




clonevar io_b_fc1_new=io_b_fc1
replace io_b_fc1_new=0 
replace io_b_fc1_new=1 if e1_creditinf==1 | e1_creditform ==1
replace io_b_fc1_new=2 if e1_creditinf==1 & e1_creditform ==1

*description
ktau io_b_fc1_new tlu_tot b1_land_op_ha io_b_fc4 y_div_inv  f_y_pc_mon_mio, stats(taub p)
spearman io_b_fc1_new tlu_tot b1_land_op_ha io_b_fc4 y_div_inv  f_y_pc_mon_mio, star(0.1) 
tabstat io_b_fc1_new tlu_tot b1_land_op_ha io_b_fc4 y_div_inv f_y_pc_mon_mio, by(strategy) statistics(mean sd median iqr) longstub nototal columns(statistics) format(%9.3fc)
swilk io_b_fc1_new tlu_tot b1_land_op_ha io_b_fc4 y_div_inv  f_y_pc_mon_mio

prtest e1_creditinf, by(strategy)
prtest e1_creditform, by(strategy)

ranksum io_b_fc1_new, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_fc1_new i.strategy
cendif io_b_fc1_new, by(strategy)

ranksum tlu_tot, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 tlu_tot i.strategy
cendif tlu_tot, by(strategy)

ranksum b1_land_op_ha, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 b1_land_op_ha i.strategy
cendif b1_land_op_ha, by(strategy)

ranksum io_b_fc4, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_fc4 i.strategy
cendif io_b_fc4, by(strategy)

ranksum y_div_inv, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 y_div_inv i.strategy
cendif y_div_inv, by(strategy)

ranksum f_y_pc_mon_mio, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 f_y_pc_mon_mio i.strategy
cendif f_y_pc_mon_mio, by(strategy)


graph bar (count) ///
	c7_q1_item_1 c7_q1_item_2 c7_q1_item_3 c7_q1_item_4 c7_q1_item_5 c7_q1_item_6 c7_q1_item_7 c7_q1_item_8 c7_q1_item_9 c7_q1_item_10, ///
	by(strategy, title("Community group support during crises") note("Agroforestry n=38, Conventional n=40" "Multiple answers possible")) scheme(plotplainblind) blabel(total) legend(rows(1) order(1 "Moral support" 2 "Cash-support" 3 "In-kind donations" 4 "Labour suppport"))





////////////////////////////////////////////////
*Diversification of farm
des io_b_cd1 io_b_cd2 // 
/*
io_b_cd1        byte    %10.0g                Crop diversity [count]
io_b_cd2        double  %10.0g                Agroforestry [Percentage]
*/
*add livestock diversity
*add crop diversity
*final var immx_b_farmdiv_index



spearman crop_div_inv lvstck_div_inv cd_n_total cd_afshare, pw stats(rho) star(0.1) //only weak to moderately correlated but significant
pwcorr  crop_div_inv lvstck_div_inv cd_n_total cd_afshare, sig //robustness

rename immx_b_cd_index immx_b_cd_index_old
label var immx_b_cd_index_old "Crop diversification OLD"

norm crop_div_inv lvstck_div_inv cd_n_total cd_afshare, method(mmx)
egen immx_b_farmdiv_index = rowmean(mmx_crop_div_inv mmx_lvstck_div_inv mmx_cd_n_total mmx_cd_afshare)
label var immx_b_farmdiv_index "Farm diversification"





*correlations
ktau crop_div_inv lvstck_div_inv cd_n_total b_cd2_100, stats(taub p)
spearman crop_div_inv lvstck_div_inv cd_n_total b_cd2_100, stats(rho p)

*description
tabstat crop_div_inv lvstck_div_inv cd_n_total b_cd2_100, by(strategy) statistics(mean sd median iqr) longstub nototal columns(statistics) format(%9.3fc)
swilk crop_div_inv lvstck_div_inv cd_n_total b_cd2_100


ranksum crop_div_inv, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 crop_div_inv i.strategy
cendif crop_div_inv, by(strategy)

ranksum lvstck_div_inv, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 lvstck_div_inv i.strategy
cendif lvstck_div_inv, by(strategy)

ranksum cd_n_total, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 cd_n_total i.strategy
cendif cd_n_total, by(strategy)

ranksum b_cd2_100, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 b_cd2_100 i.strategy
cendif b_cd2_100, by(strategy)







////////////////////////////////////////////////
***human capital 
des io_b_hc1 io_b_hc2 io_b_hc3
/*
io_b_hc1        double  %10.0g                General health of family [scale 1-10]
io_b_hc2        double  %10.0g                Dependency ratio [Percentage] (to inverse)
io_b_hc3        byte    %10.0g                Education of household head [years]
*/
*add new depratio
*add most educated member variable

rename immx_b_hc_index immx_b_hc_index_old
lab var immx_b_hc_index_old "Human capital OLD"

norm h_depratio h_edu_max h_size, method(mmx)
replace mmx_h_depratio = 1 - mmx_h_depratio

egen immx_b_hc_index = rowmean(immx_b_hc1 immx_b_hc3 mmx_h_depratio mmx_h_edu_max mmx_h_size) //immx_b_hc2 not included
label var immx_b_hc_index "Human capital"



*correlations
ktau io_b_hc1 h_depratio hh_edu h_edu_max h_size, stats(taub p)
spearman io_b_hc1 h_depratio hh_edu h_edu_max h_size, stats(rho p)

*description
tabstat io_b_hc1 h_depratio hh_edu h_edu_max h_size, by(strategy) statistics(mean sd median iqr) longstub nototal columns(statistics) format(%9.3fc)
swilk io_b_hc1 h_depratio hh_edu h_edu_max h_size

*variances
robvar hh_edu, by(village)
robvar h_size, by(village) //unewelch

*testing
ttest hh_edu, by(village)
ttest h_size, by(village) une welch

ranksum io_b_hc1, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_b_hc1 i.strategy
cendif io_b_hc1, by(strategy)

ranksum h_depratio, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 h_depratio i.strategy
cendif h_depratio, by(strategy)

ranksum h_edu_max, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 h_edu_max i.strategy
cendif h_edu_max, by(strategy)






/////////////////////////////////////////////////
*total buffer capacity (weighted arithmetic mean)
g res_cap_buffer= ((immx_b_nc_index*5) + (immx_b_fc_index*7) + (immx_b_pc_index*5) + (immx_b_hc_index*5) + (immx_b_sc_index*3) + (immx_b_farmdiv_index*4)) / (5+7+5+5+3+4)
label var res_cap_buffer "Buffer capacity"
label var immx_b_nc_index "Natural capital"
label var immx_b_sc_index "Social capital"
label var immx_b_pc_index "Physical capital"

*descriptives
tabstat immx_b_nc_index immx_b_pc_index immx_b_sc_index immx_b_hc_index immx_b_fc_index immx_b_farmdiv_index res_cap_buffer, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f)
tabstat res2, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f)

*normality
swilk immx_b_nc_index immx_b_pc_index immx_b_sc_index immx_b_hc_index immx_b_fc_index immx_b_farmdiv_index res_cap_buffer //all normal at p<0.1 except immx_b_farmdiv_index (bimodal)

*equality of variances
robvar immx_b_nc_index, by(strategy)
robvar immx_b_pc_index, by(strategy) //une
robvar immx_b_sc_index, by(strategy)
robvar immx_b_hc_index, by(strategy) //une
robvar immx_b_fc_index, by(strategy) 
*farm diversity
robvar res_cap_buffer, by(strategy) 

*testingttest immx_b_nc_index, by(strategy)
ttest immx_b_nc_index, by(strategy)
ttest immx_b_pc_index, by(strategy) une welch
ttest immx_b_sc_index, by(strategy) 
ttest immx_b_hc_index, by(strategy) une welch
ttest immx_b_fc_index, by(strategy)
ttest res_cap_buffer, by(strategy) 


ranksum immx_b_farmdiv_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2)


*normal io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3
*inverse io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio

alpha io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio, item std reverse(io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio) //0.0161	0.3219

*mcdonald's omega https://ideas.repec.org/c/boc/bocode/s458861.html 

omega io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio, reverse(io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio) //unstandardized - convergence not achieved

omega io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio // no reverse


preserve
drop mmx_*
norm io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio, method(mmx)
omega mmx_io_b_nc1 mmx_io_b_pc1 mmx_io_b_pc2 mmx_io_b_pc3 mmx_io_b_pc4 mmx_io_b_pc5 mmx_io_b_sc1_new mmx_io_b_sc2_new mmx_io_b_sc3_new mmx_io_b_fc1 mmx_tlu_tot mmx_b1_land_op_ha mmx_io_b_fc4 mmx_io_b_fc5 mmx_y_div_inv mmx_f_y_pc mmx_crop_div_inv mmx_lvstck_div_inv mmx_cd_n_total mmx_cd_afshare mmx_h_edu_max mmx_h_size mmx_io_b_hc1 mmx_io_b_hc3 mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio, reverse(mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio) noreverse() //standardized Number of items in the scale:	29 Scale reliability coefficient:	0.8137
restore



*https://www.researchgate.net/post/If_Cronbachs_alpha_is_low_and_I_do_not_want_to_abandon_my_questionnaire_what_needs_to_be_done
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1321890-guttman-s-lambda2-for-estimating-reliability-in-stata
*https://www.researchgate.net/publication/229682604_An_empirical_comparison_of_coefficient_alpha_Guttman's_Lambda-2_and_MSPLIT_maximized_split-half_reliability_estimates 
*https://files.eric.ed.gov/fulltext/ED542021.pdf 
*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4880791/
*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4205511/ 
*https://stats.stackexchange.com/questions/365109/mcdonalds-omega-assumptions-coefficients-and-interpretation
*https://doi.apa.org/doiLanding?doi=10.1037%2Fmet0000144  
*cutoff values 
*https://www.researchgate.net/publication/325975742_Reliability_Population_Classification_and_Weighting_in_Multidimensional_Poverty_Measurement_A_Monte_Carlo_Study 
*Revelle, W., & Zinbarg, R. (2009). Coefficients alpha, beta, omega, and the glb: Comments on sijtsma. Psy-chometrika, 74(1), 145–154. 






*******************************************************
***		Capacity for learning and adaptation	*******
*******************************************************
*recalculate each index for each subdimension for both dimensions - do tables and tests, make word tables start describing.
*check which to inverse and which not ...  


////////////////////////////////////////
*knowledge of threats and opportunities
swilk io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4
replace io_c_kto2=0 if io_c_kto2==.

*descriptives
replace io_c_kto3 =1 if io_c_kto3>1
tabstat io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 , by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal
ktau io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4, stats(taub p)
pwcorr io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4, sig

*proportiontest
prtest  io_c_kto1, by(strategy)
prtest  io_c_kto2, by(strategy)
prtest  io_c_kto3, by(strategy)
prtest  io_c_kto4, by(strategy)

ranksum io_c_kto3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_c_kto3 i.strategy
cendif io_c_kto3, by(strategy)

ranksum io_c_kto4, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_c_kto4 i.strategy
cendif io_c_kto4, by(strategy)




////////////////////////////////
*Reflective and Shared Learning
replace io_c_rsl1 =0 if io_c_rsl1==. 
replace io_c_rsl2=0 if io_c_rsl2==. 
replace io_c_rsl3 =0 if io_c_rsl3==. 
swilk io_c_rsl1 io_c_rsl2 io_c_rsl3 
tabstat io_c_rsl1 io_c_rsl2 io_c_rsl3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

ktau io_c_rsl1 io_c_rsl2 io_c_rsl3, stats(taub p)
pwcorr io_c_rsl1 io_c_rsl2 io_c_rsl3, sig

prtest io_c_rsl1, by(strategy)
prtest io_c_rsl2, by(strategy)
robvar io_c_rsl3, by(strategy)
ttest io_c_rsl3, by(strategy) une welch



//////////////////////////////
*Openness to change
replace io_c_op1 =0 if io_c_op1==. 
replace io_c_op2=0 if io_c_op2==. 
replace io_c_op3 =0 if io_c_op3==.
replace io_c_op4=0 if io_c_op4==. 
swilk io_c_op1 io_c_op2 io_c_op3 io_c_op4

tabstat io_c_op1 io_c_op2 io_c_op3 io_c_op4, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal



ktau io_c_op1 io_c_op2 io_c_op3 io_c_op4, stats(taub p)
spearman io_c_op1 io_c_op2 io_c_op3 io_c_op4, stats(rho p)

prtest io_c_op1, by(strategy)
prtest io_c_op2, by(strategy)

ranksum io_c_op3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_c_op3 i.strategy
cendif io_c_op3, by(strategy)

ranksum io_c_op4, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_c_op4 i.strategy
cendif io_c_op4, by(strategy)


*Functioning Feedback mechanisms
replace io_c_ffm1=0 if io_c_ffm1==. 
replace io_c_ffm3=0 if io_c_ffm3==. 
replace io_c_ffm2=0 if io_c_ffm2==. 
replace io_c_ffm4=0 if io_c_ffm4==. 
swilk io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 

tabstat io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal



prtest io_c_ffm1, by(strategy)
prtest io_c_ffm2, by(strategy)
prtest io_c_ffm3, by(strategy)
prtest io_c_ffm4, by(strategy)

ktau io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4, stats(taub p)
spearman io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4, stats(rho p)



*traditional ecological knowledge
ktau io_c_tek1 io_c_tek2 io_c_tek3, stats(taub p)
spearman io_c_tek1 io_c_tek2 io_c_tek3, stats(rho p)
pwcorr io_c_tek1 io_c_tek2 io_c_tek3, sig




tabstat io_c_tek1 io_c_tek2 io_c_tek3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

swilk io_c_tek1 io_c_tek2 io_c_tek3

ttest io_c_tek1, by(strategy) une welch
prtest io_c_tek2, by(strategy)

ranksum io_c_tek3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_c_tek3 i.strategy
cendif io_c_tek3, by(strategy)




*shared vision
replace io_c_sv1=0 if io_c_sv1==. 
replace io_c_sv2=0 if io_c_sv2==. 
replace io_c_sv3=0 if io_c_sv3==. 

ktau io_c_sv1 io_c_sv2 io_c_sv3, stats(taub p)
spearman io_c_sv1 io_c_sv2 io_c_sv3, stats(rho p)
pwcorr io_c_sv1 io_c_sv2 io_c_sv3, sig

tabstat io_c_sv1 io_c_sv2 io_c_sv3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

prtest io_c_sv1, by(strategy)
prtest io_c_sv2, by(strategy)
prtest io_c_sv3, by(strategy)

egen index_sv = rowmean(io_c_sv1 io_c_sv2 io_c_sv3)
label var index_sv "Shared vision"




graph bar (count) ///
	d4_swc_1 d4_swc_2 d4_swc_3 d4_swc_4 d4_swc_5 d4_swc_6 d4_swc_7 d4_swc_8 d4_swc_9 d4_swc_10 ///
	, by(strategy, title("Soil and water conservation techniques") note("Agroforestry n=40 Conventional n=38")) scheme(plotplainblind) ///
	legend(cols(3) label(1 "Terracing") label(2 "Contour farming") label(3 "Mixed cropping system") label(4 "Crop rotation") label(5 "Adding manure") label(6 "Natural mulching") label(7 "Plastic covering") label(8 "Agroforestry") label(9 "Soil bounds (trees, stones)") label(10 "Other")) ///
	blabel(total, position(base))
	
foreach var of varlist d4_swc_1 d4_swc_2 d4_swc_3 d4_swc_4 d4_swc_5 d4_swc_6 d4_swc_7 d4_swc_8 d4_swc_9 d4_swc_10 {
	replace `var'=0 if `var'==. 
}
replace d4_swc_8=1 if strategy==1
	
label var d4_swc_1 "Terracing"
label var d4_swc_2 "Contour farming"
label var d4_swc_3 "Inter-/multi-cropping"
label var d4_swc_4 "Crop rotation"
label var d4_swc_5 "Adding manure"
label var d4_swc_6 "Natural mulching"
label var d4_swc_7 "Plastic covering"
label var d4_swc_8 "Agroforestry"
label var d4_swc_9 "Soil bounds (trees, stones)"
label var d4_swc_10 "Other"

mrgraph hbar ///
	d4_swc_1 d4_swc_2 d4_swc_3 d4_swc_4 d4_swc_5 d4_swc_6 d4_swc_7 d4_swc_8 d4_swc_9 d4_swc_10 ///
	, by(strategy, seperate) title("Soil and water conservation techniques") note("Multiple answers possible" "Agroforestry n=40 Conventional n=38") scheme(plotplainblind) stat(col) bargap(20) legend(rows(1) position(6)) blabel(total, size(vsmall) format(%9.2fc)) includemissing ytitle("Percent of group") sort descending
	
	

omega io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3

snapshot save, label("before1")
foreach var of varlist io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3 {
	replace `var'=0 if `var'==. 
}
drop mmx_*
norm io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio, method(mmx)
omega mmx_io_b_nc1 mmx_io_b_pc1 mmx_io_b_pc2 mmx_io_b_pc3 mmx_io_b_pc4 mmx_io_b_pc5 mmx_io_b_sc1_new mmx_io_b_sc2_new mmx_io_b_sc3_new mmx_io_b_fc1 mmx_tlu_tot mmx_b1_land_op_ha mmx_io_b_fc4 mmx_io_b_fc5 mmx_y_div_inv mmx_f_y_pc mmx_crop_div_inv mmx_lvstck_div_inv mmx_cd_n_total mmx_cd_afshare mmx_h_edu_max mmx_h_size mmx_io_b_hc1 mmx_io_b_hc3 mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio, reverse(mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio) noreverse() //standardized Number of items in the scale:	29 Scale reliability coefficient:	0.8137
norm io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3, method(mmx)
omega mmx_io_c_kto1 mmx_io_c_kto2 mmx_io_c_kto3 mmx_io_c_kto4 mmx_io_c_rsl1 mmx_io_c_rsl2 mmx_io_c_rsl3 mmx_io_c_op1 mmx_io_c_op2 mmx_io_c_op3 mmx_io_c_op4 mmx_io_c_ffm1 mmx_io_c_ffm2 mmx_io_c_ffm3 mmx_io_c_ffm4 mmx_io_c_tek1 mmx_io_c_tek2 mmx_io_c_tek3 mmx_io_c_sv1 mmx_io_c_sv2 mmx_io_c_sv3 //




g res_cap_learnadapt = ((immx_c_kto_index*4) + (immx_c_rsl_index*3) + (immx_c_op_index*4) + (immx_c_ffm_index*4) + (immx_c_tek_index*3) + (immx_c_sv_index*3)) / (4+3+4+4+3+3)

label var res_cap_learnadapt "Capacity for learning and adaptation"
label var immx_c_kto_index "Knowledge of threat and opportunities"
label var immx_c_rsl_index "Reflective and shared learning"
label var immx_c_op_index "Openness to change"
label var immx_c_ffm_index "Functioning feedback mechanisms"
label var immx_c_tek_index "Traditional ecological knowledge"
label var immx_c_sv_index "Shared vision"


*testing of subdimensions
ranksum immx_c_kto_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_c_kto_index i.strategy
cendif immx_c_kto_index, by(strategy)


ranksum immx_c_op_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_c_op_index i.strategy
cendif immx_c_op_index, by(strategy)



ranksum immx_c_tek_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_c_tek_index i.strategy
cendif immx_c_tek_index, by(strategy)


ranksum immx_c_sv_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_c_sv_index i.strategy
cendif immx_c_sv_index, by(strategy)

ranksum res_cap_learnadapt, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 res_cap_learnadapt i.strategy







***********************************************
***		Capacity for self-organization	*******
***********************************************

foreach var of varlist io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3 {
	replace `var'=0 if `var'==. 
}
drop mmx_*
drop mmx_*
norm io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio, method(mmx)
omega mmx_io_b_nc1 mmx_io_b_pc1 mmx_io_b_pc2 mmx_io_b_pc3 mmx_io_b_pc4 mmx_io_b_pc5 mmx_io_b_sc1_new mmx_io_b_sc2_new mmx_io_b_sc3_new mmx_io_b_fc1 mmx_tlu_tot mmx_b1_land_op_ha mmx_io_b_fc4 mmx_io_b_fc5 mmx_y_div_inv mmx_f_y_pc mmx_crop_div_inv mmx_lvstck_div_inv mmx_cd_n_total mmx_cd_afshare mmx_h_edu_max mmx_h_size mmx_io_b_hc1 mmx_io_b_hc3 mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio, reverse(mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio) noreverse() //standardized Number of items in the scale:	29 Scale reliability coefficient:	0.8137
norm io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3, method(mmx)
omega mmx_io_c_kto1 mmx_io_c_kto2 mmx_io_c_kto3 mmx_io_c_kto4 mmx_io_c_rsl1 mmx_io_c_rsl2 mmx_io_c_rsl3 mmx_io_c_op1 mmx_io_c_op2 mmx_io_c_op3 mmx_io_c_op4 mmx_io_c_ffm1 mmx_io_c_ffm2 mmx_io_c_ffm3 mmx_io_c_ffm4 mmx_io_c_tek1 mmx_io_c_tek2 mmx_io_c_tek3 mmx_io_c_sv1 mmx_io_c_sv2 mmx_io_c_sv3 //
norm io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3, method(mmx)
omega io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3


des io_s_i1 io_s_i2 io_s_i3 
des io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 
des io_s_ss1 io_s_ss2 io_s_ss3//add homegarden
des io_s_opp1 io_s_opp2 io_s_opp3

egen immx_s_cn_index = rowmean(immx_s_cn1 immx_s_cn2 immx_s_cn3 immx_s_cn4)

g io_s_ss3 = b1_ownarea_homeg
replace io_s_ss3 = 0 if  b1_ownarea_homeg ==.
norm io_s_ss3, method(mmx)
rename immx_s_ss_index immx_s_ss_index_old
egen immx_s_ss_index = rowmean(immx_s_ss1 immx_s_ss2 mmx_io_s_ss3)

norm h1_pc1 h1_pc2 h1_pc3, method(mmx)
g inv_opp_1 = 1-mmx_h1_pc1
g inv_opp_2 = 1-mmx_h1_pc2
g inv_opp_3 = 1-mmx_h1_pc3 
egen mmx_io_s_opp = rowmean(inv_opp_3 inv_opp_2 inv_opp_1)

spearman io_s_i1 io_s_i2 io_s_i3, star(0.1)
spearman h1_pc1 h1_pc2 h1_pc3, star(0.1)
spearman io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 , star(0.1)
spearman io_s_ss3 io_s_ss1 io_s_ss2, star(0.1)

g res_cap_selforg = ((immx_s_inst_index*3)+(immx_s_cn_index*4)+(immx_s_ss_index*3)*(mmx_io_s_opp*3)) / (3+4+3+3)

label var immx_s_inst_index "Institutions"
label var immx_s_cn_index "Cooperations and network"
label var immx_s_ss_index "Self-sufficiency of farming"
label var mmx_io_s_opp "Political capital"
label var res_cap_selforg "Capacity for self organisation"


*institutions
replace io_s_i1=0 if io_s_i1==.
replace io_s_i2=0 if io_s_i2==.
replace io_s_i3=0 if io_s_i3==. 

swilk io_s_i1 io_s_i2 io_s_i3
ktau io_s_i1 io_s_i2 io_s_i3, stats(taub p)
spearman io_s_i1 io_s_i2 io_s_i3, stats(rho p)

tabstat io_s_i1 io_s_i2 io_s_i3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

ranksum io_s_i1, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_i1 i.strategy, wlsiter(100)
cendif io_s_i1, by(strategy)

prtest io_s_i2, by(strategy)
prtest io_s_i3, by(strategy)



*Cooperations and network

replace io_s_cn1=0 if io_s_cn1==.
replace io_s_cn2=0 if io_s_cn2==.
replace io_s_cn3=0 if io_s_cn3==. 
replace io_s_cn4=0 if io_s_cn4==.

swilk io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 
ktau io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 , stats(taub p)
spearman io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 , stats(rho p)

tabstat io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

ranksum io_s_cn1, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_cn1 i.strategy, wlsiter(1000)
cendif io_s_cn1, by(strategy)

robvar io_s_cn2, by(strategy)
ttest io_s_cn2, by(strategy)

ranksum io_s_cn3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_cn3 i.strategy, wlsiter(1000)
cendif io_s_cn3, by(strategy)

ranksum io_s_cn4, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_cn4 i.strategy, wlsiter(1000)
cendif io_s_cn4, by(strategy)


*Self-sufficiency of farming
swilk io_s_ss1 io_s_ss2 io_s_ss3 //none 

ktau io_s_ss1 io_s_ss2 io_s_ss3, stats(taub p)
spearman io_s_ss1 io_s_ss2 io_s_ss3, stats(rho p)

tabstat io_s_ss1 io_s_ss2 io_s_ss3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

ranksum io_s_ss1, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_ss1 i.strategy, wlsiter(1000)
cendif io_s_ss1, by(strategy)

ranksum io_s_ss2, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_ss2 i.strategy, wlsiter(1000)
cendif io_s_ss2, by(strategy)

ranksum io_s_ss3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_ss3 i.strategy
cendif io_s_ss3, by(strategy)



*Political capital
des io_s_opp1 io_s_opp2 io_s_opp3
swilk io_s_opp1 io_s_opp2 io_s_opp3

ktau io_s_opp1 io_s_opp2 io_s_opp3, stats(taub p)
spearman io_s_opp1 io_s_opp2 io_s_opp3, stats(rho p)

tabstat io_s_opp1 io_s_opp2 io_s_opp3, by(strategy) statistics(mean sd median iqr) columns(statistics) format(%9.3f) longstub nototal

ranksum io_s_opp1, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_opp1 i.strategy
cendif io_s_opp1, by(strategy)


ranksum io_s_opp2, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_opp2 i.strategy
cendif io_s_opp2, by(strategy)


ranksum io_s_opp3, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 io_s_opp3 i.strategy
cendif io_s_opp3, by(strategy)



***********************************************
***		Capacity for self-organization	*******
***********************************************

ranksum immx_s_inst_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_s_inst_index i.strategy
cendif immx_s_inst_index, by(strategy)

ranksum immx_s_cn_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_s_cn_index i.strategy
cendif immx_s_cn_index, by(strategy)

ranksum immx_s_ss_index, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 immx_s_ss_index i.strategy
cendif immx_s_ss_index, by(strategy)

ranksum mmx_io_s_opp, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 mmx_io_s_opp i.strategy
cendif mmx_io_s_opp, by(strategy)


*s_institutions
des io_s_i1 io_s_i2 io_s_i3 
label define marketing1 1 "1. Direct contract (as individ.)" 2 "2. Traders (as individ.)" 3 "3. Direct contract (as farmer group)" 4 "4. Traders (as farmer group)"
label values io_s_i1 marketing1
label var io_s_i1 ""

catplot ///
	io_s_i1 ///
	,over(strategy) scheme(plotplainblind) ytitle("% of group") ///
	title("Marketing methods") note("Agroforestry n=40 Conventional n=38") ///
	 percent blabel(bar, position(base) format(%9.2fc)) var1opts(gap(1)) ///
	 legend(rows(1) position(6)) ///
	 ylabel(0(10)50) yscale(range(0 50)) yline(0(5)50, lstyle(grid) lcolor(gs10) lwidth(thin)) 




foreach var of varlist io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3 io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3 {
	replace `var'=0 if `var'==. 
}
drop mmx_*
norm io_b_nc1 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_sc1_new io_b_sc2_new io_b_sc3_new io_b_fc1 tlu_tot b1_land_op_ha io_b_fc4 io_b_fc5 y_div_inv f_y_pc crop_div_inv lvstck_div_inv cd_n_total cd_afshare h_edu_max h_size io_b_hc1 io_b_hc3 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 h_depratio io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3 io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3, method(mmx)

omega mmx_io_b_nc1 mmx_io_b_pc1 mmx_io_b_pc2 mmx_io_b_pc3 mmx_io_b_pc4 mmx_io_b_pc5 mmx_io_b_sc1_new mmx_io_b_sc2_new mmx_io_b_sc3_new mmx_io_b_fc1 mmx_tlu_tot mmx_b1_land_op_ha mmx_io_b_fc4 mmx_io_b_fc5 mmx_y_div_inv mmx_f_y_pc mmx_crop_div_inv mmx_lvstck_div_inv mmx_cd_n_total mmx_cd_afshare mmx_h_edu_max mmx_h_size mmx_io_b_hc1 mmx_io_b_hc3 mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio mmx_io_c_kto1 mmx_io_c_kto2 mmx_io_c_kto3 mmx_io_c_kto4 mmx_io_c_rsl1 mmx_io_c_rsl2 mmx_io_c_rsl3 mmx_io_c_op1 mmx_io_c_op2 mmx_io_c_op3 mmx_io_c_op4 mmx_io_c_ffm1 mmx_io_c_ffm2 mmx_io_c_ffm3 mmx_io_c_ffm4 mmx_io_c_tek1 mmx_io_c_tek2 mmx_io_c_tek3 mmx_io_c_sv1 mmx_io_c_sv2 mmx_io_c_sv3 io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_ss3 inv_opp_1 inv_opp_2 inv_opp_3, reverse(mmx_io_b_nc2 mmx_io_b_nc3 mmx_io_b_nc4 mmx_io_b_nc5 mmx_h_depratio)





***********************************************
***		Livelihood resilience 			*******
***********************************************

g res = ///
	(((immx_b_nc_index*5) + (immx_b_fc_index*7) + (immx_b_pc_index*5) + (immx_b_hc_index*5) + (immx_b_sc_index*3) + (immx_b_farmdiv_index*4)) + ///
	((immx_c_kto_index*4) + (immx_c_rsl_index*3) + (immx_c_op_index*4) + (immx_c_ffm_index*4) + (immx_c_tek_index*3) + (immx_c_sv_index*3)) + ///
	((immx_s_inst_index*3)+(immx_s_cn_index*4)+(immx_s_ss_index*3)*(mmx_io_s_opp*3))) / ///
	((5+7+5+5+3+4) + (4+3+4+4+3+3) + (3+4+3+3))
	
g res2 = ///
	(((immx_b_nc_index*5) + (immx_b_fc_index*7) + (immx_b_pc_index*5) + (immx_b_hc_index*5) + (immx_b_sc_index*3) + (immx_b_farmdiv_index*4)) + ///
	((immx_c_kto_index*4) + (immx_c_rsl_index*3) + (immx_c_op_index*4) + (immx_c_ffm_index*4) + (immx_c_tek_index*3)) + ///
	((immx_s_inst_index*3)+(immx_s_cn_index*4)+(immx_s_ss_index*3)*(mmx_io_s_opp*3))) / ///
	((5+7+5+5+3+4) + (4+3+4+4+3) + (3+4+3+3))
	
label var res "Resilience capacity"
label var res2 "Resilience capacity"

drop res2




////////////////////////////////////
mvtest normality  immx_b_nc_index immx_b_fc_index immx_b_pc_index immx_b_hc_index immx_b_sc_index immx_b_farmdiv_index immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp

alpha immx_b_nc_index immx_b_fc_index immx_b_pc_index immx_b_hc_index immx_b_sc_index immx_b_farmdiv_index immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp, asis item
	
spearman immx_b_nc_index immx_b_fc_index immx_b_pc_index immx_b_hc_index immx_b_sc_index immx_b_farmdiv_index immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index immx_s_inst_index immx_s_cn_index immx_s_ss_index mmx_io_s_opp, star(0.1) print(0.1)
	


*normality
swilk res2 //learnadapt nonpara

*equality of variance
robvar res2, by(strategy)

*ttest
ttest res2, by(strategy)

*power calculations
*buffercapacity
power twomeans .5300477	.4992349, n1(40) n2(38) sd1(.0703644) sd2(.0598372) alpha(0.1) //0.6699
power twomeans 0.53	0.499, n1(40) n2(38) sd1(0.07) sd2(0.06) alpha(0.05) //0.5464
power twomeans 0.53	0.499, n1(40) n2(38) sd1(0.07) sd2(0.06) alpha(0.01) //0.3020

*capacity for learning and adaptation
power twomeans .6282138	.4750931, n1(40) n2(38) sd1(.1255437) sd2(.1033841) alpha(0.1) //1.000
power twomeans .6282138	.4750931, n1(40) n2(38) sd1(.1255437) sd2(.1033841) alpha(0.05) //0.999
power twomeans .6282138	.4750931, n1(40) n2(38) sd1(.1255437) sd2(.1033841) alpha(0.01) //0.999

*resilience capacity
power twomeans .59547	.4735311 , n1(40) n2(38) sd1(.0719896) sd2(.0623789) alpha(0.1) //1.000
power twomeans .59547	.4735311 , n1(40) n2(38) sd1(.0719896) sd2(.0623789) alpha(0.05)  //0.999
power twomeans .59547	.4735311 , n1(40) n2(38) sd1(.0719896) sd2(.0623789) alpha(0.01) //0.999





*correlations //no correlation found
g f5_wellbeing_rev = 7-f5_wellbeing

g new_f5_wellb_rev=.
replace new_f5_wellb_rev=0 			if f5_wellbeing_rev==0
replace new_f5_wellb_rev=0.16667 	if f5_wellbeing_rev==1
replace new_f5_wellb_rev=0.33333 	if f5_wellbeing_rev==2
replace new_f5_wellb_rev=0.5 		if f5_wellbeing_rev==3
replace new_f5_wellb_rev=0.66667 	if f5_wellbeing_rev==4
replace new_f5_wellb_rev=0.83333 	if f5_wellbeing_rev==5
replace new_f5_wellb_rev=1 			if f5_wellbeing_rev==6

g new_f6_res=0
replace new_f6_res=0.0	 if f4_resilience==0
replace new_f6_res=0.1	 if f4_resilience==1
replace new_f6_res=0.2	 if f4_resilience==2
replace new_f6_res=0.3	 if f4_resilience==3
replace new_f6_res=0.4	 if f4_resilience==4
replace new_f6_res=0.5	 if f4_resilience==5
replace new_f6_res=0.6	 if f4_resilience==6
replace new_f6_res=0.7	 if f4_resilience==7
replace new_f6_res=0.8	 if f4_resilience==8
replace new_f6_res=0.9	 if f4_resilience==9
replace new_f6_res=1	 if f4_resilience==10

two scatter new_f6_res new_f5_wellb_rev, jitter(20) || lfit new_f6_res new_f5_wellb_rev

swilk f5_wellbeing_rev f4_resilience
robvar f5_wellbeing_rev, by(strategy) //equal 
robvar f4_resilience, by(strategy) //equal

norm f4_resilience f5_wellbeing_rev, method(mmx)
tabstat f4_resilience f5_wellbeing_rev, by(strategy) statistics(mean sd median iqr) column(statistics) longstub nototal

ktau f3_cope_combinedpos2 f5_wellbeing_rev, stats(taub p)
ktau f_y_tot f5_wellbeing_rev, stats(taub p)
pbis f3_cope_combinedpos2 f5_wellbeing_rev
spearman f3_cope_combinedpos2 f5_wellbeing_rev

cls
ktau f3_cope_combinedpos2 f4_resilience, stats(taub p)
ktau f_y_tot f4_resilience, stats(taub p)
pbis f3_cope_combinedpos2 f4_resilience
spearman f3_cope_combinedpos2 f4_resilience

cls
ranksum f4_resilience, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 f4_resilience i.strategy
cendif f4_resilience, by(strategy)

ranksum f5_wellbeing_rev, by(strategy) porder
scalar define U1 = r(sum_obs)-r(N_1)*(r(N_1)+1)/2
scalar define U2 = r(N_1)*r(N_2)-U1
display _newline ///
in smcl as text "U1 = " as result U1 _newline ///
in smcl as text "U2 = " as result U2 _newline ///
in smcl as text "U  = " as result min(U1,U2) 
qreg2 f5_wellbeing_rev i.strategy
cendif f5_wellbeing_rev, by(strategy)

median f5_wellbeing_rev, by(strategy)
ranksum f5_wellbeing_rev, by(strategy) porder
median f4_resilience, by(strategy) 
ranksum f4_resilience, by(strategy) porder


ktau f4_resilience f5_wellbeing_rev , stats(taub p)
ktau res2 res_cap_buffer res_cap_learnadapt res_cap_selforg f5_wellbeing_rev f4_resilience, stats(taub p) //sample 
ktau res2 res_cap_buffer res_cap_learnadapt res_cap_selforg f5_wellbeing_rev f4_resilience if strategy==1, stats(taub p) 
ktau res2 res_cap_buffer res_cap_learnadapt res_cap_selforg f5_wellbeing_rev f4_resilience if strategy==2, stats(taub p)

tempfile g1 g2 
stripplot ///
	f5_wellbeing, ///
	by(strategy, compact title("Q: Wellbeing compared to neighbours") ) scheme(plottigblind) legend(off) ytitle("Rating [1-6=best]") vertical cumul box center center

tab res2 f5_wellbeing, chi2
mrtab res2 f4_resilience,  warn

ktau res2 f5_wellbeing if strategy==1, stats(taub p)
spearman res2 f5_wellbeing if strategy==1, stats(rho p)


two scatter f5_wellbeing f4_resilience, by(strategy) || lfit f5_wellbeing f4_resilience, by(strategy) scheme(plottigblind)

qplot f5_wellbeing f4_resilience, by(strategy, compact) aspect(1) scheme(plottigblind)

graph box ///
f5_wellbeing f4_resilience, by(strategy, compact) aspect(1) scheme(plottigblind)
