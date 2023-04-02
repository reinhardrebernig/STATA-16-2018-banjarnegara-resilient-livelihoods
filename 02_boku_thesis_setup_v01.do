
*setup



***
use "1 Input\boku_protocol_tomerge.dta"
merge 1:1 adm_code_boku_1 using "1 Input\ugm_protocol_tomerge.dta", nogen
save "1 Input\boku_ugm_combined_master.dta", replace
save "1 Input\boku_ugm_combined_edit_001.dta", replace

***
qui labone,nrow(1)
drop if adm_code_boku_1 == "Final code"
qui destring, replace
qui compress
drop if adm_1 != 1

***

merge 1:1 adm_code_boku_1 using "1 Input\boku_numberofcrops.dta", nogen
merge 1:1 adm_code_boku_1 using "1 Input\ugm_shareofy.dta", nogen
merge 1:1 adm_code_boku_1 using "1 Input\res_index_tomerge.dta"
replace village = "Penanggungan" if village != "Leksana"

*drop non congruent observations
drop if adm_1 != 1

count // 78 obs
count if adm_1 == 1 //78 obs
tab village //38 Leksana Obs - 40 Penanggungan Obs
qui destring, replace
qui compress
sort adm_code_ugm_1 village
replace e1_creditform = 1 if e1_creditform ==2

snapshot save, label("before delete") //dropping vars that are not part of the master raw dataset
drop cr_seas_yn_cabbage cr_seas_yn_peanut cr_seas_yn_tomato cr_seas_yn_chilli cr_seas_yn_maize cr_seas_yn_celery cr_seas_yn_bunjis cr_seas_yn_cauliflower cr_seas_yn_carrot cr_seas_yn_babybeans cr_seas_yn_sawi cr_seas_yn_mustardbean cr_seas_yn_redbeans cr_seas_yn_greenbeans cr_seas_yn_scallion cr_seas_yn_lettuce cr_seas_yn_radish cr_seas_yn_pakchoy cr_seas_yn_lobak cr_seas_yn_pumpkin cr_seas_yn_spinach cr_seas_yn_ranti cr_seas_yn_sweetpotato v25 cr_seas_yn_potato cr_seas_yn_strawberry cr_perenn_yn_coffee cr_perenn_yn_guava cr_perenn_yn_papaya cr_perenn_yn_cassava cr_perenn_yn_banana cr_perenn_yn_orange cr_perenn_yn_snakefruit cr_perenn_yn_elephantgrass cr_perenn_yn_tea cr_perenn_yn_jackfruit cr_perenn_yn_taro cr_perenn_yn_tabacco cr_perenn_yn_grassland cr_valuetrees_yn_forest cr_valuetrees_yn_albicia cr_valuetrees_yn_suren cr_valuetrees_yn_pines cr_valuetrees_yn_puspa cr_valuetrees_yn_bamboo cr_valuetrees_yn_caleandra cr_valuetrees_yn_taesiah cr_valuetrees_yn_eukalyptus cr_valuetrees_yn_manisjangan cr_valuetrees_yn_samanea cr_valuetrees_yn_kalpi cr_valuetrees_yn_kasiar cr_valuetrees_yn_talas cr_valuetrees_yn_kina cd_n_seas cd_n_perenn cd_n_valuetrees cd_n_total cd_afshare cd_hasagrofyn landuse_fertil landuse_pest f_y_seasc_100 f_y_perennc_100 f_y_ah_100 f_y_fish_100 f_y_offnonf_100 f_y_seasc_perc f_y_perennc_perc f_y_ah_perc f_y_fish_perc f_y_offnonf_perc io_b_nc1 io_b_nc2 io_b_nc3 io_b_nc4 io_b_nc5 io_b_fc1 io_b_fc2 io_b_fc3 io_b_fc4 io_b_fc5 io_b_pc1 io_b_pc2 io_b_pc3 io_b_pc4 io_b_pc5 io_b_hc1 io_b_hc2 io_b_hc3 io_b_sc1 io_b_sc2 io_b_sc3 io_b_cd1 io_b_cd2 io_c_kto1 io_c_kto2 io_c_kto3 io_c_kto4 io_c_rsl1 io_c_rsl2 io_c_rsl3 io_c_op1 io_c_op2 io_c_op3 io_c_op4 io_c_ffm1 io_c_ffm2 io_c_ffm3 io_c_ffm4 io_c_tek1 io_c_tek2 io_c_tek3 io_c_sv1 io_c_sv2 io_c_sv3 io_s_i1 io_s_i2 io_s_i3 io_s_cn1 io_s_cn2 io_s_cn3 io_s_cn4 io_s_ss1 io_s_ss2 io_s_opp1 io_s_opp2 io_s_opp3 immx_b_nc1 immx_b_nc2 immx_b_nc3 immx_b_nc4 immx_b_nc5 immx_b_fc1 immx_b_fc2 immx_b_fc3 immx_b_fc4 immx_b_fc5 immx_b_pc1 immx_b_pc2 immx_b_pc3 immx_b_pc4 immx_b_pc5 immx_b_hc1 immx_b_hc2 immx_b_hc3 immx_b_sc1 immx_b_sc2 immx_b_sc3 immx_b_cd1 b_cd2 immx_c_kto1 immx_c_kto2 immx_c_kto3 immx_c_kto4 immx_c_rsl1 immx_c_rsl2 immx_c_rsl3 immx_c_op1 immx_c_op2 immx_c_op3 immx_c_op4 immx_c_ffm1 immx_c_ffm2 immx_c_ffm3 immx_c_ffm4 immx_c_tek1 immx_c_tek2 immx_c_tek3 immx_c_sv1 immx_c_sv2 immx_c_sv3 immx_s_i1 immx_s_i2 immx_s_i3 immx_s_cn1 immx_s_cn2 immx_s_cn3 immx_s_cn4 immx_s_ss1 immx_s_ss2 immx_s_opp1 immx_s_opp2 immx_s_opp3 immx_b_nc_index immx_b_fc_index immx_b_pc_index immx_b_hc_index immx_b_sc_index immx_b_cd_index immx_c_kto_index immx_c_rsl_index immx_c_op_index immx_c_ffm_index immx_c_tek_index immx_c_sv_index immx_s_inst_index immx_s_cn immx_s_ss_index immx_s_opp_index index_buffer index_learningadapt index_selforga _merge

save "1 Input\boku_ugm_combined_master_raw.dta, replace", replace 
snapshot restore 1

save "1 Input\boku_ugm_combined_edit_002.dta", replace
***
use "1 Input\boku_ugm_combined_edit_002.dta"

*/
