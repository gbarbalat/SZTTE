# SZTTE

## Paper  
submitted  

## Scripts  
1. Create database on SAS  
2. SZTTE_PreProcess_data.R creates merged_.RData (all data merged in one db)  
3. SZTTE_Process_data processes merged_, rearranges db, does feature ingeneering, and does preliminary analysis.   
Creates merged_listwise.RData with   
merged_listwise <- merged_listwise %>% filter(!Psychosis_Type=="Short") : Short is less than 1% of the data and is subject to heaps of Dx change  
merged_sensit_sev <- merged_listwise %>% filter(N_HOSPIT>0): those who have been admitted at least once  
merged_sensit_dte <- merged_listwise %>%  mutate(TIME_SINCE_ALD=TIME_SINCE_ALD2)  
merged_sensit_ald <- merged_listwise %>% filter(!BEN_IDT_ANO %in% ids_multi_med2): those with only one dx type  
Note that dx changes occur for Short-Term psychosis so we removed this category from our analysis (and anyway, was less than 1% of the data)  
Also,   
Uses compare_imp_nonimp.R, creates table imputed-nonimputed (not used here)  
Uses compare_inc_full.R, creates table included-nonincluded  
Note that 1st dx is updated (dplyr::distinct mistake in SZTTE_PreProcess_data - does not change the results)  
and  causes of death are rearranged (based on Swedish paper)  
4. SZTTE_Anal_Surv.R does the survival and CR analysis, using   
RMST_Final_anal.R, using a surving model and CR models  
It also does the effect mdif analysis by simply including an interaction term between Psychosis Type and EDI quintiles and running   
RMST_Final_anal_IAEDI.R, using a surving model and CR models  
Produces Table1 for baseline anal and  
$analysis(bsl/sensit)_$IAEDI(or not)_ipw(or not)_$POGEE (or Cox)   
[1] "bsl_Cox.RData"                "bsl_IAEDI_ipw_POGEE.RData"    "bsl_IAEDI_POGEE.RData"        "bsl_ipw_POGEE.RData"         
 [5] "bsl_POGEE.RData"              "bsl_Table_1.csv"             "sensit_ald_IAEDI_POGEE.RData"
 [9] "sensit_ald_POGEE.RData"       "sensit_ald_Table_1.csv"       "sensit_dte_IAEDI_POGEE.RData" "sensit_dte_POGEE.RData"      
[13] "sensit_dte_Table_1.csv"       "sensit_sev_IAEDI_POGEE.RData" "sensit_sev_POGEE.RData"       "sensit_sev_Table_1.csv" 
6. Use anal_results.R and anal_results_IAEDI.R to create csv files  
7. Use Produce_graph_CoD and Produce_graph_CoDIAED to produce figures  
8. Download to PC  
