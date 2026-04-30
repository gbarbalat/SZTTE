#
rm(list=ls())

library(tidyverse)
library(survival)
library(cmprsk)
library(stringr)


# load defaveur data ----
defaveur_pop <- c("FDEP09","FDEP13","fdep15","FDEP20","FDEP20_quint","FDEP20_quint_pop","P15_POP","P20_POP","Q_COM_09","Q_COM_13","Q_POP_09","Q5_15","UU09")
defa_uu2009 <- haven::read_sas("./sasdata1/defa_uu2009.sas7bdat"); colSums(is.na(defa_uu2009)); 
defa_uu2009 %>%
  filter(!is.na(FDEP09)) %>%
  distinct(depcom) -> defa_uu2009_present

defa_uu2013 <- haven::read_sas("./sasdata1/defa_uu2013.sas7bdat"); colSums(is.na(defa_uu2013))
setdiff(defa_uu2009_present$depcom, defa_uu2013$depcom)
setdiff(defa_uu2013$depcom, defa_uu2009_present$depcom) %>% sort

defa_uu2015 <- haven::read_sas("./sasdata1/defa_uu2015.sas7bdat"); colSums(is.na(defa_uu2015))
setdiff(defa_uu2009_present$depcom, defa_uu2015$CODGEO)
setdiff(defa_uu2015$CODGEO, defa_uu2009_present$depcom) %>% sort

defa_uu2020 <- haven::read_sas("./sasdata1/defa_uu2020.sas7bdat"); colSums(is.na(defa_uu2020))
setdiff(defa_uu2009_present$depcom, defa_uu2020$CODGEO)
setdiff(defa_uu2020$CODGEO, defa_uu2009_present$depcom) %>% sort


# load and preprocess data ----
full_db <- haven::read_sas("./sasdata1/final.sas7bdat")
colnames(full_db)

#Inspect those with aberrant age
aberrant_age <- full_db %>%
  select(-c(DCD_CIR_COD,BEN_RNG_GEM,BEN_NIR_PSA, BEN_NIR_ANO, BEN_NAI_ANN_KI)) %>%
  mutate(AGE_AT_ALD=as.numeric(year(as.Date(IMB_ALD_DTD)))-as.numeric(BEN_NAI_ANN)) %>%
  #filter(AGE_AT_ALD < 60)
  filter(AGE_AT_ALD < 0) %>% 
  select(c(BEN_IDT_ANO, BEN_NAI_ANN, IMB_ALD_DTD, IMB_ALD_DTF,AGE_AT_ALD, BEN_DCD_DTE, CIM_LIB))
View(aberrant_age)

# merged_ step ----
merged_ <- full_db %>%
  select(-c(DCD_CIR_COD,BEN_RNG_GEM,BEN_NIR_PSA, BEN_NIR_ANO, BEN_NAI_ANN_KI)) %>%
  
  ## earliest and latest ALD ----
  group_by(BEN_IDT_ANO) %>%
  mutate(
    ALD_start = min(IMB_ALD_DTD, na.rm = TRUE),
    ALD_end   = max(IMB_ALD_DTF, na.rm = TRUE),
    ALD_nrecords = n() 
  ) %>%
  ungroup() %>%
  
  ## age at ALD ----
  mutate(AGE_AT_ALD=as.numeric(year(as.Date(ALD_start)))-as.numeric(BEN_NAI_ANN)) %>%
  
  ## filter by age ----
  filter(AGE_AT_ALD < 50 & AGE_AT_ALD > 15) %>% 
  mutate(
    BEN_DCD_DTE_date = as.Date(BEN_DCD_DTE),   # convert to Date
    MAX_TRT_DTD_date = as.Date(MAX_TRT_DTD),   # convert to Date
    BEN_DCD_DTE_year = year(BEN_DCD_DTE_date),
    MAX_TRT_DTD_year = year(MAX_TRT_DTD_date),
  
  # indicates if year of death matches from 2 db
  diff_FLX = if_else(
    as.numeric(FLX_PER_ANN)==as.numeric(BEN_DCD_DTE_year),
    1,
    0
  )) %>%
  
  #commented lines = check a few things 
  # filter( diff_FLX==0 | is.na(diff_FLX)) %>%  
  distinct(BEN_IDT_ANO, .keep_all = TRUE) %>% 
  #filter(!(BEN_DCD_DTE_year==1600 & FLX_PER_ANN=="")) %>% #those are not dead!!!
  #filter(BEN_DCD_DTE_year<=2023) %>% #FLX_PER_ANN stops in 2023 (2yr gap)
  #filter(BEN_DCD_DTE_year>=2006) %>% #FLX_PER_ANN starts in 2006
  #filter(BEN_DCD_DTE_year==1600 ) %>%
  #filter(FLX_PER_ANN=="") %>%
  
  ## filter by date of death ----
  #only keep patients for whom you are sure of date of death (2006-2023)
  #BEN_DCD_DTE never NA or ""
  filter(!BEN_DCD_DTE_year>2023) %>% #
  filter(!(BEN_DCD_DTE_year<2006 & BEN_DCD_DTE_year!=1600)) %>% # if 1600 then may not be dead
  filter(!(BEN_DCD_DTE_year==1600 & FLX_PER_ANN!="")) %>% #if 1600, and FLX_PER_ANN is not empty, dunno if patient is dead
  filter(!(BEN_DCD_DTE_year!=1600 & FLX_PER_ANN=="")) %>% #if other than 1600 and FLX_PER_ANN empty, dunno if patient is dead
  
  # to check that all went well
  mutate(
  diff_years_BEN = if_else(
    BEN_DCD_DTE_year == 1600,
    2025 - MAX_TRT_DTD_year,
    as.numeric(difftime(BEN_DCD_DTE_date, MAX_TRT_DTD_date, units = "days")) / 365.25
  ),
  diff_years_FLX = if_else(
    FLX_PER_ANN == "",
    2025 - MAX_TRT_DTD_year,
    as.numeric(FLX_PER_ANN)-as.numeric(MAX_TRT_DTD_year)
  ))  %>%
  ## clean causes of death ----
  mutate(
    DCD_CIM_COD = str_trim(DCD_CIM_COD),
    
    death_or_not = case_when(
      
      FLX_PER_ANN=="" ~ 0, #not dead
      FLX_PER_ANN!="" ~ 1 # dead
      
    ),
    
    cause_nat_unnat = case_when(
      death_or_not == 0 ~ "Alive",
      str_detect(DCD_CIM_COD, "^[A-R]") ~ "Natural causes",
      str_detect(DCD_CIM_COD, "^U0[1-3]|^V|^W|^X|^Y") ~ "Unnatural causes",
      TRUE ~ "Other" #Other codes for natural causes
      
      ),
    
    cause_category = case_when( #from JAMA study ... we will use EPS study 
      
      death_or_not == 0 ~ "Alive",
    
      # Cardiovascular
      str_detect(DCD_CIM_COD, "^I2[0-5]") ~ "Ischemic heart disease",
      str_detect(DCD_CIM_COD, "^I0[0-9]|^I11|^I13|^I26|^I27|^I28|^I29|^I3[0-9]|^I4[0-9]|^I5[0-1]") ~ "Non-ischemic heart disease",
      str_detect(DCD_CIM_COD, "^I6[0-6]") ~ "Stroke",
      str_detect(DCD_CIM_COD, "^I") ~ "Other circulatory disease",
      
      # Cancer
      str_detect(DCD_CIM_COD, "^C34") ~ "Cancer - Lung",
      str_detect(DCD_CIM_COD, "^C18") ~ "Cancer - Colon",
      str_detect(DCD_CIM_COD, "^C50") ~ "Cancer - Breast",
      str_detect(DCD_CIM_COD, "^C22") ~ "Cancer - Liver",
      str_detect(DCD_CIM_COD, "^C25") ~ "Cancer - Pancreas",
      str_detect(DCD_CIM_COD, "^C8[1-9]|^C9[0-6]") ~ "Cancer - Hematologic",
      str_detect(DCD_CIM_COD, "^C") ~ "Cancer - Other",
      
      # Other natural causes
      str_detect(DCD_CIM_COD, "^E1[0-4]") ~ "Diabetes mellitus",
      str_detect(DCD_CIM_COD, "^N1[7-9]") ~ "Renal failure",
      str_detect(DCD_CIM_COD, "^J0[8-9]|^J1[0-8]") ~ "Influenza or pneumonia",  # fixed here
      str_detect(DCD_CIM_COD, "^A40|^A41") ~ "Sepsis",
      str_detect(DCD_CIM_COD, "^J4[0-4]") ~ "Chronic obstructive pulmonary disease",
      str_detect(DCD_CIM_COD, "^K7[0-7]") ~ "Liver diseases",
      
      # Remaining natural causes
      str_detect(DCD_CIM_COD, "^[A-R]") ~ "Other natural causes",
      
      # Unnatural causes
      str_detect(DCD_CIM_COD, "^X6[0-9]|^X7[0-9]|^X8[0-4]|^Y87\\.0|^U03") ~ "Suicide",
      str_detect(DCD_CIM_COD, "^V0[1-9]|^V[1-9]|^X[0-5][0-9]|^Y8[5-6]") ~ "Accidents",
      str_detect(DCD_CIM_COD, "^U0[1-2]|^X8[5-9]|^Y") ~ "Assault (homicide)",
      str_detect(DCD_CIM_COD, "^U0[1-3]|^V|^W|^X|^Y") ~ "Injuries with undetermined intent and other injuries",
      
      # Default
      TRUE ~ "Other"
    )
  ) %>%
  
  ## Nb of admission per year ----
  mutate(
    N_HOSPIT = rowSums(
      #select(., matches("^N_HOSPIT_((0[7-9])|(1[0-9])|(2[0-3]))C?$")),
      !is.na(select(., matches("^N_HOSPIT_((0[7-9])|(1[0-9])|(2[0-3]))C?$"))),
      na.rm = TRUE
    )
  ) %>%
  
  ## recode CODGEO, merge EDI+POP ----
  mutate(
    CODGEO_final = case_when(
      BEN_RES_DPT %in% c("999", "991", "977", "099", "000", "209", "201", "202", "098") ~ NA_character_,
      str_starts(BEN_RES_DPT, "97") ~ paste0(BEN_RES_DPT, str_sub(BEN_RES_COM, -2)),
      TRUE ~ paste0(str_sub(BEN_RES_DPT, -2), BEN_RES_COM)
    ), 
    CODGEO_DCD_final = case_when(
      BEN_RES_DPT_DCD %in% c("999", "991", "977", "099", "000", "209", "201", "202", "098") ~ NA_character_,
      str_starts(BEN_RES_DPT_DCD, "97") ~ paste0(BEN_RES_DPT_DCD, str_sub(BEN_RES_COM_DCD,-2)),
      TRUE ~ paste0(str_sub(BEN_RES_DPT_DCD, -2), BEN_RES_COM_DCD)
    )
  ) %>%
    select(-all_of(defaveur_pop)) %>%
    left_join(defa_uu2009, by=c("CODGEO_final"="depcom")) %>%
    left_join(defa_uu2013, by=c("CODGEO_final"="depcom")) %>%
    left_join(defa_uu2015, by=c("CODGEO_final"="CODGEO")) %>%
    left_join(defa_uu2020, by=c("CODGEO_final"="CODGEO"))

colSums(is.na(merged_))

# aberrant_age_filtered <- aberrant_age_filtered %>%
#   mutate(
#     CODGEO_final = case_when(
#       BEN_RES_DPT %in% c("999", "991", "977", "099", "000", "209", "201", "202", "098") ~ NA_character_,
#       str_starts(BEN_RES_DPT, "97") ~ paste0(BEN_RES_DPT, str_sub(BEN_RES_COM, -2)),
#       TRUE ~ paste0(str_sub(BEN_RES_DPT, -2), BEN_RES_COM)
#     ), 
#     CODGEO_DCD_final = case_when(
#       BEN_RES_DPT_DCD %in% c("999", "991", "977", "099", "000", "209", "201", "202", "098") ~ NA_character_,
#       str_starts(BEN_RES_DPT_DCD, "97") ~ paste0(BEN_RES_DPT_DCD, str_sub(BEN_RES_COM_DCD,-2)),
#       TRUE ~ paste0(str_sub(BEN_RES_DPT_DCD, -2), BEN_RES_COM_DCD)
#     )
#   ) #%>% select(c(BEN_RES_DPT, BEN_RES_COM, CODGEO_final, BEN_RES_DPT_DCD, BEN_RES_COM_DCD, CODGEO_DCD_final))
#   


# save ----
save(merged_,file="./sasdata1/merged_.RData")
