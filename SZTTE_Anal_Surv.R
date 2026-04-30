rm(list=ls())


library(tidyverse)
library(survminer)
library(survival)
library(stringr)
library(purrr)
library(broom)
library(mice)
library(cmprsk)
library(tableone)

analysis <- "bsl"#bsl, sensit_sev, sensit_ald, sensit_dte
do_IAEDI <- TRUE
do_Cox <- FALSE
do_parametric <- FALSE
CR_FineGray <- FALSE

load("./sasdata1/merged_imputed_listwise.RData")
if (analysis == "sensit_sev") merged_listwise <- merged_sensit_sev#merged_sensit_sev, merged_sensit_ald, merged_sensit_dte
if (analysis == "sensit_ald") merged_listwise <- merged_sensit_ald#merged_sensit_sev, merged_sensit_ald, merged_sensit_dte
if (analysis == "sensit_dte") merged_listwise <- merged_sensit_dte#merged_sensit_sev, merged_sensit_ald, merged_sensit_dte


full_set <- c("AGE_AT_ALD_cat", "AGE_AT_ALD", 
                     "time", 
                     "AGE_AT_LastPoint",
                     "Sex", 
                     "N_HOSPIT", 
                     "Region", #"CODGEO_final", 
                     "EDIq_2009",#"EDIq_2009","POPq_2009",
                     "Calendar_year","Calendar_year_cat",
                     "URBANcat_2009",
                     "death_or_not", "cause_nat_unnat",  "cause_category", 
                     "Psychosis_Type")
merged_listwise <- merged_listwise %>% 
  mutate(time = if_else(TIME_SINCE_ALD < 0, 0L, TIME_SINCE_ALD)) %>% 
  mutate(time=time+1, time2=time+1) %>%
  select(all_of(full_set))
str(merged_listwise)
numerical_var <- c("AGE_AT_ALD", "time", "AGE_AT_LastPoint", "N_HOSPIT", "Calendar_year")
factorVars <- setdiff(names(merged_listwise), c("Psychosis_Type",numerical_var))

# population characteristics ----
tableOne <- CreateTableOne(
  vars = full_set, 
  strata = "Psychosis_Type", 
  data = merged_listwise,
  factorVars = factorVars,
  addOverall = TRUE
)
tableOne
Table_1 <- print(tableOne, quote = TRUE, noSpaces = TRUE, smd=TRUE)
write.csv(Table_1, file=paste0("./sasdata1/download/",analysis,"_Table_1.csv"))

#additional characteristics not in table 1 or table inc non-inc
merged_listwise %>%
  filter(death_or_not=="1") %>%
  summarise(mean_age_death=mean(AGE_AT_LastPoint),
            sd_age_death=sd(AGE_AT_LastPoint),
            mean_time=mean(time),
            sd_time=sd(time))

# imputed db ----

# Complete Cases ----
#prepare database
merged_listwise <- merged_listwise %>% 
  #filter(cause_category!="Other natural causes") %>%
  mutate(death_or_not = if_else(death_or_not=="1",1,0)) %>% mutate(time=time+1, time2=time+1) %>%
  mutate(cause_nat_unnat = case_when(cause_nat_unnat == "Alive" ~ 0,
                                     cause_nat_unnat == "Natural causes" ~ 1,
                                     cause_nat_unnat == "Unnatural causes" ~ 2,
                                     TRUE ~ 2 #2 or 3 #Other is natural causes
  )) %>%
  mutate(cause_nat_unnat = case_when(cause_category == "Accidents" ~ 1,
                                     cause_category == "Certain conditions originating in the perinatal period" ~ 2,
                                     cause_category == "Certain infectious and parasitic diseases" ~ 2,
                                     cause_category == "Codes for special purposes" ~ 2,
                                     cause_category == "Congenital malformations and chromosomal abnormalities" ~ 2,
                                     cause_category == "Diseases of the blood and immune mechanism" ~ 2,
                                     cause_category == "Diseases of the circulatory system" ~ 2,
                                     cause_category == "Diseases of the digestive system" ~ 2,
                                     cause_category == "Diseases of the ear and mastoid process" ~ 2,
                                     cause_category == "Diseases of the genitourinary system" ~ 2,
                                     cause_category == "Diseases of the musculoskeletal system" ~ 2,
                                     cause_category == "Diseases of the nervous system" ~ 2,
                                     cause_category == "Diseases of the respiratory system" ~ 2,
                                     cause_category == "Diseases of the skin and subcutaneous tissue" ~ 2,
                                     cause_category == "Endocrine, nutritional, and metabolic diseases" ~ 2,
                                     cause_category == "External causes of morbidity and mortality" ~ 1,
                                     cause_category == "Mental and behavioral disorders" ~ 2,
                                     cause_category == "Neoplasms" ~ 2,
                                     cause_category == "Pregnancy, childbirth, and the puerperium" ~ 2,
                                     cause_category == "Suicide" ~ 1,
                                     cause_category == "Symptoms, signs and abnormal findings NOS" ~ 2,
                                     cause_category == "Alive" ~ 0  # 
  )) %>%
  mutate(cause_id = case_when(
    cause_category == "Accidents" ~ 1,
    cause_category == "Certain conditions originating in the perinatal period" ~ 2,
    cause_category == "Certain infectious and parasitic diseases" ~ 3,
    cause_category == "Codes for special purposes" ~ 4,
    cause_category == "Congenital malformations and chromosomal abnormalities" ~ 5,
    cause_category == "Diseases of the blood and immune mechanism" ~ 6,
    cause_category == "Diseases of the circulatory system" ~ 7,
    cause_category == "Diseases of the digestive system" ~ 8,
    cause_category == "Diseases of the ear and mastoid process" ~ 9,
    cause_category == "Diseases of the genitourinary system" ~ 10,
    cause_category == "Diseases of the musculoskeletal system" ~ 11,
    cause_category == "Diseases of the nervous system" ~ 12,
    cause_category == "Diseases of the respiratory system" ~ 13,
    cause_category == "Diseases of the skin and subcutaneous tissue" ~ 14,
    cause_category == "Endocrine, nutritional, and metabolic diseases" ~ 15,
    cause_category == "External causes of morbidity and mortality" ~ 16,
    cause_category == "Mental and behavioral disorders" ~ 17,
    cause_category == "Neoplasms" ~ 18,
    cause_category == "Pregnancy, childbirth, and the puerperium" ~ 19,
    cause_category == "Suicide" ~ 20,
    cause_category == "Symptoms, signs and abnormal findings NOS" ~ 21,
    cause_category == "Alive" ~ 0  # 
  )) %>%
  mutate(cause_id_rgp = case_when(
    cause_category == "Accidents" ~ 1,
    cause_category == "Certain conditions originating in the perinatal period" ~ 2,
    cause_category == "Certain infectious and parasitic diseases" ~ 3,
    cause_category == "Codes for special purposes" ~ 2,
    cause_category == "Congenital malformations and chromosomal abnormalities" ~ 2,
    cause_category == "Diseases of the blood and immune mechanism" ~ 2,
    cause_category == "Diseases of the circulatory system" ~ 4,
    cause_category == "Diseases of the digestive system" ~ 5,
    cause_category == "Diseases of the ear and mastoid process" ~ 2,
    cause_category == "Diseases of the genitourinary system" ~ 2,
    cause_category == "Diseases of the musculoskeletal system" ~ 2,
    cause_category == "Diseases of the nervous system" ~ 6,
    cause_category == "Diseases of the respiratory system" ~ 7,
    cause_category == "Diseases of the skin and subcutaneous tissue" ~ 2,
    cause_category == "Endocrine, nutritional, and metabolic diseases" ~ 8,
    cause_category == "External causes of morbidity and mortality" ~ 9,
    cause_category == "Mental and behavioral disorders" ~ 10,
    cause_category == "Neoplasms" ~ 11,
    cause_category == "Pregnancy, childbirth, and the puerperium" ~ 2,
    cause_category == "Suicide" ~ 12,
    cause_category == "Symptoms, signs and abnormal findings NOS" ~ 13,
    cause_category == "Alive" ~ 0  # 
  ))
# merged_listwise10 <- merged_listwise %>%
#   mutate(
#     time = pmin(time, 10),                     # truncate time at 10
#     death_or_not = ifelse(time <= 10 & death_or_not == 1, 1, 0)  # event only if before 10
#   )

## KM ----
# non-parametric and purely empirical, always unadjusted
merged_listwise_KM <- merged_listwise #%>% mutate(time=round(time/365.25,0))
merged_listwise_KM$Psychosis_Type <- factor(merged_listwise_KM$Psychosis_Type, 
                                            levels=c("Short","SZTyp",'SZAff',"TbleDelir","SCZ","Unspecified"))
fit_all <- survfit(Surv(time = time, event = death_or_not) ~ 1, data = merged_listwise_KM)
fit_GP <- survfit(Surv(time, death_or_not) ~ Psychosis_Type , data = merged_listwise_KM)

#varify assumptions
plot(log(fit_all$time), log(-log(fit_all$surv)))
plot(log(fit_GP$time), log(-log(fit_GP$surv)))

# KM curves
# Define your exact colors (matching Psychosis_Type order)
p <- ggsurvplot(
  fit_GP,
  data = merged_listwise_KM,
  conf.int = F,
  xlab = "Time since diagnosis (days)",
  ylab = "Survival probability",
  title = element_blank()
)

p$plot +
  coord_cartesian(ylim = c(0.85, 1.00)) +
  scale_y_continuous(
    breaks = seq(0.85, 1.00, by = 0.05),
    limits = c(0.85, 1.00)
  )+
  scale_color_manual(
    labels = c(
      "Psychosis_Type=Short"       = "Short",
      "Psychosis_Type=TbleDelir"   = "Delus.",
      "Psychosis_Type=SZAff"       = "SZAff",
      "Psychosis_Type=SZTyp"       = "SZTyp",
      "Psychosis_Type=SCZ"         = "SCZ",
      "Psychosis_Type=Unspecified" = "Unsp."
    ), 
    values=c(
      "Psychosis_Type=Short"       = "purple",
      "Psychosis_Type=TbleDelir"   = "black",
      "Psychosis_Type=SZAff"       = "blue",
      "Psychosis_Type=SZTyp"       = "orange",
      "Psychosis_Type=SCZ"         = "red",
      "Psychosis_Type=Unspecified" = "darkgreen"
    )
  ) +
  guides(fill = "none",
         color = guide_legend(
           override.aes = list(size = 3.5)  # thickness of legend lines
         )) + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.30),   # left–down quadrant (inside)
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_blank(),    
    legend.key.width = unit(1.5, "cm"),
    legend.key = element_blank(), 
    legend.text=element_text(size = 12)
      )

#log-rank test for differences between survival curves
survdiff(Surv(time = time, event = death_or_not) ~ Psychosis_Type , data = merged_listwise)#log-rank

## prob of survival at different times
fit <- fit_all#fit_all OR fit_gp
summary(fit, times=c(2000,4000,6000), se.fit=TRUE);
## quantiles of the survival function
quantile(fit, probs= c(0.025,0.05, 0.075))
## RMST
print(fit, print.rmean=TRUE) 

## POGEE ----
source("./sasdata1/RMST_Final_anal.R")

if (do_IAEDI) {
source("./sasdata1/RMST_Final_anal_IAEDI.R")
}

## Cox PH ----
# cox model-based survival curves from a Cox proportional hazards model
#semi-param, adjusted
if (do_Cox) {
cox_m <- coxph(Surv(time, death_or_not) ~  Psychosis_Type + Sex + EDIq_2009 + URBANcat_2009+  
                 Calendar_year + AGE_AT_ALD , 
               data = merged_listwise);summary(cox_m);
test_ph <- cox.zph(cox_m, terms = TRUE)
print(test_ph); plot(test_ph, resid = FALSE)


mm <- model.matrix(
  ~ time + death_or_not + Sex + Calendar_year + AGE_AT_ALD + Psychosis_Type + EDIq_2009 + URBANcat_2009 - 1,
  data = merged_listwise
) %>% as.data.frame() %>% 
  mutate(time=round(time/365))
# Rename URBANcat columns to keep only part before hyphen
urban_cols <- grep("^URBANcat", colnames(mm), value = TRUE)
new_names <- sub("URBANcat_2009(.*)-.*", "URBANcat_2009\\1", urban_cols)
colnames(mm)[match(urban_cols, colnames(mm))] <- new_names
cox_m <- coxph(Surv(time, death_or_not) ~ SexMale + 
                 URBANcat_20091+URBANcat_20092+URBANcat_20093+URBANcat_20094+URBANcat_20095+URBANcat_20096+URBANcat_20097+URBANcat_20098+ 
                 # (Psychosis_TypeShort + Psychosis_TypeSZAff + Psychosis_TypeSZTyp + Psychosis_TypeTbleDelir + Psychosis_TypeUnspecified)* 
                 # (EDIq_20092+EDIq_20093+EDIq_20094+EDIq_20095)+
                 #Psychosis_TypeShort + 
                 Psychosis_TypeSZAff + Psychosis_TypeSZTyp + Psychosis_TypeTbleDelir + Psychosis_TypeUnspecified+ 
                 EDIq_20092+EDIq_20093+EDIq_20094+EDIq_20095+
                 Calendar_year + AGE_AT_ALD +
                 tt(SexMale) +
                 #tt(EDIq_20092) + tt(EDIq_20093) + tt(EDIq_20094) + tt(EDIq_20095) +
                 #tt(URBANcat_20091) + tt(URBANcat_20092) + tt(URBANcat_20093) + tt(URBANcat_20094) +
                 #tt(URBANcat_20095) + tt(URBANcat_20096) + tt(URBANcat_20097) + tt(URBANcat_20098) +
                 #tt(Psychosis_TypeShort) + tt(Psychosis_TypeSZAff) + tt(Psychosis_TypeSZTyp) + tt(Psychosis_TypeTbleDelir) + tt(Psychosis_TypeUnspecified) + 
                 tt(Calendar_year) + tt(AGE_AT_ALD) ,
               tt = function(x, t, ...) x * log(t+1) ,
               data = mm); summary(cox_m);
save(cox_m, test_ph, file=paste0("./sasdata1/download/",analysis,"_Cox.RData"))
}
#tt = function(x, t, ...) x * log(t) in coxph(), it includes BOTH the linear term AND the log-time interaction term.

## Parametric ----
if(do_parametric) {
m_W <- rms::psm(Surv(time, death_or_not) ~ Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + 
                  EDIq_2009 + URBANcat_2009   , 
                dist="weibull", #weibull, lognormal, loglogistic exponential logistic
                data = merged_listwise);m_W
resid <- residuals(m_W)  # 
rms::survplot(resid) 

#Hazard is exp(-beta/scale)
m_W <- survreg(Surv(time, death_or_not) ~  Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + EDIq_2009 + URBANcat_2009 ,
               dist="weibull", data = merged_listwise);
m_LN <- survreg(Surv(time, death_or_not) ~  Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + EDIq_2009 + URBANcat_2009 ,
                dist="lognormal", data = merged_listwise);
m_LL <- survreg(Surv(time, death_or_not) ~  Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + EDIq_2009 + URBANcat_2009 ,
                dist="loglogistic", data = merged_listwise);
m_E <- survreg(Surv(time, death_or_not) ~  Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + EDIq_2009 + URBANcat_2009 ,
               dist="exponential", data = merged_listwise);
m_L <- survreg(Surv(time, death_or_not) ~  Sex + Calendar_year + AGE_AT_ALD+ Psychosis_Type + EDIq_2009 + URBANcat_2009 ,
               dist="logistic", data = merged_listwise);
AIC(m_W,m_LN,m_LL, m_E, m_L)
}

## CR Fine Gray ----
if (CR_FineGray) {
table(merged_listwise$cause_category)
table(merged_listwise$cause_nat_unnat)

# cumulative incidence for each event type, 
merged_listwise$Psychosis_Type <- factor(merged_listwise_KM$Psychosis_Type, 
                                            levels=c("Short","SZTyp",'SZAff',"TbleDelir","SCZ","Unspecified"))
cuminc_fit <- cuminc(
  ftime = merged_listwise$time/365.25,
  fstatus = merged_listwise$death_or_not,#cause_nat_unnat cause_category
  group = merged_listwise$Psychosis_Type,
  strata = merged_listwise$Psychosis_Type,
  
)
cuminc_fit_cause1 <- cuminc_fit[1:6]
cuminc_fit_cause2 <- cuminc_fit[7:12]

#analogous to KM Plot
plot(cuminc_fit_cause1, lty=1,lwd=2,
     curvlab = c("Short","SZTyp","SZAff",  "Delus.","SCZ",  "Unsp."),
     color = c("purple", "orange", "blue", "black", "red", "darkgreen"),
     xlab = "Time since diagnosis (years)", ylab = "Cumulative incidence", ylim = c(0,0.08))

# Gray's test: p val for equality of cumulative incidence curves across groups, analogous to log-rank test 
cuminc_fit


# # 2. Survival probability = 1 - CIF of event of interest (cause 1)
# # Extract CIF at specific times (e.g., 6, 12, 24 months)
# times <- c(2000, 4000, 6000)
# surv_prob <- 1 - timepoints(cuminc_fit, times)`SCZ 1`  # Adjust "group1 1" to your group/cause
# 
# # 3. Unadjusted RMST from CIF (area under 1-CIF curve)
# rmst <- timepoints(cif_fit, tau = 24)  # tau = restriction time
# rmst_table <- summary(rmst, times = FALSE)  # RMST per group
# 
# # 4. Quantiles of CIF (time when CIF(p) reaches probability p)
# quantiles <- quantile(cuminc_fit, probs = c(0.05))



# sub-hazard ratio, analogous to Cox, high computational time
fg_model <- crr(
  ftime = merged_listwise$time, 
  fstatus = merged_listwise$cause_category,#cause_nat_unnat cause_nat_unnat
  failcode = 1,   # event of interest
  cencode = 0,    # censoring code
  cov1 =  model.matrix(~ Sex + Calendar_year + AGE_AT_ALD + Psychosis_Type + EDIq_2009 + URBANcat_2009 - 1,
                       data = merged_listwise)[,-1]#matrix of fixed cv
  # cov2 =  cbind(model.matrix(~ Sex + Calendar_year + AGE_AT_ALD - 1,data = merged_listwise)[,-1],
  #               model.matrix(~ Sex + Calendar_year + AGE_AT_ALD - 1,data = merged_listwise)[,-1]),
  #tf = function(Uft) cbind(Uft, Uft * log(Uft)),
  #tf = function(Uft) cbind(Uft+1, (Uft+1)*log(Uft+1))
  
  #exact equivalent of tt (see above)
  #az+bzt+zlog(t) can be fit by specifying cov1=z, cov2=cbind(z,z), tf=function(uft) cbind(uft,uft** log(Uft)).
)
save(fg_model, file=paste0("./sasdata1/download/", analysis, "_crrSubHaz_suicide.RData"))
summary(fg_model)
}