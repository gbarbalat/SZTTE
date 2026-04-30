# hdr ----
rm(list=ls())

plot_var_outcome <- FALSE
#mice param
do_impute <- FALSE
m <- 5
maxit <- 5

library(tidyverse)
library(lubridate)
library(stringr)
library(purrr)
library(broom)
library(mice)

# find ID with different dx classes
#all Ids of final db (merged_listwise) are in dc_merged_cohort_defaveur_pmsi
#all ALD dates are as in the SNDS
#There is only individuals with psychotic disorders
haven::read_sas("./sasdata1/dc_merged_cohort_defaveur_pmsi.sas7bdat") -> dc_merged_cohort_defaveur_pmsi
#haven::read_sas("./sasdata1/ald23_2006_2015.sas7bdat") -> ald23_2006_2015

#gives id of individuals with either one diagnosis OR more than one dx but that are different
ids_oneDx_diffDx <- dc_merged_cohort_defaveur_pmsi %>%
  mutate(which_MH = case_when(
    is.na(MED_MTF_COD) ~ NA_character_, ###
    grepl("^F28|^F29", MED_MTF_COD) ~ "Unspecified",
    grepl("^F21", MED_MTF_COD)      ~ "SZTyp",
    grepl("^F20", MED_MTF_COD)      ~ "SCZ",
    grepl("^F25", MED_MTF_COD)      ~ "SZAff",
    grepl("^F23", MED_MTF_COD)      ~ "Short",
    
    grepl("^F22|^F24", MED_MTF_COD) ~ "TbleDelir",
    TRUE ~ "Other"#no other dx and no NA, all dx are F2x
  )) %>%
  group_by(BEN_IDT_ANO ) %>% summarise(n_dx = dplyr::n(), 
                                       n_dx_types = dplyr::n_distinct(which_MH), .groups = "drop") %>% ungroup %>%
  filter(#n_dx==1 | #in fact, individuals with just one ALD event can be automatically renewed so comment here
           n_dx_types > 1) %>% 
  pull(BEN_IDT_ANO)

# load data
load("./sasdata1/merged_.RData")

#sets of variables
preliminary_set <- c("AGE_AT_ALD_cat", "AGE_AT_ALD", 
                     "TIME_SINCE_ALD",
                     "AGE_AT_LastPoint","AGE_AT_LastPoint_BEN",
                     "FLAG",
                     "Sex", 
                     "N_HOSPIT", 
                     "Region", #"CODGEO_final", 
                     "EDIq_2009",#"EDIq_2009","POPq_2009",
                     "Calendar_year","Calendar_year_cat",
                     "URBANcat_2009",
                     "death_or_not", "cause_nat_unnat",  "cause_category", 
                     "Psychosis_Type")
add_set_imput <- c("DCD_CIM_COD",#"EDI_2009", "POPq_2013",#"EDIq_2013",
                   "EDI_2013", "POPq_2013",#"EDIq_2013",
                   "EDI_2015", "POPq_2015",
                   "EDI_2020", "POPq_2020",
                   "dept")
add_set_sensit <- c("TIME_SINCE_ALD2", "N_HOSPIT", "BEN_IDT_ANO")#"ALD_start","ALD_end",

#merged_ ----
#merge Exp, Out, Cv - explore #, unique and NA in ID and col names (.x and .y)
summary_na_unique <- merged_ %>%
  summarise(
    across(
      everything(),
      list(
        n_na = ~sum(is.na(.)),
        n_unique = ~n_distinct(.)
      )
    )
  )

summary_long <- summary_na_unique %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    variable = str_remove(metric, "_n_na$|_n_unique$"),
    stat = ifelse(str_detect(metric, "_n_na$"), "n_na", "n_unique")
  ) %>%
  select(variable, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)
summary_long

#merged_col_obs ----
#Add on obvious var, obvious select, filter (excl criteria) & recode (e.g. G027B=Citizen, S022= Year + Month) 
#inc. na_if, make categ; Explore NA/distributions; 
merged_col_obs <- merged_ %>%
  
  mutate(cause_category = case_when(
    # External Causes Sub-categories
    str_detect(DCD_CIM_COD, "^X6|^X7|^X8[0-4]|^Y1|^Y2|^Y3[0-4]") ~ "Suicide",
    str_detect(DCD_CIM_COD, "^V|^W|^X[0-5][0-9]") ~ "Accidents",
    
    # Standard Chapters
    str_detect(DCD_CIM_COD, "^A|^B") ~ "Certain infectious and parasitic diseases",
    str_detect(DCD_CIM_COD, "^C|^D[0-3]|D4[0-8]") ~ "Neoplasms",
    str_detect(DCD_CIM_COD, "^D[5-8]") ~ "Diseases of the blood and immune mechanism",
    str_detect(DCD_CIM_COD, "^E") ~ "Endocrine, nutritional, and metabolic diseases",
    str_detect(DCD_CIM_COD, "^F") ~ "Mental and behavioral disorders",
    str_detect(DCD_CIM_COD, "^G") ~ "Diseases of the nervous system",
    str_detect(DCD_CIM_COD, "^H[0-5]") ~ "Diseases of the eye and adnexa",
    str_detect(DCD_CIM_COD, "^H[6-9]") ~ "Diseases of the ear and mastoid process",
    str_detect(DCD_CIM_COD, "^I") ~ "Diseases of the circulatory system",
    str_detect(DCD_CIM_COD, "^J") ~ "Diseases of the respiratory system",
    str_detect(DCD_CIM_COD, "^K") ~ "Diseases of the digestive system",
    str_detect(DCD_CIM_COD, "^L") ~ "Diseases of the skin and subcutaneous tissue",
    str_detect(DCD_CIM_COD, "^M") ~ "Diseases of the musculoskeletal system",
    str_detect(DCD_CIM_COD, "^N") ~ "Diseases of the genitourinary system",
    str_detect(DCD_CIM_COD, "^O") ~ "Pregnancy, childbirth, and the puerperium",
    str_detect(DCD_CIM_COD, "^P") ~ "Certain conditions originating in the perinatal period",
    str_detect(DCD_CIM_COD, "^Q") ~ "Congenital malformations and chromosomal abnormalities",
    str_detect(DCD_CIM_COD, "^R") ~ "Symptoms, signs and abnormal findings NOS",
    str_detect(DCD_CIM_COD, "^U") ~ "Codes for special purposes",
    
    # Catch-all for other external causes (Y-codes not in suicide)
    str_detect(DCD_CIM_COD, "^V|^W|^X|^Y") ~ "External causes of morbidity and mortality",
    
    TRUE ~ "Alive"
  )) %>%
  
  mutate(cause_nat_unnat = case_when(cause_nat_unnat == "Alive" ~ 0,
                                     cause_nat_unnat == "Natural causes" ~ 1,
                                     cause_nat_unnat == "Unnatural causes" ~ 2,
                                     TRUE ~ 1 #
  )) %>%
  
  mutate(cause_category_rgp = case_when(
    cause_category == "Accidents" ~ "Accidents",
    cause_category == "Certain conditions originating in the perinatal period" ~ "Other natural causes",
    cause_category == "Certain infectious and parasitic diseases" ~ "Certain infectious and parasitic diseases",
    cause_category == "Codes for special purposes" ~ "Other natural causes",
    cause_category == "Congenital malformations and chromosomal abnormalities" ~ "Other natural causes",
    cause_category == "Diseases of the blood and immune mechanism" ~ "Other natural causes",
    cause_category == "Diseases of the circulatory system" ~ "Diseases of the circulatory system",
    cause_category == "Diseases of the digestive system" ~ "Diseases of the digestive system",
    cause_category == "Diseases of the ear and mastoid process" ~ "Other natural causes",
    cause_category == "Diseases of the genitourinary system" ~ "Other natural causes",
    cause_category == "Diseases of the musculoskeletal system" ~ "Other natural causes",
    cause_category == "Diseases of the nervous system" ~ "Diseases of the nervous system",
    cause_category == "Diseases of the respiratory system" ~ "Diseases of the respiratory system",
    cause_category == "Diseases of the skin and subcutaneous tissue" ~ "Other natural causes",
    cause_category == "Endocrine, nutritional, and metabolic diseases" ~ "Endocrine, nutritional, and metabolic diseases",
    cause_category == "External causes of morbidity and mortality" ~ "External causes of morbidity and mortality",
    cause_category == "Mental and behavioral disorders" ~ "Mental and behavioral disorders",
    cause_category == "Neoplasms" ~ "Neoplasms",
    cause_category == "Pregnancy, childbirth, and the puerperium" ~ "Other natural causes",
    cause_category == "Suicide" ~ "Suicide",
    cause_category == "Symptoms, signs and abnormal findings NOS" ~ "Symptoms, signs and abnormal findings NOS",
    cause_category == "Alive" ~ "Alive"  # 
  ))   %>%
  
  #take only cases from year 2006 because that is when cause of death db starts
  filter(year(as.Date(ALD_start))>2005) %>%
 
  mutate(FLX_PER_ANN=as.numeric(FLX_PER_ANN),
         BEN_NAI_ANN=as.numeric(BEN_NAI_ANN)
         ) %>%
  #make AGE_AT_ALD categorical
  mutate(
    AGE_AT_ALD_cat = case_when(
      AGE_AT_ALD >= 16 & AGE_AT_ALD <= 20 ~ "16–20",
      AGE_AT_ALD >= 21 & AGE_AT_ALD <= 25 ~ "21–25",
      AGE_AT_ALD >= 26 & AGE_AT_ALD <= 30 ~ "26–30",
      AGE_AT_ALD >= 31 & AGE_AT_ALD <= 40 ~ "31–40",
      AGE_AT_ALD >= 41 & AGE_AT_ALD <= 50 ~ "41–50",
      TRUE ~ NA_character_
    ))%>%
  
  #make Calendar year categorical
  mutate(Calendar_year=year(as.Date(ALD_start)),
         Calendar_year_cat=case_when(
           Calendar_year >= 1942 & Calendar_year <= 1995 ~ "1942–1995",
           Calendar_year >= 1996 & Calendar_year <= 2000 ~ "1996–2000",
           Calendar_year >= 2001 & Calendar_year <= 2005 ~ "2001–2005",
           Calendar_year >= 2006 & Calendar_year <= 2010 ~ "2006–2010",
           Calendar_year >= 2011 & Calendar_year <= 2015 ~ "2011–2015",
           TRUE ~ NA_character_
         ))%>%
  
  # Create a region column
  mutate(
    # Extract department from the first two chars
    dept = substr(CODGEO_final, 1, 2),
    # Corsica special case: 2A/2B become 20 (region CORSE)
    dept = if_else(dept %in% c("2A", "2B"), "20", dept),
    
    Region = case_when(
      is.na(CODGEO_final) ~ NA_character_,   # keep NA
      
      dept %in% c("01","03","07","15","26","38","42","43","63","69","73","74") ~ "ARA",#"Auvergne-Rhône-Alpes",
      dept %in% c("21","25","39","58","70","71","89","90") ~ "BourgFraComte",#"Bourgogne-Franche-Comté",
      dept %in% c("22","29","35","56") ~ "Bretagne",
      dept %in% c("18","28","36","37","41","45") ~ "Centre",#"Centre-Val de Loire",
      dept == "20" ~ "Corse",
      dept %in% c("08","10","51","52","54","55","57","67","68","88") ~ "GdEst",#"Grand Est",
      dept %in% c("02","59","60","62","80") ~ "HDF",#"Hauts-de-France",
      dept %in% c("75","77","78","91","92","93","94","95") ~ "IDF",#"Île-de-France",
      dept %in% c("14","27","50","61","76") ~ "Normandie",
      dept %in% c("16","17","19","23","24","33","40","47","64","79","86","87") ~ "NelleAq",#"Nouvelle-Aquitaine",
      dept %in% c("09","11","12","30","31","32","34","46","48","65","66","81","82") ~ "Occitanie",
      dept %in% c("44","49","53","72","85") ~ "PDLoire",#"Pays de la Loire",
      dept %in% c("04","05","06","13","83","84") ~ "PACA",#"Provence-Alpes-Côte d'Azur",
      
      # Overseas departments & territories
      dept %in% c("97", "98") ~ "DomTom",
      
      # Any other codes
      TRUE ~ "Other"
    )
  ) %>%
  
  #analysis only on continental France
  filter(!Region %in% c("Corse", "DomTom")) %>%

  
  #rename Defavor and Population indices and urban areas
  rename(EDI_2009=FDEP09,EDI_2013=FDEP13,EDI_2015=fdep15,EDI_2020=FDEP20,
         EDIq_2009=quintile_com,EDIq_2013=QUINTILE_COM,EDIq_2020=FDEP20_quint,
         POPq_2009=quintile_pop.x, POPq_2013=quintile_pop.y,POPq_2015=Q5,POPq_2020=FDEP20_quint_pop,
         URBANcat_2009=taille_uu) %>%
         
  #recode sex
  mutate(Sex = case_when(BEN_SEX_COD==1 ~ "Male",
                         BEN_SEX_COD==2 ~  "Female",
                         TRUE ~ NA_character_)) %>%
  #add AGE_AT_LastPoint variable
  mutate(
    AGE_AT_LastPoint = case_when(
      death_or_not == 1 ~ FLX_PER_ANN - BEN_NAI_ANN,
      death_or_not == 0 ~ 2023 - BEN_NAI_ANN,#
      TRUE ~ NA_real_
    ),
    AGE_AT_LastPoint_BEN = case_when(
      death_or_not == 1 ~ BEN_DCD_DTE_year - BEN_NAI_ANN,
      death_or_not == 0 ~ 2023 - BEN_NAI_ANN,#
      TRUE ~ NA_real_
    )
  ) %>%
  #add TIME_SINCE_ALD variable
  mutate(
    TIME_SINCE_ALD = case_when(
      death_or_not == 1 ~ interval(as.Date(ALD_start),BEN_DCD_DTE_date) %/% days(1),#FLEX_PER_ANN-year(as.Date(ALD_start)),
      death_or_not == 0 ~ interval(as.Date(ALD_start),as.Date("2023/12/31") ) %/% days(1),#2023 - year(as.Date(ALD_start)),
      TRUE ~ NA_real_
    ),
    TIME_SINCE_ALD2 = case_when(
      is.na(MAX_TRT_DTD) ~ NA_real_,
      death_or_not == 1 ~ TIME_SINCE_ALD,
      
      death_or_not == 0 ~ case_when(
        # Priority condition: If year is after 2023, use the pre-calculated variable
        year(as.Date(MAX_TRT_DTD)) > 2023 ~ TIME_SINCE_ALD,
        
        # Otherwise, perform the interval calculation with the 0 floor
        interval(as.Date(ALD_start), as.Date(MAX_TRT_DTD)) %/% days(1) > 0 ~ 
          interval(as.Date(ALD_start), as.Date(MAX_TRT_DTD)) %/% days(1),
        
        # Default if interval is negative or zero
        TRUE ~ 0
      ),
      TRUE ~ NA_real_
    ), 
    FLAG = (AGE_AT_LastPoint-AGE_AT_ALD)*365.25-TIME_SINCE_ALD
  ) %>%
  
  #rmv hospit variables
  select(-matches("^N_HOSPIT_\\d")) %>%
  #make empty cells NA
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  #regp psychotic categories
  mutate(
    Psychosis_Type = case_when(
      grepl("^F28|^F29", MED_MTF_COD) ~ "Unspecified",
      grepl("^F21", MED_MTF_COD)      ~ "SZTyp",
      grepl("^F20", MED_MTF_COD)      ~ "SCZ",
      grepl("^F25", MED_MTF_COD)      ~ "SZAff",
      grepl("^F23", MED_MTF_COD)      ~ "Short",
      grepl("^F22|^F24", MED_MTF_COD) ~ "TbleDelir",
      TRUE                             ~ "Other"
    )
  )   %>%
  #remove some variables 
  select(-CAT_PCS_COD, -CODGEO, -ECD_CAU_LIB, -ECD_CAU_RNG, -ECD_CIM_COD, -ETA_MAR_COD, -PFV_ACP_COD, -PRD_SCR_COF , 
         -BEN_DCD_DTE,-CIM_LIB,-diff_FLX,-diff_years_FLX, -diff_years_BEN)  #         -DCD_CIM_COD,                                                                                       
str(merged_col_obs)

merged_col_obs%>% 
  group_by(BEN_IDT_ANO) %>% 
  summarise(n_diags = n_distinct(Psychosis_Type),
            .groups = "drop") %>% 
  summary()

summary_na_unique <- merged_col_obs %>%
  summarise(
    across(
      everything(),
      list(
        n_na = ~sum(is.na(.)),
        n_unique = ~n_distinct(.)
      )
    )
  )
summary_long <- summary_na_unique %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    variable = str_remove(metric, "_n_na$|_n_unique$"),
    stat = ifelse(str_detect(metric, "_n_na$"), "n_na", "n_unique")
  ) %>%
  select(variable, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)

summary_long

# Identify numeric and character columns
num_vars <- merged_col_obs %>% select(where(is.numeric)) %>% names()
char_vars <- merged_col_obs %>% select(where(is.character)) %>% names()

# Function to summarize numeric variables
summarize_numeric <- function(var) {
  x <- merged_col_obs[[var]]
  n_unique <- length(unique(na.omit(x)))
  
  if(n_unique > 10) {
    # summary for numeric with >10 unique values
    summary(x)
  } else {
    # frequency table for numeric with <=10 unique values
    table(x, useNA = "ifany")
  }
}

# Function to summarize character variables
summarize_character <- function(var) {
  x <- merged_col_obs[[var]]
  n_unique <- length(unique(na.omit(x)))
  
  if(n_unique <= 27) {
    # frequency table for character variables with <=20 unique values
    table(x, useNA = "ifany")
  } else {
    paste("Too many unique values (", n_unique, ")", sep = "")
  }
}

# Apply functions
num_summary <- map(num_vars, summarize_numeric)
names(num_summary) <- num_vars

char_summary <- map(char_vars, summarize_character)
names(char_summary) <- char_vars

# View results
num_summary
char_summary


#merged_gp ----
# Add on new set of var; Group/arrange levels based on 30-2% per level & not too many levels & further steps
merged_gp <- merged_col_obs %>%
  select(all_of(preliminary_set), all_of(add_set_imput), all_of(add_set_sensit)) %>%
  mutate(across(
    .cols = where(function(x) is.character(x) || (is.numeric(x) && n_distinct(x) < 10)),
    .fns = as.factor
  ))
         
# Function to calculate non-missing counts and percentages
non_missing_summary <- function(df) {
  df %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      total = n(),
      n_non_missing = sum(!is.na(value)),
      pct_non_missing = 100 * n_non_missing / total,
      .groups = "drop"
    )
}

# For numeric variables
num_vars <- merged_gp %>% select(where(is.numeric))
num_summary <- non_missing_summary(num_vars)

# For character/factor variables: per level
cat_vars <- merged_gp %>% select(-BEN_IDT_ANO, where(~is.character(.) | is.factor(.)))
summary_cat <- function(df) {
  
  df %>%
    mutate(across(everything(), as.factor)) %>%   # ensure categorical
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "level") %>%
    
    group_by(variable) %>%
    mutate(
      non_missing_pct = mean(!is.na(level)) * 100
    ) %>%
    
    group_by(variable, level, non_missing_pct) %>%
    summarise(
      N = sum(!is.na(level)),
      pct = N / nrow(merged_gp) * 100,
      .groups = "drop"
    ) %>%
    arrange(variable, desc(N))
}
cat_summary <- summary_cat(cat_vars)

# View results
num_summary
cat_summary


# plot var-outcome ----
if (plot_var_outcome) {
#& biV, multV; redo merged_gp if necessary
# Separate numeric and categorical variables
num_vars <- merged_gp %>% select(where(is.numeric)) 
cat_vars <- merged_gp %>% select(-BEN_IDT_ANO, where(~is.character(.) | is.factor(.))) 

# Numeric variables: boxplots vs death_or_not
num_plots <- map(names(num_vars), function(var) {
  ggplot(merged_gp, aes(x = factor(cause_category), y = .data[[var]])) + #x as death_or_not, cause_nat_unnat, cause_system, cause_category
    geom_boxplot(fill = "skyblue") +
    labs(x = "Death", y = var, title = paste("Boxplot of", var, "by cause of death")) +
    theme_minimal()
})
num_plots

# Categorical variables: barplots vs death_or_not
cat_plots <- map(names(cat_vars), function(var) {
  ggplot(merged_gp, aes(x = .data[[var]], fill = factor(cause_nat_unnat))) + #x as death_or_not, cause_nat_unnat, cause_system, cause_category
    geom_bar(position = "dodge") +
    labs(x = var, y = "Count", fill = "Death", 
         title = paste("Barplot of", var, "by cause of death")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
cat_plots
# 
# # Optionally: save all plots to PDF
# pdf("plots_by_death_or_not.pdf")
# walk(c(num_plots, cat_plots), print)
# dev.off()

# Select predictors (exclude outcome)
predictors <- merged_gp %>% select(-death_or_not, -BEN_IDT_ANO)

# Function to run bivariate logistic regression for each predictor
bivariate_results <- lapply(names(predictors), function(var) {
  
  formula <- as.formula(paste("death_or_not ~", var))
  
  # Fit model
  fit <- glm(formula, data = merged_gp, family = binomial)
  
  # Extract OR, CI, p-value
  tidy(fit) %>%
    mutate(
      OR = exp(estimate),
      CI_lower = exp(estimate - 1.96 * std.error),
      CI_upper = exp(estimate + 1.96 * std.error),
      variable = var
    )
})

# Combine results
bivariate_results <- bind_rows(bivariate_results) %>%
  select(variable, term, OR, CI_lower, CI_upper, p.value)

bivariate_results


# Build formula with all predictors
predictors <- merged_gp %>% select(all_of(preliminary_set)) %>% 
  select(-all_of(starts_with("cause_") ))
all_vars <- paste(names(predictors), collapse = " + ")
formula_multivar <- as.formula(paste("death_or_not ~", all_vars))

# Fit multivariate logistic regression
fit_multivar <- glm(formula_multivar, data = merged_gp, family = binomial)

# Extract ORs and CIs
multivar_results <- tidy(fit_multivar) %>%
  mutate(
    OR = exp(estimate),
    CI_lower = exp(estimate - 1.96 * std.error),
    CI_upper = exp(estimate + 1.96 * std.error)
  ) %>%
  select(term, OR, CI_lower, CI_upper, p.value)

multivar_results
}

# merged_final ----
#Add on final set of var and last mdif (inc. char, numeric)
merged_final <- merged_gp %>%
  mutate(across(where(is.character), as.factor))


# merged_ignore ----
#: CHECK corr, naniar and drymice; rmv var/cases: obvious rmv (no value in observation) 
#and more strategic rmv (influx-outflux)
merged_ignore <- merged_final #%>% select(all_of(preliminary_set))
str(merged_ignore)

md_pattern <- md.pattern(merged_ignore)
write.csv(md_pattern,"./sasdata1/md_pattern.csv")
#md.pairs(merged_ignore)
fx <- flux(merged_ignore);
plot(fx$influx, fx$outflux, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Influx", ylab = "Outflux", main = "Flux Plot")
text(fx$influx, fx$outflux, row.names(fx), pos = 4, cex = 0.8)
# based on influx-outflux, EDI_2009 is only gonna add some noise. 
# Take it off when imputing (but keep it when running cc analysis)
# and do not use auxiliary variables. 
#you'll have urbanity and region as variables with missing values. 

merged_ignore <- merged_ignore %>%
  mutate(across(
    .cols = where(function(x) is.character(x) || (is.numeric(x) && n_distinct(x) < 10)),
    .fns = as.factor
  ))
str(merged_ignore)
dryrun <- mice(merged_ignore, maxit = 0, print = FALSE)
# Inspect results of the dry run
print(dryrun$method)
print(dryrun$predictorMatrix)
print(dryrun$nmis); print(dryrun$nmis*100/nrow(merged_ignore)); 
print(dryrun$loggedEvents)
#Remove any constant and collinear variables before imputation.

# merged_imputed ----
#imp model, beware IA/non-linear, aux var, squeeze, post and passive imputation (trsf var e.g. BMI). sensitivity anal (MNAR). Data leak (ignore).
if (do_impute) {
  
merged_imputed <- mice(merged_ignore, m=m, maxit = maxit, print = T)
## imputation dx ----
#(inc. Table1Imputed/NonImputed, density, strip) - warnings - logged events - FMI/LAMBDA ...
warnings()
merged_imputed$loggedEvents
colSums(is.na(merged_imputed %>% complete("long")))
colSums(is.na(merged_imputed %>% complete(1)))

plot(merged_imputed) #convergence
stripplot(merged_imputed)#values for imputed datasets imputed and non-imputed points
densityplot(merged_imputed)

## Table1Imputed/NonImputed ---- 
#compare merged_ignore and merge_imputed
source("./sasdata1/compare_imp_nonimp.R")
#save(table1, file=paste0("./sasdata1/Table1_imp_nonimp.RData"))

}

# merged_listwise ----
#compare included-full sample; calculate attrition weights if necessary
merged_ignore <- merged_ignore %>% filter(!Psychosis_Type=="Short") #we're not going to use Psychosis_Type=Short
str(merged_ignore)
merged_ignore_full <- merged_ignore[complete.cases(merged_ignore %>% select(all_of(preliminary_set))),]
merged_listwise  <- merged_ignore_full

#checks: Psychosis_Type as 1st  ----
#April 2026
ids_merged_listwise <- merged_listwise$BEN_IDT_ANO
dc_merged_cohort_defaveur_pmsi %>%
  filter(BEN_IDT_ANO %in% ids_merged_listwise) %>%
  mutate(which_MH = case_when(
    is.na(MED_MTF_COD) ~ NA_character_, ###
    grepl("^F28|^F29", MED_MTF_COD) ~ "Unspecified",
    grepl("^F21", MED_MTF_COD)      ~ "SZTyp",
    grepl("^F20", MED_MTF_COD)      ~ "SCZ",
    grepl("^F25", MED_MTF_COD)      ~ "SZAff",
    grepl("^F23", MED_MTF_COD)      ~ "Short",
    grepl("^F22|^F24", MED_MTF_COD) ~ "TbleDelir",
    TRUE ~ "Other"#no other dx and no NA, all dx are F2x
  )) %>%
  select(which_MH, IMB_ALD_DTD, IMB_ALD_DTF, BEN_IDT_ANO) -> dc_merged_cohort_defaveur_pmsi_inMergedListwise
#same number of distinct individuals as in merged_listwise

#append all dx to individuals listed in merged_listwise
merged_listwise_alldx <- merged_listwise %>%
  left_join(dc_merged_cohort_defaveur_pmsi_inMergedListwise, by="BEN_IDT_ANO") %>%
  select(Psychosis_Type, which_MH, IMB_ALD_DTD, IMB_ALD_DTF, BEN_IDT_ANO)

merged_listwise_alldx_incongruence <- merged_listwise_alldx %>%
  mutate(IMB_ALD_DTF=case_when(year(as.Date(IMB_ALD_DTF))<2006 ~ "2026/01/01",
                               TRUE ~ as.character(IMB_ALD_DTF)
  )) %>%
  group_by(BEN_IDT_ANO) %>%
  # 1. Keep only patients with at least one mismatch
  filter(any(Psychosis_Type != which_MH)) %>%
  
  # 2. Arrange by date to identify the 'First' diagnosis accurately
  arrange(as.Date(IMB_ALD_DTF), .by_group = TRUE) %>%
  
  mutate(
    # 3. Column for the 1st diagnosis (the which_MH at the earliest date)
    first_dx = first(which_MH),
    
    # 4. Logic for the most frequent diagnosis (freqDx)
    freqDx = {
      # Count occurrences of each which_MH for this patient
      counts <- table(which_MH)
      # Find the maximum frequency
      max_freq <- max(counts)
      # Get all names that match that maximum frequency
      modes <- names(counts)[counts == max_freq]
      # Merge them with a separator if there's a tie
      paste(sort(modes), collapse = " / ")
    }
  ) %>%
  mutate(
    match_first = case_when(
      # 1. Match with the very first diagnosis recorded
      Psychosis_Type == first_dx ~ "YES",
      TRUE ~ "No"),
    match_freq = case_when(
      # 2. Match with the most frequent (handles ties/merged strings)
      # We use mapply/grepl to check if Psychosis_Type is inside freqDx
      mapply(grepl, Psychosis_Type, freqDx) ~ "Yes",
      # 3. If it doesn't match either
      TRUE ~ "No")
  ) %>%
  ungroup() %>%
  distinct(BEN_IDT_ANO, .keep_all = T)

table(merged_listwise_alldx_incongruence$match_first, useNA = "always")
table(merged_listwise_alldx_incongruence$match_freq, useNA = "always")

# redo merged_listwise w/ correct 1st Dx ----
#because merged_ was created with a simple distinct that did not
#pre-arrange dates of ALD start!!
mismatch_first <- merged_listwise_alldx_incongruence %>%
  filter(match_first == "No") %>% 
  select(BEN_IDT_ANO, first_dx)
merged_listwise_old <- merged_listwise 
merged_listwise %>%
  left_join(mismatch_first, by="BEN_IDT_ANO") %>%
  mutate(Psychosis_Type=case_when(!is.na(first_dx) ~ first_dx,
                                  TRUE ~ Psychosis_Type)) -> merged_listwise
table(merged_listwise$Psychosis_Type, useNA = "always")
table(merged_listwise_old$Psychosis_Type, useNA = "always")


#  dx stb ----
#add on April 2026

merged_listwise_dx_stb <- merged_listwise %>% 
  left_join(dc_merged_cohort_defaveur_pmsi %>% select(BEN_IDT_ANO, IMB_ALD_DTD, IMB_ALD_DTF, MED_MTF_COD), by="BEN_IDT_ANO") %>%
  
  # 1. Group by Patient ID
  group_by(BEN_IDT_ANO) %>%
  
  # 2. Sort by the date of the diagnosis (IMB_ALD_DTF)
  # We use as.Date inside arrange to ensure chronological sorting
  arrange(as.Date(IMB_ALD_DTF), .by_group = TRUE) %>%
  
  # 3. Create the 'step' column (1 for first visit, 2 for second, etc.)
  # row_number() assigns the sequence based on the sorted order above
  mutate(step = row_number()) %>%
  
  # 4. Recode the Medical Motif Codes using case_when and grepl
  mutate(MED_MTF_COD_recode = case_when(
    is.na(MED_MTF_COD) ~ "NA",
    grepl("^F28|^F29", MED_MTF_COD) ~ "Unspecified",
    grepl("^F21", MED_MTF_COD)      ~ "SZTyp",
    grepl("^F20", MED_MTF_COD)      ~ "SCZ",
    grepl("^F25", MED_MTF_COD)      ~ "SZAff",
    grepl("^F23", MED_MTF_COD)      ~ "Short",
    grepl("^F22|^F24", MED_MTF_COD) ~ "TbleDelir",
    # TRUE                            ~ as.character(MED_MTF_COD) # Safety net for codes not in your list
    TRUE                            ~ "Other"# Safety net for codes not in your list
    
  )) %>%
  
  # 5. Remove grouping so future operations don't get slowed down
  ungroup()

# transition matrix ----

# 1. Create 'Lagged' pairs for every single transition
all_transitions <- merged_listwise_dx_stb %>%
  group_by(BEN_IDT_ANO) %>%
  arrange(step) %>%
  mutate(
    current_dx = MED_MTF_COD_recode,
    next_dx = lead(MED_MTF_COD_recode) # Gets the diagnosis of the next step
  ) %>%
  filter(!is.na(next_dx)) %>% # Remove the last step because there is no 'next'
  ungroup()

# 2. Generate the Frequency Matrix
raw_matrix <- table(all_transitions$current_dx, all_transitions$next_dx)

# 3. Convert to Probabilities (Row Percentages)
# This shows: "If a patient is at Dx 'i', what is the % chance their NEXT Dx is 'j'?"
prob_matrix <- prop.table(raw_matrix, 1)

# 4. View the result
print(round(prob_matrix, 2))

# too much change in Short Psych ----
merged_listwise_withShort <- merged_listwise

merged_listwise <- merged_listwise %>% filter(!Psychosis_Type=="Short") 

# merged_sensit ----
summary(merged_listwise$TIME_SINCE_ALD2);hist(merged_listwise$TIME_SINCE_ALD2)
table(merged_listwise$ALD_nrecords)

merged_sensit_sev <- merged_listwise %>%  filter(N_HOSPIT>0)
merged_sensit_ald <- merged_listwise %>% filter(!BEN_IDT_ANO %in% ids_oneDx_diffDx)
merged_sensit_dte <- merged_listwise %>%  mutate(TIME_SINCE_ALD=TIME_SINCE_ALD2) %>% filter(!is.na(TIME_SINCE_ALD))


# saving ----
source("./sasdata1/compare_inc_full.R")
save(table1, file=paste0("./sasdata1/Table1_inc_noninc.RData"))

save(merged_listwise_old, merged_listwise_withShort, merged_listwise, merged_listwise_dx_stb, merged_sensit_sev, merged_sensit_ald, merged_sensit_dte,
     file=paste0("./sasdata1/merged_imputed_listwise.RData"))
