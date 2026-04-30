library(dplyr)
library(broom)
library(survival)


trsf_df <- function(data) {
  
  data <- data %>%
    mutate(
      # columns 1 and 2: row-dependent rounding
      across(
        1:2,
        ~ if_else(
          row_number() <= 5,
          round(.x, 1),
          round(.x, 3)
        )
      ),
      
      # column 3: always 2 decimals
      across(
        3,
        ~ round(.x, 2)
      ),
      
      # column 4: always 3 decimals
      across(
        4,
        ~ round(.x, 3)
      )
    )
  
}


# 1. Define the names based on your categories
cause_names <- c(
  "Accidents",                                  # 1
  "Other",                                      # 2
  "Infectious_and_parasitic",                   # 3
  "Circulatory_system",                         # 4
  "Digestive_system",                           # 5
  "Nervous_system",                             # 6
  "Respiratory_system",                         # 7
  "Endocrine_nutritional_metabolic",            # 8
  "External_causes",                            # 9
  "Mental_behavioral_disorders",                 # 10
  "Neoplasms",                                  # 11
  "Suicide",                                    # 12
  "Symptoms_signs_findings_NOS"                 # 13
)

#bsl POGEE ----
load("./sasdata1/download/bsl_IAEDI_POGEE.RData")
POGEE_surv <- trsf_df(POGEE_surv); write.csv(POGEE_surv, "./sasdata1/download/bsl_IAEDI_POGEE_surv.csv")
POGEE_nat <- trsf_df(POGEE_nat); write.csv(POGEE_nat, "./sasdata1/download/bsl_IAEDI_POGEE_nat.csv")
POGEE_unnat <- trsf_df(POGEE_unnat); write.csv(POGEE_unnat, "./sasdata1/download/bsl_IAEDI_POGEE_unnat.csv")
# 2. Assign names to your list
names(POGEE_allcauses) <- cause_names

# 3. Use a loop or purrr::iwalk to transform and save
# This will go through each data frame, apply trsf_df, and save to your path
lapply(names(POGEE_allcauses), function(nm) {
  
  # Extract and transform the dataframe
  temp_df <- trsf_df(POGEE_allcauses[[nm]])
  
  # Construct the file path using the name
  file_path <- paste0("./sasdata1/download/bsl_IAEDI_POGEE_", nm, ".csv")
  
  # Write the CSV
  write.csv(temp_df, file = file_path, row.names = FALSE)
  
  # Return nothing (or the name for tracking)
  return(paste("Saved:", file_path))
})

# sensit POGEE ----
##sev ----
load("./sasdata1/download/sensit_sev_IAEDI_POGEE.RData")
POGEE_surv <- trsf_df(POGEE_surv); write.csv(POGEE_surv, "./sasdata1/download/sensit_sev_IAEDI_POGEE_surv.csv")
POGEE_nat <- trsf_df(POGEE_nat); write.csv(POGEE_nat, "./sasdata1/download/sensit_sev_IAEDI_POGEE_nat.csv")
POGEE_unnat <- trsf_df(POGEE_unnat); write.csv(POGEE_unnat, "./sasdata1/download/sensit_sev_IAEDI_POGEE_unnat.csv")
# 2. Assign names to your list
names(POGEE_allcauses) <- cause_names

# 3. Use a loop or purrr::iwalk to transform and save
# This will go through each data frame, apply trsf_df, and save to your path
lapply(names(POGEE_allcauses), function(nm) {
  
  # Extract and transform the dataframe
  temp_df <- trsf_df(POGEE_allcauses[[nm]])
  
  # Construct the file path using the name
  file_path <- paste0("./sasdata1/download/sensit_sev_IAEDI_POGEE_", nm, ".csv")
  
  # Write the CSV
  write.csv(temp_df, file = file_path, row.names = FALSE)
  
  # Return nothing (or the name for tracking)
  return(paste("Saved:", file_path))
})

##ald ----
load("./sasdata1/download/sensit_ald_IAEDI_POGEE.RData")
POGEE_surv <- trsf_df(POGEE_surv); write.csv(POGEE_surv, "./sasdata1/download/sensit_ald_IAEDI_POGEE_surv.csv")
POGEE_nat <- trsf_df(POGEE_nat); write.csv(POGEE_nat, "./sasdata1/download/sensit_ald_IAEDI_POGEE_nat.csv")
POGEE_unnat <- trsf_df(POGEE_unnat); write.csv(POGEE_unnat, "./sasdata1/download/sensit_ald_IAEDI_POGEE_unnat.csv")
# 2. Assign names to your list
names(POGEE_allcauses) <- cause_names

# 3. Use a loop or purrr::iwalk to transform and save
# This will go through each data frame, apply trsf_df, and save to your path
lapply(names(POGEE_allcauses), function(nm) {
  
  # Extract and transform the dataframe
  temp_df <- trsf_df(POGEE_allcauses[[nm]])
  
  # Construct the file path using the name
  file_path <- paste0("./sasdata1/download/sensit_ald_IAEDI_POGEE_", nm, ".csv")
  
  # Write the CSV
  write.csv(temp_df, file = file_path, row.names = FALSE)
  
  # Return nothing (or the name for tracking)
  return(paste("Saved:", file_path))
})

##dte ----
load("./sasdata1/download/sensit_dte_IAEDI_POGEE.RData")
POGEE_surv <- trsf_df(POGEE_surv); write.csv(POGEE_surv, "./sasdata1/download/sensit_dte_IAEDI_POGEE_surv.csv")
POGEE_nat <- trsf_df(POGEE_nat); write.csv(POGEE_nat, "./sasdata1/download/sensit_dte_IAEDI_POGEE_nat.csv")
POGEE_unnat <- trsf_df(POGEE_unnat); write.csv(POGEE_unnat, "./sasdata1/download/sensit_dte_IAEDI_POGEE_unnat.csv")
# 2. Assign names to your list
names(POGEE_allcauses) <- cause_names

# 3. Use a loop or purrr::iwalk to transform and save
# This will go through each data frame, apply trsf_df, and save to your path
lapply(names(POGEE_allcauses), function(nm) {
  
  # Extract and transform the dataframe
  temp_df <- trsf_df(POGEE_allcauses[[nm]])
  
  # Construct the file path using the name
  file_path <- paste0("./sasdata1/download/sensit_dte_IAEDI_POGEE_", nm, ".csv")
  
  # Write the CSV
  write.csv(temp_df, file = file_path, row.names = FALSE)
  
  # Return nothing (or the name for tracking)
  return(paste("Saved:", file_path))
})

##ipw ----
load("./sasdata1/download/bsl_IAEDI_ipw_POGEE.RData")
POGEE_surv <- trsf_df(POGEE_surv); write.csv(POGEE_surv, "./sasdata1/download/bsl_IAEDI_ipw_POGEE_surv.csv")
POGEE_nat <- trsf_df(POGEE_nat); write.csv(POGEE_nat, "./sasdata1/download/bsl_IAEDI_ipw_POGEE_nat.csv")
POGEE_unnat <- trsf_df(POGEE_unnat); write.csv(POGEE_unnat, "./sasdata1/download/bsl_IAEDI_ipw_POGEE_unnat.csv")
# 2. Assign names to your list
names(POGEE_allcauses) <- cause_names

# 3. Use a loop or purrr::iwalk to transform and save
# This will go through each data frame, apply trsf_df, and save to your path
lapply(names(POGEE_allcauses), function(nm) {
  
  # Extract and transform the dataframe
  temp_df <- trsf_df(POGEE_allcauses[[nm]])
  
  # Construct the file path using the name
  file_path <- paste0("./sasdata1/download/bsl_IAEDI_ipw_POGEE_", nm, ".csv")
  
  # Write the CSV
  write.csv(temp_df, file = file_path, row.names = FALSE)
  
  # Return nothing (or the name for tracking)
  return(paste("Saved:", file_path))
})

