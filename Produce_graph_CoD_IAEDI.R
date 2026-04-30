library(tidyverse)
rm(list=ls())
pattern <- "bsl_IAEDI_POGEE_"# "bsl_IAEDI_POGEE_" "bsl_IAEDI_ipw_POGEE_" "sensit_dte_IAEDI_POGEE_" "sensit_sev_IAEDI_POGEE_"  "sensit_ald_IAEDI_POGEE_" 
#"bsl_IAEDI_ipw_POGEE_" "sensit_dte_IAEDI_POGEE_" "sensit_sev_IAEDI_POGEE_"  "sensit_ald_IAEDI_POGEE_" 

path <- "./sasdata1/download/"
natural_order <- c("nat",#
                   "Infectious_and_parasitic",
                   "Circulatory_system",
                   "Digestive_system",
                   "Nervous_system",
                   "Respiratory_system",
                   "Endocrine_nutritional_metabolic",
                   "Mental_behavioral_disorders",
                   "Neoplasms",
                   "Symptoms_signs_findings_NOS",
                   "Other"
)

unnatural_order <- c('unnat',#
                     "Suicide",
                     "Accidents",
                     "External_causes"
)

# Define the "Good" labels for the Y-axis
natural_labels <- c("All Natural",#
                    "Certain infectious and parasitic diseases", "Diseases of the circulatory system", 
                    "Diseases of the digestive system", "Diseases of the nervous system", 
                    "Diseases of the respiratory system", "Endocrine, nutritional, and metabolic diseases", 
                    "Mental and behavioral disorders", "Neoplasms", "Symptoms, signs and abnormal findings NOS", 
                    "Other Natural Causes")
natural_labels <- c("All Natural", # 
                    "Infectious", "Circulatory", 
                    "Digestive", "Nervous", 
                    "Respiratory", "Endocrine", 
                    "Mental", "Neoplasms", "Abnormal NOS", 
                    "Other nat.")

unnatural_labels <- c("All Unnatural",#
                      "Suicide", "Accidents", "External causes")
unnatural_labels <- c("All Unnatural",#
                      "Suicide", "Accidents", "Other unnat.")

# Create a lookup named vector for mapping
label_lookup <- c(setNames(natural_labels, natural_order), 
                  setNames(unnatural_labels, unnatural_order))

# 2. Define your specific color code
psychosis_colors <- c(
  "Delus."      = "black",     # Mapping TbleDelir to your data label
  "SZAff"       = "blue",
  "SZTyp"       = "orange",
  "SCZ"         = "red",       # Note: SCZ is the baseline (0), but included for legend
  "Unsp."       = "darkgreen"
)

# 2. Identify the files (excluding the summary files nat, unnat, surv)
all_files <- list.files(path, pattern = paste0("^", pattern, ".*\\.csv$"), full.names = TRUE)
# Filter out the summary files
#cause_files <- all_files[!grepl("nat|unnat|surv", all_files)]
cause_files <- all_files[!grepl("surv", all_files)]

# 3. Import and Process Data
plot_data <- map_df(cause_files, function(file) {
  # Extract cause name from filename (removes path and prefix)
  cause_id <- gsub(paste0(pattern, "|.csv"), "", basename(file))
  
  df <- read.csv(file)
  if (cause_id=="nat" | cause_id=="unnat") df <- df[,-1]
  
  # Map rows to Diagnoses (Skip row 1 which is headers/labels)
  # Based on your description: Row 2=SZAff, 3=SZTyp, 4=Delus, 5=Unsp
  df_clean <- tail(df,4)
  colnames(df_clean) <- c("Estimate", "StdError", "Wald", "PValue")
  
  df_clean %>%
    mutate(
      Cause_ID = cause_id,
      Diagnosis = c("SZAff", "SZTyp", "Delus.", "Unsp."),
      #across(everything(), as.numeric),
      Facet = ifelse(Cause_ID %in% unnatural_order, "Unnatural Causes", "Natural Causes")
    )
})

# 4. Cleanup Labels, Adjust P-values, and Set Order
plot_data <- plot_data %>%
  mutate(
    # Apply the "Good" labels
    Cause_Label = label_lookup[Cause_ID],
    # FDR Adjustment
    AdjP = p.adjust(PValue, method = "fdr"),
    Significant = ifelse(p.adjust(PValue, method = "fdr") < 0.05, "Yes", "No"),
    # Convert to factors for ordering
    Cause_Label = factor(Cause_Label, levels = rev(c(natural_labels, unnatural_labels))),
    Facet = factor(Facet, levels = c("Natural Causes", "Unnatural Causes")),
    Diagnosis = factor(Diagnosis, levels = c("SZAff", "SZTyp", "Delus.", "Unsp."))
  )


# 4. Redefine the Diagnosis order. 
# We reverse it here because ggplot dodges from bottom-to-top relative to the factor levels.
plot_data$Diagnosis <- factor(plot_data$Diagnosis, 
                              levels = rev(c("SZAff", "SZTyp", "Delus.", "Unsp.")))

# 5. Generate the Plot
p <- ggplot(plot_data, aes(x = Estimate, y = Cause_Label, color = Diagnosis, group = Diagnosis)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = Estimate - 1.96 * StdError, 
                     xmax = Estimate + 1.96 * StdError),
                 position = position_dodge(width = 0.7), height = 0.3) +
  
  # Main Points with Variable Size
  geom_point(aes(size = Significant), 
             position = position_dodge(width = 0.7)) +
  
  # Manual controls
  scale_size_manual(values = c("No" = 1.2, "Yes" = 4), guide = "none") +
  
  # FIXED LINE: We use 'breaks' to force the legend order back to SZAff -> Unsp.
  scale_color_manual(values = psychosis_colors, 
                     breaks = c("SZAff", "SZTyp", "Delus.", "Unsp.")) + 
  
  facet_wrap(~Facet, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    # title = "RMST Difference vs. Schizophrenia (SCZ) by Cause of Death",
    # subtitle = "Large points indicate FDR-adjusted significance (p < 0.05); Bars represent 95% CI",
    x = "RMST Difference (Days)",
    y = NULL,
    color = "","Diagnosis Group"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_rect(fill = "gray95", color = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    
    panel.spacing = unit(2, "lines"),
    axis.text=element_text(size = 12)
    
  )

print(p)