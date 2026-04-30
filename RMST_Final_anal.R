library(survival)
library(prodlim)
library(ipw)

#hdr ----

#merged_listwise$time <- merged_listwise$time/365 %>% round
dt <-10#computational issues if dt <- 1
tau <- max(merged_listwise$time)
times <- seq(1, tau, by = dt)  # discretized times up to restriction time tau
time <- merged_listwise$time; 


# IPW ----
w_ipw <- ipwpoint(
  exposure   = Psychosis_Type,
  family     = "multinomial",
  #link       = "logit",
  # numerator: stabilization factors (often subset of baseline covariates)
  numerator   = ~ 1,
  # denominator: full confounder set (includes numerator vars)
  denominator = ~ Sex + AGE_AT_ALD + Calendar_year+ EDIq_2009+URBANcat_2009,#models the full treatment mechanism conditional on confounders
  data       = as.data.frame(merged_listwise)
)
ipwplot(weights = w_ipw$ipw.weights, logscale = FALSE,
        main = "Stabilized weights", xlim = c(0, 8))
summary(w_ipw$ipw.weights)
# extract stabilized weights
merged_listwise$sw <- w_ipw$ipw.weights

#function to calculate coefficient
#anal = survival or competing risk
#var = on death_or_not (cause=1), cause_nat_unnat (cause=1 or 2) or cause_category (cause=1 ie suicide)
#weights = NULL or sw
#POGEE ----
POGEE <- function(data, anal="RMST_survival", var="death_or_not", cause=1, weights = NULL) { 
  
  status <- data %>% select(var) %>% pull
  table(status)
  f <- prodlim(Hist(time, status)~1, data=data)
  pv <- jackknife(f, times = times, cause = cause)  # pseudo-values matrix: subjects x times
  #same if CR model
  # but if CR: pv are CIF pseudo-values, have to transform to make it survival
  # Overall Survival (Non-Competing): pv are already survival pseudo-values - no transformation!!!
  
  ## RMST survival ----
  if (anal=="RMST_survival") {
  outcome <- apply(pv, 1, function(s) {
    sum(diff(times) * (head(s, -1) + tail(s, -1)) / 2)
  })
  } else if (anal=="RMST_cr") {
  ## RMST competing risks ----
  outcome <- apply(pv, 1, function(s) {
    surv_pv <- 1 - s  # CIF → survival
    sum(diff(times) * (head(surv_pv, -1) + tail(surv_pv, -1)) / 2)
  }) 
  } else if (anal=="surv_prob_6000") {
  ## survival probabilities at time 6000 ----
  outcome <- pv[,601] 
  } else if (anal=="surv_prob_4000") {
  ## survival probabilities at time 4000 ----
  outcome <- pv[,401]   
  } else if (anal=="surv_prob_2000") {
    ## survival probabilities at time 2000 ----
    outcome <- pv[,201]   
  } else if (anal=="surv_prob_cr_6000") {
  ## CR - survival probabilities at time 6000 ----
  outcome <- 1- pv[,601] 
  } else if (anal=="surv_prob_cr_4000") {
  ## CR -survival probabilities at time 4000 ----
  outcome <- 1- pv[,401]   
  } else if (anal=="quant_sf") {
  ## quantile of the survival function ----
  # Full sample quantile
  q_full <- quantile(f, q = 0.05, cause = cause)  # e.g., 95th percentile (S(t)=0.05)
  # Jackknife loop: compute quantile leaving out each subject
  n <- nrow(data)
  pseudo_q <- numeric(n)
  
  for (i in 1:n) {
    data_loo <- data[-i, ]
    time <- data_loo$time; status <- data_loo %>% select(var) %>% pull
    f_loo <- prodlim(Hist(time, status) ~ 1, data = data_loo)
    q_loo <- quantile(f_loo, q = 0.05, cause = cause)
    pseudo_q[i] <- n * q_full$quantile - (n-1) * q_loo$quantile
    print(i)
    }
  outcome <- pseudo_q
  }
  
  #### model ----
  data <- data %>% mutate(id=1:nrow(data))
  gee_model <- geepack::geeglm(outcome ~  
                                 Psychosis_Type + EDIq_2009 + Sex + AGE_AT_ALD + Calendar_year+URBANcat_2009, 
                                 #Psychosis_Type * EDIq_2009 + Sex + AGE_AT_ALD + Calendar_year+URBANcat_2009, 
                              id=id,
                              weights = weights,#NULL,#sw,
                              data = data, corstr = "independence"); summary(gee_model)
  summary(gee_model)$coefficients[2:5,]#5 or 6 disorders
  #summary(gee_model)$coefficients#5 or 6 disorders
  
}

## run POGEE anal ----
#anal="RMST_survival", anal="RMST_cr", anal="surv_prob_6000"), anal="surv_prob_4000", anal="quant_sf"
#var = on death_or_not (cause=1), cause_nat_unnat (cause=1 or 2) or cause_category (cause=1 ie suicide)
#for cr models, anal with natural (1) vs. unnatural (2)
#anal with suicide vs. assault vs. accidents vs. other unnatural
# beware 7hrs to run POGEE on quantile of the survival function

#Survival table
POGEE_surv <- POGEE(data=merged_listwise, anal="RMST_survival", var="death_or_not", cause=1, weights = NULL) #%>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_6000", var="death_or_not", cause=1, weights = NULL)) %>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_4000", var="death_or_not", cause=1, weights = NULL)) %>% 
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_2000", var="death_or_not", cause=1, weights = NULL)) #%>%

#UNNatural cause table
POGEE_unnat <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_nat_unnat", cause=1, weights = NULL) #%>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_6000", var="cause_nat_unnat", cause=1, weights = NULL)) %>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_4000", var="cause_nat_unnat", cause=1, weights = NULL)) #%>%
#rbind(POGEE(data=merged_listwise, anal="quant_sf", var="cause_nat_unnat", cause=1, weights = NULL))

#Natural cause table
POGEE_nat <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_nat_unnat", cause=2, weights = NULL) #%>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_6000", var="cause_nat_unnat", cause=2, weights = NULL)) %>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_4000", var="cause_nat_unnat", cause=2, weights = NULL)) #%>%
  #rbind(POGEE(data=merged_listwise, anal="quant_sf", var="cause_nat_unnat", cause=2, weights = NULL))

POGEE_allcauses <- list()
for (i in 1:max(merged_listwise$cause_id_rgp)) {
POGEE_allcauses[[i]] <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_id_rgp", cause=i, weights = NULL) 
print(i)
}

save(POGEE_allcauses,POGEE_surv, POGEE_nat, POGEE_unnat, 
     file=paste0("./sasdata1/download/",analysis, "_POGEE.RData"))


##same with IPW ----
if (analysis=="bsl") {
#Survival table
POGEE_surv <- POGEE(data=merged_listwise, anal="RMST_survival", var="death_or_not", cause=1, weights = merged_listwise$sw) #%>%
#rbind(POGEE(data=merged_listwise, anal="surv_prob_6000", var="death_or_not", cause=1, weights = merged_listwise$sw)) %>%
#rbind(POGEE(data=merged_listwise, anal="surv_prob_4000", var="death_or_not", cause=1, weights = merged_listwise$sw)) #%>%
#rbind(POGEE(data=merged_listwise, anal="quant_sf", var="death_or_not", cause=1, weights = merged_listwise$sw))

#Natural cause table
POGEE_nat <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_nat_unnat", cause=1, weights = merged_listwise$sw) #%>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_6000", var="cause_nat_unnat", cause=1, weights = merged_listwise$sw)) %>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_4000", var="cause_nat_unnat", cause=1, weights = merged_listwise$sw)) #%>%
#rbind(POGEE(data=merged_listwise, anal="quant_sf", var="cause_nat_unnat", cause=1, weights = merged_listwise$sw))

#UNNatural cause table
POGEE_unnat <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_nat_unnat", cause=2, weights = merged_listwise$sw) #%>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_6000", var="cause_nat_unnat", cause=2, weights = merged_listwise$sw)) %>%
  #rbind(POGEE(data=merged_listwise, anal="surv_prob_cr_4000", var="cause_nat_unnat", cause=2, weights = merged_listwise$sw)) #%>%
#rbind(POGEE(data=merged_listwise, anal="quant_sf", var="cause_nat_unnat", cause=2, weights = merged_listwise$sw))

POGEE_allcauses <- list()
for (i in 1:max(merged_listwise$cause_id_rgp)) {
  POGEE_allcauses[[i]] <- POGEE(data=merged_listwise, anal="RMST_cr", var="cause_id_rgp", cause=i, weights = merged_listwise$sw) 
  print(i)
}

save(POGEE_allcauses,POGEE_surv, POGEE_nat, POGEE_unnat, 
     file=paste0("./sasdata1/download/",analysis, "_ipw_POGEE.RData"))
}