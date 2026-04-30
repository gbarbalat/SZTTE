library(rstatix)     # for cohen_d
library(DescTools)   # for CramerV


make_table1 <- function(merged_ignore, merged_imputed, i_imp=1) {
  
  # If multiple imputations, pick one completed dataset for display
  if (inherits(merged_imputed, "mids")) {
    merged_imputed <- complete(merged_imputed, i_imp)
  }
  
  vars <- intersect(names(merged_ignore), names(merged_imputed))
  res_list <- list()
  
  for (v in vars) {
    is_imp <- is.na(merged_ignore[[v]])
    v_obs  <- merged_ignore[[v]]#[!is_imp]
    v_imp  <- merged_imputed[[v]]#[is_imp]
    
    if (is.numeric(merged_imputed[[v]])) {
      # ---- continuous
      test <- tryCatch(t.test(v_obs, v_imp), error = function(e) NULL)
      smd <- tryCatch({
        df <- data.frame(val = c(v_obs, v_imp),
                         grp = rep(c("obs","imp"), c(length(v_obs), length(v_imp))))
        cohens_d(val ~ grp, data = df)$effsize
      }, error = function(e) NA_real_)
      
      row <- tibble(
        Variable = v,
        Level = "",
        Obs = if (length(v_obs)>0) sprintf("%.2f (%.2f)", mean(v_obs, na.rm=TRUE), sd(v_obs, na.rm=TRUE)) else NA,
        Imp = if (length(v_imp)>0) sprintf("%.2f (%.2f)", mean(v_imp, na.rm=TRUE), sd(v_imp, na.rm=TRUE)) else NA,
        p_value = if (!is.null(test)) signif(test$p.value, 3) else NA,
        Effect = round(smd, 3)
      )
      res_list[[v]] <- row
      
    } else {
      # ---- categorical
      v_obs <- factor(v_obs)
      v_imp <- factor(v_imp, levels = levels(v_obs))
      tbl <- table(c(v_obs, v_imp), rep(c("obs","imp"), c(length(v_obs), length(v_imp))))
      chi <- tryCatch(suppressWarnings(chisq.test(tbl)), error = function(e) NULL)
      crv <- tryCatch(CramerV(tbl, bias.correct=TRUE), error = function(e) NA_real_)
      
      # main row (variable name, with p + cramer)
      main_row <- tibble(
        Variable = v,
        Level = "",
        Obs = "",
        Imp = "",
        p_value = if (!is.null(chi)) signif(chi$p.value, 3) else NA,
        Effect = round(crv, 3)
      )
      
      # rows per level
      levs <- union(levels(v_obs), levels(v_imp))
      obs_tab <- table(v_obs)
      imp_tab <- table(v_imp)
      lev_rows <- tibble(
        Variable = "",
        Level = levs,
        Obs = paste0(obs_tab[levs], " (", round(100*prop.table(obs_tab)[levs],1), "%)"),
        Imp = paste0(imp_tab[levs], " (", round(100*prop.table(imp_tab)[levs],1), "%)"),
        p_value = NA,
        Effect = NA
      )
      
      res_list[[v]] <- bind_rows(main_row, lev_rows)
    }
  }
  
  bind_rows(res_list)
}

# ---- Run it ----
table1 <- make_table1(merged_ignore, merged_imputed)
print(table1, n=Inf)
