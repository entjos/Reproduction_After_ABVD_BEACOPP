
estimate_cox_models <- function(surv,
                                cox_unadjusted,
                                cox_adjusted,
                                cox_adjusted_strat,
                                cox_interaction = NULL,
                                interaction_var = NULL,
                                data){
  
  # Define left had site of furmolar
  surv_lh <- as.formula(paste(surv, "~ 1"))
  
  # Fit Cox models
  cox_unadjusted <- survival::coxph(update(surv_lh, 
                                           paste("~", cox_unadjusted)),
                                    data = data)
  
  cox_adjusted <- survival::coxph(update(surv_lh, 
                                         paste("~", cox_adjusted)),
                                  data = data)
  
  cox_adjusted_strat <- survival::coxph(update(surv_lh, 
                                               paste("~", cox_adjusted_strat)),
                                        data = data)
  
  if(!is.null(cox_interaction)){
    
    cox_interaction <- survival::coxph(update(surv_lh, 
                                              paste("~", cox_interaction)),
                                       data = data)
    
    test_interaction <- Epi::Wald(cox_interaction, 
                                   subset = paste0(interaction_var, ":")) %>% 
      as.data.frame() %>% 
      rownames_to_column("Estimate")
    
    out <- list(cox_unadjusted     = cox_unadjusted,
                cox_adjusted       = cox_adjusted,
                cox_adjusted_strat = cox_adjusted_strat,
                test_interaction   = test_interaction)
    
  } else {
    
    out <- list(cox_unadjusted     = cox_unadjusted,
                cox_adjusted       = cox_adjusted,
                cox_adjusted_strat = cox_adjusted_strat)
    
  }
  
  cat("\nThe follwoing models were fitted:\n",
      "-------------------------------------------------------------------\n")
  
  lapply(1:3, function(i){
    
    cat("Model", names(out)[[i]], "\n")
    
    cat("Test of Schoenefeld residuals")
    print(summary(out[[i]]))
    print(survival::cox.zph(out[[i]]))
    
    cat("\n-------------------------------------------------------------------\n")
  })
  
  if(!is.null(cox_interaction)){
    
    cat("Test for interaction for", interaction_var, "\n")
    print(test_interaction)
    
    cat("\n-------------------------------------------------------------------\n")
    
  }
  
  return(out)
  
}