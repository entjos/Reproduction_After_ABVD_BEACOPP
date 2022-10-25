#' Compare Model Estimates
#' 
#' A function that combines esimtates from multiple regression models into
#' one data frame.
#' 
#' @param models
#' A named list of regression models. The names of the models will be used
#' as suffix for the model estimates in the combined data frame.
#' 
#' @return 
#' A wide `data.frame` in which each row represents one variable and each
#' column different estimates from each model.
#' 
#' @export compare_model_estimates

compare_model_estimates <- function(models){
  
  # Create unique column names
  tidy_models <- lapply(seq_along(models), function(i){
    
    tidy_model <- broom::tidy(models[[i]], conf.int = TRUE)
    
    new_colnames <- paste(colnames(tidy_model), 
                          names(models)[[i]], 
                          sep = "_")[-1]        # Keep term column for matching
    
    new_colnames <- gsub("\\.", "_", new_colnames)
    
    colnames(tidy_model)[-1] <- new_colnames
    
    return(tidy_model)
    
  })
  
  # Merge estimates into model data frame
  out <- Reduce(function(model1, model2){
    merge(model1,
          model2,
          by  = "term",
          all = TRUE)
  },
  tidy_models)
  
  # Return combined data frame
  return(out)
  
}
