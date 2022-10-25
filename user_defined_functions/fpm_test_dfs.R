#' Tests DFs for FPMs
#' 
#' Test of degrees of freedome (DFs) for Flexible Paramteric Survival Models
#' (FPMs).
#' 
#' @param formula
#'    A R-formula that is passed to the stpm2 call.
#'    
#' @param dfs_bh
#'    A vector of DFs to be tested for the baseline hazard function.
#'    
#' @param dfs_tvc
#'    A list of variables with a vector of DFs that should be tested for, e.g.
#'    `list(case = 1:10, female = 1:3)`.
#'    
#' @param same_dfs_tvc
#'    If `TRUE` no combinations of DFs between the different tvc varaibles will
#'    be tested. Instead only DFs from the minimum DF specified until the
#'    largest specified DF in `dfs_tvc` will be tested for all variables
#'    specified in `dfs_tvc` at the same time.
#'
#' @export fpm_test_dfs

fpm_test_dfs <- function(formula,
                         dfs_bh  = 0,
                         dfs_tvc = NULL,
                         by_vars = NULL,
                         same_dfs_tvc = FALSE,
                         data){
  
  # Test DFs for non-stratified models
  if(is.null(by_vars)){
    
    test_dfs(formula, dfs_bh, dfs_tvc, same_dfs_tvc, data)
    
    # Test DFs for stratified models
  } else {
    
    data <- tidyr::unite(data, filter_vars, all_of(by_vars))
    
    by(data,
       data$filter_vars,
       function(x) test_dfs(formula, dfs_bh, dfs_tvc, same_dfs_tvc, x))
  }
  
}

test_dfs <- function(formula,
                     dfs_bh,
                     dfs_tvc,
                     same_dfs_tvc,
                     data){
  
  if(is.null(dfs_tvc)){
    
    out <- purrr::map_df(.x = dfs_bh,
                         .f = ~ test_df(df_bh   = .x, 
                                        df_tvc  = NULL,
                                        formula = formula,
                                        data    = data))
    
    
  } else {
    
    if(same_dfs_tvc){
      
      tmp <- expand.grid(dfs_bh  = dfs_bh,
                         dfs_tvc = min(unlist(dfs_tvc)):max(unlist(dfs_tvc)))
      
      tmp <- cbind(tmp[-2], setNames(rep(data.frame(tmp$dfs_tvc), 
                                         length(names(dfs_tvc))), 
                                     names(dfs_tvc)))
      
    } else {
      
      tmp <- expand.grid(c(list(dfs_bh = dfs_bh),
                           dfs_tvc))
    }
    
    out <- purrr::map_df(.x = seq_len(nrow(tmp)),
                         .f = ~ test_df(df_bh   = tmp$dfs_bh[[.x]], 
                                        df_tvc  = as.list(tmp[.x, ])[-1],
                                        formula = formula,
                                        data    = data))
  }
  
  # Improve naming
  if(!any(grepl("df_tvc.*", colnames(out)))){
    
    out <- setNames(out,
                    gsub(names(dfs_tvc), 
                         paste0("dfs_tvc_", names(dfs_tvc)),
                         colnames(out),
                         fixed = TRUE))
    
  }
  
  out <- dplyr::rename_with(out, ~ gsub("\\.", "_", .x))
  
  return(out)
  
}

test_df <- function(formula,
                    df_bh, 
                    df_tvc = NULL,
                    data){
  
  argument_list <- list(formula = formula,
                        data = data,
                        df   = df_bh,
                        tvc  = NULL)
  
  # Add tvc argument if tvc > 0
  if(!is.null(df_tvc)){ 
    
    argument_list$tvc <- df_tvc 
    
  } else {
    
    df_tvc <- 0
    
  }
  
  #Create model call which return NULL if model does not converge
  model_call <- purrr::possibly(~ do.call(rstpm2::stpm2,
                                          args = argument_list),
                                otherwise = NULL)
  
  model <- model_call()
  
  
  # Obtain AIC and BIC criteria
  if(is.null(model)){
    
    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = Inf,
                      bic    = Inf)
    
  } else {
    
    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = AIC(model),
                      bic    = BIC(model))
  }
  
  return(out)
}