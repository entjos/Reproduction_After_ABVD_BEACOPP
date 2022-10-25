#' Summary Table
#' 
#' Creates stratified summary table with rates.
#'
#' @param df
#'    A `data.frame` that includes the data that should be summarised.
#'
#' @param var
#'    A character vector specifying the variables that should be summarised.
#'    
#' @param strata
#'    An optional character vector specifying the variables on which the
#'    summary table should be stratified on.
#'    
#' @param overall
#'    If `TRUE` an additionall summary table will be created including the
#'    complete dataset. This can be used in combination with `strata`.
#'    
#' @param get_rates
#'    If `TRUE` the summary tables includes events rates for events specified
#'    in the `event` argument for each level of variables in `var`. Rates can
#'    only be estimated for factor variables in `var`.
#'    
#' @param  event
#'    Name of the event indicator that should be used to compute rates. 
#'    Needs to be specified if `get_rates == TRUE`.
#'    
#' @param st
#'    Name of the varaible that includes the survival time that should be used
#'    to comptue rates. Needs to be specified if `get_rates == TRUE`. 
#'    
#' @return 
#'    A `data.frame` with the following columns:
#'    - `varname`: Including the name of the variable or the name of the category for factor variables.
#'    - `mean`: The mean of the variable, i.e. the proportion for binary variables.
#'    - `p25`: The 25th percentile (only for metric variables.)
#'    - `p75`: The 75th percentile (only for metric varaibles.)
#'    - `n_category`: The number of non-NA observations for each variable or cateogry.
#'    - `n_total`: The number of non-NA observations for the variable.
#'    - `n_NA`: The number of NA observations for this variable or category. 
#'
#' @importFrom magritter %>%
#' @importFrom dplyr .data
#' @export summary_table

summary_table <- function(df, 
                          vars, 
                          strata = NULL,
                          overall = FALSE,
                          get_rates = FALSE,
                          event = NULL,
                          st = NULL){
  
  # Convert data.table to data.frame ------------------------------------------
  
  if("data.table" %in% class(df)){
    df <- as.data.frame(df)
  }
  
  # Checks --------------------------------------------------------------------
 
  if(get_rates & is.null(event) & is.null(st)){
    stop("For competing rates both the `event` and `st` 
         argument need to be specified.")
  }
  
  if(!all(vars %in% colnames(df))){
    stop("Could not find all variables specified in `vars` in `df`.")
  }
  
  if(!all(strata %in% colnames(df))){
    stop("Could not find all variables specified in `strata` in `df`.")
  }
  
  if(overall & is.null(strata)){
    message("Setting overall to TRUE without specifying the strata argument
            is most likely useless.")
  }
  
  if(get_rates & any(unlist(lapply(df[, vars], class)) != "factor")){
    stop(paste("Rates can only be estimted for factor variables in `vars`.",
               "Please make sure that all variables specified in `vars` are",
               "of class `factor`."))
  }
  
  # Create stratified summary table -------------------------------------------
  
  if(!is.null(strata)){
    
    # Get unique values of strata variables
    strata_values <- lapply(strata, function(x){unique(df[[x]])})
    
    # Create matrix with posssible combination of unique strata values
    stratas <- expand.grid(strata_values)
    
    # Subset each strat and obtain summary variables
    out_strat <- lapply(seq_len(nrow(stratas)), function(i){
      
      if(length(strata) > 1){
        
        strat_sub <- do.call(paste, df[, strata]) == do.call(paste, stratas[i, ])
        
      } else {
        
        strat_sub <- df[, strata] == stratas[i, ]
        
      }
      
      strata_df <- subset(df, strat_sub)
      
      temp_out  <- purrr::map_df(vars, 
                                 .f = function(x){
                                   var_summary(df  = strata_df,
                                               var = x
                                   )})
      
      if(get_rates){
        
        temp_out_rates <- purrr::map_df(vars, 
                                        .f = function(x){
                                          summary_rates(df  = strata_df,
                                                        var = x,
                                                        event = event,
                                                        st    = st)
                                        })
        
        return(cbind(temp_out, temp_out_rates))
        
      } else {
        return(temp_out)
      }
      
    })
    
    # Create list labels
    stratas <- stratas %>% 
      mutate(across(everything(), as.character))
    
    labs <- purrr::map(seq_len(nrow(stratas)), 
                       function(i){paste(strata, 
                                         stratas[i,], 
                                         sep = "==", 
                                         collapse = "_")
                       })
    
    # Label list elements
    names(out_strat) <- labs
    
  }
  
  # Create non-stratified summary table ---------------------------------------
  
  if(is.null(strata) | overall){
    
    out_overall <- purrr::map_df(vars, var_summary, df = df)
    
    if(get_rates){
      
      temp_out_rates <- purrr::map_df(vars, 
                                      .f = function(x){
                                        summary_rates(df  = df,
                                                      var = x,
                                                      event = event,
                                                      st    = st)
                                      })
      
      out_overall <- cbind(out_overall, temp_out_rates)
      
    }
    
  }
  
  # Define output object ------------------------------------------------------
  
  if(is.null(strata)){
    
    out <- out_overall
    
  } else if(overall){
    
    out <- append(out_strat, list(out_overall))
    
    names(out) <- c(names(out_strat),
                    paste0(paste(strata, sep = "_"), "==", "overall"))
    
  } else {
    
    out <- out_strat
  }
  
  return(out)
  
}

# var_summary function --------------------------------------------------------

var_summary <- function(df, var){
  
  # Calculate numbers and proportion in each category for factor variables
  if(class(df[[var]]) == "factor"){
    
    lev <- levels(df[[var]])
    
    lapply(seq_along(lev), function(i){
      
      mean <- mean(as.numeric(df[[var]] == lev[i]), 
                   na.rm = TRUE) %>% 
        round(3)
      
      p25  <-  NA
      
      p75  <-  NA
      
      n_category <- sum(df[[var]] == lev[i], na.rm = TRUE)
      
      n_total    <- sum(!is.na(df[[var]]))
      
      n_NA       <- sum(is.na(df[[var]]))
      
      varname <- paste(var, levels(df[[var]])[i]) %>% 
        as.character()
      
      data.frame(varname, mean, p25, p75, 
                 n_category, n_total, n_NA)
      
    }) %>% dplyr::bind_rows()
    
    # Calculate numbers and proportion in each category for dichotomiesed
    # integer variables
  } else if(class(df[[var]]) %in% c("integer", "numeric") & 
            max(df[[var]], na.rm = TRUE) == 1){
    
    mean <- mean(df[[var]], 
                 na.rm = TRUE) %>% 
      round(3)
    
    p25  <- NA
    
    p75  <- NA
    
    n_category <- sum(df[, var], na.rm = TRUE)
    
    n_total    <- sum(!is.na(df[[var]]))
    
    n_NA       <- sum(is.na(df[[var]]))
    
    data.frame(mean, p25, p75, 
               n_category, n_total, n_NA)
    
    # Calculate median, q25, q75 for numeric variables    
  } else if(class(df[[var]]) == "numeric" & max(df[[var]], 
                                                na.rm = TRUE) != 1){
    
    mean <- quantile(df[[var]], 0.5, na.rm = TRUE) %>% 
      round(1)
    
    p25  <- quantile(df[[var]], 0.25, na.rm = TRUE) %>% 
      round(1)
    
    p75  <- quantile(df[[var]], 0.75, na.rm = TRUE) %>% 
      round(1)
    
    n_category <- NA
    
    n_total    <- sum(!is.na(df[[var]]))
    
    n_NA       <- sum(is.na(df[[var]]))
    
    data.frame(mean, p25, p75, 
               n_category, n_total, n_NA)
    
  }
}

# summary_rates function ------------------------------------------------------

summary_rates <- function(df, var, event, st){
  
  # Get no. births by variable
  rate_dt <- df %>%
    dplyr::filter(!is.na(.data[[var]])) %>%           # Drop obs with NA
    dplyr::group_by(.data[[var]], .drop = FALSE) %>%
    dplyr::summarise(no_events = sum(.data[[event]]),
                     t_at_risk = sum(.data[[st]]),
                     .groups = "drop") %>% 
    dplyr::mutate(p_events = no_events / sum(no_events)) %>% 
    dplyr::select(-.data[[var]])
  
  # Calculate rates with CIs
  rate <- epiR::epi.conf(cbind(rate_dt[["no_events"]],
                               rate_dt[["t_at_risk"]]),
                         ctype = "inc.rate",
                         method = "exact")
  
  # Improve naming
  names(rate) <- paste0("rate_", names(rate))
  
  return(cbind(rate_dt, rate))
  
}