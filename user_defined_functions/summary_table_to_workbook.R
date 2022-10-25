#' Summary Table to Workbook
#' 
#' Saves a list of summary tables in seperate sheets of an excel workbook.
#' 
#' @param workbook
#'    Path to an `.xlsx` workbook in which the summary tables should be saved
#'    in.
#'    
#' @param summary_tables
#'    A list of summary tables created with `summary_table`.
#'
#' @return 
#'    Write the data stored in the summary tables in an excel workbook.        

summary_table_to_workbook <- function(workbook,
                                      summary_tables){
  
  wb <- openxlsx::loadWorkbook(workbook)
  
  for(i in seq_along(summary_tables)){
    openxlsx::writeData(wb,
                        sheet = names(summary_tables)[[i]],
                        x = summary_tables[[i]])
  }
  
  openxlsx::saveWorkbook(wb = wb,
                         file = workbook,
                         overwrite = TRUE)
  
}