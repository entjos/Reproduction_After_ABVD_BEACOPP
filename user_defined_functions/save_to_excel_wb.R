#' Save to Excel Workbook
#' 
#' @param x
#'    A `data.frame` that should be saved to a workbook sheet.
#'    
#' @param wb_path
#'    A path to the Excel workbook to which the `data.frame` should be 
#'    exported.
#'    
#' @param sheet_name
#'    The sheet name of the sheet into which the `data.frame` should be 
#'    exported.
#'    
#' @export save_to_excel_wb

save_to_excel_wb <- function(x,
                             wb_path,
                             sheet_name){
  
  wb <- openxlsx::loadWorkbook(wb_path)
  
  openxlsx::writeData(wb,
                      x,
                      sheet = sheet_name)
  
  openxlsx::saveWorkbook(wb,
                         wb_path,
                         overwrite = TRUE)
  
}