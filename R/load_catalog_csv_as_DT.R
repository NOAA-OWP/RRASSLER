#' @title load_catalog_csv_as_DT
#' @description Helper to ensure catalog edge cases and incorrect formats are correctly handled (most commonly numeric strings parsed into characters)
#' @param path_to_csv path to the model_catalog.csv file under your catalog folder
#' @param is_quiet if TRUE, function will suppress message, Default: TRUE
#' @return returns the model_catalog as a data.table object with enforced column types
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{fread}}
#' @rdname load_catalog_csv_as_DT
#' @export
#' @import magrittr
#' @import data.table

load_catalog_csv_as_DT <- function(path_to_csv, is_quiet = TRUE) {
  # sinew::moga(file.path(getwd(),"R/load_catalog_csv_as_DT.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  #
  # path_to_csv = "/home/rstudio/g/data/ras_dbase"
  # is_quiet = TRUE

  ## -- Start --
  if (!is_quiet) {
    message('Loading generated ras database')
  }
  ras_catalog_dbase <-
    data.table::fread(
      path_to_csv,
      colClasses = c(
        "nhdplus_comid" = "character",
        "model_name" = "character",
        "units" = "character",
        "crs" = "character",
        "final_name_key" = "character"
      )
    )
  return(ras_catalog_dbase)
}
