#' @title dedupe
#' @description de duplicates RAS models from the catalog
#' @param path_to_ras_dbase the path to the folder in which you are building your catalog, Default: NULL
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return a list of files with otherwise identical geometries
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dedupe
#' @export
#' @import data.table
#' @import magrittr

dedupe <- function(path_to_ras_dbase=NULL,quiet=FALSE) {

  # sinew::moga(file.path(getwd(),"R/dedupe.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()

  ## -- Start --
  if(!quiet) { print("In the todo pile") }

  return(TRUE)
}
