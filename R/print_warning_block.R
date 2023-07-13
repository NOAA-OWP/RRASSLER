#' @title print_warning_block
#' @description A warning block helper
#' @return print output
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  RRASSLER::print_warning_block()
#'  }
#' }
#' @rdname print_warning_block
#' @export

print_warning_block <- function() {

  # sinew::moga(file.path(getwd(),"R/print_warning_block.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()

  ## -- Start --
  print(cat("NULL_WARNING -- ¯\\_(o_0)_/¯  -- WARNING_"))
  return(TRUE)
}
