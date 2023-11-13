#' @title get_datum_from_crs
#' @description attempts to parse datum and datum unit from file based on WKT
#' @param x PARAM_DESCRIPTION
#' @return list of CRS and vertical unit
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [st_crs][sf::st_crs]
#' @rdname get_datum_from_crs
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom sf st_crs

get_datum_from_crs <- function(x) {
  # sinew::moga(file.path(getwd(),"R/get_datum_from_crs.R"),overwrite = TRUE)

  ## -- Start --
  # x <- sf_cross_section_lines

  xcrs <- sf::st_crs(x)
  if (is.na(xcrs)) {
    stop("No CRS defined")
  }
  crs_df <- unlist(strsplit(xcrs$wkt,split='\n')) %>%
    as.data.frame()
  crs_datum_row <- crs_df[grepl('*DATUM*', crs_df$.),]
  crs_datum_row_value <- sub(".*DATUM", "", crs_datum_row)
  datum_value <- gsub("\\[|\\]", "", gsub(",","",gsub("\"", "", crs_datum_row_value)))    # Drop ",[]\

  datum_start_index <- grep('*DATUM*', crs_df$.)
  datum_end_index <- datum_start_index + 3
  crs_datum_rows <- crs_df[datum_start_index:datum_end_index,] %>%
    as.data.frame()
  crs_datumunit_row <- crs_datum_rows[grepl('*LENGTHUNIT*', crs_datum_rows$.),]
  if(grepl("meter|metre", crs_datumunit_row)) {
    datum_units = "meter"
  } else {
    datum_units = "foot"
  }

  return(c(datum_value,datum_units))
}
