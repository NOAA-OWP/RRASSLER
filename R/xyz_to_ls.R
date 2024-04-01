#' @title xyz_to_ls
#' @description FUNCTION_DESCRIPTION
#' @param dat PARAM_DESCRIPTION, Default: NULL
#' @param path_to_ras_dbase PARAM_DESCRIPTION, Default: NULL
#' @param is_quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[arrow]{read_parquet}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[sf]{st_crs}}
#' @rdname xyz_to_ls
#' @export
#' @importFrom arrow read_parquet
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_set_crs st_crs
xyz_to_ls <- function(dat = NULL,
                      path_to_ras_dbase = NULL,
                      is_quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/xyz_to_ls.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()

  if(class(dat) %in% c('data.frame','data.table')) {
    xyz = dat
  } else {
    xyz = arrow::read_parquet(dat)
  }

  ls = sfheaders::sf_linestring(
    obj = xyz
    , x = "x"
    , y = "y"
    # , z = "z"
    , linestring_id = "xid"
    , keep = FALSE
  ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  return(ls)
}
