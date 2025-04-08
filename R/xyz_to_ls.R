#' @title xyz_to_ls
#' @description helper to transform points into linestrings
#' @param dat The dataframe of points, Default: NULL
#' @param path_to_ras_dbase  The path to the folder in which you are building your catalog, is also location agnostic (disk or cloud), Default: NULL
#' @param is_quiet flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @return linestrings
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
