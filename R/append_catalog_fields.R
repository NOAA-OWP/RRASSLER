#' @title append_catalog_fields
#' @description adds helper fields to accounting.csv.  Request based on https://github.com/NOAA-OWP/ras2fim/issues/34
#' @param path_to_ras_dbase the path to the folder in which you are building your catalog, Default: NULL
#' @param out_name the name of the csv you want to generate, Default: NULL
#' @param overwrite flag to dictate whether or not to overwrite the out_name, should it exist. set to TRUE to delete and (re)generate, FALSE to safely exit, Default: FALSE
#' @param verbose flag to determine whether print statements are suppressed, TRUE to show messages and FALSE to surpress them, Default: TRUE
#' @return a new csv with helper columns
#' @family post-process
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  RRASSLER::append_catalog_fields(path_to_ras_dbase = "G:/data/ras_catalog",out_name = "OWP_ras_model_catalog.csv",overwrite = FALSE,verbose = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[sf]{s2}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
#'  \code{\link[data.table]{fwrite}}
#' @rdname append_catalog_fields
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom sf sf_use_s2 st_transform st_read st_crs
#' @importFrom data.table fwrite
append_catalog_fields <- function(path_to_ras_dbase = NULL,
                                  out_name = NULL,
                                  overwrite = FALSE,
                                  is_verbose = TRUE) {
  # sinew::moga(file.path(getwd(),"R/append_catalog_fields.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()

  # path_to_ras_dbase="/home/rstudio/g/data/ras_dbase"
  # overwrite=TRUE
  # quiet=FALSE

  ## -- Start --
  if (verbose) {
    message(glue::glue("Appending catalog at {path_to_ras_dbase} into {out_name}"))
  }

  if (file.exists(file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep))) {
    if (!overwrite) {
      print_warning_block()
      message("Alert: file already exists and overwrite is set to FALSE")
      return(FALSE)
    }
    unlink(file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep))
  }
  ras_catalog_dbase = load_catalog_csv_as_DT(
    file.path(path_to_ras_dbase, "accounting.csv", fsep = .Platform$file.sep),
    quiet = !verbose
  )

  # Date as YYYYMMDD
  ras_catalog_dbase <- ras_catalog_dbase %>%
    dplyr::mutate(date = as.Date(as.POSIXct(last_modified, origin = "1970-01-01"), "%Y%m%d") %>% format("%Y%m%d"))

  # HUC as pythonic list
  sf::sf_use_s2(FALSE)
  ras_catalog_dbase[, hucs := character()]
  list_vec <- c()
  template_hucs <-
    sf::st_transform(sf::st_read(
      file.path(path_to_ras_dbase, "HUC8.fgb", fsep = .Platform$file.sep),
      quiet = !verbose
    ),
    sf::st_crs("EPSG:4326"))
  for (row in 1:nrow(ras_catalog_dbase)) {
    if (verbose) {
      print(glue::glue("Processing row:{row} of {nrow(ras_catalog_dbase)}"))
    }
    if (is.na(ras_catalog_dbase[row, final_name_key]) ||
        !file.exists(
          file.path(
            path_to_ras_dbase,
            "models",
            ras_catalog_dbase[row, final_name_key],
            "hull.fgb",
            fsep = .Platform$file.sep
          )
        )) {
      if (verbose) {
        print_warning_block()
        message("No HUC found")
      }
      ras_catalog_dbase[row, hucs := noquote(paste0("{}"))]
    } else {
      footprint <-
        sf::st_read(
          file.path(
            path_to_ras_dbase,
            "models",
            ras_catalog_dbase[row, final_name_key],
            "hull.fgb",
            fsep = .Platform$file.sep
          ),
          quiet = TRUE
        )
      val <- template_hucs[footprint, ]$huc8
      ras_catalog_dbase[row, hucs := noquote(paste0("{", paste(noquote(val), collapse = ";"), "}"))]
    }
  }
  sf::sf_use_s2(TRUE)

  # Status field
  ras_catalog_dbase <- ras_catalog_dbase[, status := "ready"]
  ras_catalog_dbase <-
    ras_catalog_dbase[ras_catalog_dbase$nhdplus_comid == 1, status := "no_crosswalk"]
  ras_catalog_dbase <-
    ras_catalog_dbase[ras_catalog_dbase$nhdplus_comid == 2, status := "reingest"]

  # Change catalog
  # updated
  data.table::fwrite(
    ras_catalog_dbase,
    file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep),
    row.names = FALSE,
    append = FALSE
  )

  return(TRUE)
}
