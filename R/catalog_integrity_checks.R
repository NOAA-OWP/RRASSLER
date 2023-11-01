#' @title catalog integrity checks
#' @description Check the status of the catalog for errors and inconsistencies
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @family post-process
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'
#'  }
#' }
#' @rdname catalog_integrity_checks
#' @import magrittr
#' @import data.table
#' @export
catalog_integrity_checks <-
  function(path_to_ras_dbase, quiet = TRUE) {
    # sinew::moga(file.path(getwd(),"R/catalog_integrity_checks.R"),overwrite = TRUE)
    # devtools::document()
    # pkgdown::build_site(new_process=TRUE)
    #
    # devtools::load_all()

    ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase, "accounting.csv", fsep = .Platform$file.sep))

    # Remerge master features
    xyz_files <-
      list.files(
        path_to_ras_dbase,
        pattern = utils::glob2rx("*ras_xyz.parquet$"),
        full.names = TRUE,
        ignore.case = TRUE,
        recursive = TRUE
      ) %>% sort()
    hull_files <-
      list.files(
        path_to_ras_dbase,
        pattern = utils::glob2rx("*hull.fgb$"),
        full.names = TRUE,
        ignore.case = TRUE,
        recursive = TRUE
      ) %>% sort()

    if (!(length(xyz_files) == length(hull_files))) {
      print_warning_block()
      print("Alert, something is off here...")
    }

    if (!(length(xyz_files) == length(ras_catalog_dbase))) {
      print_warning_block()
      print("Alert, something is off here...")
    }

    return(TRUE)
  }
