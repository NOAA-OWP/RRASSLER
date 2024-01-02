#' @title ingest_FEMA6_BLE
#' @description helper to ingest FEMA region 6 BLE data
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param HUCID string to huc8
#' @param proj_override a CRS string to apply should a projection not be found, Default: NULL
#' @param apply_vdat_trans a flag to dictate whether or not to apply a vdatum transformation, TRUE to apply, FALSE to skip, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: TRUE
#' @param overwrite overwrite files if we find identical models, Default: FALSE
#' @param parallel_proc Flag to determine if this should this parallel process, will check for enough free cores and boot this back if it exceeds available resources.  Will suppress all intermediate messages if active, Default: TRUE
#' @param free_treads number of threads to leave free if parallel processing, Default: 2
#' @param clean number of threads to leave free if parallel processing, Default: 2
#' @param opt_local_path PARAM_DESCRIPTION, Default: NULL
#' @return a RRASSLE'd catalog of models or added desired HUC8 models
#' @family ingest
#' @details As one of the best centralized and accessible databases, the FEMA region 6 BLE data are perfect candidates to RRASSL.  This provides a wrapper around BLE scrapers and the ingest_into_database function.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::ingest_FEMA6_BLE(path_to_ras_dbase = "G:/data/ras_catalog/","12090301",proj_override = "EPSG:2277",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = FALSE,overwrite = FALSE,parallel_proc = TRUE,free_treads = 2)
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[utils]{unzip}}
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
#' @rdname ingest_FEMA6_BLE
#' @export
#' @importFrom stringr str_sub
#' @importFrom glue glue
#' @importFrom utils unzip
#' @importFrom sf st_read st_crs

ingest_FEMA6_BLE <- function(path_to_ras_dbase,
                             HUCID,
                             proj_override = proj_override,
                             apply_vdat_trans = FALSE,
                             is_quiet = FALSE,
                             is_verbose = TRUE,
                             overwrite = FALSE,
                             parallel_proc = TRUE,
                             free_treads = 2,
                             clean = FALSE,
                             opt_local_path = NULL) {
  # sinew::moga(file.path(getwd(),"R/ingest_FEMA6_BLE.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  #
  # path_to_ras_dbase = "G:/data/ras_catalog/"
  # path_to_ras_dbase <- "s3://ras-models/"

  ## -- Start --
  fn_time_start <- Sys.time()

  # Cloud or disk?
  cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    if (is.null(opt_local_path)) {
      print_warning_block()
      top_of_dir <- tempdir()
      clean <- TRUE
      if(is_verbose) { message(glue::glue("non-local execution and no temp dir selected, making temp at:{top_of_dir}")) }
    } else {
      path_to_ras_dbase = opt_local_path
    }
  } else {
    path_to_ras_dbase <- gsub("/$", "", path_to_ras_dbase)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep),recursive = TRUE)
  }
  if(!is_quiet) { message(glue::glue("RRASSL-ing FEMA Region 6 data")) }

  scrape_ble_lib(database_path = path_to_ras_dbase,HUCID = HUCID,is_quiet = is_quiet,overwrite = overwrite,files = "m")
  util_unzip(
    file.path(path_to_ras_dbase,"_temp","BLE",HUCID,glue::glue("{HUCID}_models.zip"),fsep = .Platform$file.sep),
    is_quiet = is_quiet
  )

  if (is.null(proj_override)) {
    if(!is_quiet) { message(glue::glue("Looking for a projection")) }
    scrape_ble_lib(database_path = path_to_ras_dbase,HUCID,is_quiet = is_quiet,overwrite = overwrite,files = "s")
    zip_file <- file.path(path_to_ras_dbase,"_temp","BLE",HUCID,glue::glue("{HUCID}_SpatialData.zip"),fsep = .Platform$file.sep)
    utils::unzip(zip_file,exdir = file.path(dirname(zip_file),gsub('.{4}$', '', basename(zip_file)),fsep = .Platform$file.sep))
    path_to_gdb <- file.path(path_to_ras_dbase,"_temp","BLE",HUCID,glue::glue("{HUCID}_SpatialData"),"Spatial Files",glue::glue("{HUCID}_SpatialData.gdb"),fsep = .Platform$file.sep)
    fc <- sf::st_read(path_to_gdb,layer="BLE_DEP01PCT")
    if (!is.null(sf::st_crs(fc))) {
      proj_override <- sf::st_crs(fc)
    } else {
      return(FALSE)
    }
  }

  ingest_into_database(
    path_to_ras_dbase = path_to_ras_dbase,
    top_of_dir_to_scrape = file.path(path_to_ras_dbase,"_temp","BLE",HUCID,glue::glue("{HUCID}_models"),fsep = .Platform$file.sep),
    code_to_place_in_source = glue::glue("FEMA Region 6:{HUCID}"),
    proj_override = proj_override,
    apply_vdat_trans = apply_vdat_trans,
    is_quiet = is_quiet,
    is_verbose = is_verbose,
    overwrite = overwrite,
    parallel_proc = parallel_proc,
    free_treads = free_treads
  )
  return(TRUE)
}
