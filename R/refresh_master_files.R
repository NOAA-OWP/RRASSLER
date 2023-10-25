#' @title refresh_master_files
#' @description remerge individual files into spatial model key
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param point_merge Dev flag to shave point processing, Default: TRUE
#' @param verbose flag to determine whether print statements are shown - TRUE to show messages - FALSE to skip non-critical ones, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  RRASSLER::refresh_master_files(path_to_ras_dbase = "G:/data/ras_catalog",verbose=TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{st_write}}
#'  \code{\link[arrow]{read_parquet}}, \code{\link[arrow]{write_parquet}}
#'  \code{\link[data.table]{as.data.table}}, \code{\link[data.table]{rbindlist}}
#'  \code{\link[sfheaders]{sf_linestring}}
#' @rdname refresh_master_files
#' @export
#' @importFrom utils glob2rx
#' @importFrom glue glue
#' @importFrom sf st_read st_set_crs st_crs st_write
#' @importFrom arrow read_parquet write_parquet
#' @importFrom data.table as.data.table rbindlist
#' @importFrom sfheaders sf_linestring
refresh_master_files <- function(path_to_ras_dbase,
                                 point_merge = TRUE,
                                 verbose = TRUE) {

  # sinew::moga(file.path(getwd(),"R/refresh_master_files.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # refresh_master_files(path_to_ras_dbase = "/home/rstudio/g/data/ras_dbase",quiet=FALSE)

  ## -- Start --
  fn_time_start <- Sys.time()

  if(!file.exists(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))) {
    print_error_block()
    message("Not a RRASSLE'd archive")
    return(FALSE)
  }
  if(verbose) { message("(re)merging database outputs") }

  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))

  # Remerge master features
  xyz_files <- list.files(path_to_ras_dbase, pattern = utils::glob2rx("*ras_xyz.parquet$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()
  hull_files <- list.files(path_to_ras_dbase, pattern = utils::glob2rx("*hull.fgb$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()

  if(!(length(xyz_files)==length(hull_files))) {
    print_error_block()
    message("Alert, something is off here...")
    stop()
  }

  point_concat <- c()
  xs_concat <- c()
  hull_concat <- c()
  master_id <- 0
  rows_in_table <- length(xyz_files)
  for(index in 1:rows_in_table) {
    if(verbose) { message(glue::glue("Processing {index} of {rows_in_table}")) }
    # index = 1

    final_folder_name <- basename(dirname(hull_files[index]))
    row <- ras_catalog_dbase[ras_catalog_dbase$final_name_key==final_folder_name,]
    hull <- sf::st_read(hull_files[index],quiet = TRUE)

    hull$start_master_id <- master_id + 1

    point_data <- arrow::read_parquet(xyz_files[index],as_data_frame = TRUE) %>%
      data.table::as.data.table()
    point_data[, master_id := xid + master_id]
    point_concat <- data.table::rbindlist(list(point_concat, point_data))
    master_id <- max(point_data[,master_id])

    hull$Name <- row$model_name
    hull$crs <- row$crs
    hull$units <- row$units
    hull$path <- row$final_name_key
    hull$source <- row$source
    hull$end_master_id <- master_id
    hull_concat <- rbind(hull_concat,hull)
  }

  if(verbose) { message(glue::glue("Making cross sections")) }
  xs_lines <- sfheaders::sf_linestring(
    obj = point_concat,
    x = "x",
    y = "y",
    # z = "z",
    linestring_id = "master_id",
    keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  if(verbose) { message(glue::glue("Writing files")) }
  unlink(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
  unlink(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  unlink(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))

  arrow::write_parquet(point_concat,file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
  sf::st_write(xs_lines,file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  sf::st_write(hull_concat,file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))

  if(verbose) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(glue::glue("(re)-Merged {nrow(hull_concat)} models with {nrow(xs_lines)} cross sections and {nrow(point_concat)} points"))
    message(glue::glue("Wall time: {round(runtime, digits = 3)} hours"))
  }

  return(TRUE)
}
