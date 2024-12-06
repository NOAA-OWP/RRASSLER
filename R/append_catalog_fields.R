#' @title append_catalog_fields
#' @description adds helper fields to accounting.csv.
#' @param path_to_ras_dbase the path to the folder in which you are building your catalog, Default: NULL
#' @param out_name the name of the csv you want to generate, Default: NULL
#' @param overwrite flag to dictate whether or not to overwrite the out_name, should it exist. set to TRUE to delete and (re)generate, FALSE to safely exit, Default: FALSE
#' @param is_verbose flag to determine whether print statements are suppressed, TRUE to show messages and FALSE to surpress them, Default: TRUE
#' @param HUC8_override a path to the spatial key if you need to run this over a temp dir for eg ras2fim, Default: NULL
#' @return a new csv with helper columns
#' @family post-process
#' @details TRUE
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE
#'  RRASSLER::append_catalog_fields(path_to_ras_dbase = "G:/data/ras_catalog",out_name = "OWP_ras_model_catalog.csv",overwrite = FALSE,is_verbose = TRUE)
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
                                  is_verbose = TRUE,
                                  HUC8_override = NULL) {
  # sinew::moga(file.path(getwd(),"R/append_catalog_fields.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()

  # path_to_ras_dbase = "~/data/ras_catalog/"
  # path_to_ras_dbase = "s3://ras-models/"
  # out_name = "OWP_ras_models_catalog.csv"
  # overwrite = FALSE
  # is_verbose = TRUE

  ## -- Start --
  # make sure out_name is valid
  fn_time_start <- Sys.time()
  if(out_name == "accounting.csv") {
    print_error_block()
    message("Can not use accounting.csv")
    return(FALSE)
  }

  # Cloud or disk?
  cloud <- FALSE
  process_dir <- path_to_ras_dbase
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    cloud <- TRUE
  }

  if(cloud) {
    # Does accounting exist?
    rest_of_bucket_prefix <- stringr::str_sub(path_to_ras_dbase, nchar(path_to_root_bucket)+1, nchar(path_to_ras_dbase)-1)
    process_dir <- tempdir()
    # unlink(root_temp,recursive = TRUE)

    # What is in the database at this very moment?
    if(is_verbose) { message(glue::glue("Gathering bucket contents")) }
    df_bucket_data <- aws.s3::get_bucket(bucket = path_to_root_bucket, prefix = "accounting.csv",max = Inf)
    if(length(df_bucket_data) == 0) {
      print_error_block()
      message("Not a RRASSLE'd archive")
      return(FALSE)
    } else {
      list_bucket_data_dt <- data.table::as.data.table(df_bucket_data)
      aws.s3::save_object(
        object = list_bucket_data_dt[1][[1]],
        bucket = path_to_root_bucket,
        file = file.path(process_dir,list_bucket_data_dt[1][[1]],fsep = .Platform$file.sep)
      )
    }
  }

  # We have what we need?
  if(!file.exists(file.path(process_dir,"accounting.csv",fsep = .Platform$file.sep))) {
    print_error_block()
    message("Not a RRASSLE'd archive")
    return(FALSE)
  }
  # TODO: Could remove dependency, could use NHDPlusTools, I'm trying to be nice to servers?
  template_hucs_path <- file.path(process_dir,"HUC8.fgb",fsep = .Platform$file.sep)
  if(!file.exists(template_hucs_path)) {
    if(!file.exists(HUC8_override)) {
      print_error_block()
      message("Missing spatial key")
      return(FALSE)
    }
    template_hucs_path <- HUC8_override
  }
  template_hucs <-
    sf::st_transform(
      sf::st_read(template_hucs_path,quiet = !is_verbose),
      sf::st_crs("EPSG:6349"))

  # Are we overwriting something we shouldn't?
  if(cloud) {
    df_bucket_data <- aws.s3::get_bucket(bucket = path_to_root_bucket, prefix = out_name,max = Inf)
    if(length(df_bucket_data) == 1) {
      if(!overwrite) {
        print_warning_block()
        message("Alert: file already exists and overwrite is set to FALSE")
        return(FALSE)
      }
      list_bucket_data <- c()
      for(i in 1:length(df_bucket_data)) {
        list_bucket_data <- c(
          list_bucket_data,
          df_bucket_data[[i]]$Key)
      }
      list_bucket_data_dt <- data.table::as.data.table(list_bucket_data)
      file_to_remove <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c(out_name),]$list_bucket_data
      aws.s3::delete_object(
        object = file_to_remove,
        bucket = path_to_root_bucket
      )
    }
  } else {
    if (file.exists(file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep))) {
      if(!overwrite) {
        print_warning_block()
        message("Alert: file already exists and overwrite is set to FALSE")
        return(FALSE)
      }
      unlink(file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep))
    }
  }

  # Finally start
  if (is_verbose) { message(glue::glue("Appending catalog at {path_to_ras_dbase} into {out_name}")) }

  ras_catalog_dbase = load_catalog_csv_as_DT(
    file.path(process_dir, "accounting.csv", fsep = .Platform$file.sep),
    is_quiet = !is_verbose
  )

  # Date as YYYYMMDD
  ras_catalog_dbase <- ras_catalog_dbase %>%
    dplyr::mutate(date = as.Date(as.POSIXct(last_modified, origin = "1970-01-01"), "%Y%m%d") %>% format("%Y%m%d"))

  # HUC as pythonic list
  # TODO: Make cloud native, use something like 'https://{ras-models}.s3.{us-east-2}.amazonaws.com/{rest_of_bucket_prefix}/{ras_catalog_dbase[row, final_name_key]}/RRASSLER_hull.fgb'
  sf::sf_use_s2(FALSE)
  ras_catalog_dbase <- ras_catalog_dbase[, hucs := character()]
  list_vec <- c()

  for (row in 1:nrow(ras_catalog_dbase)) {
    if (is_verbose) { message(glue::glue("Processing row:{row} of {nrow(ras_catalog_dbase)}")) }
    if (is.na(ras_catalog_dbase[row, final_name_key]) ||
        !file.exists(file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row, final_name_key],"RRASSLER_hull.fgb",fsep = .Platform$file.sep))
        ) {
      if (is_verbose) {
        print_warning_block()
        message("No HUC found")
      }
      ras_catalog_dbase[row, hucs := noquote(paste0("{}"))]
    } else {
      footprint <- sf::st_read(file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row, final_name_key],"RRASSLER_hull.fgb",fsep = .Platform$file.sep),quiet = TRUE)
      val <- template_hucs[footprint, ]$huc8
      ras_catalog_dbase[row, hucs := noquote(paste0("{", paste(noquote(val), collapse = ";"), "}"))]
    }
  }
  sf::sf_use_s2(TRUE)

  # Short source field
  # https://github.com/NOAA-OWP/RRASSLER/issues/7
  ras_catalog_dbase <- ras_catalog_dbase[, source_code := "ras"]
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$source %like% c('FEMA Region 6'), source_code := "ble"]
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$source %like% c('IFC'), source_code := "ifc"]
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$source %like% c('RFC'), source_code := "rfc"]

  # Status field
  ras_catalog_dbase <- ras_catalog_dbase[, status := character()]
  list_of_valid_names <- unique(ras_catalog_dbase$initial_scrape_name)
  ## Most recent model is ready
  for(same_model_index in 1:length(list_of_valid_names)) {
    same_model <- list_of_valid_names[same_model_index]
    dbase_rows <- ras_catalog_dbase[ras_catalog_dbase$initial_scrape_name == same_model, ]
    if(nrow(dbase_rows) > 1) {
      # For now, just take the last one...
      ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$initial_scrape_name == same_model, status := "superseded"]
      ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$final_name_key == dbase_rows[nrow(dbase_rows),final_name_key], status := "ready"]
    } else {
      ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$initial_scrape_name == same_model, status := "ready"]
    }
  }
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$nhdplus_comid == 1, status := "no_crosswalk"]
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$nhdplus_comid == 2, status := "reingest"]
  ras_catalog_dbase <- ras_catalog_dbase[ras_catalog_dbase$nhdplus_comid == "", status := "investigate"]

  # push
  if(cloud) {
    temp_file <- tempfile(fileext = ".csv")
    data.table::fwrite(ras_catalog_dbase, temp_file, row.names = FALSE)
    aws.s3::put_object(
      file = temp_file,
      object = out_name,
      bucket = path_to_root_bucket
    )
  } else {
    data.table::fwrite(
      ras_catalog_dbase,
      file.path(path_to_ras_dbase, out_name, fsep = .Platform$file.sep),
      row.names = FALSE,
      append = FALSE
    )
  }

  if(is_verbose) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(glue::glue("Wall time: {round(runtime, digits = 3)} hours"))
  }

  return(TRUE)
}


