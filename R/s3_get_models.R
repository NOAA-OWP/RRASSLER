s3_get_models <- function(path_to_ras_dbase,
                          path_to_place_download,
                          huc_number = NULL,
                          projection = NULL,
                          source_code = FALSE,
                          list_only = FALSE,
                          refresh=FALSE,
                          is_quiet = FALSE,
                          is_verbose = FALSE) {
  # sinew::moga(file.path(getwd(),"R/s3_get_models"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  #
  # path_to_ras_dbase = "s3://ras-models/"
  path_to_ras_dbase
  path_to_place_download
  huc_number
  projection = NULL
  source_code = FALSE
  list_only = FALSE
  refresh=FALSE
  is_quiet = FALSE
  is_verbose = FALSE

  ## -- Start --
  fn_time_start <- Sys.time()

  ## Input validation
  if(refresh) {}

  # Cloud or disk?
  cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    cloud <- TRUE
  }

  if(is.null(c(path_to_ras_dbase,path_to_place_download,huc_number,projection,source_code))) {
    print_error_block()
    message(glue::glue("{path_to_place_download} should be empty, I'm forcing you to manually empty it"))
    return(FALSE)
  }
  if(dir.exists(path_to_place_download)) {
    if(list.files(path_to_place_download,recursive = T) > 0) {
      print_error_block()
      message(glue::glue("{path_to_place_download} should be empty, I'm forcing you to manually empty it"))
      return(FALSE)
    }
  } else {
    dir.create(path_to_place_download)
  }

  # Input parsing
  src_cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    src_cloud <- TRUE
  } else {
    path_to_ras_dbase <- gsub("/$", "", path_to_ras_dbase)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep), recursive = TRUE)
  }

  dst_cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    dst_cloud <- TRUE
  } else {
    path_to_ras_dbase <- gsub("/$", "", path_to_ras_dbase)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep), recursive = TRUE)
  }

  if(refresh) {
    refresh_master_files(path_to_ras_dbase = path_to_ras_dbase, is_verbose = is_verbose)
  }



  # Filter to list of models
  dt1[ which(dt1$A == dt_filter$A & dt1$B != dt_filter$B) ,]

  if(length(list) < 1) {

    unique_values <- cara[, unique_values := paste0(source, '_', crs)]
    valid_combos <-

    return(TRUE)
  }
  if(list_only) {
    return(TRUE)
  }

  if(src_cloud) {
    catalog <- aws.s3::s3read_using(read.csv, object = "s3://ras-models/accounting.csv")

  } else {
    print('not')
  }



  if(dst_cloud) {
    message("Why?")
  } else {
    print('not')
  }

  # Wrap up
  if (!is_quiet) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(paste("RAS Library appended in", round(runtime, digits = 3), "hours"))
  }

  return(TRUE)
}
