#' @title ingest_into_database
#' @description ingest files from a source directory on your drive into a formatted input directory
#' @param path_to_ras_dbase A path to a directory to write your RRASSLED directory to.  See Methods and Structures for more details.  is location agnostic so this can be either a local path or an s3 bucket
#' @param top_of_dir_to_scrape The top of the directory to look for models.  Will greedy search and find all models as described in Ingest logic
#' @param code_to_place_in_source a string to place into the model source column.  Useful to distinguish data authors
#' @param proj_override a string to override projection information should none be found, Default: NULL
#' @param apply_vdat_trans Should VDATUM be applied to the HEC-RAS model geometry.  See https://vdatum.noaa.gov/, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param overwrite overwrite files if we find identical models, Default: FALSE
#' @param parallel_proc Flag to determine if this should this parallel process, will check for enough free cores and boot this back if it exceeds available resources.  Will suppress all intermediate messages if active, Default: TRUE
#' @param free_treads number of threads to leave free if parallel processing, Default: 2
#' @return a RRASSLE'd catalog of models
#' @family ingest
#' @details here 'ingest' means add to our accounting system and database refers to our folder structure
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  # ras_dbase <- file.path("~/data/ras_catalog/")
#'  ras_dbase <- file.path("./inst/extdata/sample_output/ras_catalog/")
#'
#'  dir_to_scrape <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/"
#'  RRASSLER::ingest_into_database(path_to_ras_dbase = ras_dbase,top_of_dir_to_scrape = dir_to_scrape,code_to_place_in_source = "test: FEMA6",proj_override = "EPSG:2277",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE,parallel_proc = FALSE)
#'
#'  dir_to_scrape <- "./inst/extdata/sample_ras/ras2fim-sample-dataset/input_iowa/"
#'  RRASSLER::ingest_into_database(path_to_ras_dbase = ras_dbase,top_of_dir_to_scrape = dir_to_scrape,code_to_place_in_source = "test: Iowa input",proj_override = "EPSG:26915",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE,parallel_proc = FALSE)
#'
#'  dir_to_scrape <- "./inst/extdata/sample_ras/ras2fim-sample-dataset/output_iowa/"
#'  RRASSLER::ingest_into_database(path_to_ras_dbase = ras_dbase,top_of_dir_to_scrape = dir_to_scrape,code_to_place_in_source = "test: RAS2FIM V1",proj_override = "EPSG:26915",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE,parallel_proc = FALSE)
#'
#'  # Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIASUPERSECRET","AWS_SECRET_ACCESS_KEY" = "evenmoresecret","AWS_DEFAULT_REGION" = "us-also-secret")
#'  dir_to_scrape <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/"
#'  RRASSLER::ingest_into_database(path_to_ras_dbase = "s3://ras-models/",top_of_dir_to_scrape = dir_to_scrape,code_to_place_in_source = "test: FEMA6",proj_override = "EPSG:2277",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE,parallel_proc = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[parallel]{detectCores}}, \code{\link[parallel]{makeCluster}}
#'  \code{\link[doParallel]{registerDoParallel}}
#'  \code{\link[foreach]{foreach}}
#' @rdname ingest_into_database
#' @export
#' @importFrom glue glue
#' @importFrom stringr str_sub
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom foreach `%do%`
#' @importFrom foreach `%dopar%`

ingest_into_database <- function(path_to_ras_dbase,
                                 top_of_dir_to_scrape,
                                 code_to_place_in_source,
                                 proj_override = NULL,
                                 apply_vdat_trans = FALSE,
                                 is_quiet = FALSE,
                                 is_verbose = FALSE,
                                 overwrite = FALSE,
                                 parallel_proc = TRUE,
                                 free_treads = 2) {
  # sinew::moga(file.path(getwd(),"R/ingest_into_database.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  #
  # path_to_ras_dbase = "s3://ras-models/"
  # top_of_dir_to_scrape = "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/"
  # code_to_place_in_source = "test: testing"
  # proj_override = "EPSG:2277"
  # apply_vdat_trans = FALSE
  # is_quiet = FALSE
  # is_verbose = TRUE
  # overwrite = FALSE
  # parallel_proc = FALSE

  ## -- Start --
  fn_time_start <- Sys.time()
  if (!is_quiet) {
    message(glue::glue("Parsing {top_of_dir_to_scrape} to place in {path_to_ras_dbase}"))
  }

  # Global constants
  names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","initial_scrape_name","final_name_key","notes")

  # Input sanitize
  code_to_place_in_source <- as.character(code_to_place_in_source)

  # Cloud or disk?
  cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    cloud <- TRUE
  } else {
    path_to_ras_dbase <- gsub("/$", "", path_to_ras_dbase)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep), recursive = TRUE)
  }

  # Find a list of all the .prj files
  list_of_prj_files <- list.files(top_of_dir_to_scrape,pattern = glob2rx("*.prj$"),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  n_files_to_process <- length(list_of_prj_files)
  if (!is_quiet) {
    message(glue::glue("Found {n_files_to_process} potential ras files"))
  }
  if(n_files_to_process == 0) {
    print_warning_block()
    message(glue::glue("No files to ingest from {top_of_dir_to_scrape}"))
    return(TRUE)
  }

  # Is it worth/safe to parallel_proc?
  if (parallel_proc) {
    no_cores <- parallel::detectCores() - free_treads
    if (no_cores < 1) {
      print_warning_block()
      message(glue::glue("Not enough cores to make parallel processing work, running as a single thread instead"))
      parallel_proc <- FALSE
    }
  }

  if (parallel_proc) {
    # Set up par proc
    no_cores <- parallel::detectCores() - free_treads
    if (no_cores < 1) {
      no_cores = 1
    }
    doParallel::registerDoParallel(cores = no_cores)
    cl <- parallel::makeCluster(no_cores)
    if (cloud) {
      if (!is_quiet) { message("par proc to cloud") }
      # foreach::foreach(x = 1:n_files_to_process) %dopar% cloud_ingest_record(
      #   in_file = list_of_prj_files[x],
      #   ras_dbase = path_to_ras_dbase,
      #   root_bucket = path_to_root_bucket,
      #   code_to_place_in_source = code_to_place_in_source,
      #   proj_override = proj_override,
      #   apply_vdat_trans = apply_vdat_trans,
      #   is_quiet = TRUE,
      #   is_verbose = FALSE,
      #   overwrite = overwrite
      # )
      temp_dir_to_write <- tempdir()
      dir.create(file.path(temp_dir_to_write,"models","_unprocessed",fsep = .Platform$file.sep), recursive = TRUE)
      foreach::foreach(x = 1:n_files_to_process) %dopar% disk_ingest_record(
        in_file = list_of_prj_files[x],
        path_to_ras_dbase = temp_dir_to_write,
        code_to_place_in_source = code_to_place_in_source,
        proj_override = proj_override,
        apply_vdat_trans = apply_vdat_trans,
        is_quiet = TRUE,
        is_verbose = FALSE,
        overwrite = overwrite
      )
      rest_of_bucket_prefix <- stringr::str_sub(path_to_ras_dbase, nchar(path_to_root_bucket)+1, nchar(path_to_ras_dbase)-1)
      all_rrassled_files <- list.files(temp_dir_to_write, full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
      for(file_to_copy in all_rrassled_files) {
        if (!is_verbose) { message(glue::glue("Trying to move:{file_to_copy}")) }
        aws.s3::put_object(
          file = file_to_copy,
          object = glue::glue("{rest_of_bucket_prefix}{stringr::str_sub(file_to_copy, nchar(temp_dir_to_write)+1, nchar(file_to_copy))}"),
          bucket = path_to_root_bucket
        )
      }
      unlink(temp_dir_to_write,recursive = TRUE)
    } else {
      foreach::foreach(x = 1:n_files_to_process) %dopar% disk_ingest_record(
        in_file = list_of_prj_files[x],
        path_to_ras_dbase = path_to_ras_dbase,
        code_to_place_in_source = code_to_place_in_source,
        proj_override = proj_override,
        apply_vdat_trans = apply_vdat_trans,
        is_quiet = TRUE,
        is_verbose = FALSE,
        overwrite = overwrite
      )
    }
    parallel::stopCluster(cl)

  } else {
    if (cloud) {
      # for (x in 1:n_files_to_process) {
      #   cloud_ingest_record(
      #     in_file = list_of_prj_files[x],
      #     ras_dbase = path_to_ras_dbase,
      #     root_bucket = path_to_root_bucket,
      #     code_to_place_in_source = code_to_place_in_source,
      #     proj_override = proj_override,
      #     apply_vdat_trans = apply_vdat_trans,
      #     is_quiet = is_quiet,
      #     is_verbose = is_verbose,
      #     overwrite = overwrite
      #   )
      temp_dir_to_write <- tempdir()
      dir.create(file.path(temp_dir_to_write,"models","_unprocessed",fsep = .Platform$file.sep), recursive = TRUE)
      for (x in 1:n_files_to_process) {
        disk_ingest_record(
          in_file = list_of_prj_files[x],
          path_to_ras_dbase = temp_dir_to_write,
          code_to_place_in_source = code_to_place_in_source,
          proj_override = proj_override,
          apply_vdat_trans = apply_vdat_trans,
          is_quiet = is_quiet,
          is_verbose = is_verbose,
          overwrite = overwrite
        )
      }

      rest_of_bucket_prefix <- stringr::str_sub(path_to_ras_dbase, nchar(path_to_root_bucket)+1, nchar(path_to_ras_dbase)-1)
      all_rrassled_files <- list.files(file.path(temp_dir_to_write,"models",fsep = .Platform$file.sep), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
      if(!is_quiet) { message(glue::glue("Pushing {length(all_rrassled_files)} files to the cloud")) }
      for(file_to_copy in all_rrassled_files) {
        # file_to_copy <- all_rrassled_files[2]
        if (is_verbose) { message(glue::glue("Trying to copy:{file_to_copy}")) }
        aws.s3::put_object(
          file = file_to_copy,
          object = glue::glue("{rest_of_bucket_prefix}{stringr::str_sub(file_to_copy, nchar(temp_dir_to_write)+1, nchar(file_to_copy))}"),
          bucket = path_to_root_bucket
        )
      }
      unlink(temp_dir_to_write,recursive = TRUE)
    } else {
      for (x in 1:n_files_to_process) {
        if (!is_quiet) { message(glue::glue("Processing {x} of {n_files_to_process}")) }
        disk_ingest_record(
          in_file = list_of_prj_files[x],
          path_to_ras_dbase = path_to_ras_dbase,
          code_to_place_in_source = code_to_place_in_source,
          proj_override = proj_override,
          apply_vdat_trans = apply_vdat_trans,
          is_quiet = is_quiet,
          is_verbose = is_verbose,
          overwrite = overwrite
        )
      }
    }
  }

  # Wrap up
  if (!is_quiet) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(paste("RAS Library appended in", round(runtime, digits = 3), "hours"))
  }

  return(TRUE)
}
