#' @title ingest_into_database
#' @description ingest files from a source directory on your drive into a formatted input directory
#' @param path_to_ras_dbase A path to a directory to write your RRASSLED directory to.  See Methods and Structures for more details.  is location agnostic so this can be either a local path or an s3 bucket
#' @param top_of_dir_to_scrape The top of the directory to look for models.  Will greedy search and find all models as described in Ingest logic
#' @param code_to_place_in_source a string to place into the model source column.  Useful to distinguish data authors
#' @param proj_override a string to override projection information should none be found, Default: NULL
#' @param apply_vdat_trans Should VDATUM be applied to the HEC-RAS model geometry.  See https://vdatum.noaa.gov/, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param quick_check on initial ingest, if the model name is found we assume the models are the same without fully spatializing them which saves processing time, Default: FALSE
#' @param quick_hull a flag to dictate whether the end points of a models cross sections are used or if the entire point database is fed to the hull creation, Default: FALSE
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
#'  RRASSLER::ingest_into_database(path_to_ras_dbase = "G:/data/ras_catalog3/",
#'top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Willbarger Creek-Colorado River/WILLBARGER 0516/",
#'code_to_place_in_source = "test",
#'proj_override = "EPSG:2277",
#'apply_vdat_trans = FALSE,
#'is_quiet = FALSE,
#'is_verbose = TRUE,
#'quick_check = FALSE,
#'quick_hull = FALSE,
#'overwrite = FALSE,
#'parallel_proc = FALSE)
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
                                 quick_check = FALSE,
                                 quick_hull = FALSE,
                                 overwrite = FALSE,
                                 parallel_proc = TRUE,
                                 free_treads = 2) {
  # sinew::moga(file.path(getwd(),"R/ingest_into_database.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase
  # top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Willbarger Creek-Colorado River"
  # code_to_place_in_source = "test"
  # proj_override = "EPSG:2277"
  # apply_vdat_trans = FALSE
  # is_quiet = FALSE
  # is_verbose = FALSE
  # quick_check = FALSE
  # quick_hull = FALSE
  # overwrite = FALSE
  # parallel_proc = TRUE
  # free_treads = 2

  # path_to_ras_dbase = "G:/data/ras_catalog3"
  # top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Willbarger Creek-Colorado River/WILLBARGER 0516/"
  # code_to_place_in_source = "test"
  # proj_override = NULL
  # apply_vdat_trans = FALSE
  # is_quiet = FALSE
  # is_verbose = TRUE
  # quick_check = FALSE
  # quick_hull = FALSE
  # overwrite = FALSE
  # parallel_proc = FALSE

  # gmailr::gm_auth_configure(path = "C:/Users/jimma/Desktop/client_secret_765662520275-iduoi88oke14pqst3ebukn5rb2qf0895.apps.googleusercontent.com.json")

  ## -- Start --
  fn_time_start <- Sys.time()
  if (!is_quiet) {
    message(glue::glue(
      "Parsing {top_of_dir_to_scrape} to place in {path_to_ras_dbase}"
    ))
  }

  # Global constants
  names <-
    c(
      "nhdplus_comid",
      "model_name",
      "g_file",
      "last_modified",
      "source",
      "units",
      "crs",
      "initial_scrape_name",
      "final_name_key",
      "notes"
    )

  # Input sanitize
  code_to_place_in_source <- as.character(code_to_place_in_source)

  # Cloud or disk?
  cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <-
      ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <-
      paste0("s3://", ls_srt_folders_file[2], "/")
    if (!is_quiet) {
      message(
        glue::glue(
          "Parsing {top_of_dir_to_scrape} to place in bucket {path_to_root_bucket}"
        )
      )
    }
    cloud <- TRUE
  } else {
    path_to_ras_dbase <-
      substr(path_to_ras_dbase, 1, nchar(path_to_ras_dbase) - 1)
    dir.create(
      file.path(
        path_to_ras_dbase,
        "models",
        "_unprocessed",
        fsep = .Platform$file.sep
      ),
      recursive = TRUE
    )
  }

  # Find a list of all the .prj files
  list_of_prj_files <-
    list.files(
      top_of_dir_to_scrape,
      pattern = glob2rx("*.prj$"),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  n_files_to_process <- length(list_of_prj_files)
  if (!is_quiet) {
    message(glue::glue("Found {n_files_to_process} potential ras files"))
  }

  # Is it worth/safe to parallel_proc?
  if (parallel_proc) {
    no_cores <- parallel::detectCores() - free_treads
    if (no_cores < 1) {
      if (!is_quiet) {
        print_warning_block()
        message(
          glue::glue(
            "Not enough cores to make parallel processing work, running as a single thread instead"
          )
        )
      }
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
      foreach::foreach(x = 1:n_files_to_process) %dopar% cloud_ingest_record(
        in_file = list_of_prj_files[x],
        ras_dbase = path_to_ras_dbase,
        root_bucket = path_to_root_bucket,
        code_to_place_in_source = code_to_place_in_source,
        proj_override = proj_override,
        apply_vdat_trans = apply_vdat_trans,
        is_quiet = TRUE,
        is_verbose = FALSE,
        quick_check = quick_check,
        quick_hull = quick_hull,
        overwrite = overwrite
      )
    } else {
      foreach::foreach(x = 1:n_files_to_process) %dopar% disk_ingest_record(
        in_file = list_of_prj_files[x],
        path_to_ras_dbase = path_to_ras_dbase,
        code_to_place_in_source = code_to_place_in_source,
        proj_override = proj_override,
        apply_vdat_trans = apply_vdat_trans,
        is_quiet = TRUE,
        is_verbose = FALSE,
        quick_check = quick_check,
        quick_hull = quick_hull,
        overwrite = overwrite
      )
    }
    parallel::stopCluster(cl)
  } else {
    if (cloud) {
      for (x in 1:n_files_to_process) {
        cloud_ingest_record(
          in_file = list_of_prj_files[x],
          ras_dbase = path_to_ras_dbase,
          root_bucket = path_to_root_bucket,
          code_to_place_in_source = code_to_place_in_source,
          proj_override = proj_override,
          apply_vdat_trans = apply_vdat_trans,
          is_quiet = is_quiet,
          is_verbose = is_verbose,
          quick_check = quick_check,
          quick_hull = quick_hull,
          overwrite = overwrite
        )
      }
    } else {
      for (x in 1:n_files_to_process) {
        # x = 1
        disk_ingest_record(
          in_file = list_of_prj_files[x],
          path_to_ras_dbase = path_to_ras_dbase,
          code_to_place_in_source = code_to_place_in_source,
          proj_override = proj_override,
          apply_vdat_trans = apply_vdat_trans,
          is_quiet = is_quiet,
          is_verbose = is_verbose,
          quick_check = quick_check,
          quick_hull = quick_hull,
          overwrite = overwrite
        )
      }
    }
  }

  #

  # Wrap up
  if (is_quiet) {
    runtime <- Sys.time() - fn_time_start
    units(runtime) <- "hours"
    message(paste("RAS Library appended in", round(runtime, digits = 3), "hours"))
  }

  return(TRUE)
}
