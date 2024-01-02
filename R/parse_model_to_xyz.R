#' @title parse_model_to_xyz
#' @description parse a model into an xyz dataframe from basename, considers both g## and g##.hdf files
#' @param geom_path path to the base model
#' @param units units found in the project, "English Units" or "SI Units"
#' @param proj_string PARAM_DESCRIPTION
#' @param in_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param out_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  # g_path <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/12090301/12090301_models/Model/Alum Creek-Colorado River/ALUM 006/ALUM 006.g01"
#'  g_path <- fs::path_package("extdata/shapes.fgb", package = "mypkg")
#'  pts <- parse_model_to_xyz(geom_path = g_path,units = "English Units",proj_string = "EPSG:2277",quiet = FALSE)
#'
#'  g_path <- "./inst/extdata/sample_ras/ras2fim-sample-dataset/input_iowa/10170204000897/Hydraulic_Models/Simulations/10170204000897.g01"
#'  pts <- parse_model_to_xyz(geom_path = g_path,units = "SI Units",proj_string = "EPSG:26915",quiet = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[unglue]{unglue}}
#' @rdname parse_model_to_xyz
#' @export
#' @importFrom stringr str_sub
#' @importFrom unglue unglue_vec

parse_model_to_xyz <- function(geom_path,
                               units,
                               proj_string,
                               in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                               out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                               vdat_trans = FALSE,
                               quiet = FALSE,
                               is_verbose = TRUE) {

  # sinew::moga(file.path(getwd(),"R/parse_model_to_xyz.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  #
  # geom_path = g_path
  # units = "SI Units"
  # proj_string = "EPSG:26915"
  # quiet = FALSE
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # vdat_trans = FALSE

  ## -- Start --
  current_g_value <- stringr::str_sub(geom_path,-3,-1)

  # Attempt to parse the g file
  g_pts <- list()
  g_pts[[1]] <- data.frame()
  g_pts = process_ras_g_to_xyz(
    geom_path = geom_path,
    units = units,
    proj_string = proj_string,
    in_epoch_override = as.integer(as.POSIXct(file.info(geom_path)$mtime)),
    vdat = vdat_trans,
    quiet = !is_verbose)

  # Was it successful?
  if( nrow(g_pts[[1]]) > 0) {
    if(vdat_trans) {
      g_ptserr <- unglue::unglue_vec(g_pts[[2]], "{}:{}:{x}") %>% as.numeric()
    } else {
      g_ptserr <- unglue::unglue_vec(g_pts[[2]],"{}:{x}") %>% as.numeric()
    }
    if(is.na(g_ptserr)) { g_ptserr = -99999 } # A very large number, I did something wrong here
  }

  # Attempt to parse the ghdf file, if it exists
  ghdf_pts <- list()
  ghdf_pts[[1]] <- data.frame()
  if(file.exists(paste0(geom_path,".hdf"))) {
    ghdf_pts = process_ras_hdf_to_xyz(
      geom_path = paste0(geom_path,".hdf"),
      units = units,
      proj_string = proj_string,
      in_epoch_override = as.integer(as.POSIXct(file.info(geom_path)$mtime)),
      vdat = vdat_trans,
      quiet = !is_verbose)

    # Was it successful?
    if(nrow(ghdf_pts[[1]]) > 0) {
      if(vdat_trans) {
        ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]], "{}:{}:{x}") %>% as.numeric()
      } else {
        ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]],"{}:{x}") %>% as.numeric()
      }
      if(is.na(ghdf_ptserr)) { ghdf_ptserr = -1000000 } # A very large number, I did something wrong here
    }
  }

  # What was successful?
  cond4 = tryCatch({
    isTRUE(nrow(g_pts[[1]]) > 0)
    },
    error = function(e) {FALSE})
  cond5 = tryCatch({
    isTRUE(nrow(ghdf_pts[[1]]) > 0)
    },
    error = function(e) {FALSE})

  # If we could parse both, which was a better extraction?
  if(cond4 & cond5) {

    if(!quiet) {
      message("Extraction status:")
      message(glue::glue("g file:{nrow(g_pts[[1]])} rows with {g_ptserr} meters of error"))
      message(glue::glue("hdf file:{nrow(ghdf_pts[[1]])} rows with {ghdf_ptserr} meters of error"))
    }

    if(abs(ghdf_ptserr) > abs(g_ptserr)) {
      extrated_pts <- g_pts
      extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")
    } else {
      extrated_pts <- ghdf_pts
      extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")
    }

    if(!quiet) { message(glue::glue("Extraction status:{extrated_pts[[2]]}")) }

  } else if(cond4) {
    # We could only parse a g file
    extrated_pts <- g_pts
    extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")

    if(!quiet) {
      message(glue::glue("g file:{nrow(g_pts[[1]])} rows with {g_ptserr} meters of error"))
    }

  } else if(cond5) {
    # We could only parse a ghdf file
    extrated_pts <- ghdf_pts
    extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")

    if(!quiet) {
      message(glue::glue("hdf file:{nrow(ghdf_pts[[1]])} rows with {ghdf_ptserr} meters of error"))
    }

  } else if(!cond4 & !cond5) {
    # Nothing was parsed
    if(!quiet) {
      print_warning_block()
      message("nothing was able to be appropriately parsed")
    }
    extrated_pts <- FALSE
  }

  return(extrated_pts)
}

