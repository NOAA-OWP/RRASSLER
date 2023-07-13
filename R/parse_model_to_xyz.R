#' @title parse_model_to_xyz
#' @description FUNCTION_DESCRIPTION
#' @param geom_path PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION
#' @param proj_string PARAM_DESCRIPTION
#' @param in_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param out_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @param default_g PARAM_DESCRIPTION, Default: FALSE
#' @param try_both PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
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
                               vdat_trans=FALSE,
                               quiet=FALSE,
                               default_g=FALSE,
                               try_both=TRUE) {

  # sinew::moga(file.path(getwd(),"R/parse_model_to_xyz.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()

  # geom_path="G:/data/ras_catalog/_temp/BLE/12010005/12010005_models/EngineeringModels/Hydraulic Models/AdamsBayou_SabineRiver/ADAMS BAYOU LATERAL 18/ADAMS BAYOU LATERAL 18.g01"
  # geom_path="G:/data/ras_catalog/_temp/BLE/12010005/12010005_models/EngineeringModels/Hydraulic Models/BigCowCreek/Big Cow Creek Tributary 1/Big Cow Creek Tributary 1.g01"
  # units = "Foot"
  # proj_string="ESRI:102739"
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # vdat_trans=FALSE
  # quiet=FALSE
  # default_g=FALSE
  # try_both=TRUE

  ## -- Start --
  current_g_value <- stringr::str_sub(geom_path,-3,-1)

  cond3 = file.exists(paste0(geom_path,".hdf"))

  # Attempt to parse the g file
  g_pts <- list()
  g_pts[[1]] <- data.frame()
  g_pts = process_ras_g_to_xyz(
    geom_path=geom_path,
    units=units,
    proj_string=proj_string,
    in_epoch_override=as.integer(as.POSIXct(file.info(geom_path)$mtime)),
    vdat = vdat_trans,
    quiet=quiet)

  if(nrow(g_pts[[1]]) > 0) {
    if(vdat_trans) {
      g_ptserr <- unglue::unglue_vec(g_pts[[2]], "{}:{}:{x}") %>% as.numeric()
    } else {
      g_ptserr <- unglue::unglue_vec(g_pts[[2]],"{}:{x}") %>% as.numeric()
    }
    if(is.na(g_ptserr)) { g_ptserr = -1000000 } # A very large number, I did something wrong here
  }

  if(nrow(g_pts[[1]]) == 0 & !try_both) {
    return(FALSE)
  }

  # Attempt to parse the ghdf file
  ghdf_pts <- list()
  ghdf_pts[[1]] <- data.frame()
  if(cond3) {
    ghdf_pts = process_ras_hdf_to_xyz(
      geom_path=paste0(geom_path,".hdf"),
      units=units,
      proj_string=proj_string,
      in_epoch_override=as.integer(as.POSIXct(file.info(geom_path)$mtime)),
      vdat = vdat_trans,
      quiet=quiet)

    # Record errors for comp
    if(nrow(ghdf_pts[[1]]) > 0) {
      if(vdat_trans) {
        ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]], "{}:{}:{x}") %>% as.numeric()
      } else {
        ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]],"{}:{x}") %>% as.numeric()
      }
      if(is.na(ghdf_ptserr)) { ghdf_ptserr = -99999 } # A very large number, I did something wrong here
    }
  }

  cond4 = tryCatch({
      isTRUE(nrow(g_pts[[1]]) > 0)
    },
    error = function(e) {
      FALSE
    }
  )
  cond5 = tryCatch({
      isTRUE(nrow(ghdf_pts[[1]]) > 0)
    },
    error = function(e) {
      FALSE
    }
  )

  # cond5 = nrow(ghdf_pts[[1]]) > 0

  # If we could parse both, which was a better extraction?
  if(cond4 & cond5) {
    if(default_g) {
      extrated_pts <- g_pts
      extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")
    }
    if(is.na(g_ptserr)){
      extrated_pts <- ghdf_pts
      extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")
    } else if(abs(ghdf_ptserr) > abs(g_ptserr)) {
      extrated_pts <- g_pts
      extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")
    } else {
      extrated_pts <- ghdf_pts
      extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")
    }

    # We could only parse a g file
  } else if(cond4) {
    extrated_pts <- g_pts
    extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")

    # We could only parse a ghdf file
  } else if(cond5) {
    extrated_pts <- ghdf_pts
    extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")

    # Nothing was parsed
  } else if(!cond4 & !cond5) {
    extrated_pts <- FALSE
    return(extrated_pts)
  }

  return(extrated_pts)
}

