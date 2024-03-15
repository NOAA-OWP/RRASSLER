#' @title util_ras2plucker
#' @description pulls models from model catalog in a similar fashion to https://github.com/NOAA-OWP/ras2fim/blob/dev/tools/s3_get_models.py and compatible with ras2fim v2
#' @param path_to_ras_dbase PARAM_DESCRIPTION, Default: NULL
#' @param AOI PARAM_DESCRIPTION, Default: NULL
#' @param out_name PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param is_verbose PARAM_DESCRIPTION, Default: TRUE
#' @param is_quiet PARAM_DESCRIPTION, Default: FALSE
#' @return a RRASSLE'd catalog of models
#' @family dev
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname util_ras2fim_helper
#' @export
#' @importFrom glue glue

util_ras2plucker <- function(path_to_ras_dbase = NULL,
                             AOI = NULL,
                             out_name = NULL,
                             overwrite = FALSE,
                             is_verbose = TRUE,
                             is_quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/util_ras2fim_helper.R"),overwrite = TRUE)
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


  if (!is_quiet) {
    message(glue::glue("Finished: Unzipped {length(list_of_processed_zips)} files with a disk size of ~{disk_size} GB in {round(difftime(Sys.time(), fn_time_start, units='mins'), digits = 2)} minutes"))
  }
  return(TRUE)
}
