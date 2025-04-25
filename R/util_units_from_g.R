#' @title util_units_from_g
#' @description helper to grab units from geom path
#' @param gpath path to geometry file
#' @param is_quiet PARAM_DESCRIPTION, Default: FALSE
#' @returns units for RRASSLER processing
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[glue]{glue}}
#' @rdname util_units_from_g
#' @export
#' @importFrom utils glob2rx read.delim
#' @importFrom glue glue
#'
util_units_from_g <- function(gpath, is_quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/util_units_from_g.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  # gpath <- list.files(file.path(ras_dbase,"_temp","M3",tools::file_path_sans_ext(basename(target_zips[index])),fsep = .Platform$file.sep),pattern = "*g01$",full.names = TRUE,ignore.case = TRUE,recursive = TRUE)[1]
  # dirname(gpath)
  prj_files <- list.files(dirname(gpath),pattern = utils::glob2rx(glue::glue("{tools::file_path_sans_ext(basename(gpath))}.prj$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)

  if(length(prj_files) > 0) {
    print_warning_block()
    return(FALSE)
  }

  for (potential_file in prj_files) {
    # potential_file <- prj_files[1]
    file_text <- utils::read.delim(potential_file, header = FALSE)

    if (any(c('PROJCS', 'GEOGCS', 'DATUM', 'PROJECTION') == file_text)) {
      current_model_projection = potential_file
    } else if (grepl("SI Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "SI Units"
    } else if (grepl("English Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "English Units"
    }
  }
  return(current_model_units)
}
