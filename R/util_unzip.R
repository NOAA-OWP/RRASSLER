#' @title unzip nested folders
#' @description unzips nested zip files
#' @param zippath path to zip
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return unzipped dir
#' @family helper
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::util_unzip(file.path(database_path,"_temp","BLE",HUCID,glue::glue("{HUCID}_models.zip"),fsep = .Platform$file.sep),is_quiet = is_quiet)
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[utils]{unzip}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[dplyr]{setops}}
#' @rdname util_unzip
#' @export
#' @importFrom stringr str_sub
#' @importFrom utils unzip
#' @importFrom glue glue
#' @importFrom dplyr setdiff

util_unzip <- function(zippath, is_quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/util_unzip.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  fn_time_start <- Sys.time()

  if (stringr::str_sub(zippath,-3,-1) == "zip") {
    utils::unzip(zippath, exdir = file.path(
      dirname(zippath),
      gsub('.{4}$', '', basename(zippath)),
      fsep = .Platform$file.sep
    ))
    zippath <-
      file.path(dirname(zippath),
                gsub('.{4}$', '', basename(zippath)),
                fsep = .Platform$file.sep)
  }
  list_of_processed_zips <- c()
  files_to_process <-
    list.files(
      zippath,
      pattern = glob2rx(glue::glue("*.zip$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  i = 1

  while (length(files_to_process) > 0) {
    print(
      glue::glue(
        "Unzipping iteration: {i} - Files processed: {length(list_of_processed_zips)}"
      )
    )
    for (zip_file in files_to_process) {
      utils::unzip(zip_file,
                   exdir = file.path(
                     dirname(zip_file),
                     gsub('.{4}$', '', basename(zip_file)),
                     fsep = .Platform$file.sep
                   ))
    }
    list_of_processed_zips <-
      append(list_of_processed_zips, files_to_process)
    list_of_all_zips <-
      list.files(
        zippath,
        pattern = glob2rx(glue::glue("*.zip$")),
        full.names = TRUE,
        ignore.case = TRUE,
        recursive = TRUE
      )
    files_to_process <-
      dplyr::setdiff(list_of_all_zips, list_of_processed_zips)
    i <- i + 1
  }

  disk_size <-
    round(sum(file.info(
      list.files(zippath, full.names = TRUE, recursive = TRUE)
    )$size) * 1e-9, 3)
  if (!is_quiet) {
    disk_size <-
      round(sum(file.info(
        list.files(zippath, full.names = TRUE, recursive = TRUE)
      )$size) * 1e-9, 3)
    print(
      glue::glue(
        "Finished: Unzipped {length(list_of_processed_zips)} files with a disk size of ~{disk_size} GB in {round(difftime(Sys.time(), fn_time_start, units='mins'), digits = 2)} minutes"
      )
    )
  }
  return(TRUE)
}
