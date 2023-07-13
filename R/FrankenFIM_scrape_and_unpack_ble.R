#' @title scrape_and_unpack_ble
#' @description wrapper around BLE handles
#' @return unzipped dir of BLE data
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::FrankenFIM_scrape_and_unpack_ble(database_path=NULL,HUCID=NULL,quiet=TRUE,overwrite=FALSE,full=FALSE)
#'  }
#' }
#' @param database_path PARAM_DESCRIPTION, Default: NULL
#' @param HUCID PARAM_DESCRIPTION, Default: NULL
#' @param quiet PARAM_DESCRIPTION, Default: TRUE
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param full PARAM_DESCRIPTION, Default: FALSE
#' @seealso
#'  \code{\link[httr]{HEAD}}, \code{\link[httr]{GET}}, \code{\link[httr]{status_code}}, \code{\link[httr]{write_disk}}
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[utils]{unzip}}
#'  \code{\link[dplyr]{setops}}
#' @rdname FrankenFIM_scrape_and_unpack_ble
#' @export
#' @importFrom httr HEAD GET status_code write_disk
#' @importFrom sf st_transform st_read st_crs
#' @importFrom glue glue
#' @importFrom stringr str_sub
#' @importFrom utils unzip
#' @importFrom dplyr setdiff

FrankenFIM_scrape_and_unpack_ble <- function(database_path=NULL,HUCID=NULL,quiet=TRUE,overwrite=FALSE,full=FALSE) {

  # sinew::moga(file.path(getwd(),"R/FrankenFIM_scrape_and_unpack_ble.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()

  # RRASSLER::FrankenFIM_scrape_and_unpack_ble(database_path="Z:/data/ras_catalog",HUCID='12090301,quiet=FALSE,overwrite=FALSE,full=TRUE)

  ## -- Start --
  url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {

    suppressPackageStartupMessages({ require("httr", quietly = FALSE, warn.conflicts = FALSE) })
    capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
      tryCatch(
        list(result = code, error = NULL),
        error = function(e) {
          if (!quiet)
            message("Error: ", e$message)

          list(result = otherwise, error = e)
        },
        interrupt = function(e) {
          stop("Terminated by user", call. = FALSE)
        }
      )
    }

    safely <- function(.f, otherwise = NULL, quiet = TRUE) {
      function(...) capture_error(.f(...), otherwise, quiet)
    }

    sHEAD <- safely(httr::HEAD)
    sGET <- safely(httr::GET)

    # Try HEAD first since it's lightweight
    res <- sHEAD(x, ...)

    if (is.null(res$result) ||
        ((httr::status_code(res$result) %/% 200) != 1)) {

      res <- sGET(x, ...)

      if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors

      if (((httr::status_code(res$result) %/% 200) != 1)) {
        if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
        return(non_2xx_return_value)
      }

      return(TRUE)

    } else {
      return(TRUE)
    }

  }
  scrape_ble_lib <- function(database_path,HUCID,quiet=quiet,overwrite=overwrite,full=full) {
    fn_time_start <- Sys.time()

    output_dir <- file.path(database_path,"_temp","BLE",HUCID, fsep=.Platform$file.sep)
    if(file.exists(output_dir)){
      if(overwrite) {
        unlink(output_dir, recursive=TRUE)
      } else {
        print_warning_block()
        print("file downloaded and overwrite is set to FALSE")
        return(FALSE)
      }
    }
    dir.create(output_dir,recursive = TRUE)

    template_hucs <- sf::st_transform(sf::st_read(file.path(database_path,"HUC8.fgb",fsep=.Platform$file.sep),quiet=quiet),sf::st_crs("EPSG:5070"))

    Potential_features <- template_hucs[template_hucs$huc8 == HUCID,]

    if(nrow(Potential_features)==0) {
      print_warning_block()
      print("No features found")
      return(FALSE)
    }
    Potential_features$SpatialData_url =
      paste0("https://ebfedata.s3.amazonaws.com/",Potential_features$huc8,"_",gsub(" ","",gsub("-","",Potential_features$name,fixed = TRUE),fixed = TRUE),"/",Potential_features$huc8,"_SpatialData.zip")
    Potential_features$RASData_url =
      paste0("https://ebfedata.s3.amazonaws.com/",Potential_features$huc8,"_",gsub(" ","",gsub("-","",Potential_features$name,fixed = TRUE),fixed = TRUE),"/",Potential_features$huc8,"_Models.zip")
    Potential_features$Reports_url =
      paste0("https://ebfedata.s3.amazonaws.com/",Potential_features$huc8,"_",gsub(" ","",gsub("-","",Potential_features$name,fixed = TRUE),fixed = TRUE),"/",Potential_features$huc8,"_Documents.zip")

    if(url_exists(Potential_features$SpatialData_url)) {
      if (!dir.exists(output_dir)) { dir.create(output_dir) }
      httr::GET(Potential_features$RASData_url,
                httr::write_disk(file.path(output_dir,basename(Potential_features$RASData_url), fsep=.Platform$file.sep), overwrite=TRUE),
                overwrite=TRUE)
      if(full) {
        httr::GET(Potential_features$SpatialData_url,
                httr::write_disk(file.path(output_dir,basename(Potential_features$SpatialData_url), fsep=.Platform$file.sep), overwrite=TRUE),
                overwrite=TRUE)
        httr::GET(Potential_features$Reports_url,
                  httr::write_disk(file.path(output_dir,basename(Potential_features$Reports_url), fsep=.Platform$file.sep), overwrite=TRUE),
                  overwrite=TRUE)
      }

      if(!quiet) {
        disk_size <- round(sum(file.info(list.files(output_dir,full.names=TRUE,recursive = TRUE))$size) * 1e-9,3)
        print(glue::glue("Scraped {disk_size} GB in {round(difftime(Sys.time(), fn_time_start, units='mins'), digits = 2)} minutes"))
      }
      return(TRUE)
    } else {
      if(!quiet) {
        print_warning_block()
        print("URL was not found, tested values:")
        print(Potential_features$SpatialData_url)
      }
      return(FALSE)
    }
  }
  unzipZipfiles <- function(zippath,quiet=quiet) {
    fn_time_start <- Sys.time()

    if(stringr::str_sub(zippath,-3,-1) == "zip") {
      utils::unzip(zippath, exdir = file.path(dirname(zippath),gsub('.{4}$', '',basename(zippath)),fsep = .Platform$file.sep))
      zippath <- file.path(dirname(zippath),gsub('.{4}$', '',basename(zippath)),fsep = .Platform$file.sep)
    }
    list_of_processed_zips <- c()
    files_to_process <- list.files(zippath, pattern=glob2rx(glue::glue("*.zip$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    i = 1
    while(length(files_to_process) > 0) {
      print(glue::glue("Unzipping iteration: {i} - Files processed: {length(list_of_processed_zips)}"))
      for(zip_file in files_to_process) {
        utils::unzip(zip_file, exdir = file.path(dirname(zip_file),gsub('.{4}$', '',basename(zip_file)),fsep = .Platform$file.sep))
      }
      list_of_processed_zips <- append(list_of_processed_zips,files_to_process)
      list_of_all_zips <- list.files(zippath, pattern=glob2rx(glue::glue("*.zip$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
      files_to_process <- dplyr::setdiff(list_of_all_zips,list_of_processed_zips)
      i <- i+1
    }
    disk_size <- round(sum(file.info(list.files(zippath,full.names=TRUE,recursive = TRUE))$size) * 1e-9,3)
    if(!quiet) {
      disk_size <- round(sum(file.info(list.files(zippath,full.names=TRUE,recursive = TRUE))$size) * 1e-9,3)
      print(glue::glue("Finished: Unzipped {length(list_of_processed_zips)} files with a disk size of ~{disk_size} GB in {round(difftime(Sys.time(), fn_time_start, units='mins'), digits = 2)} minutes"))
    }
    return(TRUE)
  }

  if(file.exists(file.path(database_path,"_temp","BLE",HUCID,glue::glue("{HUCID}_models"),fsep = .Platform$file.sep))) {
    if(!overwrite) {
      print_warning_block()
      print("found prior download")
      return(FALSE)
    }
  }

  scrape_ble_lib(database_path,HUCID,quiet=quiet,overwrite=overwrite,full=full)
  unzipZipfiles(file.path(database_path,"_temp","BLE",HUCID,glue::glue("{HUCID}_models.zip"),fsep = .Platform$file.sep),quiet=quiet)

  return(TRUE)
}
