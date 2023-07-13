#' @title ingest_FEMA6_BLE
#' @description ingest or append a new set of HEC-RAS models from FEMA region 6 Base Level Engineering (BLE) into your database
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param HUC8 The HUC8 string you'd like to ingest
#' @param full PARAM_DESCRIPTION, Default: FALSE
#' @param proj_override an EPSG string to apply should a projection not be found, Default: NULL
#' @param vdat_trans a flag to dictate whether or not to apply a vdatum transformation, TRUE to apply, FALSE to skip, Default: FALSE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param chatty flag to dictate whether print statements from within the extraction are suppressed, TRUE to show messages and FALSE to suppress them, Default: FALSE
#' @param ping_me a string with an email used to send emails after processing.  Uses gmailr and requires config, Default: NULL
#' @param quick_check a flag to dictate whether or not to perform a quick check to see if a model has already been processed based on the raw name of the file.  Useful in reingesting the same directory after an error but not recommended otherwise.  TRUE to see if a name matches and skips, FALSE to process all the way though, Default: FALSE
#' @param quick_hull a flag to dictate how tightly you want the footprints wrapped, TRUE uses just end points of the linestrings to place the model, FALSE uses the full point database, Default: FALSE
#' @param overwrite not currently implemented, Default: FALSE
#' @param refresh flag to dictate whether or not to recollate spatial database after ingest process.  FALSE to skip, TRUE to regenerate, Default: TRUE
#' @return a RRASSLED catalog of HEC-RAS models
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::ingest_FEMA6_BLE(
#'path_to_ras_dbase="/home/rstudio/g/data/ras_catalog/",
#'HUC8="12090301",
#'full=FALSE,
#'proj_override="EPSG:2277",
#'vdat_trans=FALSE,
#'quiet=FALSE,
#'chatty = TRUE,
#'quick_check=FALSE,
#'quick_hull = FALSE,
#'overwrite = FALSE,
#'refresh = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.table]{as.data.table}}, \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
#'  \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
#'  \code{\link[unglue]{unglue}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[holyhull]{holyhull}}
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[arrow]{write_parquet}}
#'  \code{\link[gmailr]{gm_mime}}, \code{\link[gmailr]{gm_to}}, \code{\link[gmailr]{gm_send_message}}
#' @rdname ingest_FEMA6_BLE
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom stringr str_sub str_detect
#' @importFrom unglue unglue_vec
#' @importFrom sfheaders sf_linestring
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom holyhull holyhull
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet
#' @importFrom gmailr gm_mime gm_to gm_from gm_subject gm_text_body gm_send_message
#' @importFrom data.table as.data.table data.table rbindlist fwrite `:=`
#'
ingest_FEMA6_BLE <- function(path_to_ras_dbase,
                             HUC8,
                             full=FALSE,
                             proj_override = NULL,
                             vdat_trans = FALSE,
                             quiet = TRUE,
                             chatty = FALSE,
                             ping_me = NULL,
                             quick_check = FALSE,
                             quick_hull = FALSE,
                             overwrite = FALSE,
                             refresh = TRUE) {
  # sinew::moga(file.path(getwd(),"R/ingest_FEMA6_BLE.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase="/home/rstudio/g/data/ras_dbase"
  # top_of_dir_to_scrape="/home/rstudio/g/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data"
  # code_to_place_in_source="ras2fim_test_data"
  # proj_overwrite="EPSG:26915"

  # top_of_dir_to_scrape="/home/rstudio/g/data/raw/BLE/FEMA/12090301/12090301_Models/Model/Alum Creek-Colorado River/ALUM 006"
  # code_to_place_in_source="FEMA R6"
  # proj_overwrite="EPSG:2277"

  # path_to_ras_dbase="C:/Users/jimma/Desktop/data/newer_ras_dbase"
  # top_of_dir_to_scrape="C:/Users/jimma/Desktop/data/BLE/FEMA/12090301/12090301_Models/Model/Walnut Creek-Colorado River/CEDAR HOLLOW CREEK"
  # top_of_dir_to_scrape="C:/Users/jimma/Desktop/data/BLE/FEMA/12090301/12090301_Models/Model/Walnut Creek-Colorado River/ELM CREEK"
  # code_to_place_in_source="FEMA 6 BLE"
  # proj_overwrite="EPSG:2277"
  # vdat_trans=FALSE
  # quiet=FALSE
  # chatty = TRUE
  # quick_check=TRUE
  # ping_me = TRUE
  # overwrite = FALSE
  # refresh = TRUE
  # gmailr::gm_auth_configure(path = "C:/Users/jimma/Desktop/client_secret_765662520275-iduoi88oke14pqst3ebukn5rb2qf0895.apps.googleusercontent.com.json")

  ## -- Start --
  fn_time_start <- Sys.time()
  FrankenFIM_scrape_and_unpack_ble(database_path=path_to_ras_dbase,HUCID=HUC8,quiet=quiet,full=full)

  ingest_into_database(path_to_ras_dbase=path_to_ras_dbase,
                                   top_of_dir_to_scrape=file.path(path_to_ras_dbase,"_temp","BLE",HUC8,glue::glue("{HUC8}_models"),fsep = .Platform$file.sep),
                                   code_to_place_in_source="FEMA Region 6",
                       proj_override = proj_override,
                                   vdat_trans = vdat_trans,
                                   quiet = quiet,
                                   chatty = chatty,
                                   ping_me = ping_me,
                                   quick_check = quick_check,
                                   quick_hull = quick_hull,
                                   overwrite = overwrite,
                                   refresh = refresh)
  return(TRUE)
}
