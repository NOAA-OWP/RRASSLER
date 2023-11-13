#' @title FEMA region 6 BLE data scraper
#' @description Helper to download relevant BLE data
#' @param database_path Path to load data into.  NOTE: Must be LOCAL path
#' @param HUCID The huc8 ID as a string to try and scrape
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param overwrite overwrite flag for data already loaded in your path. TRUE to remove and re-download., Default: overwrite
#' @param files a string encoding to determine what files to attempt to download, can contain 'm' for models, 's' for spatial data, and 'd' for documentation, Default: 'm'
#' @return The requested BLE data as a scraped zip file
#' @family pre-process
#' @details Scraping data from https://ebfedata.s3.amazonaws.com/ based on pointers from https://webapps.usgs.gov/infrm/estBFE/.  See also the Texas specific dashboard at https://www.arcgis.com/apps/dashboards/1e98f1e511fc40d3b08790a4251a64ee for more BLE base models.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  database_path <- file.path("~/data/ras_catalog/")
#'  RRASSLER::scrape_ble_lib(database_path,'12090301',is_quiet = FALSE,overwrite = FALSE,files = "ms")
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
#'  \code{\link[httr]{GET}}, \code{\link[httr]{write_disk}}
#'  \code{\link[glue]{glue}}
#' @rdname scrape_ble_lib
#' @export
#' @importFrom sf st_transform st_read st_crs
#' @importFrom httr GET write_disk
#' @importFrom glue glue
scrape_ble_lib <-
  function(database_path,
           HUCID,
           is_quiet = FALSE,
           overwrite = overwrite,
           files = "m") {
    # sinew::moga(file.path(getwd(),"R/scrape_ble_lib.R"),overwrite = TRUE)
    # devtools::document()
    # pkgdown::build_site(new_process=FALSE)
    # devtools::load_all()

    ## -- Start --
    fn_time_start <- Sys.time()

    output_dir <-
      file.path(database_path, "_temp", "BLE", HUCID, fsep = .Platform$file.sep)
    if (file.exists(output_dir)) {
      if (overwrite) {
        unlink(output_dir, recursive = TRUE)
      } else {
        print_warning_block()
        print("file downloaded and overwrite is set to FALSE")
        return(FALSE)
      }
    }

    template_hucs <-
      sf::st_transform(sf::st_read(
        file.path(database_path, "HUC8.fgb", fsep = .Platform$file.sep),
        quiet = is_quiet
      ),
      sf::st_crs("EPSG:5070"))
    Potential_features <- template_hucs[template_hucs$huc8 == HUCID, ]

    if (nrow(Potential_features) == 0) {
      print_warning_block()
      print("No features found")
      return(FALSE)
    }
    Potential_features$SpatialData_url =
      paste0(
        "https://ebfedata.s3.amazonaws.com/",
        Potential_features$huc8,
        "_",
        gsub(
          " ",
          "",
          gsub("-", "", Potential_features$name, fixed = TRUE),
          fixed = TRUE
        ),
        "/",
        Potential_features$huc8,
        "_SpatialData.zip"
      )
    Potential_features$RASData_url =
      paste0(
        "https://ebfedata.s3.amazonaws.com/",
        Potential_features$huc8,
        "_",
        gsub(
          " ",
          "",
          gsub("-", "", Potential_features$name, fixed = TRUE),
          fixed = TRUE
        ),
        "/",
        Potential_features$huc8,
        "_Models.zip"
      )
    Potential_features$Reports_url =
      paste0(
        "https://ebfedata.s3.amazonaws.com/",
        Potential_features$huc8,
        "_",
        gsub(
          " ",
          "",
          gsub("-", "", Potential_features$name, fixed = TRUE),
          fixed = TRUE
        ),
        "/",
        Potential_features$huc8,
        "_Documents.zip"
      )

    if (url_exists(Potential_features$SpatialData_url)) {
      dir.create(output_dir, recursive = TRUE)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      if (grepl("m", files, fixed = TRUE)) {
        if (!is_quiet) {
          message("Trying models")
        }
        httr::GET(
          Potential_features$RASData_url,
          httr::write_disk(
            file.path(
              output_dir,
              basename(Potential_features$RASData_url),
              fsep = .Platform$file.sep
            ),
            overwrite = TRUE
          ),
          overwrite = TRUE
        )
      }
      if (grepl("s", files, fixed = TRUE)) {
        if (!is_quiet) {
          message("Trying spatial")
        }
        httr::GET(
          Potential_features$SpatialData_url,
          httr::write_disk(
            file.path(
              output_dir,
              basename(Potential_features$SpatialData_url),
              fsep = .Platform$file.sep
            ),
            overwrite = TRUE
          ),
          overwrite = TRUE
        )
      }
      if (grepl("d", files, fixed = TRUE)) {
        if (!is_quiet) {
          message("Trying docs")
        }
        httr::GET(
          Potential_features$Reports_url,
          httr::write_disk(
            file.path(
              output_dir,
              basename(Potential_features$Reports_url),
              fsep = .Platform$file.sep
            ),
            overwrite = TRUE
          ),
          overwrite = TRUE
        )
      }

      if (!is_quiet) {
        disk_size <-
          round(sum(file.info(
            list.files(
              output_dir,
              full.names = TRUE,
              recursive = TRUE
            )
          )$size) * 1e-9, 3)
        message(
          glue::glue(
            "Scraped {disk_size} GB in {round(difftime(Sys.time(), fn_time_start, units='mins'), digits = 2)} minutes"
          )
        )
      }
      return(TRUE)
    } else {
      if (!is_quiet) {
        print_warning_block()
        print("URL was not found, tested values:")
        print(Potential_features$SpatialData_url)
      }
      return(FALSE)
    }
  }
