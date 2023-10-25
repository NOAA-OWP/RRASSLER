#' @title shotgun_proj_test
#' @description FUNCTION_DESCRIPTION
#' @param path_to_model_g PARAM_DESCRIPTION
#' @param out_path PARAM_DESCRIPTION
#' @param proj_test_list PARAM_DESCRIPTION, Default: c("EPSG:2277", "ESRI:102739")
#' @param units PARAM_DESCRIPTION, Default: 'Foot'
#' @param in_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param out_epoch_override PARAM_DESCRIPTION, Default: as.integer(as.POSIXct(Sys.time()))
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @param default_g PARAM_DESCRIPTION, Default: FALSE
#' @param try_both PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @family pre-process
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::shotgun_proj_test(path_to_model_g="G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Walnut Creek-Colorado River/CEDAR CREEK/CEDAR CREEK.g01",out_path="G:/data/ras_catalog/_temp/test",units = "Foot",proj_test_list = c("EPSG:2277","ESRI:102739"),in_epoch_override = as.integer(as.POSIXct(Sys.time())),out_epoch_override = as.integer(as.POSIXct(Sys.time())),vdat_trans = FALSE,quiet = FALSE,default_g = FALSE,try_both = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[holyhull]{holyhull}}
#' @rdname shotgun_proj_test
#' @export
#' @importFrom glue glue
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_set_crs st_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom holyhull holyhull
shotgun_proj_test <- function(path_to_model_g,
                              out_path,
                              proj_test_list = c("EPSG:2277","ESRI:102739"),
                              units = "Foot",
                              in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                              out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                              vdat_trans = FALSE,
                              quiet = FALSE,
                              default_g = FALSE,
                              try_both = TRUE) {
  # sinew::moga(file.path(getwd(),"R/shotgun_proj_test.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # geom_path="G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Walnut Creek-Colorado River/CEDAR CREEK/CEDAR CREEK.g01"
  # out_path="G:/data/ras_catalog/_temp/test"
  # units = "Foot"
  # proj_test_list = c("EPSG:2277","ESRI:102739")
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # vdat_trans = FALSE
  # quiet = FALSE
  # default_g = FALSE
  # try_both = TRUE

  ## -- Start --
  # geom_path="G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Walnut Creek-Colorado River/CEDAR CREEK/CEDAR CREEK.g01"

  # extrated_pts <- parse_model_to_xyz(geom_path=geom_path,
  #                                    units=units,
  #                                    proj_string=proj_string,
  #                                    in_epoch_override = as.integer(as.POSIXct(Sys.time())),
  #                                    out_epoch_override = as.integer(as.POSIXct(Sys.time())),
  #                                    vdat_trans=vdat_trans,
  #                                    quiet=quiet,
  #                                    default_g=default_g,
  #                                    try_both=try_both)
  dir.create(file.path(out_path,fsep = .Platform$file.sep), showWarnings = TRUE)

  for(test_proj in proj_test_list) {
    extrated_pts <- try({
      message(glue::glue("Trying: {test_proj}"))
      parse_model_to_xyz(geom_path=path_to_model_g,
                         units=units,
                         proj_string=test_proj,
                         in_epoch_override = in_epoch_override,
                         out_epoch_override = out_epoch_override,
                         vdat_trans = vdat_trans,
                         quiet = quiet,
                         default_g = default_g,
                         try_both = try_both)
    })
    if(nrow(extrated_pts[[1]])==0){
      message("No points extracted")
    }

    # Footprint the points we extracted
    ls = sfheaders::sf_linestring(
      obj = extrated_pts[[1]]
      , x = "x"
      , y = "y"
      # , z = "z"
      , linestring_id = "xid"
      , keep = FALSE
    ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
    #
    if(quick_hull) {
      end_points <- c(ls %>% lwgeom::st_endpoint(), ls %>% lwgeom::st_startpoint())
      end_points <- sf::st_coordinates(end_points) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X,Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X","Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    } else {
      end_points <- sf::st_coordinates(ls) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X,Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X","Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    }

    ahull_poly = holyhull::holyhull(sf_frame=end_points, method='convave', alpha_value=0.01, concavity = 2, length_threshold = 0)
    sf::st_write(ahull_poly,file.path(out_path,glue::glue("footprint_{gsub(':','',test_proj)}.fgb"),fsep = .Platform$file.sep))
  }

  message("fin")

}
