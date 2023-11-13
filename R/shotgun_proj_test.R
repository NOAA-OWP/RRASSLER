#' @title shotgun_proj_test
#' @description a helper function to geographically test projections
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
#' @details in dev
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  RRASSLER::shotgun_proj_test(path_to_model_g="G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Model/Walnut Creek-Colorado River/CEDAR CREEK/CEDAR CREEK.g01",out_path="G:/data/ras_catalog/_temp/test",units = "Foot",proj_test_list = c("EPSG:2277","ESRI:102739"),in_epoch_override = as.integer(as.POSIXct(Sys.time())),out_epoch_override = as.integer(as.POSIXct(Sys.time())),vdat_trans = FALSE,quiet = FALSE,default_g = FALSE,try_both = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[sfheaders]{sf_linestring}}, \code{\link[sfheaders]{sf_polygon}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_write}}
#'  \code{\link[lwgeom]{st_startpoint}}
#' @rdname shotgun_proj_test
#' @export
#' @importFrom glue glue
#' @importFrom sfheaders sf_linestring sf_polygon
#' @importFrom sf st_set_crs st_crs st_coordinates st_write
#' @importFrom lwgeom st_endpoint st_startpoint

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
      obj = extrated_pts[[1]],
      x = "x",
      y = "y",
      linestring_id = "xid",
      keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
    ls_final_line_index <- nrow(ls)
    ls_end_index <- nrow(ls)-1
    ls_middlel_lines_end <- ls[2:ls_end_index,] |> lwgeom::st_endpoint()
    ls_middlel_lines_start <- ls[2:ls_end_index,] %>% lwgeom::st_startpoint()

    df_hull_pts <- rbind(
      sf::st_coordinates(ls[1,]$geometry)[, -c(3)],
      sf::st_coordinates(ls_middlel_lines_end),
      apply(sf::st_coordinates(ls[ls_final_line_index,]$geometry)[, -c(3)], 2, rev),
      apply(sf::st_coordinates(ls_middlel_lines_start), 2, rev))
    hull = sfheaders::sf_polygon(
      obj = df_hull_pts,
      x = "X",
      y = "Y",
      keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
    sf::st_write(hull,file.path(out_path,glue::glue("footprint_{gsub(':','',test_proj)}.fgb"),fsep = .Platform$file.sep))
  }

  message("fin")

}
