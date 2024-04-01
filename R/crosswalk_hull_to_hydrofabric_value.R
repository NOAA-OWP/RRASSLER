#' @title crosswalk_hull_to_hydrofabric_value
#' @description crosswalk_hull_to_hydrofabric_value
#' @param hull the model cross section hulls
#' @param river the river streamlines
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @family helpers
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  extrated_pts <- parse_model_to_xyz(geom_path = "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/12090301/12090301_models/Model/Alum Creek-Colorado River/ALUM 006/ALUM 006.g01",units = "English Units",proj_string = "EPSG:2277",vdat_trans = FALSE,quiet = FALSE)
#'
#'  ls = sfheaders::sf_linestring(
#'    obj = extrated_pts[[1]],
#'    x = "x",
#'    y = "y",
#'    linestring_id = "xid",
#'    keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
#'  ls_final_line_index <- nrow(ls)
#'  ls_end_index <- nrow(ls)-1
#'  ls_middle_lines_end <- ls[2:ls_end_index,] |> lwgeom::st_endpoint()
#'  ls_middle_lines_start <- ls[2:ls_end_index,] %>% lwgeom::st_startpoint()
#'
#'  df_hull_pts <- rbind(
#'    sf::st_coordinates(ls[1,]$geometry)[, -c(3)],
#'    sf::st_coordinates(ls_middle_lines_end),
#'    apply(sf::st_coordinates(ls[ls_final_line_index,]$geometry)[, -c(3)], 2, rev),
#'    apply(sf::st_coordinates(ls_middle_lines_start), 2, rev))
#'  hull = sfheaders::sf_polygon(
#'    obj = df_hull_pts,
#'    x = "X",
#'    y = "Y",
#'    keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
#'
#'  river <- extrated_pts[[3]]
#'  current_nhdplus_comid <- crosswalk_hull_to_hydrofabric_value(hull,river)
#'  }
#' }
#' @seealso
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_crs}}
#' @rdname crosswalk_hull_to_hydrofabric_value
#' @export
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom sf st_transform st_crs

crosswalk_hull_to_hydrofabric_value <- function(hull,river) {
  # sinew::moga(file.path(getwd(),"R/crosswalk_hull_to_hydrofabric_value.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()

  ## -- Start --
  flow_catch_list <- try({
  nhdplusTools::get_nhdplus(AOI::aoi_get(hull), realization = "catchment")
  }) |> suppressMessages()
  flow_line_list <- try({
    nhdplusTools::get_nhdplus(AOI::aoi_get(hull), realization = "flowline")
  }) |> suppressMessages()

  # Was the nhdtool successful
  if (c("try-error") %in% class(c(flow_catch_list,flow_line_list))) {
    crosswalk_feature = 3
    return(crosswalk_feature)
  }

  # What are the actual intersections?
  flow_catch_intersect <- flow_catch_list[sf::st_transform(river,crs = sf::st_crs(flow_catch_list)),]
  if(length(flow_line_list) > 0) {

    flow_line_intersect <- try(flow_line_list[sf::st_make_valid(sf::st_transform(hull,crs = sf::st_crs(flow_line_list))),], silent = TRUE)
    if ("try-error" %in% class(flow_line_intersect)) {
      print_warning_block()
      message("Conflation error: invalid geometry found, attempting to fix...")
      sf::sf_use_s2(FALSE)
      flow_line_intersect <- try(flow_line_list[sf::st_make_valid(sf::st_transform(hull,crs = sf::st_crs(flow_line_list))),], silent = TRUE)
      sf::sf_use_s2(TRUE)
      if ("try-error" %in% class(flow_line_intersect)) {
        message("failed")
        return(5)
      }
    }
  } else {
    flow_line_intersect <- data.frame()
  }

  # If nothing in lines conflates, use the largest catchment featureid or 2
  if (nrow(flow_line_intersect) == 0) {
    if(nrow(flow_catch_intersect) == 0) {
      crosswalk_feature = 2
    } else {
      crosswalk_feature = flow_catch_intersect[order(flow_catch_intersect$shape_length, decreasing = TRUE), ][1,]$featureid
    }

    return(crosswalk_feature)
  }

  valid_catchment_flow_lines <- flow_line_intersect[flow_line_intersect$comid %in% flow_catch_intersect$featureid,]

  # Is anything conflated across both?
  if (nrow(valid_catchment_flow_lines) == 0) {
    if(nrow(flow_catch_intersect) == 0) {
      crosswalk_feature = 2
    } else {
      crosswalk_feature = flow_catch_intersect[order(flow_catch_intersect$shape_length, decreasing = TRUE), ][1,]$featureid
    }
  } else {
    largest_order_streams = valid_catchment_flow_lines[valid_catchment_flow_lines$streamorde == max(valid_catchment_flow_lines$streamorde),]
    lowest_streamline = largest_order_streams[order(largest_order_streams$arbolatesu, decreasing = TRUE),]
    crosswalk_feature = lowest_streamline[1,]$comid
  }

  return(crosswalk_feature)
}
