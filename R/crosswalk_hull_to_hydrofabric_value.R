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

  # Was the nhdtool sucessful
  if (c("try-error") %in% class(c(flow_catch_list,flow_line_list))) {
    crosswalk_feature = 3
    return(crosswalk_feature)
  }

  # What are the actual intersections?
  flow_catch_intersect <- flow_catch_list[sf::st_transform(river,crs = sf::st_crs(flow_catch_list)),]
  flow_line_intersect <- flow_line_list[sf::st_transform(hull,crs = sf::st_crs(flow_line_list)),]

  # If nothing in lines conflates, use the largest catchment featureid or 2
  if (nrow(flow_line_intersect) == 0) {
    if(nrow(flow_catch_intersect) == 0) {
      crosswalk_feature = 2
    } else {
      crosswalk_feature = flow_catch_intersect[order(flow_catch_intersect$shape_length, decreasing = TRUE), ]$featureid
    }

    return(crosswalk_feature)
  }

  valid_catchment_flow_lines <- flow_line_intersect[flow_line_intersect$comid %in% flow_catch_intersect$featureid,]

  # Is anything conflated across both?
  if (nrow(valid_catchment_flow_lines) == 0) {
    if(nrow(flow_catch_intersect) == 0) {
      crosswalk_feature = 2
    } else {
      crosswalk_feature = flow_catch_intersect[order(flow_catch_intersect$shape_length, decreasing = TRUE), ]$featureid
    }
  } else {
    largest_order_streams = valid_catchment_flow_lines[valid_catchment_flow_lines$streamorde == max(valid_catchment_flow_lines$streamorde),]
    lowest_streamline = largest_order_streams[order(largest_order_streams$arbolatesu, decreasing = TRUE),]
    crosswalk_feature = lowest_streamline[1,]$comid
  }

  return(crosswalk_feature)
}
