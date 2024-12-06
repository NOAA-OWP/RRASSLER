#' @title xs_to_transect_picker
#' @description Picks transects
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param line_select_subset PARAM_DESCRIPTION, Default: NULL
#' @param hf_lines Hydrofabric lines
#' @param reference_divides reference divide paths
#' @param reference_flowlines reference flowline paths
#' @param existing_transects  reference transect paths
#' @param existing_points reference points paths
#' @param overwrite overwrite files if we find identical models, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param test_limit Number of transects to use, Default: NULL
#' @returns selected HEC-RAS lines in hd3d form
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  xs_to_transect_picker(path_to_ras_dbase = NULL,line_select_subset = NULL,hf_lines,overwrite = FALSE,is_verbose = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{sf}}, \code{\link[sf]{sfc}}, \code{\link[sf]{st_cast}}, \code{\link[sf]{st_line_sample}}, \code{\link[sf]{st_as_sfc}}, \code{\link[sf]{st_bbox}}, \code{\link[sf]{geos_binary_ops}}, \code{\link[sf]{st_nearest_feature}}, \code{\link[sf]{geos_measures}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_write}}
#'  \code{\link[arrow]{read_parquet}}, \code{\link[arrow]{open_dataset}}, \code{\link[arrow]{write_parquet}}
#'  \code{\link[dplyr]{compute}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}
#'  \code{\link[glue]{glue}}
#' @rdname xs_to_transect_picker
#' @export
#' @importFrom sf st_read st_as_sf st_crs st_transform st_sf st_sfc st_cast st_line_sample st_as_sfc st_bbox st_intersection st_nearest_feature st_length st_coordinates st_write
#' @importFrom arrow read_parquet open_dataset write_parquet
#' @importFrom dplyr collect mutate row_number
#' @importFrom glue glue
xs_to_transect_picker <- function(path_to_ras_dbase = NULL,
                                  line_select_subset = NULL,
                                  hf_lines,
                                  reference_divides,
                                  reference_flowlines,
                                  existing_transects,
                                  existing_points,
                                  overwrite = FALSE,
                                  is_verbose = TRUE,
                                  test_limit = NULL) {
  # sinew::moga(file.path(getwd(),"R/xs_to_transect_picker.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()

  # path_to_ras_dbase = "~/data/ras_catalog/"
  # # path_to_ras_dbase = "s3://ras-models/"
  # line_select_subset = NULL
  # hf_lines = "~/data/temp/Study_Area.gpkg"
  # is_verbose = TRUE
  # overwrite = TRUE

  # path_to_ras_dbase = "~/data/ras_catalog/"
  # #
  # # # reference_flowlines = "~/data/raw/lynker-spatial/hydrofabric/v2.2/reference/conus_flowlines/"
  # # # reference_divides = "~/data/raw/lynker-spatial/hydrofabric/v2.2/reference/conus_divides/"
  # reference_divides = "~/data/raw/lynker-spatial/hydrofabric/v2.2/final/conus/conus_divides/"
  # reference_flowlines =  "~/data/raw/lynker-spatial/hydrofabric/v2.2/final/conus/conus_flowpaths/"
  # existing_transects = "~/data/raw/lynker-spatial/hydrofabric/v2.2/final/conus/3D/transects.gpkg"
  # existing_points = "~/data/raw/lynker-spatial/hydrofabric/v2.2/final/conus/3D/cs_pts.parquet"
  #
  # is_verbose = TRUE
  # overwrite = TRUE
  #
  # test_limit = 10

  ## -- Start --
  ## Load catalog
  fn_time_start <- Sys.time()
  if(!file.exists(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))) {
    print_error_block()
    message("Not a RRASSLE'd archive")
    return(FALSE)
  }

  ## Prep landing
  if(file.exists(file.path(path_to_ras_dbase,"hf_3D","ras_transects.gpkg",fsep = .Platform$file.sep))) {
    if(!overwrite) {
      print_error_block()
      message("Output already exists and overwrite is set to false")
      return(FALSE)
    }
    if(is_verbose) {
      print_warning_block()
      message("Output already exists, overwriting")
    }
    file.remove(file.path(path_to_ras_dbase,"hf_3D","cs_pts.parquet.parquet",fsep = .Platform$file.sep))
    file.remove(file.path(path_to_ras_dbase,"hf_3D","ras_transects.gpkg",fsep = .Platform$file.sep))
  } else if(!dir.exists(file.path(path_to_ras_dbase,"hf_3D",fsep = .Platform$file.sep))) {
    dir.create(file.path(path_to_ras_dbase,"hf_3D",fsep = .Platform$file.sep))
  }

  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase, "accounting.csv", fsep = .Platform$file.sep),is_quiet = !is_verbose)
  # ras_models <- sf::st_read(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))
  ras_xs <- sf::st_read(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  # ras_xs <- ras_xs[seelct == line_select_subset,]
  ras_pts <- arrow::read_parquet(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))

  auto_transects <- sf::st_read(existing_transects,layer ='transects')
  # auto_pts <- arrow::open_dataset(existing_points) %>% dplyr::collect()

  hf_divides <- arrow::open_dataset(reference_divides) %>% dplyr::collect() %>% sf::st_as_sf()
  sf::st_crs(hf_divides) <- sf::st_crs(auto_transects)
  hf_flowlines <- arrow::open_dataset(reference_flowlines) %>% dplyr::collect() %>% sf::st_as_sf()
  sf::st_crs(hf_flowlines) <- sf::st_crs(auto_transects)

  valid_basins <- hf_divides[sf::st_transform(ras_xs,sf::st_crs(auto_transects)),]
  valid_flowlines <- hf_flowlines[sf::st_transform(ras_xs,sf::st_crs(auto_transects)),]

  the_transects_this_database_can_replace <- auto_transects[auto_transects$id %in% valid_flowlines$id,]
  the_flowlines_that_we_want_to_replace <- valid_flowlines[valid_flowlines$id %in% unique(the_transects_this_database_can_replace$id),]

  export_transects <- sf::st_sf(sf::st_sfc())
  sf::st_crs(export_transects) <- sf::st_crs(the_transects_this_database_can_replace)
  export_cs_pts <- list()

  if(!is.null(test_limit)) {
    print_warning_block()
    message("Capping")
    the_flowlines_that_we_want_to_replace <- the_flowlines_that_we_want_to_replace[1:test_limit,]
    the_transects_this_database_can_replace <- the_transects_this_database_can_replace[the_transects_this_database_can_replace$id %in% unique(the_flowlines_that_we_want_to_replace$id),]
  }

  for(i in 1:nrow(the_flowlines_that_we_want_to_replace)) {
    # i = 103
    if(is_verbose) { message(glue::glue("Crosswalking line {i} of {nrow(the_flowlines_that_we_want_to_replace)}")) }
    target_flowline <- the_flowlines_that_we_want_to_replace[the_flowlines_that_we_want_to_replace$id %in% unique(the_transects_this_database_can_replace$id)[i],] %>%
      sf::st_cast("LINESTRING")
    subset_of_transects <- the_transects_this_database_can_replace[the_transects_this_database_can_replace$id %in% unique(the_transects_this_database_can_replace$id)[i],]

    # intersection_points <- sf::st_intersection(subset_of_transects,target_flowline)
    # -or-
    intersection_points <- sf::st_line_sample(target_flowline,n = 10) %>%
      sf::st_cast("POINT") %>%
      sf::st_as_sf() %>%
      dplyr::mutate(cs_id = dplyr::row_number())

    potential_ras_lines <- ras_xs[sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(intersection_points))) %>% sf::st_transform(sf::st_crs(ras_xs)),]
    ras_flowline_crossings = sf::st_intersection(potential_ras_lines,sf::st_transform(target_flowline,sf::st_crs(ras_xs))) %>%
      sf::st_transform(sf::st_crs(target_flowline))

    # mapview::mapview(target_flowline) +
    #   mapview::mapview(subset_of_transects) +
    #   mapview::mapview(intersection_points, color = "red") +
    #   mapview::mapview(potential_ras_lines, color = "darkgreen") +
    #   mapview::mapview(ras_flowline_crossings, color = "lightgreen")

    if(nrow(ras_flowline_crossings) == 0) {
      if(is_verbose) {
        print_warning_block()
        message(glue::glue(" - No intersections of flowline to cross section"))
      }
      next
    }

    for(point_selection_index in 1:nrow(intersection_points)) {
      # point_selection_index = 1
      if(is_verbose) { message(glue::glue(" - appending {point_selection_index} of {nrow(intersection_points)} transects")) }

      target_auto_point <- intersection_points[point_selection_index,]
      nearest_ras_index <- sf::st_nearest_feature(target_auto_point, ras_flowline_crossings)

      # mapview::mapview(target_flowline) +
      #   mapview::mapview(subset_of_transects) +
      #   mapview::mapview(target_auto_point, color = "red") +
      #   mapview::mapview(ras_flowline_crossings, color = "pink") +
      #   mapview::mapview(ras_flowline_crossings[nearest_ras_index,], color = "darkred") +
      #   mapview::mapview(potential_ras_lines[potential_ras_lines$master_id== ras_flowline_crossings[nearest_ras_index,]$master_id,], color = "darkgreen") +
      #   mapview::mapview(potential_ras_lines, color = "lightgreen")

      # Push transect data
      new_line_entry <- data.frame(id = target_flowline$id,
                              cs_source = "ras",
                              cs_id = point_selection_index,
                              # cs_measure = target_auto_point$cs_measure,
                              cs_measure = sf::st_length(target_flowline) * point_selection_index/10,
                              cs_length = sf::st_length(potential_ras_lines[potential_ras_lines$master_id== ras_flowline_crossings[nearest_ras_index,]$master_id,]),
                              geom = potential_ras_lines[potential_ras_lines$master_id== ras_flowline_crossings[nearest_ras_index,]$master_id,]$geometry) %>%
        sf::st_as_sf() %>%
        sf::st_transform(sf::st_crs(auto_transects))


      # Push point data
      needed_ras_pts <-  sf::st_as_sf(ras_pts[ras_pts$master_id == ras_flowline_crossings[nearest_ras_index,]$master_id,], coords = c("x", "y"), crs = sf::st_crs(ras_xs)) %>%
        sf::st_transform(sf::st_crs(auto_transects))
      new_point_entry = data.table(
        id = rep(target_flowline$id,nrow(needed_ras_pts)),
        cs_id = rep(point_selection_index,nrow(needed_ras_pts)),
        pt_id = c(1:nrow(needed_ras_pts)),
        Z = needed_ras_pts$z,
        relative_distance = needed_ras_pts$xid_d,
        cs_length = rep(sf::st_length(potential_ras_lines[potential_ras_lines$master_id== ras_flowline_crossings[nearest_ras_index,]$master_id,]),nrow(needed_ras_pts)),
        X = sf::st_coordinates(needed_ras_pts)[,1],
        Y = sf::st_coordinates(needed_ras_pts)[,2],
        Z_source = rep("HEC-RAS",nrow(needed_ras_pts)),
        roughness = needed_ras_pts$n
      )

      export_transects <- rbind(export_transects,new_line_entry)
      export_cs_pts <- rbind(export_cs_pts,new_point_entry)
    }
  }

  # Write data out
  unlink(file.path(path_to_ras_dbase,"hf_3D"),recursive=TRUE)
  arrow::write_parquet(export_cs_pts,file.path(path_to_ras_dbase,"hf_3D",glue::glue("cs_pts.parquet"),fsep = .Platform$file.sep))
  sf::st_write(export_transects,file.path(path_to_ras_dbase,"hf_3D",glue::glue("ras_transects.gpkg"),fsep = .Platform$file.sep),layer="transects")

  if(is_verbose) {
    runtime <- Sys.time() - fn_time_start
    message(glue::glue("Wall time: {round(units::as_units(runtime,'hours'), digits = 3)} hours"))
  }

  return(TRUE)
}
