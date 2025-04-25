#' @title make_xs_hyfab_comp
#' @description testing function to crosswalk cross sections to flowline distance
#' @param path_to_ras_dbase A path to a RRASSLED directory to, Default: NULL
#' @param line_select_subset A subset of the lines you want to crosswalk, Default: NULL
#' @param hf_lines the hydrofabric network you want to crosswalk to
#' @param overwrite overwrite outputs, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: TRUE
#' @returns testing
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  make_xs_hyfab_comp(path_to_ras_dbase = NULL,line_select_subset = NULL,hf_lines,overwrite = FALSE,is_verbose = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{sf}}, \code{\link[sf]{sfc}}, \code{\link[sf]{geos_binary_ops}}, \code{\link[sf]{geos_unary}}, \code{\link[sf]{st_cast}}, \code{\link[sf]{geos_measures}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_write}}
#'  \code{\link[arrow]{read_parquet}}, \code{\link[arrow]{write_parquet}}
#'  \code{\link[units]{units}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}
#' @rdname make_xs_hyfab_comp
#' @export
#' @importFrom glue glue
#' @importFrom sf st_read st_transform st_crs st_sf st_sfc st_intersection st_segmentize st_cast st_distance st_length st_as_sf st_write
#' @importFrom arrow read_parquet write_parquet
#' @importFrom units as_units
#' @importFrom dplyr mutate group_by summarise ungroup
#'
make_xs_hyfab_comp <- function(path_to_ras_dbase = NULL,
                               line_select_subset = NULL,
                               hf_lines,
                               overwrite = FALSE,
                               is_verbose = TRUE) {
  # sinew::moga(file.path(getwd(),"R/make_xs_hyfab_comp.R"),overwrite = TRUE)
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
  # hf_lines = "~/data/raw/lynker-spatial/hydrofabric/v2.2/ls_conus.gpkg"
  # is_verbose = TRUE
  # overwrite = TRUE

  ## -- Start --
  # due to NSE notes in R CMD check
  group = NULL

  ## Load catalog
  fn_time_start <- Sys.time()
  if(!file.exists(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))) {
    print_error_block()
    message("Not a RRASSLE'd archive")
    return(FALSE)
  }

  ## Prep landing
  if(file.exists(file.path(path_to_ras_dbase,"hf_3D",glue::glue("{tools::file_path_sans_ext(basename(hf_lines))}_transects.gpkg"),fsep = .Platform$file.sep))) {
    if(!overwrite) {
      print_error_block()
      message("Output already exists and overwrite is set to false")
      return(FALSE)
    }
    if(is_verbose) {
      print_warning_block()
      message("Output already exists, overwriting")
    }
    file.remove(file.path(path_to_ras_dbase,"hf_3D",glue::glue("{tools::file_path_sans_ext(basename(hf_lines))}_cross_sections.parquet"),fsep = .Platform$file.sep))
    file.remove(file.path(path_to_ras_dbase,"hf_3D",glue::glue("{tools::file_path_sans_ext(basename(hf_lines))}_transects.gpkg"),fsep = .Platform$file.sep))
  } else if(!dir.exists(file.path(path_to_ras_dbase,"hf_3D",fsep = .Platform$file.sep))) {
    dir.create(file.path(path_to_ras_dbase,"hf_3D",fsep = .Platform$file.sep))
  }

  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase, "accounting.csv", fsep = .Platform$file.sep),is_quiet = !is_verbose)
  ras_models <- sf::st_read(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))
  ras_xs <- sf::st_read(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
  # ras_xs <- ras_xs[seelct == line_select_subset,]
  ras_pts <- arrow::read_parquet(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))

  # network <- sf::st_read(hf_lines,layer ='flowline_mainstem') # For "Study_Area.gpkg"
  # network <- sf::st_read(hf_lines,layer ='flowlines_subset')
  network <- sf::st_read(hf_lines,layer ='flowpaths')

  # Subset to requested hf intersections
  network_proj <- sf::st_transform(network,sf::st_crs(ras_xs))
  target_ras_xs <- ras_xs[network_proj,]
  if(nrow(target_ras_xs) == 0) {
    print_warning_block()
    message("No RAS models intersect")
    return(FALSE)
  }

  valid_hf_flowlines <- network_proj[target_ras_xs,]
  if(!(nrow(network_proj) == nrow(valid_hf_flowlines))) {
    if(is_verbose) { message(glue::glue("Skipping {nrow(network_proj) - nrow(valid_hf_flowlines)} (of {nrow(network_proj)} input) lines")) }
  }

  export_transects <- sf::st_sf(sf::st_sfc())
  sf::st_crs(export_transects) <- sf::st_crs(valid_hf_flowlines)
  export_cs_pts <- list()

  # Index each flowline we can paramiterize
  for(index in 1:nrow(valid_hf_flowlines)) {
    # index = 1
    if(is_verbose) { message(glue::glue("Indexing {index} of {nrow(valid_hf_flowlines)}")) }

    # Find the xs that intersect it
    target_flowline <- valid_hf_flowlines[index,]
    xs_to_index <- target_ras_xs[target_flowline,]

    # related_ras_river_paths <- ras_models[(ras_models$start_master_id <= xs_to_index$master_id) & (ras_models$end_master_id >= xs_to_index$master_id),]
    # related_ras_rivers <- sf::st_read(file.path(path_to_ras_dbase,"models",related_ras_river_paths$final_name_key,"RRASSLER_river.fgb",fsep = .Platform$file.sep))
    # mapview::mapview(related_ras_rivers, color = "cyan", map.types = c("Esri.WorldShadedRelief"), legend = TRUE) +
    #   mapview::mapview(target_flowline, color = "red", legend = TRUE) +
    #   mapview::mapview(xs_to_index, color = "green", legend = TRUE)

    # Get the distance along the flowline each xs crosses
    xs_to_index$distance_up_flowline <- 0
    for(xs_index in 1:nrow(xs_to_index)) {
      # xs_index <- 1
      site <- sf::st_intersection(target_flowline,xs_to_index[xs_index,])

      points <- sf::st_segmentize(target_flowline, units::as_units(5,"m")) %>%
        sf::st_sf() %>%
        sf::st_cast('POINT') %>%
        dplyr::mutate(group = 1)

      which.point <- which.min(sf::st_distance(site, points))

      nearest <- points[which.point,]

      segment1 <- points[1:which.point,] %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(do_union = FALSE) %>%
        sf::st_cast("LINESTRING") %>%
        dplyr::ungroup()

      segment2 <- points[which.point:nrow(points),] %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(do_union = FALSE) %>%
        sf::st_cast("LINESTRING") %>%
        dplyr::ungroup()

      # mapview::mapview(segment1, color = 'red') +
      #   mapview::mapview(segment2, color = 'blue') +
      #   mapview::mapview(nearest) +
      #   mapview::mapview(site)

      xs_to_index[xs_index,]$distance_up_flowline <- sf::st_length(segment2)

      # Should also do stream centerline here?
    }

    # Order them
    xs_to_index <- xs_to_index[order(xs_to_index$distance_up_flowline, decreasing = TRUE), ]

    # Stuff into database
    ## Matching https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html
    for(xs_index in 1:nrow(xs_to_index)) {
      # xs_index <- 1
      reindexed_xs <- data.frame(hy_id = target_flowline$id,
                                 cs_source = "ras",
                                 cs_id = xs_index,
                                 cs_measure = xs_to_index$distance_up_flowline/sf::st_length(target_flowline),
                                 cs_length = sf::st_length(xs_to_index),
                                 geometry = xs_to_index$geometry) %>% sf::st_as_sf()

      ras_model_points <- ras_pts[ras_pts$master_id == xs_to_index$master_id,]
      reindexed_cs_pts <- data.frame(hy_id = rep(target_flowline$id,nrow(ras_model_points)),
                                     cs_id = rep(xs_index,nrow(ras_model_points)),
                                     pt_id = c(1:nrow(ras_model_points)),
                                     pt_measure = ras_model_points$relative_dist,
                                     relative_dist = ras_model_points$xid,
                                     X = ras_model_points$x,
                                     Y = ras_model_points$y,
                                     Z = ras_model_points$z,
                                     Z_source = "ras",
                                     roughness = ras_model_points$n)

      export_transects <- rbind(export_transects,reindexed_xs)
      export_cs_pts <- rbind(export_cs_pts,reindexed_cs_pts)
    }
  }

  # Write data out
  unlink(file.path(path_to_ras_dbase,"hf_3D"),recursive=TRUE)
  arrow::write_parquet(export_cs_pts,file.path(path_to_ras_dbase,"hf_3D",glue::glue("{tools::file_path_sans_ext(basename(hf_lines))}_cross_sections.parquet"),fsep = .Platform$file.sep))
  sf::st_write(export_transects,file.path(path_to_ras_dbase,"hf_3D",glue::glue("{tools::file_path_sans_ext(basename(hf_lines))}_transects.gpkg"),fsep = .Platform$file.sep),layer="transects")

  if(is_verbose) {
    runtime <- Sys.time() - fn_time_start
    message(glue::glue("Wall time: {round(units::as_units(runtime,'hours'), digits = 3)} hours"))
  }

  return(TRUE)
}
