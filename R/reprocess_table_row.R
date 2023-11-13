#' @title reprocess_table_row
#' @description regenerate a row in the catalog
#' @param path_to_ras_dbase The path to the top level folder of the ras_catalog structure
#' @param excel_row if you opened this up in excel (1 based index) the row you want to process
#' @return updates a row to hopefully place the model in a better space
#' @family post-process
#' @details in dev
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [glue][glue::glue]
#'  [fwrite][data.table::fwrite]
#'  [sf_linestring][sfheaders::sf_linestring]
#'  [st_set_crs][sf::st_set_crs], [st_crs][sf::st_crs], [st_coordinates][sf::st_coordinates], [st_as_sf][sf::st_as_sf], [st_transform][sf::st_transform], [st_convex_hull][sf::st_convex_hull], [st_union][sf::st_union], [st_write][sf::st_write]
#'  [st_endpoint][lwgeom::st_endpoint], [st_startpoint][lwgeom::st_startpoint]
#'  [group_by][dplyr::group_by], [distinct][dplyr::distinct]
#'  [get_nhdplus][nhdplusTools::get_nhdplus]
#'  [aoi_get][AOI::aoi_get]
#'  [write_parquet][arrow::write_parquet]
#' @rdname reprocess_table_row
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom data.table fwrite
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_set_crs st_crs st_coordinates st_as_sf st_transform st_convex_hull st_union st_write
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet

reprocess_table_row <- function(path_to_ras_dbase, excel_row) {
  # path_to_ras_dbase = "H:/ras_dbase"
  # excel_row = 3682
  row = excel_row - 1
  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase, "accounting.csv", fsep = .Platform$file.sep))

  if (ras_catalog_dbase[row, final_name_key] == "") {
    path <-
      file.path(
        path_to_ras_dbase,
        "models",
        "_unprocessed",
        ras_catalog_dbase[row, initial_scrape_name],
        glue::glue(
          "{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"
        ),
        fsep = .Platform$file.sep
      )
    current_path <-
      file.path(
        path_to_ras_dbase,
        "models",
        "_unprocessed",
        ras_catalog_dbase[row, initial_scrape_name],
        fsep = .Platform$file.sep
      )
  } else {
    path <-
      file.path(
        path_to_ras_dbase,
        "models",
        ras_catalog_dbase[row, final_name_key],
        glue::glue(
          "{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"
        ),
        fsep = .Platform$file.sep
      )
    current_path <-
      file.path(path_to_ras_dbase,
                "models",
                ras_catalog_dbase[row, final_name_key],
                fsep = .Platform$file.sep)
  }

  print(glue::glue("processing {path}"))
  extrated_pts = tryCatch(
    expr = {
      process_ras_to_xyz(
        geom_path = path,
        units = ras_catalog_dbase[row, units],
        proj_string = ras_catalog_dbase[row, crs],
        in_epoch_override = ras_catalog_dbase[row, last_modified],
        quiet = FALSE
      )
    },
    error = function(e) {
      NA
    }
  )

  if (!is.data.frame(extrated_pts)) {
    ras_catalog_dbase[row, ]$notes = "unparsed_geo"
    data.table::fwrite(
      ras_catalog_dbase,
      file.path(path_to_ras_dbase, "model_catalog.csv", fsep = .Platform$file.sep),
      row.names = FALSE
    )
    return(FALSE)
  }

  ls = sfheaders::sf_linestring(
    obj = extrated_pts
    ,
    x = "x"
    ,
    y = "y"
    ,
    z = "z"
    ,
    linestring_id = "xid"
    ,
    keep = FALSE
  ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  end_points <-
    c(ls %>% lwgeom::st_endpoint(), ls %>% lwgeom::st_startpoint())
  end_points <- sf::st_coordinates(end_points) %>%
    as.data.frame() %>%
    dplyr::group_by(X, Y) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(coords = c("X", "Y")) %>%
    sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
    sf::st_transform(sf::st_crs("EPSG:4326"))

  ahull_poly = tryCatch(
    expr = {
      st_alpha_hull(
        x = sf::st_coordinates(end_points)[, 1],
        y = sf::st_coordinates(end_points)[, 2],
        alpha = 0.1
      )
    },
    error = function(e) {
      sf::st_convex_hull(sf::st_union(end_points)) %>%
        sf::st_as_sf() %>%
        sf::st_set_crs(sf::st_crs(end_points)) %>%
        sf::st_transform(sf::st_crs("EPSG:6349"))
    }
  )

  # Join to mainstems
  ras_catalog_dbase[row, notes := "processed"]
  relevent_aoi_features = nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly), realization = "flowline")
  if (length(relevent_aoi_features) == 0) {
    ras_catalog_dbase[row, nhdplus_comid := 001]
  } else {
    ras_catalog_dbase[row, nhdplus_comid := relevent_aoi_features[relevent_aoi_features$streamorde ==
                                                                    max(relevent_aoi_features$streamorde), ][1, ]$comid]
  }

  ras_catalog_dbase[row, final_name_key := paste0(
    ras_catalog_dbase[row, nhdplus_comid],
    "_",
    ras_catalog_dbase[row, model_name],
    "_",
    ras_catalog_dbase[row, g_file],
    "_",
    ras_catalog_dbase[row, last_modified]
  )]

  dir.create(
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      fsep = .Platform$file.sep
    ),
    showWarnings = FALSE
  )
  file.copy(
    list.files(
      current_path,
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    ),
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      fsep = .Platform$file.sep
    )
  )
  unlink(current_path, recursive = TRUE)

  unlink(
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      "ras_xyz.parquet",
      fsep = .Platform$file.sep
    )
  )
  unlink(
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      "hull.fgb",
      fsep = .Platform$file.sep
    )
  )
  arrow::write_parquet(
    extrated_pts,
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      "ras_xyz.parquet",
      fsep = .Platform$file.sep
    )
  )
  sf::st_write(
    ahull_poly,
    file.path(
      path_to_ras_dbase,
      "models",
      ras_catalog_dbase[row, ]$final_name_key,
      "hull.fgb",
      fsep = .Platform$file.sep
    )
  )
  data.table::fwrite(
    ras_catalog_dbase,
    file.path(path_to_ras_dbase, "model_catalog.csv", fsep = .Platform$file.sep),
    row.names = FALSE
  )

  refresh_master_files(path_to_ras_dbase)
  return(TRUE)
}
