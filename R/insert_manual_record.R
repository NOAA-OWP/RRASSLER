#' @title insert_manual_record
#' @description forces a manually exported version of a model into the RRASSLED structure
#' @param in_file the path to the file on disk that we want to ingest, Default: NULL
#' @param path_to_ras_dbase A path to a directory to write your RRASSLED catalog to, Default: NULL
#' @param xs_shapefile the path to the exported cross section shapefile
#' @param river_shapefile the path to the exported river shapefile
#' @param geometric_realization_override the geometric realization you are parsing out as a sting with leading g character ie g01, g02..., Default: 'g01'
#' @param code_to_place_in_source a code to place in the metadata as the owner of the model, Default: NULL
#' @param proj_override a string to override projection information should none be found, Default: NULL
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param overwrite overwrite overwrite files if we find identical models, Default: FALSE
#' @family ingest
#' @returns a set of files in a newly RRASSLE'd record.
#' @details To export open the 'RAS Mapper' (GIS Tools > RAS Mapper) and right click on the 'Rivers' and 'Cross sections' layers and 'Export Layer > Save Layer to Shapefile' to a shapefile (I append mine with the model name). See https://github.com/NOAA-OWP/RRASSLER/blob/dev/man/figures/_export_for_insert_manual.png for a verbose example.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  ras_dbase <- "./inst/extdata/sample_output/ras_catalog/"
#'  host_path = file.path(".../ras_catalog/_temp/supercustommodel/", fsep = .Platform$file.sep)
#'
#'  insert_manual_record(in_file = file.path(host_path, "Ohio2018a.prj",fsep = .Platform$file.sep),path_to_ras_dbase = ras_dbase,xs_shapefile = file.path(host_path, "Ohio2018a.RASexport_xs_z.shp", fsep = .Platform$file.sep),river_shapefile = file.path(host_path, "Ohio2018a.RASexport_stream.shp", fsep = .Platform$file.sep),geometric_realization_override = "g02",code_to_place_in_source = "RFC",proj_override = file.path(host_path, "Ohio2018a.RASexport_stream.shp", fsep = .Platform$file.sep),is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{glob2rx}}, \code{\link[utils]{read.table}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_cast}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{geos_measures}}, \code{\link[sf]{st_geometry}}, \code{\link[sf]{st_write}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{c("rowwise", "rowwise")}}, \code{\link[dplyr]{group_by}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[sfheaders]{sf_polygon}}, \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[units]{units}}
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[arrow]{write_parquet}}
#' @rdname insert_manual_record
#' @export
#' @importFrom utils glob2rx read.delim
#' @importFrom glue glue
#' @importFrom sf st_read st_set_crs st_crs st_transform st_cast st_coordinates st_length st_drop_geometry st_write
#' @importFrom dplyr mutate row_number rowwise ungroup
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom sfheaders sf_polygon sf_linestring
#' @importFrom units set_units
#' @importFrom stringr str_detect
#' @importFrom data.table data.table fwrite
#' @importFrom arrow write_parquet

insert_manual_record <- function(in_file = NULL,
                               path_to_ras_dbase = NULL,
                               xs_shapefile,
                               river_shapefile,
                               geometric_realization_override='g01',
                               code_to_place_in_source = NULL,
                               proj_override = NULL,
                               is_quiet = FALSE,
                               is_verbose = FALSE,
                               overwrite = FALSE) {
  # sinew::moga(file.path(getwd(),"R/insert_manual_record.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  # library(magarittr)

  ## -- Start --

  if (!is_quiet) {
    message(paste("Parent proj file:",in_file))
    message(paste("RRASSLING to:",path_to_ras_dbase))
    message(paste("Proj override:",proj_override))
  }

  names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","initial_scrape_name","final_name_key","notes")

  # What is in the database at this very moment?
  all_scrape_names <- basename(list.dirs(path = file.path(path_to_ras_dbase, "models", fsep = .Platform$file.sep),full.names = TRUE,recursive = TRUE))
  inital_scrape_names <- all_scrape_names[grepl("^unknown_", all_scrape_names)]
  processed_scrape_names <- all_scrape_names[!grepl("^unknown_", all_scrape_names)]

  # Per-model constants
  dir_of_file <- dirname(in_file)
  current_model_name <- gsub('.{4}$', '', basename(in_file))
  current_nhdplus_comid = NA
  current_model_units = NA
  current_model_projection = NA
  current_last_modified = as.integer(as.POSIXct(file.info(in_file)$mtime))

  # Files to copy around
  g_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.g??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  ghdf_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.g??.hdf$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  f_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.f??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  h_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.h??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  v_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.v??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  o_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.o??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  r_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.r??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  u_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.u??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  x_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.x??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  rasmap_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.rasmap$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  prj_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.prj$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  p_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.p??$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  xml_files <- list.files(dir_of_file,pattern = utils::glob2rx(glue::glue("{current_model_name}.xml$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  # pdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.pdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  p_files <- p_files[!p_files %in% prj_files]
  list_of_files <- c(g_files,ghdf_files,p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,xml_files,rasmap_files)

  if (length(g_files) == 0) {
    print_warning_block()
    message("Probably not a valid HEC-RAS model?")
    return(FALSE)
  }

  # populate what we can from a projection file and project file
  for (potential_file in prj_files) {
    # potential_file <- prj_files[1]
    file_text <- utils::read.delim(potential_file, header = FALSE)

    if (any(c('PROJCS', 'GEOGCS', 'DATUM', 'PROJECTION') == file_text)) {
      current_model_projection = potential_file
    } else if (grepl("SI Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "SI Units"
    } else if (grepl("English Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "English Units"
    }
  }

  if (is.na(current_model_projection) & !is.null(proj_override)) {
    current_model_projection = proj_override
    if(file.exists(proj_override)) {
      list_of_files <- append(list_of_files,proj_override)
    }
  }

  if (!is_quiet) { message(glue::glue("Parsing (what would be):{file.path(dirname(in_file),paste0(current_model_name,'.',geometric_realization_override), fsep = .Platform$file.sep)}")) }
  # extrated_pts <- try({
  #   parse_model_to_xyz(
  #     geom_path = g_file,
  #     units = current_model_units,
  #     proj_string = current_model_projection,
  #     in_epoch_override = as.integer(as.POSIXct(Sys.time())),
  #     out_epoch_override = as.integer(as.POSIXct(Sys.time())),
  #     vdat_trans = FALSE,
  #     quiet = is_quiet,
  #     is_verbose = is_verbose
  #   )
  # })

  river <- sf::st_read(river_shapefile)
  XS <- sf::st_read(xs_shapefile)
  if (!is.null(proj_override)) {
    current_model_projection = proj_override
    XS <- sf::st_set_crs(XS, sf::st_crs(current_model_projection))
    river <- sf::st_set_crs(river, sf::st_crs(current_model_projection))
  }
  XS <- sf::st_transform(XS,sf::st_crs("EPSG:6349"))
  XS <- dplyr::mutate(XS, xid = dplyr::row_number())
  river <- sf::st_transform(river,sf::st_crs("EPSG:6349"))
  pts <- sf::st_cast(XS,'POINT')

  ls = XS
  ls_final_line_index <- nrow(ls)
  ls_end_index <- nrow(ls)-1
  ls_middle_lines_end <- ls[2:ls_end_index,] |> lwgeom::st_endpoint()
  ls_middle_lines_start <- ls[2:ls_end_index,] %>% lwgeom::st_startpoint()
  df_hull_pts <- rbind(
    sf::st_coordinates(ls[1,]$geometry)[, -c(3:4)],
    sf::st_coordinates(ls_middle_lines_end),
    apply(sf::st_coordinates(ls[ls_final_line_index,]$geometry)[, -c(3:4)], 2, rev),
    apply(sf::st_coordinates(ls_middle_lines_start), 2, rev))
  hull = sfheaders::sf_polygon(
    obj = df_hull_pts,
    x = "X",
    y = "Y",
    keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  point_database <- c()
  for (t in 1:nrow(XS)) {
    # t = 1
    if (!is_quiet) { message(paste("processing cross section number:",t,"of",nrow(XS))) }

    point_slice <- pts[pts$FID == (t - 1), ] %>%
      dplyr::mutate(idx = dplyr::row_number())
    geom_xs_linestring <- XS[t, ]$geometry
    pt_xid_length <- sf::st_length(XS[t, ])

    # Thanks Gemini
    tests <- point_slice %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        xid = t,
        xid_length = pt_xid_length,
        xid_d = {
          coords <- sf::st_coordinates(XS[t, ])[1:.data$idx, , drop = FALSE]
          if (nrow(coords) > 1) {
            sfheaders::sf_linestring(obj = coords,
                                     x = "X",
                                     y = "Y",
                                     keep = FALSE) |>
              sf::st_set_crs(sf::st_crs("EPSG:6349")) |>
              sf::st_length()
          } else {
            units::set_units(0, "m")
          }
        }
      ) %>%
      dplyr::ungroup()
    tests$relative_dist <- tests$xid_d/tests$xid_length
    tests$x = sf::st_coordinates(tests)[,1]
    tests$y = sf::st_coordinates(tests)[,2]
    tests$z = sf::st_coordinates(tests)[,3]
    tests$n = NULL
    tests$source = 7

    point_database <- rbind(point_database, sf::st_drop_geometry(tests[ , which(names(tests) %in% c("xid","xid_length","xid_d","relative_dist","x","y","z","n","source"))]))
  }
  # point_database

  # Conflation
  current_nhdplus_comid <- try(crosswalk_hull_to_hydrofabric_value(hull,river), silent = TRUE)
  if ("try-error" %in% class(current_nhdplus_comid)) {
    current_nhdplus_comid <- 3
  }
  current_final_name_key = paste0(current_nhdplus_comid,"_",current_model_name,"_",geometric_realization_override,"_",current_last_modified)
  current_initial_name = current_final_name_key
  notes <- glue::glue("profiles normalized by:{sum(sf::st_length(XS)) - sum(XS$CutlinetoX * sf::st_length(XS))} * Manually ingested")
  if (!is_quiet) { message(glue::glue("Model processed into {current_final_name_key}")) }
  if (sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) > 0) {
    if (!is_quiet) {
      print_warning_block()
      message(glue::glue("Model with a name like this was already in the database"))
    }
    time_added_to_unique <- 0
    while(!(sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) == 0)) {
      time_added_to_unique <- time_added_to_unique + 1
      current_final_name_key <- paste0(current_nhdplus_comid,"_",current_model_name,"_",geometric_realization_override,"_",current_last_modified + time_added_to_unique)
    }
    if (!is_quiet) {
      message(glue::glue("Added {time_added_to_unique} second(s) to time to make this unique"))
    }
    notes <- glue::glue("profiles normalized by:{sum(sf::st_length(XS)) - sum(XS$CutlinetoX * sf::st_length(XS))} * Manually ingested * Time added to filename:{time_added_to_unique}")
  }

  new_row <-
    data.table::data.table(
      current_nhdplus_comid,
      current_model_name,
      geometric_realization_override,
      current_last_modified,
      code_to_place_in_source,
      current_model_units,
      current_model_projection,
      current_initial_name,
      current_final_name_key,
      notes
    )
  names(new_row) <- names

  dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))
  data.table::fwrite(
    new_row,
    file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_metadata.csv",fsep = .Platform$file.sep),
    row.names = FALSE
  )

  # I should Always get points
  arrow::write_parquet(
    point_database,
    file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_cs_pts.parquet",fsep = .Platform$file.sep)
  )

  # I got a hull of some sort?
  sf::st_write(
    hull,
    file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_hull.fgb",fsep = .Platform$file.sep),
    quiet = is_verbose
  )

  # I may or may not have gotten good rivers?
  try({
    sf::st_write(
      river,
      file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_river.fgb",fsep = .Platform$file.sep),
      quiet = is_verbose
    )
  })

  file.copy(
    list_of_files,
    file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep)
  )

  return(TRUE)
}
