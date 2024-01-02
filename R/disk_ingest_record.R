#' @title disk_ingest_record
#' @description add a file as a record in the RRASSLED structure
#' @param in_file the path to the file on disk that we want to ingest, Default: NULL
#' @param ras_dbase A path to a directory to write your RRASSLED catalog to., Default: NULL
#' @param code_to_place_in_source a code to place in the metadata as the owner of the model, Default: NULL
#' @param proj_override a string to override projection information should none be found, Default: NULL
#' @param apply_vdat_trans Should VDATUM be applied to the HEC-RAS model geometry.  See https://vdatum.noaa.gov/, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param overwrite overwrite files if we find identical models, Default: FALSE
#' @family ingest
#' @return a set of files in a newly RRASSLE'd record.
#' @details the disk version of the ingest process
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  # ras_dbase <- "./inst/extdata/sample_output/ras_catalog/"
#'  # dir_to_scrape <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/"
#'  dir_to_scrape <- fs::path_package("extdata/shapes.fgb", package = "mypkg")
#'  list_of_prj_files <- list.files(dir_to_scrape,pattern = glob2rx("*.prj$"),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
#'  disk_ingest_record(in_file = list_of_prj_files[1], path_to_ras_dbase = ras_dbase,code_to_place_in_source = "test",proj_override = "EPSG:2277",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = FALSE,overwrite = FALSE)
#'
#'  dir_to_scrape <- "./inst/extdata/sample_ras/ras2fim-sample-dataset/"
#'  list_of_prj_files <- list.files(dir_to_scrape,pattern = glob2rx("*.prj$"),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
#'  disk_ingest_record(in_file = list_of_prj_files[1], path_to_ras_dbase = ras_dbase,code_to_place_in_source = "test",proj_override = "EPSG:26915",apply_vdat_trans = FALSE,is_quiet = FALSE,is_verbose = TRUE,overwrite = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_write}}
#'  \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[sfheaders]{sf_linestring}}, \code{\link[sfheaders]{sf_polygon}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[arrow]{write_parquet}}
#' @rdname disk_ingest_record
#' @export
#' @importFrom utils glob2rx
#' @importFrom glue glue
#' @importFrom sf st_crs st_set_crs st_coordinates st_write
#' @importFrom stringr str_sub str_detect
#' @importFrom data.table data.table fwrite
#' @importFrom sfheaders sf_linestring sf_polygon
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom arrow write_parquet

disk_ingest_record <- function(in_file = NULL,
                               path_to_ras_dbase = NULL,
                               code_to_place_in_source = NULL,
                               proj_override = NULL,
                               apply_vdat_trans = FALSE,
                               is_quiet = FALSE,
                               is_verbose = FALSE,
                               overwrite = FALSE) {
  # sinew::moga(file.path(getwd(),"R/disk_ingest_record.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  #
  # ras_dbase <- "./inst/extdata/sample_output/ras_catalog/"
  # dir_to_scrape <- "./inst/extdata/sample_ras/ras2fim-sample-dataset/"
  # list_of_prj_files <- list.files(dir_to_scrape,pattern = glob2rx("*.prj$"),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  # in_file = list_of_prj_files[1]
  # path_to_ras_dbase = ras_dbase
  # code_to_place_in_source = "test"
  # proj_override = "EPSG:26915"
  # apply_vdat_trans = FALSE
  # is_quiet = FALSE
  # is_verbose = TRUE
  # overwrite = FALSE
  #
  # ras_dbase <- "./inst/extdata/sample_output/ras_catalog/"
  # dir_to_scrape <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/12090301/12090301_models/"
  # list_of_prj_files <- list.files(dir_to_scrape,pattern = glob2rx("*.prj$"),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)
  # in_file = list_of_prj_files[1]
  # path_to_ras_dbase = ras_dbase
  # code_to_place_in_source = "test"
  # proj_override = "EPSG:2277"
  # apply_vdat_trans = FALSE
  # is_quiet = FALSE
  # is_verbose = TRUE
  # overwrite = FALSE

  ## -- Start --
  if (!is_quiet) {
    message(paste("Parent proj file:",in_file))
    message(paste("RRASSLING to:",path_to_ras_dbase))
    message(paste("Proj override:",proj_override))
    message(paste("vdat:",apply_vdat_trans))
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
  list_of_files <- c(g_files,ghdf_files,p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)

  if (length(g_files) == 0) {
    print_warning_block()
    message("Probably not a valid HEC-RAS model?")
    return(FALSE)
  }

  # populate what we can from a projection file and project file
  for (potential_file in prj_files) {
    # potential_file <- prj_files[1]
    file_text <- read.delim(potential_file, header = FALSE)

    if (any(c('PROJCS', 'GEOGCS', 'DATUM', 'PROJECTION') == file_text)) {
      current_model_projection = sf::st_crs(potential_file)
    } else if (grepl("SI Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "SI Units"
    } else if (grepl("English Units", file_text, fixed = TRUE)) {
      current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
      current_model_units = "English Units"
    }
  }

  if (is.na(current_model_projection) & !is.null(proj_override)) {
    if(file.exists(proj_override)) {
      current_model_projection = sf::st_crs(readLines(proj_override))
    } else {
      current_model_projection = proj_override
    }
  }

  # For each geometric realization of the model
  for (g_file in g_files) {
    # g_file <- g_files[1]
    current_g_value <- stringr::str_sub(g_file,-3,-1)

    # Do we have enough info to parse the file at this point?
    cond1 = !is.na(current_model_units)
    cond2 = !is.na(current_model_projection)
    cond3 = file.exists(paste0(g_file, ".hdf"))

    # We don't know either the projection or the units and can't parse this yet
    if (!(cond1 & cond2)) {
      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- NA
      notes <- "unparsed_units_proj"

      if (!is_quiet) { message(glue::glue("Model unparsed: {notes}")) }

      if (sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if (!is_quiet) {
          print_warning_block()
          message(glue::glue("Model was unparsed but a name like this was already in the database:{current_initial_name}"))
        }
        time_added_to_unique <- 0
        while(!(sum(stringr::str_detect(inital_scrape_names, current_initial_name)) == 0)) {
          time_added_to_unique <- time_added_to_unique + 1
          current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified + time_added_to_unique)
        }
        if (!is_quiet) {
          message(glue::glue("Added {time_added_to_unique} second(s) to time to make this unique"))
        }
        notes <- glue::glue("{notes} * Time added to filename:{time_added_to_unique}")
      }

      new_row <-
        data.table::data.table(
          current_nhdplus_comid,
          current_model_name,
          current_g_value,
          current_last_modified,
          code_to_place_in_source,
          current_model_units,
          current_model_projection,
          current_initial_name,
          current_final_name_key,
          notes
        )
      names(new_row) <- names

      dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))

      data.table::fwrite(new_row,file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,"RRASSLER_metadata.csv",fsep = .Platform$file.sep),row.names = FALSE)

      if(cond3) {
        files_to_copy <- c(g_file,paste0(g_file, ".hdf"),p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      } else {
        files_to_copy <- c(g_file,p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      }

      file.copy(
        files_to_copy,
        file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep)
      )
      next
    }

    if (!is_quiet) { message(glue::glue("Parsing:{g_file}")) }
    extrated_pts <- try({
      parse_model_to_xyz(
        geom_path = g_file,
        units = current_model_units,
        proj_string = current_model_projection,
        in_epoch_override = as.integer(as.POSIXct(Sys.time())),
        out_epoch_override = as.integer(as.POSIXct(Sys.time())),
        vdat_trans = FALSE,
        quiet = is_quiet,
        is_verbose = is_verbose
      )
    })

    # If it fails or is non-existant, parsing failed and it goes into _unprocessed
    if (isFALSE(extrated_pts[[1]]) |
        (c("try-error") %in% class(extrated_pts))
        ) {
      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- NA
      notes <- "parse_model_to_xyz failed"

      if (!is_quiet) { message(glue::glue("Model unparsed: {notes}")) }

      if (sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if (!is_quiet) {
          print_warning_block()
          message(glue::glue("Model was unparsed but a name like this was already in the database:{current_initial_name}"))
        }
        time_added_to_unique <- 0
        while(!(sum(stringr::str_detect(inital_scrape_names, current_initial_name)) == 0)) {
          time_added_to_unique <- time_added_to_unique + 1
          current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified + time_added_to_unique)
        }
        if (!is_quiet) {
          message(glue::glue("Added {time_added_to_unique} second(s) to time to make this unique"))
        }
        notes <- glue::glue("{notes} * Time added to filename:{time_added_to_unique}")
      }

      dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))
      new_row <-
        data.table::data.table(
          current_nhdplus_comid,
          current_model_name,
          current_g_value,
          current_last_modified,
          code_to_place_in_source,
          current_model_units,
          current_model_projection,
          current_initial_name,
          current_final_name_key,
          notes
        )
      names(new_row) <- names

      data.table::fwrite(new_row,file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,"RRASSLER_metadata.csv",fsep = .Platform$file.sep),row.names = FALSE)

      if(cond3) {
        files_to_copy <- c(g_file,paste0(g_file, ".hdf"),p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      } else {
        files_to_copy <- c(g_file,p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
      }
      file.copy(
        files_to_copy,
        file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep)
      )
      next
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
    ls_middle_lines_end <- ls[2:ls_end_index,] |> lwgeom::st_endpoint()
    ls_middle_lines_start <- ls[2:ls_end_index,] %>% lwgeom::st_startpoint()

    df_hull_pts <- rbind(
      sf::st_coordinates(ls[1,]$geometry)[, -c(3)],
      sf::st_coordinates(ls_middle_lines_end),
      apply(sf::st_coordinates(ls[ls_final_line_index,]$geometry)[, -c(3)], 2, rev),
      apply(sf::st_coordinates(ls_middle_lines_start), 2, rev))
    hull = sfheaders::sf_polygon(
      obj = df_hull_pts,
      x = "X",
      y = "Y",
      keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))


    # Hull creation failure check

    # Conflation
    current_nhdplus_comid <- try(crosswalk_hull_to_hydrofabric_value(hull,extrated_pts[[3]]), silent = TRUE)
    if ("try-error" %in% class(current_nhdplus_comid)) {
      current_nhdplus_comid <- 3
    }

    current_initial_name = paste0(current_nhdplus_comid,"_",current_model_name,"_",current_g_value,"_",current_last_modified)
    current_final_name_key = current_initial_name
    notes <- extrated_pts[[2]]

    if (!is_quiet) { message(glue::glue("Model processed into {current_final_name_key}")) }

    if (sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) > 0) {
      if (!is_quiet) {
        print_warning_block()
        message(glue::glue("Model with a name like this was already in the database"))
      }
      time_added_to_unique <- 0
      while(!(sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) == 0)) {
        time_added_to_unique <- time_added_to_unique + 1
        current_final_name_key <- paste0(current_nhdplus_comid,"_",current_model_name,"_",current_g_value,"_",current_last_modified + time_added_to_unique)
      }
      if (!is_quiet) {
        message(glue::glue("Added {time_added_to_unique} second(s) to time to make this unique"))
      }
      notes <- glue::glue("{extrated_pts[[2]]} * Time added to filename:{time_added_to_unique}")
    }

    new_row <-
      data.table::data.table(
        current_nhdplus_comid,
        current_model_name,
        current_g_value,
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

    arrow::write_parquet(
      extrated_pts[[1]],
      file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_cs_pts.parquet",fsep = .Platform$file.sep)
    )

    sf::st_write(
      hull,
      file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_hull.fgb",fsep = .Platform$file.sep),
      quiet = is_verbose
    )

    sf::st_write(
      extrated_pts[[3]],
      file.path(path_to_ras_dbase,"models",current_final_name_key,"RRASSLER_river.fgb",fsep = .Platform$file.sep),
      quiet = is_verbose
    )

    if(cond3) {
      files_to_copy <- c(g_file,paste0(g_file, ".hdf"),p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
    } else {
      files_to_copy <- c(g_file,p_files,f_files,h_files,v_files,prj_files,o_files,r_files,u_files,x_files,rasmap_files)
    }
    file.copy(
      files_to_copy,
      file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep)
    )

  }
  return(TRUE)
}
