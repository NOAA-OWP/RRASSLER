#' @title cloud_ingest_record
#' @description add a file as a record in the RRASSLED structure
#' @param in_file the path to the file on disk that we want to ingest, Default: NULL
#' @param ras_dbase A path to an S3 bucket to write your RRASSLED catalog to., Default: NULL
#' @param root_bucket first path in s3 url, will auto-populate as needed, Default: NULL
#' @param code_to_place_in_source a code to place in the metadata as the owner of the model, Default: NULL
#' @param proj_override a string to override projection information should none be found, Default: NULL
#' @param apply_vdat_trans Should VDATUM be applied to the HEC-RAS model geometry.  See https://vdatum.noaa.gov/, Default: FALSE
#' @param is_quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param is_verbose flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE
#' @param quick_check on initial ingest, if the model name is found we assume the models are the same without fully spatializing them which saves processing time, Default: FALSE
#' @param quick_hull a flag to dictate whether the end points of a models cross sections are used or if the entire point database is fed to the hull creation, Default: FALSE
#' @family ingest
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
#'  \code{\link[aws.s3]{get_bucket}}, \code{\link[aws.s3]{delete_object}}, \code{\link[aws.s3]{put_object}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[holyhull]{holyhull}}
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[arrow]{write_parquet}}
#' @rdname cloud_ingest_record
#' @export
#' @importFrom stringr str_sub str_detect
#' @importFrom aws.s3 get_bucket_df delete_object put_object
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom dplyr select pull group_by distinct
#' @importFrom data.table data.table fwrite
#' @importFrom sfheaders sf_linestring
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom holyhull holyhull
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet
cloud_ingest_record <- function(in_file = NULL,
                                ras_dbase = NULL,
                                root_bucket = NULL,
                                code_to_place_in_source = NULL,
                                proj_override = NULL,
                                apply_vdat_trans = FALSE,
                                is_quiet = FALSE,
                                is_verbose = FALSE,
                                quick_check = FALSE,
                                quick_hull = FALSE,
                                overwrite = FALSE) {
  # sinew::moga(file.path(getwd(),"R/cloud_ingest_record.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # x = list_of_prj_files[1]
  # root_bucket = "s3://ras2fim/"
  # ras_dbase = "s3://ras2fim/temp/jim/ras_models/"
  # code_to_place_in_source = "test"
  # proj_override = "EPSG:2277"
  # vdat_trans = FALSE
  # quiet = FALSE
  # verbose = FALSE
  # quick_check = FALSE
  # quick_hull = FALSE
  # overwrite = FALSE

  rest_of_bucket_prefix <-
    stringr::str_sub(ras_dbase, nchar(root_bucket), nchar(ras_dbase))
  rest_of_bucket_prefix <- gsub('^.', '', rest_of_bucket_prefix)

  names <-
    c(
      "nhdplus_comid",
      "model_name",
      "g_file",
      "last_modified",
      "source",
      "units",
      "crs",
      "initial_scrape_name",
      "final_name_key",
      "notes"
    )

  file <- in_file
  dir_of_file <- dirname(file)
  current_model_name <- gsub('.{4}$', '', basename(file))
  current_nhdplus_comid = NA
  current_model_units = NA
  current_model_projection = NA
  current_last_modified = as.integer(as.POSIXct(file.info(file)$mtime))

  # What is in the database at this very moment?
  full_bucket <-
    aws.s3::get_bucket_df(
      bucket = ras_dbase,
      prefix = glue::glue("{basename(ras_dbase)}/models/"),
      max = Inf
    )
  all_scrape_names <-
    basename(dirname(full_bucket$Key)) %>% unique()
  inital_scrape_names <-
    all_scrape_names[grepl("^unknown_", all_scrape_names)]
  processed_scrape_names <-
    all_scrape_names[!grepl("^unknown_", all_scrape_names)]

  # Files to copy around
  g_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.g??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  ghdf_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.g??.hdf$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  f_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.f??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  h_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.h??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  v_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.v??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  o_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.o??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  r_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.r??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  u_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.u??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  x_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.x??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  rasmap_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.rasmap$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  prj_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.prj$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  p_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.p??$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  xml_files <-
    list.files(
      dir_of_file,
      pattern = utils::glob2rx(glue::glue("{current_model_name}.xml$")),
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE
    )
  # pdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.pdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  p_files <- p_files[!p_files %in% prj_files]
  list_of_files <-
    c(
      g_files ,
      ghdf_files ,
      p_files ,
      f_files ,
      h_files,
      v_files,
      prj_files,
      o_files,
      r_files,
      u_files,
      x_files,
      rasmap_files
    )

  # No geometry found for model:{file}
  if (length(g_files) == 0) {
    return(TRUE)
  }

  # populate what we can from a projection file and project file
  for (potential_file in prj_files) {
    file_text <- read.delim(potential_file, header = FALSE)

    if (any(c('PROJCS', 'GEOGCS', 'DATUM', 'PROJECTION') == file_text)) {
      if (!is_quiet) {
        message('found a projection')
      }
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
    current_model_projection = proj_override
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
      current_initial_name <-
        paste0(
          "unknown_",
          current_model_name,
          "_",
          current_g_value,
          "_",
          current_last_modified
        )
      current_final_name_key <- NA

      if (sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if (overwrite) {
          # Model already processed into _unprocessed {current_initial_name}, overwriting
          bucket <-
            aws.s3::get_bucket_df(
              bucket = ras_dbase,
              prefix = glue::glue("models/_unprocessed/{current_initial_name}/")
            )

          # List of files to delete
          files_to_delete <-
            bucket %>% dplyr::select(Key) %>% dplyr::pull()
          for (file in files_to_delete) {
            aws.s3::delete_object(
              object = file,
              bucket = ras_dbase,
              prefix = glue::glue("models/_unprocessed/{current_initial_name}/")
            )
          }

        } else {
          # Model already processed into _unprocessed {current_initial_name}
          next
        }
      }

      files_to_copy <-
        c(
          g_file ,
          paste0(g_file, ".hdf") ,
          p_files ,
          f_files ,
          h_files,
          v_files,
          prj_files,
          o_files,
          r_files,
          u_files,
          x_files,
          rasmap_files
        )
      for (file in files_to_copy) {
        aws.s3::put_object(
          file = file,
          object = glue::glue(
            "{rest_of_bucket_prefix}models/_unprocessed/{current_initial_name}/{basename(file)}"
          ),
          bucket = root_bucket
        )
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
          "unparsed_units_proj"
        )
      names(new_row) <- names
      temp_file <- tempfile(fileext = ".csv")
      data.table::fwrite(new_row, temp_file, row.names = FALSE)
      aws.s3::put_object(
        file = temp_file,
        object = glue::glue(
          "{rest_of_bucket_prefix}models/_unprocessed/{current_initial_name}/RRASSLER_metadata.csv"
        ),
        bucket = root_bucket
      )
      unlink(temp_file)

      next
    }

    # Parsing:{g_file}
    extrated_pts <- try({
      parse_model_to_xyz(
        geom_path = g_file,
        units = current_model_units,
        proj_string = current_model_projection,
        in_epoch_override = as.integer(as.POSIXct(Sys.time())),
        out_epoch_override = as.integer(as.POSIXct(Sys.time())),
        vdat_trans = apply_vdat_trans,
        quiet = is_verbose,
        default_g = FALSE,
        try_both = TRUE
      )
    })

    if (isFALSE(extrated_pts[[1]]) |
        (c("try-error") %in% class(extrated_pts))) {
      current_initial_name <-
        paste0(
          "unknown_",
          current_model_name,
          "_",
          current_g_value,
          "_",
          current_last_modified
        )
      current_final_name_key <- NA
      # Model unparsed

      current_initial_name <-
        paste0(
          "unknown_",
          current_model_name,
          "_",
          current_g_value,
          "_",
          current_last_modified
        )
      current_final_name_key <- NA

      if (sum(stringr::str_detect(inital_scrape_names, current_initial_name)) > 0) {
        if (overwrite) {
          # Model already processed into _unprocessed {current_initial_name}, overwriting"
          bucket <-
            aws.s3::get_bucket_df(
              bucket = ras_dbase,
              prefix = glue::glue("models/_unprocessed/{current_initial_name}/")
            )

          # List of files to delete
          files_to_delete <-
            bucket %>% dplyr::select(Key) %>% dplyr::pull()
          for (file in files_to_delete) {
            aws.s3::delete_object(
              object = file,
              bucket = ras_dbase,
              prefix = glue::glue("models/_unprocessed/{current_initial_name}/")
            )
          }
        } else {
          # Model already processed into _unprocessed {current_initial_name}
          next
        }
      }

      files_to_copy <-
        c(
          g_file ,
          paste0(g_file, ".hdf") ,
          p_files ,
          f_files ,
          h_files,
          v_files,
          prj_files,
          o_files,
          r_files,
          u_files,
          x_files,
          rasmap_files
        )
      for (file in files_to_copy) {
        aws.s3::put_object(
          file = file,
          object = glue::glue(
            "{rest_of_bucket_prefix}models/_unprocessed/{current_initial_name}/{basename(file)}"
          ),
          bucket = root_bucket
        )
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
          "unparsed_units_proj"
        )
      names(new_row) <- names
      temp_file <- tempfile(fileext = ".csv")
      data.table::fwrite(new_row, temp_file, row.names = FALSE)
      aws.s3::put_object(
        file = temp_file,
        object = glue::glue(
          "{rest_of_bucket_prefix}models/_unprocessed/{current_initial_name}/RRASSLER_metadata.csv"
        ),
        bucket = root_bucket
      )
      unlink(temp_file)

      next
    }

    # Footprint the points we extracted
    ls = sfheaders::sf_linestring(
      obj = extrated_pts[[1]]
      ,
      x = "x"
      ,
      y = "y"
      # , z = "z"
      ,
      linestring_id = "xid"
      ,
      keep = FALSE
    ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

    # Use full data frame or just end points
    if (quick_hull) {
      end_points <-
        c(ls %>% lwgeom::st_endpoint(),
          ls %>% lwgeom::st_startpoint())
      end_points <- sf::st_coordinates(end_points) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X, Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X", "Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    } else {
      end_points <- sf::st_coordinates(ls) %>%
        as.data.frame(extrated_pts[[1]]) %>%
        dplyr::group_by(X, Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X", "Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))
    }

    ahull_poly = holyhull::holyhull(
      sf_frame = end_points,
      method = 'convave',
      alpha_value = 0.01,
      concavity = 2,
      length_threshold = 0
    )

    flowline_list <- try({
      nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly), realization = "flowline")
    })

    # Join to comids
    if (c("try-error") %in% class(flowline_list)) {
      current_nhdplus_comid = 3
    } else if (isFALSE(flowline_list)) {
      current_nhdplus_comid = 1
    } else if (length(flowline_list) == 0) {
      current_nhdplus_comid = 2
    } else {
      current_nhdplus_comid = flowline_list[flowline_list$streamorde == max(flowline_list$streamorde),][1,]$comid
    }

    current_initial_name = paste0(
      current_nhdplus_comid,
      "_",
      current_model_name,
      "_",
      current_g_value,
      "_",
      current_last_modified
    )
    # Parsed into:{current_initial_name}
    current_final_name_key = current_initial_name

    if (sum(stringr::str_detect(processed_scrape_names, current_final_name_key)) > 0) {
      if (overwrite) {
        # Model already processed into {current_final_name_key}, overwriting
        bucket <-
          aws.s3::get_bucket_df(
            bucket = ras_dbase,
            prefix = glue::glue("models/{current_final_name_key}/")
          )

        # List of files to delete
        files_to_delete <-
          bucket %>% dplyr::select(Key) %>% dplyr::pull()
        for (file in files_to_delete) {
          aws.s3::delete_object(
            object = file,
            bucket = ras_dbase,
            prefix = glue::glue("models/{current_final_name_key}/")
          )
        }
      } else {
        # Model already processed into {current_final_name_key}
        next
      }
    }

    files_to_copy <-
      c(
        g_file ,
        paste0(g_file, ".hdf") ,
        p_files ,
        f_files ,
        h_files,
        v_files,
        prj_files,
        o_files,
        r_files,
        u_files,
        x_files,
        rasmap_files
      )
    for (file in files_to_copy) {
      aws.s3::put_object(
        file = file,
        object = glue::glue(
          "{rest_of_bucket_prefix}models/{current_final_name_key}/{basename(file)}"
        ),
        bucket = root_bucket
      )
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
        "unparsed_units_proj"
      )
    names(new_row) <- names
    temp_file <- tempfile(fileext = ".csv")
    data.table::fwrite(new_row, temp_file, row.names = FALSE)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue(
        "{rest_of_bucket_prefix}models/{current_final_name_key}/RRASSLER_metadata.csv"
      ),
      bucket = root_bucket
    )
    unlink(temp_file)

    temp_file <- tempfile(fileext = ".parquet")
    arrow::write_parquet(extrated_pts[[1]], temp_file)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue(
        "{rest_of_bucket_prefix}models/{current_final_name_key}/RRASSLER_cs_pts.parquet"
      ),
      bucket = root_bucket
    )
    unlink(temp_file)

    temp_file <- tempfile(fileext = ".fgb")
    sf::st_write(ahull_poly, temp_file, quiet = TRUE)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue(
        "{rest_of_bucket_prefix}models/{current_final_name_key}/RRASSLER_hull.fgb"
      ),
      bucket = root_bucket
    )
    unlink(temp_file)
  }
  return(TRUE)
}
