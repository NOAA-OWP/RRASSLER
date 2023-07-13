#' @title ingest_into_database
#' @description ingest or append a new set of HEC-RAS models into your database
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param top_of_dir_to_scrape The path to the top of the directory which you want to ingest
#' @param code_to_place_in_source a string which encodes the owner or maintainer of that model
#' @param proj_override an EPSG string to apply should a projection not be found, Default: NULL
#' @param vdat_trans a flag to dictate whether or not to apply a vdatum transformation, TRUE to apply, FALSE to skip, Default: FALSE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @param chatty flag to dictate whether print statements from within the extraction are suppressed, TRUE to show messages and FALSE to suppress them, Default: FALSE
#' @param ping_me a string with an email used to send emails after processing. Uses gmailr and requires config, Default: NULL
#' @param quick_check a flag to dictate whether or not to perform a quick check to see if a model has already been processed based on the raw name of the file. Useful in reingesting the same directory after an error but not recommended otherwise. TRUE to see if a name matches and skips, FALSE to process all the way though, Default: FALSE
#' @param quick_hull a flag to dictate how tightly you want the footprints wrapped, TRUE uses just end points of the linestrings to place the model, FALSE uses the full point database, Default: FALSE
#' @param overwrite not currently implemented, Default: FALSE
#' @param refresh flag to dictate whether or not to recollate spatial database after ingest process. FALSE to skip, TRUE to regenerate, Default: TRUE
#' @return a RRASSLED catalog of HEC-RAS models
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.table]{as.data.table}}, \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
#'  \code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
#'  \code{\link[unglue]{unglue}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[holyhull]{holyhull}}
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[arrow]{write_parquet}}
#'  \code{\link[gmailr]{gm_mime}}, \code{\link[gmailr]{gm_to}}, \code{\link[gmailr]{gm_send_message}}
#' @rdname ingest_into_database
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom stringr str_sub str_detect
#' @importFrom unglue unglue_vec
#' @importFrom sfheaders sf_linestring
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom holyhull holyhull
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet
#' @importFrom gmailr gm_mime gm_to gm_from gm_subject gm_text_body gm_send_message
#' @importFrom data.table as.data.table data.table rbindlist fwrite `:=`

ingest_into_database <- function(path_to_ras_dbase,
                                 top_of_dir_to_scrape,
                                 code_to_place_in_source,
                                 proj_override = NULL,
                                 vdat_trans = FALSE,
                                 quiet = TRUE,
                                 chatty = FALSE,
                                 ping_me = NULL,
                                 quick_check = FALSE,
                                 quick_hull = FALSE,
                                 overwrite = FALSE,
                                 refresh = TRUE) {
  # sinew::moga(file.path(getwd(),"R/ingest_into_database.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase="/home/rstudio/g/data/ras_dbase"
  # top_of_dir_to_scrape="/home/rstudio/g/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data"
  # code_to_place_in_source="ras2fim_test_data"
  # proj_override="EPSG:26915"

  # path_to_ras_dbase="G:/data/ras_catalog"
  # top_of_dir_to_scrape="G:/data/ras_catalog/_temp/BLE/12010005/"
  # code_to_place_in_source="FEMA 6 BLE"
  # proj_override="ESRI:102739"
  # vdat_trans=FALSE
  # quiet=FALSE
  # chatty = TRUE
  # quick_check=TRUE
  # ping_me = TRUE
  # overwrite = FALSE
  # refresh = TRUE

  # gmailr::gm_auth_configure(path = "C:/Users/jimma/Desktop/client_secret_765662520275-iduoi88oke14pqst3ebukn5rb2qf0895.apps.googleusercontent.com.json")

  ## -- Start --
  fn_time_start <- Sys.time()
  if(!quiet) {
    print(glue::glue("Parsing {top_of_dir_to_scrape} to place in {path_to_ras_dbase}"))
  }

  # Global constants
  names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","initial_scrape_name","final_name_key","notes")
  process_count = 0
  skip_count = 0
  duplicate_count = 0

  # Load past runs
  if(file.exists(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))) {
    ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep), quiet = quiet)
  } else {
    if (!quiet) { print('Constructing ras database structure') }
    dir.create(file.path(path_to_ras_dbase,"models",fsep = .Platform$file.sep), showWarnings = FALSE)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep), showWarnings = FALSE, recursive=TRUE)
    ras_catalog_dbase <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(ras_catalog_dbase) <- names
    ras_catalog_dbase <- data.table::as.data.table(ras_catalog_dbase)
    ras_catalog_dbase[,nhdplus_comid := as.character(nhdplus_comid)]
    ras_catalog_dbase[,final_name_key := as.character(final_name_key)]
  }

  # Find a list of all the .prj files
  list_of_prj_files <- list.files(top_of_dir_to_scrape, pattern = glob2rx("*.prj$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  n_files_to_process <- length(list_of_prj_files)
  if(!quiet) { print(glue::glue("Found {n_files_to_process} potential ras files")) }

  # For each prj file:
  for(l in 1:n_files_to_process) {
  # for(file in list_of_prj_files) {
    # Per-model constants
    # l = 1
    file <- list_of_prj_files[l]
    dir_of_file <- dirname(file)
    current_model_name <- gsub('.{4}$', '', basename(file))
    current_nhdplus_comid = NA
    current_model_units = NA
    current_model_projection = NA
    current_last_modified = as.integer(as.POSIXct(file.info(file)$mtime))

    if (!quiet) {
      print(glue::glue("File {l} of {n_files_to_process}"))
      print(glue::glue("Processing {current_model_name} model"))
      file_conn = file(file.path(path_to_ras_dbase,"ingest_record.txt"))
  lines_to_write = c(" --  --  -- -- ",
                     glue::glue("Touching:{file}"))
  writeLines(lines_to_write, file_conn)
  close(file_conn)
    }

    # Files to copy around
    g_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    ghdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??.hdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    f_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.f??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    h_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.h??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    v_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.v??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    o_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.o??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    r_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.r??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    u_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.u??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    x_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.x??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    rasmap_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.rasmap$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    prj_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.prj$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    p_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.p??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    # xml_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.xml$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    # pdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.pdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    p_files <- p_files[!p_files %in% prj_files]
    list_of_files <- c(g_files ,ghdf_files ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files)

    if(length(g_files) == 0) {
      if(!quiet) {
        print_warning_block()
        print(glue::glue('No geometry found for model:{file}'))
      }
      skip_count = skip_count + 1
      next
    }

    # populate what we can from a projection file and project file
    for(potential_file in prj_files) {
      # potential_file <- prj_files[1]
      file_text <- read.delim(potential_file, header = FALSE)

      if(any(c('PROJCS','GEOGCS','DATUM','PROJECTION') == file_text)) {
        if(!quiet) { print('found a projection') }
        current_model_projection = sf::st_crs(potential_file)
      } else if(grepl("SI Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Meter"
      } else if(grepl("English Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Foot"
      }
    }

    if(is.na(current_model_projection) & !is.null(proj_override)) {
      if(!quiet) {
        print_warning_block()
        print(glue::glue('using proj overwrite'))
      }
      current_model_projection = proj_override
    }

    # For each geometric realization of the model
    for(g_file in g_files) {
      # g_file <- g_files[1]
      current_g_value <- stringr::str_sub(g_file,-3,-1)

      if(quick_check) {
        if(stringr::str_sub(basename(g_file), end = -5) %in% ras_catalog_dbase$model_name) {
          if(!quiet) { print("Model with inital name already in the que") }
          duplicate_count = duplicate_count + 1
          next()
        }
      }

      # Do we have enough info to parse the file at this point?
      cond1 = !is.na(current_model_units)
      cond2 = !is.na(current_model_projection)
      cond3 = file.exists(paste0(g_file,".hdf"))

      # We don't know either the projection or the units and can't parse this yet
      if(!(cond1 & cond2)) {
        current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
        current_final_name_key <- NA

        if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$initial_scrape_name), current_initial_name)) == 0) {
          new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_initial_name,current_final_name_key,"unparsed_units_proj")
          names(new_row) <- names
          ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

          dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
          file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                    file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))
          data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep), row.names = FALSE)
          process_count = process_count + 1
        } else {
          if(!quiet) { print("Model with inital scrape name already in the que") }
          duplicate_count = duplicate_count + 1
        }
        next()
      }

      extrated_pts <- try({
        parse_model_to_xyz(geom_path=g_file,units=current_model_units,proj_string=current_model_projection,in_epoch_override = as.integer(as.POSIXct(Sys.time())),out_epoch_override = as.integer(as.POSIXct(Sys.time())),vdat_trans=FALSE,quiet=chatty,default_g=FALSE,try_both=TRUE)
      })

      if(isFALSE(extrated_pts[[1]]) | (class(extrated_pts) == "try-error")) {
        current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
        current_final_name_key <- NA
        if(!quiet) { print("Model unparsed") }

        if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$initial_scrape_name), current_initial_name)) == 0) {
          new_row <- data.table::data.table(current_nhdplus_comid,
                                            current_model_name,
                                            current_g_value,
                                            current_last_modified,
                                            code_to_place_in_source,
                                            current_model_units,
                                            current_model_projection,
                                            current_initial_name,
                                            current_final_name_key,
                                            "unparsed_units_proj")
          names(new_row) <- names
          ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

          dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
          file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                    file.path(path_to_ras_dbase,"models","_unprocessed",current_initial_name,fsep = .Platform$file.sep))
          if(!quiet) { print(glue::glue("Model sitting in _unprocessed {current_initial_name}")) }
          data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep), row.names = FALSE)
          process_count = process_count + 1
        } else {
          if(!quiet) { print("Model with inital scrape name already in the que") }
          duplicate_count = duplicate_count + 1
        }
        next()
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

      flowline_list <- try({
        nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly),realization = "flowline")
      })

      # Join to comids
      if((class(flowline_list) == "try-error")) {
        current_nhdplus_comid = 3
      } else if(isFALSE(flowline_list)) {
        current_nhdplus_comid = 1
      } else if(length(flowline_list) == 0) {
        current_nhdplus_comid = 2
      } else {
        current_nhdplus_comid = flowline_list[flowline_list$streamorde == max(flowline_list$streamorde),][1,]$comid
      }

      current_initial_name = paste0(current_nhdplus_comid,"_",current_model_name,"_",current_g_value,"_",current_last_modified)
      if(!quiet) { print(glue::glue("Parsed into:{current_initial_name}")) }
      current_final_name_key = current_initial_name

      if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$final_name_key), current_final_name_key)) == 0) {
        new_row <- data.table::data.table(current_nhdplus_comid,
                                          current_model_name,
                                          current_g_value,
                                          current_last_modified,
                                          code_to_place_in_source,
                                          current_model_units,
                                          current_model_projection,
                                          current_initial_name,
                                          current_final_name_key,
                                          as.character(extrated_pts[[2]]))
        names(new_row) <- names
        ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

        dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
        file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,rasmap_files),
                  file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))

        arrow::write_parquet(extrated_pts[[1]],file.path(path_to_ras_dbase,"models",current_final_name_key,"ras_xyz.parquet",fsep = .Platform$file.sep))
        sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",current_final_name_key,"hull.fgb",fsep = .Platform$file.sep),quiet=!chatty)
        data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep), row.names = FALSE)
        process_count = process_count + 1
      } else {
        if(!quiet) { print(glue::glue("Model already processed: {current_final_name_key} at row {ras_catalog_dbase[ras_catalog_dbase$final_name_key == current_final_name_key,which=TRUE]}")) }
        duplicate_count = duplicate_count + 1
      }
    }
    # print(ras_catalog_dbase)
  }

  if(!quiet) {
    print(glue::glue("Started at:{fn_time_start}"))
    print(glue::glue("Finished at:{Sys.time()}"))
    print(glue::glue("Scrape location:{top_of_dir_to_scrape}"))
    print(glue::glue("proj assingned:{proj_override}"))
    print(glue::glue("vdat applied:{vdat_trans}"))
    print(glue::glue("Items added:{process_count}"))
    print(glue::glue("Duplicates found:{duplicate_count}"))
    print(glue::glue("Files skipped:{skip_count}"))
  }

  # Accounting report-out
  file_conn = file(file.path(path_to_ras_dbase,"ingest_record.txt"))
  lines_to_write = c(" --  --  -- -- ",
                     glue::glue("Started at:{fn_time_start}"),glue::glue("Finished at:{Sys.time()}"),
                     glue::glue("Scrape location:{top_of_dir_to_scrape}"),
                     glue::glue("proj assingned:{proj_override}"),
                     glue::glue("vdat applied:{vdat_trans}"),
                     glue::glue("Items added:{process_count}"),
                     glue::glue("Duplicates found:{duplicate_count}"),
                     glue::glue("Files skipped:{skip_count}"))
  writeLines(lines_to_write, file_conn)
  close(file_conn)

  # (re) build key files
  if(refresh) {
    refresh_master_files(path_to_ras_dbase)
  }

  # Wrap up
  runtime <- Sys.time() - fn_time_start
  units(runtime) <- "hours"
  print(paste("RAS Library appended in",round(runtime, digits = 3),"hours"))

  if(!is.null(ping_me)) {
    test_email <-
      gmailr::gm_mime() %>%
      gmailr::gm_to(ping_me) %>%
      gmailr::gm_from(ping_me) %>%
      gmailr::gm_subject("RRASSLER's Done") %>%
      gmailr::gm_text_body(lines_to_write)
    gmailr::gm_send_message(test_email)
  }
  return(TRUE)
}
