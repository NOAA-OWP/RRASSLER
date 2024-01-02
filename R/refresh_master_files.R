#' @title refresh_master_files
#' @description remerge individual files into spatial model key
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, is also location agnostic (disk or cloud), Default: NULL
#' @param is_verbose flag to determine whether print statements are shown - TRUE to show messages - FALSE to skip non-critical ones, Default: TRUE
#' @return updated master index files including accounting.csv, point_database.parque, XS.fgb, and model_footprints.fgb
#' @details DETAILS
#' @family post-process
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # ras_dbase <- file.path("~/data/ras_catalog/")
#'  ras_dbase <- file.path("./inst/extdata/sample_output/ras_catalog/")
#'
#'  RRASSLER::refresh_master_files(path_to_ras_dbase = ras_dbase,is_verbose = TRUE)
#'
#'  RRASSLER::refresh_master_files(path_to_ras_dbase = "s3://ras-models/",is_verbose = TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[aws.s3]{get_bucket}}, \code{\link[aws.s3]{get_object}}, \code{\link[aws.s3]{delete_object}}, \code{\link[aws.s3]{put_object}}
#'  \code{\link[data.table]{as.data.table}}, \code{\link[data.table]{fwrite}}, \code{\link[data.table]{rbindlist}}
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{st_write}}
#'  \code{\link[arrow]{read_parquet}}, \code{\link[arrow]{write_parquet}}
#'  \code{\link[sfheaders]{sf_linestring}}
#' @rdname refresh_master_files
#' @export
#' @importFrom stringr str_sub
#' @importFrom aws.s3 get_bucket save_object delete_object put_object
#' @importFrom data.table as.data.table fwrite rbindlist
#' @importFrom readr read_csv
#' @importFrom utils glob2rx
#' @importFrom glue glue
#' @importFrom sf st_read st_set_crs st_crs st_write
#' @importFrom arrow read_parquet write_parquet
#' @importFrom sfheaders sf_linestring

refresh_master_files <- function(path_to_ras_dbase,is_verbose = TRUE) {
  # sinew::moga(file.path(getwd(),"R/refresh_master_files.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  #
  #
  # path_to_ras_dbase <- file.path("./inst/extdata/sample_output/ras_catalog/")
  # is_verbose = TRUE
  # path_to_ras_dbase <- file.path("s3://ras-models/")
  # is_verbose = TRUE
  # path_to_ras_dbase <- ras_dbase

  ## -- Start --
  fn_time_start <- Sys.time()
  if(is_verbose) { message("(re)merging database outputs") }

  # Cloud or disk?
  cloud <- FALSE
  if (stringr::str_sub(path_to_ras_dbase, 1, 2) %in% c('s3', 'ht')) {
    ls_srt_folders_file <- strsplit(path_to_ras_dbase, "/")[[1]]
    ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
    path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
    cloud <- TRUE
  }

  if(cloud) {
    rest_of_bucket_prefix <- stringr::str_sub(path_to_ras_dbase, nchar(path_to_root_bucket)+1, nchar(path_to_ras_dbase)-1)
    root_temp <- tempdir()
    # unlink(root_temp,recursive = TRUE)

    # What is in the database at this very moment?
    if(is_verbose) { message(glue::glue("Gathering bucket contents")) }
    df_bucket_data <- aws.s3::get_bucket(bucket = path_to_root_bucket, prefix = "models/",max = Inf)
    list_bucket_data <- c()
    for(i in 1:length(df_bucket_data)) {
      list_bucket_data <- c(
        list_bucket_data,
        df_bucket_data[[i]]$Key)
    }
    list_bucket_data_dt <- data.table::as.data.table(list_bucket_data)
    xyz_files <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('RRASSLER_cs_pts.parquet'),]
    hull_files <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('RRASSLER_hull.fgb'),]
    meta_files <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('RRASSLER_metadata.csv'),]

    if(!(length(xyz_files)==length(hull_files))) {
      print_error_block()
      message("Alert, hulls and points dont line up...")
      stop()
    }

    if(is_verbose) { message(glue::glue("Moving requisite files")) }
    for(index in 1:nrow(hull_files)) {
      file_to_move <- hull_files[index][[1]]
      aws.s3::save_object(
        object = file_to_move,
        bucket = path_to_root_bucket,
        file = file.path(root_temp,file_to_move,fsep = .Platform$file.sep)
      )
    }
    for(index in 1:nrow(xyz_files)) {
      file_to_move <- xyz_files[index][[1]]
      aws.s3::save_object(
        object = file_to_move,
        bucket = path_to_root_bucket,
        file = file.path(root_temp,file_to_move,fsep = .Platform$file.sep)
      )
    }
    for(index in 1:nrow(meta_files)) {
      file_to_move <- meta_files[index][[1]]
      aws.s3::save_object(
        object = file_to_move,
        bucket = path_to_root_bucket,
        file = file.path(root_temp,file_to_move,fsep = .Platform$file.sep)
      )
    }

    if(is_verbose) { message("Merging catalog") }
    disk_rrassler_records <- list.files(root_temp, pattern = utils::glob2rx("*RRASSLER_metadata.csv$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    full_accounting <- rbindlist(lapply(disk_rrassler_records, function(x) data.table::fread(x, colClasses = c("nhdplus_comid" = "character","model_name" = "character","units" = "character","crs" = "character","final_name_key" = "character"))))

    if(is_verbose) { message("Writing catalog") }
    if(!(nrow(list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('accounting.csv'),]) == 0)) {
      file_to_remove <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('accounting.csv'),]$list_bucket_data
      aws.s3::delete_object(
        object = file_to_remove,
        bucket = path_to_root_bucket,
        region = "us-east-2",
      )
    }
    temp_file <- tempfile(fileext = ".csv")
    data.table::fwrite(full_accounting, temp_file, row.names = FALSE)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue("accounting.csv"),
      bucket = path_to_root_bucket
    )
    unlink(temp_file)

    disk_xyz_files <- list.files(root_temp, pattern = utils::glob2rx("*RRASSLER_cs_pts.parquet$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()
    disk_hull_files <- list.files(root_temp, pattern = utils::glob2rx("*RRASSLER_hull.fgb$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE) %>% sort()

    point_concat <- c()
    xs_concat <- c()
    hull_concat <- c()
    master_id <- 0
    rows_in_table <- length(disk_hull_files)
    for(index in 1:rows_in_table) {
      if(is_verbose) { message(glue::glue("Processing {index} of {rows_in_table}")) }
      # index = 1

      final_folder_name <- basename(dirname(disk_hull_files[index]))

      row <- full_accounting[full_accounting$final_name_key==final_folder_name,]

      hull <- sf::st_read(file.path(root_temp,"models",final_folder_name,"RRASSLER_hull.fgb",fsep = .Platform$file.sep),quiet = TRUE)
      point_data <- arrow::read_parquet(file.path(root_temp,"models",final_folder_name,"RRASSLER_cs_pts.parquet",fsep = .Platform$file.sep),as_data_frame = TRUE) %>%
        data.table::as.data.table()

      hull$start_master_id <- master_id + 1
      point_data[, master_id := xid + master_id]
      point_concat <- data.table::rbindlist(list(point_concat, point_data))
      master_id <- max(point_data[,master_id])

      hull$Name <- row$model_name
      hull$crs <- row$crs
      hull$units <- row$units
      hull$path <- row$final_name_key
      hull$source <- row$source
      hull$end_master_id <- master_id
      hull_concat <- rbind(hull_concat,hull)
    }

    if(is_verbose) { message(glue::glue("Making cross sections")) }
    xs_lines <- sfheaders::sf_linestring(
      obj = point_concat,
      x = "x",
      y = "y",
      # z = "z",
      linestring_id = "master_id",
      keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

    if(is_verbose) { message(glue::glue("Writing data files")) }
    if(!(nrow(list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('point_database.parquet'),]) == 0)) {
      file_to_remove <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('point_database.parquet'),]$list_bucket_data
      aws.s3::delete_object(
        object = file_to_remove,
        bucket = path_to_root_bucket,
      )
    }
    temp_file <- tempfile(fileext = ".parquet")
    arrow::write_parquet(point_concat,temp_file)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue("point_database.parquet"),
      bucket = path_to_root_bucket
    )
    unlink(temp_file)

    if(!(nrow(list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('XS.fgb'),]) == 0)) {
      file_to_remove <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('XS.fgb'),]$list_bucket_data
      aws.s3::delete_object(
        object = file_to_remove,
        bucket = path_to_root_bucket,
      )
    }
    temp_file <- tempfile(fileext = ".fgb")
    sf::st_write(xs_lines,temp_file,append=FALSE)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue("XS.fgb"),
      bucket = path_to_root_bucket
    )
    unlink(temp_file)

    if(!(nrow(list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('model_footprints.fgb'),]) == 0)) {
      file_to_remove <- list_bucket_data_dt[list_bucket_data_dt$list_bucket_data %like% c('model_footprints.fgb'),]$list_bucket_data
      aws.s3::delete_object(
        object = file_to_remove,
        bucket = path_to_root_bucket,
      )
    }
    temp_file <- tempfile(fileext = ".fgb")
    sf::st_write(hull_concat,temp_file,append=FALSE)
    aws.s3::put_object(
      file = temp_file,
      object = glue::glue("model_footprints.fgb"),
      bucket = path_to_root_bucket
    )
    unlink(temp_file)

    if(is_verbose) {
      runtime <- Sys.time() - fn_time_start
      units(runtime) <- "hours"
      message(glue::glue("(re)-Merged {nrow(hull_concat)} models with {nrow(xs_lines)} cross sections and {nrow(point_concat)} points"))
      message(glue::glue("Wall time: {round(runtime, digits = 3)} hours"))
    }
    unlink(root_temp,recursive = TRUE)

  } else {
    # Remerge master features
    if(is_verbose) { message("Loading file paths") }
    massive_file_list <- list.files(path_to_ras_dbase, full.names=TRUE, ignore.case=TRUE, recursive=TRUE)

    xyz_files <- massive_file_list[grepl("*RRASSLER_cs_pts.parquet$", massive_file_list)] %>% sort()
    hull_files <- massive_file_list[grepl("*RRASSLER_hull.fgb$", massive_file_list)] %>% sort()
    rrassler_records <- massive_file_list[grepl("*RRASSLER_metadata.csv$", massive_file_list)] %>% sort()

    if(is_verbose) { message("Merging catalog") }
    full_accounting <- rbindlist(lapply(rrassler_records, function(x) data.table::fread(x, colClasses = c("nhdplus_comid" = "character","model_name" = "character","units" = "character","crs" = "character","final_name_key" = "character"))))
    length(hull_files)
    nrow(full_accounting)

    if(is_verbose) { message("Writing catalog") }
    unlink(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))
    data.table::fwrite(full_accounting,file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))

    if(!(length(xyz_files)==length(hull_files))) {
      print_error_block()
      message("Alert, hulls and points dont line up...")
      stop()
    }

    point_concat <- c()
    xs_concat <- c()
    hull_concat <- c()
    master_id <- 0
    rows_in_table <- length(hull_files)
    for(index in 1:rows_in_table) {
      # index = 7822
      final_folder_name <- basename(dirname(hull_files[index]))
      if(is_verbose) { message(glue::glue("Processing {index} of {rows_in_table}:{final_folder_name}")) }

      full_accounting[full_accounting$model_name=='Hill Creek',]
      row <- full_accounting[full_accounting$final_name_key==final_folder_name,]
      rrassler_records[7821:7824]


      hull <- sf::st_read(file.path(path_to_ras_dbase,"models",final_folder_name,"RRASSLER_hull.fgb",fsep = .Platform$file.sep),quiet = TRUE)
      point_data <- arrow::read_parquet(file.path(path_to_ras_dbase,"models",final_folder_name,"RRASSLER_cs_pts.parquet",fsep = .Platform$file.sep),as_data_frame = TRUE) %>%
        data.table::as.data.table()

      hull$start_master_id <- master_id + 1
      point_data[, master_id := xid + master_id]
      point_concat <- data.table::rbindlist(list(point_concat, point_data))
      master_id <- max(point_data[,master_id])

      hull$Name <- row$model_name
      hull$crs <- row$crs
      hull$units <- row$units
      hull$path <- row$final_name_key
      hull$source <- row$source
      hull$end_master_id <- master_id
      hull_concat <- rbind(hull_concat,hull)
    }

    if(is_verbose) { message(glue::glue("Making cross sections")) }
    xs_lines <- sfheaders::sf_linestring(
      obj = point_concat,
      x = "x",
      y = "y",
      # z = "z",
      linestring_id = "master_id",
      keep = FALSE) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

    if(is_verbose) { message(glue::glue("Writing data files")) }
    unlink(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
    unlink(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))
    unlink(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep))

    arrow::write_parquet(point_concat,file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep))
    sf::st_write(xs_lines,file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep),append=FALSE)
    sf::st_write(hull_concat,file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep),append=FALSE)

    if(is_verbose) {
      runtime <- Sys.time() - fn_time_start
      units(runtime) <- "hours"
      message(glue::glue("(re)-Merged {nrow(hull_concat)} models with {nrow(xs_lines)} cross sections and {nrow(point_concat)} points"))
      message(glue::glue("Wall time: {round(runtime, digits = 3)} hours"))
    }
  }

  return(TRUE)
}
