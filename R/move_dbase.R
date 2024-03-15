#' @title move_dbase
#' @description Bulk database mover
#' @return a copy of the database in the new location
#' @family helper
#' @details dev
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  move_dbase(from = NULL, to = NULL)
#'  }
#' }
#' @rdname move_dbase
#' @export
move_dbase <- function(from = NULL, to = NULL) {
  # sinew::moga(file.path(getwd(),"R/move_dbase.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()

  # from = "~/data/ras_catalog/"
  # to = "s3://ras-models/"
  # is_verbose = TRUE

  ## -- Start --

  message(" ")
  # Moving disk to disk


  # Moving disk to cloud
  ls_srt_folders_file <- strsplit(to, "/")[[1]]
  ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
  path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
  rest_of_bucket_prefix <- stringr::str_sub(to, nchar(path_to_root_bucket)+1, nchar(to)-1)


  all_rrassled_files <- list.files(from, full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  all_relevent_rrassled_files <- Filter(function(x) !any(grepl("_temp", x)), all_rrassled_files)
  for(file_to_copy in all_relevent_rrassled_files) {
    if (is_verbose) { message(glue::glue("Trying to move:{file_to_copy}")) }
    aws.s3::put_object(
      file = file_to_copy,
      object = glue::glue("{rest_of_bucket_prefix}{stringr::str_sub(file_to_copy, nchar(from)+1, nchar(file_to_copy))}"),
      bucket = path_to_root_bucket
    )
  }

  # Moving cloud to disk
  bucket_path <- "s3://ras-models/ras_catalog///models/"
  local_path <- "C:/Users/rdp-user/Desktop/scraped_models"

  ls_srt_folders_file <- strsplit(bucket_path, "/")[[1]]
  ls_srt_folders_file <- ls_srt_folders_file[ls_srt_folders_file != ""]
  path_to_root_bucket <- paste0("s3://", ls_srt_folders_file[2], "/")
  rest_of_bucket_prefix <- stringr::str_sub(bucket_path, nchar(path_to_root_bucket)+1, nchar(bucket_path)-1)

  message(glue::glue("Gathering bucket contents"))
  df_bucket_data <- aws.s3::get_bucket(bucket = path_to_root_bucket, prefix = "ras_catalog//models/",max = Inf)
  list_bucket_data <- c()
  for(i in 1:length(df_bucket_data)) {
    list_bucket_data <- c(
      list_bucket_data,
      df_bucket_data[[i]]$Key)
  }
  list_bucket_data_dt <- data.table::as.data.table(list_bucket_data)

  message(glue::glue("Moving requisite files"))
  for(index in 1:nrow(list_bucket_data_dt)) {
    file_to_move <- list_bucket_data_dt[index][[1]]
    aws.s3::save_object(
      object = file_to_move,
      bucket = path_to_root_bucket,
      file = file.path(local_path,substring(file_to_move, 14),fsep = .Platform$file.sep)
    )
  }

  # Moving cloud to cloud


  return(TRUE)
}
