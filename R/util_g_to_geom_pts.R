#' @title util_g_to_geom_pts
#' @description FUNCTION_DESCRIPTION
#' @param geom_path PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION
#' @param proj_string PARAM_DESCRIPTION
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{glob2rx}}, \code{\link[utils]{read.table}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.table]{as.data.table}}
#'  \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_flatten}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[sf]{sf}}, \code{\link[sf]{st_cast}}
#'  \code{\link[tidyr]{fill}}
#' @rdname util_g_to_geom_pts
#' @export 
#' @importFrom utils glob2rx read.delim
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @importFrom stringr str_split str_trim str_flatten
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_sf st_cast
#' @importFrom tidyr fill

util_g_to_geom_pts <- function(geom_path,
                                   units,
                                   proj_string,
                                   quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/util_g_to_geom_pts.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  # devtools::load_all()
  #
  # geom_path = g_path
  # units = "SI Units"
  # quiet = FALSE

  ## -- Start --

  if (!quiet) {
    message('reading geom:')
    message(geom_path)
  }

  if (!file.exists(geom_path)) {
    print_error_block()
    print("404 - File not found")
    return(list(data.frame()))
  }

  if (is.null(units)) {
    # Try and find the units in line by reading and string matching all found prj files
    prj_files <- list.files(dirname(geom_path),pattern = utils::glob2rx(glue::glue("{basename(stringr::str_sub(geom_path,0,-5))}.prj$")),full.names = TRUE,ignore.case = TRUE,recursive = TRUE)

    if (length(prj_files) > 0) {
      for (potential_file in prj_files) {
        file_text <- read.delim(potential_file, header = FALSE)

        if (grepl("SI Units", file_text, fixed = TRUE)) {
          units <- "SI Units"
        } else if (grepl("English Units", file_text, fixed = TRUE)) {
          units <- "English Units"
        }
      }
    }

    if(is.null(units)) {
      print_error_block()
      message("No units found, you will need to manually specify them for this model")
      return(FALSE)
    }
  }

  # Units of model and correction for unit conversion
  if (units == "English Units") {
    stn_unit_norm = 0.3048
  } else if (units == "SI Units") {
    stn_unit_norm = 1
  }

  # reach info
  reach_info = data.frame(reach_number = numeric(0), reach_name = character(), reach_line_start = numeric(0))
  reach_number <- 0
  file_text <-  utils::read.delim(geom_path, sep = '\n', header = FALSE, comment.char = "") |>
    data.table::as.data.table()
  for(row in 1:nrow(file_text)) {
    if(grepl("River Reach", file_text[row], fixed = TRUE)) {
      reach_number = reach_number + 1
      reach_name = stringr::str_split(file_text[row],"=",simplify = TRUE)[-1] |> stringr::str_trim()
      reach_line_start = row
      reach_info[nrow(reach_info)+1,] <- c(reach_number,reach_name,reach_line_start)
    }
  }

  riv_xy_heads <-  which(grepl('River Reach', file_text$V1, fixed = TRUE))
  xs_xy_row_heads <- which(grepl('XS GIS Cut Line', file_text$V1, fixed = TRUE))
  xs_statele_row_heads <- which(grepl('Sta/Elev=', file_text$V1, fixed = TRUE))
  xs_mann_row_heads <- which(grepl('Mann=', file_text$V1, fixed = TRUE))
  non_numeric_rows <- append(grep("^[A-Za-z]", file_text$V1),grep("^#", file_text$V1)) |> sort()

  # Quick logic check
  if(!all.equal(length(xs_xy_row_heads),
                length(xs_statele_row_heads),
                length(xs_mann_row_heads))) {
    print_error_block()
    message("An oddly formatted g file was found (lines did not match up)")
    return(FALSE)
  }

  # Start by pulling rivers
  sf_reach_lines <- c()
  for(reach in 1:length(riv_xy_heads)) {
    reach_start <- riv_xy_heads[reach]
    while (reach_start %in% non_numeric_rows) {
      reach_start <- reach_start + 1
    }
    reach_end <- non_numeric_rows[first(which(non_numeric_rows > reach_start))] - 1
    raw_file <- file_text[reach_start:reach_end, ]
    for(row in 1:nrow(raw_file)) {
      raw_file[row] <- gsub("(.{16})", "\\1, ",raw_file[row])
    }
    riv_xy_dat <- raw_file$V1 %>% noquote() %>% trimws()
    riv_xy_dat <- strsplit(stringr::str_flatten(riv_xy_dat), ",")
    riv_xy_dat <- data.frame(X = as.numeric(riv_xy_dat[[1]][c(TRUE,FALSE)]),Y = as.numeric(riv_xy_dat[[1]][c(FALSE,TRUE)]))

    colnames(riv_xy_dat) <- c('X', 'Y')
    sf_riv <- sfheaders::sf_linestring(
      obj = riv_xy_dat,
      x = "X",
      y = "Y",
      keep = TRUE
    ) |> sf::st_sf() |>
      sf::st_cast()
    sf_riv$reach_id <- reach
    sf_reach_lines <- rbind(sf_reach_lines, sf_riv)
  }

  # next we'll pull xs
  sf_xs_lines <- c()
  point_database <- c()
  for (i in 1:length(xs_xy_row_heads)) {
    # Cross section planform
    xs_xy_start <- xs_xy_row_heads[i] + 1
    while (xs_xy_start %in% non_numeric_rows) {
      xs_xy_start <- xs_xy_start + 1
    }
    xs_xy_end <- non_numeric_rows[first(which(non_numeric_rows > xs_xy_start))] - 1

    raw_file <- file_text[xs_xy_start:xs_xy_end, ]
    for(row in 1:nrow(raw_file)) {
      raw_file[row] <- gsub("(.{16})", "\\1, ",raw_file[row])
    }
    xs_xy_dat <- raw_file$V1 %>% noquote() %>% trimws()
    xs_xy_dat <- strsplit(stringr::str_flatten(xs_xy_dat), ",")
    xs_xy_dat <- data.frame(X = as.numeric(xs_xy_dat[[1]][c(TRUE,FALSE)]),Y = as.numeric(xs_xy_dat[[1]][c(FALSE,TRUE)]))

    # Cross section station elevation
    xs_sz_start <- xs_statele_row_heads[i] + 1
    while (xs_sz_start %in% non_numeric_rows) {
      xs_sz_start <- xs_sz_start + 1
    }
    xs_sz_end <- non_numeric_rows[first(which(non_numeric_rows > xs_sz_start))] - 1

    raw_file <- file_text[xs_sz_start:xs_sz_end, ]
    for(row in 1:nrow(raw_file)) {
      raw_file[row] <- gsub("(.{8})", "\\1, ",raw_file[row])
    }
    xs_sz_dat <- raw_file$V1 %>% noquote() %>% trimws()
    xs_sz_dat <- strsplit(stringr::str_flatten(xs_sz_dat), ",")
    xs_sz_dat <- data.frame(stn = as.numeric(xs_sz_dat[[1]][c(TRUE,FALSE)]),Z = as.numeric(xs_sz_dat[[1]][c(FALSE,TRUE)]))

    # Cross section n
    # TODO: Figure out what that 3rd row is supposed to mean
    xs_n_start <- xs_mann_row_heads[i] + 1
    while (xs_n_start %in% non_numeric_rows) {
      xs_n_start <- xs_n_start + 1
    }
    xs_n_end <- non_numeric_rows[first(which(non_numeric_rows > xs_n_start))] - 1
    xs_n_dat <- gsub("[[:blank:]]+",",",do.call(paste, c(file_text[xs_n_start:xs_n_end, ], collapse = "")) %>% noquote() %>% trimws())
    xs_n_dat <- matrix(as.numeric(strsplit(xs_n_dat, ",")[[1]]), ncol = 3, byrow = TRUE) %>% as.data.frame() %>% subset(select = -c(V3))
    colnames(xs_n_dat) <- c('stn', 'n')

    # merge them into lines
    xs_lines <- sfheaders::sf_linestring(
      obj = xs_xy_dat,
      x = "X",
      y = "Y",
      keep = TRUE
    ) |> sf::st_sf() |>
      sf::st_cast()
    xs_lines$xid <- i
    sf_xs_lines <- rbind(sf_xs_lines, xs_lines)

    # merge them into points
    xs_point_data <- merge(x = xs_sz_dat,y = xs_n_dat,by = "stn",all.x = TRUE, all.y = TRUE)
    xs_point_data <- xs_point_data %>%
      tidyr::fill("n", .direction = "down")
    xs_point_data$xid <- i
    point_database <- rbind(point_database, xs_point_data)
  }

  return(point_database)
}
