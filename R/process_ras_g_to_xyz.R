#' @title process_ras_g_to_xyz
#' @description process a ras g file into xyz format
#' @param geom_path path to a file to parse
#' @param units units found in the project, "English Units" or "SI Units"
#' @param proj_string a projection string to apply
#' @param in_epoch_override vdatum parameter input epoch, Default: as.integer(as.POSIXct(Sys.time()))
#' @param out_epoch_override vdatum parameter output epoch, Default: as.integer(as.POSIXct(Sys.time()))
#' @param vdat a flag to dictate whether or not to apply a vdatum transformation, TRUE to apply, FALSE to skip, Default: FALSE, Default: FALSE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return a point database and notes about processing
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  # g_path <- "./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/12090301/12090301_models/Model/Alum Creek-Colorado River/ALUM 006/ALUM 006.g01"
#'  g_path <- fs::path_package("extdata/shapes.fgb", package = "mypkg")
#'  pts <- process_ras_g_to_xyz(geom_path = g_path,units = "English Units",proj_string = "EPSG:2277",vdat = FALSE,quiet = FALSE)
#'  }
#' }
#' @seealso
#'  \code{\link[utils]{glob2rx}}, \code{\link[utils]{read.table}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.table]{as.data.table}}
#'  \code{\link[stringr]{str_split}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_flatten}}, \code{\link[stringr]{str_sub}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[sf]{sf}}, \code{\link[sf]{st_cast}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{geos_measures}}
#'  \code{\link[tidyr]{fill}}
#'  \code{\link[lubridate]{decimal_date}}, \code{\link[lubridate]{ymd}}
#'  \code{\link[httr]{GET}}, \code{\link[httr]{http_error}}, \code{\link[httr]{content}}
#'  \code{\link[lwgeom]{st_linesubstring}}, \code{\link[lwgeom]{st_startpoint}}
#' @rdname process_ras_g_to_xyz
#' @export
#' @importFrom utils glob2rx read.delim
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @importFrom stringr str_split str_trim str_flatten str_sub
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_sf st_cast st_crs st_read st_transform st_coordinates st_as_sf st_set_crs st_length
#' @importFrom tidyr fill
#' @importFrom lubridate decimal_date ymd
#' @importFrom httr GET http_error content
#' @importFrom lwgeom st_linesubstring st_endpoint

process_ras_g_to_xyz <- function(geom_path,
                                 units,
                                 proj_string,
                                 in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                 out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                 vdat = FALSE,
                                 quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/process_ras_g_to_xyz.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=FALSE)
  #
  # devtools::load_all()

  ## First sample
  # geom_path="./inst/extdata/sample_ras/FEMA-R6-BLE-sample-dataset/12090301/12090301_models/Model/Alum Creek-Colorado River/ALUM 006/ALUM 006.g01"
  # units = "English Units"
  # proj_string="EPSG:2277"
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # vdat=FALSE
  # quiet=FALSE

  ## Second sample
  # geom_path="./inst/extdata/sample_ras/ras2fim-sample-dataset/input_iowa/10170204000897/Hydraulic_Models/Simulations/10170204000897.g01"
  # units = "SI Units"
  # proj_string="EPSG:26915"
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # vdat=FALSE
  # quiet=FALSE

  ## -- Start --
  # Input parsing
  if (!quiet) {
    message('reading geom:')
    message(geom_path)
    message(glue::glue("Found units: {units} / projection:{proj_string}"))
    if (vdat) {
      message(glue::glue("In time: {in_epoch_override} / out time:{out_epoch_override}"))
    }
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
    message("An incorrectly formatted g file was found (lines did not match up)")
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

  # Add projection info to planform lines
  if (file.exists(proj_string)) {
    if (stringr::str_sub(proj_string, -3, -1) == "gdb") {
      sf::st_crs(sf_xs_lines) = sf::st_crs(sf::st_read(proj_string,layer="BLE_DEP01PCT"))
      sf::st_crs(sf_reach_lines) = sf::st_crs(sf::st_read(proj_string,layer="BLE_DEP01PCT"))
    } else {
      sf::st_crs(sf_xs_lines) = sf::st_crs(proj_string)
      sf::st_crs(sf_reach_lines) = sf::st_crs(proj_string)
    }
  } else {
    sf::st_crs(sf_xs_lines) = proj_string
    sf::st_crs(sf_reach_lines) = proj_string
  }
  out <- get_datum_from_crs(sf_xs_lines)
  this_datum <- out[1]
  this_datum_unit <- out[2]

  # Transform lines to common datum/proj
  sf_xs_lines <- sf::st_transform(sf_xs_lines, sf::st_crs("EPSG:6349"))
  sf_reach_lines <- sf::st_transform(sf_reach_lines, sf::st_crs("EPSG:6349"))

  # Datum unit normalization
  if (this_datum_unit == "usSurveyFoot") {
    elev_unit_norm = (1200 / 3937)
  } else if (this_datum_unit == "Foot") {
    elev_unit_norm = 0.3048
  } else {
    elev_unit_norm = 1
  }

  # Do we want to apply vdatum transforms?
  if (vdat) {
    date <- as.POSIXct(in_epoch_override, origin = "1970-01-01")
    this_date_YYYY <- format(date, format = "%Y")
    this_date_mm <- format(date, format = "%m")
    this_date_dd <- format(date, format = "%d")
    this_date_start <- lubridate::decimal_date(lubridate::ymd(paste0(this_date_YYYY, "-", this_date_mm, "-", this_date_dd)))
    this_date_now <- lubridate::decimal_date(Sys.time())

    # transform datum
    mean_X <- mean(sf::st_coordinates(sf::st_cast(sf_xs_lines, "POINT"))[, 1])
    mean_Y <- mean(sf::st_coordinates(sf::st_cast(sf_xs_lines, "POINT"))[, 2])
    center_point = data.frame(lon = mean_X, lat = mean_Y) |>
      sf::st_as_sf(coords = c("lon", "lat")) |>
      sf::st_set_crs(sf::st_crs("EPSG:6349"))

    # determine z transform
    datum_url <- paste0("https://vdatum.noaa.gov/vdatumweb/api/convert?",
                        "s_x=",as.character(sf::st_coordinates(center_point)[1, ][1]),
                        "&s_y=",as.character(sf::st_coordinates(center_point)[1, ][2]),
                        "&s_v_unit=m&t_v_unit=m","&s_h_frame=",'NAD83_2011',"&s_v_frame=",this_datum,"&t_h_frame=NAD83_2011&t_v_frame=NAVD88",
                        "&epoch_in=",this_date_start,
                        "&epoch_out=",this_date_now)
    if (!quiet) {
      message(paste0("URL:", datum_url))
    }
    resp <- httr::GET(datum_url)
    if (httr::http_error(resp)) {
      print_warning_block()
      message(paste('poorly formed url - Request URL:', datum_url))
      return(FALSE)
    }
    jsonRespParsed <- httr::content(resp, as = "parsed")
  }

  mean_shift <- 0
  normalized_point_database <- c()
  for (t in 1:nrow(sf_xs_lines)) {
    if (!quiet) { message(paste("processing cross section number:",t,"of",nrow(sf_xs_lines))) }

    point_slice <- point_database[point_database$xid == t, ]

    # Normalize distance
    point_slice$relative_dist <- (point_slice$stn - min(point_slice$stn)) * stn_unit_norm
    geom_xs_linestring <- sf_xs_lines[t, ]$geometry
    pt_xid_length <- sf::st_length(sf_xs_lines[t, ])

    mean_shift <- mean_shift + (as.numeric(pt_xid_length) - max(point_slice$relative_dist))

    for (point_index in 1:nrow(point_slice)) {
      stn <- point_slice[point_index, ]$relative_dist
      ratio <- stn / as.numeric(pt_xid_length)
      capped_ratio <- min(ratio,1.0)
      pt <- lwgeom::st_linesubstring(geom_xs_linestring, from = 0, to = capped_ratio) |> lwgeom::st_endpoint() |> suppressWarnings()
      pt_x <- pt[[1]][1]
      pt_y <- pt[[1]][2]

      if (vdat) {
        pt_z <- (point_slice[point_index, ]$Z * stn_unit_norm) * elev_unit_norm + as.numeric(jsonRespParsed$t_z)
      } else {
        pt_z <- (point_slice[point_index, ]$Z * stn_unit_norm) * elev_unit_norm
      }

      pt_n <- point_slice[point_index, ]$n
      pt_b <- "test"
      normalized_point_database <- rbind(
        normalized_point_database,
        data.frame(
          xid = t,
          xid_length = pt_xid_length,
          xid_d = stn,
          relative_dist = capped_ratio,
          x = pt_x,
          y = pt_y,
          z = pt_z,
          n = pt_n,
          source = 3
        )
      )
    }
  }

  if (vdat) {
    notes <- glue::glue("* VDATUM offset:{jsonRespParsed$t_z} * profiles normalized by:{mean_shift}")
  } else {
    notes <- glue::glue("* profiles normalized by:{mean_shift}")
  }

  if (!quiet) {
    message(geom_path)
    message(notes)
  }

  return(list(normalized_point_database, notes,sf_reach_lines))
}

