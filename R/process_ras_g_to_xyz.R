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
#'  }
#' }
#' @seealso
#'  [read.delim][utils::read.delim]
#'  [as.data.table][data.table::as.data.table]
#'  [fill][tidyr::fill]
#'  [sf_linestring][sfheaders::sf_linestring]
#'  [st_sf][sf::st_sf], [st_cast][sf::st_cast], [st_crs][sf::st_crs], [st_transform][sf::st_transform], [st_coordinates][sf::st_coordinates], [st_as_sf][sf::st_as_sf], [st_set_crs][sf::st_set_crs], [st_length][sf::st_length]
#'  [decimal_date][lubridate::decimal_date], [ymd][lubridate::ymd]
#'  [GET][httr::GET], [http_error][httr::http_error], [content][httr::content]
#'  [st_linesubstring][lwgeom::st_linesubstring], [st_endpoint][lwgeom::st_endpoint]
#'  [glue][glue::glue]
#' @rdname process_ras_g_to_xyz
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom utils read.delim
#' @importFrom data.table as.data.table
#' @importFrom tidyr fill
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_sf st_cast st_crs st_transform st_coordinates st_as_sf st_set_crs st_length
#' @importFrom lubridate decimal_date ymd
#' @importFrom httr GET http_error content
#' @importFrom lwgeom st_linesubstring st_endpoint
#' @importFrom glue glue

process_ras_g_to_xyz <- function(geom_path,
                                 units,
                                 proj_string,
                                 in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                 out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                 vdat = FALSE,
                                 quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/process_ras_g_to_xyz.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()

  # geom_path ="G:/data/ras_catalog/_temp/BLE/12090301/12090301_models/Piney Creek-Colorado River/BURLSON CREEK/BURLSON CREEK.g01"
  # proj_string,
  # in_epoch_override = as.integer(as.POSIXct(Sys.time())),
  # out_epoch_override = as.integer(as.POSIXct(Sys.time())),
  # vdat=FALSE,
  # quiet=FALSE

  ## -- Start --
  if (!quiet) {
    message('reading geom:')
    message(geom_path)
    message(units)
    message(proj_string)
    message(in_epoch_override)
    message(out_epoch_override)
  }

  if (is.null(units)) {
    # ToDo
  }

  file <-
    utils::read.delim(geom_path,
                      sep = '\n',
                      header = FALSE,
                      comment.char = "") |> data.table::as.data.table()

  xs_xy_row_heads <-
    which(grepl('XS GIS Cut Line', file$V1, fixed = TRUE))
  xs_xy_row_tails <-
    which(grepl('Node Last Edited', file$V1, fixed = TRUE))
  xs_statele_row_heads <-
    which(grepl('Sta/Elev=', file$V1, fixed = TRUE))
  xs_mann_row_heads <- which(grepl('Mann=', file$V1, fixed = TRUE))
  terminal_lines <- file[file$V1 %like% '.[A-Z].', which = TRUE]

  if (length(xs_xy_row_heads) == 0) {
    print_error_block()
    message('nope')
    return(list(data.frame()))
  }
  if (!isTRUE(length(xs_xy_row_heads) == length(xs_xy_row_tails))) {
    print_error_block()
    message('nope')
    return(list(data.frame()))
  }
  if (any(is.na(matrix(
    as.numeric(strsplit(
      gsub(
        "[[:blank:]]+",
        ",",
        do.call(paste, c(file[(xs_xy_row_heads[1] + 1):(xs_xy_row_tails[1] - 1), ], collapse =
                         "")) %>% noquote() %>% trimws()
      ),
      ","
    )[[1]]), ncol = 2, byrow = TRUE
  )))) {
    print_error_block()
    message('nope, xs is that weird format')
    return(list(data.frame()))
  }

  if (units == "English Units") {
    stn_unit_norm = 0.3048
  } else if (units == "SI Units") {
    stn_unit_norm = 1
  }

  if (!quiet) {
    message('Cross setions loaded')
  }
  tmp_point_dbase <- c()
  cross_section_lines <-
    data.frame(matrix(
      ncol = 6,
      nrow = 0,
      dimnames = list(
        NULL,
        c("geometry", "xid", "stream_stn", "river", "reach", "ras_path")
      )
    ))
  for (i in 1:length(xs_xy_row_heads)) {
    # i <- 2
    start <- xs_xy_row_heads[i] + 1
    end <- xs_xy_row_tails[i] - 1
    xs_dat <-
      gsub("[[:blank:]]+",
           ",",
           do.call(paste, c(file[start:end, ], collapse = "")) %>% noquote() %>% trimws())
    xs_dat <-
      matrix(as.numeric(strsplit(xs_dat, ",")[[1]]),
             ncol = 2,
             byrow = TRUE)

    cross_section_lines[i, ]$geometry <- list(xs_dat)
    cross_section_lines[i, ]$xid <- i

    xs_statelev_start <- xs_statele_row_heads[i] + 1
    next_stop_line <-
      terminal_lines[terminal_lines > xs_statelev_start][1] - 1
    xid_dz_dat <-
      gsub("[[:blank:]]+",
           ",",
           do.call(paste, c(file[xs_statelev_start:next_stop_line, ], collapse = "")) %>% noquote() %>% trimws())
    xid_dz_dat <-
      matrix(as.numeric(strsplit(xid_dz_dat, ",")[[1]]),
             ncol = 2,
             byrow = TRUE) %>% as.data.frame()
    colnames(xid_dz_dat) <- c('xid_d', 'z')

    xs_mann_start <- xs_mann_row_heads[i] + 1
    next_stop_line <-
      terminal_lines[terminal_lines > xs_mann_start][1] - 1
    xid_n_dat <-
      gsub("[[:blank:]]+",
           ",",
           do.call(paste, c(file[xs_mann_start:next_stop_line, ], collapse = "")) %>% noquote() %>% trimws())
    xid_n_dat <-
      matrix(as.numeric(strsplit(xid_n_dat, ",")[[1]]),
             ncol = 3,
             byrow = TRUE) %>% as.data.frame() %>% subset(select = -c(V3))
    colnames(xid_n_dat) <- c('xid_d', 'n')

    xs_point_data <-
      merge(
        x = xid_dz_dat,
        y = xid_n_dat,
        by = "xid_d",
        all.x = TRUE
      )
    xs_point_data <-
      xs_point_data %>% tidyr::fill("n", .direction = "down")
    xs_point_data$xid = i

    tmp_point_dbase <- rbind(tmp_point_dbase, xs_point_data)
  }

  ls_geo_extract <- function(x) {
    x_coords <- x$geometry[[1]][, 1]
    y_coords <- x$geometry[[1]][, 2]
    len <- length(x_coords)

    xid <- rep(toString(x$xid), length.out = len)
    stream_stn <- rep(toString(x$stream_stn), length.out = len)
    river <- rep(toString(x$river), length.out = len)
    reach <- rep(toString(x$reach), length.out = len)
    ras_path <- rep(toString(x$ras_path), length.out = len)

    sf_return <- sfheaders::sf_linestring(
      obj = data.frame(x_coords, y_coords, xid, stream_stn, river, reach),
      x = "x_coords",
      y = "y_coords",
      # , z =
      linestring_id = "xid",
      keep = TRUE
    ) |> sf::st_sf() |>
      sf::st_cast()

    return(sf_return)
  }
  sf_cross_section_lines <- c()
  for (h in 1:nrow(cross_section_lines)) {
    sf_cross_section_lines <-
      rbind(sf_cross_section_lines,
            ls_geo_extract(cross_section_lines[h, ]))
  }

  if (file.exists(proj_string)) {
    if (stringr::str_sub(proj_string, -3, -1) == "gdb") {
      sf::st_crs(sf_cross_section_lines) = sf::st_crs(rgdal::readOGR(dsn = proj_string, layer =
                                                                       "BLE_DEP01PCT"))
    } else {
      sf::st_crs(sf_cross_section_lines) = sf::st_crs(proj_string)
    }
  } else {
    sf::st_crs(sf_cross_section_lines) = proj_string
  }

  out <- get_datum_from_crs(sf_cross_section_lines)
  this_datum <- out[1]
  this_datum_unit <- out[2]

  if (this_datum == "NAVD_88") {
    this_datum <- gsub("_", "", this_datum)
  } else if (this_datum == "North American Datum 1983") {
    this_datum = 'NAD83_2011'
  }

  sf_cross_section_lines <-
    sf::st_transform(sf_cross_section_lines, sf::st_crs("EPSG:6349"))
  # leaflet::leaflet() %>%
  #   leaflet::addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  #   leafgl::addGlPolylines(data = sf::st_as_sf(sf_cross_section_lines, coords = c("x", "y","z"), crs = sf::st_crs("EPSG:6349")), group = "pts")

  point_database <- c()

  if (this_datum_unit == "usSurveyFoot") {
    elev_unit_norm = (1200 / 3937)
  } else if (this_datum_unit == "Foot") {
    elev_unit_norm = 0.3048
  } else {
    elev_unit_norm = 1
  }

  if (vdat) {
    date <- as.POSIXct(in_epoch_override, origin = "1970-01-01")
    this_date_YYYY <- format(date, format = "%Y")
    this_date_mm <- format(date, format = "%m")
    this_date_dd <- format(date, format = "%d")
    this_date_start <-
      lubridate::decimal_date(lubridate::ymd(
        paste0(this_date_YYYY, "-", this_date_mm, "-", this_date_dd)
      ))
    this_date_now <- lubridate::decimal_date(Sys.time())

    # transform datum
    mean_X <-
      mean(sf::st_coordinates(sf::st_cast(sf_cross_section_lines, "POINT"))[, 1])
    mean_Y <-
      mean(sf::st_coordinates(sf::st_cast(sf_cross_section_lines, "POINT"))[, 2])
    center_point = data.frame(lon = mean_X, lat = mean_Y) |>
      sf::st_as_sf(coords = c("lon", "lat")) |>
      sf::st_set_crs(sf::st_crs("EPSG:6349"))

    # determine and apply z transform
    datum_url <- paste0(
      "https://vdatum.noaa.gov/vdatumweb/api/convert?",
      "s_x=",
      as.character(sf::st_coordinates(center_point)[1, ][1]),
      "&s_y=",
      as.character(sf::st_coordinates(center_point)[1, ][2]),
      "&s_v_unit=m&t_v_unit=m",
      "&s_h_frame=",
      'NAD83_2011',
      "&s_v_frame=",
      this_datum,
      "&t_h_frame=NAD83_2011&t_v_frame=NAVD88",
      "&epoch_in=",
      this_date_start,
      "&epoch_out=",
      this_date_now
    )
    if (!quiet) {
      message(paste0("URL:", datum_url))
    }
    resp <- httr::GET(datum_url)
    if (httr::http_error(resp)) {
      print_warning_block()
      message(paste('poorly formed url - Request URL:', datum_url))
      return(list(data.frame()))
    }
    jsonRespParsed <- httr::content(resp, as = "parsed")
  }

  mean_shift <- 0
  for (t in 1:nrow(sf_cross_section_lines)) {
    # for(t in 1:100) {
    # t=2
    if (!quiet) {
      message(paste(
        "processing cross section number:",
        t,
        "of",
        nrow(sf_cross_section_lines)
      ))
    }

    point_slice <- tmp_point_dbase[tmp_point_dbase$xid == t, ]

    # Normalize distance
    point_slice$xid_d <- point_slice$xid_d - min(point_slice$xid_d)
    geom_xs_linestring <- sf_cross_section_lines[t, ]$geometry
    pt_xid_length <- sf::st_length(sf_cross_section_lines[t, ])
    max_stn <- point_slice[nrow(point_slice), 1] * stn_unit_norm
    mean_shift <- mean_shift + (as.numeric(pt_xid_length) - max_stn)
    for (point_index in 1:nrow(point_slice)) {
      # point_index <- 1
      stn <- point_slice[point_index, ]$xid_d * stn_unit_norm
      ratio <- stn / pt_xid_length
      pt <-
        lwgeom::st_linesubstring(geom_xs_linestring, from = 0, to = ratio) |> lwgeom::st_endpoint()
      pt_x <- pt[[1]][1]
      pt_y <- pt[[1]][2]

      if (vdat) {
        pt_z <-
          (point_slice[point_index, ]$z * stn_unit_norm) * elev_unit_norm + as.numeric(jsonRespParsed$t_z)
      } else {
        pt_z <-
          (point_slice[point_index, ]$z * stn_unit_norm) * elev_unit_norm
      }

      pt_n <- point_slice[point_index, ]$n
      pt_b <- "test"
      point_database <- rbind(
        point_database,
        data.frame(
          xid = point_slice[point_index, ]$xid,
          xid_length = pt_xid_length,
          xid_d = stn,
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
    notes <-
      glue::glue("* VDATUM offset:{jsonRespParsed$t_z} * profiles normalized by:{mean_shift}")
  } else {
    notes <- glue::glue("* profiles normalized by:{mean_shift}")
  }

  return(list(point_database, notes))
}
