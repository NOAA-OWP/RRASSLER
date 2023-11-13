#' @title process_ras_hdf_to_xyz
#' @description process a ras g##.hdf file into xyz format
#' @param geom_path path to a file to parse
#' @param units units found in the project, "English Units" or "SI Units"
#' @param proj_string a projection string to apply
#' @param in_epoch_override vdatum parameter input epoch, Default: as.integer(as.POSIXct(Sys.time()))
#' @param out_epoch_override vdatum parameter output epoch, Default: as.integer(as.POSIXct(Sys.time()))
#' @param vdat a flag to dictate whether or not to apply a vdatum transformation, TRUE to apply, FALSE to skip, Default: FALSE
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
#'  [str_detect][stringr::str_detect]
#'  [stri_sub][stringi::stri_sub]
#'  [h5read][rhdf5::h5read]
#'  [sf_linestring][sfheaders::sf_linestring]
#'  [st_sf][sf::st_sf], [st_cast][sf::st_cast], [st_crs][sf::st_crs], [st_transform][sf::st_transform], [st_coordinates][sf::st_coordinates], [st_as_sf][sf::st_as_sf], [st_set_crs][sf::st_set_crs], [st_length][sf::st_length]
#'  [decimal_date][lubridate::decimal_date], [ymd][lubridate::ymd]
#'  [GET][httr::GET], [http_error][httr::http_error], [content][httr::content]
#'  [fill][tidyr::fill]
#'  [st_linesubstring][lwgeom::st_linesubstring], [st_endpoint][lwgeom::st_endpoint]
#'  [glue][glue::glue]
#' @rdname process_ras_hdf_to_xyz
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom stringr str_detect
#' @importFrom stringi stri_sub
#' @importFrom rhdf5 h5read
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_sf st_cast st_crs st_transform st_coordinates st_as_sf st_set_crs st_length
#' @importFrom lubridate decimal_date ymd
#' @importFrom httr GET http_error content
#' @importFrom tidyr fill
#' @importFrom lwgeom st_linesubstring st_endpoint
#' @importFrom glue glue

process_ras_hdf_to_xyz <- function(geom_path,
                                   units,
                                   proj_string,
                                   in_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                   out_epoch_override = as.integer(as.POSIXct(Sys.time())),
                                   vdat = FALSE,
                                   quiet = FALSE) {
  # sinew::moga(file.path(getwd(),"R/process_ras_hdf_to_xyz.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # geom_path="J:/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data/10170204000897/Hydraulic_Models/Simulations/10170204000897.g01.hdf"
  # units="Meter"
  # proj_string="EPSG:26915"
  # in_epoch_override = as.integer(as.POSIXct(Sys.time()))
  # out_epoch_override = 1616607646
  # vdat=TRUE
  # quiet=FALSE

  # geom_path="J:/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data/HICKORY CREEK/HICKORY CREEK.g01.hdf"
  # units="Foot"
  # proj_string="EPSG:2277"

  ## -- Start --
  if (!quiet) {
    message('reading geom:')
    message(geom_path)
    message(units)
    message(proj_string)
    message(in_epoch_override)
    message(out_epoch_override)
  }

  if (!file.exists(geom_path)) {
    print("404 - File not found")
    return(list(data.frame()))
  }
  if (stringr::str_detect(stringi::stri_sub(geom_path, -3, -1), "(?i)hdf")) {
    filename <- stringi::stri_sub(geom_path, -7, -1)
  } else {
    filename <- stringi::stri_sub(geom_path, -3, -1)
  }
  if (file.exists(file.path(dirname(geom_path), paste0(filename, '_ras_xyz_data.csv')))) {
    print("File already processed")
    return(list(TRUE))
  }

  if (units == "English Units") {
    stn_unit_norm = 0.3048
  } else if (units == "SI Units") {
    stn_unit_norm = 1
  }

  # How to read an .g##.hdf file
  # hf <- rhdf5::H5Fopen(geom_path)
  try(n1 <-
        rhdf5::h5read(geom_path, "Geometry/Cross Sections/Polyline Points"),
      silent = TRUE)
  try(n2 <-
        rhdf5::h5read(geom_path, "Geometry/Cross Sections/Polyline Parts"),
      silent = TRUE)
  n3 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/Attributes"),
        silent = TRUE)
  if ("try-error" %in% class(n3)) {
    n3 <- 0
  }
  n4 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/River Names"),
        silent = TRUE)
  if ("try-error" %in% class(n4)) {
    n4 <- 'Unknown-not-found'
  }
  n5 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/Reach Names"),
        silent = TRUE)
  if ("try-error" %in% class(n5)) {
    n5 <- 'Unknown-not-found'
  }
  n6 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/River Stations"),
        silent = TRUE)
  if ("try-error" %in% class(n6)) {
    n6 <- n3
  }
  n7 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/Manning's n Info"),
        silent = TRUE)
  if ("try-error" %in% class(n7)) {
    n7 <-
      try(rhdf5::h5read(geom_path,
                        "Geometry/Cross Sections/Station Manning's n Info"),
          silent = TRUE)
  }
  n8 <-
    try(rhdf5::h5read(geom_path, "Geometry/Cross Sections/Manning's n Values"),
        silent = TRUE)
  if ("try-error" %in% class(n8)) {
    n8 <-
      try(rhdf5::h5read(geom_path,
                        "Geometry/Cross Sections/Station Manning's n Values"),
          silent = TRUE)
  }
  try(n9 <-
        rhdf5::h5read(geom_path, "Geometry/Cross Sections/Station Elevation Info"),
      silent = TRUE)
  try(n10 <-
        rhdf5::h5read(geom_path,
                      "Geometry/Cross Sections/Station Elevation Values"),
      silent = TRUE)

  list_points_per_cross_section_line <- n2[2, ]

  if (isTRUE(nrow(n3) > 0)) {
    # if(isFALSE(is.null(nrow(0)))){
    list_river_name <- n3$River
    list_reach_name <- n3$Reach
    list_station <- n3$RS
  } else {
    list_river_name <- n4
    list_reach_name <- n5
    list_station <- n6
  }

  if (!quiet) {
    message('Cross setions loaded')
  }
  # Next we get the xy data for the line
  cross_section_lines <-
    data.frame(matrix(
      ncol = 6,
      nrow = 0,
      dimnames = list(
        NULL,
        c("geometry", "xid", "stream_stn", "river", "reach", "ras_path")
      )
    ))

  # Loop through the cross section lines and create GeoDataFrame
  int_startPoint <- 1

  for (j in 1:length(list_points_per_cross_section_line)) {
    # j<- 1
    int_endPoint <- sum(list_points_per_cross_section_line[1:j])

    #for(k in 1:nlist(t(n1[,int_startPoint:(int_startPoint+int_numPnts)])) )
    cross_section_lines[j, ]$geometry <-
      list(t(n1[, int_startPoint:int_endPoint]))
    cross_section_lines[j, ]$xid <- j
    cross_section_lines[j, ]$stream_stn <- list_station[j]
    cross_section_lines[j, ]$river <- list_river_name[j]
    cross_section_lines[j, ]$reach <- list_reach_name[j]
    cross_section_lines[j, ]$ras_path <- geom_path

    int_startPoint <- int_endPoint + 1
  }
  #print(cross_section_lines)

  # line string planer form of geometry
  ls_geo_extract <- function(x) {
    x_coords <- x$geometry[[1]][, 1]
    y_coords <- x$geometry[[1]][, 2]
    len <- length(x_coords)
    #xid <- rep(x$xid,length.out=len)
    #stream_stn <- rep(x$stream_stn,length.out=len)
    #river <- rep(x$river,length.out=len)
    #reach <- rep(x$reach,length.out=len)
    #ras_path <- rep(x$ras_path,length.out=len)
    xid <- rep(toString(x$xid), length.out = len)
    stream_stn <- rep(toString(x$stream_stn), length.out = len)
    river <- rep(toString(x$river), length.out = len)
    reach <- rep(toString(x$reach), length.out = len)
    ras_path <- rep(toString(x$ras_path), length.out = len)
    #print(paste("x_coords",x$geometry[[1]][,1]))
    #print(paste("y_coords",x$geometry[[1]][,2]))
    #print(paste("xid:",x$xid))
    #print(paste("stream_stn:",x$stream_stn))
    #print(paste("river:",x$river))
    #print(paste("reach:",x$reach))
    #stream_stn <- rep(x$stream_stn,length.out=len)
    #river <- rep(x$river,length.out=len)
    #reach <- rep(x$reach,length.out=len)
    #ras_path <- rep(x$ras_path,length.out=len)
    #data.frame(x_coords = x_coords,y_coords = y_coords,xid = xid,stream_stn = stream_stn,river = river,reach = reach)
    #print(data.frame(x_coords = x_coords,y_coords = y_coords,xid = xid,stream_stn = stream_stn,river = river,reach = reach))

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
  # data.frame(matrix(ncol=6,nrow=0,dimnames=list(NULL, c("geometry", "xid","stream_stn", "river","reach","ras_path"))))
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
    message(paste0("URL:", datum_url))
    resp <- httr::GET(datum_url)
    if (httr::http_error(resp)) {
      print('ALERT!!')
      print(paste('poorly formed url - Request URL:', datum_url))
      return(list(data.frame()))
    }
    jsonRespParsed <- httr::content(resp, as = "parsed")
  }

  mean_shift <- 0
  for (t in 1:ncol(n2)) {
    # for(t in 1:100) {
    # t=2
    if (!quiet) {
      message(paste("processing cross section number:", t, "of", ncol(n2)))
    }
    str_current_xs <- sf_cross_section_lines[t, ]$reach
    geom_xs_linestring = sf_cross_section_lines[t, ]$geometry

    int_prof_xs_start_pnt = n9[1, t]# + 1
    int_prof_pnts_in_xs = n9[2, t]
    int_prof_xs_end_pnt = int_prof_xs_start_pnt + int_prof_pnts_in_xs -
      1
    list_xs_station = n10[1, int_prof_xs_start_pnt:int_prof_xs_end_pnt]
    list_xs_elevation = n10[2, int_prof_xs_start_pnt:int_prof_xs_end_pnt]

    int_prof_xs_n_start_pnt = n7[1, t]
    int_prof_n_pnts_in_xs = n7[2, t]
    int_prof_xs_n_end_pnt = int_prof_xs_n_start_pnt + int_prof_n_pnts_in_xs -
      1
    list_xs_n_station = n8[1, int_prof_xs_n_start_pnt:int_prof_xs_n_end_pnt]
    list_xs_n = n8[2, int_prof_xs_n_start_pnt:int_prof_xs_n_end_pnt]

    station_elevation_data <-
      data.frame(xid_d = unlist(list_xs_station),
                 z = unlist(list_xs_elevation))
    station_n_data <-
      data.frame(xid_d = unlist(list_xs_n_station),
                 n = unlist(list_xs_n))

    xs_point_data <-
      merge(
        x = station_elevation_data,
        y = station_n_data,
        by = "xid_d",
        all.x = TRUE
      )
    xs_point_data <-
      xs_point_data %>% tidyr::fill("n", .direction = "down")

    # Normalize distance
    xs_point_data$xid_d <-
      xs_point_data$xid_d - min(xs_point_data$xid_d)

    pt_xid_length <- sf::st_length(geom_xs_linestring)
    max_stn <- xs_point_data[nrow(xs_point_data), 1] * stn_unit_norm
    mean_shift <- mean_shift + (as.numeric(pt_xid_length) - max_stn)

    for (point_index in 1:nrow(xs_point_data)) {
      stn <- xs_point_data[point_index, 1] * stn_unit_norm
      ratio <- stn / pt_xid_length
      pt <-
        lwgeom::st_linesubstring(geom_xs_linestring, from = 0, to = ratio) |> lwgeom::st_endpoint()
      pt_x <- pt[[1]][1]
      pt_y <- pt[[1]][2]

      if (vdat) {
        pt_z <-
          (xs_point_data[point_index, 2] * stn_unit_norm) * elev_unit_norm + as.numeric(jsonRespParsed$t_z)
      } else {
        pt_z <-
          (xs_point_data[point_index, 2] * stn_unit_norm) * elev_unit_norm
      }

      pt_n <- xs_point_data[point_index, 3]
      pt_b <- "test"
      point_database <- rbind(
        point_database,
        data.frame(
          xid = t,
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

  # print(file.path(dirname(geom_path),paste(filename,'_ras_xyz_data.csv')))
  # utils::write.table(point_database,file = file.path(dirname(geom_path),paste0(filename,'_ras_xyz_data.csv')))
  # unique(point_database$xid)
  # ggplot(data = point_database[point_database$xid==4,], aes(xid_d, z, color = z))+
  #   geom_point()+
  #   theme_light()+
  #   scale_color_gradientn(colors = terrain.colors(10))+
  #   labs(x = "Distance along profile [m]", y = "Elevation [m]", color = "Elevation [m]")
  # leaflet::leaflet() %>%
  #   leaflet::addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  #   leafgl::addGlPoints(data = sf::st_as_sf(point_database, coords = c("x", "y","z"), crs = sf::st_crs("EPSG:6349")), group = "pts")
  # # par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
  # # barplot(max.temp, main="Barplot")
  # # pie(max.temp, main="Piechart", radius=1)
  # ls = sfheaders::sf_linestring(
  #   obj = point_database
  #   , x = "x"
  #   , y = "y"
  #   , z = "z"
  #   , linestring_id = "xid"
  #   , keep = FALSE
  # ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

  return(list(point_database, notes))
}
