#' @title map_library
#' @description a function to help apply geographic context to the RRASSLED catalog
#' @param path_to_ras_dbase The path to the folder in which you are building your catalog, Default: NULL
#' @param AOI_to_map An AOI pass though to subset the catalog down into a smaller area to add the context too, Default: NULL
#' @param name A name for the map files to create, Default: 'model_map'
#' @param plot_lines draw lines on the map as opposed to just the footprints, Default: FALSE
#' @param chart_lines Add cross section click graphs to the plot lines.  Will break if area is too large, Default: FALSE
#' @param refresh flag to dictate whether or not to recollate spatial database prior to mapping.  FALSE to skip, TRUE to regenerate, Default: TRUE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{valid}}
#'  \code{\link[arrow]{read_parquet}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{scale_colour_gradient}}, \code{\link[ggplot2]{labs}}
#'  \code{\link[leaflet]{leaflet}}, \code{\link[leaflet]{addProviderTiles}}, \code{\link[leaflet]{addLegend}}, \code{\link[leaflet]{addLayersControl}}
#'  \code{\link[leafem]{addFeatures}}
#'  \code{\link[leafpop]{popupGraph}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[mapview]{mapshot}}
#' @rdname map_library
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom sf st_transform st_read st_crs st_make_valid
#' @importFrom arrow read_parquet
#' @importFrom ggplot2 ggplot aes geom_point theme_light scale_color_gradientn labs
#' @importFrom leaflet leaflet leafletOptions addProviderTiles addLegend addLayersControl
#' @importFrom leafem addFeatures
#' @importFrom leafpop popupGraph popupTable
#' @importFrom glue glue
#' @importFrom mapview mapshot

map_library <-  function(path_to_ras_dbase,
                         AOI_to_map=NULL,
                         name="model_map",
                         plot_lines=FALSE,
                         chart_lines=FALSE,
                         refresh=FALSE,
                         quiet=TRUE) {
  # sinew::moga(file.path(getwd(),"R/map_library.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # path_to_ras_dbase = "/home/rstudio/g/data/ras_dbase"
  # AOI_to_map="12090301"
  # name="12090301_input_models"
  # plot_lines=TRUE
  # chart_lines=FALSE
  # chart_lines=TRUE
  # refresh=FALSE
  # RRASSLER::map_library("/home/rstudio/g/data/ras_dbase",NULL,name="model_map",plot_lines=TRUE,chart_lines=TRUE,refresh=FALSE,quiet=FALSE)
  # map_library("H:/ras_dbase","12090301","12090301_model_footprints",TRUE,FALSE,FALSE)

  ## -- Start --
  if(name=="model") {
    print_warning_block()
    print("Can not use 'model' as a name")
    stop()
  }

  if(refresh) {
    refresh_master_files(path_to_ras_dbase)
  }

  if(class(AOI_to_map)=="sf") {
    template_hucs <- AOI_to_map
  } else {
    template_hucs <- sf::st_transform(sf::st_read(file.path(path_to_ras_dbase,"HUC8.fgb",fsep=.Platform$file.sep),quiet=FALSE),sf::st_crs("EPSG:6349"))
    template_hucs <- template_hucs[template_hucs$huc8 %in% AOI_to_map,]
  }

  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))
  hull_features <- sf::st_read(file.path(path_to_ras_dbase,"model_footprints.fgb",fsep = .Platform$file.sep)) |> sf::st_make_valid()

  if(plot_lines) {

    xs_features <- sf::st_read(file.path(path_to_ras_dbase,"XS.fgb",fsep = .Platform$file.sep))

    if(!is.null(AOI_to_map)){
      valid_xs_range <- c()
      hull_features <- hull_features[template_hucs,]
      for(hull_i in 1:nrow(hull_features)) {
        valid_xs_range <- append(valid_xs_range,seq(hull_features[hull_i,]$start_master_id, hull_features[hull_i,]$end_master_id))
      }

      xs_features <- xs_features[xs_features$master_id %in% valid_xs_range,]
    }

    if(chart_lines) {
      data_concat <- arrow::read_parquet(file.path(path_to_ras_dbase,"point_database.parquet",fsep = .Platform$file.sep),as_data_frame = TRUE)
      create_xs_plot <- function(database,id) {
        # z Plots
        # plot <- ggplot(data = database[database$master_id==id,], aes(xid_d, z, color = z))+
        #   geom_point()+
        #   theme_light()+
        #   scale_color_gradientn(colors = terrain.colors(10))+
        #   labs(x = "Distance along profile [m]", y = "Elevation [m]", color = "Elevation [m]")

        # N Plots
        plot <- ggplot2::ggplot(data = database[database$master_id==id,], ggplot2::aes(xid_d, z, color = n)) +
          ggplot2::geom_point() +
          ggplot2::theme_light() +
          ggplot2::scale_color_gradientn(colors = terrain.colors(10)) +
          ggplot2::labs(x = "Distance along profile [m] (left to right, looking downstream)", y = "Elevation [m]", color = "Mannings n")
        return(plot)
      }
      popup_charts_for_lines <- lapply(1:nrow(xs_features), function(i) {
        create_xs_plot(data_concat,xs_features[i,]$master_id)
      })

      # create_xs_plot <- function(id) {
      #   # For every site, create the plot below.
      #   data <- data_concat[data_concat$master_id==id,]
      #
      #   # This plot is a plot of water depth over time ~ 3 years
      #   plot <- ggplot2::ggplot(data = data, ggplot2::aes(xid_d, z, color = n)) +
      #     ggplot2::geom_point() +
      #     ggplot2::theme_light() +
      #     ggplot2::scale_color_gradientn(colors = terrain.colors(10)) +
      #     ggplot2::labs(x = "Distance along profile [m] (left to right, looking downstream)", y = "Elevation [m]", color = "Mannings n")
      #   return(plot)
      # }
      # popup_charts_for_lines <- lapply(1:length(unique(xs_lines_to_map$master_id)), function(i) {
      #   create_xs_plot(unique(xs_lines_to_map$master_id)[i])
      # })

      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") |>
        leaflet::addProviderTiles("Stamen.Toner",group = "Stamen.Toner") |>
        leaflet::addProviderTiles("Stamen.Terrain",group = "Stamen.Terrain") |>
        leaflet::addProviderTiles("Esri.WorldStreetMap",group = "Esri.WorldStreetMap") |>
        leaflet::addProviderTiles("Wikimedia",group = "Wikimedia") |>
        leaflet::addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") |>
        leafem::addFeatures(xs_features,
                            fillColor = 'black',
                            popup = leafpop::popupGraph(popup_charts_for_lines,
                                                        width = 400,
                                                        height = 300,
                                                        type = "png"),
                            group = "XS") |>
        leafem::addFeatures(hull_features,opacity = 1,fillOpacity = 0.3,weight = 2,color = 'black', popup = leafpop::popupTable(hull_features),group = "Footprints") |>
        leaflet::addLegend("bottomright",colors = c("black","red"),
                           labels = c(paste0("Cross sections (click for chart)"), paste0("Model footprints")),
                           title = "RAS model database",opacity = 1) |>
        leaflet::addLayersControl(
          baseGroups = c(
            "OpenStreetMap", "Stamen.Toner",
            "Stamen.Terrain", "Esri.WorldStreetMap",
            "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
          ),
          position = "topleft",
          overlayGroups = c("Footprints","XS")
        )
    } else {
      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") |>
        leaflet::addProviderTiles("Stamen.Toner",group = "Stamen.Toner") |>
        leaflet::addProviderTiles("Stamen.Terrain",group = "Stamen.Terrain") |>
        leaflet::addProviderTiles("Esri.WorldStreetMap",group = "Esri.WorldStreetMap") |>
        leaflet::addProviderTiles("Wikimedia",group = "Wikimedia") |>
        leaflet::addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") |>
        leafem::addFeatures(xs_features,
                            Color = 'black',
                            stroke = TRUE,
                            weight = 1.25,
                            opacity = 1,
                            fill = TRUE,
                            # fillColor = 'black',
                            fillOpacity = 0,
                            group = "XS") |>
        leafem::addFeatures(hull_features,opacity = 1,fillOpacity = 0,weight = 2,color = 'black', popup = leafpop::popupTable(hull_features),group = "Footprints") |>
        # leafgl::addGlPolygons(ahulls,fillOpacity = 0,stroke = TRUE,color = 'black',popup="huc10", group = "huc") %>%
        leaflet::addLegend("bottomright",colors = c("black","black"),
                           labels = c(paste0("Cross sections (click for chart)"), paste0("Model footprints")),
                           title = "RAS model database",opacity = 1) |>
        leaflet::addLayersControl(
          baseGroups = c(
            "OpenStreetMap", "Stamen.Toner",
            "Stamen.Terrain", "Esri.WorldStreetMap",
            "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
          ),
          position = "topleft",
          overlayGroups = c("Footprints","XS")
        )
    }
  } else {
    m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") |>
      leaflet::addProviderTiles("Stamen.Toner",group = "Stamen.Toner") |>
      leaflet::addProviderTiles("Stamen.Terrain",group = "Stamen.Terrain") |>
      leaflet::addProviderTiles("Esri.WorldStreetMap",group = "Esri.WorldStreetMap") |>
      leaflet::addProviderTiles("Wikimedia",group = "Wikimedia") |>
      leaflet::addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") |>
      leaflet::addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") |>
      leafem::addFeatures(hull_features,opacity = 1,fillOpacity = 0,weight = 2,color = 'black', popup = leafpop::popupTable(hull_features),group = "Footprints") |>
      leaflet::addLegend("bottomright",colors = c("black"),
                         labels = c(paste0("Model footprints")),
                         title = "RAS model database",opacity = 1) |>
      leaflet::addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Stamen.Toner",
          "Stamen.Terrain", "Esri.WorldStreetMap",
          "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
        ),
        position = "topleft",
        overlayGroups = c("Footprints")
      )
  }

  # Update map
  unlink(file.path(path_to_ras_dbase,name),recursive=TRUE)
  unlink(file.path(path_to_ras_dbase,glue::glue('{name}.html')))
  mapview::mapshot(m,file.path(path_to_ras_dbase,glue::glue('{name}.html')))
  return(TRUE)
}
