% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_library.R
\name{map_library}
\alias{map_library}
\title{map_library}
\usage{
map_library(
  path_to_ras_dbase,
  AOI_to_map = NULL,
  name = "model_map",
  plot_lines = FALSE,
  chart_lines = FALSE,
  refresh = FALSE,
  quiet = TRUE
)
}
\arguments{
\item{path_to_ras_dbase}{The path to the folder in which you are building your catalog, Default: NULL}

\item{AOI_to_map}{An AOI pass though to subset the catalog down into a smaller area to add the context too, Default: NULL}

\item{name}{A name for the map files to create, Default: 'model_map'}

\item{plot_lines}{draw lines on the map as opposed to just the footprints, Default: FALSE}

\item{chart_lines}{Add cross section click graphs to the plot lines.  Will break if area is too large, Default: FALSE}

\item{refresh}{flag to dictate whether or not to recollate spatial database prior to mapping.  FALSE to skip, TRUE to regenerate, Default: TRUE}

\item{quiet}{flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
a function to help apply geographic context to the RRASSLED catalog
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE
 RRASSLER::map_library(path_to_ras_dbase = cat_path,NULL,name = "model_map",plot_lines = TRUE,chart_lines = TRUE,refresh = FALSE,quiet = FALSE)
 }
}
}
\seealso{
\code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}, \code{\link[sf]{valid}}
 \code{\link[arrow]{read_parquet}}
 \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{scale_colour_gradient}}, \code{\link[ggplot2]{labs}}
 \code{\link[leaflet]{leaflet}}, \code{\link[leaflet]{addProviderTiles}}, \code{\link[leaflet]{addLegend}}, \code{\link[leaflet]{addLayersControl}}
 \code{\link[leafem]{addFeatures}}
 \code{\link[leafpop]{popupGraph}}
 \code{\link[glue]{glue}}
 \code{\link[mapview]{mapshot}}
}
