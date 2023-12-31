% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_ble_lib.R
\name{scrape_ble_lib}
\alias{scrape_ble_lib}
\title{FEMA region 6 BLE data scraper}
\usage{
scrape_ble_lib(
  database_path,
  HUCID,
  is_quiet = FALSE,
  overwrite = overwrite,
  files = "m"
)
}
\arguments{
\item{database_path}{Path to load data into.  NOTE: Must be LOCAL path}

\item{HUCID}{The huc8 ID as a string to try and scrape}

\item{is_quiet}{flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE}

\item{overwrite}{overwrite flag for data already loaded in your path. TRUE to remove and re-download., Default: overwrite}

\item{files}{a string encoding to determine what files to attempt to download, can contain 'm' for models, 's' for spatial data, and 'd' for documentation, Default: 'm'}
}
\value{
The requested BLE data as a scraped zip file
}
\description{
Helper to download relevant BLE data
}
\details{
Scraping data from https://ebfedata.s3.amazonaws.com/ based on pointers from https://webapps.usgs.gov/infrm/estBFE/.  See also the Texas specific dashboard at https://www.arcgis.com/apps/dashboards/1e98f1e511fc40d3b08790a4251a64ee for more BLE base models.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 RRASSLER::scrape_ble_lib(database_path,'12090301',is_quiet = FALSE,overwrite = FALSE,files = "ms")
 }
}
}
\seealso{
\code{\link[sf]{st_transform}}, \code{\link[sf]{st_read}}, \code{\link[sf]{st_crs}}
 \code{\link[httr]{GET}}, \code{\link[httr]{write_disk}}
 \code{\link[glue]{glue}}

Other pre-process: 
\code{\link{shotgun_proj_test}()}
}
\concept{pre-process}
