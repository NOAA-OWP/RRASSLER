% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cloud_ingest_record.R
\name{cloud_ingest_record}
\alias{cloud_ingest_record}
\title{cloud_ingest_record}
\usage{
cloud_ingest_record(
  in_file = NULL,
  ras_dbase = NULL,
  root_bucket = NULL,
  code_to_place_in_source = NULL,
  proj_override = NULL,
  apply_vdat_trans = FALSE,
  is_quiet = FALSE,
  is_verbose = FALSE,
  quick_check = FALSE,
  quick_hull = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{in_file}{the path to the file on disk that we want to ingest, Default: NULL}

\item{ras_dbase}{A path to an S3 bucket to write your RRASSLED catalog to., Default: NULL}

\item{root_bucket}{first path in s3 url, will auto-populate as needed, Default: NULL}

\item{code_to_place_in_source}{a code to place in the metadata as the owner of the model, Default: NULL}

\item{proj_override}{a string to override projection information should none be found, Default: NULL}

\item{apply_vdat_trans}{Should VDATUM be applied to the HEC-RAS model geometry.  See https://vdatum.noaa.gov/, Default: FALSE}

\item{is_quiet}{flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE}

\item{is_verbose}{flag to determine whether internal print statements (i.e. cross section parsing, vdat trans, file info) are suppressed, TRUE to show these messages and FALSE to suppress them, Default: FALSE}

\item{quick_check}{on initial ingest, if the model name is found we assume the models are the same without fully spatializing them which saves processing time, Default: FALSE}

\item{quick_hull}{a flag to dictate whether the end points of a models cross sections are used or if the entire point database is fed to the hull creation, Default: FALSE}
}
\description{
add a file as a record in the RRASSLED structure
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[stringr]{str_sub}}, \code{\link[stringr]{str_detect}}
 \code{\link[aws.s3]{get_bucket}}, \code{\link[aws.s3]{delete_object}}, \code{\link[aws.s3]{put_object}}
 \code{\link[glue]{glue}}
 \code{\link[utils]{glob2rx}}
 \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{st_write}}
 \code{\link[dplyr]{select}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
 \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{fwrite}}
 \code{\link[sfheaders]{sf_linestring}}
 \code{\link[lwgeom]{st_startpoint}}
 \code{\link[holyhull]{holyhull}}
 \code{\link[nhdplusTools]{get_nhdplus}}
 \code{\link[AOI]{aoi_get}}
 \code{\link[arrow]{write_parquet}}

Other ingest: 
\code{\link{disk_ingest_record}()},
\code{\link{ingest_FEMA6_BLE}()},
\code{\link{ingest_into_database}()}
}
\concept{ingest}
