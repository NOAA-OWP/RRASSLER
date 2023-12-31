% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catalog_integrity_checks.R
\name{catalog_integrity_checks}
\alias{catalog_integrity_checks}
\title{catalog integrity checks}
\usage{
catalog_integrity_checks(path_to_ras_dbase, quiet = TRUE)
}
\arguments{
\item{path_to_ras_dbase}{The path to the folder in which you are building your catalog, Default: NULL}

\item{quiet}{flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE, Default: TRUE}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
Check the status of the catalog for errors and inconsistencies
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1

 }
}
}
\seealso{
Other post-process: 
\code{\link{append_catalog_fields}()},
\code{\link{dedupe}()},
\code{\link{reprocess_table_row}()},
\code{\link{update_models_from_table}()}
}
\concept{post-process}
