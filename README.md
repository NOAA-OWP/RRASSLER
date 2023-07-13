
# RRASSLER  

![RRASSLER LOGO](https://github.com/JimColl/RRASSLER/blob/main/man/figures/RRASSLER_knockout.png)

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Description**:  The HEC-RAS model format, both as stand alone models or in archive formats, are incompatable for end users whose use case includes widespread accounting and deployment of that data as inputs into other workflows.  This tool should be deployed to ingest HEC-RAS models into a "HECRAS_model_catalog", a normalized and spatialized representation of those models with the requisite metadata and formatting needed to mesh seamlessly with [national scale hydrofabric efforts and data models](https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html), and applications such as [T-Route](https://github.com/NOAA-OWP/t-route) and [RAS2FIM](https://github.com/NOAA-OWP/ras2fim).

**Introduction**:  There are few hydraulic models as prolific as HEC-RAS, and since it's first named release in 1995 users have created these models using public and private data and countless hours of engineering scrutinization in order to generate the best possible purpose-built representation of the world.  Like any model, some level of input massaging is necessary in order to get the data into the specified mathematical format a model requires.  Like most domain specific software solutions, that massaging was rather, forceful, to the point of permanently altering the shape of those inputs into something that most geospatial data readers are unable to handle.  This creates a great deal of friction both in terms of model accountability and interoperability, particularly when you take the standpoint as a model consumer.   The R based HEC-RAS Wrestler (RRASSLER) is here to mediate that.  

**Ingest logic**:  In order to wrassle that friction, RRASSLER has a few theoretical value judgments that need to be described in order to help understand why it does what it does and how to use it properly.  RRASSLER is focused on making the critical data objects needed to run a HEC-RAS model readily available and accountable as a model (as opposed to as individual data points or large clusters of otherwise "vistigal" data; and with an emphasis on the particular geometric realization of the model).  It is also designed to be a standardized and centralized source of models, regardless of their origin.  For that reason, RRASSLER expects to operate within it's own controlled directory, or "HECRAS_model_catalog".   Users first download and unpack desired models into a temporary location, and then point RRASSLER at that directory and the place which you want to store your catalog.  It will:  
1) Scrape the entire directory structure for any HEC-RAS model projects
2) for each geometric realization of that model (variation of .g##), grab all relevant HEC-RAS related files defined as:  
   - .g## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Geometry definitions  
   - .prj &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Projection (can be non-standard proj4 string defined file)  
   - .prj &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Projection (can be non-standard proj4 string defined file)  
   - .p## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # plan file, used to drive the model  
   - .f## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Steady Flow file. Profile information, flow data and boundary conditions   
   - .h## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Hydraulic Design data file  
   - .v## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Velocity file  
   - .o## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Output file  
   - .r## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # run file for steady flow  
   - .u## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Steady Flow file. Profile information, flow data and boundary conditions  
   - .x## &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # run file for unsteady flow  
   - .dss &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; # Data  
   - .rasmap &nbsp;&nbsp; # output plan  
3) Attempt to place the model in space.  This is done by attempting to parse the g** and g**.hdf files, guess at projections, and pulling and collating the data into an xid-xyz table.  
4) Using that extracted geometry, attempt to create a model footprint (hull).  If that can be constructed, the model was assumed to be correctly placed in space.  The relevant model files are copied to the uniquely parsed "final_model_name_key" folder under the _/models/_ folder.  
5) If the creation of the model hull errors out, the model is placed in the _/models/_unprocessed_ folder for further investigation, correction, and rewrassling.  
6) After all iterations of files are done, RRASSLER will (re)generate a unified source for model footprints, cross sections, and points (now in spatial form) from the HEC-RAS model, and pointers back to the copied source data which remains unaltered.  

**Installation:**  It is recommended that you wait to start using this tool until edge case and accounting is fully ironed out, the final form of these tables is still in flux.  If that doesn't dissuade you, install the development version of RRASSLER from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("JimColl/RRASSLER")
```

Test it by generating your first set of models.
```r
library(data.table)
RRASSLER::ingest_FEMA6_BLE(
  path_to_ras_dbase="G:/data/ras_catalog",
  HUC8="12090301",
  full=TRUE,
  proj_overwrite="EPSG:2277",
  vdat_trans=FALSE,
  quiet=FALSE,
  chatty = TRUE,
  quick_check=TRUE,
  quick_hull = FALSE,
  overwrite = FALSE,
  refresh = TRUE)
```

** Model Catalog:** Users may look and manipulate the _accounting.csv_, and the models underneath, but should avoid manual manipulation of individual files, paths, or other aspects.  RRASSLER scripts will do all needed manipulation, file sorting, and cataloging for you but there is very limited error checking should the workflow be applied incorrectly.  As a general rule of thumb, once you've selected your destination for the catalog, only manipulate the csv, preferably _never_ with EXCEL, which may chew up data along edge cases.

**Dependencies:**  As a modern R package this should include all needed dependencies nessasssary to spin this off in your own environment.  Several of the steps rely on [sp](https://cran.r-project.org/web/packages/sp/sp.pdf), whihc has been depreciated and needs to be replaced.



**Data & Attributes:**

> [in line with Next Gen hydrofabric parameters](https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html)

Footprints - model_footprints.fgb

Cross sections - XS.fgb

points
points - point_database.parquet
pt_id	Identifier, unique to cs_id, that increases from left band to right bank	cs_pts
pt_measure	Percent along the transect from left bank (0-1)	cs_pts
relative_dist	Distance of cs_id, starting at 0, in meters	cs_pts
X	X coordinate in CRS of geometry	cs_pts
Y	Y coordinate in CRS of geometry	cs_pts
Z	Z coordinate (in meters) in CRS of geometry	cs_pts
Z_source	Source of elevation data	cs_pts
roughness	(Optional) Estimated Manning's Roughness value	cs_pts

Attribute	Description	layers
hy_id	A hydrofabric specfic, globaly unique flowpath/flowline identifier	flowlines, transects, cs_pts
geometry	Simple Features Geometry (LINESTRING)	flowlines, transects
cs_source	Source of transect information	transects
cs_id	Identifier, unique to a hy_id, that increases from the inlet to the outlet	transects, cs_pts
cs_measure	Percent along the flowpath from inlet	transects
cs_length	Width (in meters) of cross section transect	transects

RAS Source extensions:
**mean_shift**: the amount of space added to normalize a cross section's station-elevation to geographic distance.  Positive indicates that distance was added to the station-elevation series to make it match to the line (length) as it was specified in the geography.  This is reported as a running sum.
**roughness**: (Optional) status is now (Required)

**Use cases:**
## I am both an archivist/model creator and a RRASSLER user

You will unfortunately have to keep two copies of the data.  RRASSLER isn't creating anything you don't already have in the archive in one form or another, and completely removes all metadata and formatting that your archive has so painstakingly created.  Don't change your workflow, consider RRASSLER a "post-processing" step to your archiving work.  

**Known Limitations**: Aligning the different model surfaces is hard.  Although every effort was made to account for standard edge cases and unit cohesion, you will, more often than not, find that a surface you use and a model do not align.  That is not particularly surprising, but it is often disconcerting.  3DEP timestamps, resolutions, and even order of reprojection operations may alter the surfaces slightly, even if they are stated to have come from the same input database.  Do your own sanity checks and try not to lose your mind.  Finally, this was developed, tested, and deployed over primarily 1D data.  Although 2D model will ingest, there was no consideration for those and is not accounting or copying _.tif_ files so the value of these models is greatly diminished. 

**Getting help:**  Feel free to open issues, PR's, or ping the developers following [CONTRIBUTING Guidelines](CONTRIBUTING.md).

**Open source licensing info**
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)