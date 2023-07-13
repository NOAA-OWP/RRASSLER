#' @title update_models_from_table
#' @description try and reprocess the _unprocessed models
#' @param path_to_ras_dbase The path to the top level folder of the ras_catalog structure, Default: TRUE
#' @param refresh flag to dictate whether or not to recollate spatial database prior to mapping.  FALSE to skip, TRUE to regenerate, Default: TRUE
#' @param quiet flag to determine whether print statements are suppressed, TRUE to suppress messages and FALSE to show them, Default: FALSE
#' @return a reRRASSLED catalog
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringi]{stri_isempty}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{rbindlist}}, \code{\link[data.table]{fwrite}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[sf]{st_crs}}, \code{\link[sf]{st_coordinates}}, \code{\link[sf]{st_as_sf}}, \code{\link[sf]{st_transform}}, \code{\link[sf]{geos_unary}}, \code{\link[sf]{geos_combine}}, \code{\link[sf]{st_write}}
#'  \code{\link[lwgeom]{st_startpoint}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[nhdplusTools]{get_nhdplus}}
#'  \code{\link[AOI]{aoi_get}}
#'  \code{\link[arrow]{write_parquet}}
#' @rdname update_models_from_table
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom stringi stri_isempty
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @importFrom sfheaders sf_linestring
#' @importFrom sf st_set_crs st_crs st_coordinates st_as_sf st_transform st_convex_hull st_union st_write
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet

update_models_from_table <- function(path_to_ras_dbase,refresh=FALSE,quiet=TRUE) {
  # sinew::moga(file.path(getwd(),"R/update_models_from_table.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  # devtools::load_all()
  #
  # path_to_ras_dbase = "H:/ras_dbase"

  ## -- Start --
  ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"accounting.csv",fsep = .Platform$file.sep))

  index_to_update <- ras_catalog_dbase[
    (is.na(ras_catalog_dbase[,final_name_key]) | stringi::stri_isempty(ras_catalog_dbase[,final_name_key])) &
      (!stringi::stri_isempty(ras_catalog_dbase[,units])) &
      (!stringi::stri_isempty(ras_catalog_dbase[,crs])) &
      !(ras_catalog_dbase[,notes] %in% c("unparsed_geo","explosive")),which = TRUE]

  for(i in index_to_update) {
    # i=1
    path <- file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[i,initial_scrape_name],glue::glue("{ras_catalog_dbase[i,model_name]}.{ras_catalog_dbase[i,g_file]}"),fsep = .Platform$file.sep)
    print(glue::glue("processing {path}"))
    if(file.exists(paste0(path,".hdf"))) {
      extrated_pts = process_ras_hdf_to_xyz(
        geom_path=paste0(path,".hdf"),
        units=ras_catalog_dbase[i,units],
        proj_string=ras_catalog_dbase[i,crs],
        in_epoch_override=ras_catalog_dbase[i,last_modified],
        vdat = FALSE,
        quiet=FALSE)
    } else {
      extrated_pts = process_ras_g_to_xyz(
        geom_path=path,
        units=ras_catalog_dbase[i,units],
        proj_string=ras_catalog_dbase[i,crs],
        in_epoch_override=ras_catalog_dbase[i,last_modified],
        vdat = FALSE,
        quiet=FALSE)
    }

    if(!is.data.frame(extrated_pts)) {
      current_initial_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
      current_final_name_key <- paste0("unparsed_",current_model_name,"_",current_g_value,"_",current_last_modified)

      if(sum(stringr::str_detect(ras_catalog_dbase$final_name_key, current_final_name_key)) < 0) {
        new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_initial_name,current_final_name_key,"could not parse")
        names(new_row) <- names
        ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

        dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
        file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))
      } else (
        print(glue::glue("We've seen this model, it's at {ras_catalog_dbase[ras_catalog_dbase$final_name_key==current_final_name_key,]$final_name_key}"))
      )
    }

    ls = sfheaders::sf_linestring(
      obj = extrated_pts[[1]]
      , x = "x"
      , y = "y"
      # , z = "z"
      , linestring_id = "xid"
      , keep = FALSE
    ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

    end_points <- c(ls %>% lwgeom::st_endpoint(), ls %>% lwgeom::st_startpoint())
    end_points <- sf::st_coordinates(end_points) %>%
      as.data.frame() %>%
      dplyr::group_by(X,Y) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(coords = c("X","Y")) %>%
      sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
      sf::st_transform(sf::st_crs("EPSG:4326"))

    ahull_poly = tryCatch(
      expr = {st_alpha_hull(x = sf::st_coordinates(end_points)[,1],y = sf::st_coordinates(end_points)[,2],alpha = 0.1)},
      error = function(e){
        sf::st_convex_hull(sf::st_union(end_points)) %>%
          sf::st_as_sf() %>%
          sf::st_set_crs(sf::st_crs(end_points)) %>%
          sf::st_transform(sf::st_crs("EPSG:6349"))
      }
    )

    # Join to comids
    current_nhdplus_comid = nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly),realization = "flowline")
    if(length(current_nhdplus_comid) == 0){
      ras_catalog_dbase[i,nhdplus_comid := 001]
    } else {
      ras_catalog_dbase[i,nhdplus_comid := current_nhdplus_comid[current_nhdplus_comid$streamorde == max(current_nhdplus_comid$streamorde),][1,]$comid]
    }

    ras_catalog_dbase[i,final_name_key := paste0(ras_catalog_dbase[i,nhdplus_comid],"_",ras_catalog_dbase[i,model_name],"_",ras_catalog_dbase[i,g_file],"_",ras_catalog_dbase[i,last_modified])]

    dir.create(file.path(path_to_ras_dbase,"models",ras_catalog_dbase[i,]$final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE)
    file.copy(list.files(file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[i,]$initial_scrape_name,fsep = .Platform$file.sep), full.names=TRUE, ignore.case=TRUE, recursive=TRUE),
              file.path(path_to_ras_dbase,"models",ras_catalog_dbase[i,]$final_name_key,fsep = .Platform$file.sep))

    unlink(file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[i,]$initial_scrape_name,fsep = .Platform$file.sep),recursive=TRUE)

    arrow::write_parquet(extrated_pts,file.path(path_to_ras_dbase,"models",ras_catalog_dbase[i,]$final_name_key,"ras_xyz.parquet",fsep = .Platform$file.sep))
    sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",ras_catalog_dbase[i,]$final_name_key,"hull.fgb",fsep = .Platform$file.sep))
    data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)
  }

  refresh_master_files(path_to_ras_dbase)

  return(TRUE)
}
