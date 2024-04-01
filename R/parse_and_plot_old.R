#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param gfile_path PARAM_DESCRIPTION, Default: NULL
#' @param path_to_ras_dbase PARAM_DESCRIPTION, Default: NULL
#' @param excel_row PARAM_DESCRIPTION, Default: NULL
#' @param units_overwrite PARAM_DESCRIPTION, Default: NULL
#' @param proj_overwrite PARAM_DESCRIPTION, Default: NULL
#' @param vdat_trans PARAM_DESCRIPTION, Default: FALSE
#' @param plots_wanted PARAM_DESCRIPTION, Default: 'g'
#' @param outpath PARAM_DESCRIPTION, Default: NULL
#' @param overwrite PARAM_DESCRIPTION, Default: TRUE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @family post-process
#' @details in dev
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[utils]{glob2rx}}
#'  \code{\link[sf]{st_crs}}
#'  \code{\link[sfheaders]{sf_linestring}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_path}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{CoordSf}}
#'  \code{\link[cowplot]{plot_grid}}, \code{\link[cowplot]{ggdraw}}, \code{\link[cowplot]{draw_label}}, \code{\link[cowplot]{save_plot}}
#' @rdname parse_and_plot_old
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs
#' @importFrom sfheaders sf_linestring
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_light xlab ylab geom_sf
#' @importFrom cowplot plot_grid ggdraw draw_label save_plot

parse_and_plot_old <-function(gfile_path=NULL,
                          path_to_ras_dbase=NULL,
                          excel_row=NULL,
                          units_overwrite=NULL,
                          proj_overwrite=NULL,
                          vdat_trans=FALSE,
                          plots_wanted="g",
                          outpath=NULL,
                          overwrite=TRUE,
                          quiet=FALSE) {
  # sinew::moga(file.path(getwd(),"R/parse_and_plot_old.R"),overwrite = TRUE)
  # devtools::document()
  # pkgdown::build_site(new_process=TRUE)
  #
  # devtools::load_all()
  #
  # gfile_path=NULL
  # path_to_ras_dbase=NULL
  # excel_row=NULL
  # units_overwrite=NULL
  # proj_overwrite=NULL
  # vdat_trans=TRUE
  # full=TRUE
  # outpath=NULL
  # overwrite=TRUE
  # quiet=FALSE
  # gfile_path="J:/Dropbox/root/projects/floodmapping/methods/ras2fim/sample_data/HICKORY CREEK/HICKORY CREEK.g01"
  # proj_overwrite="EPSG:2277"

  ## -- Start --
  if(is.null(outpath)) {
    if(!is.null(gfile_path)) {
      outpath <- dirname(gfile_path)
    } else {
      row = excel_row - 1
      ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep))

      if(ras_catalog_dbase[row,final_name_key]=="") {
        path <- file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[row,inital_scrape_name],glue::glue("{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"),fsep = .Platform$file.sep)
        current_path <- file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[row,inital_scrape_name],fsep = .Platform$file.sep)
      } else {
        path <- file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],glue::glue("{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"),fsep = .Platform$file.sep)
        current_path <- file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],fsep = .Platform$file.sep)
      }
      outpath <- current_path
    }
  }

  if(length(list.files(file.path(outpath,"RRASSLER_images", fsep=.Platform$file.sep),full.names = TRUE,recursive = TRUE))>0) {
    if(overwrite) {
      print("Prior extract found there and overwrite is false")
      return(FALSE)
    }
    unlink(file.path(outpath,"RRASSLER_images",fsep = .Platform$file.sep), recursive=TRUE)
  }
  dir.create(file.path(outpath,"RRASSLER_images",fsep = .Platform$file.sep), showWarnings = FALSE)

  if(!is.null(gfile_path)) {
    dir_of_file <- dirname(gfile_path)
    current_model_name <- gsub('.{4}$', '', basename(gfile_path))

    prj_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.prj$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)

    current_model_units = NA
    current_model_projection = NA
    # current_model_projection = "ESRI:102651"  # current_model_projection = "EPSG:2277"  current_model_projection = "EPSG:26915"
    current_last_modified = as.integer(as.POSIXct(file.info(gfile_path)$mtime))

    for(potential_file in prj_files) {
      # potential_file <- prj_files[1]
      file_text <- read.delim(potential_file, header = FALSE)

      if(any(c('PROJCS','GEOGCS','DATUM','PROJECTION') == file_text)) {
        # print('found a projection')
        current_model_projection = sf::st_crs(potential_file)
      } else if(grepl("SI Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Meter"
      } else if(grepl("English Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Foot"
      }
    }
    # print(glue::glue("before g units:{current_model_units}"))
    if(is.na(current_model_projection) & !is.null(proj_overwrite)) {
      current_model_projection = proj_overwrite
    }
    if(is.na(current_model_units) & !is.null(units_overwrite)) {
      current_model_units = units_overwrite
    }


  } else {
    row = excel_row - 1
    ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep))

    if(ras_catalog_dbase[row,final_name_key]=="") {
      path <- file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[row,inital_scrape_name],glue::glue("{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"),fsep = .Platform$file.sep)
      current_path <- file.path(path_to_ras_dbase,"models","_unprocessed",ras_catalog_dbase[row,inital_scrape_name],fsep = .Platform$file.sep)
    } else {
      path <- file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],glue::glue("{ras_catalog_dbase[row,model_name]}.{ras_catalog_dbase[row,g_file]}.hdf"),fsep = .Platform$file.sep)
      current_path <- file.path(path_to_ras_dbase,"models",ras_catalog_dbase[row,final_name_key],fsep = .Platform$file.sep)
    }
    print(glue::glue("processing {path}"))

    current_model_units = ras_catalog_dbase[row,units]
    current_model_projection = ras_catalog_dbase[row,crs]
    current_last_modified = ras_catalog_dbase[row,last_modified]
  }

  g_extract <- process_ras_g_to_xyz(
    geom_path=gfile_path,
    units=current_model_units,
    proj_string=current_model_projection,
    in_epoch_override=current_last_modified,
    vdat = vdat_trans,
    quiet=FALSE)
  if(!isFALSE(g_extract[[1]])) {
    g_extract[[3]] = sfheaders::sf_linestring(
      obj = g_extract[[1]]
      , x = "x"
      , y = "y"
      # , z = "z"
      , linestring_id = "xid"
      , keep = FALSE
    ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
  }

  if(file.exists(paste0(gfile_path,".hdf"))) {
    hdf_extract <- process_ras_hdf_to_xyz(
      geom_path=paste0(gfile_path,".hdf"),
      units=current_model_units,
      proj_string=current_model_projection,
      in_epoch_override=current_last_modified,
      vdat = vdat_trans,
      quiet=FALSE)

    if(!isFALSE(hdf_extract[[1]])){
      hdf_extract[[3]] = sfheaders::sf_linestring(
        obj = hdf_extract[[1]]
        , x = "x"
        , y = "y"
        # , z = "z"
        , linestring_id = "xid"
        , keep = FALSE
      ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))
    }
  }

  if((!isFALSE(hdf_extract[[1]])) & (!isFALSE(g_extract[[1]]))) {
    number_of_cs <- nrow(hdf_extract[[3]])

    for(id in 1:number_of_cs) {
      # id='4'
      tmp_1 <- g_extract[[1]][,c('xid','xid_d','z')][g_extract[[1]]$xid==id,]
      tmp_1$series <- 1
      tmp_2 <- hdf_extract[[1]][,c('xid','xid_d','z')][hdf_extract[[1]]$xid==id,]
      tmp_2$series <- 2
      df <- merge(
        tmp_1,
        tmp_2,
        by='xid_d', all=TRUE)

      g_plot <- ggplot2::ggplot(df, ggplot2::aes(x=xid_d)) +
        ggplot2::geom_line(data=subset(df, !is.na(z.x)),ggplot2::aes(y = z.x), linewidth=0.43, colour="#fec44f") +
        ggplot2::geom_point(data=subset(df, !is.na(z.x)),ggplot2::aes(y = z.x), colour="#fe9929") +
        ggplot2::theme_light() + ggplot2::xlab("pt_id (m)") + ggplot2::ylab("z (m)")
      ghdf_plot <- ggplot2::ggplot(df, ggplot2::aes(x=xid_d)) +
        ggplot2::geom_line(data=subset(df, !is.na(z.y)),ggplot2::aes(y = z.y), linewidth=0.43, colour="#ec7014") +
        ggplot2::geom_point(data=subset(df, !is.na(z.y)),ggplot2::aes(y = z.y), colour="#cc4c02") +
        ggplot2::theme_light() + ggplot2::xlab("pt_id (m)") + ggplot2::ylab("z (m)")
      union_plot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=subset(df, !is.na(z.x)),ggplot2::aes(x=xid_d,y = z.x), linewidth=0.43, colour="#fec44f") +
        ggplot2::geom_point(data=subset(df, !is.na(z.x)),ggplot2::aes(x=xid_d,y = z.x), colour="#993404") +
        ggplot2::geom_line(data=subset(df, !is.na(z.y)),ggplot2::aes(x=xid_d,y = z.y), linewidth=0.43, colour="#ec7014") +
        ggplot2::theme_light() + ggplot2::xlab("pt_id (m)") + ggplot2::ylab("z (m)")

      glines <- ggplot2::ggplot() +
        ggplot2::geom_sf(data=g_extract[[3]]) +
        ggplot2::geom_sf(data=g_extract[[3]][g_extract[[3]]$xid==id,], col = 'red')
      hdflines <- ggplot2::ggplot() +
        ggplot2::geom_sf(data=hdf_extract[[3]]) +
        ggplot2::geom_sf(data=hdf_extract[[3]][hdf_extract[[3]]$xid==id,], col = 'red')
      mapplot <- cowplot::plot_grid(glines, hdflines,labels = c("G File", "HDF File"), ncol = 1)
      final_base <- cowplot::plot_grid(union_plot, mapplot, ncol = 2, labels =c("Combined", ""))

      title <- cowplot::ggdraw() + cowplot::draw_label(glue::glue("Cross section parsed: {id}"), fontface='bold')
      pair_row <- cowplot::plot_grid(g_plot, ghdf_plot, ncol = 2, labels =c("G", "HDF"))
      plot <- cowplot::plot_grid(title, pair_row, final_base, nrow = 3, labels = c("", "", ""),rel_heights = c(0.2, 1, 1))
      cowplot::save_plot(file.path(outpath,"RRASLER_images",glue::glue("{id}.png"), fsep=.Platform$file.sep), plot)
    }
  }
}
