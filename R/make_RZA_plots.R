#' @title Abundance Maps for Rapid Zooplankton Assesment taxa.
#' @description This function takes an RZA .xlxs dataframe and will make either
#' individual plots of each RZA taxa group or two faceted maps based on 150
#' and 500 micron mesh. The maps plot abundance per cubic meter of the
#' rza taxa for each station. The map regions are standardized for each of the
#' major cruises/grids for EcoFOCI.
#' @param rza_path The path to the directory where the .xlxs rza dataframe
#' is located along with the .xlxs file name. This is a dataframe specifically
#' formated for rza data entry.
#' @param region There are six geographic regions which produce standardized
#' maps. The six standardized maps can be set to Arctic, Northern BS, 70 meter,
#' BASIS, GOA, and MACE 2018.
#' @param facets By default facets is set to TRUE. When set to TRUE the
#' function will produce two sets of faceted maps. One of the 60 bongo rza
#' taxa and one of the 20 bongo rza taxa. When facets is set to FALSE the
#' function will produce a map for each rza taxa.
#' @return Either faceted maps or individual maps of rza taxa abundance.
#' @export make_rza_plots
#' @name rrza

make_rza_plots <- function(rza_path, region, facets = TRUE){

# reads in the first sheet in the .xlxs rza sheet------------------------------
  rza <- readxl::read_excel(rza_path,sheet = 1)

# removes NA or non entererd beaker volumes to avoid function errors-----------
  rza <- rza %>% dplyr::filter(!is.na(BEAKER_VOLUME))

# Latitude and longitude need to be in decimal degrees. Figures out if that is
# already the case and will convert if it is necessary.------------------------

   if(class(rza$LON) == "character"){
     rza <- rza %>%
        dplyr::mutate(LAT = measurements::conv_unit(LAT,
                                                from = 'deg_dec_min',
                                                to = 'dec_deg'))%>%
        dplyr::mutate(LON = measurements::conv_unit(LON,
                                                from = 'deg_dec_min',
                                                to = 'dec_deg'))%>%
        dplyr::mutate(LAT = as.numeric(LAT))%>%
        dplyr::mutate(LON = as.numeric(LON))
  } else {
    rza
  }

# Bring in shape files from extdata within package and correct projection------
  map <- sf::st_read(dsn = "inst/extdata",
                     layer = "Alaska_dcw_polygon_Project",
                     quiet = TRUE)%>%
    sf::st_transform(., "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Bring in 200 meter isobath and corrects projection---------------------------
  bath_200m <- sf::st_read(dsn = "inst/extdata",
                      layer = "ne_10m_bathymetry_K_200",
                      quiet = TRUE)%>%
    sf::st_transform(., "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Defines the latitudinal limits for each region------------------------------
  ar_xlim <- c(-170,-153)#Arctic
  bs_xlim <- c(-174,-159)#BASIS Grid
  ga_xlim <- c(-168,-148)#GOA
  is_xlim <- c(-176,-160)#70 meter isobath
  nb_xlim <- c(-172,-163)#Northern Bering Sea
  mace2018_xlim <- c(-180,-170)#MACE2018 Survey

# Determines which xlim will be used based on user input-----------------------
  region_xlim <- if(region == "Arctic"){
    ar_xlim
  } else if (region == "BASIS"){
    bs_xlim
  } else if (region == "70 meter"){
    is_xlim
  } else if (region == "Northern BS"){
    nb_xlim
  } else if (region == "MACE 2018"){
    mace2018_xlim
  } else if (region == "GOA"){
    ga_xlim
  } else {
    stop(paste("The region",region,"is not an option. The choices are
                  Arctic, Northern BS, 70 meter, BASIS, GOA, and MACE 2018."))
  }

# Defines the longitudinal limits for each region------------------------------
  ar_ylim <- c(66,73)#Arctic
  bs_ylim <- c(54,60)#BASIS Grid
  ga_ylim <- c(52,60)#GOA
  is_ylim <- c(53,63)#70 meter isobath
  nb_ylim <- c(59,66)#Northern Bering Sea
  mace2018_ylim <- c(58,62)#MACE2018 Survey

# Determines which ylim will be used based on user input-----------------------
  region_ylim <-  if(region == "Arctic"){
    ar_ylim
  } else if (region == "BASIS"){
    bs_ylim
  } else if (region == "70 meter"){
    is_ylim
  } else if (region == "Northern BS"){
    nb_ylim
  } else if (region == "MACE 2018"){
    mace2018_ylim
  } else if (region == "GOA"){
    ga_xlim
  }


# Makes nice names for the plots-----------------------------------------------
  rza_taxa_names <- c('Euphausiids_GT15' = "Euphausiids > 15mm",
                      'Copepods_GT2' = "Large Copepods",
                      'Euphausiids_LT15' = "Euphausiids < 15mm",
                      'Naked_Pteropods' = "Naked Snails",
                      'Chaetognaths' = "Chaetognaths",
                      'Amphipods' = "Amphipods",
                      'Decapods' = "Decapod larvae",
                      'Other_60BON' = "Other Large",
                      'Copepods_LT2' = "Small Copepods",
                      'Shelled_Pteropods' = "Pelagic Snail",
                      'Other_20BON' = "Other small")


# Check for plot folder and make if it doesn't exist---------------------------

  if(dir.exists(paste(rza_path,"/plots",sep = "")) == FALSE){

    dir.create(paste(rza_path,"/plots",sep = ""))
  }

# path to where the plots will be written--------------------------------------


  remove <- length(stringr::str_count(unlist(stringr::str_split(rza_path,"/"))))

  folder_path <- stringr::str_c(unlist(stringr::str_split(
      rza_path,"/"))[-c(remove -1, remove)], collapse = "/")

  plot_path <- paste(folder_path, "plots/", sep = "/")

# name for each of the faceted plots-------------------------------------------
  name_20BON_plot <- paste(plot_path,"RZA_",unique(rza$CRUISE),
                           "_20BON.png", sep = "")
  name_60BON_plot <- paste(plot_path,"RZA_",unique(rza$CRUISE),
                           "_60BON.png", sep = "")


#
  if(facets == TRUE){

    breaks_20 <- as.integer(range(rza %>% dplyr::filter(GEAR_NAME == "20BON")%>%
                                    dplyr::filter(EST_NUM_PERM3 > 0)%>%
      dplyr::collect %$% as.vector(log10(EST_NUM_PERM3)), na.rm = TRUE))


    plot_20bon <- ggplot2::ggplot()+
      ggplot2::geom_sf(color = "black", data = bath_200m[3], alpha = 0)+
      ggplot2::geom_sf(fill ="#a7ad94", color = "black", data = map[1])+
      ggplot2::coord_sf(xlim = region_xlim, ylim = region_ylim)+
      ggplot2::geom_point(ggplot2::aes(LON,LAT, color = log10(EST_NUM_PERM3)), size = 6,
                 data = rza %>% dplyr::filter(GEAR_NAME == "20BON") %>%
                   dplyr::filter(EST_NUM_PERM3 > 0))+
      ggplot2::geom_point(ggplot2::aes(LON,LAT), size = 4, shape = 4,
                 data = rza %>% dplyr::filter(GEAR_NAME == "20BON") %>%
                   dplyr::filter(EST_NUM_PERM3 == 0))+
      viridis::scale_color_viridis(option = "viridis",
                                   name = expression(paste("# ","m"^"-3")),
                                   breaks = c(seq(from = breaks_20[1],
                                         to = breaks_20[2], by = 1)),
                                   labels = c(10^seq(from = breaks_20[1],
                                            to = breaks_20[2], by = 1)))+
      ggplot2::scale_x_continuous(breaks = seq(region_xlim[1],
                                               (region_xlim[2] - 2), by = 4))+
      ggplot2::scale_y_continuous(breaks = seq(region_ylim[1],
                                               region_ylim[2], by = 2))+
      ggplot2::theme_bw()+
      ggplot2::xlab(label = "Longitude")+
      ggplot2::ylab(label = "Latitude")+
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(face = "bold", size = 12),
        axis.text.x = ggplot2::element_text(face = "bold", size = 12),
        axis.title = ggplot2::element_text(face = "bold", size = 18),
        strip.text = ggplot2::element_text(face = "bold", size = 18),
        legend.title = ggplot2::element_text(face = "bold",size = 16),
        legend.text = ggplot2::element_text(face = "bold", size = 16),
        strip.background = ggplot2::element_blank())+
      ggplot2::facet_wrap(~ RZA_TAXA,
                          labeller = ggplot2::labeller(RZA_TAXA = rza_taxa_names),
                          nrow = 1)


    breaks_60 <- as.integer(range(rza %>% dplyr::filter(GEAR_NAME == "60BON") %>%
                                    dplyr::filter(EST_NUM_PERM3 > 0)%>%
                                    dplyr::collect %$%
                                    as.vector(log10(EST_NUM_PERM3)),
                                    na.rm = TRUE))

    plot_60bon <- ggplot2::ggplot()+
      ggplot2::geom_sf(color = "black", data = bath_200m[3], alpha = 0)+
      ggplot2::geom_sf(fill ="#a7ad94", color = "black", data = map[1])+
      ggplot2::coord_sf(xlim = region_xlim, ylim = region_ylim)+
      ggplot2::geom_point(ggplot2::aes(LON,LAT, color = log10(EST_NUM_PERM3)), size = 6,
                 data = rza %>% dplyr::filter(GEAR_NAME == "60BON") %>%
                   dplyr::filter(EST_NUM_PERM3 > 0))+
      ggplot2::geom_point(ggplot2::aes(LON,LAT), size = 4, shape = 4,
                 data = rza %>% dplyr::filter(GEAR_NAME == "60BON") %>%
                   dplyr::filter(EST_NUM_PERM3 == 0))+
      viridis::scale_color_viridis(option = "viridis",
                                   name = expression(paste("# ","m"^"-3")),
                          breaks = c(seq(from = breaks_60[1],
                                         to = breaks_60[2], by = 1)),
                          labels = c(10^seq(from = breaks_60[1],
                                         to = breaks_60[2], by = 1)))+
      ggplot2::scale_x_continuous(breaks = seq(region_xlim[1],
                                               (region_xlim[2] - 2), by = 4))+
      ggplot2::scale_y_continuous(breaks = seq(region_ylim[1],
                                               region_ylim[2], by = 2))+
      ggplot2::theme_bw()+
      ggplot2::xlab(label = "Longitude")+
      ggplot2::ylab(label = "Latitude")+
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(face = "bold", size = 12),
        axis.text.x = ggplot2::element_text(face = "bold", size = 12),
        axis.title = ggplot2::element_text(face = "bold", size = 18),
        strip.text = ggplot2::element_text(face = "bold", size = 18),
        legend.title = ggplot2::element_text(face = "bold",size = 16),
        legend.text = ggplot2::element_text(face = "bold", size = 16),
        strip.background = ggplot2::element_blank())+
      ggplot2::facet_wrap(~RZA_TAXA,
                          labeller = ggplot2::labeller(RZA_TAXA = rza_taxa_names),
                          nrow = 3)


    grDevices::png(filename = name_20BON_plot, width = 1200,
                   height = 400, units = "px", bg = "transparent")

    print(plot_20bon)

    dev.off()

    grDevices::png(filename = name_60BON_plot, width = 1200,
                   height = 1200, units = "px", bg = "transparent")

    print(plot_60bon)

    dev.off()

  } else {

# Individual plots-------------------------------------------------------------
    rza_taxa <- unique(rza$RZA_TAXA)

    for(i in 1:length(rza_taxa)){

      name_taxa_plot <- paste(plot_path,"RZA_",unique(rza$CRUISE), "_",
                              rza_taxa[i], ".png", sep = "")


      breaks <- range(rza %>% dplyr::filter(RZA_TAXA == rza_taxa[i])%>%
                                        dplyr::filter(EST_NUM_PERM3 > 0)%>%
                                        dplyr::collect %$% as.vector(log10(EST_NUM_PERM3)),
                                      na.rm = TRUE)

      taxa_breaks <- if(sum(rza %>% dplyr::filter(RZA_TAXA == rza_taxa[i])%>%
                            dplyr::collect %$% as.vector(EST_NUM_PERM3),
                            na.rm = TRUE) == 0){

                      seq(from = -1,to = 1, by = 1)

                    }else if(abs(diff(breaks)) == 0){

                      breaks[1]

                    }else if(abs(diff(breaks)) > 0 & abs(diff(breaks)) < 0.5){

                      seq(from = breaks[1],to = breaks[2], by = 0.1)

                    }else if(abs(diff(breaks)) >= 0.5 & abs(diff(breaks)) < 1){

                      seq(from = breaks[1],to = breaks[2], by = 0.25)

                    }else if(abs(diff(breaks)) >= 1 & abs(diff(breaks)) < 2){

                      seq(from = breaks[1],to = breaks[2], by = 0.5)

                    }else if(abs(diff(breaks)) >= 2 & abs(diff(breaks)) < 3){

                      seq(from = breaks[1],to = breaks[2], by = 0.75)

                    }else if(abs(diff(breaks)) >= 3 & abs(diff(breaks)) < 4){

                      seq(from = breaks[1],to = breaks[2], by = 1)

                    }else if(abs(diff(breaks)) >= 4){

                      seq(from = breaks[1],to = breaks[2], by = 2)

                    }




      rza_plot <- ggplot2::ggplot()+
        ggplot2::geom_sf(color = "black", data = bath_200m[3], alpha = 0)+
        ggplot2::geom_sf(fill ="#a7ad94", color = "black", data = map[1])+
        ggplot2::coord_sf(xlim = region_xlim, ylim = region_ylim)+
        ggplot2::geom_point(ggplot2::aes(LON,LAT), size = 4, shape = 4,
                            show.legend = TRUE,
                            data = rza %>% dplyr::filter(RZA_TAXA == rza_taxa[i])%>%
                              dplyr::filter(EST_NUM_PERM3 == 0))+
        ggplot2::geom_point(ggplot2::aes(LON,LAT, color = log10(EST_NUM_PERM3)),
                            size = 6, show.legend = TRUE,
                   data = rza %>% dplyr::filter(RZA_TAXA == rza_taxa[i])%>%
                     dplyr::filter(EST_NUM_PERM3 > 0))+
        ggplot2::scale_color_viridis_c(option = "viridis",
                                     name = expression(paste("# ","m"^"-3")),
                            breaks = taxa_breaks,
                            labels = format(signif(10^taxa_breaks, digits = 1),
                                            scientific = FALSE, trim = TRUE)
                            )+
        ggplot2::scale_x_continuous(breaks = seq(region_xlim[1],
                                                 (region_xlim[2] - 2), by = 4))+
        ggplot2::scale_y_continuous(breaks = seq(region_ylim[1],
                                                 region_ylim[2], by = 2))+
        ggplot2::ggtitle(
          label = rza_taxa_names[names(rza_taxa_names) == rza_taxa[i]])+
        ggplot2::xlab(label = "Longitude")+
        ggplot2::ylab(label = "Latitude")+
        ggplot2::theme_bw()+
        ggplot2::theme(
          title = ggplot2::element_text(face = "bold", size = 16),
          axis.text.y = ggplot2::element_text(face = "bold", size = 12),
          axis.text.x = ggplot2::element_text(face = "bold", size = 12),
          axis.title = ggplot2::element_text(face = "bold", size = 16),
          strip.text = ggplot2::element_text(face = "bold", size = 16),
          legend.title = ggplot2::element_text(face = "bold",size = 16),
          legend.text = ggplot2::element_text(face = "bold", size = 16),
          legend.position = "right",
          strip.background = ggplot2::element_blank())



      grDevices::png(filename = name_taxa_plot,
                     width = 400, height = 400, units = "px",bg = "transparent")

      print(rza_plot)

      grDevices::dev.off()

      }
  }

}



