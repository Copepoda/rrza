RZA_Plot <- function(RZA,region,facets = TRUE,Year_folder,Cruise_folder){
  #RZA is a RZA dataframe
  #region is either Arctic, Bering Sea, or Gulf of Alaska. The default is Arctic.
  #facets is either a facet plot for the 20BON and 60BON or a single plot for each RZA taxa
  #with the gear type. 
  #Year folder name
  #Cruise folder name
  
  #Packages required to make plots
  
  require(tidyverse)
  require(sf)
  require(viridis)
  require(magrittr)
  require(ggplot2)
  require(measurements)
  require(readxl)

  
  #geom_sf needs the newest version of ggplot2, this checks package version and will spit out   a warning and stop the function.
  
  if(packageVersion("ggplot2") < "3.0.0"){
    stop("You need ggplot2 version 3.0.0 or higher")}
  

  ###This is for testing#####
  #Year_folder <- "2018 RZA"
  #Cruise_folder <- "NW18-01"
  #RZA <- "RZAFile_NW18-01Leg1&2.xlsx"
  #region <- "Northern BS"
  #log_override <- FALSE 
  #log_all <- FALSE
  ##########################
  #Read in the file. This function accepts an .xlxs. Only need to give the file name 
  #in quotes. 
  rza_path <- paste("G:/Rapid Zooplankton Assessment",
                    Year_folder, Cruise_folder,RZA, sep = "/")
  
  #This will clean out all the NA's, using beaker voulume column since beaker volume
  #must be entered. Huge block's of NA's occur due to left over copy pastes of RZA_TAXA
  #blocks. We will need to have the data sheet labeled in the same way, since sheet = 1
  #is problematic. Maybe sheet = "RZA data" .
  RZA <- read_excel(rza_path,sheet = 1)

  
  RZA <- RZA %>% filter(!is.na(BEAKER_VOLUME))
  
  #Convert the lat and long from degrees/minutes/seconds to decimal degrees, if the 
  #LAT and LON columns need converting
  
  if(class(RZA$LON) == "character"){
  RZA <- RZA %>% 
    mutate(LAT = conv_unit(LAT, from = 'deg_dec_min', to = 'dec_deg'))%>%
    mutate(LON = conv_unit(LON, from = 'deg_dec_min', to = 'dec_deg'))%>%
    mutate(LAT = as.numeric(LAT))%>%
    mutate(LON = as.numeric(LON))
  } else {
    RZA
  }
  
  #Bring in the shape files for the Alaska region
  MAP <- st_read(dsn = "G:/Rapid Zooplankton Assessment/RZA R Scripts/RZA Mapping Script/Shape_files_Alaska_dcw",
                 layer = "Alaska_dcw_polygon_Project")
  
  #Tranform into WGS84 coordinate system
  MAP <- st_transform(MAP, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #200 meter bathymetery contour
  BATH_200 <- st_read(dsn = "G:/Rapid Zooplankton Assessment/RZA R Scripts/RZA Mapping Script/Bathy_200_m",
                      layer = "ne_10m_bathymetry_K_200")
  BATH_200 <- st_transform(BATH_200, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #Coordinate limits for each of the three areas
  Ar_xlim <- c(-170,-153)#Arctic
  BS_xlim <- c(-174,-159)#BASIS Grid
  GA_xlim <- c(-168,-148)#GOA
  IS_xlim <- c(-176,-160)#70 meter isobath
  NB_xlim <- c(-172,-163)#Northern Bering Sea
  MACE2018_xlim <- c(-180,-170)#MACE2018 Survey
  
  region_xlim <- if(region == "Arctic"){
    Ar_xlim
  } else if (region == "BASIS"){
    BS_xlim
  } else if (region == "70 meter"){
    IS_xlim
  } else if (region == "Northern BS"){
    NB_xlim
  } else if (region == "MACE 2018"){
    MACE2018_xlim
  }  else {GA_xlim} #Where region == "Gulf of Alaska"
  
  Ar_ylim <- c(66,73)#Arctic
  BS_ylim <- c(54,60)#BASIS Grid
  GA_ylim <- c(52,60)#GOA
  IS_ylim <- c(53,63)#70 meter isobath
  NB_ylim <- c(59,66)#Northern Bering Sea
  MACE2018_ylim <- c(58,62)#MACE2018 Survey
  
  region_ylim <-  if(region == "Arctic"){
    Ar_ylim
  } else if (region == "BASIS"){
    BS_ylim
  } else if (region == "70 meter"){
    IS_ylim
  } else if (region == "Northern BS"){
    NB_ylim
  } else if (region == "MACE 2018"){
    MACE2018_ylim
  } else {GA_ylim} #Where region == "Gulf of Alaska"
  
  ######
  
  #Taxa names for labels
  
  RZA_taxa_names <- c('Euphausiids_GT15' = "Euphausiids > 15mm",
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
  
  
  
  
  plot_path <- paste("G:/Rapid Zooplankton Assessment", Year_folder,"Plots/",sep = "/")
  
  name_20BON_plot <- paste(plot_path,"RZA_",unique(RZA$CRUISE),"_20BON.png", sep = "")
  name_60BON_plot <- paste(plot_path,"RZA_",unique(RZA$CRUISE),"_60BON.png", sep = "")
  
 
  
  if(facets == TRUE){
    
    breaks_20 <- as.integer(range(RZA %>% filter(GEAR_NAME == "20BON")%>%
                                    filter(EST_NUM_PERM3 > 0)%>%
      collect %$% as.vector(log10(EST_NUM_PERM3)), na.rm = TRUE))
    
    
    Plot_20BON <- ggplot()+
      geom_sf(color = "black", data = BATH_200[3], alpha = 0)+
      geom_sf(fill ="#a7ad94", color = "black", data = MAP[1])+
      coord_sf(xlim = region_xlim, ylim = region_ylim)+
      geom_point(aes(LON,LAT, color = log10(EST_NUM_PERM3)), size = 6,
                 data = RZA %>% filter(GEAR_NAME == "20BON") %>% filter(EST_NUM_PERM3 > 0))+
      geom_point(aes(LON,LAT), size = 4, shape = 4,
                 data = RZA %>% filter(GEAR_NAME == "20BON") %>% filter(EST_NUM_PERM3 == 0))+
      scale_color_viridis(option = "viridis", name = expression(paste("# ","m"^"-3")),
                          breaks = c(seq(from = breaks_20[1], to = breaks_20[2], by = 1)),
                          labels = c(10^seq(from = breaks_20[1], to = breaks_20[2], by = 1)))+
      scale_x_continuous(breaks = seq(region_xlim[1],(region_xlim[2] - 2), by = 4))+
      scale_y_continuous(breaks = seq(region_ylim[1],region_ylim[2], by = 2))+
      theme_bw()+
      xlab(label = "Longitude")+
      ylab(label = "Latitude")+
      theme(
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 18),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold",size = 16),
        legend.text = element_text(face = "bold", size = 16),
        strip.background = element_blank())+
      facet_wrap(~ RZA_TAXA,labeller = labeller(RZA_TAXA = RZA_taxa_names),nrow = 1)
    
    
    breaks_60 <- as.integer(range(RZA %>% filter(GEAR_NAME == "60BON") %>% 
                                    filter(EST_NUM_PERM3 > 0)%>%
                                    collect %$% as.vector(log10(EST_NUM_PERM3)), na.rm = TRUE))
    
    Plot_60BON <- ggplot()+
      geom_sf(color = "black", data = BATH_200[3], alpha = 0)+
      geom_sf(fill ="#a7ad94", color = "black", data = MAP[1])+
      coord_sf(xlim = region_xlim, ylim = region_ylim)+
      geom_point(aes(LON,LAT, color = log10(EST_NUM_PERM3)), size = 6,
                 data = RZA %>% filter(GEAR_NAME == "60BON") %>% filter(EST_NUM_PERM3 > 0))+
      geom_point(aes(LON,LAT), size = 4, shape = 4,
                 data = RZA %>% filter(GEAR_NAME == "60BON") %>% filter(EST_NUM_PERM3 == 0))+
      scale_color_viridis(option = "viridis", name = expression(paste("# ","m"^"-3")),
                          breaks = c(seq(from = breaks_60[1], to = breaks_60[2], by = 1)),
                          labels = c(10^seq(from = breaks_60[1], to = breaks_60[2], by = 1)))+
      scale_x_continuous(breaks = seq(region_xlim[1],(region_xlim[2] - 2), by = 4))+
      scale_y_continuous(breaks = seq(region_ylim[1],region_ylim[2], by = 2))+
      theme_bw()+
      xlab(label = "Longitude")+
      ylab(label = "Latitude")+
      theme(
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 18),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold",size = 16),
        legend.text = element_text(face = "bold", size = 16),
        strip.background = element_blank())+
      facet_wrap(~RZA_TAXA,labeller = labeller(RZA_TAXA = RZA_taxa_names),nrow = 3)
    
    
    png(filename = name_20BON_plot, width = 1200, height = 400, units = "px",
        bg = "transparent") 
    
    print(Plot_20BON)
    
    dev.off()
    
    png(filename = name_60BON_plot, width = 1200, height = 1200, units = "px",
        bg = "transparent") 
    
    print(Plot_60BON)
    
    dev.off()
    
  } else {
    
    rza_taxa <- unique(RZA$RZA_TAXA)
    
    for(i in 1:length(rza_taxa)){
      
      name_taxa_plot <- paste(plot_path,"RZA_",unique(RZA$CRUISE), "_",
                              rza_taxa[i], ".png", sep = "")
      
      #For TESTING###
      #i <- 1
      #Check to see if the EST_NUM_PERM3 needs to be logged. 
      #max_est <- RZA %>% filter(RZA_TAXA == rza_taxa[i]) %>%
        #collect %$% as.vector(EST_NUM_PERM3)
      
      #EST_color <- if(log_override == FALSE & max(max_est, na.rm = TRUE) > 50){
      #  "log10(EST_NUM_PERM3)"
     # }else if(log_override == FALSE & max(max_est, na.rm = TRUE) < 50){
       # "EST_NUM_PERM3"
     # }else if(log_override == TRUE & log_all == FALSE){
      #  "EST_NUM_PERM3"
     # }else{
      #  "log10(EST_NUM_PERM3)"
      #  }

      
      #Legend_name <- if(log_override == FALSE & max(max_est, na.rm = TRUE) > 50){
      #  expression(paste("#","m"^"-3"))
     # }else if (log_override == FALSE & max(max_est, na.rm = TRUE) < 50){
      #  expression(paste("#","m"^"-3"))
      #}else if (log_override == TRUE & log_all == FALSE){
      #  expression(paste("#","m"^"-3"))
     # }else {
     #   expression(paste("#","m"^"-3")) #remember is using these variables use aes_sting
     # }
      
      
      png(filename = name_taxa_plot, width = 400, height = 400, units = "px",
          bg = "transparent") 
      
      breaks <- as.integer(range(RZA %>% filter(RZA_TAXA == rza_taxa[i])%>%
                                        filter(EST_NUM_PERM3 > 0)%>%
                                        collect %$% as.vector(log10(EST_NUM_PERM3)),
                                      na.rm = TRUE))
      
      taxa_breaks <- if(sum(RZA %>% filter(RZA_TAXA == rza_taxa[i])%>%
                            collect %$% as.vector(EST_NUM_PERM3),na.rm = TRUE) == 0){
        
                      taxa_breaks <- seq(from = -1,to = 1, by = 1)
        
                    }else if(abs(diff(breaks)) > 1){
        
                      taxa_breaks <- seq(from = breaks[1],to = breaks[2], by = 1)
                      
                    }else if(abs(diff(breaks)) == 1){
                      taxa_breaks <- seq(from = breaks[1],to = breaks[2], by = 0.5)
                  }
        
        
        
      
      rza_plot <- ggplot()+
        geom_sf(color = "black", data = BATH_200[3], alpha = 0)+
        geom_sf(fill ="#a7ad94", color = "black", data = MAP[1])+
        coord_sf(xlim = region_xlim, ylim = region_ylim)+
        geom_point(aes(LON,LAT, color = log10(EST_NUM_PERM3)), size = 6,
                   show.legend = TRUE,
                   data = RZA %>% filter(RZA_TAXA == rza_taxa[i])%>%
                     filter(EST_NUM_PERM3 > 0))+
        geom_point(aes(LON,LAT), size = 4, shape = 4,
                   data = RZA %>% filter(RZA_TAXA == rza_taxa[i])%>% 
                     filter(EST_NUM_PERM3 == 0))+
        scale_color_viridis(option = "viridis", name = expression(paste("# ","m"^"-3")),
                            breaks = taxa_breaks,
                            labels = format(signif(10^taxa_breaks, digits = 1),
                                     scientific = FALSE, trim = TRUE))+
        scale_x_continuous(breaks = seq(region_xlim[1],(region_xlim[2] - 2), by = 4))+
        scale_y_continuous(breaks = seq(region_ylim[1],region_ylim[2], by = 2))+
        ggtitle(label = RZA_taxa_names[names(RZA_taxa_names) == rza_taxa[i]])+
        xlab(label = "Longitude")+
        ylab(label = "Latitude")+
        theme_bw()+
        theme(
          title = element_text(face = "bold", size = 16),
          axis.text.y = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold", size = 16),
          strip.text = element_text(face = "bold", size = 16),
          legend.title = element_text(face = "bold",size = 16),
          legend.text = element_text(face = "bold", size = 16),
          legend.position = "right",
          strip.background = element_blank())
      
      print(rza_plot)
      
      dev.off()
      
      } 
  }

}



