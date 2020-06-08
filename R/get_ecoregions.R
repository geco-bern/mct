## function from https://gitlab.irstea.fr/georges.kunstler/traitcompet/-/blob/47423e2ce236f0d8a9c8045a3c0496b2744ab1d8/R/utils/ecoregions.R
get_ecoregions <- function(lons, lats, var.extract = 'eco_code'){
  
  require(rgdal)
  require(sp)
  
  #read in ecoregions and transform shapefile to lat/lon
  wwf.ecoregions <- readOGR(dsn = "~/data/biomes/wwf_ecoregions/data/commondata/data0",
                            layer = "wwf_terr_ecos")
  
  geo.proj <- proj4string(wwf.ecoregions)
  
  #create SpatialPoints object for plots
  pts <- sp::SpatialPoints(cbind(lons,lats), proj4string = CRS(geo.proj))
  
  #creates object that assigns each plot index to an ecoregion
  plot.ecocodes <- over(pts, wwf.ecoregions)[[var.extract]]
  
  #now need to lookup closest ecoregion for NAs and Lake
  ec.list <- unique(plot.ecocodes)
  
  #get the subset of ecoregions that have been matched to these plots
  ec.subset <- wwf.ecoregions[wwf.ecoregions[[var.extract]] %in% ec.list,]
  
  ec.na <- which(plot.ecocodes == "Lake" | plot.ecocodes == 98 |
                   is.na(plot.ecocodes))
  
  #get all NAs and Lake regions
  gdis <- gDistance(pts[ec.na,],ec.subset,byid = T)

  #calculate distance from na points to each polygon
  gdis[which(ec.subset[[var.extract]] == "Lake"),] <- 99999
  
  #calculate distance from na points to each polygon
  gdis[which(ec.subset[[var.extract]] == 98),] <- 99999
  
  #exclude lakes by making them seem very far away
  na.ind <- apply(gdis,2,FUN = which.min)
  
  # #find index of minimum distance for each point
  # plot.ecocodes[ec.na] <- ec.subset[[var.extract]][na.ind]
  
  #assign back to ecocodes
  return(plot.ecocodes)
  
} 
