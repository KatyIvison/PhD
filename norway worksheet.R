library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(sf)

#Using Zonal statistics
poly <- shapefile("E:\\PhD\\Durham PhD\\GloNAF GBIF\\Hypervolumes\\Norway_shapefile_unzipped\\no_10km.shp")

r<-getData("GADM",country="NOR",level=1)
r<-poly_to_raster(r,nrow=NA,ncol=NA)
hasValues(r)
#This will take considerable time
ex <- extract(r, poly, fun='mean', na.rm=TRUE, df=TRUE) 

write.csv(cbind(poly$NAME_0,ex),"Worldclim.csv", row.names = F)


# using centroids
nc <- st_read(dsn="Provide_your_drive_name" e.g. "F:\\Kriging in R\\India Shape files", layer="2011_Dist")
# just view the attributes & first 6 attribute values of the data
head(nc)
sp_cent <- gCentroid(as(nc, "Spatial"), byid = TRUE)
values <- extract(r,sp_cent)

write.csv(cbind(as.data.frame(nc$DISTRICT),as.data.frame(values)),"Worldclim_centroids.csv", row.names = F)
