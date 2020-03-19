library(rgdal)
library(ggplot2)
library(mapproj)
library(raster)
library(gridExtra)
library(ggpubr)
############ map of norway current from https://stackoverflow.com/questions/46333336/spatial-data-overlay-selection-in-r#########
nor <- getData("GADM", country = "NOR", level = 1)
r <- getData("worldclim", res = 5, var = "bio")
r <- r[[1]]####this decides to just use bioclim1?
r <- crop(r, nor)
r <- rasterToPolygons(r)
nor <- spTransform(nor, CRSobj = proj4string(r))
r <- r[nor, ]
t <- ggplot()+
  geom_polygon(data=r,aes(x=long,y=lat,group=group, fill =rep(r$bio1,each=5)))
t<-t + xlab("Longitude") + ylab ("Latitude")+scale_fill_continuous(low="yellow",high="red",limits=c(-75,100),name=("Mean Annual Temp (degreesC)"))
t<-t+theme_classic()
#############
##############rcp45 annual mean temp
r45 <- getData("CMIP5", res = 5, var = "bio",model="NO",year=70,rcp=45)
r45<- r45[[1]]
r45 <- crop(r45, nor)
r45 <- rasterToPolygons(r45)
nor <- spTransform(nor, CRSobj = proj4string(r45))
r45 <- r45[nor, ]
t45 <- ggplot()+
  geom_polygon(data=r45,aes(x=long,y=lat,group=group, fill = rep(r45$no45bi701, each = 5)))
t45<-t45 + xlab("Longitude") + ylab ("Latitude")+scale_fill_continuous(low="yellow",high="red",limits=c(-75,100),name=("Mean Annual Temp (degreesC)"))
t45<-t45+theme_classic()
################
############ map of norway annual temp rcp2.6
nor <- getData("GADM", country = "NOR", level = 1)
r26 <- getData("CMIP5", res = 5, var = "bio",model="NO",year=70,rcp=26)
r26 <- r26[[1]]####this decides to just use bioclim1?
r26 <- crop(r26, nor)
r26 <- rasterToPolygons(r26)
nor <- spTransform(nor, CRSobj = proj4string(r26))
r26 <- r26[nor, ]
t26 <- ggplot()+
  geom_polygon(data=r26,aes(x=long,y=lat,group=group, fill = rep(r26$no26bi701,each = 5)))
t26<-t26 + xlab("Longitude") + ylab ("Latitude")+scale_fill_continuous(low="yellow",high="red",limits=c(-75,100),name=("Mean Annual Temp (degreesC)"))
t26<-t26+theme_classic()
#############
plot1<-ggarrange(t,t26,t45,nrow=1,labels=c("Current","RCP2.6","RCP4.5"),label.x=0.1,label.y=0.95,common.legend=TRUE,legend="right")
##### TITLE annotate_figure(plot1, fig.lab = ("Bioclimatic variable 1"), fig.lab.face = "bold", fig.lab.size = 20)####
plot(plot1)
