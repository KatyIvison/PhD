library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=10)

r <- r[[c(5,6,12,13,14)]]
names(r) <- c("Warmest","Coldest","Prec","Wettest","Driest")

points <- albleb[,3:2]#lat and long are the wrong way round on dataframe

values <- extract(r,points)

df <- cbind.data.frame(coordinates(points),values)
df2<-df[,3:7]
df2<-na.omit(df2)
df2
hvalbleb<-hypervolume(df2,method="gaussian")
library(alphahull)
plot(hvalbleb)
get_volume(hvalbleb)
