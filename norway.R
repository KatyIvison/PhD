nor <- getData("GADM", country = "NOR", level = 1)
r <- getData("worldclim", res = 5, var = "bio")
r <- r[[c(5,6,12,13,14)]]####this decides to just use bioclim1?
r <- crop(r, nor)
r <- rasterToPolygons(r)
nor <- spTransform(nor, CRSobj = proj4string(r))
r <- r[nor, ]
r1<-c(r$bio5,r$bio6,r$bio12,r$bio13,r$bio14)
View(r1)