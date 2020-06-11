setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/envs_data")

install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapproj")
install.packages("mapdata")
install.packages("rgeos")
install.packages("maptools")
install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("dismo")


require(ggplot2)
require(ggmap)
require(maps)
require(mapproj)
require(mapdata)
require(rgeos)
require(maptools)
require(sp)
require(raster)
require(rgdal)
require(dismo)

#making basic maps with ggmap
base = get_map(location=c(-105,0,-60,30), zoom=4, maptype="terrain-background")

map1=ggmap(base)

map1
map1 + geom_point(data=k, aes(x=long, y=lat, fill=site), color="red", cex=2.5)




#load envs data with site coordinates
envs<-read.csv("allsamples_envsdata_04242020.csv", header = TRUE)




#TERRAIN MAP FOR GUATEMALA


base = get_map(location=c(-93,13,-88,18.1), zoom=8, maptype="terrain-background")

map1=ggmap(base)

map1
map1 + geom_point(data=k, aes(x=long, y=lat, fill=site), color="red", cex=2.5, show.legend = FALSE)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#EXPORT IMAGE WITH CLEAR RESOLUTION

tiff("imagekimhpan.tif", res=800, compression = "lzw", height=5, width=5, units="in")


