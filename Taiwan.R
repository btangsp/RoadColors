#-----------you need to install the following packages. this only needs to be done once.
#install.packages(c('sf', 'foreign', 'tidyverse', 'stringi', 'lwgeom', 'digest'))
#install.packages(c('showtext'))

#-----------initialize libraries. This needs to be done for each new R session 
library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)
library(stringi)
library(showtext)
options(stringsAsFactors = FALSE)

#-----------download files
#pick a region and download/unzip the .shp.zip file: http://download.geofabrik.de/

#-----------set the working directory to wherever you unzipped the downloaded files to
setwd("/Users/Zangetsu/Documents/Coding Projects/RoadColors/taiwan-latest-free.shp")

font_paths("/Users/Zangetsu/Documents/Coding Projects/RoadColors")

font_add("日本高速公路字体", regular = "日本高速公路字体.otf")

font_families()

#-----------set some basic info about the city you're mapping
city <- "taipei"
lat <- 25.037520 #center point latitude
long <- 121.509816 #center point longitude
rad <- 20000 #radius, in meters, around the center point to map
crs <- 7564 #ESRI projection for mapping. I am using: https://spatialreference.org/ref/sr-org/7564/


#-----------set up the road types you want to plot and what colors they should be
plottypes <-  c('路', '街', '巷', '隧道', '高速公路', '段', '弄', '線')
plotcolors <-  c('路' = '#59c8e5', '街' = '#fed032', '巷' = '#4cb580', '隧道' = '#fe4d64', '高速公路' = '#0a7abf',
                 '段' = '#2e968c', '弄' = '#fe9ea5', '線' = '#fe9ea5', 'Motorway' = "#ff9223", 'Other' = '#cccccc')


#-----------get to plotting
#import  road geography
filename <- "gis_osm_roads_free_1"
allroads <- read_sf(".", filename)

#subset the roads into a circle.
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  st_transform(crs) 
circle <- st_buffer(pt, dist = rad)
circle <- circle %>% st_transform(st_crs(allroads))
allroads <- st_intersection(circle, allroads)

#remove unnamed footpaths
allroads <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]

#add in length 
allroads$len <- st_length(allroads)

#-----derive road suffixes-----
#run this line if your suffixes are at the END of the name (e.g. Canal Street)

allroads$TYPE <- substr(allroads$name, nchar(allroads$name),  nchar(allroads$name)) %>% stri_trans_general(id = "Title")

#run this line if your "suffixes" are at the BEGINNING of the name (e.g. Calle de los Gatos)
#allroads$TYPE <- substr(allroads$name, 1,  str_locate(allroads$name, " ")[, 1] -1)  %>% stri_trans_general(id = "Title")   #for road prefixes

#--------uncomment and run this code to get the top roads by length.
#--------i usually run this to decide what road types to plot
#plottype <- allroads %>% select(TYPE,len)
#plottype$geometry <- NULL
#plottype <- subset(plottype, !is.na(TYPE))
#plottype <- plottype %>% group_by(TYPE) %>% summarise(Length = sum(len)) %>% arrange(-Length) 


#rename motorways that don't have some other designation
#allroads$TYPE[allroads$fclass == 'motorway' & !(allroads$TYPE %in% plottypes)] <- "Motorway"

#put other roads into their own dataframe
allroads$TYPE[!(allroads$TYPE %in% plottypes) & allroads$TYPE != ''] <- "Other"
otherroads <- allroads[(allroads$TYPE  == "Other"),]
allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

showtext_auto()

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent"), text = element_text(family = "wqy-microhei")) + 
  geom_sf(data=otherroads, size = .8, aes(color=TYPE)) + 
  geom_sf(data=allroads, size =1, aes(color=TYPE)) + 
  scale_color_manual(values = plotcolors, guide = "legend")

ggsave(paste0(".", city, ".png"), plot = last_plot(),
       scale = 1, width = 24, height = 36, units = "in",
       dpi = 500)

showtext_auto(FALSE)