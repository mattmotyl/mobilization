# Political Segregation and Mobilization

setwd("")

##### Load packages ##### 
library(psych)
library(psy)
library(car)
library(maps)
library(mapproj)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(foreign)
library(magrittr)
library(pxR)
library(reshape)
library(reshape2)
library(maptools)
require(tools)
library(rgdal)
library(shapefiles)
library(XML)
library(grImport)
library(igraph)
library(rgeos)
library(googleVis)
library(RgoogleMaps)
library(PBSmapping)
library(raster)
library(plyr)
library(hbsae)
library(lme4)
library(arm)
library(geonames)
library(RJSONIO)
library(spdep)
library(ppcor)
library(classInt)
library(USAboundaries)
library(RColorBrewer)

##### Load and merge congressional district data ##### 
cd.dir <- "data/districtShapes"
cd.map <- readOGR(dsn=cd.dir, layer="districts114")
cd.map$statechar <- as.character(cd.map$STATENAME)
cd.map$cdchar <- as.numeric(as.character(cd.map$DISTRICT))
cd.map <- cd.map[order(cd.map$statechar, cd.map$cdchar),]
cd.map$statecd <- paste(cd.map$statechar, cd.map$cdchar)
cd.map <- subset(cd.map, statechar != "Alaska" & statechar != "Hawaii")
cd.map$dnumber <- as.character(seq.int(nrow(cd.map)))
row.names(cd.map) <- cd.map$dnumber

cd.pe <- read.csv("data/datpractical.csv", header = T)
cd.pe <- subset(cd.pe, District != "D.C. At-Large" & State != "Alaska" & State != "Hawaii")
cd.pe <- cd.pe[order(cd.pe$State, cd.pe$Code),]
cd.pe$dnumber <- as.character(seq.int(nrow(cd.pe)))
row.names(cd.pe) <- cd.pe$dnumber

cd.map <- spChFIDs(cd.map, cd.map$dnumber)
identical(cd.map$dnumber, cd.pe$dnumber)
cd.map2 <- spCbind(cd.map, cd.pe)

gaincol <- colorRampPalette(brewer.pal(9, "Blues"), space = "Lab")(251)
losscol <- colorRampPalette(brewer.pal(9, "Reds"), space = "Lab")(181)
losscol <- rev(losscol)
lossgaincol <- c(losscol, gaincol)

cd.misfitdemobilization.map <- spplot(cd.map2, "RelativeClintonGainVLossAvgAvg", col.regions = lossgaincol,
                                      colorkey = list(space = "bottom", cex = 0.7, labels = list(labels = c("Clinton Votes Lost","","","","","","Clinton Votes Gained")),        
                                                      width = 2, height=1), 
                                      main=list(label="Predicted Relative Vote Shift",cex=1,family="Segoe"),
                                      par.settings = list(axis.line = list(col = 'transparent')))

cd.misfitdemobilization.map


## For Hawaii only
stateh.dir <- "data/states_21basic/states_21basic"
stateh.map <- readOGR(dsn=stateh.dir, layer="states")
stateh.map <- stateh.map[order(stateh.map$STATE_NAME),]
stateh.map$STATE_NAME <- as.character(stateh.map$STATE_NAME)

stateh.pe2 <- read.csv("data/datpracticalstate.csv")
stateh.pe2$state <- as.character(stateh.pe2$State)

stateh.map <- stateh.map[which(stateh.map$STATE_NAME %in% stateh.pe2$state),]
stateh.pe2 <- stateh.pe2[which(stateh.pe2$state %in% stateh.map$STATE_NAME),]
stateh.map <- stateh.map[order(stateh.map$STATE_NAME),]
stateh.pe2 <- stateh.pe2[order(stateh.pe2$state),]

row.names(stateh.pe2) <- stateh.pe2$state

stateh.map <- spChFIDs(stateh.map, stateh.map$STATE_NAME)
identical(stateh.map$STATE_NAME, stateh.pe2$state)
stateh.map2 <- spCbind(stateh.map, stateh.pe2)

# Keep Hawaii
stateh.map2<-subset(stateh.map2, state == "Hawaii")
stateh.pe2<-subset(stateh.pe2, state == "Hawaii")

cdh.misfitdemobilization.map <- spplot(stateh.map2, "RelativeClintonGainVLossAvgAvg", col.regions = "#FC8868",
                                      colorkey = F, par.settings = list(axis.line = list(col = 'transparent')))

cdh.misfitdemobilization.map


## For Alaska only
statea.dir <- "data/states_21basic/states_21basic"
statea.map <- readOGR(dsn=statea.dir, layer="states")
statea.map <- statea.map[order(statea.map$STATE_NAME),]
statea.map$STATE_NAME <- as.character(statea.map$STATE_NAME)

statea.pe2 <- read.csv("data/datpracticalstate.csv")
statea.pe2$state <- as.character(statea.pe2$State)

statea.map <- statea.map[which(statea.map$STATE_NAME %in% statea.pe2$state),]
statea.pe2 <- statea.pe2[which(statea.pe2$state %in% statea.map$STATE_NAME),]
statea.map <- statea.map[order(statea.map$STATE_NAME),]
statea.pe2 <- statea.pe2[order(statea.pe2$state),]

row.names(statea.pe2) <- statea.pe2$state

statea.map <- spChFIDs(statea.map, statea.map$STATE_NAME)
identical(statea.map$STATE_NAME, statea.pe2$state)
statea.map2 <- spCbind(statea.map, statea.pe2)

# Keep Alaska
statea.map2<-subset(statea.map2, state == "Alaska")
statea.pe2<-subset(statea.pe2, state == "Alaska")

cda.misfitdemobilization.map <- spplot(statea.map2, "RelativeClintonGainVLossAvgAvg", col.regions = "#94C4DE",
                                       colorkey = F, par.settings = list(axis.line = list(col = 'transparent')))

cda.misfitdemobilization.map


##### Load and merge state level data #####
state.dir <- "data/states_21basic/states_21basic"
state.map <- readOGR(dsn=state.dir, layer="states")
state.map <- state.map[order(state.map$STATE_NAME),]
state.map$STATE_NAME <- as.character(state.map$STATE_NAME)

state.pe2 <- read.csv("data/datpracticalstate.csv")
state.pe2$state <- as.character(state.pe2$State)

state.map <- state.map[which(state.map$STATE_NAME %in% state.pe2$state),]
state.pe2 <- state.pe2[which(state.pe2$state %in% state.map$STATE_NAME),]
state.map <- state.map[order(state.map$STATE_NAME),]
state.pe2 <- state.pe2[order(state.pe2$state),]

row.names(state.pe2) <- state.pe2$state

state.map <- spChFIDs(state.map, state.map$STATE_NAME)
identical(state.map$STATE_NAME, state.pe2$state)
state.map2 <- spCbind(state.map, state.pe2)

# Drop Hawaii and Alaska
state.map2<-subset(state.map2, state != "Alaska" & state != "Hawaii")
state.pe2<-subset(state.pe2, state != "Alaska" & state != "Hawaii")

# state flip mapping
# actal red blue map
actualstatevote <- as.vector(state.pe2$ActualClintonLostVotes)
actualstatevote <- ifelse(actualstatevote > 0, -1, actualstatevote)
actualstatevote <- ifelse(actualstatevote < -1, 1, actualstatevote)
actualstatevote <- data.frame(actualstatevote)
names(actualstatevote) <- c("redblue")
row.names(actualstatevote) <- row.names(state.map2)
s.map.actualredblue <- spCbind(state.map2, actualstatevote)

ActualRedBlueMap <- spplot(s.map.actualredblue, "redblue", col="black", 
                         col.regions=colorRampPalette(c("#994c3c","#3b5998"))(2),colorkey = FALSE,
                         main=list(label="Actual Vote",cex=1,family="Segoe"),  
                         par.settings = list(axis.line = list(col = 'transparent')))

# misfit adjusted map (liberal estimate)
predictedstatevotelib <- as.vector(state.pe2$StateFlipExt5)
predictedstatevotelib <- ifelse(predictedstatevotelib == "Yes", 1, actualstatevote$redblue)
predictedstatevotelib <- data.frame(predictedstatevotelib)
names(predictedstatevotelib) <- c("redblue")
row.names(predictedstatevotelib) <- row.names(state.map2)
s.map.predictedredbluelib.state <- spCbind(state.map2, predictedstatevotelib)

PredictedRedBlueMapLib <- spplot(s.map.predictedredbluelib.state, "redblue", col="black", 
                               col.regions=colorRampPalette(c("#994c3c","#3b5998"))(2),colorkey = FALSE,
                               main=list(label="Predicted Vote (Strong Partisan)",cex=1,family="Segoe"),  
                               par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapLib

# Just Alaska (liberal estimate)
PredictedRedBlueMapLibA <- spplot(statea.map2, "RelativeClintonGainVLossAvgAvg", 
                                 col.regions= "#994c3c",colorkey = FALSE,
                                 par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapLibA

# Just Hawaii (liberal estimate)
PredictedRedBlueMapLibH <- spplot(stateh.map2, "RelativeClintonGainVLossAvgAvg", 
                                  col.regions= "#3b5998",colorkey = FALSE,
                                  par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapLibH


# misfit adjusted map (conservative estimate)
predictedstatevotecon <- as.vector(state.pe2$StateFlipAvg1)
predictedstatevotecon <- ifelse(predictedstatevotecon == "Yes", 1, actualstatevote$redblue)
predictedstatevotecon <- data.frame(predictedstatevotecon)
names(predictedstatevotecon) <- c("redblue")
row.names(predictedstatevotecon) <- row.names(state.map2)
s.map.predictedredbluecon.state <- spCbind(state.map2, predictedstatevotecon)

PredictedRedBlueMapCon <- spplot(s.map.predictedredbluecon.state, "redblue", col="black", 
                               col.regions=colorRampPalette(c("#994c3c","#3b5998"))(2),colorkey = FALSE,
                               main=list(label="Predicted Vote (Weak Partisan)",cex=1,family="serif"),  
                               par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapCon

# Just Alaska (conservative estimate)
PredictedRedBlueMapConA <- spplot(statea.map2, "RelativeClintonGainVLossAvgAvg", 
                                  col.regions= "#3b5998",colorkey = FALSE,
                                  par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapConA

# Just Hawaii (conservative estimate)
PredictedRedBlueMapConH <- spplot(stateh.map2, "RelativeClintonGainVLossAvgAvg", 
                                  col.regions= "#994c3c",colorkey = FALSE,
                                  par.settings = list(axis.line = list(col = 'transparent')))
PredictedRedBlueMapConH



totalcdstate.map <- arrangeGrob(arrangeGrob(cda.misfitdemobilization.map, cdh.misfitdemobilization.map, PredictedRedBlueMapConA,
                                            PredictedRedBlueMapConH, PredictedRedBlueMapLibA, PredictedRedBlueMapLibH, nrow = 6),
                            arrangeGrob(cd.misfitdemobilization.map, PredictedRedBlueMapCon, PredictedRedBlueMapLib, nrow = 3),
                            ncol = 2, widths = c(1, 4))

ggsave(file = "plots/Fig3.tiff", totalcdstate.map, device = "tiff", dpi = 600, width = 8, height = 10, units = "in")



