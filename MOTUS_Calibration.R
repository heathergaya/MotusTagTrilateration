#### Step 1, record node locations and tower locations

node_37A5A4 <- c(0274331, 3880989)
node_3776A4 <- c(0274259, 3880843)
node_3778C1 <- c(0274453, 3880839)
Tower <- c(0274510, 3880971)
########## Calibrating Distance via Tag Walk #########
#setwd("~/Desktop/MOTUS Diagram/Calibration") 
Tagwalk <- read.csv("Tag walk.csv") #this is your known track from a GPS unit
library(rgdal)
Tagwalk[,c("UTMW", "UTMN")] <- as.data.frame(spTransform(SpatialPoints(Tagwalk[c("lon", "lat")], proj4string = CRS("+proj=longlat")), CRS("+proj=utm +zone=17 +datum=WGS84")))
Tagwalk$Time <- substr(Tagwalk$time, 12,19)
Tagwalk$min <- as.numeric(format(strptime(Tagwalk$Time, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(Tagwalk$Time, format = "%H:%M:%S"), "%M"))+as.numeric(format(strptime(Tagwalk$Time, format = "%H:%M:%S"), "%S"))*(1/60)

#setwd("~/Desktop/MOTUS Diagram/July8/TagWalkJuly8/uploaded/ctt/2022-07-08")
filelist = list.files(pattern="*.csv$") ## This is grabbing ALL csv files downloaded from CTT website during tag walk. Make sure only csv files in this folder are relevant, otherwise it will try to merge them too 
df_input_list <- lapply(filelist, read.csv)
names(df_input_list) <- gsub(filelist, pattern="\\..*", replacement="")
df_merged1 <- do.call(rbind, df_input_list)

# rawdat1 <- read.csv("CTT-V30B0154E0E1-raw-data.2022-07-08_100524.csv")
# rawdat2 <- read.csv("CTT-V30B0154E0E1-raw-data.csv")
#rawdat <- rbind(rawdat1, rawdat2)
rawdat <- df_merged1
#Tag walked around was 2A2A7855; put in your TagID here 
rawdat <- rawdat[rawdat$TagId == "2A2A7855" & rawdat$Validated == 1,]
rawdat$Time2 <- ifelse(nchar(rawdat$Time) == 19, substr(rawdat$Time, 12,19), substr(rawdat$Time, 8,12))

rawdat$min <- ifelse(nchar(rawdat$Time) == 19, (as.numeric(format(strptime(rawdat$Time2, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(rawdat$Time2, format = "%H:%M:%S"), "%M"))+as.numeric(format(strptime(rawdat$Time2, format = "%H:%M:%S"), "%S"))*(1/60)), as.numeric(format(strptime(rawdat$Time2, format = "%H:%M"), "%H"))*60+as.numeric(format(strptime(rawdat$Time2, format = "%H:%M"), "%M")))
rawdat <- rawdat[rawdat$min >= min(Tagwalk$min),] ## cut out all detection data not associated with your tag walk
rawdat <- rawdat[nchar(rawdat$Time) == 19, ]

for(i in 1:nrow(rawdat)){
  rows <- c(max(which(Tagwalk[,"min"] <= rawdat[i,"min"])), min(which(Tagwalk[,"min"] >= rawdat[i,"min"])))
  time_dif <- abs(rawdat[i,"min"]-Tagwalk[rows,"min"])
  ifelse(sum(time_dif) == 0, weight <- c(.5,.5), weight <- 1- time_dif/sum(time_dif))
  rawdat$UTMW[i] <- (Tagwalk[rows[1], "UTMW"])*weight[1] + (Tagwalk[rows[2], "UTMW"])*weight[2]
  rawdat$UTMN[i] <- (Tagwalk[rows[1], "UTMN"])*weight[1] + (Tagwalk[rows[2], "UTMN"])*weight[2] 
} #this is figuring out where you actually were during various points and averaging your location each minute

rawdat$day <- substr(rawdat$Time, 1, 10)

Tag2 <-  df_merged1[df_merged1$TagId == "4C662A66",] #conveniently we opened this tag at a net on july 8 and then didn't end up deploying it, so we can use it for calibration
Tag2$Time2 <- ifelse(nchar(Tag2$Time) == 19, substr(Tag2$Time, 12,19), substr(Tag2$Time, 8,12))
Tag2$min <- as.numeric(format(strptime(Tag2$Time2, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(Tag2$Time2, format = "%H:%M:%S"), "%M"))
Tag2$UTMW <- 274272
Tag2$UTMN <- 3880948
Tag2$day <- substr(Tag2$Time, 1, 10)

plot(Tag2$min, Tag2$TagRSSI)
##### Did a second tag walk July 12 #####
#setwd("~/Desktop/MOTUS Diagram/Calibration")
Tagwalk2 <- read.csv("Tagwalk2.csv")
library(rgdal)
Tagwalk2[,c("UTMW", "UTMN")] <- as.data.frame(spTransform(SpatialPoints(Tagwalk2[c("lon", "lat")], proj4string = CRS("+proj=longlat")), CRS("+proj=utm +zone=17 +datum=WGS84")))
Tagwalk2$Time <- substr(Tagwalk2$time, 12,19)
Tagwalk2$min <- as.numeric(format(strptime(Tagwalk2$Time, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(Tagwalk2$Time, format = "%H:%M:%S"), "%M"))+as.numeric(format(strptime(Tagwalk2$Time, format = "%H:%M:%S"), "%S"))*(1/60)

Tag3 <- read.csv("Tagwalk2_plus_extra.csv")
rawdat2 <- Tag3[Tag3$TagId != "2A2A7855" & Tag3$Validated == 1,]
rawdat2$Time2 <- substr(rawdat2$Time, 12,19)


rawdat2$min <- (as.numeric(format(strptime(rawdat2$Time2, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(rawdat2$Time2, format = "%H:%M:%S"), "%M"))+as.numeric(format(strptime(rawdat2$Time2, format = "%H:%M:%S"), "%S"))*(1/60))
rawdat2 <- rawdat2[rawdat2$min >= min(Tagwalk2$min) & rawdat2$min <= max(Tagwalk2$min),]
rawdat2 <- rawdat2[nchar(rawdat2$Time) == 19, ]

for(i in 1:nrow(rawdat2)){
  rows <- c(max(which(Tagwalk2[,"min"] <= rawdat2[i,"min"])), min(which(Tagwalk2[,"min"] >= rawdat2[i,"min"])))
  time_dif <- abs(rawdat2[i,"min"]-Tagwalk2[rows,"min"])
  ifelse(sum(time_dif) == 0, weight <- c(.5,.5), weight <- 1- time_dif/sum(time_dif))
  rawdat2$UTMW[i] <- (Tagwalk2[rows[1], "UTMW"])*weight[1] + (Tagwalk2[rows[2], "UTMW"])*weight[2]
  rawdat2$UTMN[i] <- (Tagwalk2[rows[1], "UTMN"])*weight[1] + (Tagwalk2[rows[2], "UTMN"])*weight[2] 
}

rawdat2$day <- substr(rawdat2$Time, 1, 10)




rawdat <- rbind(rawdat, Tag2, rawdat2)

#rawdat$minute <- as.numeric(format(strptime(rawdat$Time2, format = "%H:%M:%S"), "%H"))*60+as.numeric(format(strptime(rawdat$Time2, format = "%H:%M:%S"), "%M"))
rawdat$detID <- paste(rawdat$min, rawdat$day)
mod.dat <- rawdat %>% 
  dplyr::group_by(NodeId, detID, RadioId, TagId)%>% ## Now we can grab the RSSI for each detection, sorted by nodes vs. tower
  dplyr::summarise(avgRSS = mean(TagRSSI),
                   UTMW = mean(UTMW),
                   UTMN = mean(UTMN))

ch2 <- mod.dat[mod.dat$RadioId == 2, ] #downslope CTT antenna; adjust for your own setup 
ch3 <- mod.dat[mod.dat$RadioId == 3, ] #upslope ; adjust for your own setup 
ch1 <- mod.dat[mod.dat$RadioId == 1, ] #omni; adjust for your own setup 

for(i in 1:nrow(ch2)){
ch2$dist[i] <- dist(matrix(c(Tower, ch2[i,c("UTMW", "UTMN")]), nrow =2, byrow = T))
}
for(i in 1:nrow(ch3)){
  ch3$dist[i] <- dist(matrix(c(Tower, ch3[i,c("UTMW", "UTMN")]), nrow =2, byrow = T))
}

plot(ch2$dist,ch2$avgRSS, ylim = c(-70, 0))
points(ch3$dist,ch3$avgRSS, col = "blue")

yagis <- rbind(ch2,ch3)
# mod.dat <- yagis %>%
#   dplyr::group_by(NodeId, detID, RadioId)%>%
#   dplyr::summarise(avgRSS = median(TagRSSI),
#                    UTMW = median(UTMW),
#                    UTMN = median(UTMN),
#                    dist = median(dist))
# points(mod.dat$dist,mod.dat$avgRSS, col = "green")

exp.mod <- nls(avgRSS ~ SSasymp(dist, Asym, R0, lrc), data = yagis)
S <- exp(coef(exp.mod)[["lrc"]]) #decay factor
K <- coef(exp.mod)[["Asym"]] #horizontal asymptote 
a <- coef(exp.mod)[["R0"]] #intercept when RSSI = 0
nls.mod <- nls(avgRSS ~ a * exp(-S * dist) + K, start = list(a = a, S = S, K= K), 
               data = yagis)
coef(nls.mod)
plot(function(x) coef(nls.mod)[1]*exp(-coef(nls.mod)[2]*x)+coef(nls.mod)[3], from = 0, to = 250, ylim = c(-100, 0)  )
points(yagis$dist,yagis$avgRSS, col = "blue", pch = 19, cex = .5)


#a = 37.246281758   s= 0.006170729 k = -63.636587062   for yagis; PUT THIS INFO IN TRILATERATION EQUATION

### Time to calibrate for each node ####
## First get distances from nodes; make sure these are accurate for your own nodes
node_37A5A4 <- c(0274331, 3880989)
node_3776A4 <- c(0274259, 3880843)
node_3778C1 <- c(0274453, 3880839)
node_377A2F <- c(0274279, 3880921)
for(i in 1:nrow(ch1)){
  ifelse(ch1$NodeId[i] == "3778c1", n <- node_3778C1,
         ifelse(ch1$NodeId[i] == "37a5a4", n <- node_37A5A4,
                ifelse(ch1$NodeId[i] == "377a2f", n <- node_377A2F, n <- node_3776A4)))
  ch1$dist[i] <- dist(matrix(c(n, ch1[i,c("UTMW", "UTMN")]), nrow =2, byrow = T))
}

#testing -  37a5a4 giving weird results
ch1 <- ch1[ch1$NodeId != "37a5a4",]



library(ggplot2)
ggplot(ch1, aes(x = dist, y = avgRSS))+
  geom_point(aes(col = NodeId))+
  xlim(0,400)+
  ylim(-125, -50)

exp.mod_ch1 <- nls(avgRSS ~ SSasymp(dist, Asym, R0, lrc), data = ch1)
S_1 <- exp(coef(exp.mod_ch1)[["lrc"]]) #decay factor
K_1 <- coef(exp.mod_ch1)[["Asym"]] #horizontal asymptote 
a_1 <- coef(exp.mod_ch1)[["R0"]] #intercept when RSSI = 0
nls.mod_ch1 <- nls(avgRSS ~ a * exp(-S * dist) + K, start = list(a = a_1, S = S_1, K= K_1), 
               data = ch1)
coef(nls.mod_ch1)
plot(function(x) coef(nls.mod_ch1)[1]*exp(-coef(nls.mod_ch1)[2]*x)+coef(nls.mod_ch1)[3], from = 0, to = 400, ylim = c(-200, 0))
points(ch1$dist,ch1$avgRSS, col = "blue", pch = 19, cex = .25)

# a = 24.175848813    s = 0.003550618 k = -108.849075591    for nodes
#a = 37.246281758   s= 0.006170729 k = -63.636587062   for yagis 


#### Test the distances out using the trilateration function
testy1 <- rawdat[(nchar(rawdat$Time)==19 & rawdat$TagId == "4C662A66"), ] #didn't move
testy2 <- rawdat[(nchar(rawdat$Time)==19 & rawdat$TagId == "2A2A7855"), ] #did move


library(dplyr)
library(nlstools)
#### Trilateration function based on RAW CTT DATA ######
### Use trilateration function in "MOTUS_Download.R" for motus downloaded data - the column names are slightly different if you download from MOTUS vs. from CTT website
trilateration <- function(x) {
  library(dplyr)
  library(nlstools)
  
  # supress warnings
  options(warn = -1)
  x$NodeId <- ifelse(is.na(x$NodeId), "", x$NodeId)
  x$HM <- substr(x$Time, 12,16)
  x$day <- substr(x$Time, 1, 10)
  x$min_loc <- paste(as.numeric(format(strptime(x$HM, format = "%H:%M"), "%H"))*60+as.numeric(format(strptime(x$HM, format = "%H:%M"), "%M")), as.character(x$day))
  
  #determine estimate distance from node/tower that detected it
  nodesdata <- data.frame(NodeId = c("37a5a4", "3776a4", "3778c1", "377a2f", "", "NA"), 
                          UTMW = c(0274331,0274259,0274453,0274279,0274510,0274510 ), 
                          UTMN = c(3880989, 3880843, 3880839,3880921,3880971,3880971))
  # node_37A5A4 <- c(0274331, 3880989)
  # node_3776A4 <- c(0274259, 3880843)
  # node_3778C1 <- c(0274453, 3880839)
  # node_377A2F <- c(0274279, 3880921) #### moved this one july 11
  # Tower <- c(0274510, 3880971)
  # a = 24.175848813    s = 0.003550618 k = -108.849075591    for nodes
  #a = 37.246281758   s= 0.006170729 k = -63.636587062   for yagis 
  #nodes
  an <- 24.175848813
  sn <- 0.003550618
  kn <- -108.849075591
  #yagis
  ay = 37.246281758
  sy = 0.006170729
  ky = -63.636587062
  x1 <- subset(x, x$RadioId %in% c(2,3))
  x2 <- subset(x, x$RadioId == 1)
  x1$dist <- ay*exp(-sy*x1$TagRSSI)+ky
  x2$dist <- an*exp(-sn*x2$TagRSSI)+kn
  x <- rbind(x1, x2)
  x$dist <- ifelse(x$dist <= 0, 0, x$dist)
  # make a vector of unique trilaterations to run
  tests = unique(x$min_loc)
  
  # Make a dataframe with only 1 row per test
  
  test.UTM <- x %>%
    dplyr::group_by(min_loc) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(min_loc)
  
  # Create a dataframe for output estimates
  estimated.location_results <- data.frame(min_loc=character(), No.Nodes = numeric(), x.est=numeric(), y.est=numeric(), 
                                           x.ci.lower =numeric(), x.ci.upper =numeric(), y.ci.lower = numeric(), y.ci.upper = numeric())
  
  
  for(j in 1:length(tests)) {
    
    # Isolate the test 
    sub.test <- x %>% dplyr::filter(min_loc == tests[j]) 
    
    #mod.dat <- sub.test %>%
    #  dplyr::group_by(NodeId, min_loc) %>%
    #  dplyr::summarise(avgRSS = mean(TagRSSI))
    
    # Determine the node with the strongest RSS value
    max.RSS <- sub.test[which.max(sub.test$TagRSSI),]
    startx <- nodesdata[match(max.RSS$NodeId, nodesdata$NodeId), "UTMW"]
    starty <- nodesdata[match(max.RSS$NodeId, nodesdata$NodeId), "UTMN"]
    
    # Calculate number of "nodes" for the test
    no.nodes <- dplyr::n_distinct(sub.test$NodeId) #includes the tower as a potential "node"
    
    #Determine which nodes are relevant
    
    NodeUTMx <- nodesdata[match(sub.test$NodeId, nodesdata$NodeId), "UTMW"]
    NodeUTMy <- nodesdata[match(sub.test$NodeId, nodesdata$NodeId), "UTMN"]
    
    if(no.nodes == 1) {
      estimated.loc <- data.frame(TestId = tests[j], No.Nodes = no.nodes, x.est = NodeUTMx[1], y.est = NodeUTMy[1], 
                                  x.ci.lower = NA, x.ci.upper = NA,  y.ci.lower = NA, y.ci.upper = NA, Hour_min = sub.test$HM[1], day = sub.test$day[1])
      estimated.location_results <- rbind(estimated.location_results, estimated.loc)
      next } 
    
    # To deal with potential errors where the model fails due to bad starting values using tryCatch everything you want evaluated by tryCatch goes inside {},
    # then the error will be printed but the loop will continue
    
    # Non-linear test to optimize the location of unknown signal by looking at the radius around each Node based on estimated distance and the pairwise distance between all nodes
    tryCatch( {nls.test <- nls(dist ~ raster::pointDistance(data.frame(NodeUTMx, NodeUTMy), c(NodeUTMx_solution, NodeUTMy_solution), lonlat = F, allpairs = T),
                               data = sub.test, start=list(NodeUTMx_solution=startx, NodeUTMy_solution=starty),
                               control=nls.control(warnOnly = T, minFactor=1/30000, maxiter = 100)) # gives a warning, but doesn't stop the test from providing an estimate based on the last iteration before the warning
    
    
    
    # Determine an error around the point location estimate
    par.est = cbind(coef(nls.test), confint2(nls.test))
    lng.ci.upper =  par.est[1,3] 
    lng.ci.lower =  par.est[1,2]
    lat.ci.upper =  par.est[2,3] 
    lat.ci.lower =  par.est[2,2]}
    
    ,error = function(e)  {cat("ERROR :",conditionMessage(e), j, "\n")})
    
    # estimated location of the point and error
    estimated.loc <- data.frame(TestId = tests[j], No.Nodes = no.nodes, x.est = par.est[1,1], y.est = par.est[2,1], 
                                x.ci.lower = lng.ci.lower, x.ci.upper = lng.ci.upper,  y.ci.lower = lat.ci.lower, y.ci.upper = lat.ci.upper, Hour_min = sub.test$HM[1], day = sub.test$day[1])
    #, realW = mean(sub.test$UTMW), realN = mean(sub.test$UTMN)) #for testing 
    
    # Populate dataframe with results
    estimated.location_results <- rbind(estimated.location_results, estimated.loc)
    
  }
  return(estimated.location_results)
}
test_run <- trilateration(testy1)
plot(test_run$x.est, test_run$y.est)
plot(Tagwalk$UTMW, Tagwalk$UTMN, col = "green", pch = 19, cex =.5)
#points(testy$UTMW, testy$UTMN, cex = .5, col = "blue")
points(test_run$x.est, test_run$y.est)

test_run2 <- trilateration(testy2)
plot(test_run2$x.est, test_run2$y.est)
plot(Tagwalk$UTMW, Tagwalk$UTMN, col = "green", pch = 19, cex =.5)
points(testy2$UTMW, testy2$UTMN, cex = .5, col = "blue")
points(test_run2$x.est, test_run2$y.est)

#### Some more data testing #####
setwd("~/Desktop/MOTUS Diagram/July11/uploaded/ctt/2022-07-09")
filelist1 = list.files(pattern="*.csv$")
df_input_list1 <- lapply(filelist1, read.csv)
names(df_input_list1) <- gsub(filelist1, pattern="\\..*", replacement="")
df_merged_j9 <- do.call(rbind, df_input_list1)
setwd("~/Desktop/MOTUS Diagram/July8/TagWalkJuly8/uploaded/ctt/2022-07-08")
filelist = list.files(pattern="*.csv$")
df_input_list <- lapply(filelist, read.csv)
names(df_input_list) <- gsub(filelist, pattern="\\..*", replacement="")
df_merged1 <- do.call(rbind, df_input_list)
setwd("~/Desktop/MOTUS Diagram/July72022/untitled folder 2/uploaded/ctt/2022-07-07")
filelist2 = list.files(pattern="*.csv$")
df_input_list2 <- lapply(filelist2, read.csv)
names(df_input_list2) <- gsub(filelist2, pattern="\\..*", replacement="")
df_merged2 <- do.call(rbind, df_input_list2)
setwd("~/Desktop/MOTUS Diagram/July72022/untitled folder 2/uploaded/ctt/2022-07-05")
filelist3 = list.files(pattern="*.csv$")
df_input_list3 <- lapply(filelist3, read.csv)
names(df_input_list3) <- gsub(filelist3, pattern="\\..*", replacement="")
df_merged3 <- do.call(rbind, df_input_list3)
setwd("~/Desktop/MOTUS Diagram/July11/uploaded/ctt/2022-07-10")
filelist4 = list.files(pattern="*.csv$")
df_input_list4 <- lapply(filelist4, read.csv)
names(df_input_list4) <- gsub(filelist4, pattern="\\..*", replacement="")
df_merged4 <- do.call(rbind, df_input_list4)
setwd("~/Desktop/MOTUS Diagram/July11/uploaded/ctt/2022-07-11")
filelist5 = list.files(pattern="*.csv$")
df_input_list5 <- lapply(filelist5, read.csv)
names(df_input_list5) <- gsub(filelist5, pattern="\\..*", replacement="")
df_merged5 <- do.call(rbind, df_input_list5)


df_merged <- rbind(df_merged_j9, df_merged1, df_merged2, df_merged3, df_merged4, df_merged5)
df_merged <- subset(df_merged, Validated == 1)
unique(df_merged$TagId)

nodesdata <- data.frame(NodeId = c("37a5a4", "3776a4", "3778c1", "377a2f", "", "NA"), 
                        UTMW = c(0274331,0274259,0274453,0274279,0274510,0274510 ), 
                        UTMN = c(3880989, 3880843, 3880839,3880921,3880971,3880971))

Female_veer1 <- trilateration(df_merged[df_merged$TagId == "61551E1E",])
Female_veer1 <- Female_veer1[order(Female_veer1$day),]
Fveer1 <- Female_veer1[!is.na(Female_veer1$x.ci.lower),]
plot(nodesdata$UTMW, nodesdata$UTMN, pch = 19)
lines(Female_veer1$x.est, Female_veer1$y.est, col= "grey90")
lines(Fveer1$x.est, Fveer1$y.est, col= "blue")
points(Female_veer1$x.est[903], Female_veer1$y.est[903])

male_1 <- df_merged[df_merged$TagId == "1966782D",]
# a = 24.175848813    s = 0.003550618 k = -108.849075591    for nodes
#a = 44.637261166  s = 0.003830399 k -72.462686443   for yagis 
for(i in 1:nrow(male_1)){
  ifelse(male_1$RadioId %in% c(2,3), male_1$dist[i] <- 44.637261166*exp(-0.003830399*male_1$TagRSSI[i])-72.462686443 ,
         male_1$dist[i] <- 24.175848813 *exp(-0.003550618*male_1$TagRSSI[i])-108.849075591)
  ifelse(male_1$dist[i] <0, male_1$dist[i] <- 0, male_1$dist[i] <- male_1$dist[i])
}

plot(1:nrow(male_1), male_1$dist, type = "l")

male_veer1 <- trilateration(male_1)
plot(nodesdata$UTMW, nodesdata$UTMN, pch = 19)
lines(male_veer1$x.est, male_veer1$y.est, col= "blue")

testytag <- trilateration(df_merged[df_merged$TagId == "2A2A7855",])
plot(nodesdata$UTMW, nodesdata$UTMN, pch = 19)
lines(testytag$x.est, testytag$y.est, col= "blue")
points(testytag$x.est, testytag$y.est)

GAGG <- trilateration(df_merged[df_merged$TagId == "4C662A66",])
lines(GAGG$x.est, GAGG$y.est, col= "blue")

#### Forest Service Tower 
setwd("~/Desktop/MOTUS Diagram/MOTUS_july25_FStower")
filelist = list.files(pattern="*.csv$")
df_input_list <- lapply(filelist, read.csv)
names(df_input_list) <- gsub(filelist, pattern="\\..*", replacement="")
df_merged <- do.call(rbind, df_input_list)
#df_merged <- subset(df_merged, df_merged$Validated == 1)

mytags <- c("1966782D","4C662A66", "2A2A7855",  "61551E1E","55343334" )

plot(1:length(df_merged$TagRSSI), df_merged$TagRSSI, type = "l")

setwd("~/Desktop/MOTUS Diagram/MotusJuly28")
filelist = list.files(pattern="*.csv$")
df_input_list <- lapply(filelist, read.csv)
names(df_input_list) <- gsub(filelist, pattern="\\..*", replacement="")
df_merged <- do.call(rbind, df_input_list)
#df_merged <- subset(df_merged, df_merged$Validated == 1)

mytags <- c("1966782D","4C662A66", "2A2A7855",  "61551E1E","55343334" )

plot(1:length(df_merged$TagRSSI), df_merged$TagRSSI, type = "l")
