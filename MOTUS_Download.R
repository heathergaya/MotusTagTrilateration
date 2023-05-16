# install.packages(c("motus", "motusData"), 
#                  repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                            CRAN = 'https://cloud.r-project.org'))

# Load the packages for use
library(motus)
library(motusData)
library(dplyr)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### This next command is looking for data related to the Coweeta project (537) - AKA our VEERs. This will be ALL receivers in the database so may include some false detections from other projects as well. 
sql.motus <- tagme(537, new = FALSE, update = T, dir = "./data/") #new = TRUE only for first time

sql.motus2 <- tagme(545, new = F, update = T, dir = "./data2/") #new = TRUE only for first time
#metadata(sql.motus, 537) #attaches metadata to receivers in Project 537
#metadata(sql.motus2) #metadata
tbl.recvDeps <- tbl(sql.motus, "recvDeps") #creates receiver table via SQL-lite query 
tbl.BotrecvDeps <- tbl(sql.motus2, "recvDeps") #creates receiver table via SQL-lite query 

#Now we grab all the receiver names from our project 
df.serno <- tbl.recvDeps %>% 
  filter(projectID == 537) %>%
  as.data.frame() 

df.bot <- tbl.BotrecvDeps %>% 
  filter(projectID == 545) %>% 
  as.data.frame() 
# loop through each receiver (may take a while)

  receiver1 <- tagme(df.serno[1, "serno"], dir = "./data/", new = F, update = TRUE) #first time, switch new to TRUE
  receiver2 <- tagme(df.serno[2, "serno"], dir = "./data/", new = F, update = TRUE) #first time, switch new to TRUE

  for (row in 1:nrow(df.bot)) {
    tagme(df.bot[row, "serno"], dir = "./data2/", new = FALSE, update = TRUE)
  }
  
Botgar1

tellme(545, dir = "./data2") #

tellme(537) #5346 numGPS
#5603 now!
#12434 as of sept 21
#12498 oct 8
#7473 may 15 2023
#### Ready to look at data ####
allbotgar_act<- 
  tbl(Botgar1, "activity") %>%
  collect() %>% 
  as.data.frame() %>%
  mutate(time = as.POSIXct(hourBin*60*60, origin = '1970-01-01'))
range(allbotgar_act$time)

allbotgar_gps<- 
  tbl(Botgar1, "gps") %>%
  collect() %>% 
  as.data.frame() %>%
  mutate(time = as_datetime(ts))
range(allbotgar_gps$time)
#### Look at receiver data #### 

allbotgar<- 
  tbl(Botgar1, "allTags") %>%
  collect() %>% 
  as.data.frame() %>%
  mutate(time = as_datetime(ts), month = substr(as_datetime(ts), 6, 7)) %>%
  select(speciesID, speciesEN, motusFilter, motusTagID, mfgID, tagDepLat, tagDepLon, deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvDeployName,
         recvSiteName, recvProjID, recvProjName, time, month)


allbotgar <- allbotgar[order(allbotgar$time, decreasing = T),]      

table(allbotgar$motusTagID, allbotgar$month)
  
alltags_r1 <- 
  tbl(receiver1, "allTags") %>%
  collect() %>% 
  as.data.frame() %>%    
  mutate(time = as_datetime(ts), month = substr(as_datetime(ts), 6, 7))%>%
  select(speciesID, speciesEN, motusFilter, motusTagID, mfgID, tagDepLat, tagDepLon, deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvDeployName,
         recvSiteName, recvProjID, recvProjName, time, month)
table(alltags_r1$motusTagID, alltags_r1$month)


alltags_r2 <- 
  tbl(receiver2, "allTags") %>%
  collect() %>% 
  as.data.frame() %>%    
  mutate(time = as_datetime(ts), month = substr(as_datetime(ts), 6, 7)) %>%
  filter(month != "06") %>%
  select(speciesID,speciesEN, motusFilter,  motusTagID, mfgID, tagDepLat, tagDepLon, deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvDeployName,
         recvSiteName, recvProjID, recvProjName, time, month)
table(alltags_r2$motusTagID, alltags_r2$month)


#### Look at tag data #####
tbl.alltags <- tbl(sql.motus, "alltags")
alltags_mytowers <- tbl.alltags %>%
  collect() %>% 
  as.data.frame() %>%    
  mutate(time = as_datetime(ts), month = substr(as_datetime(ts), 6, 7))
#write.csv(alltags_mytowers, "alltags_mytowers.csv")


## 3,343,9863 rows!
##3440080

## Coweeta 2022 has 4 tags deployed (1 was on GAGG and fell off and is ignored)
## 67771 = 55343334 (BuABuBu)
## 67738 = 2A2A7855 (GARY)
## 67773 was GAGG but it fell off, so we did not register it to MOTUS (other towers won't look for it)
## 67827 = 61551E1E (ASY Female, WAWW)
## 67897 = 1966782D (YAYY)

df.alltagsSub_july <- alltags_mytowers %>% 
  filter(month %in% c("08", "09", "10", "11")) %>%
  select(mfgID, motusTagID, fullID, deviceID, recvDeployID, recvDeployLat, recvDeployLon, recv, recvDeployName,
         recvSiteName, recvProjID, recvProjName, time, month)

tagSummary <- df.alltagsSub_july %>%
  group_by(motusTagID) %>% 
  summarize(nDet = n(),
            nRecv = length(unique(recvDeployName)),
            timeMin = min(time),
            timeMax = max(time))

head(tagSummary)

a <- alltags_mytowers %>%
  collect() %>% 
  as.data.frame() %>%    
  mutate(time = as_datetime(ts)) %>%
  filter(time >= as.POSIXct("2022-07-04 01:01:01"))


BBBA <- tbl.alltags %>%
  collect() %>% 
  as.data.frame() %>%
  filter(motusTagID %in% c(67771)) %>% 
  collect() %>% 
  as.data.frame() %>%    
  mutate(time = as_datetime(ts)) %>%
  filter(time >= as.POSIXct("2022-07-04 01:01:01"))
BBBA


trilateration <- function(x) {
  library(dplyr)
  library(nlstools)
  # supress warnings
  options(warn = -1)
  x$Time <- as_datetime(x$tsCorrected)
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
  x1 <- subset(x, x$port %in% c("L2","L3")) ### have to change this depending on setup of your tower
  x2 <- subset(x, x$por == "L1") #for node, change based on setup of tower
  x1$dist <- ay*exp(-sy*x1$sig)+ky
  x2$dist <- an*exp(-sn*x2$sig)+kn
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
    max.RSS <- sub.test[which.max(sub.test$sig),]
    startx <- nodesdata[match(max.RSS$nodeNum, nodesdata$NodeId), "UTMW"]
    starty <- nodesdata[match(max.RSS$nodeNum, nodesdata$NodeId), "UTMN"]
    
    # Calculate number of "nodes" for the test
    no.nodes <- dplyr::n_distinct(sub.test$nodeNum) #includes the tower as a potential "node"
    
    #Determine which nodes are relevant
    
    NodeUTMx <- nodesdata[match(sub.test$nodeNum, nodesdata$NodeId), "UTMW"]
    NodeUTMy <- nodesdata[match(sub.test$nodeNum, nodesdata$NodeId), "UTMN"]
    
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
    estimated.location_results <- estimated.location_results[order(estimated.location_results$day),]
    
  }
  return(estimated.location_results)
}

BBBA_locs <- trilateration(BBBA)
BBBA_locs$Year <- format(as.Date(BBBA_locs$day), "%Y")
BBBA_locs$x.ci.lower_adj <- ifelse(BBBA_locs$x.ci.lower < 274000, 274000, BBBA_locs$x.ci.lower) ## this is just for graphing
BBBA_locs$y.ci.lower_adj <- ifelse(BBBA_locs$y.ci.lower < 3880700, 3880700, BBBA_locs$y.ci.lower)
BBBA_locs$x.ci.upper_adj <- ifelse(BBBA_locs$x.ci.upper > 275000, 275000, BBBA_locs$x.ci.upper)
BBBA_locs$y.ci.upper_adj <- ifelse(BBBA_locs$y.ci.upper > 3881100, 3881100, BBBA_locs$y.ci.upper)
library(ggplot2)

nodesdata <- data.frame(NodeId = c("37a5a4", "3776a4", "3778c1", "377a2f", "", "NA"), 
                        UTMW = c(0274331,0274259,0274453,0274279,0274510,0274510 ), 
                        UTMN = c(3880989, 3880843, 3880839,3880921,3880971,3880971))
ggplot(BBBA_locs, aes(x = x.est, y = y.est))+
  geom_point()+
  #geom_line(linewidth = .1)+
  geom_segment(aes(x = x.ci.lower_adj, yend = y.est, xend = x.ci.upper_adj), col = "grey", linewidth = .05)+ ## uncertainty
  geom_segment(aes(y = y.ci.lower_adj, xend = x.est, yend = y.ci.upper_adj), col = "grey", linewidth = .05)+ ## uncertainty
  geom_point(data = nodesdata, aes(x = UTMW, y = UTMN), col = "blue")+
  facet_wrap(~Year)+
  ylim(3880700, 3881100)+
  xlim(274000, 275000)+
  theme_classic()+
  ggtitle("BuBuBuA VEER F")+
  xlab("UTM W")+
  ylab("UTM N")

### making fancy paths
for(i in 1:nrow(BBBA_locs)){
  if(i == nrow(BBBA_locs)){
    BBBA_locs$next_loc_x[i] <- BBBA_locs$x.est[i]
    BBBA_locs$next_loc_y[i] <- BBBA_locs$y.est[i]
    next
  }
BBBA_locs$next_loc_x[i] <- BBBA_locs$x.est[i+1]
BBBA_locs$next_loc_y[i] <- BBBA_locs$y.est[i+1]

}

mostrecent <- BBBA_locs[nrow(BBBA_locs),]


ggplot(BBBA_locs, aes(x = x.est, y = y.est))+
  geom_point()+
  geom_segment(aes(x = x.est, y = y.est, xend = next_loc_x, yend = next_loc_y), arrow = arrow(length = unit(.15, "cm"), type = "closed"), linewidth = .1, col = "grey50")+
  geom_jitter()+
  geom_line(linewidth = .1)+
  geom_point(data = nodesdata, aes(x = UTMW, y = UTMN), col = "blue", pch = 5)+
  geom_point(data = mostrecent, aes(x = x.est, y = y.est), col = "green", pch = 4)+
  facet_wrap(~Year)+
  theme_classic()+
  ggtitle("BuBuBuA VEER F")+
  xlab("UTM W")+
  ylab("UTM N")


