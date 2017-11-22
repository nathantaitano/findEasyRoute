test2Xyz <- testXyz[1,]

# x refers to latitude, y refers to longitude. 
# This is in line with the lat/long order given by the xyz data format.

for(r in 2:nrow(testXyz)){
  if(r %% 3612 < 101 & r %% 3612 > 0){
    test2Xyz <- rbind(test2Xyz, testXyz[r,])
  }
}

# Shouldn't there be some better way to do this?
getAdjs <- function(row, xLen, yLen){
  adjs <- c(nw = NA, n = NA, ne = NA,
            w = NA,          e = NA,
            sw = NA, s = NA, se = NA)
  if(row > xLen){
    adjs$n <- row - xLen
  }
  if(row > xLen & row %% xLen != 1){
      adjs$nw <- adjs$n - 1
  }
  if(row > xLen & row %% xLen != 0){
      adjs$ne <- adjs$n + 1
  }
  if(row %% xLen != 1){
    adjs$w <- row - 1
  }
  if(row %% xLen != 1 & row < (xLen*(yLen-1))){
    adjs$sw <- row - 1 + xLen
  }
  if(row %% xLen != 0){
    adjs$e <- row + 1
  }
  if(row %% xLen != 0 & row < (xLen*(yLen-1))){
    adjs$se <- row + 1 + xLen
  }
  if(row < (xLen*(yLen-1))){
    adjs$s <- row + xLen
  }
  return(adjs)
}

calcKcals <- function(startElev, endElev, dist, weight = 67){
  grade <- (endElev - startElev)/dist
  if(grade < 0){grade <- 0}
  speed <- (26.8*3)*exp(-0.4*(grade)) # http://mtntactical.com/research/walking-uphill-10-grade-cuts-speed-13not-12/
  # Equations & constants below provided by Melissa Joyce McGranahan, who took them from the ACSM website:
  # https://certification.acsm.org/metabolic-calcs
  # Most accurate for speeds of 50 – 100 m/min (1.9-3.7 mph)
  # Also validated this code against the example Melissa gave 
  # 150lb person walking 20 min on flat ground at 3.0 mph, 
  # Melissa calculated 78.8 kcal, rounding for some intermediate steps.
  # calcKcals(0,100,80.4*20, speed = 80.4, weight = 68.18) returned 78.67972 kcal
  # Implemented here by me:
  vo2 <- (0.1*speed) + (1.8 * speed * grade) + 3.5
  kcals <- vo2 * weight / 1000 * 5 * (dist/speed)
  return(kcals)
}

getDiagDist <- function(startElev, endElev, res=30){
  flatDist <- (2 * res^2)^(1/2)
  diagDist <- (flatDist^2 + (endElev - startElev)^2)^(1/2)
  return(diagDist)
}

getHorDist <- function(startElev, endElev, res=30){
  horDist <- (res^2 + (endElev-startElev)^2)^(1/2)
}

findNode <- function(xyzTab, lat, long, xLen){
  xMin <- min(xyzTab[,1])
  xMax <- max(xyzTab[,1])
  yMin <- min(xyzTab[,2])
  yMax <- max(xyzTab[,2])
  degRes <- xyzTab[2,1] - xyzTab[1,1]
  
  if(lat > xMax | lat < xMin | long > yMax | long < yMin){stop("Coordinates are outside map extent")}
  xJump <- round((lat - xMin)/degRes,0)
  yJump <- round((yMax - long)/degRes,0)
  node <- xJump + xLen * yJump
}

dijkstra <- function(xyzFile, oriLat, oriLong, destLat, destLong, fileXLen, res=30){
  xyzDf <- read.table(xyzFile)
  oriNode <- findNode(xyzDf, oriLat, oriLong, fileXLen)
  destNode <- findNode(xyzDf, destLat, destLong, fileXLen)
  
  visited <- c()
  unvisited <- c()
}