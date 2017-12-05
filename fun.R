# x refers to latitude, y refers to longitude. 
# This is in line with the lat/long order given by the xyz data format.

# Just a modulo statement for extracting a subset square from an xyz gis dataframe, 
# which itself was read directly in with read.table()
# This gave me a 100x100 square at ~30m resolution in xyz format (test2Xyz)
# To use in testing the functions while I was building them
# for(r in 2:nrow(testXyz)){
#   if(r %% 3612 < 101 & r %% 3612 > 0){
#     test2Xyz <- rbind(test2Xyz, testXyz[r,])
#   }
# }

# Shouldn't there be some better way to do this?
getAdjs <- function(row, xLen, yLen){
  adjs <- c(nw = NA, n = NA, ne = NA,
            w = NA,          e = NA,
            sw = NA, s = NA, se = NA)
  if(row > xLen){
    adjs$n <- row - xLen
  }else{message("Coordinate is on edge of raster")}
  if(row > xLen & row %% xLen != 1){
      adjs$nw <- adjs$n - 1
  }else{message("Coordinate is on edge of raster")}
  if(row > xLen & row %% xLen != 0){
      adjs$ne <- adjs$n + 1
  }else{message("Coordinate is on edge of raster")}
  if(row %% xLen != 1){
    adjs$w <- row - 1
  }else{message("Coordinate is on edge of raster")}
  if(row %% xLen != 1 & row < (xLen*(yLen-1))){
    adjs$sw <- row - 1 + xLen
  }else{message("Coordinate is on edge of raster")}
  if(row %% xLen != 0){
    adjs$e <- row + 1
  }else{message("Coordinate is on edge of raster")}
  if(row %% xLen != 0 & row < (xLen*(yLen-1))){
    adjs$se <- row + 1 + xLen
  }else{message("Coordinate is on edge of raster")}
  if(row < (xLen*(yLen-1))){
    adjs$s <- row + xLen
  }else{message("Coordinate is on edge of raster")}
  return(unlist(adjs))
}

calcKcals <- function(startElev, endElev, dist, weight = 67){
  grade <- (endElev - startElev)/dist
  if(grade < 0){grade <- 0}
  # Walking speed by grade equation provided by mountain tactical:
  # http://mtntactical.com/research/walking-uphill-10-grade-cuts-speed-13not-12/
  speed <- (26.8*3)*exp(-0.4*(grade)) 
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

getDiagCals <- function(startElev, endElev, res=30){
  flatDist <- (2 * res^2)^(1/2)
  diagDist <- (flatDist^2 + (endElev - startElev)^2)^(1/2)
  diagCals <- calcKcals(startElev = startElev, endElev = endElev, dist = diagDist)
  return(diagCals)
}

getHorCals <- function(startElev, endElev, res=30){
  horDist <- (res^2 + (endElev-startElev)^2)^(1/2)
  horCals <- calcKcals(startElev = startElev, endElev = endElev, dist = horDist)
  return(horCals)
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
  return(node)
}

# This implementation of Dijkstra's algorithm has a couple of features intended to save RAM and comp time
# First, it does not calculate calories in a complete navmesh: 
#   instead, it calculates only the lengths between adjacent nodes
# Second, it does not store a complete path for each visited node while running:
#   instead, it stores only the previous node for each visited node, and backtracks to find the final path afterwards
# Third, adjacent nodes are calculated using map dimensions
#   and the consistent order of the xyz table rows (see getAdjs() above)
dijkstra <- function(xyzFile, oriLat, oriLong, destLat, destLong, fileXLen, fileYLen, res=30){
  # Part 1: set-up from original node and take first step
  xyzDf <- read.table(xyzFile)
  oriNode <- findNode(xyzDf, oriLat, oriLong, fileXLen)
  destNode <- findNode(xyzDf, destLat, destLong, fileXLen)
  
  unvisited <- 1:nrow(xyzDf)
  dists <- rep(Inf, nrow(xyzDf))
  prevSteps <- as.integer(rep(NA, nrow(xyzDf)))
  
  visited <- c(oriNode)
  unvisited <- unvisited[-oriNode]
  dists[oriNode] <- 0
  curNode <- oriNode
  
  adjNodes <- getAdjs(curNode, xLen = fileXLen, yLen = fileYLen)
  adjNodes <- adjNodes[!is.na(adjNodes)]
  diagAdjs <- adjNodes[names(adjNodes) %in% c("nw","ne","sw","se")]
  horAdjs <- adjNodes[names(adjNodes) %in% c("n","s","e","w")]
  horDists <- sapply(horAdjs, function(x) getHorCals(xyzDf[curNode,3],xyzDf[x,3]))
  diagDists <- sapply(diagAdjs, function(x) getDiagCals(xyzDf[curNode,3],xyzDf[x,3]))
  adjNodes <- c(diagAdjs, horAdjs)
  adjDists <- c(diagDists, horDists)
  dists[adjNodes] <- adjDists
  prevSteps[adjNodes] <- curNode
  
  if(length(adjDists[adjDists == min(adjDists)]) > 1){
    curNode <- sample(adjNodes[adjDists == min(adjDists)],1) # sample ties
  }else{curNode <- adjNodes[adjDists == min(adjDists)]}
  curNode <- unname(curNode)
  visited <- c(visited,curNode)
  unvisited <- unvisited[-which(unvisited %in% curNode)]
  
  # Part 2: Keep running algorithm until the destination node is visited
  while(!destNode %in% visited){
    if(sum(!is.infinite(dists)) == 0){stop("Route blocked.")}
    if(length(visited) %% 5000 == 0){print(paste(length(visited)/length(dists)*100, "% of nodes visited"))}
    
    adjNodes <- getAdjs(curNode, xLen = fileXLen, yLen = fileYLen)
    adjNodes <- adjNodes[!is.na(adjNodes) & !adjNodes %in% visited]
    diagAdjs <- adjNodes[names(adjNodes) %in% c("nw","ne","sw","se")]
    horAdjs <- adjNodes[names(adjNodes) %in% c("n","s","e","w")]
    
    horDists <- c()
    diagDists <- c()
    if(length(horAdjs) > 0){
      horDists <- sapply(horAdjs, function(x) getHorCals(xyzDf[curNode,3],xyzDf[x,3])) +
        dists[curNode]
    }
    if(length(diagAdjs) > 0){
      diagDists <- sapply(diagAdjs, function(x) getDiagCals(xyzDf[curNode,3],xyzDf[x,3])) +
        dists[curNode]
    }
    
    if(length(diagAdjs) > 0 | length(horAdjs) > 0){
      adjNodes <- c(diagAdjs, horAdjs)
      adjDists <- c(diagDists, horDists)
      
      for(d in 1:length(adjDists)){
        if(adjDists[d] < dists[adjNodes[d]]){
          dists[adjNodes[d]] <- adjDists[d]
          prevSteps[adjNodes[d]] <- curNode
        }else next
      }
    }
    
    ### Fix me in terms of unvisited. 
    ### I want the unvisited node with the minimum value in dists
    if(sum(dists[unvisited]==min(dists[unvisited])) > 1){
      curNode <- sample(which(1:length(dists) %in% unvisited & dists == min(dists[unvisited])), 1)
    }else{curNode <- which(1:length(dists) %in% unvisited & dists == min(dists[unvisited]))}
    
    unvisited <- unvisited[-which(unvisited %in% curNode)]
    visited <- c(visited, curNode)
  }
  
  # Part 3: backtrack from destination node to original to return path
  backPath <- c(destNode)
  backDists <- c() # for test (sum of each )
  prev <- prevSteps[destNode]
  while(!oriNode %in% backPath){
    backPath <- c(backPath, prevSteps[prev])
    backDists <- c(backDists,dists[prev])
    prev <- prevSteps[prev]
  }
  
  return(list(
    calsToDest = dists[destNode], pathToDest = rev(backPath)))
}