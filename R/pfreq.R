#' Polygonal Symbolic Relative Frequency
#'
#' Compute the bivariate relative frequency.
#' 
#' @param pol A list of matrices of dimension l x 2, where l represent number of sides polygon. 
#' This matrices represent the polygons.
#' @examples 
#' x <- psim(10, 3) #simulate 10 polygons of 3 sides
#' frequency <- pfreq(x)
#' @export
pfreq <- function(pol){
  nr <- length(pol)
  nv <- nrow(pol[[1]])
  
  pol1 <- matrix(0,nrow = nv, ncol = nr)
  pol2 <- matrix(0,nrow = nv, ncol = nr)
  for(temp in 1:nr){
    pol1[ ,temp] <- pol[[temp]][,1]
    pol2[ ,temp] <- pol[[temp]][,2]
  }
  minX <- min(pol1)
  maxX <- max(pol1)
  minY <- min(pol2)
  maxY <- max(pol2)
  
  ratioX <- (maxX - minX)/nr
  ratioY <- (maxY - minY)/nr
  
  rectangles <- vector("list", nr*nr)
  l <- 1
  for(j in 1:(nr)){
    for(i in 1:(nr)){
      rectangles[[l]] <- matrix(c(minX + (i-1)*ratioX, minY + (j-1)*ratioY, minX + (i-1)*ratioX, minY + j*ratioY,
                                  minX + i*ratioX, minY + j*ratioY, minX + i*ratioX, minY + (j-1)*ratioY), ncol = 2, byrow = T)
      l <- l + 1
    }
  }
  len_rectangles <- length(rectangles)
  
  frequency <- matrix(0, nr, len_rectangles)
  for(k in 1 : nr){
    for(l in 1 : len_rectangles){
      p1 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(pol[[k]])), ID = c('a'))))
      p2 <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rectangles[[l]])), ID = c('b'))))
      gI <- rgeos::gIntersection(p1, p2)
      frequency[k, l] <- ifelse(is.null(gI), 0, rgeos::gArea(gI))/rgeos::gArea(p1)
    }
  }
  frequency <- t(frequency)
  frequency <- matrix(apply(frequency, 1, sum), ncol = nr)
  relative_frequency_temp <- frequency/nr
  relative_frequency_temp
}