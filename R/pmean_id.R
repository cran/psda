#' Polygonal symbolic internal mean
#' @description Compute the symbolic polygonal empirical mean for only one observation (classes).
#' @param polygon a matrix representing the polygon.
#' @examples
#' x <- psim(1, 3)
#' x <- x[[1]]
#' pmean_id(x)
#' @return a polygonal empiric mean of a polygon.
#' @export
pmean_id <- function(polygon){
  if(nrow(polygon) < 3){
    stop("Insert a valid polygon!")
  }
  a <- polygon[,1]
  b <- polygon[,2]
  sumX <- 0
  sum1X <- 0
  sumY <- 0
  sum1Y <- 0
  area1 <- 0
  area2 <- 0
  for(i in 1:length(a)){
    if(i < length(a)){
      sumX <- sumX + (a[i] + a[i+1])*(a[i]*b[i+1] - a[i+1]*b[i])
      sumY <- sumY + (b[i] + b[i+1])*(a[i]*b[i+1] - a[i+1]*b[i])
      area1 <- area1 + (a[i]*b[i+1] - a[i+1]*b[i])
    }
    else{
      sum1X <- sum1X + (a[i] + a[1])*(a[i]*b[1] - a[1]*b[i])
      sumY <- sumY + (b[i] + b[1])*(a[i]*b[1] - a[1]*b[i])
      area2 <- area2 + (a[i]*b[1] - a[1]*b[i])
    }
  }
  area <- .5*(area1 + area2)
  if(area == 0) print("Area of one polygon is degenerated")
  else return(c(sumX + sum1X, sumY + sum1Y)/(6*area))
}


