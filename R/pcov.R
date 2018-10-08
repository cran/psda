#' Polygonal symbolic covariance
#' 
#' Compute the symbolic polygonal empirical covariance.
#'  
#' @param polygons A list of polygonal datas.
#' @return The method returns a integer.  
#' @examples 
#' x <- psim(10, 3) #simulate 10 polygons of 3 sides
#' pcov(x) 
#' @export
pcov <- function(polygons){
  if(length(polygons) < 1){
    stop("Insert a valid number of polygons!")
  }
  pcov_temp <- function(poligon){
    a <- poligon[,1]
    b <- poligon[,2]
    sumXY <- sum1XY <-area1 <- area2 <- 0
    for(i in 1:length(a)){
      if(i < length(a)){ 
        sumXY <- sumXY + (a[i]*b[i+1] + 2*a[i]*b[i] + 2*a[i+1]*b[i+1])*(a[i]*b[i+1] - a[i+1]*b[i])
        area1 <- area1 + .5*(a[i]*b[i+1] - a[i+1]*b[i])
      }
      else{
        sum1XY <- sumXY + (a[i]*b[1] + 2*a[i]*b[i] + 2*a[1]*b[1])*(a[i]*b[1] - a[1]*b[i])
        area2 <- area2 + .5*(a[i]*b[1] - a[1]*b[i])
      }
    }
    area <- area1 + area2
    return(abs((sumXY + sum1XY)/(24*area)))
  }
  
  covariance <- mean(sapply(polygons, pcov_temp))
  x <- covariance - pmean(polygons)[1]*pmean(polygons)[2]
  return(x/length(polygons))
}