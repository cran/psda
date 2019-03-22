#' Polygonal Area
#' @description Compute the area of polygon.
#' @param polygon a matrix representing the polygon.
#' @return a integer the area of polygon.
#' @examples 
#' x <- psim(10, 10) #simulate 10 polygons of 10 sides
#' x <- x[[1]] 
#' parea(x)
#' @export
parea <- function(polygon){
  if(!is.matrix(polygon)){
    stop("Insert a valid polygon!")
  }
  if(all(polygon == 0)){
    area <- 0
    message("Warning: Degenerated polygon! \n")
  }
  a <- polygon[,1]
  b <- polygon[,2]
  area1 <- 0
  area2 <- 0
  for(i in 1:length(a)){
    if(i < length(a)){
      area1 <- area1 + (a[i]*b[i+1] - a[i+1]*b[i])
    }
    else{
      area2 <- area2 + (a[i]*b[1] - a[1]*b[i])
    }
  }
  area <- .5*(area1 + area2)
  
  if(area == 0) {
    message("Warning: Degenerated polygon! \n")
  }
  area
}