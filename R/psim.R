#' Polygonal symbolic data simulation
#' 
#' Simulate a polygonal variable with one or more individuals.
#' 
#' @param n number of simulated polygons.
#' @param vertices number of vertex of the polygon. 
#' 
#' @details The argument \code{radius} should have all values greater than zero. Otherwise,
#' we cannot construct the polygons that compose the symbolic polygonal random variable. Besides,
#' the size of the \code{center} vector should be equal to \code{range} vector.
#' 
#' @return A list of polygons.
#' @examples 
#' number_polygons <- 10
#' psim(number_polygons, 4) 
#' @export
psim <- function(n, vertices){
  center <- runif(n, -1, 1)
  radius <- runif(n)
  if(!is.vector(center)){
    stop('Insert a vector for center!')
  }
  if(!is.vector(radius)){
    stop('Insert a vector for radius!')
  }
  if(length(center) != length(radius)){
    stop('Insert center and radius vector with the same size!')
  }
  
  objects <- length(center)
  if(vertices <= 2){
    stop("Insert a vertex number more than 2")
  }
  if(objects < 1){
    stop("Insert a valid objects number")
  }
  if(min(radius) <= 0){
    stop('Insert radius greater than 0!')
  }
  
  polygons <- vector("list", objects)
  
  for(i in 1 : objects){
    polygons[[i]] <- spolygon(center[i], radius[i], vertices)
  }
  class(polygons) <- 'polygonal'
  polygons
}
