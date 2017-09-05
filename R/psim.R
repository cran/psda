#' Polygonal symbolic data simulation
#' 
#' Function to create a polygonal data list.
#' 
#' @param objects number of objects simulated.
#' @param vertices number of vertex of the polygon 
#' @return A list of polygons.
#' @examples 
#' psim(2, 3)
#' @export
psim <- function(objects, vertices){
  if(vertices <= 2){
    stop("Insert a vertex number more than 2")
  }
  if(objects < 1){
    stop("Insert a valid objects number")
  }
  tmp <- NULL
  for(i in 1:objects) {
    tmp[[i]] <- matrix(runif(vertices*2), ncol = 2)
  }
  
  center <- lapply(tmp, function(x) apply(x, 2, function(y) mean(y)))
  radius <- sapply(tmp, function(x)  2*sd(x))
  
  polygons <- vector("list", objects)
  res <- matrix(rep(0, vertices*2), ncol = 2)
  
  l <- 1
  for(i in 1 : objects){
    for(j in 1 : vertices){
      res[j, ] <- c(center[[i]][1] + radius[i]*cos(2*pi*j/vertices), 
                    center[[i]][2] + radius[i]*sin(2*pi*j/vertices))
      polygons[[l]] <- res
    }
    l <- l + 1
  }
  polygons
}
