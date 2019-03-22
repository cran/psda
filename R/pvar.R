#' Polygonal symbolic variance
#'
#' Estime the symbolic polygonal empirical variance.
#'
#' @param polygons A list of matrices of dimension l x 2 where l represent number of sides polygon.
#' @return The method returns a bi-dimensional vector.
#' @examples 
#' x <- psim(8, 12) #simulate 8 polygons of 12 sides
#' pvar(x)
#' @export
pvar <- function(polygons){
  if(length(polygons) < 1){
    stop("Insert a valid number of polygons!")
  }
  first_moment <- pmean(polygons)
  second_moment <- sapply(polygons, psmi)
  
  sm <- c(mean(second_moment[1,]), mean(second_moment[2,]))
  x <- (sm[1] - first_moment[1]^2)
  y <- (sm[2] - first_moment[2]^2)
  return(c(x,y))
}

