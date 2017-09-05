#' Polygonal symbolic variance
#'
#' Estime the polygonal empirical variance between polygonal variables.
#'
#' @param polygons A list of matrices of dimension l x 2 where l represent number of sides polygon.
#' @return The method returns a vector.
#' @examples 
#' x <- psim(10, 3) #simulate 10 polygons of 3 sides
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

