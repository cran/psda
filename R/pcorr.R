#' Polygonal symbolic correlation
#'
#' Compute the symbolic polygonal empirical correlation.
#' 
#' @param polygons A list of matrices of dimension l x 2, where l represent number of sides polygon.
#' @return The method returns a integer.
#' @examples 
#' x <- psim(10, 10) #simulate 10 polygons of 10 sides
#' pcorr(x)
#' @export
pcorr <- function(polygons){
  if(length(polygons) < 1){
    stop("Insert a valid number of polygons!")
  }
  covariance <- pcov(polygons)
  variance <- pvar(polygons)
  correlation <- covariance/(sqrt(variance[1])*sqrt(variance[2]))
  correlation
}