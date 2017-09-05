#' Polygonal internal variance
#' @description Caltulate polygonal internal variance for polygonal data.
#' @param polygon a matrix that represents a polygonal variable.
#' @return The internal variance.
#' @examples 
#' x <- psim(1, 3) #simulate a polygons of 3 sides
#' pvari(x[[1]])
#' @export
pvari <- function(polygon){
  if(nrow(polygon) < 3){
    stop("Insert a valid polygon!")
  }
  first_moment <- pmean_id(polygon)
  second_moment <- psmi(polygon)
  x <- (second_moment[1] - first_moment[1]^2)
  y <- (second_moment[2] - first_moment[2]^2)
  return(c(x,y))
}

