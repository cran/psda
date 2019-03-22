#' Symbolic Polygon
#'
#'@description The function obtains a simple symbolic polygon from center and radius
#' representation.
#' 
#' @param center a integer that represents the barycenter of polygon.
#' @param radius a integer that represents the radius of polygon.
#' @param vertices represents the number of vertices for the polygon.
#' 
#' @return matrix that represents the polygon.
#' 
#' @examples 
#' spolygon(2.5, 3, 5) #pentagon
#'
#' @export
spolygon <- function(center, radius, vertices){
  #print(radius)
  if(vertices <= 2){
    stop("Insert vertices number greater than 2!")
  }
  if(radius < 0){
    stop('Insert a positive value for radius!')
  }
  else if(radius == 0){
    stop('Degenerated polygon. Insert a positive value for radius!')
  }
  
  i <- 1 : vertices
  matrix(c(center + radius * cos(2 * pi * i / vertices), 
           center + radius * sin(2 * pi * i / vertices)), ncol = 2, byrow = F)
}
