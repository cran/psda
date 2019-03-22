#' Handle Missing Values in Polygonal Objects
#' 
#' @description The function omits missing polygons.
#' @param object objects of the class "\emph{polygonal}".
#' @param ... further arguments special methods could require.
#' @return polygons an object of the class "\emph{polygonal}" without missing values.
#' @examples 
#' y <- psim(5, 3)
#' y[[1]] <- NA
#' na.omit(y) 
#' @export 
na.omit <- function(object, ...){
  missing_polygons <- is.na(object)
  if(sum(missing_polygons != 0)){
    polygons <- object[!missing_polygons]
  }
  class(polygons) <- 'polygonal'
  polygons
}