#'  Convex verification
#' 
#' @description Verify convexity of the polygons.
#' 
#' @param polygon A matrix of dimension l x 2 where l represent number of sides polygon.
#' @return A boolean.
#' @examples
#' x <- psim(1,4)
#' x <- x[[1]] 
#' pconvex(x) 
#' 
#' x <- matrix(unlist(psim(1,4)), ncol= 2)
#' pconvex(x) #run
#' @export 
pconvex <- function(polygon) {
  if(nrow(polygon) < 3){
    stop("Insert a valid polygon!")
  }
  crossProductLength <- function(ax, ay, bx, by, cx, cy) {
    BAx <- ax - bx
    BAy <- ay - by
    BCx <- cx - bx
    BCy <- cy - by
    (BAx * BCy - BAy * BCx)
  }
  vertex_number <- nrow(polygon)
  testFor <- function(a) {
    b <- (a + 1) %% vertex_number
    c <- (b + 1) %% vertex_number
    sign(crossProductLength(polygon[a + 1,1], polygon[a + 1,2],
                            polygon[b + 1,1], polygon[b + 1,2],
                            polygon[c + 1,1], polygon[c + 1,2]))
  }
  signs <- sapply(0:(vertex_number - 1), testFor)
  convex <- all(signs == signs[1])
  return(convex)
}