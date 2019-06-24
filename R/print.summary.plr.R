#' Print Summary Polygonal Linear Regression
#'
#' @description print arguments of the class "\emph{summary.plr}" and returns it \emph{invisibly} (via \code{\link[base]{invisible}} (x)).
#' @param x an object of the class "\emph{summary.plr}".
#' @param digits  non-null value for \code{digits} specifies the minimum number of significant 
#' digits to be printed in values.
#' @param concise a \emph{logical} used to determine the type of digits.
#' @param ... further arguments special methods could require.
#' @examples 
#' yp <- psim(50, 10) #simulate 50 polygons of 10 sides
#' xp1 <- psim(50, 10) #simulate 50 polygons of 10 sides
#' xp2 <- psim(50, 10) #simulate 50 polygons of 10 sides
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1 + xp2, data = e)
#' s <- summary(fit)  
#' s
#' @method print summary.plr
#' @rdname print
#' @exportClass summary.plr

#' @export
print.summary.plr <- function(x, digits = max(3L, getOption("digits") - 3L), 
                              concise = FALSE, ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")

  resid <- x$residuals
  
  coefs <- as.data.frame(x$coefficients)
  
  nam <- c('Min', '1Q', 'Median', '3Q', 'Max')
  res <- structure(zapsmall(quantile(x$residuals), digits = digits), 
                   dim = length(nam), dimnames = list(nam))
  
  cat('Residuals:\n')
  print(res)
  cat('\n', sep ='')
  if (length(x$coefficients)) {
    cat("Coefficients:\n")
    printCoefmat(coefs)
  }
  else cat("No coefficients\n")
  cat("\n")

  invisible(x)
}