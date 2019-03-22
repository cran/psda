#' Print method for Polygonal Linear Regression
#' @description \code{print.plr} is the \code{plr} method of the generic print function
#' which prints its argument.
#' @param x the object to be printed.
#' @param digits a non-null value for \code{digits} specifies the minimum number of significant digits to be printed in values.
#' @param ... further arguments passed to or from other methods.
#' @examples 
#' yp <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp1 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp2 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1 + xp2, data = e)
#' fit  
#' @method print plr
#' @exportClass plr
#' @export
print.plr <- function (x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  if (length(x$coefficients)) {
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits), print.gap = 2L, 
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}