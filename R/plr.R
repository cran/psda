#' Polygonal linear regression
#' @description  plr is used to fit polygonal linear models.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param data a environment that contains the variables of the study.
#' @param model logicals. If TRUE the corresponding components of the fit are returned.
#' @param ... additional arguments to be passed to the low level polygonal linear regression fitting functions.
#' @return residuals is calculated as the response variable minus the fitted values.
#' @return rank the numeric rank of the fitted polygonal linear model.
#' @return df.residual degrees freedom of the residuals.
#' @return call the matched call.
#' @return fitted.values the fitted mean values.
#' @return terms the \code{\link[stats]{terms}}.
#' @return coefficients a named vector of coefficients.
#' @return model the matrix model for center and radius.
#' @examples
#' yp <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp1 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' xp2 <- psim(10, 10) #simulate 10 polygons of 10 sides
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1+xp2, e)
#' @references Silva, W.J.F, Souza, R.M.C.R, Cysneiros, F.J.A. (2019) \url{https://www.sciencedirect.com/science/article/pii/S0950705118304052}.
#' @exportClass plr
#' @export

plr <- function(formula, data, model = TRUE, ...){
  if(class(formula) != "formula"){
    stop("Insert a valid formula!")
  }
  
  terms_formula <- terms(formula)
  intercept <- attr(terms_formula, 'intercept')
  
  nf <- all.vars(formula)
  n_variables <- length(nf)
  n_regressors <- n_variables - 1
  n_observations <- length( get(nf[1], envir = data))

  args <- list()
  
  cl <- match.call()
  
  for(i in 1:n_variables){
    args[[i]] <- get(nf[i], envir = data)
  }

  names(args) <- nf
  missings_position <- unlist(sapply(args, function(x) which(is.na(x))))

  if(length(missings_position) != 0){
    args <- lapply(args, function(x) x[-c(missings_position)])
    len_missings <- length(missings_position)
  }
  else{
    args <- args[sapply(args, function(x) !is.null(x))]
    len_missings <- 0
  }
  
  response_variable <- args[[1]]
  args <- args[nf[-1]]
  
  x_centers <- lapply(args, function(x) t(sapply(x, pmean_id)))
  y_centers <- t(sapply(response_variable, pmean_id))
  
  n_observations <- nrow(x_centers[[1]])
  mat_x <- lapply(x_centers, function(x) t(sapply(x, function(y) y)))

  x_radius <- lapply(args, function(x) lapply(x, function(y){
    dist(matrix(c(pmean_id(y), y[1,]), ncol = 2, byrow = T))
  } ))
  
  x_radius <- matrix(unlist(x_radius), ncol = (n_variables - 1))
  colnames(x_radius) <- NULL
  
  y_radius <- sapply(response_variable, function(y) {
    dist(matrix(c(pmean_id(y), y[1,]), ncol = 2, byrow = T))
  })
  
  mat_xc <- list()
  
  for (j in 1:n_regressors){
    for (i in 1:n_observations) {
      mat_xc[[j]] <- matrix(mat_x[[j]][1:n_observations], nrow = n_observations, byrow = T)
    }
  }
  mat_xc <- matrix(unlist(mat_xc), ncol = (n_variables - 1))
  
  colnames(mat_xc) <- nf[-1]
  colnames(x_radius) <- nf[-1]
  
  yc <- y_centers[, 1]
  yr <- y_radius

  if(intercept == T){
    xc <- cbind(rep(1, n_observations), mat_xc)
    xr <- cbind(rep(1, n_observations), x_radius)
    
    mat_zero <- matrix(0, nrow = nrow(xc), ncol = ncol(xc))
    
    xc_zero <- cbind(xc, mat_zero)
    xr_zero <- cbind(mat_zero, xr)
    X <- rbind(xc_zero, xr_zero)
  }
  else{
    xc <- mat_xc
    xr <- x_radius
    mat_zero <- matrix(0, nrow = nrow(xc), ncol = ncol(xc))
    xc_zero <- cbind(xc, mat_zero)
    xr_zero <- cbind(mat_zero, xr)
    X <- rbind(xc_zero, xr_zero)
  }
  
  Y <- c(yc, yr)
  coefficients <- solve(t(X) %*% X) %*% t(X) %*% Y

  beta_pol_center <- coefficients[1:n_variables]
  beta_pol_radius <- coefficients[-(1:n_variables)]
  
  coefficients <- c(beta_pol_center, beta_pol_radius)
  
  Y_hat <- X %*% coefficients
  
  rownames(coefficients) <- NULL
  
  if(intercept == T){
    names(coefficients) <- c('(center-intercept)', paste('center-', nf[-1], sep = ''),
                         '(radius-intercept)', paste('radius-', nf[-1], sep = ''))
  }
  else{
    names(coefficients) <- c(paste('center-', nf[-1], sep = ''),
                             paste('radius-', nf[-1], sep = ''))
  }
  
  res <- new.env()
  res$coefficients <- coefficients

  residuals <- Y - Y_hat
  
  if (n_variables == 0L) {
    return(list(coefficients = numeric(), residuals = Y, 
                fitted.values = 0 , rank = 0, df.residual = n_observations))
  }
  
  rank <- ncol(X)
  
  res$rank <- rank
  res$call <- cl
  res$fitted.values <- Y_hat
  res$residuals <- residuals
  mf <- sum(is.na(response_variable))
  class(len_missings) <- 'omit'
  res$na.action <- missings_position
  res$df.residual <- nrow(X) - rank
  
  if(model){
    center_radius_data <- data.frame(yc = yc, yr = yr, mat_xc, x_radius)
    names_center_radius_data <- paste(nf[1], 1:2, paste('_center', sep = ''), sep = '')
    names(center_radius_data) <- c('yc', 'yr', paste0("xp", rep(1 : (2 * n_regressors), each = 2, 
                    length.out = (2 * n_regressors)), "", c('c', 'r'))) 
    res$model <- center_radius_data
    res$model <- X
  }
  res$terms <- terms_formula
  
  class(res) <- 'plr'
  res
}

