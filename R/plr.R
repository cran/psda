#' Polygonal linear regression
#' @description  plr is used to fit polygonal linear models.
#' @param formula an object of class "formula": a symbolic description of the model to be fitted.
#' @param env environment that contain the variables of study.
#' @param residuals_type type of residual, e.g., rmsd and mse residuals by area.
#' @param intercept logical, if TRUE the model present intercept, otherwise it do not present intercept.
#' @return beta_hat a vector of estimators for beta proposed in model.
#' @return SSRES sum square residuals.
#' @return SSREG sum square of regressions.
#' @return SST sum square toral.
#' @return residuals_type a type of residuals mse or rmsd.
#' @return prediction_polygons a list of prediction polygons by plr model.
#' @examples
#' require(psda)
#' yp <- psim(10, 4)
#' xp1 <- psim(10, 4)
#' xp2 <- psim(10, 4)
#' e <- new.env()
#' e$yp <- yp
#' e$xp1 <- xp1
#' e$xp2 <- xp2
#' fit <- plr(yp~xp1+xp2, env = e)
#' @export

plr <- function(formula, env, residuals_type = 'mse', intercept = TRUE){
  if(class(formula) != "formula"){
    stop("Insert a valid formula!")
  }
  nf <- all.vars(formula)
  response_variable <- get(nf[1], envir = env)
  n_variables <- length(nf)
  n_sides <- nrow(response_variable[[1]])
  args <- list()
  
  for(i in 2:n_variables){
    args[[i - 1]] <- get(nf[i], envir = env)
  }
  names(args) <- nf[-1]
  
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
  
  for (j in 1:(n_variables - 1)) {
    for (i in 1:n_observations) {
      mat_xc[[j]] <- matrix(mat_x[[j]][1:n_observations], nrow = n_observations, byrow = T)
    }
  }
  mat_xc <- matrix(unlist(mat_xc), ncol = (n_variables - 1))
  
  colnames(mat_xc) <- nf[-1]
  colnames(x_radius) <- nf[-1]
  
  yc <- y_centers[, 1]
  
  yr <- y_radius
  mat_zero <- matrix(0, nrow = n_observations, ncol = n_variables)
  
  if(intercept == T){
    xc <- cbind(rep(1, n_observations), mat_xc)
    xr <- cbind(rep(1, n_observations), x_radius)
  }
  else{
    xc <- mat_xc
    xr <- x_radius
  }
  beta_pol_center <- solve(t(xc)%*%xc)%*%t(xc)%*%yc
  beta_pol_radius <- solve(t(xr)%*%xr)%*%t(xr)%*%yr
  Y <- c(yc, yr)
  
  beta_hat <- c(beta_pol_center, beta_pol_radius)
  rownames(beta_hat) <- NULL
  
  yc_hat <- xc%*%beta_pol_center
  yr_hat <- xr%*%beta_pol_radius
  
  res <- new.env()
  res$beta_hat <- beta_hat

  pol_hat <- list()
  pol_ind <- matrix(0, nrow = n_sides, ncol = 2)

  for (j in 1:n_observations){
    for (i in 1 : n_sides){
      pol_ind[i, ] <- c(yc_hat[j] + yr_hat[j]*cos(2*pi*i/n_sides), yc_hat[j] + yr_hat[j]*sin(2*pi*i/n_sides))
    }
    pol_hat[[j]] <- pol_ind
  }
  
  area_real <- sapply(response_variable, parea)
  area_predict <- sapply(pol_hat, parea)
  residuals <- area_real - area_predict 
  
  if (!is.null(residuals_type)){
    if(residuals_type == 'rmsd'){
      residual_total <- sqrt(sum(residuals^2)/n_observations)
    }
    else if (residuals_type == 'mse'){
      residual_total <- mean(residuals^2)
    }
    else {
      residual_total <- NULL
      stop("Select a valid type error!")
    }
    if (!is.null(residual_total)){
      res$residual_type <- residual_total
    }
  }
  else{
    stop('Insert a valid error type!')
  }
  res$original <- area_real
  res$predicted <- area_predict
  res$residual_type <- residual_total
    
  res$residuals <- residuals
  res$prediction_polygons <- pol_hat
  res
}

