
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/psda)](https://CRAN.R-project.org/package=psda)

# Overview - Polygonal Symbolic Data Analysis (PSDA)

This vignette document is a brief tutorial for psda 1.3.2. Descriptive, auxiliary a modeling functions are presented and applied an example.

Data Science is fundamental to handle and extract knowledge about the data. Silva et al. [1] presented the Symbolic Polygonal Data Analysis as an approach to this task. The psda package is a toolbox to transform number in knowledge. We highlight some important characteristics of the package:
  
* It constructs symbolic polygonal data from classical data;
* It calculates symbolic polygonal descriptive measures;
* It models symbolic polygonal data through a symbolic polygonal linear regression model.

## WNBA 2014 Data
Women national basketball american (WNBA) dataset is used to demostrate the funcionality of the package. It has classical data with dimension 4022 by 6.

```{r wnba}
library(psda)
library(ggplot2)
data(wnba2014)
dta <- wnba2014

```

To construct the symbolic polygonal variables we need to have a class, i.e. a categorical variable. Then, we use the `player_id variable` 

```{r aggregation}
dta$player_id <- factor(dta$player_id)
head(dta)
```

Next, we can obtain the center and radius of the polygon through the `paggreg` function. The only argument necessary is a dataset which has the first column as a factor (the class). From `head` function we can show the first six symbolic polygonal individuals in center and radius representation.

```{r representation}
center_radius <- paggreg(dta)
head(center_radius$center, 6)
head(center_radius$radius, 6)
```

To construct the polygons it is necessary define the number of sides disered. We use as an example a pentagon, i.e. polygons with five vertices. The construction of polygons is given by `psymbolic` function that need of an object of the class `paggregated` and the number of vertices. To exemplify, we use the `head` function to show the first three individuals of the `team_pts` polygonal variable.

```{r polygons}
v <- 5 
polygonal_variables <- psymbolic(center_radius, v)
head(polygonal_variables$team_pts, 3)
```

## Descriptive Measures

After to obtain the symbolic polygonal data we can start to extract knowledge of this type of data through polygonal descriptive measure. Some of this measures are bi-dimensionals, because indicate the relation with the dimensions of the polygons [1]. In this vignette we present the mean, variance, covariance and correlation as can be seen below:
  
  ```{r descriptivel}
### symbolic polygonal mean
pmean(polygonal_variables$team_pts)
pmean(polygonal_variables$opp_pts)

### symbolic polygonal variance
pvar(polygonal_variables$team_pts)
pvar(polygonal_variables$opp_pts)

### symbolic polygonal covariance
pcov(polygonal_variables$team_pts)
pcov(polygonal_variables$opp_pts)

### symbolic polygonal correlation
pcorr(polygonal_variables$team_pts) 
pcorr(polygonal_variables$opp_pts) 
```

The construction of symbolic polygonal scatterplot is done through [ggplot2](https://CRAN.R-project.org/package=ggplot2) package, including all modification. From `pplot` we use a symbolic polygonal variable to plot the scatterplot. The graphic is a powerful tool to understand the data, for example, in this case, we can observe a pentagon with a radius greater than all. This can indicate outliers.

## Visualization
```{r scatter}
pplot(polygonal_variables$team_pts) + labs(x = 'Dimension 1', y = 'Dimension 2') +
  theme_bw()
```

## Modeling

To explain the behavior of a `team_pts` polygonal variable across `fgaat`, `minutes`, `efficiency` and `opp_pts`polygonal variable, we use the polygonal linear regression model `plr`. The function needs of a `formula` and an `environment` containing the symbolic polygonal variables.

```{r modeling}
fit <- plr(team_pts ~ fgatt + minutes + efficiency + opp_pts, data = polygonal_variables)
```

The `summary` function is a method of `plr`. A summary of the polygonal linear regression model is showed from this method. In details, we can observe the quartile of the residuals, estimates of the parameters and its standard deviation. Besides, the statistical of test and the p-value is displayed.

```{r summary}
s <- summary(fit)
s
```

We plot the residuals of the model from `plot` and the histogram.

```{r residuals}
plot(fit$residuals, ylab = 'Residuals')
hist(fit$residuals, xlab = 'Residuals', prob = T, main = '')
```

The fitted values to the model can be accessed from `fitted` method. The arguments are: (i) `model` that is an object of the class `plr`; (ii) a boolean, named `polygon`, if `TRUE` the output is the predicted polygons, otherwise, a vector  with dimension `2n x 1` is computed, the first `n` individuals indicate the fitted center and the last the radius; (iii) `vertices` should be the number of vertices of the polygon selected previously. Besides, we print the first three fitted polygons and plot all from `pplot`.

```{r fitting}
fitted_polygons <- fitted(fit, polygon = T, vertices = v)
head(fitted_polygons, 3)

pplot(fitted_polygons) + labs(x = 'Dimension 1', y = 'Dimension 2') +
  theme_bw()
```

Silva et al.[1] proposed a performance measure to evaluate the fit of model from root mean squared error for area, named rmsea. We can calculate from function `rmsea` as follow below.

```{r rmsea}
rmsea(fitted_polygons, polygonal_variables$team_pts)
```

## References
[1] Silva, W.J.F., Souza, R.M.C.R., Cysneiros, F.J.A. Polygonal data analysis: A new framework in symbolic data analysis, Knowledge Based Systems, 163 (2019). 26-35, <https://www.sciencedirect.com/science/article/pii/S0950705118304052>.
