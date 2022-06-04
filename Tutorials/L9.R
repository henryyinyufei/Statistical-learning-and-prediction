### In this lecture we will again analyze the relationship between 
### density and alcohol in the wine dataset. This time, we will use
### various kinds of spline

### Read in the wine data
source("Read Wine Data.R")

### We will use functions from the splines package to fit spline models
library(splines)

#####################################################################
### First, fit some polynomial models of various degrees and look ###
### at the plots.                                                 ###
#####################################################################

### Fit polynomial regression models
fit.poly.1 = lm(alcohol ~ density, data = data)
fit.poly.2 = lm(alcohol ~ poly(density, degree = 2), data = data)
fit.poly.4 = lm(alcohol ~ poly(density, degree = 4), data = data)
fit.poly.12 = lm(alcohol ~ poly(density, degree = 12), data = data)

### Get predictions.
### Note: For plotting to work properly, we need to get predictions
###       on the sorted density values.
density.sort = data.frame(density = sort(data$density))
pred.poly.1 = predict(fit.poly.1, density.sort)
pred.poly.2 = predict(fit.poly.2, density.sort)
pred.poly.4 = predict(fit.poly.4, density.sort)
pred.poly.12 = predict(fit.poly.12, density.sort)

### Plot data and predictions on the same axes
with(data, plot(density, alcohol))
lines(density.sort$density, pred.poly.1)
lines(density.sort$density, pred.poly.2, col = "red")
lines(density.sort$density, pred.poly.4, col = "blue")
lines(density.sort$density, pred.poly.12, col = "green")

legend(x = "bottomleft", 
       legend = c("1st order poly", "2nd order poly", "4th order poly", "12th order poly"),
       lty = "solid",
       col = c("black", "red", "blue", "green"))

##################################################
### Let's see if we can do better with splines ###
##################################################

### First, fit basis splines
fit.basis.4 = lm(alcohol ~ bs(density, df = 4), data = data) # 1 knot
fit.basis.8 = lm(alcohol ~ bs(density, df=8), data = data)   # 5 knots
fit.basis.12 = lm(alcohol ~ bs(density, df=12), data = data) # 9 knots

### Get predictions
pred.basis.4 = predict(fit.basis.4, density.sort)
pred.basis.8 = predict(fit.basis.8, density.sort)
pred.basis.12 = predict(fit.basis.12, density.sort)

### Plot data and splines
with(data, plot(density, alcohol))
lines(density.sort$density, pred.basis.4, col = "red")
lines(density.sort$density, pred.basis.8, col = "blue")
lines(density.sort$density, pred.basis.12, col = "green")

legend(x = "bottomleft",
       legend = c("Basis Spline, 4 df", "Basis Spline, 8 df", "Basis Spine, 12 df"),
       lty = "solid",
       col = c("red", "blue", "green"))


### Try again with natural splines
# two extra knots
# linear fit at the end
fit.nat.4 = lm(alcohol ~ ns(density, df = 4), data = data)
fit.nat.8 = lm(alcohol ~ ns(density, df=8), data = data)
fit.nat.12 = lm(alcohol ~ ns(density, df=12), data = data)

### Get predictions
pred.nat.4 = predict(fit.nat.4, density.sort)
pred.nat.8 = predict(fit.nat.8, density.sort)
pred.nat.12 = predict(fit.nat.12, density.sort)

### Plot data and splines
with(data, plot(density, alcohol))
lines(density.sort$density, pred.nat.4, col = "red")
lines(density.sort$density, pred.nat.8, col = "blue")
lines(density.sort$density, pred.nat.12, col = "green")

legend(x = "bottomleft",
       legend = c("Natural Spline, 4 df", "Natural Spline, 8 df", "Natural Spline, 12 df"),
       lty = "solid",
       col = c("red", "blue", "green"))
