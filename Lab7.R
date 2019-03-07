## Bar graphs
# install.packages("vcd")
library(vcd)
counts = table(Arthritis$Improved, Arthritis$Treatment)
counts    # getting the data

barplot(counts, main = "Stacked Bar Plot",
        xlab = "Treatment", ylab="Frequency",
        col = c("firebrick", "gold","forestgreen"),
        legend = rownames(counts))

barplot(counts, main ="Grouped Bar Plot",
        xlab = "Treatment", ylab="Frequency",
        col = c("firebrick", "gold", "forestgreen"),
        legend = rownames(counts), beside=TRUE)

## Pie charts
op = par(mfrow=c(2, 2))

# 1
slices = c(10, 12,4, 16, 8)
lbls1 = c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls1,
    main="Simple Pie Chart")

# 2 
pct = round(slices/sum(slices)*100)
lbls2 = paste(lbls1, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls2)),
    main = "Pie Chart with Percentages")

# 3
#install.packages("plotrix")
library(plotrix)
pie3D(slices, labels=lbls1,explode = 0.1,
      main = "3D Pie Chart ")

# 4
mytable = table(state.region)
lbls3 = paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls3,
    main = "Pie Chart from a Table\n ")

par(op)

## hisrogram with normal curve
x = mtcars$mpg
h = hist(x, breaks = 12, col="red", 
         xlab = "Miles Per Gallon",
         main = "Histogram with normal curve")
xfit = seq(min(x), max(x), length=40)
yfit = dnorm(xfit, mean=mean(x), sd=sd(x))
yfit = yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

## Density
op = par(mfrow = c(2,1))
d = density(mtcars$mpg)
plot(d, col = "blue", main = "Density of Miles Per Gallon")

d = density(mtcars$mpg)
plot(d, main = "Density of Miles Per Gallon With shade")
polygon(d, col = "gray65", border = "blue")
rug(mtcars$mpg, col = "brown")

par(op)

## Box plot
boxplot(mpg ~ cyl, data = mtcars,
        main = "Car Mileage Data",
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon")


## Scatter plot with regression lines
attach(mtcars)
plot(wt, mpg,
     main = "MPG vs. Weight",
     xlab = "Car Weight (lbs/1000)",
     ylab = "Miles Per Gallon ", pch=19)
abline(lm(mpg~wt), col ="red", lwd=2, lty=1)
lines(lowess(wt,mpg), col ="blue", lwd=2, lty=2)

## A normal curve with expression
# install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun = dnorm)
p + annotate("text", x=2, y=0.3, parse=TRUE, label="frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}")

## Difference between t and normal distribution
x = seq(-5,5,by = 0.01)
ft = dt(x,5)
fn = dnorm(x)
plot(x, ft, xlab = "X", ylab = "dengsity", col = "blue", type = "l", ylim = c(0,0.4))
points(x, fn, col = "red", type = "l", lwd = 1)

## two-dimensional standard normal distribution
x = y = seq(-4,4,by = 0.1)
f = function(x,y){exp(-0.5*(x^2+y^2))/(2*pi)}
z = outer(x,y,f)
persp(x,y,z,theta = 0,phi = 10,col='royalblue')
