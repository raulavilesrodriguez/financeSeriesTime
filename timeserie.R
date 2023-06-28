library(devtools)
library(lattice)
library(latticeExtra)
library(gridExtra)
library(TSA)
library(pander)
library(zoo)
library(xts)
#library(latticework)

# the set seed
set.seed(1234)

data("hours")
xyplot(hours)
months <- c("J", "A", "S", "O", "N", "D", "J", "F", "M", "A", "M", "J")
data("wages")
xyplot(wages, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.text(x, y, labels = months)
})

wages_fit1 <- lm(wages ~ time(wages))
summary(wages_fit1)
wages_rst <- rstudent(wages_fit1)
xyplot(wages_rst ~ time(wages_rst), type = "l",
       xlab = "Time", ylab = "Studentized residuals")










