practice: Ch16 

oilChanges <- c(3,5,2,3,1,4,6,4,3,2,0,10,7,8)
repairs <- c(300, 300, 500, 400, 700, 420, 100, 290, 475, 620, 600, 0, 200, 50)
miles <- c(20100, 23200, 19200, 22100, 18400, 23400, 17900, 19900, 20100, 24100, 18200, 19600, 20800, 19700)
oil <- data.frame(oilChanges, repairs, miles)
View(oil)

#inspect for any patterns
plot(oil$oilChanges, oil$repairs)
plot(oil$miles, oil$repairs)

#no patterns= modeling start1: oilchanges
model1 <- lm(formula=repairs ~ oilChanges, data=oil)
summary(model1)

#abline = linear visualization
plot(oil$oilChanges, oil$repairs)
abline(model1)

m <- lm(formula = repairs ~ oilChanges + miles, data = oil)
summary(m)

#The following code creates additional
#columns in our data set to compute and show the total cost for each car
#(which is the cost of the repairs = dv(trying to predict) plus the cost of doing the oil changes + miles = iv).
oil$oilChangeCost <- oil$oilChanges * 350
oil$totalCost <- oil$oilChangeCost + oil$repairs
m1 <- lm(formula=totalCost ~ oilChanges, data=oil)
plot(oil$oilChanges, oil$totalCost)
abline(m1)

test = data.frame(oilChanges=0)
predict(m1,test, type="response")

test = data.frame(oilChanges=5)
predict(m1, test, type="response")

test = data.frame(oilChanges=10)
predict(m1, test, type="response")

install.packages("ggplot2")
library(ggplot2)
#ggplot
ggplot(oil, aes(x=oilChanges, y=totalCost)) +
       geom_point() +
       stat_smooth(method = "lm", col="red")
