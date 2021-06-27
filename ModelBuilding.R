library(dplyr)
data = read.csv("~/winequality-white.csv", sep = ";")
data$quality_class = ifelse(data$quality > 6, 1, 0)
data = data %>% select (-quality)
attach(data)
mod0 = glm(quality_class~1, data = data, family = binomial)
mod0
add1(mod0, ~. + data$fixed.acidity + data$volatile.acidity + data$citric.acid + data$residual.sugar + data$chlorides + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$pH + data$sulphates + data$alcohol, test ="F")
mod1 = update(mod0, ~.+data$alcohol)
summary(mod1)

add1(mod1, ~. + data$fixed.acidity + data$volatile.acidity + data$citric.acid + data$residual.sugar + data$chlorides + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$pH + data$sulphates, test = "F")
mod2 = update(mod1, ~.+data$volatile.acidity)
summary(mod2)

add1(mod2, ~. + data$fixed.acidity + data$citric.acid + data$residual.sugar + data$chlorides + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$pH + data$sulphates, test = "F")
mod3 = update(mod2, ~.+data$chlorides)
summary(mod3)

add1(mod3, ~. + data$fixed.acidity + data$citric.acid + data$residual.sugar + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$pH + data$sulphates, test = "F")
mod4 = update(mod3, ~.+data$residual.sugar)
summary(mod4)

add1(mod4, ~. + data$fixed.acidity + data$citric.acid + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$pH + data$sulphates, test = "F")
mod5 = update(mod4, ~.+data$pH)
summary(mod5)

add1(mod5, ~. + data$fixed.acidity + data$citric.acid + data$free.sulfur.dioxide + 
       data$total.sulfur.dioxide + data$density + data$sulphates, test = "F")
mod6 = update(mod5, ~.+data$density)
summary(mod6)

add1(mod6, ~. + data$fixed.acidity + data$citric.acid + data$free.sulfur.dioxide + data$total.sulfur.dioxide + data$sulphates, test = "F")
mod7 = update(mod6, ~.+data$sulphates)
summary(mod7)

add1(mod7, ~. + data$fixed.acidity + data$citric.acid + data$free.sulfur.dioxide + data$total.sulfur.dioxide, test = "F")
mod8 = update(mod7, ~.+data$fixed.acidity)
summary(mod8)
mod8 = update(mod8, ~. -data$alcohol) #Summary shows alcohol being insignificant

add1(mod8, ~. + data$citric.acid + data$free.sulfur.dioxide + data$total.sulfur.dioxide, test = "F")
mod9 = update(mod8, ~.+data$free.sulfur.dioxide)
summary(mod9)

add1(mod9, ~. + data$citric.acid + data$total.sulfur.dioxide, test = "F")

finalmod = mod9
summary(finalmod)
# The final model is:  quality_class ~ data$volatile.acidity + data$chlorides + data$residual.sugar + data$pH + data$density + data$sulphates + 
# data$fixed.acidity + data$free.sulfur.dioxide

#Alcohol, citric.acid, and total.sulfur.dioxide are not significant with alpha=0.05