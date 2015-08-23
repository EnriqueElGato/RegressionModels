Exploratory 
1. A pair wise graph would be good.
2. violin plot also good for looking at relative means.

Analysis
1. something about strenght of beta, e.g. T scores and p scores
2. state unit difference in predictor results in beta change in outcome
3. Although since this is a factor related regression, need to do things like
T stats when intercept is removed
T status when means are compared by making one of predictors the intercept  
OR just interpret coefficents returned by regression it get an idea of impact or predictors that are not factors

am by itself explinas very little of the data as RM shows

Quantitative differs
1. See third session on multivariable analysis for discussion on impact of a factor on slope (beta).

#Interpretation of MV is change in outcome for a unit change in 
# one predictor holding all other predictors constant.

#1. The T tests are testing if the beta is significantly different from 0, e.g.
#  where 0 means no relationsship.

#Omitting variables results in bias vs. Including variables that we shouldn't 
#have increases standard errors of the regression variables.

#What to exclude highly correclated variables as increases variance in estimates

#http://www.stats4stem.org/r-mtcars-data.html

#Transmission (0 = automatic, 1 = manual) 

#do a paris plot
data(mtcars)
names(mtcars)
pairs(mtcars, panel = panel.smooth, main = "MT Cards", col = mtcars$am)
pairs(mpg ~ wt + hp,data=mtcars)

par(mfrow = c(2, 5)) 
plot(x=mtcars$wt,y=mtcars$mpg)
plot(x=mtcars$hp,y=mtcars$mpg)
plot(x=mtcars$cyl,y=mtcars$mpg)
plot(x=mtcars$disp,y=mtcars$mpg)
plot(x=mtcars$drat,y=mtcars$mpg)
plot(x=mtcars$qsec,y=mtcars$mpg)
plot(x=mtcars$vs,y=mtcars$mpg)
plot(x=mtcars$am,y=mtcars$mpg)
plot(x=mtcars$gear,y=mtcars$mpg)
plot(x=mtcars$carb,y=mtcars$mpg)

df <- data.frame(matrix(rep(mtcars[,1],10),ncol=10))
names(df) <- names(mtcars[,2:11])
mapply(cor,df,mtcars[,2:11])
with(mtcars,cor(wt,hp))
f1 <- lm(mpg ~ wt,data=mtcars)
f2 <- lm(mpg ~ wt + hp,data=mtcars)
f3 <- lm(mpg ~ wt + hp + factor(am),data=mtcars)
anova(f1,f2)

banova <- function(f,list){
        fit <- lm(f)
        
        
}

f4 <- lm(mpg ~ wt + hp + factor(cyl),data=mtcars)
anova(f1,f2,f4)
f5 <- lm(mpg ~ wt + hp + drat,data=mtcars)
anova(f1,f2,f5)$"Pr(>F)"[3]
f6 <- lm(mpg ~ wt + hp + qsec,data=mtcars)
anova(f1,f2,f4)
f7 <- lm(mpg ~ wt + hp + factor(vs),data=mtcars)
f8 <- lm(mpg ~ wt + hp + factor(gear),data=mtcars)
f9 <- lm(mpg ~ wt + hp + factor(carb),data=mtcars)
f10 <- lm(mpg ~ wt + hp + disp,data=mtcars)


summary(f3)

par(mfrow = c(2, 2)) 
plot(f3)

ggplot(f3)

fit2 <- lm(mpg ~ . ,data=mtcars)
summary(fit1)
summary(f2)

summary(fit1)$adj.r.squared
summary(fit1)$fstatistic
summary(fit1)$sigma


g1 = ggplot(mtcars, aes(x = wt, y = mpg)) 
g1 = g1 + geom_point(size = 2, colour = "black") 
g1 = g1 + geom_smooth(data=mtcars, stat="smooth",method="lm")
g1 = g1 + xlab("wt") + ylab("mpg")

g2 = ggplot(mtcars, aes(x = hp, y = mpg)) 
g2 = g2 + geom_point(size = 2, colour = "black") 
g2 = g2 + geom_smooth(data=mtcars, stat="smooth",method="lm")
g2 = g2 + xlab("hp") + ylab("mpg")

g2 = ggplot(mtcars, aes(x = hp, y = mpg)) 
g2 = g2 + geom_point(size = 2, colour = "black") 
g2 = g2 + geom_smooth(data=mtcars, stat="smooth",method="lm")
g2 = g2 + xlab("hp") + ylab("mpg")

library(gridExtra)
grid.arrange(g1,g2)




fit <- lm(mpg ~ am,data=mtcars)
fit
summary(fit)
summary(fitted_model)$adj.r
names(fit)
par(mfrow = c(2, 2))
plot(fit)

#Variance of residuals
summary(fit)$sigma

#Residuals plot to see residuals as X grows large, Heteroskedasticity
plot(factor(mtcars$am),resid(lm(mpg ~ am,data=mtcars)));
abline(h = 0)


#fit <- lm(mpg ~ factor(cyl) + disp + hp + drat + wt + qsec + factor(vs) + factor(am) + factor(gear)+factor(carb),data=mtcars) 
fit <- lm(mpg ~ .,data=mtcars)
summary(fit)
names(fit)
plot(fit)
library(car)
vif(fit)
sqrt(vif(fit)) #I prefer sd 
str(mtcars)
