###4x


ra=read.table(file="C:/Users/weich/Dropbox/thesis/RA.csv",header=T,sep=",")

names(ra)
str(ra)
data.frame(ra)
RAscore=ra$y
gender=ra$sex
treatment=ra$trt
RAscorebaseline=ra$baseline
age=ra$age
table(RAscore)
barplot(table(RAscore))
table(RAscorebaseline)
barplot(table(RAscorebaseline))

boxplot(age~gender,data=ra,horizontal=TRUE, col=c("turquoise","tomato"))
boxplot(age~treatment,data=ra,col=c("yellow","blue"),horizontal=TRUE)

library(VGAM)

f=vglm(factor(ra$y) ~ factor(ra$sex)+factor(ra$trt)+ factor(ra$baseline)+age, family=cumulative(parallel=TRUE), data = ra)
summary(f)
###true Y
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

head(ra)
ftable(xtabs(~ y + sex + trt+baseline, data = ra)
m=polr(factor(ra$y) ~ sex+trt+ factor(ra$baseline)+age, data = ra, Hess=TRUE)
summary(m)
ctable=coef(summary(m))
p=pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable=cbind(ctable, "p value" = p)
ctable
ci=confint(m)
confint.default(m)
exp(coef(m))
exp(cbind(OR = coef(m), ci))
library(aod)

####y>=3
Y=(as.numeric(ra$y)>=3)
mc=glm(Y ~trt+age+factor(baseline)+sex, family="binomial", data = ra)
summary(mc)
ctable=coef(summary(mc))
p=pnorm(abs(ctable[, "z value"]), lower.tail = FALSE) * 2
ctable=cbind(ctable, "p value" = p)
ctable
ci=confint(mc)
confint.default(mc)
exp(coef(mc))
exp(cbind(OR = coef(mc), ci))



###total lasso1
library(glmpathcr)
sex=as.factor(ra$sex)
trt=as.factor(ra$trt)
baselin=as.factor(ra$baseline)
xfactors=model.matrix(ra$y ~sex+trt+baseline)[,-1]
x=as.matrix(data.frame(ra$age, xfactors))
y=as.factor(ra$y)
fit=glmpath.cr(x, y)
summary(fit)
plot(fit, xvar = "step", type = "bic")
BIC.step=model.select(fit) 
BIC.step
AIC.step=model.select(fit) 
AIC.step
coefficients <- coef(fit, s = BIC.step) 
sum(coefficients != 0)
nonzero.coef(fit, s = BIC.step)
pred=predict(fit)
table(pred, y)
pred=predict(fit, type = "probs") 
pred
fit=glmpath.cr(x, y, method = "forward")
coefficients=coef(fit, s = BIC.step) 
nonzero.coef(fit, s = BIC.step)
pred=predict(fit) 
table(pred, y)
###total lasso 2
library(glmnetcr)
fit <- glmnet.cr(x, y)
head(fit)
plot(fit, xvar = "step", type = "bic") 
plot(fit, xvar = "step", type = "coefficients")
plot(fit, xvar = "lambda", type = "coefficients")
plot(fit, xvar = "lambda", type = "bic")
BIC.step=select.glmnet.cr(fit) 
BIC.step
AIC.step=select.glmnet.cr(fit, which = "AIC") 
AIC.step
coefficients=coef(fit, s = BIC.step) 
coefficients$a0
sum(coefficients$beta != 0)
nonzero.glmnet.cr(fit, s = BIC.step)
fit=glmnet.cr(x, y, method = "forward")
BIC.step=select.glmnet.cr(fit) 
BIC.step
coefficients=coef(fit, s = BIC.step) 
coefficients$a0
sum(coefficients$beta != 0)
nonzero.glmnet.cr(fit, s = BIC.step)
hat=fitted(fit, s = BIC.step) 
names(hat)
table(hat$class, y)
barplot(table(hat$class, y))









###net&lasso
library(glmnet)
trt=as.factor(ra$trt)
sex=as.factor(ra$sex)
baseline=as.factor(ra$baseline)
baseline
Y=(as.numeric(ra$y)>=3)
Y=as.factor(Y)
xfactors=model.matrix(Y~sex+trt+baseline)[,-1]
x=as.matrix(data.frame(ra$age, xfactors))
x
eout =glmnet (x,Y,alpha =0.5,family="binomial")
plot(eout,xvar="lambda")
predict (eout ,type="coefficients",lambda=bestlam )
cv.out3 =cv.glmnet (x,Y,alpha =0.5,family="binomial")
cvfit3 = cv.glmnet(x, Y,alpha =0.5, family = "binomial", type.measure = "class")
plot(cvfit3)
plot(cv.out3)
coef(cv.out3, s = "lambda.min")
bestlam =cv.out3$lambda.min
bestlam

####lasso
out2=glmnet (x,Y,alpha =1,family="binomial")
plot(out2, xvar="lambda")
predict (out2 ,type="coefficients",lambda=bestlam2 )
cv.out2 =cv.glmnet (x,Y,alpha =1,family="binomial")
cvfit2 = cv.glmnet(x, Y,alpha =1, family = "binomial", type.measure = "class")
plot(cvfit2)
coef(cv.out2, s = "lambda.min")
plot(cv.out2)
bestlam2 =cv.out2$lambda.min
bestlam2
auc=cv.glmnet(x,Y,alpha=1,type.measure="auc",family="binomial")
pred2 = predict(cv.out2,x, s = "lambda.min", type = "class")
library(caret)
confusionMatrix(pred2, Y)

rm(list=ls())
x=runif(200,-10,10)
a=c(18,0,-0.5,0)
Y=a[1]*x^1+a[2]*x^2+a[3]*x^3+a[4]
Y=Y+rnorm(length(Y),0,5)
plot(x,Y)



