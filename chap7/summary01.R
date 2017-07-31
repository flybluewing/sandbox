library(ISLR)
attach(Wage)

#-------------------------------------------------------------------
#	Polynomial functions and Step Functions
fit <- lm(wage~poly(age,4),data=Wage)
coef(summary(fit1))

# 	raw : raw and not orthogonal polynomials
fit2 <- lm(wage~poly(age,4,raw=T),data=Wage)
#	on the fly
fit2a <- lm(wage~age+I(age^2), data=Wage )
#	cbind( ) - don't need I()
fit2b <- lm(wage~cbind(age,age^2), data=Wage )

#	plot predict and SE areas..
agelims = range(age)
age.grid <- seq(from=agelims[1],to=agelims[2])
preds <- predict(fit,newdata=list(age=age.grid),se=T)
par( mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0) )
plot( age, wage, xlim=agelims, cex=0.5, col="darkgray" )
title("Degree -4 Polynomial",outer=T)
#	-	Standard Error areas..
se.bands <- cbind( preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit )
matlines(age.grid, se.bands, lwd=2, col="blue", lty=3 )
#	- one more try using an orthogonal set of basis function
preds2 <- predict(fit2, newdata=list(age=age.grid), se=T )
max( abs(preds$fit-preds2$fit) )

#	- hypothesis tests
#		ANVA : analysis of variance using an F-Test
fit.1 <- lm(wage~age,data=Wage)
fit.2 <- lm(wage~poly(age,2),data=Wage)
fit.3 <- lm(wage~poly(age,3),data=Wage)
fit.4 <- lm(wage~poly(age,4),data=Wage)
fit.5 <- lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)	# select 5% F-Statistic

#	classification( wage>250 )
fit <- glm(I(wage>250)~poly(age,4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid), se=T)
pfit = exp(preds$fit)/(1+exp(preds$fit))	# ISLR, 291 page
se.bands.logit <- cbind( preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit )
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))
#	logit이 아닌 바로 확률을 얻는 것도 가능하긴 함.
preds <- predict(fit,newdata=list(age=age.grid),type="response",se=T)
plot( age, I(wage>250),xlim=agelims, type="n", ylim=c(0,0.2))
points( jitter(age), I((wage>250)/5), cex=0.5, pch="I", col="darkgray" )
	#	jitter() : 빈도가 많음을 표현하기 위해, 데이터들을 쬐끔식 비튼다. 
	#				동일데이터가 한 개로 나타나지 않기위해.
lines(age.grid, pfit, lwd=2, col="blue")
matlines( age.grid, se.bands, lwd=1, col="blue", lty=3 )

#	- 드디어.. 구간을 나눠보자.
table( cut(age,4) )	#	cut() 직접 구간설정은 break 옵션을 사용
fit = lm(wage~cut(age,4),data=Wage)
coef( summary(fit) )


#-------------------------------------------------------------------
#	Splines
library(splines)
fit <- lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T )
plot( age, wage, col="gray" )
lines( age.grid, pred$fit, lwd=2 )
lines( age.grid, pred$fit+2*pred$se, lty="dashed" )
lines( age.grid, pred$fit-2*pred$se, lty="dashed" )

attr( bs(age,df=6), "knots" )
	#	25%   50%   75% 
	# 33.75 42.00 51.00 
attr( bs(age,knots=c(25,40,60)), "knots" )
	# 25 40 60
#	- bs() 함수의 degree 파라미터를 통해 함수의 차원지정 가능(default=3)

#	natural spline
fit2 <- lm(wage~ns(age,df=4),data=Wage)
pred2 <- predict( fit2, newdata=list(age=age.grid), se=T )
lines( age.grid, pred2$fit, col="red", lwd=2 )

#	smoothing spline
plot( age, wage, xlim=agelims, cex=.5, col="darkgray")
title("Smoothing Splines")
fit = smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age,wage, cv=T)	# cv에 따라 DF자동선택
fit2$df
lines( fit, col="red", lwd=2 )
lines( fit2, col="blue", lwd=2 )
legend( "topright", legend=c("16DF", "6.8DF"), col=c("red","blue"), lty=1, cex=.8)

#	local regression
plot( age, wage, xlim=agelims, cex=.5, col="darkgray" )
title("Local Regression")
fit = loess(wage~age, span=.2, data=Wage)
fit2 = loess(wage~age, span=.5, data=Wage)
lines( age.grid, predict(fit,data.frame(age=age.grid)),	col="red",	lwd=2 )
lines( age.grid, predict(fit2,data.frame(age=age.grid)),	col="blue",	lwd=2 )
legend("topright", legend=c("Span 0.2","Span 0.5"), col=c("red","blue"), lty=1, cex=.8)



#-------------------------------------------------------------------
#	GAM
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)
	#	education은 qualitative variable

library(gam)
gam.m3 <- gam(wage~s(year,4)+s(age,5)+education, data=Wage)
	#	s() : smoothing splines

par(mfrow=c(3,3))
plot(gam.m3, se=T, col="blue" )
plot(gam1, se=T, col="red")	# lm으로 생성한 GAM도 plot.gam()이 먹힌다.
#	-	ANOVA test
gam.m1 <- gam( wage~s(age,5)+education, data=Wage )
gam.m2 <- gam( wage~year+s(age,5)+education, data=Wage )
anova(gam.m1, gam.m2, gam.m3, test="F")
preds <- predict( gam.m2, newdata=Wage )

#	-	lo() : local regression
gam.lo <- gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage )
plot.gam( gam.lo, se=T, col="green" )
gam.lo.i <- gam(wage~lo(year,age,span=0.5)+education, data=Wage)
library( akima )	#	- Two-dimensional surface
plot(gam.lo.i)

#	logistic regression
gam.lr <- gam(I(wage>250)~year+s(age,df=5)+education, family=binomial, data=Wage )
par( mfrow=c(1,3) )
plot( gam.lr, se=T, col="green" )
table( education, I(wage>250) )
gam.lr.s <- gam( I(wage>250)~year+s(age,df=5)+education, family=binomial, data=Wage, subset=(education!="1. < HS Grad") )
plot( gam.lr.s, se=T, col="green" )
