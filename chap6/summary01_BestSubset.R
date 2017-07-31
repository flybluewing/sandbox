library(ISLR)
names(Hitters)	#	fix(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters2 <- na.omit(Hitters)	# remove rows have any NA

library(leaps)
regfit.full <- regsubsets(Salary~.,Hitters)
summary( regfit.full )	# 유력 predictor가 1~8개 까지의 선정된 것을 확인("*"표시) 

regfit.full <- regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary <- summary( regfit.full )	# 유력 predictor를 19개까지 선별
names( reg.summary )
reg.summary$rsq		# R제곱 검증 값.

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max( reg.summary$adjr2 )
	#	11
	# Adjusted R제곱 최고값의 위치는 11임을 확인.(즉 최적인 p 갯수 확인)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)
	# cex : 점 크기

plot(reg.summary$cp,xlab="Var num", ylab="Cp", type="l")
minCp <- which.min( reg.summary$cp )
points( minCp, reg.summary$cp[minCp], col="red" )
plot(reg.summary$bic,type="l")
minBic <- which.min( reg.summary$bic )
points( minBic, reg.summary$bic[minBic], col="red" )

# Built-in plot() command
plot( regfit.full, scale="r2" )	# r2, adjr2, Cp, bic

# coefficient
coef( regfit.full, 6 )


# Forward and Backward
regfit.fwd <- regsubsets( Salary~., data=Hitters, nvmax=19, method="forward" )
summary( regfit.fwd )
coef( regfit.fwd, 3 )
regfit.bwd <- regsubsets( Salary~., data=Hitters, nvmax=19, method="backward" )


# Validation Set Approach and Cross-Validation
set.seed(1)
train = sample( c(T,F), nrow(Hitters2), rep=T )
test = !train

regfit.best = regsubsets(Salary~.,data=Hitters2[train,],nvmax=19)
test.mat <- model.matrix( Salary~., data=Hitters2[test,] )

val.errors = rep(NA,19)
for( i in 1:19 ){
	coefi <- coef(regfit.best, id=i)
	pred <- test.mat[,names(coefi)]%*%coefi
	val.errors[i] <- mean((Hitters2$Salary[test]-pred)^2)
}
which.min(val.errors)

predict.regsubsets = function( object, newdata, id, ... ){
	form 	<- as.formula(object$call[[2]])
	mat 	<- model.matrix(form,newdata)
	coefi 	<- coef( object, id=id )
	xvars	<- names(coefi)
	mat[,xvars]%*%coefi
}

#변수를 고르기 위해 모든 데이터를 사용한다.
#training용 데이터만 사용 시, 선택된 변수의 목록이 달라질 수 있음
regfit.best <- regsubsets(Salary~.,data=Hitters2, nvmax=19)
coef( regfit.best, 10 )	# 똑같은 10개라도, 선택된 변수가 다르다...!!

k = 10; set.seed(1)
folds <- sample( 1:k, nrow(Hitters2), rep=T )	# test fold에 대한 인덱스.
cv.errors <- matrix( NA, k, 19, dimnames=list(NULL,paste(1:19)) )

for( j in 1:k ){
	best.fit <- regsubsets(Salary~.,data=Hitters2[folds!=j,],nvmax=19)
	for( i in 1:19 ){
		pred <- predict( best.fit, Hitters2[folds==j,], id=i )
		cv.errors [j,i] <- mean( (Hitters2$Salary[folds==j]-pred)^2 )
	}
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors
par( mfrow=c(1,1) )
plot(mean.cv.errors, type='b')

reg.best <- regsubsets(Salary~.,data=Hitters2,nvmax=19)
coef(reg.best,11)
