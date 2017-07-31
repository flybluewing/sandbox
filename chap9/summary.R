set.seed(1)
x = matrix( rnorm(20*2), ncol=2 )
y = c( rep(-1,10), rep(1,10) )
x[y==1,]=x[y==1,]+1
plot( x, col=(3-y) )

#-------------------------------------------------------------
#	9.6.1 Support Vector Classifier
dat <- data.frame( x=x, y=as.factor(y) )
library(e1071)
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=F )
plot( svmfit, dat )
svmfit$index	#	identify Support Vector

#	- cost 값이 작아질수록 margin은 커진다.
svmfit <- svm( y~., data=dat, kernel="linear", cost=0.1, scale=F )

#	Cross Validation : tune()
set.seed(1)
tune.out <- tune( svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)) )
summary( tune.out )

bestmod <- tune.out$best.model
summary( bestmod )

xtest <- matrix( rnorm(20*2), ncol=2 )
ytest <- sample( c(-1,1), 20, rep=T )
xtest[ytest==1,] = xtest[ytest==1,]+1
testdat = data.frame( x=xtest, y=as.factor(ytest) )
ypred = predict( bestmod, testdat )
table( predict=ypred, truth=testdat$y )

#	What about lineary seperable..?
x[y==1,] = x[y==1,] + 0.5
plot( x, col=(y+5)/2, pch=19 )

dat <- data.frame( x=x, y=as.factor(y) )
svmfit <- svm( y~., data=dat, kernel="linear", cost=1e5 )
summary( svmfit )

svmfit <- svm( y~., data=dat, kernel="linear", cost=1 )
summary( svmfit )
plot( svmfit, dat )

#-------------------------------------------------------------
#	9.6.2 Support Vector Machine
set.seed(1)
x <- matrix( rnorm(200*2), ncol=2 )
x[1:100,] <- x[1:100,]+2
x[101:150,] <- x[101:150,]-2
y <- c( rep(1,150), rep(2,50) )
dat <- data.frame( x=x, y=as.factor(y) )
plot( x, col=y )

train <- sample( 200, 100 )
svmfit <- svm( y~., data=dat[train,], kernel="radial", gamma=1, cost=1 )
plot( svmfit, dat[train,] )
summary( svmfit )
    #    Parameters:
    #           SVM-Type:  C-classification 
    #         SVM-Kernel:  radial 
    #               cost:  1 
    #              gamma:  1 
    #    Number of Support Vectors:  35 ( 15 20 )
    #    Number of Classes:  2 
    #    Levels: 1 2

svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5 )
plot( svmfit, dat[train,] )

set.seed(1)
tune.out <- tune( svm, y~., data=dat[train,], kernel="radial", 
					ranges=list( cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4) )
				)
summary( tune.out )

table( true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]) )

#-------------------------------------------------------------
#	9.6.3 ROC curves
library( ROCR )
rocplot <- function( pred, truth, ... ){
		predob <- prediction(pred,truth)
		perf <- performance( predob, "tpr", "fpr" )
		plot(perf,...)
	}

svmfit.opt <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T )
svmfit.pred <- predict( svmfit.opt, dat[train,], decision.values=T )
fitted <- attributes( svmfit.pred )$decision.values
par( mfrow=c(1,2) )
rocplot( fitted, dat[train,"y"], main="Training Data" )

svmfit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T )
svmfit.flex.pred <- predict( svmfit.flex, dat[train,], decision.values=T )
fitted <- attributes( svmfit.flex.pred )$decision.values
rocplot( fitted, dat[train,"y"], add=T, col="red" )

svmfit.opt.pred <- predict(svmfit.opt, dat[-train,], decision.values=T )
fitted <- attributes( svmfit.opt.pred )$decision.values
rocplot( fitted, dat[-train,"y"], main="Test Data" )
fitted.flex.pred <- predict(svmfit.flex, dat[-train,], decision.values=T )
fitted <- attributes( fitted.flex.pred )$decision.values
rocplot( fitted, dat[-train,"y"], add=T, col="red" )

#-------------------------------------------------------------
#	9.6.4 SVM with Multiple Classes
set.seed(1)
x <- rbind( x, matrix(rnorm(50*2),ncol=2) )
y <- c( y, rep(0,50) )
x[y==0,2] <- x[y==0,2] + 2
dat <- data.frame( x=x, y=as.factor(y) )
par( mfrow=c(1,1) )
plot( x, col=(y+1) )

svmfit <- svm( y~., data=dat, kernel="radial", cost=10, gamma=1 )
plot( svmfit, dat )

#-------------------------------------------------------------
#	9.6.5 Application to Gene Expression Data
library( ISLR )
names( Khan )
	#	"xtrain" "xtest"  "ytrain" "ytest" 
dim( Khan$xtrain )	#   63 2308
dim( Khan$xtest )	#   20 2308
length( Khan$ytrain )	#	63
length( Khan$ytest )	#	20

table( Khan$ytrain )
    #     1  2  3  4 
    #     8 23 12 20 
table( Khan$ytest )
    #    1 2 3 4 
    #    3 6 6 5 

dat <- data.frame( x=Khan$xtrain, y=as.factor(Khan$ytrain) )
out <- svm( y~., data=dat, kernel="linear", cost=10 )
summary( out )
    #    Parameters:
    #           SVM-Type:  C-classification 
    #         SVM-Kernel:  linear 
    #               cost:  10 
    #              gamma:  0.0004332756 
    #    Number of Support Vectors:  58 ( 20 20 11 7 )
    #    Number of Classes:  4 
    #    Levels: 1 2 3 4

table( out$fitted, dat$y )
    #     1  2  3  4
    #  1  8  0  0  0
    #  2  0 23  0  0
    #  3  0  0 12  0
    #  4  0  0  0 20
	
dat.te <- data.frame( x=Khan$xtest, y=as.factor(Khan$ytest) )	
pred.te <- predict( out, newdata=dat.te )
table( pred.te, dat.te$y )
    #    pred.te 1 2 3 4
    #          1 3 0 0 0
    #          2 0 6 2 0
    #          3 0 0 4 0
    #          4 0 0 0 5

