# -[neuralnet]--------------------------------------------------------------------------------
library(neuralnet)
require( Metrics )

data("Boston",package="MASS")
data <- Boston

keeps <- c( "crim" ,"indus" ,"nox" ,"rm" ,"age" ,"dis" ,"tax" ,"ptratio" ,"lstat" ,"medv" )
data <- data[keeps]
# apply( data, 2, function(p){sum(is.na(p))} )

f <- medv ~ crim + indus + nox + rm + age + dis + tax + ptratio + lstat

set.seed(2016)
n = nrow(data)
train <- sample( 1:n ,400 , F )
 
fit <- neuralnet( f ,data=data[train,] ,hidden=c(10,12,20) ,algorithm="rprop+" ,err.fct="sse" ,act.fct="logistic" ,threshold=0.1 ,linear.output=T ) 
	# algorithm did not converge in 1 of 1 repetition(s) within the stepmax 
fit <- neuralnet( f ,data=data[train,] ,hidden=c(10,12,20) ,algorithm="backprop" ,learningrate=0.01 ,err.fct="sse" ,act.fct="logistic" ,threshold=0.1 ,linear.output=T ) 
	# Error in if (reached.threshold < min.reached.threshold) {
	
pred <- compute( fit ,data[-train,1:9] )

round( cor(pred$net.result,data[-train,10])^2 ,6 )
mse( data[-train,10], pred$net.result )
rmse( data[-train,10] ,pred$net.result )



# -[deepnet]--------------------------------------------------------------------------------
require( deepnet )
set.seed( 2016 )
X = data[train,1:9]
Y = data[train,10]

fitB <- nn.train( x=as.matrix(X) ,y=Y ,initW=NULL ,initB=NULL ,hidden=c(10,12,20)
					,learningrate=0.58 ,momentum=0.74 ,learningrate_scale=1
					,activationfun="sigm" ,output="linear" ,numepochs=970 ,batchsize=60
					,hidden_dropout=0 ,visible_dropout=0
				)

Xtest <- data[-train,1:9]
predB <- nn.predict( fitB ,Xtest )
	# 똑같은 값들이 줄줄이 나온다. 제대로 되는 게 하나도 없냐...
round( cor(predB,data[-train,10])^2 ,6 )
mse( data[-train,10] ,predB )
rmse( data[-train,10] ,predB )


# -[RSNNS]--------------------------------------------------------------------------------

data( "PimaIndiansDiabetes2" ,package="mlbench" )
ncol(PimaIndiansDiabetes2);	nrow(PimaIndiansDiabetes2)
str(PimaIndeansDiabetes2)

sapply( PimaIndiansDiabetes2 ,function(p){sum(is.na(p))} )
temp <- PimaIndiansDiabetes2
temp$insulin <- NULL; temp$triceps <- NULL; temp <- na.omit(temp)

y <- temp$diabetes
temp$diabetes <- NULL
temp <- scale(temp)
temp <- cbind(as.factor(y),temp)

set.seed(2016)
n = nrow(temp)
n_train <- 600
n_test <- n-n_train
train <- sample( 1:n ,n_train ,F )

require( RSNNS )

set.seed(2016)
X <- temp[train,1:6]
Y <- temp[train,7]

fitMLP <- mlp( x=X ,y=Y ,size=c(12,8) ,maxit=1000 ,initFunc="Ramdomize_Weights" ,initFuncParams=c(-0.3,0.3)
				,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
				,updateFunc="Topological_Order" ,updateFuncParams=c(0)
				,hiddenActFunc="Act_Logistic" ,shufflePatterns=T
				,linOut=T
			)

predMLP <- sign(predict(fitMLP,temp[-train,1:6]))
table( predMLP ,sign(temp[-train,7]) ,dnn=c("predicted","Observed") )

error_rate <- ( 1-sum(predMLP==sign(temp[-train,7]))/124 )
round( error_rate ,3 )

# detach("package:RSNNS" ,unload=T )

# -[AMORE]--------------------------------------------------------------------------------
library( AMORE )

net <- newff( n.neurons=c(6,12,8,1) ,learning.rate.global=0.01 ,momentum.global=0.5
			,error.criterium="LMLS" ,Stao=NA ,hidden.layer="sigmoid" ,output.layer="purelin"
			,method="ADAPTgdwm"
		)

X <- temp[train,1:6]
Y <- temp[train,7]

fit <- train( net ,P=X ,T=Y ,error.criterium="LMLS" ,report=T ,show.step=100 ,n.show=5 )
pred <- sign(sim(fit$net ,temp[-train,]))
table( pred ,sign(temp[-train,7]) ,dnn=c("Predicted","Observed") )


# -[Multi-Response]--------------------------------------------------------------------------------
# <neuralnet>
data( "bodyfat" ,package="TH.data" )
library( neuralnet )
require( Metrics )

set.seed(2016)
train <- sample( 1:71 ,50 ,F )
scale_bodyfat <- as.data.frame( scale(log(bodyfat)) )
f <- waistcirc + hipcirc ~ DEXfat+age+elbowbreadth+kneebreadth+anthro3a+anthro3b+anthro3c+anthro4
fit <- neuralnet( f ,data=scale_bodyfat[train,] ,hidden=c(8,4) ,threshold=0.1 
				,err.fct="sse" ,algorithm="rprop+" ,act.fct="logistic" ,linear.output=F
			)


without_fat <- scale_bodyfat
without_fat$waistcirc <- NULL; without_fat$hipcirc <- NULL

pred <- compute( fit ,without_fat[-train,] )
head(pred$net.result)

# <deepnet>
set.seed(2016)
X <- as.matrix( without_fat[train,] )
Y <- as.matrix( scale_bodyfat[train,3:4] )
fitB <- nn.train( x=X ,y=Y ,initW=NULL ,initB=NULL ,hidden=c(8,4) ,activationfun="sigm"
				,learningrate=0.02 ,momentum=0.74 ,learningrate_scale=1 ,output="linear"
				,numepochs=970 ,batchsize=60 ,hidden_dropout=0 ,visible_dropout=0
			)

Xtest <- as.matrix(without_fat[-train,])
predB <- nn.predict( fitB ,Xtest )
