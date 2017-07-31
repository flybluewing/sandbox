library( tree )
library( ISLR )
attach(Carseats)
High <- ifelse( Sales <=8, "No", "Yes" )
Carseats <- data.frame( Carseats, High )

tree.carseats <- tree(High~.-Sales,Carseats )
plot( tree.carseats )
text( tree.carseats, pretty=0 )

#	Validation
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~.-Sales, Carseats, subset=train)
tree.pred <- predict( tree.carseats, Carseats.test, type="class" )
table( tree.pred, High.test )

#	-	CV & pruning process
set.seed(3)
cv.carseats <- cv.tree( tree.carseats, FUN=prune.misclass )
names( cv.carseats )
cv.carseats
par( mfrow=c(1,2) )
plot( cv.carseats$size, cv.carseats$dev, type="b" )
plot( cv.carseats$k,	 cv.carseats$dev, type="b" )

#	- prune tree (select 9 terminal nodes )
prune.carseats <- prune.misclass( tree.carseats, best=9 )
plot( prune.carseats )
text( prune.carseats, pretty=0 )

tree.pred <- predict(prune.carseats, Carseats.test, type="class" )
table( tree.pred, High.test )


#-------------------------------------------------------------------------
#	8.3.2 Fitting Regression Tree
library(MASS)
set.seed(1)
train <- sample( 1:nrow(Boston), nrow(Boston)/2 )
tree.boston <- tree( medv~., Boston, subset=train )
summary( tree.boston )
plot( tree.boston )
text( tree.boston, pretty=0 )

yhat <- predict( tree.boston, newdata=Boston[-train,] )
boston.test <- Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean( (yhat-boston.test)^2 )	#	25.05

#-------------------------------------------------------------------------
#	8.3.3 Bagging and Random Forests

#	Bagged model
library( randomForest )
set.seed(1)
bag.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=T )
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean( (yhat.bag-boston.test)^2 )	#	13.16
#	-	test set
bag.boston <- randomForest(medv~.,data=Boston, subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean( (yhat.bag-boston.test)^2 )

#	Random Forests
set.seed(1)
rf.boston <- randomForest( medv~.,data=Boston, subset=train, mtry=6, importance=T )
yhat.rf <- predict( rf.boston, newdata=Boston[-train,] )
mean( (yhat.rf-boston.test)^2 )
importance( rf.boston )
	#			%IncMSE 	IncNodePurity
	# crim    12.4532926    1025.78688
	# zn       2.7950457      52.75363
	# indus   10.8424427     982.29679
varImpPlot( rf.boston )	# ploting importance

#	Boosting(Boosted regression tree)
library( gbm )
set.seed(1)
boost.boston <- gbm( medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4 )
		#	"gaussian" : for regression
		#	"bernoulli": for classification
summary( boost.boston )
    #                var     rel.inf
    #    lstat     lstat 45.96758091
    #    rm           rm 31.22024973
    #    dis         dis  6.80540085
    #    crim       crim  4.06995214

par( mfrow=c(1,2) )
plot( boost.boston, i="rm" )
plot( boost.boston, i="lstat" )
yhat.boost <- predict( boost.boston, newdata=Boston[-train,], n.trees=5000 )
mean( (yhat.boost-boston.test)^2 )

boost.boston <- gbm(medv~.,data=Boston[train,], distribution="gaussian", n.tree=5000, interaction.depth=4, shrinkage=0.2, verbose=F )
mean( (yhat.boost-boston.test)^2 ) 


