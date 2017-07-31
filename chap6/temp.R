

set.seed(1)
train = sample( c(T,F), nrow(Hitters), rep=T )
test = !train

regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat <- model.matrix( Salary~., data=Hitters[test,] )

val.errors = reg(NA,19)
for( i in 1:19 ){
	coefi <- coef(regfit.best, id=i)
	pred <- test.mat[,names(coefi)]%*%coefi
	val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}


coefi <- coef(regfit.best, id=i)
pred <- test.mat[,names(coefi)]	%*% coefi
	
	
	