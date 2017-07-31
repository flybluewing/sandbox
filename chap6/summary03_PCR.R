# continues from summary02_Ridge.R

library(pls)

#-----------------------------------------------------------
#  Principal Component Regression
set.seed(2)
pcr.fit = pcr(Salary~.,data=Hitters2, scale=T, validation="CV" )
summary( pcr.fit )	# CV값은 단순한 오차값임.(MSE가 아님)
validationplot( pcr.fit, val.type="MSEP" )	# ploting

seed(1)
pcr.fit <- pcr(Salary~., data=Hitters2, subset=train, scale=T, validation="CV" )
validationplot( pcr.fit, val.type="MSEP" )
pcr.pred <- predict( pcr.fit, x[test,], ncomp=7 )
mean( (pcr.pred-y.test)^2 )

pcr.fit <- pcr(y~x,scale=T,ncomp=7)
summary(pcr.fit)

#-----------------------------------------------------------
#  Partial Least Squares
set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters2, subset=train, scale=T, validation="CV" )
summary(pls.fit)
pls.pred <- predict( pls.fit, x[test,], ncomp=2 )
mean( (pls.pred-y.test)^2 )
pls.fit <- plsr(Salary~., data=Hitters2, scale=T, ncomp=2 )
summary( pls.fit )
