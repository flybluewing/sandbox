# continues from summary01_BestSubset.R
x = model.matrix( Salary~.,Hitters2 )[,-1]
y = Hitters2$Salary

library(glmnet)

# ridge regression
grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet( x, y, alpha=0, lambda=grid ) # choice ridge regression(alpha)
coef(ridge.mod)

set.seed(1)
train = sample( 1:nrow(x), nrow(x)/2 )
test = (-train)
y.test = y[test]

# 잘 모르겠다.. Ridge와 Lasso를 익히고 나서 하자..
