    library( ISLR )
    set.seed(1)
    train = sample(392,196)

    attach(Auto)
    lm.fit <- lm(mpg~horsepower, subset=train )
    mean((mpg-predict(lm.fit,Auto))[-train]^2)
    lm.fit2 <- lm(mpg~poly(horsepower,2),subset=train)
    mean((mpg-predict(lm.fit2,Auto))[-train]^2)
    lm.fit3 <- lm(mpg~poly(horsepower,3),subset=train)
    mean((mpg-predict(lm.fit3,Auto))[-train]^2)

[Cross-Validation]

    glm.fit <- glm(mpg~horsepower)
    coef(glm.fit)
    lm.fit <- lm(mpg~horsepower)
    coef(lm.fit)

    # LOOCV (leave one out cross validation)
    library(boot)
    glm.fit <- glm(mpg~horsepower)
    cv.err <- cv.glm(Auto,glm.fit)
    cv.err$delta

    cv.error <- rep(0,5)
    for( i in 1:5 ){
        glm.fit <- glm( mpg~poly(horsepower,i), data=Auto )
        cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
    }
    cv.error

    # k - Fold Cross Validation
    set.seed(17)
    cv.error.10 = rep(0,10)
    for( i in 1:10 ){
        glm.fit <- glm( mpg~poly(horsepower,i), data=Auto )
        cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
    }
    cv.error.10


[Boot Strap]
    
    alpha.fn = function( data, index ){
        X <- data$X[index]
        Y <- data$Y[index]
        rv <- (var(Y)-cov(X,Y)) / (var(X)+var(Y)-2*cov(X,Y))
        return( rv )
    }

    set.seed(1)
    alpha.fn( Portfolio, sample(100,100,replace=T) )
    boot( Portfolio, alpha.fn, R=1000 )
        #    ORDINARY NONPARAMETRIC BOOTSTRAP
        #    Call:
        #    boot(data = Portfolio, statistic = alpha.fn, R = 1000)
        #    Bootstrap Statistics :
        #         original        bias    std. error
        #    t1* 0.5758321 -7.315422e-05  0.08861826

    # Boot Strap for Linear regression
    boot.fn1 = function( data, index ){
        lm.fit <- lm(mpg~horsepower, data=data, subset=index)
        return( coef(lm.fit) )
    }
    boot.fn2 = function( data, index ){
        lm.fit <- lm(mpg~poly(horsepower,2), data=data, subset=index)
        return( coef(lm.fit) )
    }
    set.seed(1)
    boot( Auto, boot.fn1, R=1000 )
    boot( Auto, boot.fn2, R=1000 )


