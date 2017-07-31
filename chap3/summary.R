[library]
	library(MASS)
	library(ISLR)
	
[Boston]
	fix(Boston)
	names(Boston)
	?Boston
		# medv : median house value
		# rm : average numvber of rooms per house
		# age : average age of houses
		# lstat : percent of households with low socioeconomic status
	
[lm]
	lm.fit <- lm(medv~lstat, data=Boston)
		# attach(Boston)
		# lm(medv~lstat)
	
	summary(lm.fit)
	names(lm.fit)
	
	#속성 값 가져오기. (소문자로 시작함을 유의)
	# 	lm.fit$residuals 형태도 가능하나, 함수사용 권장.
	coef(lm.fit)
	residuals( lm.fit )
	rstudent( lm.fit )	# studentized residuals
	
	confint(lm.fit)	# confidence interval
	
	predict( lm.fit, data.frame( lstat=(c(5,10,15)) ), interval="confidence" )
	predict( lm.fit, data.frame( lstat=(c(5,10,15)) ), interval="prediction" )
	
[plot]	
	attach( Boston )
	plot( lstat, medv, pch="+" )		# Ploting. (pch:데이터 표기.
										#	plot( 1:20, 1:20, pch=1:20 ) <-- 함 해봐.
	abline( lm.fit, lwd=3, col="red" )	# 회귀선 추가. (lwd : 라인 두께.)
	
	par( mfrow=c(2,2) )
	plot( lm.fit )	# 회귀 품질검사를 위한 그래프 4개가 동시에... 나이쓰~!!
	plot( predict(lm.fit), residuals(lm.fkt) )
	plot( predict(lm.fit), rstudent(lm.fit) )
	
	plot( hatvalues(lm.fit) )		# leverage 검토
	which.max( hatvalues(lm.fit) )	# 최대 leverage의 index 조사.
		
[P > 1]
	lm.fit <- lm( medv~lstat+age, data=Boston)
		# 다음과 같이 전체 P 사용도 가능
		#	lm.fit <- lm( medv~.-age, data=Boston ) 
		#		: age를 제외한 모든 predictor를 사용한 회귀분석 수행
		#
		#  update()함수를 써서 기존 회귀분석 결과로부터 새로운 분석을 다시 수행할 수도 있다.
		#	lm.fit1 <- update( lm.fit, ~.-age )
		#		lm.fit 결과에서 age를 뺀 회귀분석 수행.
		
	summary( lm.fit )
	summary( lm.fit )$r.sq		# R square
	summary( lm.fit )$sigma		# RSE
	
	# VIF (collinearity)
	library( car )
	vif( lm.fit )
	
[Interaction]
	lm(medv~lstat+age+lstat:age,data=Boston)
		# lm(medv~lstat*age,data=Boston)	<-- 단축코딩 버전
	
[Non-linear regression]
	# 사실 lstat와 medv는 관계는 (lstat^-1)로 모델링하는 게 적절하다.
	lm( medv~lstat+I(lstat^2) )	
		# I( ) : '^'가 formular로 해석되지 않고 (terms.formula)
		#		수치계산 연산자로 사용되도록 I( )함수 사용
		
	anova( lm.linear, lm.nonLinear )	# 2개 회귀결과의 성능비교.
	
	lm( medv~poly(lstat,5), data=Boston )
		# fifth-order polynomial fit
	
	
[Qualitative Predictor]	
	attach( Carseats )
	head( Carseats )
	unique( ShelveLoc )
		[1] Bad    Good   Medium
		Levels: Bad Good Medium
		
	par( mfrow=c(1,2) )
	plot( ShelveLoc, Sales )
	plot( Price, Sales )
	
	lm.seat <- lm( Sales~Price+ShelveLoc )
	summary( lm.seat )
    contrasts( ShelveLoc )      # 각 State 별 대입 코드 확인.
                    Good 	Medium
        Bad		    0	    0
        Good	    1	    0
        Medium	    0 	    1	
	


	