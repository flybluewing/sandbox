
		# > tail(zhF)
		# 	E1 E2 E3 E4 E5 E6
		# 784  3 10 23 24 31 39
		# 785  4  6 15 25 26 33
		# 786 12 15 16 20 24 30
		# 787  5  6 13 16 27 28
		# 788  2 10 11 19 35 39
		# 789  2  6  7 12 19 45

dice789 <- function( allZoidMtx ,zhF ,allChosenIdx ){

	lastZoid <- zhF[nrow(zhF),]
	tempAllMtx <- allZoidMtx[allChosenIdx,]

	# Quotient 10 그룹 크기비율 3:2:1...
	stdDist <- table(lastZoid%/%10)	# 3:2:1 비율
	allDistLst <- apply( allZoidMtx[allChosenIdx,]%/%10 ,1 ,table )
	flag <- sapply( allDistLst ,function(p){p[1]!=stdDist[1]} ) # 그냥 3:2:1, 3:2:1, 3:3 모두 제거하자.
	allChosenIdx <- allChosenIdx[flag]

	# reb는 0
	flag <- apply( allZoidMtx[allChosenIdx,] ,1 ,function(p){sum(lastZoid%in%p)} )
	allChosenIdx <- allChosenIdx[flag==0]

	# 2,3컬럼에서의 1차이 제외
	flag <- apply( allZoidMtx[allChosenIdx,2:6] - allZoidMtx[allChosenIdx,1:5]
					,1 ,function( p ){ p[2]!=1 }
				)
	allChosenIdx <- allChosenIdx[flag]

	# 5 컬럼에서의 13 제외
	flag <- apply( allZoidMtx[allChosenIdx,] ,1 ,function(p){ p[5]!=13 } )
	allChosenIdx <- allChosenIdx[flag]

	# 2 컬럼에서의 6 제외
	flag <- apply( allZoidMtx[allChosenIdx,] ,1 ,function(p){ p[2]!=6 } )
	allChosenIdx <- allChosenIdx[flag]

	return( allChosenIdx )

} # dice789()

