# to20180526_H.R 최종접근

finalCut.rebCnt0 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 0!=sum(aZoid%in%fObj$lastZoid) ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[1] %in% c( 7)		# decline1
					flag[ 2] <- aZoid[2] %in% c(11)		# decline1
					flag[ 4] <- aZoid[4] %in% c(24)		# decline1
					flag[ 6] <- aZoid[6] %in% c(45)		# rebPtn1.s2
					flag[ 7] <- aZoid[6] %in% c(41)		# decline1.s3
					flag[ 8] <- aZoid[4] %in% c(25)		# rebPtn2.s3
					flag[ 9] <- aZoid[1] %in% c( 4)		# symm1.s3
					flag[10] <- cStep[1] %in% c( 1)		# symm1_C
					flag[11] <- cStep[1] %in% c( 3)		# decline1.s2_C
					flag[12] <- cStep[2] %in% c( 4)		# decline1.s2_C
					flag[13] <- cStep[3] %in% c(13)		# decline1.s2_C
					flag[14] <- cStep[1] %in% c( 4)		# rebAgain.s2_C
					flag[15] <- cStep[2] %in% c( 5)		# rebAgain.s2_C
					flag[16] <- aZoid[1] %in% c(10)		# lastReb.s3
					flag[17] <- fStep[1] %in% c(-5)		# lastReb.s3_F
					flag[18] <- cStep[1] %in% c( 4)		# lastReb_C

					flag[51] <- all(rCode[c(4,5)]==c(6,5))	# (14,15,q6) ,(43,34,q5)
					flag[52] <- aZoid[3] %in% c(15)	# 3(5,10,15?)
					flag[53] <- aZoid[4] %in% c(27)	# 4(9,18,27?)
					flag[54] <- aZoid[1] %in% c( 6)	# 1(18,14,10,6?)
					flag[55] <- fStep[1]==fStep[2]	#  1  1  4 -1  4 -8
					flag[56] <- fStep[3]==fStep[5]	#  1  1  4 -1  4 -8
					flag[57] <- (-2*fStep[5])==fStep[6]	#  1  1  4 -1  4 -8

					flag[50] <- aZoid[3] %in% c(26)		# 12,14,18,26(?)
					flag[60] <- 0<chkHaveSeq( fStep ,c(-5,NA,2) )	# seqReb.s2_F
					flag[70] <- any(dInfo$dLast==aZoid)

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.rebCnt0()


finalCut.rebCnt1 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 1!=sum(aZoid%in%fObj$lastZoid) ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[1] %in% c( 6)		# symm1.s2
					flag[ 2] <- cStep[2] %in% c( 3)		# rebAgain_C
					flag[ 3] <- cStep[4] %in% c( 6)		# rebAgain_C
					flag[ 4] <- cStep[2] %in% c( 3)		# rebPtn1_C
					flag[ 5] <- cStep[4] %in% c( 6)		# rebPtn1_C
					flag[ 6] <- cStep[5] %in% c( 4)		# rebPtn1_C
					flag[ 7] <- cStep[5] %in% c( 1)		# rebPtn2_C
					flag[ 8] <- cStep[2] %in% c( 3)		# symm1_C
					flag[ 9] <- cStep[4] %in% c( 6)		# symm1_C
					flag[10] <- cStep[1] %in% c( 8)		# rebAgain.s3_C
					flag[11] <- cStep[4] %in% c( 6)		# rebAgain.s3_C
					flag[12] <- cStep[5] %in% c( 8)		# rebPtn1.s3_C
					flag[13] <- aZoid[1] %in% c( 1)		# lastReb.s2
					flag[14] <- cStep[2] %in% c( 3)		# lastReb_C
					flag[15] <- cStep[4] %in% c( 6)		# lastReb_C

					flag[50] <- aZoid[2] %in% c(10,20)	# 10,20,? 10 단위가 너무 많음.
					flag[51] <- aZoid[2] %in% c(20)		# 26,23,20?
					flag[52] <- aZoid[3] %in% c(30)		# 32,31,30?
					flag[53] <- aZoid[4] %in% c(38)		# 36,37,38?
					flag[54] <- fStep[2]==fStep[3]		# 13 10 10  5  5  2 
					flag[55] <- fStep[4]==fStep[5]		# 13 10 10  5  5  2 
					flag[56] <- fStep[2]==(fStep[4]*2)	# 13 10 10  5  5  2 
					flag[57] <- fStep[3]==(fStep[5]*2)	# 13 10 10  5  5  2 
					flag[58] <- fStep[2]==(fStep[5]*2)	# 13 10 10  5  5  2 
					flag[59] <- fStep[3]==(fStep[4]*2)	# 13 10 10  5  5  2 
					flag[60] <- all(fStep[2:3]==c(10,10))	# 13 10 10  5  5  2 
					flag[61] <- all(fStep[2:3]==c( 5, 5))	# 13 10 10  5  5  2 
					flag[62] <- (fStep[6]>1) && ( fStep[2]==(fStep[4]*fStep[6]) )	# 13 10 10  5  5  2 
					flag[63] <- (fStep[6]>1) && ( fStep[3]==(fStep[5]*fStep[6]) )	# 13 10 10  5  5  2 
					flag[64] <- (fStep[6]>1) && ( fStep[2]==(fStep[5]*fStep[6]) )	# 13 10 10  5  5  2 
					flag[65] <- (fStep[6]>1) && ( fStep[3]==(fStep[4]*fStep[6]) )	# 13 10 10  5  5  2 
					flag[66] <- all(cStep[c(1,4)]==c(6,6))					#  6  3  8  6  1 
					flag[67] <- (cStep[2]>1) && ( cStep[1]==(2*cStep[2]) )	#  6  3  8  6  1 
					flag[68] <- (cStep[2]>1) && ( cStep[4]==(2*cStep[2]) )	#  6  3  8  6  1 

					flag[70] <- any(dInfo$dLast==aZoid)

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.rebCnt1()



finalCut.nextZWidth <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( 1!=sum(aZoid%in%fObj$lastZoid) ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[1] %in% c(11)		# decline1.s2
					flag[ 2] <- aZoid[2] %in% c(16)		# decline1.s2
					flag[ 3] <- aZoid[1] %in% c(14)		# rebPtn1.s2
					flag[ 4] <- fStep[3] %in% c( 6)		# decline1_F
					flag[ 5] <- fStep[3] %in% c( 8)		# rebAgain.s3_F
					flag[ 6] <- fStep[6] %in% c(-13)	# rebAgain.s3_F
					flag[ 7] <- 0<chkHaveSeq( fStep ,c(11, NA, 8) )	# seqReb_F (11, NA, 8)
					flag[ 8] <- cStep[3] %in% c( 8)		# rebAgain_C
					flag[ 9] <- cStep[5] %in% c( 6)		# rebPtn1_C
					flag[10] <- cStep[5] %in% c( 6)		# lastReb.s2_C
					flag[11] <- cStep[5] %in% c( 7)		# symm1.s2_C
					flag[12] <- cStep[3] %in% c( 5)		# decline1.s3_C
					flag[13] <- cStep[2] %in% c( 3)		# rebPtn1.s3_C

					flag[70] <- any(dInfo$dLast==aZoid)

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

}	# finalCut.nextZWidth()



finalCut.1stC1 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=1 )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[3] %in% c(10)		# rebAgain.s2
					flag[ 2] <- aZoid[6] %in% c(41)		# lastReb.s3
					flag[ 3] <- aZoid[6] %in% c(44)		# symm1.s3
					flag[ 4] <- 0<chkHaveSeq( aZoid ,c(21, 26) )	# seqReb.s3 (21, 26)
					flag[ 5] <- cStep[4] %in% c(16)		# lastReb.s2_C
					flag[ 6] <- cStep[4] %in% c( 5)		# symm1.s2_C
					flag[ 7] <- cStep[3] %in% c(11)		# decline1.s3_C
					flag[ 8] <- cStep[4] %in% c( 3)		# decline1.s3_C

					flag[50] <- (fStep[3]>1) && (fStep[2]==(fStep[3]*2))		# fStep  0  6  3 14  4 -9 
					flag[51] <- (cStep[3]>1) && (cStep[1]==(cStep[3]*3))		# cStep  9  3 13  6  4 
					flag[52] <- (cStep[3]>1) && (cStep[4]==(cStep[3]*2))		# cStep  9  3 13  6  4 

					flag[70] <- any( (dInfo$dLast==aZoid)[-fObj$col] )

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stC1()

finalCut.1stC2 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=2 )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[6] %in% c(36)		# lastReb
					flag[ 2] <- aZoid[2] %in% c( 7)		# rebPtn1
					flag[ 3] <- aZoid[6] %in% c(42)		# symm1
					flag[ 4] <- aZoid[2] %in% c( 8)		# decline1.s2
					flag[ 5] <- aZoid[5] %in% c(19)		# rebPtn1.s2
					flag[ 6] <- aZoid[4] %in% c(38)		# decline2.s3
					flag[ 7] <- fStep[2] %in% c(-2)		# decline1.s2_F
					flag[ 8] <- fStep[4] %in% c(-5)		# decline1.s2_F
					flag[ 9] <- fStep[4] %in% c(-7)		# decline1.s3_F
					flag[10] <- fStep[2] %in% c(-8)		# decline2.s3_F
					flag[11] <- fStep[2] %in% c( 7)		# rebPtn2.s3_F
					flag[12] <- fStep[1] %in% c( 5)		# rebPtn1_C
					flag[13] <- cStep[1] %in% c( 6)		# decline1.s2_C
					flag[14] <- cStep[3] %in% c( 7)		# decline1.s2_C

					flag[70] <- any( (dInfo$dLast==aZoid)[-fObj$col] )

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stC2()

finalCut.1stC5 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=5 )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[3] %in% c(20)		# rebPtn1.s3
					flag[ 2] <- fStep[5] %in% c(12)		# rebAgain.s3_F
					flag[ 3] <- cStep[4] %in% c( 7)		# lastReb.s2_C
					flag[ 4] <- cStep[2] %in% c( 9)		# rebAgain.s2_C
					flag[ 5] <- cStep[4] %in% c( 3)		# symm1.s2_C
					flag[ 6] <- cStep[2] %in% c( 9)		# decline2.s3_C

					flag[70] <- any( (dInfo$dLast==aZoid)[-fObj$col] )

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stC5()

finalCut.1stC7 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=7 )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[4] %in% c(29)		# decline1
					flag[ 2] <- aZoid[6] %in% c(52)		# decline2
					flag[ 3] <- aZoid[2] %in% c(22)		# lastReb.s2
					flag[ 4] <- aZoid[6] %in% c(37)		# decline1.s2
					flag[ 5] <- aZoid[2] %in% c(15)		# symm1.s2
					flag[ 6] <- aZoid[5] %in% c(33)		# symm1.s2
					flag[ 7] <- aZoid[5] %in% c(23)		# rebAgain.s3
					flag[ 8] <- 0<chkHaveSeq( aZoid ,c(22, NA, NA, 34) )	# seqReb.s2 (22, NA, NA, 34)
					flag[ 9] <- fStep[6] %in% c( 8)		# lastReb_F
					flag[10] <- fStep[6] %in% c(-7)		# symm1_F
					flag[11] <- fStep[2] %in% c(12)		# decline1.s2_F
					flag[12] <- 0<chkHaveSeq( fStep ,c(0, NA, NA, NA, NA, 8) )	# seqReb_F (0, NA, NA, NA, NA, 8)
					flag[13] <- cStep[2] %in% c( 2)		# lastReb_C
					flag[14] <- cStep[2] %in% c( 3)		# symm1_C
					flag[15] <- cStep[1] %in% c(15)		# lastReb.s2_C
					flag[16] <- cStep[5] %in% c( 3)		# decline1.s2_C
					flag[17] <- cStep[1] %in% c( 8)		# symm1.s2_C
					flag[18] <- cStep[4] %in% c(10)		# decline1.s3_C
					flag[19] <- cStep[2] %in% c( 1)		# decline2.s3_C

					flag[70] <- any( (dInfo$dLast==aZoid)[-fObj$col] )

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stC7()

finalCut.1stC8 <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=8 )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- aZoid[4] %in% c(21)		# lastReb
					flag[ 2] <- aZoid[3] %in% c(17)		# decline1
					flag[ 3] <- aZoid[6] %in% c(35)		# rebPtn1
					flag[ 4] <- aZoid[4] %in% c(36)		# symm1
					flag[ 5] <- aZoid[6] %in% c(35)		# rebAgain.s2
					flag[ 6] <- fStep[6] %in% c( 5)		# rebAgain_F
					flag[ 7] <- fStep[2] %in% c( 2)		# lastReb.s2_F
					flag[ 8] <- fStep[2] %in% c( 2)		# symm1.s2_F
					flag[ 9] <- fStep[2] %in% c( 2)		# lastReb.s3_F
					flag[10] <- fStep[3] %in% c( 5)		# decline1.s3_F
					flag[11] <- fStep[5] %in% c(-5)		# decline1.s3_F
					flag[12] <- fStep[2] %in% c(-5)		# symm1.s3_F
					flag[13] <- 0<chkHaveSeq( fStep ,c(0, 2) )	# seqReb.s2_F (0, 2)
					flag[14] <- 0<chkHaveSeq( fStep ,c(0, 2) )	# seqReb.s3_F (0, 2)
					flag[15] <- cStep[3] %in% c( 4)		# decline1_C
					flag[16] <- cStep[2] %in% c( 7)		# lastReb.s2_C
					flag[17] <- cStep[3] %in% c( 1)		# decline1.s2_C
					flag[18] <- cStep[2] %in% c( 1)		# symm1.s2_C
					flag[19] <- cStep[4] %in% c(15)		# rebPtn1.s3_C

					flag[50] <- all(rCode[c(3,5)]==c(8,8))	# xxxxx   8  9 18 21 28 40 
					flag[50] <- all(fStep[c(2,5)]==c(-3,-3))	# xxxxx   0 -3 -1  0 -3  5 

					flag[70] <- any(dInfo$dLast==aZoid)

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stC8()



finalCut.1stCx <- function( gEnv ,allIdxF ,uAnaObj ,localZhF ,thld=1 ){

	# ===================================================================================
	fObj <- list( col=1 ,val=x )
	fObj$lastMtx	<- tail(gEnv$zhF ,n=10)
	fObj$lastZoid	<- fObj$lastMtx[nrow(fObj$lastMtx),]
	fObj$lastMtx.l	<- tail(localZhF ,n=10)
	fObj$lastZoid.l	<- fObj$lastMtx.1[nrow(fObj$lastMtx.1),]

	dInfo <- uAnaObj$dInfo

	chkCnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					if( aZoid[fObj$col]!=fObj$val ) return( 0 )

					cStep <- aZoid[2:6]-aZoid[1:5]
					fStep <- aZoid-dInfo$dLast
					rCode <- aZoid%%10
					zWidth <- aZoid[6]-aZoid[1]

					flag <- rep( F ,80 )
					flag[ 1] <- xxxxx[3] %in% c(10)		# 
					flag[ 2] <- xxxxx[6] %in% c(41)		# 
					flag[ 3] <- xxxxx[6] %in% c(44)		# 
					flag[ 4] <- 0<chkHaveSeq( xxxxx ,c(21, 26) )	# seqReb.s3 (21, 26)
					flag[ 5] <- xxxxx[4] %in% c(16)		# 
					flag[ 6] <- xxxxx[4] %in% c( 5)		# 
					flag[ 7] <- xxxxx[3] %in% c(11)		# 
					flag[ 8] <- xxxxx[4] %in% c( 3)		# 
					flag[ 9] <- xxxxx[4] %in% c( 3)		# 
					flag[10] <- xxxxx[4] %in% c( 3)		# 
					flag[11] <- xxxxx[4] %in% c( 3)		# 

					flag[50] <- (xxxxx[3]>1) && (xxxxx[2]==(xxxxx[3]*2))		# xxxxx  0  6  3 14  4 -9 
					flag[51] <- (xxxxx[3]>1) && (xxxxx[1]==(xxxxx[3]*3))		# xxxxx  9  3 13  6  4 

					flag[70] <- any(dInfo$dLast==aZoid)

					return( sum(flag) )
				})
	flag <- chkCnt<=thld			;kIdx<-head(which(!flag))

    allIdxF <- allIdxF[flag]
    # cat(sprintf("allIdxF %d\n",length(allIdxF)))

	return( list( allIdxF=allIdxF ,chkCnt=chkCnt ,fObj=fObj ) )

} # finalCut.1stCx()





