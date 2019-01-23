# toZ843_H.R 최종접근
cntMtx.colName <- c( "raw","rawFV","rem","cStep","fStep"
						,"raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2"
					)

#	centerVal 값이 aCode내에 존재하고, 좌우 값이 있으면 좌우 값 반환.
#		centerVal 값은 aCode내에 다수 존재할 수도 있다.
getSideValues <- function( aCode ,centerVal ){
	# aCode<-1:5	;centerVal<-4
	indices <- which( aCode==centerVal )
	indices <- indices[ !(indices %in% c(1,length(aCode)) ) ]

	rLst <- list()
	for( idx in indices ){
		rLst[[1+length(rLst)]] <- aCode[c(idx-1,idx+1)]
	}

	return( rLst )

} # getSideValues()

# 공용
# undone
fCut.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF	;zMtxLen <- nrow(zMtx)	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	;rptObj<-anaMtx( stdMI$rawTail )

	# col[1] 은 1~15
	flag <- gEnv$allZoidMtx[allIdxF,1]<=15	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	# 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% stdMI$lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	# zoid[2] 에서 quo3 나온 적은 15/814
	flag <- gEnv$allZoidMtx[allIdxF,2]<30	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    # 어느 rem 값이 혼자 3개 이상 나타나는 거 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- max( table(aZoid%%10) )
					return( cnt<3 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
    # rem 동일값 연속  rx,rx,r3,r3,rx,rx - (위험)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    aCode <- aZoid%%10
					return( !any(aCode[2:6]==aCode[1:5]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
    # rem 동일위치 재현 2 이상(위험)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( stdMI$rem==(aZoid%%10) )
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    # rem ptn 재현 3이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
    # rem ptn 재현 4이상 - hIdx-1 에서.
	tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# cStep 반복. 동일 재현이 2개 붙어서 발생.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# quo 4개 이상은 제외(사실 기본 필터에 포함되어있다.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- max(table(aZoid%/%10))
					return( 4 > cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )
} # fCut.default()
# undone
fCutCnt.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )

	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )	# rptObj<-anaMtx( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	# cStep 2,4,6,8 같은 식의 증가 감소(11% 정도라 불안하긴 한데..)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6]-aZoid[1:5]
					for( cIdx in 2:4 ){
						if( (aCode[cIdx]-aCode[cIdx-1])==(aCode[cIdx+1]-aCode[cIdx]) ){
							return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# fStep :  15 ,14 ,-5 , 3 , 4  처럼 연속증가/감소 2개 이상.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid - stdMI$lastZoid
					cnt <- 0
					aFStep.cStep <- aFStep[2:6]-aFStep[1:5]
					if( 1<sum(abs(aFStep.cStep)==1) ) cnt <- cnt + 1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# stdMI$quo10$tbl
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !stdMI$quo10$sameTbl( fCutU.getQuoObj(aZoid)$tbl ) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.default()

# UNdone - working
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	allIdxF <- rmvRaw( gEnv ,allIdxF )
	allIdxF <- rmvColValSeqNext( gEnv ,allIdxF )
	allIdxF <- rmvColValSeqNext.cStep( gEnv ,allIdxF )

	allIdxF <- rmvCStep( gEnv ,allIdxF )
	# allIdxF <- rmvFStep( gEnv ,allIdxF )

	# allIdxF <- rmvQuo10( gEnv ,allIdxF )
	# allIdxF <- rmvZW( gEnv ,allIdxF )
	# allIdxF <- rmvFV3( gEnv ,allIdxF )

	# // \\ 재현

	save( allIdxF ,file="Obj_fCut.basic.save" )

	return( allIdxF )

} # fCut.basic()

# done
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,0,3
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 0,3,0
					#	unique : 1 2 0 3 0 -- * --> 2 1 2 0 1
					#	unique : quoSize[3:5] a 3 a
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  2,15 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,NA,31    ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,NA,33    ),aZoid) ) cnt<-cnt+1
			# <28>
			# <30>
			if( fCutU.hasPtn(c(          30,33    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(          30,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,NA,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,NA,33,40 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 14,NA,NA,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       31,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          36,40 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,3,5    ),c(  2,15 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6,7      ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,3      ),c( 40    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,5,6    ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7        ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6,23    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4, 5    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3, 1, 5 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 3, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  6, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,16 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  7,17 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 16, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 17, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 8,NA, 1,27) ( 1:13,NA, 1,22)
			#			( 3:18, 9,NA, 3, 1) ( 3: 6, 5,NA, 3)
			#			(16: 7, 3,16, 3, 7)
			#	unique	
			#	unique ( 3 a 3 ) (aCStep[2:4] 7,17, 1) ( a,16,a )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (4)   3 (8)   5 (2)   6 (4)   12 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 25 28 30 33 45    |23  3  2  3 12 |                        |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3, 1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique ( 4: 9,10, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -6 (2)   -5 (3)   3 (4)   4 (2)   6 (2)   7 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 24
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 24
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# 27

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.basic()

# done
fCutCnt.colValSeqNext <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

	# =========================================================
	# anaColEndPtn() - 문제 많다. 개량필요.
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(aZoid[banVal.idx]==banVal)
					return( cnt<2 )
				})	;kIdx<-anaFltCnt(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# -- conditional
    # flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				aRem <- aZoid %% 10
	# 				cnt <- 0
	# 				if( 1<sum(aRem[banVal.idx]==(banVal%%10)) ) cnt <- cnt + 1
	# 				return( 1>cnt )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					# [1]    8  4 20  1
					# [2]   40 13
					# [3]   10
					# [4]   26 19 42 21
					# [5]   42 16 31 35 28 36
					# [6]   41 37 15

					if( 1<sum(aZoid==c(  8,40,10,26,42,41 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  4,13,NA,19,16,37 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 20,NA,NA,42,31,15 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  1,NA,NA,21,35,NA ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(       ) ) score<-score+1
					if( aZoid[2]%in%c(       ) ) score<-score+1
					if( aZoid[3]%in%c(       ) ) score<-score+1
					if( aZoid[4]%in%c(       ) ) score<-score+1
					if( aZoid[5]%in%c(       ) ) score<-score+1
					if( aZoid[6]%in%c(       ) ) score<-score+1
					if( score>1 ) cnt<-cnt+1

					return( 1>cnt )
				})	;kIdx<-anaFltCnt(flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )
	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					for( cIdx in banCode.span ){
						if( all(aRem[cIdx+0:1]==banCodeLst[[cIdx]]) ) return(FALSE)
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						for( rIdx in 1:nrow(fndMtx) ){
							if( all(aZoid[cIdx+0:1]==fndMtx[rIdx,]) ){
								cnt <- cnt+1
								break	# 다수 발생된 패턴의 중복 체크 피하기 위해.
							}
						}
					}
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0	# 현재 동일쌍이 전혀 존재치 않음.
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						for( rIdx in 1:nrow(fndMtx) ){
							if( all(aZoid[cIdx+0:1]==fndMtx[rIdx,]) ){
								cnt <- cnt+1
								break	# 다수 발생된 패턴의 중복 체크 피하기 위해.
							}
						}
					}
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional custom
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid%%10

					cnt <- 0
					if( aZoid[1]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 15     ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(        ) ) cnt<-cnt+1

					# [  1] 30 33    18 31    31 32    24 25    37 40
					# [  2] 13 26             42 45    35 37    30 34
					# [  3] 14 16             34 42    38 40    44 45
					# [  4] 29 32             29 42    32 44    38 43
					# [  5]  9 22             37 41    34 45         
					# [  6]  9 17             14 25    23 38         
					# [  7] 34 36             21 29    11 23         
					# [  8]                   25 33    39 44         
					# [  9]                            42 43         
					# [ 10]                            30 36         

					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 3      ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 3,5    ),c( 15 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 0,4    ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 3,2    ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c(        ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 5      ),c(    )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
						# grp (1:2+3)
						# grp (1:2+4)
					if(remCnt>2) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){

					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c(  3,13, 1, 1, 3 ),na.rm=T)
					matCnt <- sum(aCStep==c( 13,NA, 3, 2, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  2,NA, 8, 2, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,NA,13,12, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 13,NA, 4,11,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  8,NA,11,15,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  2,NA, 8,12,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 8, 5,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  3  13   2   3  13   8   2 
					#	[2] 13
					#	[3]  1   3   8  13   4  11   8   8 
					#	[4]  1   2   2  12  11  15  12   5   1   6   2   8  11   4   3 
					#	[5]  3   4   1   5 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1

						if( aCStep[1]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 13     ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  1     ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  2, 3  ) ) cnt<-cnt+1

						if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) )	cnt<-cnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 
					score <- score + ifelse(cnt>1,1,0)

					return( score )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt>=2] <- flgCnt[fltCnt>=2] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
				# -- conditional rem과 val의 조건이 겹치기 때문에 한 군데에서 처리.
					# rem : 한 개 그룹에서 완전 일치는 아니지만, 전체 일치 수가 3이상
					aRem <- aZoid %% 10
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aRem[cIdx+0:1]==banCodeLst[[cIdx]])
					}
					if( cnt>=3 ) return(FALSE)
					# fndMtx 내에서 일치가 1개 이상.
					cnt <- 0
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						for( rIdx in 1:nrow(fndMtx) ){
							if( all(aZoid[cIdx+0:1]==fndMtx[rIdx,]) ){
								cnt <- cnt+1
								break	# 다수 발생된 패턴의 중복 체크 피하기 위해.
							}
						}
					}
					if( cnt>=1 ) return( FALSE )
					# 한 개 그룹에서 완전 일치는 아니지만, 전체 일치 수가 2이상
					cnt <- 0
					for( cIdx in banCode.span ){
						fndVal <- cvSeqNextLst[[cIdx]]$fndMtx[1,]
						if( all(aZoid[cIdx+0:1]==fndVal) ) next

						cnt <- cnt + sum(aZoid[cIdx+0:1]==fndVal)
					}
					if( cnt>=2 ) return( FALSE )

					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )
	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10	# 한 개 그룹에서 2개 이상 rem 일치
					for( cIdx in banCode.span ){
						if( 1<sum(aRem[cIdx+0:2]==banCodeLst[[cIdx]]) ) return(FALSE)
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( cIdx in banCode.span ){ # 값 연속이 2개 이상
						cnt <- cnt + sum(aZoid[cIdx+0:2]==cvSeqNextLst[[cIdx]]$fndMtx[1,])
					}
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10	# 한 개 그룹에서 2개 이상 rem 일치
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						if( 2>nrow(fndMtx) ) next

						for( rIdx in 2:nrow(fndMtx) ){
							if( 1<sum(aRem[cIdx+0:2]==(fndMtx[rIdx,]%%10) ) ) return(FALSE)
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					# [  1]             18 31 32                35 37 40
					# [  2]                                     30 38 43                              

					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.colValSeqNext()                     

# done
fCutCnt.colValSeqNext.cStep <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}) )
	stdMI <- fCutU.getMtxInfo( zMtx )

	# =========================================================
	# anaColEndPtn() - 문제 많다. 개량필요.
	colPtnLst <- anaColEndPtn( zMtx ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]
	valMtx <- NULL
	if( 0<length(banVal.idx) ){
		valLen <- sapply( colPtnLst ,function(colPtn){ length(colPtn$val) })
		valMtx <- matrix( NA, ncol=5 ,nrow=max(valLen) )
		for( colIdx in (1:5)[valLen>0] ){
			valMtx[1:valLen[colIdx],colIdx] <- colPtnLst[[colIdx]]$val
		}
		valCnt <- apply( valMtx ,1 ,function(val){ sum(!is.na(val)) })
		valMtx <- valMtx[valCnt>1,]
	}
	#	<remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 2>sum(aCStep[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]

					# [1]*  13  5  5  4  3  6 | 7  2 17  5
					# [2]*   3  9  3  3  6  5 | 3  5 15  7  5 11  2 25  1  4  4  1  2  2 12 11 21  4  6  9 10 10 14 11  8  4  2  6  4 12  6  3
					# [3]    2  8  6  4 | 
					# [4]*   2  1  5 10  7  4 | 1  6  4  9 12 13  1  6  7  1  8 11  1 10 10  1  3  2  3  6  4  7  1  1  3 15 11  7  6  1 12 15  2  2  ...
					# [5]*  11 13  2  1  8  6 | 2  4  8  4  1  6  3  1  8  2 14  1  8 25  6  1  7 26  5  1  1  1  1  1  4  1  3

					tCnt <- 0
						if( aCStep[1]%in%c( 13, 6, 4 ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  3       ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(          ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  3, 6    ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(          ) ) tCnt<-tCnt+1

						if( aCStep[ 1 ]==sum(aCStep[c(3,5)]) )	cnt<-cnt+1
						if( aCStep[ 1 ]==sum(aCStep[c(4,5)]) )	cnt<-cnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1

					if( is.null(valMtx) ){
						# matCnt 작업을 루프문으로 바꾼 것.
						# 	matCnt<-sum(aCStep==c( 5, 6, 5, 8, 9 ),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
						matFlag <- apply( valMtx ,1 ,function(val){ 2<=sum(aCStep==val,na.rm=T) })
						cnt <- cnt + sum(matFlag)
					}

					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# =========================================================
	# colValSeqNext( ,pColSize=2 )
	cvSeqNextLst <- colValSeqNext( zMtx ,pColSize=2 )
	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )
	# remove
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in banCode.span ){	# 최근 값과 3개 이상 일치
						cnt <- cnt + sum(aCStep[cIdx+0:1]==banCodeLst[[cIdx]])
					}
					return( 3>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in banCode.span ){
						if( all(aCStep[cIdx+0:1]==banCodeLst[[cIdx]]) ) return(FALSE)
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					cnt <- 0	# fndMtx 일치가 2개 이상 발생.
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						if( 2>nrow(fndMtx) ) next

						for( rIdx in 2:nrow(fndMtx) ){
							if( all(aCStep[cIdx+0:1]==banCodeLst[[cIdx]]) ) {
								cnt <- cnt+1
								break	# 다수 발생된 패턴의 중복 체크 피하기 위해.
							}
						}
					}
					return( cnt <2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    fltScore <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# banVal 10 -2     6 14    -2 10    -3 11	#	대각선 영역의 diff
					# [  1]  11  4    10  7     2 13     3 18
					# [  2]   5 17    11  3    17  5    12 10
					cnt <- 0
					aCStep <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						if( 2>nrow(fndMtx) ) next

						banVal <- fndMtx[1,1] + (fndMtx[1,2]-fndMtx[2,1])
						if( aCStep[cIdx+0]==banVal ) cnt<-cnt+1
						banVal <- fndMtx[1,2] + (fndMtx[1,1]-fndMtx[2,2])
						if( aCStep[cIdx+1]==banVal ) cnt<-cnt+1					
					}
					return( cnt )
				})	;kIdx<-anaFltCnt(fltScore,rpt)
	flgCnt[fltScore> 2] <- flgCnt[fltScore> 2] + 2
	flgCnt[fltScore==2] <- flgCnt[fltScore==2] + 1

	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					cnt <- 0
					# [  1]  6 14    13  1     2 13     1 17
					# [  2]  5 15    14  3     1  9     1  6
					# [  3]  8  3     5  6    24  3     8  2
					# [  4]  1  5     6  4    10  9     6  4
					# [  5] 22  5     7  7     8  3     3 15
					# [  6]  6  6     8  3    17  5     3  7
					# [  7] 13  8    22  3     2 19    19  8
					# [  8]  2 10     7 16     1 14     2  8
					# [  9]  3  5     6  5     3  5    10 10
					# [ 10]           7  4    12  3     3  3

					if( aCStep[1]%in%c(  7       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 13,12    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 16, 2, 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  9, 5, 3, 1, 8 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( zMtx ,pColSize=3 )
	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )
	# <remove>
    fltScore <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0	# 마지막 값 연속
					aCStep <- aZoid[2:6]-aZoid[1:5]
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCStep[cIdx+0:2]==cvSeqNextLst[[cIdx]]$fndMtx[1,])
					}
					return( cnt )
				})	;kIdx<-anaFltCnt(fltScore,rpt)
	flgCnt[fltScore> 2] <- flgCnt[fltScore> 2] + 2
	flgCnt[fltScore==2] <- flgCnt[fltScore==2] + 1
	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(        ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  8, 2  ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 15, 4  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  6     ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 18     ) ) cnt<-cnt+1

					# [  1]  6 14  3     8  3  5     2 19  8
					# [  2] 13  8  7     8  2  4    20  5  6
					# [  3]             11 13  3            

					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.colValSeqNext.cStep()

# done
fCutCnt.nextZW <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )	
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  8, 7 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 18    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6, 9       ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  6, 9       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     9,32,41 ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 10,11,NA,NA,18 ),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c(       14,17    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,13,NA,17,27 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <19>			# <23>			# <28>
			# <34>
			if( fCutU.hasPtn(c(          34,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,30,27,34    ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(          34,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,30,27,NA,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(  8,7    ),c(  8, 7 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  3,5    ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  8,3    ),c( 18    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  8,5,6  ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(  6      ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 2    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4, 5, 6 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  2,15 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 4, 7 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  2,15 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 6 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 4, 1, 4,NA, 3) ( 1: 7,NA,10, 1) ( 2: 3, 7, 2) ( 3:16,23, 3, 4) ( 5: 5, 6, 6) ( 7: 2, 7, 7)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (3)   3 (2)   4 (7)   5 (3)   6 (2)   7 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 11 19 25 28 32    | 9  8  6  3  4 |                        |1 2 2 1 0 |1 2 2 1
			#     23 27 28 38 42 43(1) | 4  1 10  4  1 | 21  16   9  13  14  11 |0 0 3 1 2 |3 1 2
			#      4  8  9 16 17 19    | 4  1  7  1  2 |-19 -19 -19 -22 -25 -24 |3 3 0 0 0 |3 3
			#      6 18 31 34 38 45    |12 13  3  4  7 |  2  10  22  18  21  26 |1 1 0 3 1 |1 1 3 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  0  -7 -16 -17 -15  -5 |1 3 1 0 1 |1 3 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 |  1  -2   9  12  11  -2 |2 0 2 2 0 |2 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  9,11 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(  9: 9,11, 8) (11: 5,10,11)
			#	unique	(-19:-19,-19)
			# -------------------------------------------------------------------------------------
			#     FV :    -19 (3)   -2 (2)   9 (2)   11 (2)   21 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-6, 1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,1,2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,1,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextZW()

# done
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 33 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(4,0,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 16    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,13       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,15,NA,26,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6, 8,NA,10,27,37 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,17          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,18,37 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(    14,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# <15>
			# <16>
			if( fCutU.hasPtn(c( 16,NA,38 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  9,18,29,29 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(  6,19,25 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 12,NA,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,24,NA,37 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 16,NA,38 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c( 18,22,11,30,41 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(    14,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,NA,NA,NA,35,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,4    ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,3    ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,6,7  ),c( 17 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6      ),c( 16 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 7      ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2      ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 10, 1   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	UNIQUE ( 1: 3,25, 1) ( 4: 6,NA,15,17, 4) ( 6: 6, 5,NA,16) (10:10, 1,11,10*, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   3 (3)   4 (4)   6 (3)   7 (3)   8 (2)   10 (4)   21 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(3,3)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(4,5)])==sum(aCStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  5 15 18 19 23    | 3 10  3  1  4 |                        |2 3 1 0 0 |2 3 1
			#      6 16 37 38 41 45    |10 21  1  3  4 |  4  11  22  20  22  22 |1 1 0 2 2 |1 1 2 2
			#      6 12 19 24 34 41(2) | 6  7  5 10  7 |  0  -4 -18 -14  -7  -4 |1 2 1 1 1 |1 2 1 1 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  8   3  -3  -7   4   4 |0 4 0 1 1 |4 1 1
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
			#     12 18 24 26 39 40    | 6  6  2 13  1 | 10   8  10   4   7   4 |0 2 2 1 1 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -6     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -5     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -4     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	( 4: 12,13,NA,NA,10, 4) ( 8: 8,17,11,NA, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -4 (2)   4 (5)   8 (2)   10 (2)   22 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(2,1)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,4)]*c(1,1)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextQuo10()

# done
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(2,0,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+1]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  8    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 29    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 23,17 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 35,12 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,31,25,24 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(  4,11,19,41 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 10,15          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    15,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    15,31,44    ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  9,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       24,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,24,33    ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c(  7,NA,25,31 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  8, 5,NA, 9,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 11,28,36,NA,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(  4, 8,13,14,40 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 8       ),c(  8    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9       ),c( 29    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,7,4   ),c( 23,17 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,2     ),c( 35,12 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1,13, 3, 2, 6    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4, 2    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4, 1    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 1, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 1, 3,NA,NA,14 ) ( 4: 4, 2,22,16) (15:15, 9, 2)
			#	unique	( 4: 5, 4 ) (13:13, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (3)   3 (3)   4 (4)   5 (3)   6 (2)   10 (2)   13 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 20 22 25 38 40    |13  2  3 13  2 |                        |1 0 3 1 1 |1 3 1 1
			#     12 15 19 26 40 43(1) | 3  4  7 14  3 |  5  -5  -3   1   2   3 |0 3 1 0 2 |3 1 2
			#     10 11 15 25 35 41(1) | 1  4 10 10  6 | -2  -4  -4  -1  -5  -2 |0 3 1 1 1 |3 1 1 1
			#      3  4  9 24 25 33(1) | 1  5 15  1  8 | -7  -7  -6  -1 -10  -8 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2,-1  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  1     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 23     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  1, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+0]==c( -1,-5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -5,-1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(-5:-7,-5,-1) (-1:-13,-10, -8, -1,-15,-14) ( 2:-2, 2,NA,NA,-9)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (3)   -6 (2)   -5 (2)   -4 (2)   -2 (2)   -1 (2)   1 (3)   2 (3)   3(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(5,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14
			if( sum(aFStep[c(5,3)])==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextBin()

# done
fCutCnt.nextRebNum <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,3,0)) ) return(FALSE)	# next rebind of 1,2,0
					#	unique quosize[3:5] a,3,a
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 15    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 31,40 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 37    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,17,21 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40,40 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(  7,26,28 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 11,31,NA,36 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c( 21,NA,35,NA,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,6,4    ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,2,7    ),c( 15    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,0,3    ),c( 31,40 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,9,4    ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7        ),c( 37    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 16, 5, 3, 6, 4 ) ) cnt<-cnt+1	#  6, 6, 4... unique
			if( aCStep[4]%in%c(  3, 8, 4, 5 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -	# unique. 5,4,3...2?
			if( fCutU.hasPtn(c(  6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 3:18, 9,NA, 3, 2) ( 3: 6, 5,NA, 3) ( 4:23,NA, 4,NA, 4)
			#			( 5:26, 2, 5) (10: 7, 3, 2,10) (16: 9,NA,16, 4, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    2 (2)   3 (5)   4 (3)   5 (4)   6 (4)   8 (4)   10 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 15 20 25 33 43    | 8  5  5  8 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#     11 17 21 26 36 45    | 6  4  5 10  9 |  4   2   1   1   3   2 |0 2 2 1 1 |2 2 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -8     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -8     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 18, 6 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+2]==c( -17, -5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -10, -6 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(-10: -3,-10,-6) (18:18,NA,18, 6, 0, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -7 (2)   -5 (2)   1 (3)   2 (2)   3 (2)   4 (2)   6 (5)   18 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 24
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 24
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# 27

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextRebNum()

# done
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(2,0,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+1]==c(1,0,3)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+2]==c(0,3,1)) ) return(FALSE)	# next rebind of 0,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  6, 3, 8 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  3       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 29       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 35       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,NA,NA, 8,22 ),aZoid) ) cnt<-cnt+1
			# < 6>
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,20 ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			# <14>
			# <20>
			if( fCutU.hasPtn(c(  7,NA,20 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,20 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  9,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       24,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,24,33    ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 11,16,27 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  8, 5,NA, 9,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c(  9, 9, 8,14,24,44 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,3,8    ),c(  6, 3, 8 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c(  3       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,9      ),c( 29       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,0,4    ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5        ),c( 35       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2        ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3, 2      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4, 5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 10, 4, 5, 7 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 1, 3,NA,NA,14) ( 2: 2,NA, 1, 7, 3) ( 6: 6,10,20) (15:15, 9, 2)
			#	unique	( 7: 7, 7 )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (4)   4 (2)   5 (4)   6 (2)   7 (3)   9 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  5 14 20 42 44    | 2  9  6 22  2 |                        |2 1 1 0 2 |2 1 1 2
			#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  9   9   1   4 -15 -12 |0 3 2 1 0 |3 2 1
			#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 | -3  -9 -11  -3  -3  -7 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2,-3, 3     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  1      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2:-1, 2,NA,16,-9) ( 3: 1, 3, 3,-15) ( 8: 7, 1, 1,-15, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -6 (2)   -3 (3)   1 (4)   2 (2)   3 (3)   8 (2)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(5,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14
			if( sum(aFStep[c(5,3)])==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextCStepBin()

# done
fCutCnt.nextFStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,0,0)) ) return(FALSE)	# next rebind of 2,0,3
					if( all(quoSize[1:3+2]==c(0,0,3)) ) return(FALSE)	# next rebind of 0,3,0
					if( all(quoSize[1:3+0]==c(0,0,3)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+1]==c(0,3,2)) ) return(FALSE)	# next rebind of 3,1,1
					#	unique : 	0 3 1 1 --*--> 1 2 0 3
					#				2 0 3 0 --*--> 1 0 0 3
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c( 13    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 26,12 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 13    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 32    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,NA,NA,30 ),aZoid) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c(  4,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,20,31 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  8,19,16,NA,NA,38 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <40>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(          ),c( 13    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(          ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  8, 6, 2 ),c( 26,12 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  9, 3    ),c( 13    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  2, 9    ),c( 32    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(  4       ),c( 44    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7, 3, 2     ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 9   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  7      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3, 9   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 16, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  1, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,16 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 30, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 1,17,15,10) ( 3: 7, 3,NA,NA, 3) ( 3: 3, 3*,19 ) ( 7: 5,NA,18, 7) (16: 3,16, 3, 4)
			#	unique	aZoid[2:4]-(30, 1, 1)
			#	unique	 a, 16, a
			#	unique	( 1: 1, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   3 (6)   5 (3)   6 (3)   7 (3)   16 (4) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 20 26 28 35 40    |16  6  2  7  5 |                        |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  6  33  18  16  11  -1 |1 0 0 3 2 |1 3 2
			#      2  3 12 20 27 38(1) | 1  9  8  7 11 | -5 -34 -26 -19 -13  -6 |2 1 2 1 0 |2 1 2 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1
			#      5 11 14 30 33 38    | 6  3 16  3  5 | -5  -4  -4   9  -1  -3 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  6     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  6     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (3)   -5 (3)   -4 (2)   -3 (2)   -1 (2)   6 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,6)]*c(1,-3)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1	# -8

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

}	# fCutCnt.nextFStepBin( )

# done
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 29, 36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
					#	unique : 	1 1 2 2 -> 1 2 1 2
					#				2 1 2 0 -> 0 2 1 2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  4    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 25    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,36,39,44,NA,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c(    10,NA,26       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,10,13,NA,37,31 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  9,18          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    18,29,32,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <22>
			# <25>
			if( fCutU.hasPtn(c(    25,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,25,NA,41 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 10,23,33,40,43 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 36,37 ),aZoid) ) cnt<-cnt+1
			# <37>
			if( fCutU.hasPtn(c( 36,37 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,3,4,9  ),c(  4 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,0      ),c( 25 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,4      ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4        ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,0      ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,9,7    ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  8, 9      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  8,13       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 7, 9 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 8 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  7,10 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1: 7, 5, 1) ( 7: 7, 5, 1) ( 7:15, 7,10) ( 8:14, 8, 3,NA, 9)
			#	unique	( 3: 6, 3)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   3 (3)   5 (3)   6 (3)   7 (6)   8 (3)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 10

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2 22 27 33 36 37(2) |20  5  6  3  1 |  0  14  12  11  11  -4 |1 0 2 3 0 |1 2 3
			#     11 18 21 36 37 43(2) | 7  3 15  1  6 |  9  -4  -6   3   1   6 |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -8   2 -12  -6  -4 |1 1 2 2 0 |1 1 2 2
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  3   0  -5   1   3  -4 |1 2 1 2 0 |1 2 1 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  15  15  13   6  10 |0 1 1 2 2 |1 1 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( 10     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 3,-1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 0: 0,-24,-10,-5,-19) ( 1: 10, 6,-13, 1, 0)
			# -------------------------------------------------------------------------------------
			#     FV :    -8 (2)   -6 (2)   -4 (4)   0 (2)   1 (2)   3 (3)   6 (2)   10 (2)   11 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(1,1)==aFStep[c(6,3)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_1()

# done
fCutCnt.nextColVal_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,2,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,1,0
					if( all(quoSize[1:3+2]==c(3,1,1)) ) return(FALSE)	# next rebind of 1,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 17,32,16 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(  4       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 34,43,32 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,25,NA,NA,45    ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,21,40,43 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			# <13>
			# <17>
			if( fCutU.hasPtn(c(  3, 3,17,33,30 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,32 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c(  1,14,29 ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 17,NA,32 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    28,32 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 18, 9,NA, 2,17,34 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,4        ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(            ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7         ),c( 17       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,2,6     ),c( 17,32,16 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6,4       ),c(  4       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,3,5,2,0 ),c( 34,43,32 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 13      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  8, 1   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  8      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 1, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 3, 9 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2: 2,15,17) ( 2: 2, 3,15) ( 3:12,19, 3) ( 6: 5,10,NA,NA, 6)
			#	unique	( 1: 1, 1) ( 2: 2, 2) ( 8: 2, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (4)   3 (4)   5 (2)   6 (3)   8 (3)   9 (2)   13 (3) 
			cnt.w2 <- 0
			if( aCStep[ 3 ]==sum(aCStep[c(1,2  )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ 4 ]==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  3 11 26 37 43    | 1  8 15 11  6 |                        |2 1 1 1 1 |2 1 1 1 1
			#     17 20 29 35 38 44    | 3  9  6  3  6 | 15  17  18   9   1   1 |0 1 2 2 1 |1 2 2 1
			#      4  9 13 18 21 34    | 5  4  5  3 13 |-13 -11 -16 -17 -17 -10 |2 2 1 1 0 |2 2 1 1
			#      4 17 30 32 33 34(2) |13 13  2  1  1 |  0   8  17  14  12   0 |1 1 0 4 0 |1 1 4
			#     11 13 15 17 25 34(2) | 2  2  2  8  9 |  7  -4 -15 -15  -8   0 |0 4 1 1 0 |4 1 1
			#      1  8  9 17 29 32(1) | 7  1  8 12  3 |-10  -5  -6   0   4  -2 |3 1 1 1 0 |3 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  -6    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -14    ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  -2    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	( 0: -5, 5,-4, 0) (17: 1,17,10,15,-1)
			# -------------------------------------------------------------------------------------
			#     FV :    -17 (2)   -15 (2)   -10 (2)   0 (4)   1 (2)   17 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c( 5, 3,-2)==aFStep[c( 1, 3, 5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_2()

# done
fCutCnt.nextColVal_3 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 38,39,42 ) ) return( FALSE )
					#	unique : 33,**,36,**,39,**,? 42
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 28    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 16,40 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7, 8,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(  6,14       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,28,22 ),aZoid) ) cnt<-cnt+1
			# <26>			# <27>			# <31>			# <35>
			# <37>
			if( fCutU.hasPtn(c( 37,NA,40 ),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c( 37,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             40,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,14, 9,17,40    ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(  9,12,42 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  3,34,40,41,30,43 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6       ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,8,6,2 ),c( 28    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0       ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,7,5   ),c( 16,40 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,2,4   ),c( 44    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3, 1   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 9, 4 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2:17, 2, 7,24) ( 9: 9, 4,29) 
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (3)   3 (5)   4 (4)   9 (2)   13 (3)
			cnt.w2 <- 0
			if( aCStep[ 5 ]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      9 26 35 37 40 42    |17  9  2  3  2 |                        |1 0 1 2 2 |1 1 2 2
			#      7 20 22 27 40 43(1) |13  2  5 13  3 | -2  -6 -13 -10   0   1 |1 0 3 0 2 |1 3 2
			#      8 14 18 30 31 44    | 6  4 12  1 13 |  1  -6  -4   3  -9   1 |1 2 0 2 1 |1 2 2 1
			#      5 27 31 34 35 43(1) |22  4  3  1  8 | -3  13  13   4   4  -1 |1 0 1 3 1 |1 1 3 1
			#      3  7 14 23 26 42    | 4  7  9  3 16 | -2 -20 -17 -11  -9  -1 |2 1 2 0 1 |2 1 2 1
			#      6 16 37 38 41 45    |10 21  1  3  4 |  3   9  23  15  15   3 |1 1 0 2 2 |1 1 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -9    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   3    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-6&1: 4,-6, 5,NA,-18, 1) (-2:-2,NA,-21,-12,-18,-3) (-1:-1,NA,NA,-25,-22,-1)
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -6 (2)   -2 (2)   -1 (2)   1 (3)   3 (3)   4 (2)   13 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(3,4,5,6)==aFStep[c(2,4,5,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,4)]*c(1,1)==aFStep[c(6,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..
} # fCutCnt.nextColVal_3()

# done
fCutCnt.nextColVal_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  4    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4,13,42,38,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5,38,NA,39,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,12 ),aZoid) ) cnt<-cnt+1
			# < 9>
			# <12>
			if( fCutU.hasPtn(c(  6,12 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,12,24,NA,40 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(    19,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    19,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,19,27       ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  9,20,NA,24,43 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c(    12,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,NA,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     3,NA,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       12,NA,25,29 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(    19,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       34,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 19,27,34    ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(    19,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       34,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,27,NA,41 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,7      ),c(  4 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,9,4    ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2        ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,2,0    ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2        ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9        ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  7      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  5,6,9  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 6 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1:  29, 3, 1, 6) ( 2: 2, 2*, 5, 3) ( 5: 10, 5, 5) ( 6: 1,13, 6) (13: 7,NA,13, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (2)   5 (3)   6 (3)   7 (4)   9 (2)   13 (2) 
			cnt.w2 <- 0
			if( aCStep[1]==sum(aCStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  5  6 12 25 37    | 1  1  6 13 12 |                        |3 1 1 1 0 |3 1 1 1
			#      3  4  9 24 25 33(2) | 1  5 15  1  8 | -1  -1   3  12   0  -4 |3 0 2 1 0 |3 2 1
			#     15 19 21 34 41 44    | 4  2 13  7  3 | 12  15  12  10  16  11 |0 2 1 1 2 |2 1 1 2
			#      7  9 12 14 23 28    | 2  3  2  9  5 | -8 -10  -9 -20 -18 -16 |2 2 2 0 0 |2 2 2
			#      6 12 19 24 34 41(1) | 6  7  5 10  7 | -1   3   7  10  11  13 |1 2 1 1 1 |1 2 1 1 1
			#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -1,-8  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  9     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 10     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  9     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-1:-1,17,17, 6,-1,-5) ( 3:-1, 3, 2,20,32) (10:-9,10,13, 3) (11: -17,-6, 4, 4,11)
			#			(12:-14, 5,12, 6,-6,-2) (12:12, 1,-2,-2 )
			# -------------------------------------------------------------------------------------
			#     FV :    -1 (4)   3 (2)   10 (3)   11 (2)   12 (4) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(3,2)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(5,6)]*c(2,2)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(5,6)]*c(2,3)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,6  )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# 17
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,3,6)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_4()

# done
fCutCnt.nextColVal_5 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 34 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 29,10 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			if( fCutU.hasPtn(c(  9,15,22,28,34 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,11,26,43,43 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c(       20,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6, 5,NA,21,43 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <29>
			# <34>
			if( fCutU.hasPtn(c(  9, 3,28,27,34 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(  6, 8,16,10,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 21, 7,26,28,42 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,9,2    ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2        ),c( 12    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,9,0    ),c( 29,10 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,9,0    ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,0      ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 2 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  9, 5 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 10, 8 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 15, 9 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 9 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1: 4,12, 1) ( 2: 3,12, 2) ( 2: 2,15, 1) ( 4:26, 2,NA, 4) 
			#			( 5:20, 5,NA, 5) ( 8: 2, 8,NA,26) ( 8: 8,NA,22, 8)
			#	unique  ( 3: 3, 8) ( 3: 3, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (3)   3 (4)   4 (5)   5 (3)   8 (5)   10 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      9 33 36 40 42 43    |24  3  4  2  1 |                        |1 0 0 2 3 |1 2 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 | -4 -18 -16  -9  -8  -1 |1 1 1 2 1 |1 1 1 2 1
			#      6 10 17 18 21 29    | 4  7  1  3  8 |  1  -5  -3 -13 -13 -13 |1 3 2 0 0 |1 3 2
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 | -4   0  -3   4  11   7 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  6   1   5  -1   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 | -1  -2   5   8  -2  -7 |2 0 2 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  5, -3 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -3     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-13:-13,-13)
			#	unique	(-3:-9, 5,-3,21,NA,27) (-1:-1,-8, 1) ( 1: 1,15, 1,21)
			#			( 4: 6,10, 1, 4, 7) ( 5:-8,-5, 5,17,-8,-23)
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (3)   -4 (2)   -3 (2)   -2 (2)   -1 (3)   1 (2)   4 (2)   5 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-4, 1)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,5,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(3,2,5)]) )	cnt.w2<-cnt.w2+1	# 1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# 4

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_5()

# done
fCutCnt.nextColVal_6 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
	colnames(cntMtx) = cntMtx.colName

	cnt <- 0	;cnt.w1 <- 0	;cnt.w2 <- 0
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
	
		if( TRUE ){	# raw
			cnt <- 0
			if( aZoid[1]%in%c(           ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12        ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(           ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 23        ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29,38     ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,44,36  ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 8>
			# < 9>
			if( fCutU.hasPtn(c(  2, 9,10,31,32 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,15,20 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  7,18 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 23,29 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(  3,32,42 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  7,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     7,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0       ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3       ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,3     ),c( 23       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,8,4   ),c( 29,38    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,4,6   ),c( 42,44,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  9      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6, 3   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 12, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5,12 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 2,10 )) )	cnt.w1<-cnt.w1+1
			#	unique	( 1: 1,13, 7,23) ( 4: 3,NA,18, 4,19) ( 5: 18, 5,18) (12: 5,12, 2,22) (17:19,17,10)
			#	unique	( 5: 5,15) (12: 5,12) (12: 7:12)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   4 (5)   5 (4)   7 (3)   11 (2)   12 (3)   17 (2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 | -3  -3 -11 -12 -10  -2 |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  5     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -2     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-4&-1:-4, 7,23,NA,-1) (-2: 4,-2,-3,-2) ( 1&3: 3,-4, 1,-3, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (3)   -3 (2)   -2 (3)   -1 (3)   1 (5)   3 (2)   5 (2) 
			cnt.w2 <- 0
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,3,6)]) )	cnt.w2<-cnt.w2+1	#  9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_6()


#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================

fltCntMtx		<- function( ccObjLst ,allIdxF ){
	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })
	phName <- attributes(cntMtxLst)$names

	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF
	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawCnt <- apply( rawMtx ,1 ,function(rData){ 
					if( any(rData>2) ) return( 100 )

					evtCnt <- sum(rData>1)
					if( evtCnt >2 ) return( 100 ) else return( evtCnt )
	})
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	rawFVCnt <- apply( rawFVMtx ,1 ,function(rData){ 
					if( any(rData>2) ) return( 100 )

					evtCnt <- sum(rData>1)
					if( evtCnt >1 ) return( 100 ) else return( evtCnt )
	})
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )
	remCnt <- apply( remMtx ,1 ,function(rData){ 
					if( any(rData>3) ) return( 100 )

					evtCnt <- sum(rData>2)
					if( evtCnt >2 ) return( 100 ) else return( evtCnt )
	})
	
	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )
	cStepCnt <- rep( 0 ,nrow(cStepMtx) )
	for( rIdx in 1:nrow(cStepMtx) ){
		cStep <- cStepMtx[rIdx,]
		cStep.w <- cStepMtx.w1[rIdx,] + cStepMtx.w2[rIdx,]
		cStep.all <- cStep + cStep.w

		if( any(cStep>2) || any(cStep.w>2) || (cStep.all>3) ){
			cStepCnt[rIdx] <- 100
			next
		}



	}


	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )
	fStepCnt <- rep( 0 ,nrow(fStepMtx) )



	fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( fltFlag )

} # fltCntMtx()

flagScoreMtx		<- function( ccObjLst ,allIdxF ){

} # flagScoreMtx()

flagCStepValMtx		<- function( ccObjLst ,allIdxF ){

} # flagCStepValMtx()

# done		fCut.basic() 사용
rmvRaw <- function( gEnv ,allIdxF ){
    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		# fCutCnt.basic()
		#      2 25 28 30 33 45    |23  3  2  3 12 |                        |1 0 2 2 1 |1 2 2 1
		#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
		#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
		#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
		#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
		#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
		if( any(aZoid[c(4,2,3,2,1)]==stdMI$lastZoid[c(5,1,3,3,3)]) ){	surviveFlg[idx]<-FALSE	;next }	# 1개 중복 기존 패턴. h-5,h-4,h-3,h-2,h-1
		if( any(aZoid%in%c(14)) ){	surviveFlg[idx]<-FALSE	;next }	# 11 중복 3번, 14도 3번?
		if( any(aZoid%in%c(32)) ){	surviveFlg[idx]<-FALSE	;next }	# col 3에서의 중복 3번 연속, 다음에도?
		aRem <- aZoid%%10
		if( all(aRem[c(2,3)]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 
		#	if( 2<sum(aZoid==c( 8,18,17,32,33,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
			# 동일 증감이... basic은 없겠지만 다른 ph는 유효할 듯.
		#	if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  0  -7 -16 -17 -15  -5 |1 3 1 0 1 |1 3 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 |  1  -2   9  12  11  -2 |2 0 2 2 0 |2 2 2
		aRem <- aZoid%%10
		if( all(aRem[2:3]==aRem[4:5]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 너무 이쁘장한 패턴.
		if( 2<sum(aZoid==c( 8, 7,33,41,NA,36),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextQuo10
			#      6 16 37 38 41 45    |10 21  1  3  4 |  4  11  22  20  22  22 |1 1 0 2 2 |1 1 2 2
			#      6 12 19 24 34 41(2) | 6  7  5 10  7 |  0  -4 -18 -14  -7  -4 |1 2 1 1 1 |1 2 1 1 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  8   3  -3  -7   4   4 |0 4 0 1 1 |4 1 1
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
			#     12 18 24 26 39 40    | 6  6  2 13  1 | 10   8  10   4   7   4 |0 2 2 1 1 |2 2 1 1
		if( 2<sum(aZoid[c(1,6)]==c(12,18,24,26,39,40)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
		if( 2<sum(aZoid==c(22,26,34,30,NA,44),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextBin
			#      3  4  9 24 25 33(1) | 1  5 15  1  8 | -7  -7  -6  -1 -10  -8 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2
		if( 2<sum(aZoid==c( 8,11,37,41,35,32),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 2<sum(aZoid[c(1,5)]==c( 7, 9,24,29,34,38)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(1 두개가 동시발생.)

		# fCutCnt.nextRebNum
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
		# if( 2<sum(aZoid[c(1,1)]==c(14,26,32,36,39,42)[c(2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(1 두개가 동시발생.) 불가!
		if( 2<sum(aZoid[c(1,3)]==c(14,26,32,36,39,42)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)

		# fCutCnt.nextCStepBin
			#      3  4  9 24 25 33    | 1  5 15  1  8 | -3  -9 -11  -3  -3  -7 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2
		if( 2<sum(aZoid[c(1,5)]==c( 7, 9,24,29,34,38)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(1 두개가 동시발생.)
		if( 2<sum(aZoid==c( 8,11,37,41,35,32),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextFStepBin
			#      4 20 26 28 35 40    |16  6  2  7  5 |                        |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#	...
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1
			#      5 11 14 30 33 38    | 6  3 16  3  5 | -5  -4  -4   9  -1  -3 |1 2 0 3 0 |1 2 3
		if( 2<sum(aZoid[c(2,3)]==c( 5,11,14,30,33,38)[c(1,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(NA, 7,10,39,32,35),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_1
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2 22 27 33 36 37(2) |20  5  6  3  1 |  0  14  12  11  11  -4 |1 0 2 3 0 |1 2 3
			#     11 18 21 36 37 43(2) | 7  3 15  1  6 |  9  -4  -6   3   1   6 |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -8   2 -12  -6  -4 |1 1 2 2 0 |1 1 2 2
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  3   0  -5   1   3  -4 |1 2 1 2 0 |1 2 1 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  15  15  13   6  10 |0 1 1 2 2 |1 1 2 2
		if( 2<sum(aZoid[c(1,2)]==c(16,25,33,38,40,45)[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 2<sum(aZoid[c(4,5)]==c(16,25,33,38,40,45)[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		#	rebind 결과 컬럼이 계속 2 이긴...
		rebColIdx <- which( aZoid %in% c(16,25,33,38,40,45) )
		if( 0<length(rebColIdx) && any(rebColIdx==2) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextColVal_2
			#      4  9 13 18 21 34    | 5  4  5  3 13 |-13 -11 -16 -17 -17 -10 |2 2 1 1 0 |2 2 1 1
			#      4 17 30 32 33 34(2) |13 13  2  1  1 |  0   8  17  14  12   0 |1 1 0 4 0 |1 1 4
			#     11 13 15 17 25 34(2) | 2  2  2  8  9 |  7  -4 -15 -15  -8   0 |0 4 1 1 0 |4 1 1
			#      1  8  9 17 29 32(1) | 7  1  8 12  3 |-10  -5  -6   0   4  -2 |3 1 1 1 0 |3 1 1 1
		if( 2<sum(aZoid[c(1,6)]==c( 1, 8, 9,17,29,32)[c(1,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 2<sum(aZoid[c(4,6)]==c( 1, 8, 9,17,29,32)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( all(aZoid[c(4,6)]==c(17,32) ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			#      3  7 14 23 26 42    | 4  7  9  3 16 | -2 -20 -17 -11  -9  -1 |2 1 2 0 1 |2 1 2 1
			#      6 16 37 38 41 45    |10 21  1  3  4 |  3   9  23  15  15   3 |1 1 0 2 2 |1 1 2 2
		if( aZoid[1:2]==c( 9,25) && 1<sum(aZoid%in%c( 6,16,37,38,41,45)) ) {	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. & 2reb

		# fCutCnt.nextColVal_4
			#      4  5  6 12 25 37    | 1  1  6 13 12 |                        |3 1 1 1 0 |3 1 1 1
			#      3  4  9 24 25 33(2) | 1  5 15  1  8 | -1  -1   3  12   0  -4 |3 0 2 1 0 |3 2 1
			#	...
			#      6 12 19 24 34 41(1) | 6  7  5 10  7 | -1   3   7  10  11  13 |1 2 1 1 1 |1 2 1 1 1
			#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
		aRem <- aZoid%%10
		if( all(aRem[c(1,2)]==aRem[c(6,4)]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 
		if( 2<sum(aZoid[c(2,5)]==c( 5,22,31,32,39,45)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)

		# fCutCnt.nextColVal_5
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  6   1   5  -1   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 | -1  -2   5   8  -2  -7 |2 0 2 2 0 |2 2 2
		if( 2<sum(aZoid==c( 6, 7,29,37,32,31),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_6
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
		if( 2<sum(aZoid==c(11, 4,23,NA,NA,42),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvRaw()

# done		fCut.basic() 사용
rmvColValSeqNext <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		# anaColEndPtn()
		if( 1<sum(aZoid==c(  8,40,10,26,42,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  4,13,NA,19,16,37 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 20,NA,NA,42,31,15 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  1,NA,NA,21,35,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  3,13, 1, 1, 3 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 13,NA, 3, 2, 4 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  2,NA, 8, 2, 1 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  3,NA,13,12, 5 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 13,NA, 4,11,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  8,NA,11,15,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  2,NA, 8,12,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }

		if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		# [  1]             18 31 32                35 37 40
		# [  2]                                     30 38 43 		
		if( all(aZoid[2:4]==c(18,31,32)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[4:6]==c(35,37,40)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[4:6]==c(30,38,43)) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext()

# (no Custom)done		fCut.basic() 사용 - custom 부분이 없을 듯.
rmvColValSeqNext.cStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}) )
	stdMI <- fCutU.getMtxInfo( zMtx )

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( zMtx ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]
	valMtx <- NULL
	if( 0<length(banVal.idx) ){
		valLen <- sapply( colPtnLst ,function(colPtn){ length(colPtn$val) })
		valMtx <- matrix( NA, ncol=5 ,nrow=max(valLen) )
		for( colIdx in (1:5)[valLen>0] ){
			valMtx[1:valLen[colIdx],colIdx] <- colPtnLst[[colIdx]]$val
		}
		valCnt <- apply( valMtx ,1 ,function(val){ sum(!is.na(val)) })
		valMtx <- valMtx[valCnt>1,]
	}

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		if( !is.null(valMtx) ){
			# matCnt 작업을 루프문으로 바꾼 것.
			# 	matCnt<-sum(aCStep==c( 5, 6, 5, 8, 9 ),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
			matCnt <- apply( valMtx ,1 ,function(val){ sum(aCStep==val,na.rm=T) })
			if( any(matCnt>2) ){	surviveFlg[idx]<-FALSE	;next }
		}

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext.cStep()

# UNdone		fCut.basic() 사용
rmvCStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}


	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		# fCutCnt.basic()
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 8,NA, 1,27),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(13,NA, 1,22),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 6, 5,NA, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(18, 9,NA, 3, 1),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 7, 3,16, 3, 7),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,4)]==c(3,3)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[ 2:4  ]==c( 7,17, 1)) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aCStep,16) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 1, 4,NA, 3),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA,10, 1),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 7, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 16,23, 3, 4 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 6, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 7, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			#	UNIQUE ( 1:) ( 4:) ( 6:) (10:)
			if( 1<sum( aCStep[ 3 ]*c(3,3)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,25, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,15,17, 4 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 1,11,10, 1 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3,NA,NA,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 2,22,16 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 5, 4) ,c(13, 2) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( aCStep[3]==c(4) ){	surviveFlg[idx]<-FALSE	;next }						# unique 6,6,4?
			if( fCutU.hasPtn(c( 18, 9,NA, 3, 2 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23,NA, 4,NA, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3, 2,10 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,NA,16, 4, 2 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3,4)]==c(2,5,5)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3,NA,NA,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA, 1, 7, 3 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10,20 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aCStep,16) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[2:4]==c(30, 1, 1)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,17,15,10 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,NA,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 3,19 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,NA,18, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,16, 3, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 5, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 7,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 8, 3,NA, 9 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			if( fCutU.hasPtn(c(  2,15,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 3,15 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12,19, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,NA,NA, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 1, 1) ,c( 2, 2) ,c( 2, 8) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			if( fCutU.hasPtn(c( 17, 2, 7,24 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 4,29 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( fCutU.hasPtn(c( 29, 3, 1, 6 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 2, 5, 3 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 5, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,13, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA,13, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,12, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,12, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,15, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2,NA, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 20, 5,NA, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 8,NA,26 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA,22, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 3, 8) ,c( 3, 4) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( fCutU.hasPtn(c(  1,13, 7,23 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA,18, 4,19 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18, 5,18 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,12, 2,22 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 19,17,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 5,15) ,c( 5,12) ,c( 7,12) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# if( fCutU.hasPtn(c( ,, ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fltRst <- sapply( getSideValues(aCStep,11) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		# if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	dbgZoid[,2:6]-dbgZoid[,1:5]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvCStep()

# UNdone		fCut.basic() 사용
rmvFStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMILst <- fCutU.getStdMI( gEnv )

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[c(3,4)]*c(1,2)==aFStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-1,16,15),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(5,6)]*c(1,2)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( fCutU.hasPtn(c(-2,-4,-3,-1,15),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( fCutU.hasPtn(c(-3,-10,-6),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( fCutU.hasPtn(c( 2,NA,NA,NA, -5, -3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(-1, 1)==aFStep[c(6,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5, -9,-15, -6, -3, 3),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(-1,NA,NA,NA, 4, 6),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감.

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[c(2,5)]*c(1,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-1, 6, 7,NA, 0, 0),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-2, 3, 3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3, 3,-17),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3, 3,NA, 3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3,12,-9, 4,18),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c( 2,10,12,NA, 5, 6),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[c(2,4)]*c(-1, 1)==aFStep[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -8,NA,-1,NA,NA, 0),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(10,11, 3,NA,NA,-8),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 4, 2,-3)==aFStep[c(1,3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(11, 2, 0),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(NA,-1, 6,10,NA,-11),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[c(4,5)]*c(1,1)==aFStep[c(3,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3,-1, 5,-2),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }	# -25
			if( fCutU.hasPtn(c(-1,-8,-1, 2),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5,-1, 0,-11,-3),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aFStep,-8) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(10, 4,-2,-8,NA,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감.

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvFStep()

# UNdone		fCut.basic() 사용
rmvFV3 <- function( gEnv ,allIdxF ){

	# Error in if (!is.null(fixIdx) && !matFlag[fixIdx]) { : 
	# TRUE/FALSE가 필요한 곳에 값이 없습니다  <--- fixIdx가 NA를 가리킨 경우.

    initSize <- length(allIdxF)

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		#	fCutCnt.basic
			# < 2>
			if( fCutU.hasPtn(c( 2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11,NA,31,23 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 13,14,43,33,40 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			# <30>
			if( fCutU.hasPtn(c(  8,NA,NA,30,33,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 13,14,NA,33,40 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextZW
			# < 1>
			if( fCutU.hasPtn(c(  1, 6, 5, 5,32 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 2>
			if( fCutU.hasPtn(c(  2,27,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 4>
			if( fCutU.hasPtn(c(  4,13,13,32 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,25,14,18,31 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  2,10,20,NA,17 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 13,14,19,32,40 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>			# <28>
			# <38>
			if( fCutU.hasPtn(c( 23,26,38 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 35,NA,NA,42,45 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c(  3,38,NA,NA,NA,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextQuo10
			# < 3>
			# < 8>
			if( fCutU.hasPtn(c(  2, 8,15 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <10>
			if( fCutU.hasPtn(c( 10,14,23,37,28 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			# <19>
			if( fCutU.hasPtn(c(  2,10,13, 8, 9,19 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c( 26,31,NA,45 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>			# <43>

		#	fCutCnt.nextBin
			# < 3>
			if( fCutU.hasPtn(c(  3, 4, 8,14,28 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c( 10,12,25,23,24 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			# <15>
			if( fCutU.hasPtn(c( 13,15,26 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  5, 6,18,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 15,19,NA,NA,32 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <35>

		#	fCutCnt.nextRebNum
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c( 5,NA,NA,17,21 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c( 11,16,21,NA,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 23,24,31,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>			# <45>

		#	fCutCnt.nextCStepBin
			# <12>			# <24>
			# <32>
			if( fCutU.hasPtn(c( 12, 7,19,NA,32 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 13,NA,36,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			if( fCutU.hasPtn(c( 13,36,36,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextFStepBin
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,17,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 5>
			if( fCutU.hasPtn(c(  5,NA, 7,27 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6,17,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			# <13>
			if( fCutU.hasPtn(c( 13,20,29,24,31 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>			# <16>
			# <17>
			if( fCutU.hasPtn(c( 12,17,NA,NA,30 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			if( fCutU.hasPtn(c( 12,12,22,28,35 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			# <41>
			if( fCutU.hasPtn(c(  1, 6,15,NA,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_1
			# < 2>
			if( fCutU.hasPtn(c(  2,10,17,25,29,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  5,10,19,12,30,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  6,11,19,NA,33 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c(  7, 4,13,32,30,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 12,NA,19,41,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  2,13,19, 9,31,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 10,12,30,32,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  6, 9,12,10,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  2,13,19, 9,31,39 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }  

		#	fCutCnt.nextColVal_2
			# < 5>
			if( fCutU.hasPtn(c(  5,11,12,16,NA,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,18,NA,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  2,10,NA,21 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c(  3, 4,13,37,23,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			# <21>
			if( fCutU.hasPtn(c(  2,10, 7,21 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  4, 7,NA,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  9,16,NA,NA,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>

		#	fCutCnt.nextColVal_3
			# < 1>
			if( fCutU.hasPtn(c(  1,14,15,24,30,30 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  1, 4,11,41,NA,39 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c( 13,27,22,27 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>
			if( fCutU.hasPtn(c(  4, 2,26,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c( 18,23,18,34,42 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 22,27,33,NA,42,45 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c( 22,27,33,43,42,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }		

		#	fCutCnt.nextColVal_4
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,NA,26,30,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			# <17>
			if( fCutU.hasPtn(c( 17,45,NA,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 25,33,36,36 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 25,33,36 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <44>
			if( fCutU.hasPtn(c( 13,22,44 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>

		#	fCutCnt.nextColVal_5
			# < 5>
			if( fCutU.hasPtn(c(  5,13,12,28,27,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  6, 7,NA,33,33,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,16,NA,27,29 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c(  5,NA,12,28,27,45 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			# <25>
			if( fCutU.hasPtn(c( 24,NA,NA,25,37,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c(  3, 3,14,29,NA,34 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  7,16, 5,33 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  2, 4, 9,33,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  9,17,NA,32,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		

		#	fCutCnt.nextColVal_6
			# < 3>
			# <10>
			if( fCutU.hasPtn(c( 10,14,13,28,43  ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			# <16>
			if( fCutU.hasPtn(c(  7,16,NA,37,45 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 14,10,19 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c( 21,36,30,36 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c( 20,22,NA,25,32,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c( 12,17,17,31,45 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 18,17,27,19,33,34 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )
}	# rmvFV3()

# UNdone		fCut.basic() 사용
rmvQuo10 <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)

	surviveFlg <- rep( TRUE ,length(allIdxF) )			;dbgLst <- list()
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		quoSize <- fCutU.getQuoObj( aZoid )$size
		# dbgLst[[1+length(dbgLst)]] <- paste( quoSize,collapse="," )

		# fCutCnt.nextColVal_1	: 1 3 0 2 0 --> 1 2 1 2 0
		if( all(quoSize==c( 1, 2, 1, 2, 0)) ){	surviveFlg[idx]<-FALSE	;next }
	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	allIdxF <- allIdxF[surviveFlg]
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( NA )","nextColVal_1( 37 )","nextColVal_5( 40 )","nextFStepBin( NA )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c(37,40) )]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )
}	# rmvZW()


cust.byOnePhase <- function( ccObj ,phName ){

    #   ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
    cntMtx <- ff0.byOnePhase.getCntMtx( ccObj )


    filtedLst <- vector("list",nrow(cntMtx) )
    filtedFlg <- rep( FALSE ,nrow(cntMtx) )

    for( rIdx in 1:nrow(cntMtx) ){
        score <- cntMtx[rIdx,c("raw","rawFV","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW","auxQuo")]

		# QQE customizing...

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))


    return( list(filtedLst=filtedLst ,filtedFlg=filtedFlg) )

}	# cust.byOnePhase()






