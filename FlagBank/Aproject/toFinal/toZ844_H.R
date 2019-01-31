# toZ844_H.R 최종접근
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
	allIdxF <- rmvFStep( gEnv ,allIdxF )

	allIdxF <- rmvQuo10( gEnv ,allIdxF )
	allIdxF <- rmvZW( gEnv ,allIdxF )
	allIdxF <- rmvFV3( gEnv ,allIdxF )
	save( allIdxF ,file="Obj_allIdxF.fCut.basic.save")

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
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+1]==c(1,1,3)) ) return(FALSE)	# next rebind of 1,1,3
					if( all(quoSize[1:3+2]==c(1,3,1)) ) return(FALSE)	# next rebind of 1,3,1
					#	unique : 	0 1 1 3 1 --> 0 1 1 3 1  , 1 2 0 3 0
					#				1 2 0 3 0 --*--> 1 2 0 3 0
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
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 43       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,38,41 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,NA,31    ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,NA,NA,36    ),aZoid) ) cnt<-cnt+1
			# <19>
			# <30>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,28,30    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,28,NA,33 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 14,NA,NA,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       31,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          36,40 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 24,16,28,30,29,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0       ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,4     ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3       ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,6,2,4 ),c( 43       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,8,1   ),c( 42,38,41 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 5,12 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 16, 2    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3, 1    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3,16 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 16, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 2,11,NA, 1, 2) ( 2: 2*,11,NA, 1, 2) ( 3&16: 7, 3*,16*, 3*, 8) ( 3: 6, 5,NA, 3)
			#	unique	( 3, 16, 3 )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (4)   3 (7)   5 (2)   6 (4)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(4,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      9 14 17 33 36 38    | 5  3 16  3  2 |                        |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1

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
			if( fCutU.hasPtn(c( -5, 0 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+4]==c(  7, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 6,-5 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(-5: 3,-13, -6,-10, -5) ( 0:-5,-5,0) ( 4:27,29, 9,10, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -5 (5)   0 (2)   3 (2)   4 (2)   6 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-1,-1)==aFStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1

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
					# [1]   18 25  6 22 15 10
					# [2]    9 18 25 24 30 35
					# [3]*  31 26 26 33 33
					# [4]   27 44
					# [5]   33 38 39 31 35 24 43
					# [6]   27 40 26 42 35 36 40 42

					if( 1<sum(aZoid==c( 18, 9,31,27,33,27 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 25,18,26,NA,38,40 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  6,25,26,NA,39,26 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 22,24,33,NA,35,35 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 10,35,NA,NA,24,36 ) ,na.rm=T) ) cnt<-cnt+1
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
					if( aZoid[1]%in%c( 12        ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 20        ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(           ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 32,37     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 12,30     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41,44,33  ) ) cnt<-cnt+1	# 11의 배수가 나오기는..

					# [  1] 12 14    33 35    36 39    16 17    12 28
					# [  2] 18 19    12 14    33 36    18 34    38 42
					# [  3] 18 28    29 34     8 33    33 35    38 44
					# [  4] 15 18    20 27    25 35    10 38    33 43
					# [  5] 30 34             22 35    35 41    37 42
					# [  6] 21 23             13 18    34 42    13 22
					# [  7] 20 24             28 33    41 44    25 41
					# [  8] 27 28             22 30    29 31    34 41
					# [  9] 10 19             36 37    29 33    23 31
					# [ 10] 33 35                      37 43         


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 2       ),c( 12       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 0,4     ),c( 20       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 5,2,6   ),c(          )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 7,2     ),c( 32,37    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 2,8     ),c( 12,30    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 1       ),c( 41,44,33 )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							if( aZoid[1]==18   && fCutU.remFilt(aZoid[2],c(0),c(12)) ) remCnt <- remCnt+1
							if( aZoid[2]==28   && fCutU.remFilt(aZoid[1],c(9),c(20)) ) remCnt <- remCnt+1
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
							if( aZoid[4]==33   && fCutU.remFilt(aZoid[3],c(8),c( )) ) remCnt <- remCnt+1
						# grp (1:2+3)
						# grp (1:2+4)
							if( aZoid[6]==41   && fCutU.remFilt(aZoid[5],c(6),c(12,30)) ) remCnt <- remCnt+1
							if( aZoid[6]==42   && fCutU.remFilt(aZoid[5],c(9),c(12,30)) ) remCnt <- remCnt+1
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

					score  <- sum(aCStep==c(  2, 2, 3, 1,16 ),na.rm=T)
					matCnt <- sum(aCStep==c(  1, 2, 3,16, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 10, 5,25, 2, 6 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 7,10,28,10 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  4,NA,13, 6, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  2,NA, 5, 8, 9 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  4,NA, 5, 3,16 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1,NA, 8, 2, 7 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  9,NA, 1, 4, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  2   1  10   3   4   2   4   1   9   2  11  15   1   3   1 
					#	[2]  2   2   5   7 
					#	[3]  3   3  25  10  13   5   5   8   1 
					#	[4]  1  16   2  28   6   8   3   2   4   6   4  13  14 
					#	[5] 16   4   6  10   5   9  16   7   8 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 3 ),aCStep) ) cnt<-cnt+1
						if( 1<sum(aCStep[1:2+1]==c(  2, 3 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  3, 2  ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 1  ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(  1,16  ),aCStep) ) cnt<-cnt+1

						if( aCStep[1]%in%c(  3, 1,10 ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  2, 5    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(  3,25    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  4       ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  3       ) ) cnt<-cnt+1

						if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt<-cnt+1
						if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt<-cnt+1
						if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,4)]) )	cnt<-cnt+1	# 4

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
					# [  1]                         25 35 41    20 34 41
					# [  2]                         22 35 39                                     

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

					# [1]*   6  4  7  6  5  3 | 17  2  3  4  3  3 14  3  6
					# [2]*   9  9  2  9  5  6 | 16 10  4  3  8  1  1
					# [3]*   8 17 11  8  4  1 |  7  8  3  5  3  3  3  9  6  9  6 17  7 11  4  4  9  5  2  9  9  2  1  6  8  4  6  3  2  1  1 13  3  5 14  8
					# [4]*   1  1  1  3  5  8 |  1  1 21  7  4  2 11  8  2  7  7 11  7  7  5 13 12  6  3 16  9 13 14 12 11 22 13  4  1 10 11 10  5 10 14  3  1  3...
					# [5]*   9 10  1  5 25  1 |  8 10  3  6 15  5  1  3  6 10  1  8  7  2  7  8  3 16 13  3  3  3  7  4  2  1  2

					tCnt <- 0
						if( aCStep[1]%in%c(  7          ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  9, 2, 6, 8 ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(             ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  1, 3, 7    ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(  8, 1, 3    ) ) tCnt<-tCnt+1

						if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt<-cnt+1
						if( aCStep[5]==sum(aCStep[c(3,4)]) )	cnt<-cnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					if( fCutU.hasPtn(c( 6, 9 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+0]==c( 7, 2 )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 9, 8 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+0]==c( 2,11 )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 8 ),aCStep) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 5 ),aCStep) )	cnt<-cnt+1
					#	unique	( 6, 9, 8)
					#	unique	( 6, 1 ) ( 9,17 ) ( 1, 8 ) ( 8, 1 ) ( 7, 3 ) ( 2,12 )

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
					# [  1] 19  4     9  1     5  2    17  4
					# [  2]  4  7     5  4    10  3     1 11
					# [  3]  3  1     5  6     1  7     1  5
					# [  4]  3  2     9  4    11  4     5 25
					# [  5]  5  8     9  4     5 17     2  4
					# [  6]  6 16    18  2     1 12     2  8
					# [  7]  9  9     1  8     6  8    12  6
					# [  8]  4 20     3 10     3  8    14 10
					# [  9] 10  2     4 13     1  3    13  6
					# [ 10] 10  9    13  2    10  6     1  3

					if( aCStep[1]%in%c(  5                   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  9                   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  4, 2, 9, 1,         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  1, 2,17             ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(                      ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  1, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 12, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  8, 9 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+1]==c(  1, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  6, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  9, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  2, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  5, 4 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+2]==c(  6, 9 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 11, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 11, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 13, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3,11 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c(  4, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  4, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  1, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 11, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c(  2, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c(  9, 8 )) ) cnt<-cnt+1

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
					if( aCStep[1]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

					# [  1]              1  4  3     6  7  1
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
			if( aZoid[1]%in%c( 10,11,14 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 13,12,23 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 20       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,13       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,16,NA,44,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			# <11>
			if( fCutU.hasPtn(c( 11,NA,23,27 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 12,14 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 15,NA,27 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 29,33 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 29,33 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(          35,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,11,35    ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,1,4    ),c( 10,11,14 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,0      ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,2,0    ),c( 13,12,23 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,4,0    ),c( 20       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,2      ),c( 12       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0        ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1, 5,18 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 17       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 18, 4 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (3)   4 (5)   5 (2)   6 (2)   8 (3) 
			#	unique	( 1: 4, 1,26,NA,18) ( 2: 2, 6, 1) ( 3: 2, 3, 9) ( 4:11,NA,18, 4) ( 6: 6,NA,26, 5,17) ( 8:10,NA, 8,29, 3)
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 13 25 26 29 33    | 2 12  1  3  4 |                        |0 2 3 1 0 |2 3 1
			#     17 23 27 35 38 43    | 6  4  8  3  5 |  6  10   2   9   9  10 |0 1 2 2 1 |1 2 2 1
			#     10 14 16 18 27 28(1) | 4  2  2  9  1 | -7  -9 -11 -17 -11 -15 |0 4 2 0 0 |4 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -5  -8  -5  -4  -6  13 |2 2 1 0 1 |2 2 1 1
			#      2 10 11 19 35 39(1) | 8  1  8 16  4 | -3   4   0   5  14  -2 |1 3 0 2 0 |1 3 2
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 |  3   1   1  10  -2   5 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -2     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	( -2: 2, 2,-3, 6,-2) ( 5: 5,16,-4, 5*) (10: -4,10,-6, 1) (10: 4,-7,-7,10)
			#	unique	a, a, 10
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -5 (2)   -2 (2)   1 (2)   5 (2)   9 (2)   10 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,6)]*c(1, 2)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,5)]*c(1,-5)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 8

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(4,0,1)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 40    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6, 8,NA,10,27,37 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,17          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,18,37 ),aZoid) ) cnt<-cnt+1
			# <14>
			# <16>
			if( fCutU.hasPtn(c( 16,NA,38 ),aZoid) ) cnt<-cnt+1
			# <19>
			# <24>
			if( fCutU.hasPtn(c( 12,NA,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,24 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 34,43 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 16,NA,38 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c( 18,22,11,30,41 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(    14,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,NA,NA,NA,35,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,9      ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7        ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,0      ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5        ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,3,4,2  ),c( 40 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,0,5    ),c( 40 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4, 3, 2, 1 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4, 3       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2: 2, 5, 5) ( 4: 6,NA,15,17, 4) ( 6: 6, 5,NA,16) (10:10, 1,11,10*, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (2)   3 (2)   4 (3)   6 (3)   7 (3)   8 (3)   10 (3)   21 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(4,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 16 37 38 41 45    |10 21  1  3  4 |                        |1 1 0 2 2 |1 1 2 2
			#      6 12 19 24 34 41(2) | 6  7  5 10  7 |  0  -4 -18 -14  -7  -4 |1 2 1 1 1 |1 2 1 1 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  8   3  -3  -7   4   4 |0 4 0 1 1 |4 1 1
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
			#     12 18 24 26 39 40    | 6  6  2 13  1 | 10   8  10   4   7   4 |0 2 2 1 1 |2 2 1 1
			#     19 21 30 33 34 42    | 2  9  3  1  8 |  7   3   6   7  -5   2 |0 1 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  7, -9  ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  4, 5   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 6, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 7, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	( -7:20,24, 8,-7,12) ( 3: 6, 3,15,21,-14, 0) ( 7: 6,-4, 8, 7,-14)
			#			(  4:12,13,23,15,10, 4) ( 4: 8,17,11,21, 4) ( 4:17,19,27, 4) ( 4:23,23,16, 4)
			#			(  8: 8,17,11,21, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -5 (2)   -4 (2)   3 (2)   4 (4)   7 (3)   8 (2)   10 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(1,2)==aFStep[c(4,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,6)]*c(1,3)==aFStep[c(4,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(2,6)]*c(2,3)==aFStep[c(3,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1	# 9
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(4,3)]) )	cnt.w2<-cnt.w2+1	# 9

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
					if( all(quoSize[1:3+0]==c(3,0,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,3,0
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,2,1
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
			if( aZoid[1]%in%c(  3, 9  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 14,31  ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 16,32  ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42     ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,14          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,13,NA,NA,41,36 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,NA,29    ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(  3,NA,14          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,14,NA,41,36 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(  3,NA,NA,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,16,41,36 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    16,NA,31 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  9,NA,NA,31 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    16,NA,31 ),aZoid) ) cnt<-cnt+1
			# <33>
			# <36>
			if( fCutU.hasPtn(c( 10,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 23,15,33,36,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c(  4,34,25,24,35,44 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,9     ),c(  3, 9 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,2     ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,1     ),c( 14,31 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,2     ),c( 16,32 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6,2     ),c( 42    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4, 3   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 20, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  2, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 6 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  7, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 7: 7*, 8,12,NA, 7)
			#	unique	( 10, 2 ) (  7, 8 )
			# -------------------------------------------------------------------------------------
			#     FV :    2 (5)   3 (2)   4 (2)   5 (4)   6 (3)   7 (3)   8 (2)   10 (2)   20 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(3,2)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[ 4 ]*c(3,5)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  7 14 16 31 40    | 4  7  2 15  9 |                        |2 2 0 1 1 |2 2 1 1
			#     10 20 33 36 41 44    |10 13  3  5  3 |  7  13  19  20  10   4 |0 1 1 2 2 |1 1 2 2
			#      1  6 11 28 34 42    | 5  5 17  6  8 | -9 -14 -22  -8  -7  -2 |2 1 1 1 1 |2 1 1 1 1
			#      3 10 14 16 36 38    | 7  4  2 20  2 |  2   4   3 -12   2  -4 |1 3 0 2 0 |1 3 2
			#      7 27 29 30 38 44(1) |20  2  1  8  6 |  4  17  15  14   2   6 |1 0 2 2 1 |1 2 2 1
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  1 -12  -8   1  -5  -6 |1 1 1 3 0 |1 1 1 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -7     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -5     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-12:-1,-12,-18, 6) (-8:16, 2,-8*, 9,-8) ( 2: 6,30,27,26, 2,16)
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -8 (2)   1 (2)   2 (3)   4(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,6)]*c(1,2)==aFStep[c(4,2)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6,1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# -11
			if( sum(aFStep[c(4,2)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# -11

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
					if( (aZoid[6]-aZoid[1]) %in% c( 32 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+1]==c(1,1,3)) ) return(FALSE)	# next rebind of 1,1,3
					if( all(quoSize[1:3+2]==c(1,3,1)) ) return(FALSE)	# next rebind of 1,3,1
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					#	unique 0 1 1 3 1, 1 2 0 3 0
					#	unique a,3,a	( 아예 q3를 제외시켜 버릴까...)
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
			if( aZoid[1]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 33      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 37      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 34      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,38   ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,17,21 ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			# <21>
			# <26>
			if( fCutU.hasPtn(c(  7,26,28 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,28,30    ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,28,NA,33 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 11,31,NA,36 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c( 21,NA,35,NA,39 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 24,16,28,30,29,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,1,0,1  ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,5      ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,2      ),c( 33    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7        ),c( 37    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4        ),c( 34    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,8,7,5  ),c( 42,38 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2,12, 3, 5     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4, 5, 2     ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 3      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 1,26 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 2,10 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 2, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 6, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(16, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 4: 6, 4, 1) ( 4:23,NA, 4,NA, 4) ( 5:26, 2, 5) ( 6: 6, 4, 1) ( 6: 6, 5,NA, 3) (16: 9,NA,16, 4, 2)
			#	unique	(26, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (3)   3 (6)   4 (3)   5 (2)   6 (4)   8 (3)   9 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(4,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 17 21 26 36 45    | 6  4  5 10  9 |                        |0 2 2 1 1 |2 2 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( 12     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  6     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( 18     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  6     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -10, -6 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  18,  6 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-10:-3,-10,-6) (-5:22,-5, 6, 1) (-5:-11,-10,11,-5,10) (-2:-10,-2,-21,-11,-12) 
			#			( 6:11, 6,23) (18&6:18,NA,18*, 6*, 0, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -7 (2)   -5 (4)   -2 (2)   0 (2)   6 (5)   18 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-1,-1)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextRebNum()

# UNdone
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
					if( all(quoSize[1:3+0]==c(1,1,3)) ) return(FALSE)	# next rebind of 1,1,1
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
			if( aZoid[1]%in%c( 21       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 29,26,13 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33,30    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 38,41    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  5:2   8:2   11:2   14:3   21:2   27:2   30:4   33:2   38:3   41:2
			# <  5>   0      5 ,  6 , 11 , 14 , 21 , 41 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   5*, 16 , 17 , NA , 45 , 35 
			# <  8>  -1      8 , 19 , 27 , 30 , 41 
			#                8 , 15 , 21 , 31 , 33 
			#          -->   8*, 11 , 15 , 32!, 25 
			# < 11>  -1      6 , 11 , 14 , 21 , 41 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   4!, 11*, 14!, 39 , 25 
			# < 14>  -1      6 , 11 , 14 , 21 , 41 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   4!, 11!, 14*, 39 , 25 
			# < 21>  -2     11 , 14 , 21 , 41 
			#                8 , 15 , 21 , 31 
			#          -->   5 , 16!, 21*, NA 
			# < 27>   2      7 , 27 , 29 , 30 
			#               19 , 27 , 30 , 41 
			#          -->  NA , 27*, 31!, NA 
			# < 30>  -1      8 , 19 , 27 , 30 , 41 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   2 ,  3 ,  1 , 30*, NA 
			# < 33>   0      8 , 15 , 21 , 31 , 33 , 38 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   2 ,  7 ,  7 , 29!, 33*, 38!
			# < 38>   0      8 , 15 , 21 , 31 , 33 , 38 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   2 ,  7 ,  7 , 29!, 33!, 38*
			# < 41>   0      5 ,  6 , 11 , 14 , 21 , 41 
			#                3 ,  8 , 19 , 27 , 30 , 41 
			#          -->   1 , 10 , 27 , 40 , 39 , 41*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c( 21       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c( 29,26,13 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c( 33,30    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c( 38,41    )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 790        1      3 [same    ]  3(?), ., ., 3, ., ., 3     col
			# 825        2      4       [desc1   ]  4(?),xx, 5,xx, 6     col
			# 8251       3      1       [same    ]  1(?), ., 1, ., 1     col
			# 755        3      6 [sameEnd ]  6(?),xx, 1,xx, 1,xx, 6     col
			# 841        4     -1             [desc1   ] -1(?), 0, 1     col
			# 7901       4      6 [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			# 8411       5      3             [same    ]  3(?), 3, 3     col
			# 7902       5      0          [sameEnd ]  0(?), 3, 3, 0     col
			# 7903       5      0 [same    ]  0(?), ., ., 0, ., ., 0     col
			# 8412       6      8             [same    ]  8(?), 8, 8     col
			# 7904       6      1          [sameEnd ]  1(?), 8, 8, 1     col
			# 1          1      1             [same    ]  1(?), 1, 1  Slide/
			# 11         1      7          [sameEnd ]  7(?), 1, 1, 7  Slide/
			# 12         1      1       [same    ]  1(?), ., 1, ., 1  Slide/
			# 13         2      1       [same    ]  1(?), ., 1, ., 1  Slide/
			# 14         4      3             [desc1   ]  3(?), 4, 5 Slide\\
			# 15         5     -1             [desc1   ] -1(?), 0, 1 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      841        1      5                 [desc1   ]  5(?), 6, 7     col
			#      825        1      7 [seqReb  ]  7(?), ., 7, ., 1, ., 1,...     col
			#      8251       2      7           [desc1   ]  7(?),xx, 6,xx, 5     col
			#      790        2     10     [desc1   ] 10(?),xx,xx,11,xx,xx,12     col
			#      8411       4      4                 [desc1   ]  4(?), 3, 2     col
			#      7901       4      4     [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			#      772        4      7           [symm    ]  7(?), 3, 2, 3, 7     col
			#      8412       5      5                 [same    ]  5(?), 5, 5     col
			#      7902       5     11              [sameEnd ] 11(?), 5, 5,11     col
			#      1          1     20           [symm    ] 20(?), 3,10, 3,20  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(4)   3(4)   5(4)   6(4)   7(2)   8(2)   11(2)   20(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     13 14 26 28 30 36    | 1 12  2  2  6 |                        |0 2 2 2 0 |2 2 2
			#      7 27 29 30 38 44(1) |20  2  1  8  6 | -6  13   3   2   8   8 |1 0 2 2 1 |1 2 2 1
			#      5  6 11 14 21 41    | 1  5  3  7 20 | -2 -21 -18 -16 -17  -3 |2 2 1 0 1 |2 2 1 1
			#      3  8 19 27 30 41(1) | 5 11  8  3 11 | -2   2   8  13   9   0 |2 1 1 1 1 |2 1 1 1 1
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  5   7   2   4   3  -3 |1 1 1 3 0 |1 1 1 3
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 | -3  -4  -7  -1   0   0 |1 2 0 3 0 |1 2 3

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                      descript tgt.dir
			#      1        6     -3  [same    ] -3(?), .,-3, .,-3     col
			#      2        6     -3  [symm    ] -3(?), 0,-3, 0,-3     col
			#      E4       5     -1 [seqReb  ] -1(?),-1, 2, 2,... Slide\\
			#      E5       6     -4    [desc( 4) ] -4(?), 0, 4, 8 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -3(3)   -2(2)   0(3)   2(3)   3(2)   8(3)   13(2) 
			cnt.w2 <- 0
			if( aFStep[2]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextCStepBin()

# UNdone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 28 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 12 19 21 29 32    | 5  7  2  8  3 |                        |1 2 2 1 0 |1 2 2 1
			#     10 17 18 19 23 27(1) | 7  1  1  4  4 |  3   5  -1  -2  -6  -5 |0 4 2 0 0 |4 2
			#      5 16 21 23 24 30(1) |11  5  2  1  6 | -5  -1   3   4   1   3 |1 1 3 1 0 |1 1 3 1
			#      4  9 23 33 39 44(1) | 5 14 10  6  5 | -1  -7   2  10  15  14 |2 0 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7   7  -5 -14 -15  -5 |0 4 1 1 0 |4 1 1
			#      8 15 21 31 33 38    | 7  6 10  2  5 | -3  -1   3  12   9  -1 |1 1 1 3 0 |1 1 1 3


			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  781        2     16       [same    ] 16(?), .,16, .,16     col
			#  385        2     12 [sameEnd ] 12(?),xx,16,xx,16,xx,12     col
			#  825        2     14             [desc1   ] 14(?),15,16     col
			#  660        3     23   [ptnReb   ] 23(?),21,18,23,21,18     col
			#  7811       5     24       [same    ] 24(?), .,24, .,24     col
			#  3851       5     29 [sameEnd ] 29(?),xx,24,xx,24,xx,29     col
			#  8251       6     37             [desc1   ] 37(?),38,39     col

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  16:2   18:2   19:3   21:3   23:3   24:2   33:2   39:2
			# < 16>   0      5 , 16 , 21 , 23 , 24 , 30 
			#               11 , 16 , 18 , 19 , 24 , 39 
			#          -->  NA , 16*, NA , NA , 24!, NA 
			# < 18>   0     10 , 17 , 18 , 19 , 23 , 27 
			#               11 , 16 , 18 , 19 , 24 , 39 
			#          -->  12!, 15!, 18*, 19!, 25!, NA 
			# < 19>   0     10 , 17 , 18 , 19 , 23 , 27 
			#               11 , 16 , 18 , 19 , 24 , 39 
			#          -->  12!, 15!, 18!, 19*, 25!, NA 
			# < 21>   0      5 , 16 , 21 , 23 , 24 , 30 
			#                8 , 15 , 21 , 31 , 33 , 38 
			#          -->  11 , 14!, 21*, 39 , 42 , NA 
			# < 23>  -1     16 , 21 , 23 , 24 , 30 
			#                4 ,  9 , 23 , 33 , 39 
			#          -->  NA , NA , 23*, 42 , NA 
			# < 24>   0      5 , 16 , 21 , 23 , 24 , 30 
			#               11 , 16 , 18 , 19 , 24 , 39 
			#          -->  17 , 16!, 15 , 15 , 24*, NA 
			# < 33>   1      4 ,  9 , 23 , 33 , 39 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  26 , NA , NA , 33*, 37!
			# < 39>   1      4 ,  9 , 23 , 33 , 39 
			#               16 , 18 , 19 , 24 , 39 
			#          -->  28 , 27 , 15 , 15 , 39*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 781        2      6       [same    ]  6(?), ., 6, ., 6     col
			# 385        2      2 [sameEnd ]  2(?),xx, 6,xx, 6,xx, 2     col
			# 825        2      4             [desc1   ]  4(?), 5, 6     col
			# 660        3      3   [ptnReb   ]  3(?), 1, 8, 3, 1, 8     col
			# 7811       5      4       [same    ]  4(?), ., 4, ., 4     col
			# 3851       5      9 [sameEnd ]  9(?),xx, 4,xx, 4,xx, 9     col
			# 6601       5      9 [same    ]  9(?), ., ., 9, ., ., 9     col
			# 8251       5      2             [desc1   ]  2(?), 3, 4     col
			# 3852       5      9 [symm    ]  9(?), 3, 4, 9, 4, 3, 9     col
			# 8252       6      7             [desc1   ]  7(?), 8, 9     col
			# 1          2      1      [seqReb  ]  1(?), 1, 9, 9,...  Slide/
			# 11         3      1      [seqReb  ]  1(?), 1, 4, 4,...  Slide/
			# 12         6      6       [symm    ]  6(?), 3, 9, 3, 6 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      660        1      5     [same    ]  5(?), ., ., 5, ., ., 5     col
			#      825        1      7          [seqReb  ]  7(?), 7, 5, 5,...     col
			#      7811       3      1 [seqReb  ]  1(?), ., 1, ., 2, ., 2,...     col
			#      647        3      2           [symm    ]  2(?),10, 1,10, 2     col
			#      6471       5      6           [symm    ]  6(?), 5,15, 5, 6     col
			#      1          1      6           [symm    ]  6(?), 6, 1, 6, 6  Slide/
			#      11         2     10          [seqReb  ] 10(?),10, 5, 5,...  Slide/
			#      12         3      7                 [desc1   ]  7(?), 6, 5 Slide\\
			#      13         5      3                 [desc1   ]  3(?), 2, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(4)   4(2)   5(7)   6(3)   7(3)   10(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(3,2)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[ 4 ]*c(3,5)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 12 19 21 29 32    | 5  7  2  8  3 |                        |1 2 2 1 0 |1 2 2 1
			#     10 17 18 19 23 27(1) | 7  1  1  4  4 |  3   5  -1  -2  -6  -5 |0 4 2 0 0 |4 2
			#      5 16 21 23 24 30(1) |11  5  2  1  6 | -5  -1   3   4   1   3 |1 1 3 1 0 |1 1 3 1
			#      4  9 23 33 39 44(1) | 5 14 10  6  5 | -1  -7   2  10  15  14 |2 0 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7   7  -5 -14 -15  -5 |0 4 1 1 0 |4 1 1
			#      8 15 21 31 33 38    | 7  6 10  2  5 | -3  -1   3  12   9  -1 |1 1 1 3 0 |1 1 1 3

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#        tgt.col banVal                     descript tgt.dir
			#      E3       5     -5 [same    ] -5(?), .,-5, .,-5 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -5(4)   -1(5)   3(4)   7(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 3 ]*c(-1, 4, 3)==aFStep[c(1,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,2)]*c(-1, 1)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ 5 ]==sum(aFStep[c(4,1)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

}	# fCutCnt.nextFStepBin( )

# UNdone
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 17 30 32 33 34    |13 13  2  1  1 |                        |1 1 0 4 0 |1 1 4
			#      8 14 32 35 37 45(1) | 6 18  3  2  8 |  4  -3   2   3   4  11 |1 1 0 3 1 |1 1 3 1
			#      6  7 19 25 28 38    | 1 12  6  3 10 | -2  -7 -13 -10  -9  -7 |2 1 2 1 0 |2 1 2 1
			#      3 13 20 24 33 37    |10  7  4  9  4 | -3   6   1  -1   5  -1 |1 1 2 2 0 |1 1 2 2
			#     13 14 24 32 39 41(2) | 1 10  8  7  2 | 10   1   4   8   6   4 |0 2 1 2 1 |2 1 2 1
			#      8 17 21 24 27 31(1) | 9  4  3  3  4 | -5   3  -3  -8 -12 -10 |1 1 3 1 0 |1 1 3 1

			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  434        1      2 [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			#  251        4     25       [symm    ] 25(?),24,32,24,25     col
			#  4341       5     33 [same    ] 33(?), ., .,33, ., .,33     col
			#  1          1     17      [seqReb  ] 17(?),17,24,24,...  Slide/
			#  11         5     24             [same    ] 24(?),24,24 Slide\\
			#  12         5     13          [sameEnd ] 13(?),24,24,13 Slide\\

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  8:2   13:2   14:2   17:2   24:3   32:3   33:2   37:2
			# <  8>   0      8 , 14 , 32 , 35 , 37 , 45 
			#                8 , 17 , 21 , 24 , 27 , 31 
			#          -->   8*, 20 , 10 , 13 , 17 , 17 
			# < 13>  -1     13 , 20 , 24 , 33 , 37 
			#               13 , 14 , 24 , 32 , 39 
			#          -->  13*, NA , 24!, 31!, 41 
			# < 14>   0      8 , 14 , 32 , 35 , 37 , 45 
			#               13 , 14 , 24 , 32 , 39 , 41 
			#          -->  NA , 14*, 16 , 29 , 41 , 37 
			# < 17>   0      4 , 17 , 30 , 32 , 33 , 34 
			#                8 , 17 , 21 , 24 , 27 , 31 
			#          -->  12 , 17*, NA , NA , 21 , 28 
			# < 24>   1     13 , 14 , 24 , 32 , 39 
			#               17 , 21 , 24 , 27 , 31 
			#          -->  21 , NA , 24*, NA , NA 
			# < 32>   1      8 , 14 , 32 , 35 , 37 
			#               14 , 24 , 32 , 39 , 41 
			#          -->  20 , NA , 32*, 43 , 45 
			# < 33>   0      4 , 17 , 30 , 32 , 33 , 34 
			#                3 , 13 , 20 , 24 , 33 , 37 
			#          -->   2!,  9 , 10 , 16 , 33*, 40 
			# < 37>   1      8 , 14 , 32 , 35 , 37 
			#               13 , 20 , 24 , 33 , 37 
			#          -->  18 , 26 , 16 , 31 , 37*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 434        1      2     [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			# 630        1      8          [seqReb  ]  8(?), 8, 3, 3,...     col
			# 498        2      4 [seqReb  ]  4(?), ., 4, ., 7, ., 7,...     col
			# 4341       2      3       [ptnReb   ]  3(?), 7, 4, 3, 7, 4     col
			# 4342       3      0     [same    ]  0(?), ., ., 0, ., ., 0     col
			# 251        4      5           [symm    ]  5(?), 4, 2, 4, 5     col
			# 4343       5      3     [same    ]  3(?), ., ., 3, ., ., 3     col
			# 4981       5     10           [desc1   ] 10(?),xx, 9,xx, 8     col
			# 6301       6      1                 [same    ]  1(?), 1, 1     col
			# 4344       6      7              [sameEnd ]  7(?), 1, 1, 7     col
			# 1          1      7          [seqReb  ]  7(?), 7, 4, 4,...  Slide/
			# 11         2      0              [desc1   ]  0(?), 1, 2, 3  Slide/
			# 12         5      4                 [same    ]  4(?), 4, 4 Slide\\
			# 13         5      3              [sameEnd ]  3(?), 4, 4, 3 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1,13   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 10      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(3)   3(4)   4(4)   6(2)   7(2)   8(2)   9(2)   10(3)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(1,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(4,5)]*c(1,1)==aCStep[c(3,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 17 30 32 33 34    |13 13  2  1  1 |                        |1 1 0 4 0 |1 1 4
			#      8 14 32 35 37 45(1) | 6 18  3  2  8 |  4  -3   2   3   4  11 |1 1 0 3 1 |1 1 3 1
			#      6  7 19 25 28 38    | 1 12  6  3 10 | -2  -7 -13 -10  -9  -7 |2 1 2 1 0 |2 1 2 1
			#      3 13 20 24 33 37    |10  7  4  9  4 | -3   6   1  -1   5  -1 |1 1 2 2 0 |1 1 2 2
			#     13 14 24 32 39 41(2) | 1 10  8  7  2 | 10   1   4   8   6   4 |0 2 1 2 1 |2 1 2 1
			#      8 17 21 24 27 31(1) | 9  4  3  3  4 | -5   3  -3  -8 -12 -10 |1 1 3 1 0 |1 1 3 1

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#        tgt.col banVal               descript tgt.dir
			#      E2       1      2 [desc1   ]  2(?), 3, 4  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -7(2)   -3(3)   -1(2)   1(2)   3(2)   4(4)   6(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-1,-4)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,2)]*c( 2,-1)==aFStep[c(6,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1	# -13
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1	# -15

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_1()

# UNdone
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
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 12 19 24 34 41    | 6  7  5 10  7 |                        |1 2 1 1 1 |1 2 1 1 1
			#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
			#      6 11 15 17 23 40    | 5  4  2  6 17 |  1 -11 -16 -15 -16  -5 |1 3 1 0 1 |1 3 1 1
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 11  10  10   9   4  -4 |0 1 4 1 0 |1 4 1
			#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
			#      1 12 13 24 29 44(2) |11  1 11  5 15 |-11  -6  -6  -5  -2   5 |1 2 2 0 1 |1 2 2 1

			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  816        1     12 [seqReb  ] 12(?), .,12, ., 6, ., 6,...     col
			#  821        3      7             [desc( 6) ]  7(?),13,19,25     col
			#  8161       6     38     [desc1   ] 38(?),xx,39,xx,40,xx,41     col
			#  1          1      5             [desc( 7) ]  5(?),12,19,26  Slide/
			#  11         3     12                 [same    ] 12(?),12,12 Slide\\
			#  12         6     29                 [same    ] 29(?),29,29 Slide\\
			#  13         6     25              [sameEnd ] 25(?),29,29,25 Slide\\

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  6:2   12:3   17:2   19:2   24:2   29:2   31:2   39:2
			# <  6>   0      6 , 12 , 19 , 24 , 34 , 41 
			#                6 , 11 , 15 , 17 , 23 , 40 
			#          -->   6*, 10!, 11 , 10 , 12 , 39!
			# < 12>   1     12 , 18 , 19 , 29 , 31 
			#               12 , 13 , 24 , 29 , 44 
			#          -->  12*, NA , 29 , 29!, NA 
			# < 17>  -3     17 , 23 , 40 
			#               17 , 21 , 25 
			#          -->  17*, 19 , NA 
			# < 19>   0      6 , 12 , 19 , 24 , 34 , 41 
			#               12 , 18 , 19 , 29 , 31 , 39 
			#          -->  18 , NA , 19*, 34 , 28 , 37 
			# < 24>   0      6 , 12 , 19 , 24 , 34 , 41 
			#                1 , 12 , 13 , 24 , 29 , 44 
			#          -->  NA , 12!,  7 , 24*, NA , NA 
			# < 29>   1     12 , 18 , 19 , 29 , 31 
			#               12 , 13 , 24 , 29 , 44 
			#          -->  12!,  8 , NA , 29*, NA 
			# < 31>   2      5 , 22 , 31 , 32 
			#               19 , 29 , 31 , 39 
			#          -->  NA , NA , 31*, NA 
			# < 39>   1      5 , 22 , 31 , 32 , 39 
			#               18 , 19 , 29 , 31 , 39 
			#          -->  31 , 16 , 27 , 30!, 39*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 821        1      0                 [desc1   ]  0(?), 1, 2     col
			# 815        1      8     [desc1   ]  8(?),xx,xx, 7,xx,xx, 6     col
			# 816        1      2 [seqReb  ]  2(?), ., 2, ., 6, ., 6,...     col
			# 8151       2      0     [desc1   ]  0(?),xx,xx, 1,xx,xx, 2     col
			# 1          4      9                 [same    ]  9(?), 9, 9  Slide/
			# 11         3      2                 [same    ]  2(?), 2, 2 Slide\\
			# 12         6      9                 [same    ]  9(?), 9, 9 Slide\\
			# 13         6      5              [sameEnd ]  5(?), 9, 9, 5 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#          tgt.col banVal                     descript tgt.dir
			#      816        1      7 [desc1   ]  7(?),xx, 6,xx, 5     col
			#      821        2      1       [same    ]  1(?), 1, 1     col
			#      815        2      4    [sameEnd ]  4(?), 1, 1, 4     col
			#      8211       3     12       [desc1   ] 12(?),11,10     col
			#      1          1     17 [symm    ] 17(?), 1,10, 1,17  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(2)   4(3)   5(3)   6(4)   7(3)   9(2)   10(2)   11(2)   17(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(1,3)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1	# 16
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 12 19 24 34 41    | 6  7  5 10  7 |                        |1 2 1 1 1 |1 2 1 1 1
			#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
			#      6 11 15 17 23 40    | 5  4  2  6 17 |  1 -11 -16 -15 -16  -5 |1 3 1 0 1 |1 3 1 1
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 11  10  10   9   4  -4 |0 1 4 1 0 |1 4 1
			#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
			#      1 12 13 24 29 44(2) |11  1 11  5 15 |-11  -6  -6  -5  -2   5 |1 2 2 0 1 |1 2 2 1

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                      descript tgt.dir
			#      1         3     -6        [same    ] -6(?),-6,-6     col
			#      2         3     10     [sameEnd ] 10(?),-6,-6,10     col
			#      3         5     -2 [seqReb  ] -2(?),-2, 4, 4,...     col
			#      E2        1     -6        [same    ] -6(?),-6,-6  Slide/
			#      E4        1      9     [sameEnd ]  9(?),-6,-6, 9  Slide/
			#      E21       3     -7        [desc1   ] -7(?),-6,-5 Slide\\
			#      E41       5     -4        [desc1   ] -4(?),-5,-6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -16(2)   -11(2)   -6(3)   -5(3)   3(2)   4(3)   5(2)   10(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,4)]*c(1,-1)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,6,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,5)])==sum(aFStep[c(2,3,6)]) )	cnt.w2<-cnt.w2+1	# -7

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_2()

# UNdone
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  8 13 16 30 43    | 2  5  3 14 13 |                        |2 2 0 1 1 |2 2 1 1
			#      6  7 10 16 38 41(2) | 1  3  6 22  3 |  0  -1  -3   0   8  -2 |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#      6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#     tgt.col banVal                     descript tgt.dir
			#  713       1      3 [desc1   ]  3(?),xx, 2,xx, 1     col
			#  1         2     18       [same    ] 18(?),18,18  Slide/
			#  11        2     25    [sameEnd ] 25(?),18,18,25  Slide/

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  6:3   7:2   8:2   16:2   18:2   19:2   35:2   43:2
			# <  6>   0      6 ,  7 , 10 , 16 , 38 , 41 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   6*,  9!, 26 , NA , NA , 45 
			# <  7>   0      6 ,  7 , 10 , 16 , 38 , 41 
			#                1 ,  7 , 19 , 26 , 27 , 35 
			#          -->  NA ,  7*, 28 , 36 , 16 , 29 
			# <  8>   0      6 ,  8 , 13 , 16 , 30 , 43 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   6!,  8*, 23 , NA , NA , 43!
			# < 16>   0      6 ,  8 , 13 , 16 , 30 , 43 
			#                6 ,  7 , 10 , 16 , 38 , 41 
			#          -->   6!,  6!,  7 , 16*, NA , 39 
			# < 18>  -1      5 , 15 , 18 , 19 , 23 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->   7!,  1 , 18*, NA , NA 
			# < 19>   2      1 ,  7 , 19 , 26 
			#               15 , 18 , 19 , 23 
			#          -->  NA , NA , 19*, 20 
			# < 35>  -2     19 , 26 , 27 , 35 
			#                6 ,  8 , 18 , 35 
			#          -->  NA , NA ,  9 , 35*
			# < 43>   0      6 ,  8 , 13 , 16 , 30 , 43 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   6!,  8!, 23 , NA , NA , 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 713        1      3           [desc1   ]  3(?),xx, 2,xx, 1     col
			# 7131       4      8 [seqReb  ]  8(?), ., 8, ., 6, ., 6,...     col
			# 834        6      3              [same    ]  3(?), 3, 3, 3     col
			# 624        6      5           [sameEnd ]  5(?), 3, 3, 3, 5     col
			# 710        6      3     [same    ]  3(?), ., ., 3, ., ., 3     col
			# 8341       6      3          [seqReb  ]  3(?), 3, 3, 3,...     col
			# 1          2      8                 [same    ]  8(?), 8, 8  Slide/
			# 11         2      5              [sameEnd ]  5(?), 8, 8, 5  Slide/
			# 12         4      1                 [desc1   ]  1(?), 2, 3  Slide/
			# 13         5      5                 [same    ]  5(?), 5, 5 Slide\\
			# 14         5      4              [sameEnd ]  4(?), 5, 5, 4 Slide\\
			# 15         6      9           [desc1   ]  9(?),xx, 8,xx, 7 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      834        1      1             [desc1   ]  1(?), 2, 3     col
			#      8341       2     10             [same    ] 10(?),10,10     col
			#      7101       2      5          [sameEnd ]  5(?),10,10, 5     col
			#      7102       2      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			#      713        4      1       [same    ]  1(?), ., 1, ., 1     col
			#      519        4     14 [sameEnd ] 14(?),xx, 1,xx, 1,xx,14     col
			#      8342       4      7      [seqReb  ]  7(?), 7, 1, 1,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(2)   3(5)   5(2)   6(2)   7(2)   8(2)   10(2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  8 13 16 30 43    | 2  5  3 14 13 |                        |2 2 0 1 1 |2 2 1 1
			#      6  7 10 16 38 41(2) | 1  3  6 22  3 |  0  -1  -3   0   8  -2 |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#      6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        2      2 [desc1   ]  2(?),xx, 1,xx, 0     col
			#      E4       2     -6 [same    ] -6(?), .,-6, .,-6  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -6(3)   -3(2)   -2(4)   -1(2)   0(3)   3(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(5,1)==aFStep[c(6,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,3)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..
} # fCutCnt.nextColVal_3()

# UNdone
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
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  6 13 16 29 30    | 4  7  3 13  1 |                        |2 2 1 1 0 |2 2 1 1
			#      6 16 37 38 41 45(2) |10 21  1  3  4 |  4  10  24  22  12  15 |1 1 0 2 2 |1 1 2 2
			#      4  8  9 16 17 19(1) | 4  1  7  1  2 | -2  -8 -28 -22 -24 -26 |3 3 0 0 0 |3 3
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -2   9  10   8  20  22 |1 2 1 1 1 |1 2 1 1 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 |  6  -8  -1  -3  -9  -1 |2 1 2 0 1 |2 1 2 1
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -5   0  -7  -9 -15 -21 |2 4 0 0 0 |2 4


			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  753        1      2     [same    ]  2(?), ., ., 2, ., ., 2     col
			#  839        2      9                 [same    ]  9(?), 9, 9     col
			#  7531       2     17              [sameEnd ] 17(?), 9, 9,17     col
			#  776        2     10           [desc1   ] 10(?),xx, 9,xx, 8     col
			#  7761       4     21 [seqReb  ] 21(?), .,21, .,16, .,16,...     col
			#  1          1     19           [desc1   ] 19(?),xx,18,xx,17  Slide/
			#  11         3     10                 [desc1   ] 10(?), 9, 8 Slide\\

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  2:2   6:2   8:2   9:3   13:2   16:3   17:2   19:3   37:2   41:2
			# <  2>   0      2 ,  6 , 13 , 16 , 29 , 30 
			#                2 , 17 , 19 , 24 , 37 , 41 
			#          -->   2*, 28 , 25 , 32 , 45 , NA 
			# <  6>  -1      6 , 13 , 16 , 29 , 30 
			#                6 , 16 , 37 , 38 , 41 
			#          -->   6*, 19 , NA , NA , NA 
			# <  8>  -1      8 ,  9 , 16 , 17 , 19 
			#                8 ,  9 , 18 , 21 , 28 
			#          -->   8*,  9!, 20 , 25 , 37 
			# <  9>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                3 ,  9 , 11 , 12 , 13 , 19 
			#          -->  NA ,  9*, NA , NA , NA , NA 
			# < 13>   2      2 ,  6 , 13 , 16 
			#               11 , 12 , 13 , 19 
			#          -->  NA , NA , 13*, 22 
			# < 16>   2      6 , 16 , 37 , 38 
			#                9 , 16 , 17 , 19 
			#          -->  12 , 16*, NA , NA 
			# < 17>  -3     16 , 17 , 19 
			#                2 , 17 , 19 
			#          -->  NA , 17*, 19!
			# < 19>   3      2 , 17 , 19 
			#               12 , 13 , 19 
			#          -->  NA ,  9 , 19*
			# < 37>   2      6 , 16 , 37 , 38 
			#               19 , 24 , 37 , 41 
			#          -->  32 , 32 , 37*, 44 
			# < 41>   1      6 , 16 , 37 , 38 , 41 
			#               17 , 19 , 24 , 37 , 41 
			#          -->  28 , 22 , 11 , 36!, 41*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 753        1      2     [same    ]  2(?), ., ., 2, ., ., 2     col
			# 839        2      9                 [same    ]  9(?), 9, 9     col
			# 7531       2      7              [sameEnd ]  7(?), 9, 9, 7     col
			# 776        2     10           [desc1   ] 10(?),xx, 9,xx, 8     col
			# 7532       2      8     [desc1   ]  8(?),xx,xx, 7,xx,xx, 6     col
			# 7761       3      7           [desc1   ]  7(?),xx, 8,xx, 9     col
			# 8391       4      3                 [desc1   ]  3(?), 2, 1     col
			# 7762       4      1 [seqReb  ]  1(?), ., 1, ., 6, ., 6,...     col
			# 7763       5      9           [desc1   ]  9(?),xx, 8,xx, 7     col
			# 7533       6      2     [desc1   ]  2(?),xx,xx, 1,xx,xx, 0     col
			# 1          1     10                 [desc1   ] 10(?), 9, 8  Slide/
			# 11         1      9           [desc1   ]  9(?),xx, 8,xx, 7  Slide/
			# 12         2      1                 [same    ]  1(?), 1, 1  Slide/
			# 13         2      7              [sameEnd ]  7(?), 1, 1, 7  Slide/
			# 14         3     10                 [desc1   ] 10(?), 9, 8 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#         tgt.col banVal                               descript tgt.dir
			#      776       1      1 [seqReb  ]  1(?), ., 1, ., 4, ., 4,...     col
			#      740       2      1           [symm    ]  1(?), 2, 9, 2, 1     col
			#      753       4     13     [same    ] 13(?), ., .,13, ., .,13     col
			#      1         1      1                 [desc1   ]  1(?), 2, 3  Slide/
			#      11        1      4           [desc1   ]  4(?),xx, 3,xx, 2  Slide/
			#      12        3      3                 [desc1   ]  3(?), 2, 1 Slide\\
			#      13        5      2           [desc1   ]  2(?),xx, 3,xx, 4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(7)   2(3)   3(3)   4(4)   6(2)   7(3)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  6 13 16 29 30    | 4  7  3 13  1 |                        |2 2 1 1 0 |2 2 1 1
			#      6 16 37 38 41 45(2) |10 21  1  3  4 |  4  10  24  22  12  15 |1 1 0 2 2 |1 1 2 2
			#      4  8  9 16 17 19(1) | 4  1  7  1  2 | -2  -8 -28 -22 -24 -26 |3 3 0 0 0 |3 3
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -2   9  10   8  20  22 |1 2 1 1 1 |1 2 1 1 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 |  6  -8  -1  -3  -9  -1 |2 1 2 0 1 |2 1 2 1
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -5   0  -7  -9 -15 -21 |2 4 0 0 0 |2 4

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                     descript tgt.dir
			#      1         2     -8 [same    ] -8(?), .,-8, .,-8     col
			#      E2        1      1       [desc1   ]  1(?), 0,-1  Slide/
			#      E4        3     -9       [same    ] -9(?),-9,-9  Slide/
			#      E6        3     22    [sameEnd ] 22(?),-9,-9,22  Slide/
			#      E3        4     -6       [desc1   ] -6(?),-7,-8 Slide\\
			#      E31       5      0 [desc1   ]  0(?),xx,-1,xx,-2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -9(2)   -8(2)   -2(2)   -1(2)   10(2)   22(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c(3,3)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_4()

# UNdone
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      6 21 35 36 37 41    |15 14  1  1  4 | -5   9   6   3  -1  -1 |1 0 1 3 1 |1 1 3 1
			#     15 17 19 21 27 45(1) | 2  2  2  6 18 |  9  -4 -16 -15 -10   4 |0 3 2 0 1 |3 2 1
			#      1 21 26 36 40 41(1) |20  5 10  4  1 |-14   4   7  15  13  -4 |1 0 2 1 2 |1 2 1 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 14   0   5  -4   1   2 |0 1 1 2 2 |1 1 2 2
			#      8 15 21 31 33 38(3) | 7  6 10  2  5 | -7  -6 -10  -1  -8  -5 |1 1 1 3 0 |1 1 1 3

			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  808        1     15       [same    ] 15(?), .,15, .,15     col
			#  775        1     11 [sameEnd ] 11(?),xx,15,xx,15,xx,11     col
			#  825        2     15      [seqReb  ] 15(?),15,21,21,...     col
			#  8251       4     30             [desc1   ] 30(?),31,32     col
			#  796        6     40 [desc1   ] 40(?),xx,xx,41,xx,xx,42     col
			#  1          3     31      [seqReb  ] 31(?),31,41,41,...  Slide/
			#  11         3     15             [same    ] 15(?),15,15 Slide\\
			#  12         4     21             [same    ] 21(?),21,21 Slide\\
			#  13         4      1          [sameEnd ]  1(?),21,21, 1 Slide\\
			#  14         5     31             [same    ] 31(?),31,31 Slide\\
			#  15         5     21          [sameEnd ] 21(?),31,31,21 Slide\\
			#  16         6     34             [desc1   ] 34(?),33,32 Slide\\

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  15:3   21:5   31:2   33:2   36:2   38:2   41:3
			# < 15>   1     15 , 21 , 31 , 32 , 41 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  15*, 21!, 31!, 34!, 35 
			# < 21>   1     15 , 21 , 31 , 32 , 41 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  15!, 21*, 31!, 34!, 35 
			# < 31>   1     15 , 21 , 31 , 32 , 41 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  15!, 21!, 31*, 34!, 35 
			# < 33>   1     11 , 12 , 29 , 33 , 38 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  19 , 30 , NA , 33*, 38!
			# < 36>   0      6 , 21 , 35 , 36 , 37 , 41 
			#                1 , 21 , 26 , 36 , 40 , 41 
			#          -->  NA , 21!, 17 , 36*, 43 , 41!
			# < 38>   1     11 , 12 , 29 , 33 , 38 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  19 , 30 , 33 , 33!, 38*
			# < 41>  -1     21 , 26 , 36 , 40 , 41 
			#               15 , 21 , 31 , 32 , 41 
			#          -->   9 , 16 , 26 , 24 , 41*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 808        1      5           [same    ]  5(?), ., 5, ., 5     col
			# 775        1      1     [sameEnd ]  1(?),xx, 5,xx, 5,xx, 1     col
			# 796        1      1     [same    ]  1(?), ., ., 1, ., ., 1     col
			# 7961       2      0     [desc1   ]  0(?),xx,xx, 1,xx,xx, 2     col
			# 825        2      5          [seqReb  ]  5(?), 5, 1, 1,...     col
			# 8251       3      1                 [same    ]  1(?), 1, 1     col
			# 7962       3      6              [sameEnd ]  6(?), 1, 1, 6     col
			# 8081       3      1 [seqReb  ]  1(?), ., 1, ., 9, ., 9,...     col
			# 8252       4      0                 [desc1   ]  0(?), 1, 2     col
			# 8082       4      3           [desc1   ]  3(?),xx, 2,xx, 1     col
			# 7963       6      0     [desc1   ]  0(?),xx,xx, 1,xx,xx, 2     col
			# 1          2      0                 [desc1   ]  0(?), 1, 2  Slide/
			# 11         3      1              [same    ]  1(?), 1, 1, 1  Slide/
			# 12         3      1          [seqReb  ]  1(?), 1, 1, 1,...  Slide/
			# 13         4      3                 [same    ]  3(?), 3, 3  Slide/
			# 14         3      5                 [same    ]  5(?), 5, 5 Slide\\
			# 15         4      1              [same    ]  1(?), 1, 1, 1 Slide\\
			# 16         4      1          [seqReb  ]  1(?), 1, 1, 1,... Slide\\
			# 17         5      1              [same    ]  1(?), 1, 1, 1 Slide\\
			# 18         5      5           [sameEnd ]  5(?), 1, 1, 1, 5 Slide\\
			# 19         5      1          [seqReb  ]  1(?), 1, 1, 1,... Slide\\
			# 110        6      4                 [desc1   ]  4(?), 3, 2 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                           descript tgt.dir
			#      825       1      8             [desc1   ]  8(?), 7, 6     col
			#      780       3      2       [symm    ]  2(?),10, 1,10, 2     col
			#      796       4      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			#      1         2     11             [desc1   ] 11(?),10, 9  Slide/
			#      11        3      2             [same    ]  2(?), 2, 2  Slide/
			#      12        3      6             [same    ]  6(?), 6, 6 Slide\\
			#      13        4     10             [same    ] 10(?),10,10 Slide\\
			#      14        4     20          [sameEnd ] 20(?),10,10,20 Slide\\
			#      15        5      3             [desc1   ]  3(?), 2, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(5)   4(4)   5(3)   6(3)   10(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(3,2)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[ 4 ]*c(3,5)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      6 21 35 36 37 41    |15 14  1  1  4 | -5   9   6   3  -1  -1 |1 0 1 3 1 |1 1 3 1
			#     15 17 19 21 27 45(1) | 2  2  2  6 18 |  9  -4 -16 -15 -10   4 |0 3 2 0 1 |3 2 1
			#      1 21 26 36 40 41(1) |20  5 10  4  1 |-14   4   7  15  13  -4 |1 0 2 1 2 |1 2 1 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 14   0   5  -4   1   2 |0 1 1 2 2 |1 1 2 2
			#      8 15 21 31 33 38(3) | 7  6 10  2  5 | -7  -6 -10  -1  -8  -5 |1 1 1 3 0 |1 1 1 3
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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      E4       6     -4 [same    ] -4(?), .,-4, .,-4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -5(2)   -4(3)   -1(3)   4(2)   9(2) 
			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(6,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,6)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# -11
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# -15
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# -13
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1	# -16
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(1,2,6)]) )	cnt.w2<-cnt.w2+1	# -18

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.
	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_5()

# UNdone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8  9 18 21 28 40    | 1  9  3  7 12 |                        |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 13  13  12  -2  -8  -1 |0 1 1 3 1 |1 1 3 1

			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  834        1      7           [desc1   ]  7(?),xx, 6,xx, 5     col
			#  8341       2      7           [desc1   ]  7(?),xx, 8,xx, 9     col
			#  8342       2      8 [seqReb  ]  8(?), ., 8, ., 9, ., 9,...     col
			#  821        5     30     [desc1   ] 30(?),xx,xx,29,xx,xx,28     col
			#  8343       6     43           [same    ] 43(?), .,43, .,43     col
			#  776        6     40     [sameEnd ] 40(?),xx,43,xx,43,xx,40     col
			#  843        6     41              [desc1   ] 41(?),42,43,44     col
			#  1          6     33                 [desc1   ] 33(?),34,35 Slide\\

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
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  8:2   9:2   18:2   19:2   21:2   29:2   30:2   42:2   43:2
			# <  8>   1      8 ,  9 , 18 , 21 , 28 
			#                8 , 18 , 35 , 42 , 43 
			#          -->   8*, 27 , NA , NA , NA 
			# <  9>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                5 ,  9 , 14 , 26 , 30 , 43 
			#          -->   2 ,  9*, 10 , 31 , 32 , NA 
			# < 18>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   4 ,  7!, 18*, NA , NA , NA 
			# < 19>  -2     19 , 25 , 29 , 36 
			#               19 , 21 , 30 , 33 
			#          -->  19*, NA , 31!, 30 
			# < 21>  -2     18 , 21 , 28 , 40 
			#               19 , 21 , 30 , 33 
			#          -->  20!, 21*, 32 , 26 
			# < 29>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#                1 , 12 , 13 , 24 , 29 , 44 
			#          -->  NA , 17 ,  7 , 23!, 29*, NA 
			# < 30>  -2     14 , 26 , 30 , 43 
			#               19 , 21 , 30 , 33 
			#          -->  24 , 16 , 30*, NA 
			# < 42>   1      6 ,  8 , 18 , 35 , 42 
			#               21 , 30 , 33 , 34 , 42 
			#          -->  36 , NA , NA , 33!, 42*
			# < 43>   0      5 ,  9 , 14 , 26 , 30 , 43 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   7!,  7!, 22 , NA , NA , 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 834        1      7           [desc1   ]  7(?),xx, 6,xx, 5     col
			# 8341       2      7           [desc1   ]  7(?),xx, 8,xx, 9     col
			# 8342       2      8 [seqReb  ]  8(?), ., 8, ., 9, ., 9,...     col
			# 8343       4      4           [desc1   ]  4(?),xx, 5,xx, 6     col
			# 821        5     10     [desc1   ] 10(?),xx,xx, 9,xx,xx, 8     col
			# 8344       6      3           [same    ]  3(?), ., 3, ., 3     col
			# 776        6      0     [sameEnd ]  0(?),xx, 3,xx, 3,xx, 0     col
			# 843        6      1              [desc1   ]  1(?), 2, 3, 4     col
			# 1          3      4                 [desc1   ]  4(?), 3, 2  Slide/
			# 11         4      5                 [desc1   ]  5(?), 4, 3  Slide/
			# 12         6      3                 [desc1   ]  3(?), 4, 5 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#          tgt.col banVal                     descript tgt.dir
			#      843        1      2       [same    ]  2(?), 2, 2     col
			#      821        1     11    [sameEnd ] 11(?), 2, 2,11     col
			#      8431       2      8       [desc1   ]  8(?), 9,10     col
			#      1          3      1       [same    ]  1(?), 1, 1  Slide/
			#      11         5      4 [symm    ]  4(?), 1,17, 1, 4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(2)   3(2)   4(3)   5(3)   7(3)   9(2)   11(2)   12(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(4,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8  9 18 21 28 40    | 1  9  3  7 12 |                        |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 13  13  12  -2  -8  -1 |0 1 1 3 1 |1 1 3 1

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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#         tgt.col banVal                      descript tgt.dir
			#      1         4      1  [symm    ]  1(?),-2,11,-2, 1     col
			#      2         6     -1        [same    ] -1(?),-1,-1     col
			#      3         6      1     [sameEnd ]  1(?),-1,-1, 1     col
			#      E3        2     13        [desc1   ] 13(?),12,11  Slide/
			#      E31       4     12 [seqReb  ] 12(?),12,-4,-4,... Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -4(3)   -2(3)   -1(4)   1(5)   3(2)   5(2)   13(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,4)]*c(1,-6)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# 11

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

# UNdone		fCut.basic() 사용
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

# UNdone		fCut.basic() 사용
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

		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 9,10, 4),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(-6, 1)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,11, 8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,11 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(2,1)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(1,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,17,11,NA, 4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12,13,NA,NA,10, 4 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7,-5,-1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -13,-10, -8, -1,-15,-14 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -2, 2,NA,NA,-9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -3,-10,-6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  18,NA,18, 6, 0, 2 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 2,NA,16,-9 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3, 3,-15 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 1, 1,-15, 8 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(2,6)]*c(1,-3)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(1,1)==aFStep[c(6,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  0,-24,-10,-5,-19 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,  6,-13, 1,  0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c( 5, 3,-2)==aFStep[c( 1, 3, 5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -5, 5,-4, 0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   1,17,10,15,-1 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( fCutU.hasPtn(c( 17, 2, 7,24 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 4,29 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(3,2)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(5,6)]*c(2,2)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(5,6)]*c(2,3)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14, 5,12, 6,-6,-2 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  12, 1,-2,-2 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -1,17,17, 6,-1,-5 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -1, 3, 2,20,32 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -9,10,13, 3 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -17,-6, 4, 4,11 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(-4, 1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -9, 5,-3,21,NA,27 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1,-8, 1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,15, 1,21 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10, 1, 4, 7 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -8,-5, 5,17,-8,-23 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( fCutU.hasPtn(c( -4, 7,23,NA,-1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4, 7,23,NA,-1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,-2,-3,-2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-4, 1,-3, 1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-4, 1,-3, 1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

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
			if( fCutU.hasPtn(c(  2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11,NA,31,23 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 14,22,31,34 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			# <30>
			if( fCutU.hasPtn(c(  8,NA,NA,30,33,31 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 13,14,NA,33,40 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 14,35,31,36,40 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextZW
			# < 6>
			# < 9>
			if( fCutU.hasPtn(c(  6, 9,32,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 10,11,NA,NA,18 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <17>
			if( fCutU.hasPtn(c(  4,13,14,17,27 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>			# <23>			# <28>
			# <34>
			if( fCutU.hasPtn(c( 12,30,27,34,38 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 12,30,27,34,38 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextQuo10
			# < 2>
			if( fCutU.hasPtn(c(  2,15,13,26,45 ),aZoid,thld=3,fixIdx= 1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6, 8,NA,10,27,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c( 12,17,24,18,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>			# <15>			# <16>
			# <18>
			if( fCutU.hasPtn(c(  9,18,29,29 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c(  6,19,25 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 12,17,24,NA,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 24,16,NA,38 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <41>
			if( fCutU.hasPtn(c( 18,22,11,30,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c( 22,14,NA,NA,35,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextBin
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,31,25,24 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,24,33,35 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  4,11,19,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <15>
			if( fCutU.hasPtn(c( 10,15,31,44,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 10, 9,24,33,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c(  7,NA,25,31 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  5,NA, 9,33 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 11,28,36,NA,38 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <40>
			if( fCutU.hasPtn(c(  4, 8,13,14,40 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextRebNum
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,17,21 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 14,22,31,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>
			if( fCutU.hasPtn(c(  7,26,28,27 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 11,31,NA,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c( 21,NA,35,NA,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>			# <45>

		#	fCutCnt.nextCStepBin
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,NA,28, 8,22 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			# < 7>
			# < 9>
			if( fCutU.hasPtn(c(  9,24,33,35 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			# <20>
			if( fCutU.hasPtn(c(  7,12,20 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 10, 9,24,33,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <27>
			if( fCutU.hasPtn(c( 11,16,27 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  8, 5,NA, 9,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <44>
			if( fCutU.hasPtn(c(  9, 9, 8,14,24,44 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextFStepBin
			# < 4>
			if( fCutU.hasPtn(c(  4,20,20,30 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <20>
			if( fCutU.hasPtn(c(  5,NA,20,31 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  8,19,16,NA,NA,38 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <40>

		#	fCutCnt.nextColVal_1
			# < 2>
			if( fCutU.hasPtn(c(  2,36,39,44,NA,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  9,10,13,26,37,31 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <18>
			if( fCutU.hasPtn(c(  9,18,29,32,33 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <22>
			# <25>
			if( fCutU.hasPtn(c( 14,25,32,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 10,23,33,40,43 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  9, 9,36,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			if( fCutU.hasPtn(c(  9, 9,36,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_2
			# < 4>
			if( fCutU.hasPtn(c(  4,25,NA,NA,45,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,21,40,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>			# <13>
			# <17>
			if( fCutU.hasPtn(c(  3, 3,17,33,30 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c(  1,14,29 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 14,17,28,32 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c( 18, 9,NA, 2,17,34 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_3
			# < 7>
			if( fCutU.hasPtn(c(  7, 8,24,25,44 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c(  6,14,28,22 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>			# <27>			# <31>			# <35>			# <37>
			# <40>
			if( fCutU.hasPtn(c(  5,14, 9,17,40,44 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c(  9,12,42 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>
			if( fCutU.hasPtn(c(  3,34,40,41,30,43 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_4
			# < 4>
			if( fCutU.hasPtn(c(  4,13,42,38,41 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 5>
			if( fCutU.hasPtn(c(  5,38,NA,39,41 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6,12,13 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <12>
			if( fCutU.hasPtn(c(  3,12,24,25,40 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c(  9,20,NA,24,43 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c(  2, 3,12,NA,25,29 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <41>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_5
			# < 9>
			if( fCutU.hasPtn(c(  9,15,22,28,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c( 10,11,26,43,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c(  6, 5,20,21,43 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			# <34>
			if( fCutU.hasPtn(c(  9, 3,28,27,34 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  6, 8,16,10,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 21, 7,26,28,42 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_6
			# < 8>
			# < 9>
			if( fCutU.hasPtn(c(  2, 9,10,31,32 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c( 12,NA,15,20 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <18>
			if( fCutU.hasPtn(c(  4, 7,18 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c( 17, 7,23,29 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c(  3,32,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>
			if( fCutU.hasPtn(c(  7, 7,22,NA,NA,43 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

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
		dbgLst[[1+length(dbgLst)]] <- paste( quoSize,collapse="," )

		# fCutCnt.basic
			#      2 25 28 30 33 45    |23  3  2  3 12 |                        |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
		if( 3==quoSize[4] && all(quoSize[3]==quoSize[5]) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(quoSize==c(2,1,2,0,1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#      4 20 26 28 35 40    |16  6  2  7  5 |                        |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  6  33  18  16  11  -1 |1 0 0 3 2 |1 3 2
			#      2  3 12 20 27 38(1) | 1  9  8  7 11 | -5 -34 -26 -19 -13  -6 |2 1 2 1 0 |2 1 2 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1
			#      5 11 14 30 33 38    | 6  3 16  3  5 | -5  -4  -4   9  -1  -3 |1 2 0 3 0 |1 2 3
		if( all(quoSize[1:4]==c(0,0,3,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 0 3 1 1 --*--> 0 0 3 2
		if( all(quoSize[2:5]==c(1,0,0,3)) ){	surviveFlg[idx]<-FALSE	;next }	# 2 0 3 0 --*--> 1 0 0 3

		# fCutCnt.nextColVal_1
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2 22 27 33 36 37(2) |20  5  6  3  1 |  0  14  12  11  11  -4 |1 0 2 3 0 |1 2 3
			#     11 18 21 36 37 43(2) | 7  3 15  1  6 |  9  -4  -6   3   1   6 |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -8   2 -12  -6  -4 |1 1 2 2 0 |1 1 2 2
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  3   0  -5   1   3  -4 |1 2 1 2 0 |1 2 1 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  15  15  13   6  10 |0 1 1 2 2 |1 1 2 2
		if( all(quoSize[2:5]==c(1,2,1,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 1 1 2 2 -> 1 2 1 2
		if( all(quoSize[2:5]==c(0,2,1,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 2 1 2 0 -> 0 2 1 2

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	table( sapply( dbgLst,function(str){str}) )
	allIdxF <- allIdxF[surviveFlg]


	# 0,1,1,1,3 0,1,2,0,3 0,1,2,1,2 0,1,2,3,0 0,1,3,0,2 0,1,3,1,1 0,1,3,2,0 0,2,0,1,3 0,2,0,2,2 0,2,1,0,3 0,2,1,1,2 0,2,1,2,1 0,2,1,3,0 0,2,2,0,2 0,2,2,1,1 0,2,2,2,0 0,2,3,0,1 0,2,3,1,0 0,3,0,0,3 0,3,0,1,2 0,3,0,2,1 0,3,0,3,0 0,3,1,0,2 
	#        55        58       612       803       300       641      1202        54       125       117       562       646       709       374       864      1819       194       525        23       249       306       342       270 
	# 0,3,1,1,1 0,3,1,2,0 0,3,2,0,1 0,3,2,1,0 1,0,1,1,3 1,0,1,2,2 1,0,2,0,3 1,0,2,1,2 1,0,2,3,0 1,0,3,0,2 1,0,3,1,1 1,0,3,2,0 1,1,0,1,3 1,1,0,3,1 1,1,1,0,3 1,1,1,1,2 1,1,1,2,1 1,1,1,3,0 1,1,2,0,2 1,1,2,1,1 1,1,2,2,0 1,1,3,0,1 1,1,3,1,0 
	#       778      1782       332      1394       272       642       318      2275      1135      1087      1804      1507       283       198       538      3346      3169      1705      2411      3595      2955       242       364 
	# 1,2,0,0,3 1,2,0,1,2 1,2,0,2,1 1,2,1,0,2 1,2,1,1,1 1,2,1,2,0 1,2,2,0,1 1,2,2,1,0 1,2,3,0,0 1,3,0,0,2 1,3,0,1,1 1,3,0,2,0 1,3,1,0,1 1,3,1,1,0 1,3,2,0,0 2,0,0,1,3 2,0,0,2,2 2,0,0,3,1 2,0,1,0,3 2,0,1,1,2 2,0,1,2,1 2,0,1,3,0 2,0,2,0,2 
	#       315      2265      2132      2555      4438      4044      2268      4295      2098       610      1004       922      1156      2124      1455       294       726       397       529      3141      3282      1186      2317 
	# 2,0,2,1,1 2,0,2,2,0 2,0,3,0,1 2,0,3,1,0 2,1,0,0,3 2,1,0,1,2 2,1,0,2,1 2,1,0,3,0 2,1,1,0,2 2,1,1,1,1 2,1,1,2,0 2,1,2,1,0 2,1,3,0,0 2,2,0,0,2 2,2,0,1,1 2,2,0,2,0 2,2,1,0,1 2,2,1,1,0 2,3,0,0,1 2,3,0,1,0 2,3,1,0,0 3,0,0,0,3 3,0,0,1,2 
	#      3998      2657       687       980       497      3351      3285      1397      3654      7175      5488      5750      2978      1723      3251      2595      3255      4887       561       753       739       145      1428 
	# 3,0,0,2,1 3,0,0,3,0 3,0,1,0,2 3,0,1,1,1 3,0,1,2,0 3,0,2,0,1 3,0,2,1,0 3,0,3,0,0 3,1,0,0,2 3,1,0,1,1 3,1,0,2,0 3,1,1,0,1 3,1,1,1,0 3,1,2,0,0 3,2,0,0,1 3,2,0,1,0 3,2,1,0,0 
	#      1458       334      1491      3039      1605      1595      1984      1260      1701      3307      1695      3702      4195      4874      1600      1480      1905 

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( NA )","nextColVal_1( 29, 36 )","nextColVal_5( 34 )","nextFStepBin( 36 )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c(29,36,34) )]

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






