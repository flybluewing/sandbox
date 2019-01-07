# toZ841_H.R 최종접근
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

	# col[1] 은 1~9
	flag <- gEnv$allZoidMtx[allIdxF,1]<10	;kIdx<-anaFlagFnd(!flag,rpt)
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

	#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
	#     12 18 30 39 41 42    | 6 12  9  2  1 |                        |0 2 0 2 2 |2 2 2
	#      6  8 18 35 42 43(2) | 2 10 17  7  1 | -6 -10 -12  -4   1   1 |2 1 0 1 2 |2 1 1 2
	#      9 10 13 28 38 45    | 1  3 15 10  7 |  3   2  -5  -7  -4   2 |1 2 1 1 1 |1 2 1 1 1
	#      1  9 11 14 26 28(2) | 8  2  3 12  2 | -8  -1  -2 -14 -12 -17 |2 2 2 0 0 |2 2 2
	#      2 25 28 30 33 45(1) |23  3  2  3 12 |  1  16  17  16   7  17 |1 0 2 2 1 |1 2 2 1
	#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
	surviveFlag <- rep( T ,length(allIdxF) )	;names(surviveFlag) <- allIdxF
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid
		aRem <- aZoid%%10

		if( aZoid[6]%in%c(42) ){ surviveFlag[idx]<-FALSE	;next  }
		
		if( aRem[6]%in%c(5) ){ surviveFlag[idx]<-FALSE	;next  }

		# aCStep
		if( aCStep[2]==aCStep[4] ){ 				surviveFlag[idx]<-FALSE	;next  }
		if( all(aCStep[c(2,4)]==c(2,12)) ){ 		surviveFlag[idx]<-FALSE	;next  }
		if( fCutU.hasPtn(c(  2, 3,12 ),aCStep) ){ 	surviveFlag[idx]<-FALSE	;next  }
		if( 1<sum(aCStep[1:3+2]==c(  3,16, 3 )) ){ 	surviveFlag[idx]<-FALSE	;next  }
		if( fCutU.hasPtn(c( 12, 1, 7 ),aCStep) ){ 	surviveFlag[idx]<-FALSE	;next  }	#	unique in fCutCnt.nextZW
		fltRst <- sapply( getSideValues(aCStep,20) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		if( any(fltRst) ){ 							surviveFlag[idx]<-FALSE	;next  }	#   unique in fCutCnt.nextRebNum

		# aFStep
		if( 1<sum( aFStep[c(1,2,4)]*c(-1,1,1)==aFStep[c(6,3,5)] ) ){ 	surviveFlag[idx]<-FALSE	;next  }
		# aFStep
		fltRst <- sapply( getSideValues(aFStep,-8) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		if( any(fltRst) ) surviveFlag[idx] <- FALSE						#   unique in fCutCnt.nextColVal_6

	} # for( idx ) - surviveFlag
	table(surviveFlag)	;tIdx <- head(which(!surviveFlag))	# for debug data
	allIdxF <- allIdxF[surviveFlag]

	# quotion
	surviveFlag <- rep( T ,length(allIdxF) )	;names(surviveFlag) <- allIdxF
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		quoSize <- fCutU.getQuoObj( aZoid )$size

		if( all(quoSize==c(2,1,2,0,1)) ){ 		surviveFlag[idx]<-FALSE	;next  }	#	unique in fCutCnt.nextBin
		if( all(quoSize==c(3,2,1,0,0)) ){ 		surviveFlag[idx]<-FALSE	;next  }	#	unique in fCutCnt.nextBin
		if( all(quoSize==c(1,2,0,3,0)) ){ 		surviveFlag[idx]<-FALSE	;next  }	#	unique in fCutCnt.nextCStepBin

		if( all(quoSize[2:5]==c(0,0,1,4)) ){		surviveFlag[idx]<-FALSE	;next  }	#	unique in fCutCnt.nextColVal_2

	} # for( idx ) - surviveFlag
	table(surviveFlag)	;tIdx <- head(which(!surviveFlag))	# for debug data
	allIdxF <- allIdxF[surviveFlag]


	# qqe working

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
					if( all(quoSize[1:3+1]==c(2,4,0)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+2]==c(0,2,1)) ) return(FALSE)	# next rebind of 2,0,1
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
			if( aZoid[1]%in%c(  9, 1, 4 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 10       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 11,17, 5 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 11       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 19       ) ) cnt<-cnt+1
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
			if( fCutU.hasPtn(c(  1,NA,11       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       11,44,45 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <14>
			if( fCutU.hasPtn(c(  7,14,NA,38 ),aZoid) ) cnt<-cnt+1
			# <28>			# <33>
			# <38>
			if( fCutU.hasPtn(c( 19,24,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 9,1,4    ),c(  9, 1, 4 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,0      ),c( 10       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,7,5    ),c( 11,17, 5 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,1      ),c( 11       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,8,0    ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9        ),c( 19       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(            ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 1, 2   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 17, 8      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 3      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	
			if( 1<sum(aCStep[1:2+2]==c(  7,17 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  7,17 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,12 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 17, 1 )) )	cnt.w1<-cnt.w1+1
			# unique : 2, 3, 12 중복발생 상태.
			# unique : 다음 중 2개 이상 발생.
			#	6,2(3,2 3,2 6,2 ...)	
			#	5,3(2,3 2,3 5,3 ...)	16,3(2,3 2,3 16,3 ...)		3,16(3,12 3,12 3,16 ...)	3,2(3,12 3,12 3,2 ...)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (6)   3 (6)   6 (2)   7 (2)   12 (2) 

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      9 10 13 28 38 45    | 1  3 15 10  7 |                        |1 2 1 1 1 |1 2 1 1 1
			#      1  9 11 14 26 28(2) | 8  2  3 12  2 | -8  -1  -2 -14 -12 -17 |2 2 2 0 0 |2 2 2
			#      2 25 28 30 33 45(1) |23  3  2  3 12 |  1  16  17  16   7  17 |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  -5,-11 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  -4     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 16,15 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 15,16 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -6 (2)   -5 (2)   -1 (2)   3 (2)   7 (2)   16 (4)   17 (2)
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					# [1]*   2  2  8 14  5 11
					# [2]   11  8 12
					# [3]    8 20 13
					# [4]   14
					# [5]   35 30 29
					# [6]   
					if( 1<sum(aZoid==c(  2,11, 8,14,35,NA ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  2, 8,20,NA,30,NA ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  8,12,13,NA,29,NA ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 6, 7 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(  2, 8    ) ) score<-score+1
					if( aZoid[2]%in%c(          ) ) score<-score+1
					if( aZoid[3]%in%c(          ) ) score<-score+1
					if( aZoid[4]%in%c(  8       ) ) score<-score+1
					if( aZoid[5]%in%c(          ) ) score<-score+1
					if( aZoid[6]%in%c(          ) ) score<-score+1
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
					if( aZoid[1]%in%c( 16       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 14, 3, 5 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(  9       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 24       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 34       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1

					# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

					# [  1] 11 24     4  8    20 25    38 40    35 38
					# [  2] 15 17     5 10    18 26    14 16    36 40
					# [  3] 18 21     6 13             32 41         
					# [  4] 14 15     5 11             31 34         
					# [  5]  2  7    12 18             18 20         
					# [  6]  7 17                      11 39         
					# [  7] 11 15                      29 35         
					# [  8] 10 14                                    
					# [  9]  5 10                                    
					# [ 10]  3 12                                    

					
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 6,9     ),c( 16       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 4,3,5   ),c( 14, 3, 5 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c(         ),c(  9       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 4,3     ),c( 24       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 4       ),c( 34       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c(         ),c(          )) )	remCnt <- remCnt+1

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

					score  <- sum(aCStep==c( 13, 4, 5, 2, 3 ),na.rm=T)
					matCnt <- sum(aCStep==c(  2, 5, 8, 2, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 7,NA, 9,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1, 6,NA, 3,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5, 6,NA, 2,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1] 13   2   3   1   5  10   4   4   5   9   1   1  16 
					#	[2]  4   5   7   6   6 
					#	[3]  5   8 
					#	[4]  2   2   9   3   2  28   6 
					#	[5]  3   4 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 2  ),aCStep) ) cnt<-cnt+1	# -
						#	unique : (2:2,2,2)

						if( aCStep[1]%in%c(  3             ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  4             ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(                ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  2, 9, 1, 5, 3 ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  2             ) ) cnt<-cnt+1

						if( aCStep[3]==sum(aCStep[c(4,5)]) )	cnt<-cnt+1
						if( sum(aCStep[c(2,5)])==sum(aCStep[c(3,4)]) )	cnt<-cnt+1	# 7
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
					# [  1]  3 12 18  
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

					# [1]*   1  7  5  1 20 10 | 1  5  5  2  4  7  4  6  3  1  3 11  1  3  6 20  2 13  6  4  5  9 19 10  1  5
					# [2]*  10  3  4  2  6  2 | 4  5  5  7 14  7  5  4  1  6 13 10  9  7 12  3  7  1  2  4 10  9  6
					# [3]   16  5 12  4  3  2 | 1
					# [4]*   7  7  2  2  1 11 |10  6  3 12 10  2 14  4  2 10  8 15  1  1  7  3  7  9 11  9 16 15  2 10  2  6  1  2 ...
					# [5]*   1  1  1  1  8  1 |

					tCnt <- 0
						if( aCStep[1]%in%c(     ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  4  ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(  6  ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  7  ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(     ) ) tCnt<-tCnt+1

						if( aCStep[ 3 ]==sum(aCStep[c(2,4,5)]) )	cnt<-cnt+1
						if( aCStep[ 3 ]==sum(aCStep[c(2,4,1)]) )	cnt<-cnt+1
						if( sum(aCStep[c(2,4)])==sum(aCStep[c(1,3)]) )	cnt<-cnt+1	# 17
						if( sum(aCStep[c(2,4)])==sum(aCStep[c(5,3)]) )	cnt<-cnt+1	# 17
						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  8, 3 ),aCStep) )	cnt<-cnt+1	# unique (3:8,3,9)

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
					# [  1]  5  4     6  1    13  2    20  2
					# [  2] 10  3     4 12     6  4     1  1
					# [  3]  9 10              2 15    19  2
					# [  4]  5  9              3  6     6  5
					# [  5]  1  2              2  5     2  9
					# [  6]  3  1                       3  2
					# [  7]  7 14                           
					# [  8]  7 10                           
					# [  9]  1  6                           
					# [ 10]  4  9                           

					if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  5       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  3, 5, 2 ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aCStep[1:2+0]==c(  1, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  4,10 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  6, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  5, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  6, 9 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c( 21, 2 )) ) cnt<-cnt+1
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
					if( aCStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(    ) ) cnt<-cnt+1

					# [  1]  6  4 12                        

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,2	# unique 2 1 2 0 1
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,0,1
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
			if( aZoid[1]%in%c(  1, 5, 3 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 17       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  7       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 20       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,NA,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA, 7,NA,24,37 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,10,13,40,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,15          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,14,NA,31 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  6,15          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,NA,32 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,15,18,27    ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(  8,16,NA,41,36 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c(  1,NA,NA,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    17,20          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       20,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <22>
			if( fCutU.hasPtn(c(  7,NA,22    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    19,22,33 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 12,15,33,37 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(       20,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    17,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,NA,41,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,5,3   ),c(  1, 5, 3 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,6     ),c( 17       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  7       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,0     ),c( 20       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,3     ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1       ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 11      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 11      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 13      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 9 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : a 11 a
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   5 (5)   7 (2)   8 (2)   9 (2)   10 (3)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(3,5)]*c(5,1)==aCStep[c(2,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 15 22 23 34 35    |10  7  1 11  1 |                        |1 1 2 2 0 |1 1 2 2
			#      7 15 20 25 33 43(1) | 8  5  5  8 10 |  2   0  -2   2  -1   8 |1 1 2 1 1 |1 1 2 1 1
			#      1  6 17 22 28 45    | 5 11  5  6 17 | -6  -9  -3  -3  -5   2 |2 1 2 0 1 |2 1 2 1
			#      4 16 20 33 40 43    |12  4 13  7  3 |  3  10   3  11  12  -2 |1 1 1 1 2 |1 1 1 1 2
			#      1  3 12 21 26 41    | 2  9  9  5 15 | -3 -13  -8 -12 -14  -2 |2 1 2 0 1 |2 1 2 1
			#      5  6 16 18 37 38    | 1 10  2 19  1 |  4   3   4  -3  11  -3 |2 2 0 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  -4, -3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -3 (5)   -2 (3)   2 (3)   3 (3)   4 (2)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-1,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(6,5)]) )	cnt.w2<-cnt.w2+1	# 8

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 31, 35 ) ) return( FALSE )	# unique 35 (29   n   31   n   33   n ... )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 0,2,1
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+2]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,2,1
					#	unique : -2h(0 2 1 2 1), -4h(1 2 1 2 0)... -0h에서 (0 2 1 2 1)이 나올까?
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
			if( aZoid[2]%in%c(  7    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 21    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 38    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,19          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,17,26    ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,NA,29    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,14,14,NA,42 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			# <15>
			if( fCutU.hasPtn(c(  8,15,26 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  4, 5,18,NA,32,36 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c(       21,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,13,21       ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 24,29 ),aZoid) ) cnt<-cnt+1
			# <30>
			# <34>
			if( fCutU.hasPtn(c(  5,NA,NA,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             34,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     2,10,11,34    ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  4,21,17,25,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,5     ),c(  7 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0       ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5       ),c( 21 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,8     ),c( 38 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  7,16   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 15, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# unique 4,7,21
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   3 (5)   4 (3)   5 (3)   6 (3)   7 (2)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 22 24 31 34 36    |15  2  7  3  2 |                        |1 0 2 3 0 |1 2 3
			#      8  9 18 21 28 40    | 1  9  3  7 12 |  1 -13  -6 -10  -6   4 |2 1 2 0 1 |2 1 2 1
			#      6 12 17 21 34 37(1) | 6  5  4 13  3 | -2   3  -1   0   6  -3 |1 2 1 2 0 |1 2 1 2
			#     12 15 16 20 24 30(1) | 3  1  4  4  6 |  6   3  -1  -1 -10  -7 |0 3 2 1 0 |3 2 1
			#     10 15 21 35 38 43(1) | 5  6 14  3  5 | -2   0   5  15  14  13 |0 2 1 2 1 |2 1 2 1
			#      6  7 18 19 30 38(1) | 1 11  1 11  8 | -4  -8  -3 -16  -8  -5 |2 2 0 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -2,  4 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3,-1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( -8, -3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -10, -8 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  -1, -2 ),aFStep) )	cnt.w1<-cnt.w1+1	# unique  3,-1,-2
			if( fCutU.hasPtn(c(  -3,  0 ),aFStep) )	cnt.w1<-cnt.w1+1	# unique -3, 0, 4
			if( fCutU.hasPtn(c(   0,  4 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -8 (2)   -6 (2)   -3 (2)   -2 (2)   -1 (3)   0 (2)   3 (2)   6 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(2,4,2)==aFStep[c(2,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 2 ]*c(2,1)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 22 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,1)) ) return(FALSE)	# next rebind of 2,1,1	unique 2 1 1 1
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,1,1
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
			if( aZoid[1]%in%c(  5,20 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 44    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41,36 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>			# <13>
			# <14>
			if( fCutU.hasPtn(c(  9,14,16 ),aZoid) ) cnt<-cnt+1
			# <21>
			# <27>
			if( fCutU.hasPtn(c( 11,17,27,NA,38 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(       26,30 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,12,NA,30 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(  8,22,23,25,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(  1,10,27,40,39,41 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c(  9,19,32,44 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,0,4   ),c(  5,20 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,3     ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,5     ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4       ),c( 44    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,6     ),c( 41,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4, 9    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3,12    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  1,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# unique 1,2,11  21,2,3
			#	unique (3: 8, 3) (5: 5,11)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (3)   3 (4)   5 (3)   6 (2)   7 (2)   8 (2)   10 (3)   11 (2) 
			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     20 30 33 35 36 44    |10  3  2  1  8 |                        |0 0 1 4 1 |1 4 1
			#      5 10 13 27 37 41    | 5  3 14 10  4 |-15 -20 -20  -8   1  -3 |1 2 1 1 1 |1 2 1 1 1
			#      5 21 27 34 44 45(2) |16  6  7 10  1 |  0  11  14   7   7   4 |1 0 2 1 2 |1 2 1 2
			#     13 14 26 28 30 36    | 1 12  2  2  6 |  8  -7  -1  -6 -14  -9 |0 2 2 2 0 |2 2 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -8  -8 -15 -14  -9   5 |2 2 1 0 1 |2 2 1 1
			#      3  8 19 27 30 41(1) | 5 11  8  3 11 | -2   2   8  13   9   0 |2 1 1 1 1 |2 1 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  8     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( 13     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  6     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -14, -9 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+4]==c(  9, 0 )) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -20 (2)   -15 (2)   -14 (2)   -9 (2)   -8 (3)   0 (2)   7 (2)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-1,-4)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 34 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c(  4,11 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,16          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 11,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    21,NA,35 ),aZoid) ) cnt<-cnt+1
			# <31>
			# <36>
			if( fCutU.hasPtn(c( 23,24,31,36 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(  3,22,24,39 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <43>			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,1,3   ),c(  4,11 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,8     ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,6     ),c(  6    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3       ),c( 43    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  7      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  8      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (3)   3 (2)   4 (2)   5 (3)   6 (4)   8 (4)   9 (2)   10 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(13,2,4)==aCStep[c(2,3,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 14 23 36 38 39    | 6  9 13  2  1 |                        |1 1 1 3 0 |1 1 1 3
			#     11 18 21 36 37 43(1) | 7  3 15  1  6 |  3   4  -2   0  -1   4 |0 2 1 2 1 |2 1 2 1
			#      7 15 20 25 33 43(1) | 8  5  5  8 10 | -4  -3  -1 -11  -4   0 |1 1 2 1 1 |1 1 2 1 1
			#     11 17 21 26 36 45    | 6  4  5 10  9 |  4   2   1   1   3   2 |0 2 2 1 1 |2 2 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  18    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -6    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0,-3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (2)   -2 (2)   -1 (2)   0 (3)   1 (2)   2 (2)   3 (3)   4 (3)   6 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-9,-3,-6,-3)==aFStep[c(3,4,5,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 6 ]*c(3,1,2)==aFStep[c(3,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 4 ]*c(2,1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
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
			if( aZoid[1]%in%c(  5    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 44    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 32    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 38,27 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			# < 8>
			if( fCutU.hasPtn(c(  8,NA,NA,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,11,15,NA,25 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(  9,14,16 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 16,21 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 27,31 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(  9,NA,NA,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,25,30,44 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 23,15,33,36,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(  1,10,27,40,39,41 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 37,33,42,44 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,0     ),c(  5    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9,4     ),c( 44    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,7,2   ),c( 32    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,7     ),c( 38,27 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  7      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 9   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 16      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 6, 6 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 5,10 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(10, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique 5,10,3
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (4)   3 (2)   5 (3)   6 (4)   7 (3)   8 (2)   10 (2)   11 (2)   20 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,5)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(4,5)]*c(3,2)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 21 27 34 44 45    |16  6  7 10  1 |                        |1 0 2 1 2 |1 2 1 2
			#     13 14 26 28 30 36    | 1 12  2  2  6 |  8  -7  -1  -6 -14  -9 |0 2 2 2 0 |2 2 2
			#      7 27 29 30 38 44(1) |20  2  1  8  6 | -6  13   3   2   8   8 |1 0 2 2 1 |1 2 2 1
			#      5  6 11 14 21 41    | 1  5  3  7 20 | -2 -21 -18 -16 -17  -3 |2 2 1 0 1 |2 2 1 1
			#      3  8 19 27 30 41(1) | 5 11  8  3 11 | -2   2   8  13   9   0 |2 1 1 1 1 |2 1 1 1 1
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  5   7   2   4   3  -3 |1 1 1 3 0 |1 1 1 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  5, 8, 6 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  2,-2    ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 10       ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  8,13    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 2, 8 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( 13, 9 )) )	cnt.w1<-cnt.w1+1
			#	unique : 2,4
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -3 (2)   -2 (2)   2 (3)   3 (2)   8 (4)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(3,5)]*c( 2,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 7
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,3,0)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+1]==c(3,0,1)) ) return(FALSE)	# next rebind of 3,1,1
					if( all(quoSize[1:3+1]==c(0,3,0)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[1]%in%c(  3    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  2    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 22    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
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
			# <27>
			if( fCutU.hasPtn(c(  2,NA,NA,27    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,NA,27    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       18,27,42 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 16,17,14,15,34,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <38>			# <40>			# <44>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3        ),c(  3  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,8      ),c(  2  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0        ),c(     )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2        ),c( 22  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,3      ),c(     )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4        ),c( 44  )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 8,19    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 9, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (9:9,7,9)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   3 (4)   5 (3)   6 (2)   7 (4)   9 (3)   16 (3) 

			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 13 22 27 34 44    | 9  9  5  7 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#      4 20 26 28 35 40(1) |16  6  2  7  5 |  0   7   4   1   1  -4 |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  6  33  18  16  11  -1 |1 0 0 3 2 |1 3 2
			#      2  3 12 20 27 38(1) | 1  9  8  7 11 | -5 -34 -26 -19 -13  -6 |2 1 2 1 0 |2 1 2 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1

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
			if( fCutU.hasPtn(c( 2,7 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 7,2 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique : (7:2,7,2)
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (3)   -5 (2)   1 (3)   6 (2)   7 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(4,2)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# 15
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 13
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	#  9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(4,1,0)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c( 24    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 36    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 41    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <17>
			# <19>
			if( fCutU.hasPtn(c(  1,NA,19          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     6,19          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       19,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       19,23,NA,31 ),aZoid) ) cnt<-cnt+1
			# <21>			# <25>			# <27>
			# <36>
			if( fCutU.hasPtn(c(  3, 8,NA,36 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(          37,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 18,19,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,3,8    ),c( 24 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,8      ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9        ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,6      ),c( 36 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,6,1    ),c( 41 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6,3,7    ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5, 6, 3, 4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(             ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 8, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 11, 8 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 5, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 11, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 6,11 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 7,11 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (3:11,8,3) (11:7,11,8)
			#	unique : (5:5,3) (6:12,6)...
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   3 (5)   4 (3)   5 (6)   6 (3)   8 (2)   11 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  8 19 27 30 41    | 5 11  8  3 11 |                        |2 1 1 1 1 |2 1 1 1 1
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 | -1  -1   0  -2  -1  -5 |2 1 2 1 0 |2 1 2 1
			#     10 15 21 35 38 43    | 5  6 14  3  5 |  8   8   2  10   9   7 |0 2 1 2 1 |2 1 2 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 |  5   4   2  -8 -15  -9 |0 1 4 1 0 |1 4 1
			#      9 14 17 33 36 38(2) | 5  3 16  3  2 | -8  -7  -8   7   9   2 |1 2 0 3 0 |1 2 3
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -8,  4  ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  2, -8  ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  7      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  7      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 4, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( 9, 2 )) )	cnt.w1<-cnt.w1+1
			#	unique : (4:11,4,2)
			# -------------------------------------------------------------------------------------
			#     FV :    -8 (3)   -1 (4)   2 (7)   4 (2)   7 (2)   8 (2)   9 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 6 ]*c(-4,-4)==aFStep[c(1,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( all(quoSize[1:3+1]==c(1,3,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(1,1,3)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+0]==c(1,3,1)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+1]==c(1,3,1)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(3,1,1)) ) return(FALSE)	# next rebind of 1,1,2
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
			if( aZoid[2]%in%c( 17      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 37      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 37,43      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,43      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c( 11,33,40,31,33,35 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c( 17,NA,28 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  7,24,29,29,25 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 17,NA,28 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  8, 8,33,45 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <35>
			# <37>
			if( fCutU.hasPtn(c(    26,NA,37    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 19,NA,21,37,41 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(  7,31,40 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 18,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    30,23,34,43 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 32,38,37,NA,NA,44 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 21,24,31,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,0,4   ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4,7     ),c( 17    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,6     ),c( 37    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,7,3   ),c( 37,43 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,3,2   ),c( 45,43 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2,11   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 9, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 2, 9 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : (9:2,9,1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (2)   3 (2)   4 (2)   5 (5)   6 (2)   7 (2)   8 (3)   9 (2)   11 (2)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  6 17 22 28 45    | 5 11  5  6 17 |                        |2 1 2 0 1 |2 1 2 1
			#      2 12 19 24 39 44    |10  7  5 15  5 |  1   6   2   2  11  -1 |1 2 1 1 1 |1 2 1 1 1
			#     11 15 24 35 37 45(1) | 4  9 11  2  8 |  9   3   5  11  -2   1 |0 2 1 2 1 |2 1 2 1
			#     11 24 32 33 35 40(3) |13  8  1  2  5 |  0   9   8  -2  -2  -5 |0 1 1 3 1 |1 1 3 1
			#      4 16 20 33 40 43(2) |12  4 13  7  3 | -7  -8 -12   0   5   3 |1 1 1 1 2 |1 1 1 1 2
			#     17 25 28 37 43 44(1) | 8  3  9  6  1 | 13   9   8   4   3   1 |0 1 2 1 2 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  3,-1  ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  5, 3  ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 9, 8 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( -8,-12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 11, -3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (8:9,8,10) (11:8,11,-3)
			#	unique : (9:9,3)
			# -------------------------------------------------------------------------------------
			#     FV :    -2 (3)   0 (2)   1 (3)   2 (2)   3 (3)   5 (2)   8 (2)   9 (3)   11 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(4,5)]*c(2,3)==aFStep[c(3,2)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,6,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 41 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,0)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+1]==c(0,0,1)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+0]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[1]%in%c(  2, 8, 3 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 30       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28, 6    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,28    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,15,NA,25,NA,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  1,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,12    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,19 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(    12,19 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,19 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 15,NA,NA,21,36,29 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <28>
			# <43>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       10,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,8,3    ),c(  2, 8, 3 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3,2      ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7        ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,0      ),c( 30       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,8,6    ),c( 28, 6    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1      ),c( 43,28    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 9   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3,13 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (4)   3 (3)   4 (4)   5 (2)   7 (3)   9 (2) 

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  3 12 21 26 41    | 2  9  9  5 15 |                        |2 1 2 0 1 |2 1 2 1
			#      1  9 12 23 39 43(2) | 8  3 11 16  4 |  0   6   0   2  13   2 |2 1 1 1 1 |2 1 1 1 1
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  7   3   7  -2  -8  -8 |1 2 1 2 0 |1 2 1 2
			#      2  6  7 12 19 45(2) | 4  1  5  7 26 | -6  -6 -12  -9 -12  10 |3 2 0 0 1 |3 2 1
			#      2 25 28 30 33 45(2) |23  3  2  3 12 |  0  19  21  18  14   0 |1 0 2 2 1 |1 2 2 1
			#      2  4 11 28 29 43(2) | 2  7 17  1 14 |  0 -21 -17  -2  -4  -2 |2 1 2 0 1 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  0, -6 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -8 (2)   -6 (2)   -2 (3)   0 (5)   2 (2)   7 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 4 ]*c(2,1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,4,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,0,0
					if( all(quoSize[1:3+2]==c(0,4,2)) ) return(FALSE)	# next rebind of 1,2,2
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
			if( aZoid[1]%in%c(  9     ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12,11  ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41     ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,NA,26       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,18,25,NA,41    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,16,23,23 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,14       ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 12,14       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,36,38 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(    15,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,15,16,NA,23 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,32,39 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c(    15,NA,25    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    25,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,25,NA,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          25,28 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(          25,28 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4, 6,10,NA,28 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(    25,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       33,38    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,21,33,NA,38 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(       33,38    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,21,NA,38 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  7,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       26,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 18,25,NA,41,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,9     ),c(  9    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,6,8,1 ),c( 12,11 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8,4,2,7 ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5       ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1       ),c( 41    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7, 2   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  6      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  9, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  5, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  1,17 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 9 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (9:1,9,1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (5)   3 (2)   4 (3)   5 (6)   8 (4)   9 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(4,6,1)==aCStep[c(1,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(2,3)]*c(4,4)==aCStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 12 15 24 25 43    | 5  3  9  1 18 |                        |1 2 2 0 1 |1 2 2 1
			#      7 15 20 25 33 43(4) | 8  5  5  8 10 |  0   3   5   1   8   0 |1 1 2 1 1 |1 1 2 1 1
			#     10 14 16 18 27 28    | 4  2  2  9  1 |  3  -1  -4  -7  -6 -15 |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  5     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 5 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique : (5:2,5,3)
			# -------------------------------------------------------------------------------------
			#     FV :    -15 (2)   0 (2)   1 (2)   3 (3)   5 (3)   13 (2) 

			cnt.w2 <- 0
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# -31
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# -38
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# -39

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,2,2
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
			if( aZoid[2]%in%c( 20    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 37    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			# <25>
			if( fCutU.hasPtn(c( 13,25,38,44 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(       27,28 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15, 9,NA,28 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       33,37    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 23,27,33,NA,36 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(  5,22,14,20,24,36 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 17,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    29,31,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(     )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,8,0        ),c( 20  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(     )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8,3        ),c(     )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,9,7,3,5        ),c( 37  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,4,2,6        ),c( 42  )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9, 5, 4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2, 6, 4 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  5, 4, 9 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  9, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 5, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  1, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7,14 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : (2:13, 2, 3) (8:9,8,4) 
			#	unique : (9,2)
			# -------------------------------------------------------------------------------------
			#     FV :    2 (4)   3 (3)   4 (2)   5 (6)   6 (4)   8 (2)   9 (4)   14 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,3)==aCStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(5,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  9 23 33 39 44    | 5 14 10  6  5 |                        |2 0 1 2 1 |2 1 2 1
			#      2 11 19 25 28 32    | 9  8  6  3  4 | -2   2  -4  -8 -11 -12 |1 2 2 1 0 |1 2 2 1
			#     13 14 26 28 30 36(1) | 1 12  2  2  6 | 11   3   7   3   2   4 |0 2 2 2 0 |2 2 2
			#     10 15 21 35 38 43    | 5  6 14  3  5 | -3   1  -5   7   8   7 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  6  10  12   3   2   2 |0 1 1 2 2 |1 1 2 2
			#      9 18 20 24 27 36    | 9  2  4  3  9 | -7  -7 -13 -14 -13  -9 |1 1 3 1 0 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(   2      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   3      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(   2, 13  ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+3]==c(  3, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 13, 3 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique : (2:3,2,0) (3:13,3,-3)
			#	unique : (2:-2,2)
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (2)   -7 (2)   2 (4)   3 (3)   7 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(1,2)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,3)]*c(1,1)==aFStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					if( all(quoSize[1:3+0]==c(0,1,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(1,0,3)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(0,3,2)) ) return(FALSE)	# next rebind of 1,1,1
					# unique : 1 2 1 1 1 -> 0 1 0 3 2
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
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 35    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 37    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c( 11,NA,NA,34 ),aZoid) ) cnt<-cnt+1
			# <15>
			# <28>
			if( fCutU.hasPtn(c(  4, 5,NA,28 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 30,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 30,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 10,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     9,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(        7,35,40,45 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,0,8 ),c( 13 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3     ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5     ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,9,4 ),c( 35 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0     ),c( 40 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0,7   ),c( 37 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 19       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4,21    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7,11,19 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 1 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 15,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : (1:15,3,1) (15:4,15,11)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (4)   3 (3)   4 (2)   6 (2)   7 (2)   9 (2)   10 (2)   15 (2) 
			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,3)])==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 11 15 17 23 40    | 5  4  2  6 17 |                        |1 3 1 0 1 |1 3 1 1
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  2   0   4   4  13   5 |1 2 1 1 1 |1 2 1 1 1
			#     11 30 34 35 42 44(1) |19  4  1  7  2 |  3  19  15  14   6  -1 |0 1 0 3 2 |1 3 2
			#     14 15 25 28 29 30(1) | 1 10  3  1  1 |  3 -15  -9  -7 -13 -14 |0 2 3 1 0 |2 3 1
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 | -2   3   5  11  12  12 |0 2 0 2 2 |2 2 2
			#      9 10 13 28 38 45    | 1  3 15 10  7 | -3  -8 -17 -11  -3   3 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -4, 4 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -17    ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -3 (2)   3 (4)   4 (2)   5 (2)   12 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c( 1,-1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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












