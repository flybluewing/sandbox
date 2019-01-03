# toZ840_H.R 최종접근
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
			if( aZoid[2]%in%c( 11      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  9     ) ) cnt<-cnt+1
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
			if( fCutU.hasPtn(c(  9,11 ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(  9,11 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <14>
			if( fCutU.hasPtn(c(  7,14,NA,38 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 24,28 ),aZoid) ) cnt<-cnt+1
			# <33>
			# <38>
			if( fCutU.hasPtn(c( 19,24,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			# <45>
			cntMtx[idx,"rawFV"] <- cnt

		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(  8       ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(  1       ),c( 11 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  8,9     ),c(  9 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  1,2     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  6,2,4,6 ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(  8,3,0   ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 7        ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 1, 4, 2, 3     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 1        ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 2, 1     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# unique 2, 3,12
			if( 1<sum(aCStep[1:2+2]==c(  2, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  2, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,12 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  1, 1 )) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (6)   3 (6)   6 (2)   7 (2)   10 (2)   12 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  8 18 35 42 43    | 2 10 17  7  1 |                        |2 1 0 1 2 |2 1 1 2
			#      9 10 13 28 38 45    | 1  3 15 10  7 |  3   2  -5  -7  -4   2 |1 2 1 1 1 |1 2 1 1 1
			#      1  9 11 14 26 28(2) | 8  2  3 12  2 | -8  -1  -2 -14 -12 -17 |2 2 2 0 0 |2 2 2
			#      2 25 28 30 33 45(1) |23  3  2  3 12 |  1  16  17  16   7  17 |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -10    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -5, -5 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3,  4 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -7 (2)   -6 (2)   -5 (2)   2 (2)   3 (3)   7 (2)   16 (2)   17 (2) 
			cnt.w2 <- 0
			if( sum(aFStep[c( 5, 6 )])==sum(aFStep[c( 1,2,3,5 )]) )	cnt.w2<-cnt.w2+1	# -40

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
					# [1]    6 14  4  9  8  7
					# [2]    7 21 27 10
					# [3]   19 37 10  3
					# [4]   13
					# [5]   
					# [6]   
					if( 1<sum(aZoid==c(  6, 7,19,13,NA,NA ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 14,21,37,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  4,27,10,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  9,10, 3,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 6, 7 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(          ) ) score<-score+1
					if( aZoid[2]%in%c(          ) ) score<-score+1
					if( aZoid[3]%in%c(          ) ) score<-score+1
					if( aZoid[4]%in%c(          ) ) score<-score+1
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
					if( aZoid[1]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 15     ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 12,15  ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(        ) ) cnt<-cnt+1

					# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

					# [  1] 14 15    25 28     7 13    18 20    29 34
					# [  2] 33 36     8 19     9 14    25 28     3 21
					# [  3] 13 14    14 20     8  9    20 23     2 14
					# [  4]          13 32     3 13    31 35     8 14
					# [  5]          24 34    25 26    13 26    38 39
					# [  6]          13 18    25 30     5 27         
					# [  7]          14 29     1  4    25 29         
					# [  8]           3 13     7  8    10 15         
					# [  9]                    8 23     6  9         
					# [ 10]                    7  8    11 37         
					
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 4      ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 4,8,5  ),c( 15    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 7      ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 2,5    ),c( 12,15 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c(        ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 4      ),c(       )) )	remCnt <- remCnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
							if( aZoid[3]==7  && fCutU.remFilt(aZoid[4],c(3),c(12,15)) ) remCnt <- remCnt+1
							if( aZoid[4]==8  && fCutU.remFilt(aZoid[3],c(7),c( )) ) remCnt <- remCnt+1
						# grp (1:2+3)
							if( aZoid[4]==25 && fCutU.remFilt(aZoid[5],c(7),c( )) ) remCnt <- remCnt+1
						# grp (1:2+4)
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){

					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c(  1, 3, 6, 2, 5 ),na.rm=T)
					matCnt <- sum(aCStep==c(  3,11, 5, 3,18 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1, 6, 1, 3,12 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,19,10, 4, 6 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,10, 1,13, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 5, 5,22 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,15, 3, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,10, 1, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA,15, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 1,26 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 3, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 1, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 8, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  1   3   1 
					#	[2]  3  11   6  19  10   5  15  10 
					#	[3]  6   5   1  10   1   5   3   1  15   1   3   1   8  14 
					#	[4]  2   3   3   4  13  22   4   5   3  26   1   5   8 
					#	[5]  5  18  12   6   1 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  1, 3 ),aCStep) ) cnt<-cnt+1
						if( 1<sum(aCStep[1:2+0]==c(  5, 3 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  6, 3  ),aCStep) ) cnt<-cnt+1	# -

						if( aCStep[1]%in%c(       ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  7    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(  7, 1 ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  2    ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(       ) ) cnt<-cnt+1

						if( aCStep[2]==sum(aCStep[c(1,4)]) )	cnt<-cnt+1
						if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt<-cnt+1
						if( aCStep[5]==sum(aCStep[c(2,4)]) )	cnt<-cnt+1
						if( sum(aCStep[c(1,3)])==sum(aCStep[c(4,5)]) )	cnt<-cnt+1	# 7
						if( sum(aCStep[c(2,5)])==sum(aCStep[c(3,4)]) )	cnt<-cnt+1	# 8
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
					# [  1]                         21 35 40    30 38 39
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

					# [1]*   9  1 23  9  1  1 | 3  3  7  9  3  2  1  5  6  4  2  2  5  1  2
					# [2]*  16  3 19  4 10  5 | 9  3  3  7  8  7  1 15  7 12  3  4  5  2  2 12  6  9  3  6  3  9  2  1 12  1  8  5 10  1 10...
					# [3]*  28  2  4 12  6  7 | 8  2  3 19  1  1  4
					# [4]*   1  1  3  5  8  1 | 1 21  7  4  2 11  8  2  7  7 11  7  7  5 13 12  6  3 16  9 13 14 12 11 22 13  4  1 10 11 10...
					# [5]*  15 18  1  3  7  5 |19  6  3  3  1  2  9  7  6  3 16 11  7 10  7 11  9  4 15 16  3  2 22 12  5 12


					tCnt <- 0
						if( aCStep[1]%in%c(  3,23  ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  2,28  ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  1, 3,21 ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(        ) ) tCnt<-tCnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt<-cnt+1

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
					# [  1] 15  5     7  1     2  5     5 12
					# [  2]  3  9    13  3    10  2    21  3
					# [  3]  9  9     4  5     4  5     2  7
					# [  4]  3  1    11  1     7 13     1  5
					# [  5]  4  1    13  8     5 10     7  1
					# [  6]  1 16     2  2     9 21    15  6
					# [  7]  4  7     9  4     1  2     7  3
					# [  8]  6  3     2  1     2 20     8  9
					# [  9]  2  4    13  7     2  2     5 25
					# [ 10] 13  9     1 12     7  2     5  1

					if( aCStep[1]%in%c(  3, 1     ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  5     ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 13      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  8     ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aCStep[1:2+0]==c(  5, 5  )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  5, 1  )) ) cnt<-cnt+1

					if( all(aCStep[1:2+1]==c(  2, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 13, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 14, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 19, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  4, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  8, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 13, 8  )) ) cnt<-cnt+1

					if( all(aCStep[1:2+2]==c(  2,20 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  5, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  8, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 12, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  5, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  4, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3, 5 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c(  2, 2 )) ) cnt<-cnt+1
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
					if( aCStep[1]%in%c(  9     ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(        ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  9     ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(        ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(        ) ) cnt<-cnt+1

					# [  1]  9  9  4     3 21  6     4  5 12
					# [  2]  9 11  3                 9 21  3
					# [  3]                          2  2  7

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
					if( all(quoSize[1:3+1]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[3]%in%c( 39    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 41,33 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4, 7          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,NA,NA,41,42 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 23,22,33,40,35 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 28,41,38,43,44,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(          ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(  6, 3    ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  3, 9    ),c( 39    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  8, 2, 3 ),c( 41,33 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  8, 9    ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(          ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5, 2   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			# -------------------------------------------------------------------------------------
			#     FV :    2 (2)   3 (2)   5 (5)   9 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 10

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  9 28 33 36 45    | 5 19  5  3  9 |                        |2 0 1 2 1 |2 1 2 1
			#      1  4  8 13 37 39(1) | 3  4  5 24  2 | -3  -5 -20 -20   1  -6 |3 1 0 2 0 |3 1 2
			#     16 25 33 38 40 45    | 9  8  5  2  5 | 15  21  25  25   3   6 |0 1 1 2 2 |1 1 2 2

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
			# -------------------------------------------------------------------------------------
			#     FV :    -20 (2)   25 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 5 ]*c(5,7,2)==aFStep[c(1,2,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(3,5)]*c(1,2)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1	# 46

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
					if( (aZoid[6]-aZoid[1]) %in% c( 31 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+1]==c(2,1,0)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,2	unique 2 1 2 1 0 (대칭:1 0 3 0 2)
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,1,0
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
			if( aZoid[1]%in%c(  1, 7 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29,40 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 32,41 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,NA,NA,NA,32 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,NA,NA,29    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,13,21,16       ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,20    ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,23 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 16,NA,21,24,25 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <22>			# <27>
			# <29>
			if( fCutU.hasPtn(c(  1,NA,NA,NA,29    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          29,32 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,21,16,29    ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  9,25,33 ),aZoid) ) cnt<-cnt+1
			# <34>
			# <42>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 10,32,39,30,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,7     ),c(  1, 7 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,3     ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,0     ),c( 29,40 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,1     ),c( 32,41 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  8, 2    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 13       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  8,11    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3, 4    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 13, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  8, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  8, 8 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  4, 8 ),aCStep) )	cnt.w1<-cnt.w1+1	#	unique a,8,a
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (4)   3 (2)   4 (3)   5 (3)   8 (5)   13 (4) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(6,3,1,4)==aCStep[c(1,2,3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(5,2)]*c(1,2)==aCStep[c(3,2)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 20 22 27 40 43    |13  2  5 13  3 |                        |1 0 3 0 2 |1 3 2
			#      2 11 21 34 41 42    | 9 10 13  7  1 | -5  -9  -1   7   1  -1 |1 1 1 1 2 |1 1 1 1 2
			#      1  5 13 26 29 34(1) | 4  8 13  3  5 | -1  -6  -8  -8 -12  -8 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 11 22 42    | 1  5  2 11 20 |  2  -1  -4 -15  -7   8 |3 1 1 0 1 |3 1 1 1
			#      1  9 17 21 29 33(1) | 8  8  4  8  4 | -2   5   8  10   7  -9 |2 1 2 1 0 |2 1 2 1
			#     15 27 33 35 43 45(1) |12  6  2  8  2 | 14  18  16  14  14  12 |0 1 1 2 2 |1 1 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -3    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -10    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3, -1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -8 (3)   -1 (4)   7 (2)   8 (2)   14 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(1,1)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# 30
			if( sum(aFStep[c(4,3)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# 30
			if( sum(aFStep[c(5,3)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# 30

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(3,1,0)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[1]%in%c(  6, 3,10  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 13        ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  7        ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(           ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 22        ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 23,15     ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,15       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,18    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,15,NA,NA,26 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6, 7       ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  6, 7       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,10,11 ),aZoid) ) cnt<-cnt+1
			# <12>
			# <15>
			if( fCutU.hasPtn(c(  6,NA,15       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,18 ),aZoid) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c( 11,NA,NA,20,41 ),aZoid) ) cnt<-cnt+1
			# <23>
			if( fCutU.hasPtn(c(          23,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 21,21,22,23    ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 20,22,31 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(  4,25,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 11,36,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,3,1,0 ),c(  6, 3,10  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c( 13        )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,1     ),c(  7        )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3       ),c(           )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,3     ),c( 22        )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,5     ),c( 23,15     )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 18 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 11 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2,11 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   3 (2)   4 (4)   5 (2)   6 (4)   7 (2)   8 (2)   11 (2)   13 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,3)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,2,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 12 18 31 38 43    | 6  6 13  7  5 |                        |1 2 0 2 1 |1 2 2 1
			#     12 25 29 35 42 43(2) |13  4  6  7  1 |  6  13  11   4   4   0 |0 1 2 1 2 |1 2 1 2
			#      3  6  7 20 21 39    | 3  1 13  1 18 | -9 -19 -22 -15 -21  -4 |3 0 2 1 0 |3 2 1
			#      6  7 15 16 20 31(3) | 1  8  1  4 11 |  3   1   8  -4  -1  -8 |2 2 1 1 0 |2 2 1 1
			#      1  9 12 23 39 43    | 8  3 11 16  4 | -5   2  -3   7  19  12 |2 1 1 1 1 |2 1 1 1 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  5   2   3  -6 -16  -3 |1 3 1 0 1 |1 3 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  2,  1 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (2)   -3 (2)   2 (2)   3 (2)   4 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,3)]*c(-3,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 3 ]*c(-2,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 2 ]*c(-3,-8)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# -1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1	# -9

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,0,3)) ) return(FALSE)	# next rebind of 0,2,2
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
			if( aZoid[2]%in%c( 31    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 17,28 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 32    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c(  6,11 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,27,34 ),aZoid) ) cnt<-cnt+1
			# <17>
			# <30>
			if( fCutU.hasPtn(c( 18,26,30,44,45 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <38>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4       ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c( 31    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,7     ),c( 17,28 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,1     ),c( 32    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 1    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4, 3, 9 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 17, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 4 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (4)   4 (5)   6 (2)   7 (3)   11 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,6)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 11 13 17 20 31    | 7  2  4  3 11 |                        |1 3 1 1 0 |1 3 1 1
			#      8 24 28 35 38 40    |16  4  7  3  2 |  4  13  15  18  18   9 |1 0 2 2 1 |1 2 2 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 | -5 -18 -18  -5  -4  -3 |2 1 0 3 0 |2 1 3
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 11   9   6 -13   4   8 |0 4 0 1 1 |4 1 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -9  -4  -4  12  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 |  7   7  18  10   8  -2 |0 2 0 2 2 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -6, -3 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  -3     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -3, -4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -18 (2)   -5 (3)   -4 (3)   4 (2)   7 (2)   8 (2)   9 (2)   18 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,6)]*c( 1,-4)==aFStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 6 ]*c(-9,-5,-4)==aFStep[c(3,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2,4)])==sum(aFStep[c(3,5,6)]) )	cnt.w2<-cnt.w2+1	# 24

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,1)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[1]%in%c(  1, 3,10  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  8,13     ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(           ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(           ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 22        ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 23,13     ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,11, 8,28,44 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,15       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,18    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,15,NA,NA,26 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  3, 7       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     7,10,32 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  6,NA,15       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,18 ),aZoid) ) cnt<-cnt+1
			# <16>
			# <20>
			if( fCutU.hasPtn(c( 11,NA,NA,20,41 ),aZoid) ) cnt<-cnt+1
			# <23>
			if( fCutU.hasPtn(c(          23,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    21,22,23    ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c( 11, 4,25,39 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,3,0    ),c(  1, 3,10 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8        ),c(  8,13    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1        ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3        ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,1      ),c( 22       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,7      ),c( 23,13    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 11       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 16, 1    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4,18, 6 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 16, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 16, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2,11 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   3 (3)   4 (4)   5 (2)   6 (3)   8 (2)   11 (2)   16 (2)   17 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,3)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,2,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  6  7 20 21 39    | 3  1 13  1 18 |                        |3 0 2 1 0 |3 2 1
			#      6  7 15 16 20 31(3) | 1  8  1  4 11 |  3   1   8  -4  -1  -8 |2 2 1 1 0 |2 2 1 1
			#      1  7 16 18 34 38(2) | 6  9  2 16  4 | -5   0   1   2  14   7 |2 2 0 2 0 |2 2 2
			#      2  7 13 25 42 45(1) | 5  6 12 17  3 |  1   0  -3   7   8   7 |2 1 1 0 2 |2 1 1 2
			#      1  9 12 23 39 43    | 8  3 11 16  4 | -1   2  -1  -2  -3  -2 |2 1 1 1 1 |2 1 1 1 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  5   2   3  -6 -16  -3 |1 3 1 0 1 |1 3 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  2,  0 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -4     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -3 (3)   -2 (2)   -1 (3)   0 (2)   1 (3)   2 (3)   3 (2)   7 (3)   8 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,3)]*c(-3,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 3 ]*c(-2,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 2 ]*c(-3,-8)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# -1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1	# -9

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
					if( (aZoid[6]-aZoid[1]) %in% c( 32,43,31 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag


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
			if( aZoid[2]%in%c( 10, 6    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 25,12, 2 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA, 9,19    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,12,17,27 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>			# <18>
			# <24>
			if( fCutU.hasPtn(c( 20,17,21,24 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  1,13,31 ),aZoid) ) cnt<-cnt+1
			# <33>
			# <36>
			if( fCutU.hasPtn(c(  6,NA,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    23,NA,35,36 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA, 9,19,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6        ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,6,2    ),c( 10, 6    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,1      ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,5,7,2  ),c( 25,12, 2 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5        ),c( 29       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,4,6    ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 11, 2, 1, 6 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 15          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  8          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4, 5       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  6,15 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  1, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 11,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (5)   4 (3)   5 (3)   6 (3)   8 (2)   10 (2)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(1,3)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  8 13 23 31 36    | 2  5 10  8  5 |                        |2 1 1 2 0 |2 1 1 2
			#      2 12 37 39 41 45    |10 25  2  2  4 | -4   4  24  16  10   9 |1 1 0 2 2 |1 1 2 2
			#      7 18 31 33 36 40    |11 13  2  3  4 |  5   6  -6  -6  -5  -5 |1 1 0 3 1 |1 1 3 1
			#      2  7 15 24 30 45(1) | 5  8  9  6 15 | -5 -11 -16  -9  -6   5 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 26 27 29 33    |11 14  1  2  4 | -1   5  11   3  -1 -12 |1 1 3 1 0 |1 1 3 1
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 |  9  -1 -14  -9  -5   9 |0 4 1 0 1 |4 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -1     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -6     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -9,-4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -6 (3)   -5 (4)   -1 (3)   5 (3)   9 (3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-1, 1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 2,4 )])==sum(aFStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# -10
			if( sum(aFStep[c( 2,4 )])==sum(aFStep[c(6,3,5)]) )	cnt.w2<-cnt.w2+1	# -10

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
					if( all(quoSize[1:3+1]==c(1,0,2)) ) return(FALSE)	# next rebind of 3,1,0
					if( all(quoSize[1:3+2]==c(0,1,2)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[1]%in%c( 12,13 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 13,10 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 24,14 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 27    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <14>
			if( fCutU.hasPtn(c( 14,NA,27 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,25 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 15,25 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,NA,25,30 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c( 17,26,44 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 26,30,NA,31,41,34 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <33>
			# <40>
			if( fCutU.hasPtn(c(  7,12,16,40,45 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,2,3   ),c( 12,13 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3,0     ),c( 13,10 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4       ),c( 24,14 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,7     ),c( 27    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 6     ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  7        ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3        ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(           ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 7, 3,14 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 9 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  1, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (3)   3 (3)   5 (2)   6 (2)   7 (2)   8 (2)   9 (2)   10 (3) 

			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  6 15 25 26 33    | 2  9 10  1  7 |                        |2 1 2 1 0 |2 1 2 1
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -2   4  -3   6   7   9 |1 2 0 2 1 |1 2 2 1
			#      1 21 26 36 40 41    |20  5 10  4  1 | -1  11  14   5   7  -1 |1 0 2 1 2 |1 2 1 2
			#     14 20 23 31 37 38    | 6  3  8  6  1 | 13  -1  -3  -5  -3  -3 |0 1 2 3 0 |1 2 3
			#     14 15 25 28 29 30(1) | 1 10  3  1  1 |  0  -5   2  -3  -8  -8 |0 2 3 1 0 |2 3 1
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 | -1  -1  -6  -2  11  13 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -2,  1    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  11, -6     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  -2      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  -1, -7     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  11, -2     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -3,11 ),aFStep) )	cnt.w1<-cnt.w1+1	# unique -3,11,12
			if( fCutU.hasPtn(c( 11,12 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -8 (2)   -5 (2)   -3 (5)   -2 (2)   -1 (5)   7 (2)   11 (2)   13 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,4)]*c(1,3)==aFStep[c(1,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(1,2,4,5)]) )	cnt.w2<-cnt.w2+1	# 7

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
			if( aZoid[2]%in%c( 25,15    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 29       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33,29,34 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 31       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,17          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,14,NA,24,NA,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,NA,34,43,43 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  2,15,17,34,37 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c(  6,NA,17          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,17,24,NA,45 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c(  4,13,21,28,29 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 25,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 25,NA,31 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 25,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 28,31 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 18,13,NA,NA,29 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 29,33 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 27,30 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 29,33 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0       ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,0     ),c( 25,15    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8       ),c( 29       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,9,4,1 ),c( 33,29,34 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2       ),c( 31       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2, 1    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  6,11, 3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  7, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   3 (6)   4 (3)   5 (2)   6 (3)   7 (2)   10 (2)   13 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(2,3)]*c(1,6)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 10 17 18 21 29    | 4  7  1  3  8 |                        |1 3 2 0 0 |1 3 2
			#      6 12 17 21 34 37(3) | 6  5  4 13  3 |  0   2   0   3  13   8 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36    | 9  3 13  6  4 | -5  -2  -4   5  -2  -1 |1 2 1 2 0 |1 2 1 2
			#     14 15 25 28 29 30    | 1 10  3  1  1 | 13   5  12   2  -3  -6 |0 2 3 1 0 |2 3 1
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 | -6   0  -4   3   4   8 |1 1 1 3 0 |1 1 1 3
			#      2 25 28 30 33 45(1) |23  3  2  3 12 | -6  10   7  -1   0   7 |1 0 2 2 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -6, 13, -7     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  -4      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -3      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   6      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (3)   -4 (2)   -2 (2)   -1 (2)   0 (4)   2 (2)   3 (2)   5 (2)   7 (2)   8 (2)   13 (2) 

			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c( 3,1,4 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c( 6,1,4 )]) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 43,27,44 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[2]%in%c(  6       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  7       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 19       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,35    ) ) cnt<-cnt+1
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
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  1,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,12    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,19 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(    12,19 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,19 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 15,NA,NA,21,36,29 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <30>
			# <39>
			if( fCutU.hasPtn(c(  9,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       39,43 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  9,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       39,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,8,3    ),c(  2, 8, 3 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4,6,1    ),c(  6       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,7      ),c(  7       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,2      ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9        ),c( 19       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,3,4    ),c( 45,35    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4,23, 6 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 11, 2    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 11, 3    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4,26, 1 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3,13 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  6, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    2 (3)   3 (4)   4 (6)   5 (2)   7 (2)   9 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(2,3)]*c(1,6)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      1  3 12 21 26 41(1) | 2  9  9  5 15 | -4  -6   0  -9 -13  -2 |2 1 2 0 1 |2 1 2 1
			#      1  9 12 23 39 43(2) | 8  3 11 16  4 |  0   6   0   2  13   2 |2 1 1 1 1 |2 1 1 1 1
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  7   3   7  -2  -8  -8 |1 2 1 2 0 |1 2 1 2
			#      2  6  7 12 19 45(2) | 4  1  5  7 26 | -6  -6 -12  -9 -12  10 |3 2 0 0 1 |3 2 1
			#      2 25 28 30 33 45(2) |23  3  2  3 12 |  0  19  21  18  14   0 |1 0 2 2 1 |1 2 2 1

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
			if( fCutU.hasPtn(c(  -9,-11 ),aFStep) )	cnt.w1<-cnt.w1+1	# - 
			# -------------------------------------------------------------------------------------
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -9 (2)   -8 (2)   -6 (3)   -2 (2)   0 (5)   2 (2)   7 (2) 

			cnt.w2 <- 0

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
					if( all(quoSize[1:3+0]==c(0,1,3)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,1,1	unique 1,1,1,1
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,1,1
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
			if( aZoid[1]%in%c(  3, 8,10 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 29       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,11,31,40,34 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 8>
			if( fCutU.hasPtn(c(  8,NA,NA,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,NA,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,22,29       ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 21,25,30,45 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(  1, 3,26,39 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(  8,NA,NA,30    ),aZoid) ) cnt<-cnt+1
			# <34>
			# <41>
			if( fCutU.hasPtn(c(  4,19,21,41 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 18,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    25,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       26,32,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,8,0,6 ),c(  3, 8,10 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9       ),c( 29       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,3     ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,5,8,9 ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,3     ),c( 44       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  8      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 10, 9   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 12, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 6, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 9, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5,10 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (4)   3 (3)   4 (3)   5 (2)   8 (4)   9 (2)   11 (2)   12 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 16 25 30 42 43    | 8  9  5 12  1 |                        |1 1 1 1 2 |1 1 1 1 2
			#     11 13 25 26 29 33(1) | 2 12  1  3  4 |  3  -3   0  -4 -13 -10 |0 2 3 1 0 |2 3 1
			#      3  5  7 14 26 34(1) | 2  2  7 12  8 | -8  -8 -18 -12  -3   1 |3 1 1 1 0 |3 1 1 1
			#      9 30 34 35 39 41(1) |21  4  1  4  2 |  6  25  27  21  13   7 |1 0 0 4 1 |1 4 1
			#      3  8 19 27 30 41(2) | 5 11  8  3 11 | -6 -22 -15  -8  -9   0 |2 1 1 1 1 |2 1 1 1 1
			#     17 25 28 37 43 44    | 8  3  9  6  1 | 14  17   9  10  13   3 |0 1 2 1 2 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -3    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  -1, -8 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -3, 2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -8 (3)   -3 (2)   0 (2)   3 (2)   13 (2) 
			cnt.w2 <- 0
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 27
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 23
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(4,5,6)]) )	cnt.w2<-cnt.w2+1	# 26

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

	# 발견된 과거 사례가 단 하나 뿐.
	#	6 19 21 35 40 45 | 

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
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0

			cnt.w1 <- 0

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(2,4)]*c(7,1)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  6 19 21 35 40 45    |13  2 14  5  5 |                        |1 1 1 1 2 |1 1 1 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep

			cnt.w2 <- 0

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
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,1,2
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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,29,NA,39 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 16,25,34 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8       ),c( 28 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1       ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4       ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5       ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6       ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			# -------------------------------------------------------------------------------------
			#     FV :  
			cnt.w2 <- 0
			if( aCStep[1]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  5 13 26 29 34    | 4  8 13  3  5 |                        |2 1 2 1 0 |2 1 2 1
			#      5 21 27 34 44 45(2) |16  6  7 10  1 |  4  16  14   8  15  11 |1 0 2 1 2 |1 2 1 2

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
			# -------------------------------------------------------------------------------------
			#     FV :  
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(4,2)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1	# 19
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1	# 26

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












