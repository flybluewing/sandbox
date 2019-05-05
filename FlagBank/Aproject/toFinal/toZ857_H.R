# toZ857_H.R 최종접근
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
	# allIdxF <- rmvFV3( gEnv ,allIdxF )
	save( allIdxF ,file="Obj_allIdxF.fCut.basic.save")

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
			if( aZoid[1]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 41      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 19,42    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 43,36    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44,43,42 ) ) cnt<-cnt+1	# unique 44
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 8>
			if( fCutU.hasPtn(c(  8,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <17>
			# <26>
			if( fCutU.hasPtn(c( 24,26       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    26,NA,44 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 24,31 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 12,33,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 12,33,NA,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             43,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    26,NA,44 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,7        ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3,4,5        ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,1        ),c( 41       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,2        ),c( 19,42    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,6        ),c( 43,36    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,3,2,0        ),c( 44,43,42 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  8, 6      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 3      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 7, 2, 6    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (5)   3 (2)   4 (4)   5 (2)   6 (3)   7 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(7,8)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(3,4)]*c(1,7)==aCStep[c(5,1)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(3,4)]*c(1,8)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     14 18 22 26 31 44    | 4  4  4  5 13 |                        |0 2 2 1 1 |2 2 1 1
			#     11 17 28 30 33 35    | 6 11  2  3  2 | -3  -1   6   4   2  -9 |0 2 1 3 0 |2 1 3
			#      2  8 23 26 27 44    | 6 15  3  1 17 | -9  -9  -5  -4  -6   9 |2 0 3 0 1 |2 3 1
			#     20 25 31 32 36 43    | 5  6  1  4  7 | 18  17   8   6   9  -1 |0 0 2 3 1 |2 3 1
			#      8 15 17 19 43 44(1) | 7  2  2 24  1 |-12 -10 -14 -13   7   1 |1 3 0 0 2 |1 3 2
			#     10 24 40 41 43 44(2) |14 16  1  2  1 |  2   9  23  22   0   0 |0 1 1 0 4 |1 1 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  -11      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   -1     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   -1    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (3)   -1 (2)   0 (2)   2 (2)   6 (2)   9 (3) 
			cnt.w2 <- 0

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.basic()

# UNdone
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

					# [1]    2 14 11
					# [2]   34 20 22 38 33
					# [3]   16 20 12 17 33
					# [4]   
					# [5]*  38 41 39 39 43 24 20
					# [6]   45 31 41 40 41 31 39 45 25

					if( 1<sum(aZoid==c(  2,34,16,NA,38,45 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 14,20,20,NA,41,31 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 11,22,12,NA,39,41 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,38,17,NA,39,40 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 33,33,NA,NA,43,41 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,24,31 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,20,39 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					# unique	3<=[](NA,NA,12,NA,35,NA)
					if( 2<=sum(aZoid==c(NA,NA,12,NA,35,NA),na.rm=T) ) cnt<-cnt+1

					#	seqNextVal
					if( 1<sum(aZoid==c( NA,NA,NA,NA,22,39 ) ,na.rm=T) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(     ) ) score<-score+1
					if( aZoid[2]%in%c(     ) ) score<-score+1
					if( aZoid[3]%in%c(     ) ) score<-score+1	
					if( aZoid[4]%in%c(     ) ) score<-score+1
					if( aZoid[5]%in%c(     ) ) score<-score+1
					if( aZoid[6]%in%c(     ) ) score<-score+1	
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
					if( aZoid[1]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 27,10   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 19      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 38,36   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41,42,43 ) ) cnt<-cnt+1

					if( all(aZoid[1:2+4]==c( 40,43 )) ) cnt<-cnt+1

					# if( all(aZoid[c( , )]==c( , )) ) cnt<-cnt+1
					# if( all(aZoid[1:2+ ]==c( , )) ) cnt<-cnt+1
					# if( all(aZoid[c( , )]==c( , )) ) cnt<-cnt+1	# -

					# [  1] 13 16    13 21    16 18             24 42
					# [  2]  8 28    11 16     8 17             39 43
					# [  3] 22 27    12 28    44 45             38 43
					# [  4] 26 29    12 14    25 33             27 31
					# [  5]  1 17    26 29    34 41             36 41
					# [  6] 11 19    19 25    26 31             38 40
					# [  7] 20 23     1 13     4 16             39 41
					# [  8]           2  5     8 22             30 35
					# [  9]           3 13    13 33             36 40
					# [ 10]          27 34     5 11             33 35
					# [ 11]           5 24    25 36             20 25


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 3      ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 7,8,0  ),c( 27,10 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c(        ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 9      ),c( 19    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 8,6    ),c( 38,36 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 3,1    ),c( 41,42,43 )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( , ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
								if( aZoid[3]==40   && fCutU.remFilt(aZoid[4],c( 0 ),c( 19 )) ) remCnt <- remCnt+1
						# grp (1:2+3)
						# grp (1:2+4)
								if( aZoid[5]==36   && fCutU.remFilt(aZoid[6],c( 2 ),c( 41,42,43 )) ) remCnt <- remCnt+1
								if( aZoid[6]==43   && fCutU.remFilt(aZoid[5],c( 0 ),c( 38,36 )) ) remCnt <- remCnt+1

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

					score  <- sum(aCStep==c(  3, 6, 2,NA,18 ),na.rm=T)
					matCnt <- sum(aCStep==c( 20, 5, 9,NA, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5,16, 1,NA, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 2, 8,NA, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 16, 3, 7,NA, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  8, 6, 5,NA, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,12,12,NA, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 3,14,NA, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,10,20,NA, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 7, 6,NA, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,19,11,NA, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  3  20   5   3  16   8   3 
					#	[2]  8   5  16   2   3   6  12   3  10   7  19 
					#	[3]  2   9   1   8   7   5  12  14  20   6  11   3   8 
					#	[4]
					#	[5] 18   4   5   4   5   2   2   5   4   2   5 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1 # -

						#	unique	3>[]( 5, 1,16,NA,NA )
						
						# unique 3<=sum(aCStep==c( NA,11,NA,NA,32 ),na.rm=T)
						if( 2<=sum(aCStep==c( NA,11,NA,NA,32 ),na.rm=T) ) cnt<-cnt+1

						if( aCStep[1]%in%c(  7    ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(       ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 10    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  2    ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  4, 2 ) ) cnt<-cnt+1

						if( 1<sum( aCStep[ 3 ]*c(4,9)==aCStep[c(2,5)] ) )	cnt<-cnt+1	# unique

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
					#        *  *  *     *  *  *     *  *  *     *  *  *
					# [  1]             11 16 18                        
					# [  2]              1 13 33                        
					# [  3]              2  5 11                        
					# [  4]             27 34 42 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.colValSeqNext()

# UNdone
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

					# [1]*  15 13  3  2  2 10  3  6  6 16  6 13  7  7  3  7  2
					# [2]*   6  1  7  5  1 20 10  1  5  5  2  4  7  4  6  3  1  3 11  1  3  6 20  2 13  6  4  5  9 19 10  1  5
					# [3]*   6  5 12  8 17  8  1 20  2  2  8  1  4  1  6  9  2 10 20  6  8 12  2  2  3  6  5  2  5  8  5  7 24 13...
					# [4]   12  4  3
					# [5]    6  2  1  4  6 12  8  7 22  7  3 11  5  9  4 13  7  5  4  8  1 25  5 22


					tCnt <- 0
						if( aCStep[1]%in%c( 7      ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[2]%in%c(        ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[3]%in%c( 7, 5   ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[4]%in%c(        ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[5]%in%c(        ) ) tCnt<-tCnt+1	# double(  )

						if( 1<sum( aCStep[ 2 ]*c(1,2,1)==aCStep[c(3,4,5)] ) )	cnt<-cnt+1
						if( 1<sum( aCStep[c(2,3)]*c(2,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					#	unique	3<=[]( NA, 4, 4, 5, 4 )
					#	unique	3<=[]( 17,11, 7,20,10 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1
					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1	# -
					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1

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
					# [  1]  4  3     6 16     5  4     4  1
					# [  2]  3 10     2 12     1  4     3  1
					# [  3] 10  7     1  7    11  9     2  5
					# [  4]  5 20     5  2                  
					# [  5]  2 13     4  6                  
					# [  6]  5  6     2  9                  
					# [  7]  5  3     2 20                  
					# [  8]  8  5     6  9                  
					# [  9]  2  8     2  8                  
					# [ 10]  5 13     1  6                  


					if( aCStep[1]%in%c(  5, 3    ) ) cnt<-cnt+1  # double( )
					if( aCStep[2]%in%c(  8, 6    ) ) cnt<-cnt+1  # double( )
					if( aCStep[3]%in%c(          ) ) cnt<-cnt+1  # double( )
					if( aCStep[4]%in%c(  4, 9, 5 ) ) cnt<-cnt+1  # double( )
					if( aCStep[5]%in%c(  1, 5    ) ) cnt<-cnt+1  # double( )

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  3,11 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  4, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  4, 1 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  3, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  9, 5 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  5, 6 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+1]==c(  1, 8 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  2,10 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  4,11 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  5, 2 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  5, 1 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  3, 1 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  4, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  3, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  3, 6 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  1, 7 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c( 11, 9 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  3,12 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+3]==c(  5, 1 )) ) cnt<-cnt+1

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
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					#        *  *  *     *  *  *     *  *  *
					# [  1]  5  3 14                 5  4  1
					# [  2]  9  1 24                        

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
					#   zoid width  ... 18   33   33   33   38   23 and ?
					#     10 22 27 31 42 43(2) |12  5  4 11  1 |  6  12  13  16  24  21 |0 1 2 1 2 |1 2 1 2
					#     10 15 21 35 38 43(2) | 5  6 14  3  5 |  0  -7  -6   4  -4   0 |0 2 1 2 1 |2 1 2 1
					#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(1,3,2)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[5]%in%c( 40,33 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,22 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <10>
			if( fCutU.hasPtn(c( 10,NA,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,11,NA,43 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  6,15,24 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 10,NA,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,21,43 ),aZoid) ) cnt<-cnt+1
			# <22>
			# <39>
			# <42>
			if( fCutU.hasPtn(c( 14,12,19,37,42 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(          40,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5, 5, 7,NA,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 7        ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,7      ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,2,4,7  ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,1      ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,9      ),c( 40,33 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,2      ),c( 43,22 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 5,6,3,4   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 5         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 3         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 3,5       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 2,3,5,8,6 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 4, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 18, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 8 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   3 (5)   4 (7)   5 (5)   6 (3)   8 (3)   11 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      4 10 14 15 18 22    | 6  4  1  3  4 |                        |1 4 1 0 0 |1 4 1
			#     10 22 27 31 42 43(2) |12  5  4 11  1 |  6  12  13  16  24  21 |0 1 2 1 2 |1 2 1 2
			#     10 15 21 35 38 43(2) | 5  6 14  3  5 |  0  -7  -6   4  -4   0 |0 2 1 2 1 |2 1 2 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
			#      5 10 13 21 39 43    | 5  3  8 18  4 | -7  -7 -10 -13  -3  -2 |1 2 1 1 1 |1 2 1 1 1
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11  10  11   7  -3  -4 |0 1 3 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -7      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -4       ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -3, 4,-2    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -7,-8 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (3)   -4 (2)   -3 (2)   0 (2)   2 (4)   4 (2)   11 (2) 			
			cnt.w2 <- 0
			if( aFStep[4]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 

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
					if( (aZoid[6]-aZoid[1]) %in% c( 27 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+0]==c(2,2,0)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+1]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,2,2
					if( all(quoSize[1:3+2]==c(0,1,1)) ) return(FALSE)	# next rebind of 2,2,0
					#	unique	(0 2 2 2 0)	rebind. (2 2 0 1 1) ban
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
			if( aZoid[3]%in%c( 24       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29,35,31  ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 35        ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <10>
			# <11>
			if( fCutU.hasPtn(c( 11,NA,16,16,25 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c(       13,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       13,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11, 6,13,NA,23    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(       13,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          16,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11, 6,NA,16,23    ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(  9,10,30 ),aZoid) ) cnt<-cnt+1
			# <37>
			if( fCutU.hasPtn(c( 21,NA,35,NA,37 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(       13,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11, 6,NA,NA,23,43 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,6        ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,3        ),c( 24       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,4,0        ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,1        ),c( 29,35,31 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,5        ),c( 35       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 6, 4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5,12    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3,21 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  1,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 12, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   3 (4)   4 (3)   5 (6)   6 (3)   12 (2)   13 (2) 

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     16 17 22 30 37 43    | 1  5  8  7  6 |                        |0 2 1 2 1 |2 1 2 1
			#      4  9 13 18 21 34    | 5  4  5  3 13 |-12  -8  -9 -12 -16  -9 |2 2 1 1 0 |2 2 1 1
			#      1 10 13 16 37 43(1) | 9  3  3 21  6 | -3   1   0  -2  16   9 |1 3 0 1 1 |1 3 1 1
			#     10 11 23 24 36 37(2) | 1 12  1 12  1 |  9   1  10   8  -1  -6 |0 2 2 2 0 |2 2 2
			#     11 15 20 26 31 35(1) | 4  5  6  5  4 |  1   4  -3   2  -5  -2 |0 2 2 2 0 |2 2 2
			#      6  8 13 16 30 43    | 2  5  3 14 13 | -5  -7  -7 -10  -1   8 |2 2 0 1 1 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  5      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  0      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -3       ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -9 (2)   -7 (2)   -5 (2)   -3 (2)   -2 (2)   -1 (2)   1 (3)   8 (2)   9 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(2,1)==aFStep[c(4,3)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,5)])==sum(aFStep[c(1,2,6,3)]) )	cnt.w2<-cnt.w2+1	# -11

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
					if( (aZoid[6]-aZoid[1]) %in% c( 28 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+1]==c(0,1,2)) ) return(FALSE)	# next rebind of 1,0,1
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(2,1,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,1,0
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
			if( aZoid[2]%in%c(  9, 6 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 39    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 40,42 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2, 7          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,29,40    ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  2, 7          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,29,40 ),aZoid) ) cnt<-cnt+1
			# < 8>
			if( fCutU.hasPtn(c(  8,NA,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8, 9,NA,45 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,16 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  1,18,31 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 21,25,NA,30 ),aZoid) ) cnt<-cnt+1
			# <39>
			# <42>
			if( fCutU.hasPtn(c(       42,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,33,42    ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(       42,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,33,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 9,7        ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9,6,8,2    ),c(  9, 6 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,6        ),c( 17    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,7,6,9     ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,8        ),c( 39    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0,2        ),c( 40,42 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 3, 4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5, 7    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 8    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 10,17 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  7, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 12, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (4)   4 (2)   5 (4)   7 (3)   12 (2) 

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     11 12 14 15 18 39    | 1  2  1  3 21 |                        |0 5 0 1 0 |5 1
			#      1  2  7  9 10 38    | 1  5  2  1 28 |-10 -10  -7  -6  -8  -1 |4 1 0 1 0 |4 1 1
			#      4  8 27 34 39 40    | 4 19  7  5  1 |  3   6  20  25  29   2 |2 0 1 2 1 |2 1 2 1
			#     15 17 25 37 42 43    | 2  8 12  5  1 | 11   9  -2   3   3   3 |0 2 1 1 2 |2 1 1 2
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 |-13 -10  -6 -12 -13  -7 |2 1 2 1 0 |2 1 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  4   1  -1  10  13   7 |2 1 0 1 2 |2 1 1 2

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
			if( fCutU.hasPtn(c( -10, -5 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (2)   -10 (3)   -7 (2)   -6 (2)   -1 (2)   3 (4) 
			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 17
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1	# 11

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
					if( all(quoSize[1:3+0]==c(3,1,0)) ) return(FALSE)	# next rebind of 0,1,2
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
			if( aZoid[3]%in%c( 23,25 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 38,35 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7, 9          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,NA,NA,33 ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  7, 9          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,33 ),aZoid) ) cnt<-cnt+1
			# <23>
			# <24>
			if( fCutU.hasPtn(c(    24,29       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,24,NA,28,35 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c(    24,29       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,29,NA,35 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 25,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    27,31,38 ),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(       27,40 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,2        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,5        ),c( 23,25 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,5,2        ),c( 38,35 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (5)   3 (3)   5 (3)   6 (2)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,4)]*c(2,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     16 24 29 40 41 42    | 8  5 11  1  1 |                        |0 1 2 0 3 |1 2 3
			#      1 10 23 26 28 40(1) | 9 13  3  2 12 |-15 -14  -6 -14 -13  -2 |1 1 3 0 1 |1 1 3 1
			#      5  7  9 11 32 35    | 2  2  2 21  3 |  4  -3 -14 -15   4  -5 |3 1 0 2 0 |3 1 2
			#     14 20 23 31 37 38    | 6  3  8  6  1 |  9  13  14  20   5   3 |0 1 2 3 0 |1 2 3
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -7 -11   1  -2  -3   0 |2 0 2 2 0 |2 2 2

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
			#     FV :    -15 (2)   -14 (3)   -3 (2)   -2 (2)   4 (2) 
			cnt.w2 <- 0
			if( aFStep[4]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# -10

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
					if( all(quoSize[1:3+0]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(1,0,0)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[1]%in%c(  2,20  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  9     ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17     ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 40      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# < 8>
			# <10>
			if( fCutU.hasPtn(c( 10,12 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,19 ),aZoid) ) cnt<-cnt+1
			# <18>
			# <25>
			if( fCutU.hasPtn(c( 21,25,NA,30 ),aZoid) ) cnt<-cnt+1
			# <36>
			# <37>
			if( fCutU.hasPtn(c(  5, 9,NA,37,38 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c( 12,12,NA,38,35,40 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(       42,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,33,42    ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(       42,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,33,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,7,0        ),c(  2,20 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9        ),c(  9    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7        ),c( 17    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,6,4,9        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0        ),c( 40    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 4, 6    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 7       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 5,10    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(10, 6, 8 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  5,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  7, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 17, 7 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 12, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (3)   4 (4)   5 (3)   6 (2)   7 (3)   12 (2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      4  8 27 34 39 40    | 4 19  7  5  1 |                        |2 0 1 2 1 |2 1 2 1
			#     15 17 25 37 42 43    | 2  8 12  5  1 | 11   9  -2   3   3   3 |0 2 1 1 2 |2 1 1 2
			#      4 10 14 15 18 22(1) | 6  4  1  3  4 |-11  -7 -11 -22 -24 -21 |1 4 1 0 0 |1 4 1
			#      8 10 13 36 37 40(1) | 2  3 23  1  3 |  4   0  -1  21  19  18 |1 2 0 2 1 |1 2 2 1
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 | -6  -3   6 -11  -8  -4 |2 1 2 1 0 |2 1 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  4   1  -1  10  13   7 |2 1 0 1 2 |2 1 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -11   ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  -33   ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -11, -9 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(   2, -1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(   4,  2 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (3)   -1 (2)   3 (3)   4 (2) 
			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c(1,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 17
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1	# 11

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
					if( (aZoid[6]-aZoid[1]) %in% c( 33,18 ) ) return( FALSE )
					#   zoid width  ... 31   34   36   18   33   33 and ?
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(4,1,0)) ) return(FALSE)	# next rebind of 0,2,2
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
			if( aZoid[1]%in%c(  4,36     ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 16      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 36      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 50      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# <10>
			if( fCutU.hasPtn(c( 10,16 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,14,16,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,33,34 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  6,15,24 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 24,27 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c( 23,NA,26,41 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 24,27 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,27       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          27,42,43 ),aZoid) ) cnt<-cnt+1
			# <34>
			# <35>
			if( fCutU.hasPtn(c(  3, 8,35 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 15,24,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,3,6        ),c(  4,36 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,5        ),c( 16    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,1,6        ),c( 36    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,1        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0        ),c( 50    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2,18       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 11, 4       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 20, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  5, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 6 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (4)   3 (2)   4 (2)   5 (3)   6 (2)   11 (3)   20 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(2,1)==aCStep[c(2,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(5,2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      4 24 25 27 34 35    |20  1  2  7  1 |                        |1 0 3 2 0 |1 3 2
			#      5  6 26 27 38 39(1) | 1 20  1 11  1 |  1 -18   1   0   4   4 |2 0 2 2 0 |2 2 2
			#      1 12 29 34 36 37    |11 17  5  2  1 | -4   6   3   7  -2  -2 |1 1 1 3 0 |1 1 1 3
			#      4 10 14 15 18 22    | 6  4  1  3  4 |  3  -2 -15 -19 -18 -15 |1 4 1 0 0 |1 4 1
			#     12 14 24 26 34 45(1) | 2 10  2  8 11 |  8   4  10  11  16  23 |0 2 2 1 1 |2 2 1 1
			#     10 15 21 35 38 43    | 5  6 14  3  5 | -2   1  -3   9   4  -2 |0 2 1 2 1 |2 1 2 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  8     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  5,-2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -18 (2)   -15 (2)   -2 (5)   1 (3)   3 (2)   4 (4) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-2, 1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,3)]*c( 1,-3)==aFStep[c(6,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,3)]*c(-2,-3)==aFStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,1,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,6,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1	# 5
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,6,5)]) )	cnt.w2<-cnt.w2+1	# -1

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
					#   zoid width  ... 22   38   41   32   38   43 and ?
					#		32	 6  7 18 19 30 38
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(1,1,0)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,1,1
					#	unique (2 2 0 1) rebind. ban [2:5](2 1 1 0)
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
			if( aZoid[3]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28,29 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,22,23,39 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6, 7             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,25,21,27,32 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  6, 7             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     7,11    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,25,14,18,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  7, 9,16,29 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(     7,11    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,11,31 ),aZoid) ) cnt<-cnt+1
			# <12>
			# <24>
			if( fCutU.hasPtn(c(       24,NA,43 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,30 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 17,15,31,25,44 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,9      ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2        ),c( 12    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,0,4,7  ),c( 28,29 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5        ),c( 45    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 5      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 10,11        ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  5,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  4, 5 )) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 12, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 11, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5,12 )) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   3 (3)   4 (4)   5 (2)   6 (2)   11 (6)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(1,3)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1	# 16
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      1  2  3  9 12 23    | 1  1  6  3 11 |                        |4 1 1 0 0 |4 1 1
			#      6  7 11 17 33 44    | 1  4  6 16 11 |  5   5   8   8  21  21 |2 2 0 1 1 |2 2 1 1
			#      4  7 11 24 42 45(2) | 3  4 13 18  3 | -2   0   0   7   9   1 |2 1 1 0 2 |2 1 1 2
			#      6  7 18 19 30 38(1) | 1 11  1 11  8 |  2   0   7  -5 -12  -7 |2 2 0 2 0 |2 2 2
			#      5  9 14 26 30 43(1) | 4  5 12  4 13 | -1   2  -4   7   0   5 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  0,-7  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  7,-1  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -2     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -1     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0, 7 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( -1,-2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -2,-2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -1, 0 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (2)   -2 (2)   -1 (3)   0 (4)   1 (2)   2 (2)   5 (3)   7 (3)   8 (2)   21 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(4,5)]*c(2,1)==aFStep[c(1,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+0]==c(2,2,0)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,2,1
					#	unique (1 2 2 1) rebind. ban [2:5](1 1 0 2)
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
			if( aZoid[2]%in%c( 14       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 38,36,15 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,39    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <12>
			if( fCutU.hasPtn(c( 12,14 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 12,14 ),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c( 17,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,26    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,27       ),aZoid) ) cnt<-cnt+1
			# <23>
			if( fCutU.hasPtn(c( 17,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 23,26    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 23,NA,34 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 17,NA,27       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    26,27,33,33 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <37>
			# <38>
			if( fCutU.hasPtn(c(  5,NA,38,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5         ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,4,6     ),c( 14       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,2,8,6,5 ),c( 38,36,15 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6         ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,1,9     ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,9,6     ),c( 43,39    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(   6     ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  21     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(   3, 5  ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(   3     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  7,10 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   3 (4)   4 (3)   5 (2)   6 (3)   8 (4)   10 (2)   21 (2) 

			cnt.w2 <- 0
			if( aCStep[5]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     11 12 14 15 18 39    | 1  2  1  3 21 |                        |0 5 0 1 0 |5 1
			#      8 17 20 27 37 43    | 9  3  7 10  6 | -3   5   6  12  19   4 |1 1 2 1 1 |1 1 2 1 1
			#      3  4 12 14 25 43(1) | 1  8  2 11 18 | -5 -13  -8 -13 -12   0 |2 2 1 0 1 |2 2 1 1
			#      5 13 17 23 28 36    | 8  4  6  5  8 |  2   9   5   9   3  -7 |1 2 2 1 0 |1 2 2 1
			#     17 23 27 35 38 43(2) | 6  4  8  3  5 | 12  10  10  12  10   7 |0 1 2 2 1 |1 2 2 1
			#      6 16 37 38 41 45(1) |10 21  1  3  4 |-11  -7  10   3   3   2 |1 1 0 2 2 |1 1 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  10, 5 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  10, 2 ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -12    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 11, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (2)   -7 (2)   2 (2)   3 (3)   5 (2)   9 (2)   10 (4)   12 (3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(5,6)]*c(1,5)==aFStep[c(4,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# -1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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
	if( 0==nrow(zMtx) ){
		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
		cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntMtx.colName) )
		return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..
	}

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
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique	()
			#	unique	[]()

			cnt.w2 <- 0
			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  none
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
			#	unique	()
			#	unique	[]()
			
			cnt.w2 <- 0
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
					if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,1,3)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 0,2,1
					#	unique (2 1 1 1 1) rebind. ban (0 2 2 1 1)
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
			if( aZoid[1]%in%c( 12, 5    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  9       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 26       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41,30,21 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,33,38    ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,18,31,26,21    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,31,33,36 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 19,12,21,37 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <23>
			# <28>
			if( fCutU.hasPtn(c( 28,NA,43 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 20,NA,NA,30,45 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 11,18,30,34,38 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(  9,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 25,NA,40,NA,42 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,5,3        ),c( 12, 5    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9        ),c(  9       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,6,7        ),c( 26       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2        ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1        ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,0      ),c( 41,30,21 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 11       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 16       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 10, 9    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3,10    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    2(4)   3(2)   4(3)   5(3)   6(3)   7(2)   8(4)   9(2)   11(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      7 12 16 34 42 45    | 5  4 18  8  3 |                        |1 2 0 1 2 |1 2 1 2
			#      2  9 15 23 34 40(1) | 7  6  8 11  6 | -5  -3  -1 -11  -8  -5 |2 1 1 1 1 |2 1 1 1 1
			#      1  9 12 28 36 41(1) | 8  3 16  8  5 | -1   0  -3   5   2   1 |2 1 1 1 1 |2 1 1 1 1
			#      5 16 21 23 24 30    |11  5  2  1  6 |  4   7   9  -5 -12 -11 |1 1 3 1 0 |1 1 3 1
			#     12 14 21 30 39 43(2) | 2  7  9  9  4 |  7  -2   0   7  15  13 |0 2 1 2 1 |2 1 2 1
			#     12 16 26 28 30 42(2) | 4 10  2  2 12 |  0   2   5  -2  -9  -1 |0 2 2 1 1 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(   1    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -5(3)   -3(2)   -2(2)   -1(3)   0(3)   2(2)   5(2)   7(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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
	# 	rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 33 ) ) return( FALSE )
					#   zoid width  ... 33   31   32   33   30   34 and ?
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,0)) ) return(FALSE)	# next rebind of 0,2,2
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
			if( aZoid[1]%in%c(  5      ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 16      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 44      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,36   ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <10>
			# <11>
			# <12>
			if( fCutU.hasPtn(c( 12,14,28 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 13,14          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,27,28,42 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,34,19,22 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  1,NA,18,NA,34 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  2,24,38 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 20,NA,NA,30,45 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 14,21,NA,38,36,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  6,27,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,2        ),c(  5    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4,6        ),c( 16    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,2        ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8,5,1,4        ),c( 44    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,6        ),c( 42,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 16          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3, 1       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2,19       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 18, 2,10, 3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    1(7)   2(5)   4(3)   6(2)   9(2)   10(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      3 11 14 15 32 36    | 8  3  1 17  4 |                        |1 3 0 2 0 |1 3 2
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 |  9   3   7  15   7   7 |0 2 1 2 1 |2 1 2 1
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 | -2  -3  -9 -12 -15  -1 |0 4 1 0 1 |4 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 | -5  -5   4   0  13  -4 |2 2 0 2 0 |2 2 2
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  10  10  10  -7   4 |0 2 2 1 1 |2 2 1 1
			#     10 24 40 41 43 44    |14 16  1  2  1 | -2   8  14  13  13   2 |0 1 1 0 4 |1 1 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(   9    ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -15    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    -5(2)   -2(2)   4(2)   7(4)   10(3)   13(3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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
			if( aZoid[1]%in%c( 31      ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 23      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 28      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 43      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <20>
			if( fCutU.hasPtn(c( 20,26 ),aZoid) ) cnt<-cnt+1
			# <24>
			# <28>
			# <31>
			# <36>
			if( fCutU.hasPtn(c( 22,30,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <43>

			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,1        ),c( 31 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c( 23 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 8        ),c( 28 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,3        ),c( 43 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,1,7        ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,4        ),c( 45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6, 7      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 2      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2, 1      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  6, 1, 3  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(5)   3(4)   4(3)   6(3)   7(2)   9(2)   16(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      2 21 28 38 42 45    |19  7 10  4  3 |                        |1 0 2 1 2 |1 2 1 2
			#      9 18 20 24 27 36    | 9  2  4  3  9 |  7  -3  -8 -14 -15  -9 |1 1 3 1 0 |1 1 3 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -5 -11  -7   5   4   3 |2 1 1 2 0 |2 1 1 2
			#     11 17 28 30 33 35    | 6 11  2  3  2 |  7  10  15   1   2  -4 |0 2 1 3 0 |2 1 3
			#     20 25 31 32 36 43    | 5  6  1  4  7 |  9   8   3   2   3   8 |0 0 2 3 1 |2 3 1
			#     10 24 40 41 43 44(1) |14 16  1  2  1 |-10  -1   9   9   7   1 |0 1 1 0 4 |1 1 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  1, 9     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  6,10      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  2      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(2)   3(3)   7(3)   8(2)   9(3)
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

fltCntMtx.lastPtn	<- function( ccObjLst ,allIdxF ){
	# 	# w1, w2는 이전에서 정산되어 있어야 한다.

	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })
	phName <- attributes(cntMtxLst)$names

	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF
	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )
	
	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )

	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )


    #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
	#	nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6 

	cat("todo..\n")
	allIdxF.len <- length(allIdxF)
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)
	for( aIdx in seq_len(allIdxF.len) ){
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="fStep021")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	# fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( list( allIdxF=allIdxF ,fltCnt=fltCnt ) )

} # fltCntMtx.lastPtn()

fltScoreMtx.lastPtn	<- function( ccObjLst ,allIdxF ){
	allIdxF.len <- length( allIdxF )
	scoreMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx })
	phName <- attributes(scoreMtxLst)$names
	colName <- colnames(scoreMtxLst[[1]])

	scoreMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx) <- phName	;colnames(scoreMtx)=colName
	scoreMtx.evt <- scoreMtx

		cat("todo..\n")
	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx[,] <- 0		;scoreMtx.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx[phIdx,] <- scoreMtxLst[[phIdx]][aIdx,]
			#	scoreMtx.evt[phIdx,] <- scoreMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3 w1CStep.cnt w1FStep.cnt w1CStep.matLen w1FStep.matLen
		}
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="remHn.00")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx.lastPtn()

fltScoreMtx2.lastPtn	<- function( ccObjLst ,allIdxF ){
	
	allIdxF.len <- length( allIdxF )
	scoreMtx2Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx2 })
	phName <- attributes(scoreMtx2Lst)$names
	colName <- colnames(scoreMtx2Lst[[1]])

	scoreMtx2 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx2) <- phName	;colnames(scoreMtx2)=colName
	scoreMtx2.evt <- scoreMtx2

		cat("todo..\n")
	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx2[,] <- 0		;scoreMtx2.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx2[phIdx,] <- scoreMtx2Lst[[phIdx]][aIdx,]
			scoreMtx2.evt[phIdx,] <- scoreMtx2[phIdx,] >= c( 3, 2, 2, 1, 1, 100,100, 2, 2 )	# rebL.cnt,rebR.cnt은 100으로
			#	"rebV" "rebC" "rebC2" "rebL" "rebR" "rebL.cnt" "rebR.cnt" "inc.raw" "inc.cStep"
		}

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx2.lastPtn()

fltScoreMtx3.lastPtn	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx3Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx3 })
	phName <- attributes(scoreMtx3Lst)$names
	colName <- colnames(scoreMtx3Lst[[1]])

	scoreMtx3 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx3) <- phName	;colnames(scoreMtx3)=colName
	scoreMtx3.evt <- scoreMtx3

		cat("todo..\n")
	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx3[,] <- 0		;scoreMtx3.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx3[phIdx,] <- scoreMtx3Lst[[phIdx]][aIdx,]
			scoreMtx3.evt[phIdx,] <- scoreMtx3[phIdx,] >= c( 1, 1, 2, 2, 2, 2 )	# 
			#	rebPtn.1 rebPtn.n rebC.C1 rebC.F1 rebC.C2 rebC.F2
		}

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx3.lastPtn()

fltScoreMtx4.lastPtn	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx4Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx4 })
	phName <- attributes(scoreMtx4Lst)$names
	colName <- colnames(scoreMtx4Lst[[1]])

	scoreMtx4 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx4) <- phName	;colnames(scoreMtx4)=colName
	scoreMtx4.evt <- scoreMtx4

		cat("todo..\n")
	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx4[,] <- 0		;scoreMtx4.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx4[phIdx,] <- scoreMtx4Lst[[phIdx]][aIdx,]
			scoreMtx4.evt[phIdx,] <- scoreMtx4[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	incRaw3     incC3     incF3   incRaw2     incC2     incF2     (1,6) nextVal.r nextVal.c nextVal.f 
		}

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx4.lastPtn()

fltCStepValMtx.lastPtn	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	cStepValMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$cStepValMtx })
	phName <- attributes(cStepValMtxLst)$names
	colName <- colnames(cStepValMtxLst[[1]])

	cStepValMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(cStepValMtx) <- phName	;colnames(cStepValMtx)=colName
	cStepValMtx.evt <- cStepValMtx

		cat("todo..\n")
	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		cStepValMtx[,] <- 0		;cStepValMtx.evt[,] <- 0
		for( phIdx in phName ){
			cStepValMtx[phIdx,] <- cStepValMtxLst[[phIdx]][aIdx,]
			# cStepValMtx.evt[phIdx,] <- cStepValMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	c31 c32 c33 c34 	c21 c22 c23 c24 c25 	max2 min2
		}

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltCStepValMtx.lastPtn()


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
		aBin <- aZoid %%  2
		aRem <- aZoid %% 10

		# #	a,b,b.. a?
		# 	if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 42,42,33,39, 8,41 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	a,b,b.. b?
		# 	if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 39,45,34,42, 4,33 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	a,a,b.. b?
		# 	if( 2<sum(aZoid[c( 6,6 )]==c( 44,19 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	q,a,b,a.. q?
		# 	if( 2<sum(aZoid[c( 6,4,3 )]==c( 43,36,26 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	이번엔 죄다 5,6 컬럼들 뿐이라서..

		# -	a,b,b.. a?	nextQuo10 nextColVal_4
			if( 2<sum(aZoid[c( 4,3 )]==c( 38,12 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aZoid[c( 4,3 )]==c( 26,26 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	a,a,b.. a?	nextBin nextColVal_5
			if( 2<sum(aZoid[c( 6,4 )]==c( 43,18 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	a,a,b.. b?
			if( 2<sum(aZoid[c( 6,4 )]==c( 36,28 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	q,a,b,a.. q?	basic nextZW nextCStepBin nextColVal_5
			if( 2<sum(aZoid[c( 6,5,1,6 )]==c( 35,31,17,43 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. a?
			if( 2<sum(aZoid[c( 6,5,1,6 )]==c( 44,26, 5,42 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. b?
			if( 2<sum(aZoid[c( 6,5,1,6 )]==c( 43,40,13,38 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	a,a+1,b,a+2?
			# if( 2<sum(aZoid[c( ,,,, )]==c( ,,,, )) ){	surviveFlg[idx]<-FALSE	;next }

		#	bin pattern
		if( all(aBin[1:6]==c(  1, 0, 1, 0, 0, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextZW
		if( all(aBin[ -3]==c(  0, 0, 0, 1, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextRebNum
		if( all(aBin[c(-1,-4)]==c(  0, 1, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextFStepBin
		if( all(aBin[1:6]==c(  0, 1, 1, 0, 0, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_2
		if( all(aBin[ -2]==c(  1, 1, 0, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_3
		if( all(aBin[ -6]==c(  1, 0, 1, 0, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_4
		if( all(aBin[ -2]==c(  0, 1, 0, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_5

		# fCutCnt.basic()
			#     16 20 24 28 36 39    | 4  4  4  8  3 |                        |0 1 3 2 0 |1 3 2
			#     14 18 22 26 31 44    | 4  4  4  5 13 | -2  -2  -2  -2  -5   5 |0 2 2 1 1 |2 2 1 1
			#     11 17 28 30 33 35    | 6 11  2  3  2 | -3  -1   6   4   2  -9 |0 2 1 3 0 |2 1 3
			#      2  8 23 26 27 44    | 6 15  3  1 17 | -9  -9  -5  -4  -6   9 |2 0 3 0 1 |2 3 1
			#     20 25 31 32 36 43    | 5  6  1  4  7 | 18  17   8   6   9  -1 |0 0 2 3 1 |2 3 1
			#      8 15 17 19 43 44(1) | 7  2  2 24  1 |-12 -10 -14 -13   7   1 |1 3 0 0 2 |1 3 2
			if( all(aZoid[5:6]==c(44,45)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 1,23)) ){	surviveFlg[idx]<-FALSE	;next }		# zw 22
			if( all(aZoid[c(1,6)]==c( 1,39)) ){	surviveFlg[idx]<-FALSE	;next }		# zw 38

		# fCutCnt.nextZW
			#      1 28 35 41 43 44    |27  7  6  2  1 |                        |1 0 1 1 3 |1 1 1 3
			#      2 11 17 18 21 27    | 9  6  1  3  6 |  1 -17 -18 -23 -22 -17 |1 3 2 0 0 |1 3 2
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  6   1   2   3  10   8 |1 2 1 2 0 |1 2 1 2
			#      4  6 15 25 26 33    | 2  9 10  1  7 | -4  -6  -4   4  -5  -2 |2 1 2 1 0 |2 1 2 1
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   8   4   1  14  10 |0 3 1 0 2 |3 1 2
			#      1  9 11 14 26 28(2) | 8  2  3 12  2 |-12  -5  -8 -12 -14 -15 |2 2 2 0 0 |2 2 2
			if( all(aZoid[c(1,6)]==c( 4,33)) ){	surviveFlg[idx]<-FALSE	;next }		# zw 29

		# fCutCnt.nextQuo10
			#      5  6 11 14 21 41    | 1  5  3  7 20 |                        |2 2 1 0 1 |2 2 1 1
			#      2  6  7 12 19 45(1) | 4  1  5  7 26 | -3   0  -4  -2  -2   4 |3 2 0 0 1 |3 2 1
			#     14 20 23 31 37 38    | 6  3  8  6  1 | 12  14  16  19  18  -7 |0 1 2 3 0 |1 2 3
			#      2 21 28 38 42 45(1) |19  7 10  4  3 |-12   1   5   7   5   7 |1 0 2 1 2 |1 2 1 2
			#     13 14 19 26 40 43    | 1  5  7 14  3 | 11  -7  -9 -12  -2  -2 |0 3 1 0 2 |3 1 2
			#     14 18 22 26 31 44(2) | 4  4  4  5 13 |  1   4   3   0  -9   1 |0 2 2 1 1 |2 2 1 1
			if( all(aZoid[c(1,6)]==c(15,45)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 2,45)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	43
			if( all(aZoid[c(1,6)]==c(14,44)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	30

		# fCutCnt.nextBin
			#      2  3 12 20 27 38    | 1  9  8  7 11 |                        |2 1 2 1 0 |2 1 2 1
			#      2  6 13 16 29 30(1) | 4  7  3 13  1 |  0   3   1  -4   2  -8 |2 2 1 1 0 |2 2 1 1
			#      5  9 12 30 39 43(1) | 4  3 18  9  4 |  3   3  -1  14  10  13 |2 1 0 2 1 |2 1 2 1
			#     10 15 21 35 38 43(1) | 5  6 14  3  5 |  5   6   9   5  -1   0 |0 2 1 2 1 |2 1 2 1
			#      5 10 13 21 39 43(3) | 5  3  8 18  4 | -5  -5  -8 -14   1   0 |1 2 1 1 1 |1 2 1 1 1
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 |  8   6  11   4  -6  -7 |0 2 2 2 0 |2 2 2
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,5,6)]==c( 5,39,43)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(2,3)]==c(11,14)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 5,43)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	38
			if( all(aZoid[c(1,6)]==c( 2,38)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	36

		# fCutCnt.nextRebNum
			#      5  7 20 22 37 42    | 2 13  2 15  5 |                        |2 0 2 1 1 |2 2 1 1
			#      1  2  4 23 31 34    | 1  2 19  8  3 | -4  -5 -16   1  -6  -8 |3 0 1 2 0 |3 1 2
			#     10 20 33 36 41 44    |10 13  3  5  3 |  9  18  29  13  10  10 |0 1 1 2 2 |1 1 2 2
			#      7 10 17 29 33 44(3) | 3  7 12  4 11 | -3 -10 -16  -7  -8   0 |1 2 1 1 1 |1 2 1 1 1
			#     15 27 33 35 43 45(1) |12  6  2  8  2 |  8  17  16   6  10   1 |0 1 1 2 2 |1 1 2 2
			#     11 24 32 33 35 40(2) |13  8  1  2  5 | -4  -3  -1  -2  -8  -5 |0 1 1 3 1 |1 1 3 1
			if( all(aZoid[c(1,6)]==c( 7,44)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	37

		# fCutCnt.nextCStepBin
			#      7 22 24 31 34 36    |15  2  7  3  2 |                        |1 0 2 3 0 |1 2 3
			#     10 15 21 35 38 43    | 5  6 14  3  5 |  3  -7  -3   4   4   7 |0 2 1 2 1 |2 1 2 1
			#     17 25 28 37 43 44(1) | 8  3  9  6  1 |  7  10   7   2   5   1 |0 1 2 1 2 |1 2 1 2
			#      5 10 13 21 39 43(1) | 5  3  8 18  4 |-12 -15 -15 -16  -4  -1 |1 2 1 1 1 |1 2 1 1 1
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 |  8   6  11   4  -6  -7 |0 2 2 2 0 |2 2 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 | -8  -3  -7   4   1   3 |1 2 1 2 0 |1 2 1 2

		# fCutCnt.nextFStepBin
			#      1  3  7  8 24 42    | 2  4  1 16 18 |                        |4 0 1 0 1 |4 1 1
			#      4 13 18 31 33 45    | 9  5 13  2 12 |  3  10  11  23   9   3 |1 2 0 2 1 |1 2 2 1
			#     14 21 29 31 32 37(1) | 7  8  2  1  5 | 10   8  11   0  -1  -8 |0 1 2 3 0 |1 2 3
			#      3  4  9 24 25 33    | 1  5 15  1  8 |-11 -17 -20  -7  -7  -4 |3 0 2 1 0 |3 2 1
			#     12 14 21 30 39 43    | 2  7  9  9  4 |  9  10  12   6  14  10 |0 2 1 2 1 |2 1 2 1
			#      2  8 33 35 37 41    | 6 25  2  2  4 |-10  -6  12   5  -2  -2 |2 0 0 3 1 |2 3 1

		# fCutCnt.nextColVal_1
			#      7 10 17 29 33 44    | 3  7 12  4 11 |                        |1 2 1 1 1 |1 2 1 1 1
			#     15 19 21 34 41 44(1) | 4  2 13  7  3 |  8   9   4   5   8   0 |0 2 1 1 2 |2 1 1 2
			#     12 15 18 28 34 42(2) | 3  3 10  6  8 | -3  -4  -3  -6  -7  -2 |0 3 1 1 1 |3 1 1 1
			#      6 12 17 21 34 37(2) | 6  5  4 13  3 | -6  -3  -1  -7   0  -5 |1 2 1 2 0 |1 2 1 2
			#      1  3 12 14 16 43(1) | 2  9  2  2 27 | -5  -9  -5  -7 -18   6 |2 3 0 0 1 |2 3 1
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 | 12  13  12  11  17  -7 |0 2 2 2 0 |2 2 2
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 4,37)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	33

		# fCutCnt.nextColVal_2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -2  -6  -9  -7  -3 |1 1 2 2 0 |1 1 2 2
			#      5  6 13 16 27 28    | 1  7  3 11  1 |  2  -4 -10  -8  -4 -11 |2 2 2 0 0 |2 2 2
			#      6  7 18 19 30 38(1) | 1 11  1 11  8 |  1   1   5   3   3  10 |2 2 0 2 0 |2 2 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  18  15  19  10   7 |0 1 1 2 2 |1 1 2 2
			#     13 16 24 25 33 36(3) | 3  8  1  8  3 | -3  -9  -9 -13  -7  -9 |0 2 2 2 0 |2 2 2
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 5,41)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	36

		# fCutCnt.nextColVal_3
			#     13 15 18 24 27 41    | 2  3  6  3 14 |                        |0 3 2 0 1 |3 2 1
			#     13 14 26 28 30 36(1) | 1 12  2  2  6 |  0  -1   8   4   3  -5 |0 2 2 2 0 |2 2 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -8  -8 -15 -14  -9   5 |2 2 1 0 1 |2 2 1 1
			#      6 21 35 36 37 41(3) |15 14  1  1  4 |  1  15  24  22  16   0 |1 0 1 3 1 |1 1 3 1
			#      3  9 11 12 13 19    | 6  2  1  1  6 | -3 -12 -24 -24 -24 -22 |2 4 0 0 0 |2 4
			#     16 20 24 28 36 39    | 4  4  4  8  3 | 13  11  13  16  23  20 |0 1 3 2 0 |1 3 2

		# fCutCnt.nextColVal_4
			#      3  6 10 30 34 37    | 3  4 20  4  3 |                        |2 1 0 3 0 |2 1 3
			#      3  4 16 20 28 44(1) | 1 12  4  8 16 |  0  -2   6 -10  -6   7 |2 1 2 0 1 |2 1 2 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  3  14  15  14  10   1 |1 1 0 3 1 |1 1 3 1
			#      2  6  7 12 19 45(2) | 4  1  5  7 26 | -4 -12 -24 -22 -19   0 |3 2 0 0 1 |3 2 1
			#      3 10 13 26 34 38    | 7  3 13  8  4 |  1   4   6  14  15  -7 |1 2 1 2 0 |1 2 1 2
			#     13 14 19 26 40 43(2) | 1  5  7 14  3 | 10   4   6   0   6   5 |0 3 1 0 2 |3 1 2

		# fCutCnt.nextColVal_5
			#     17 23 27 35 38 43    | 6  4  8  3  5 |                        |0 1 2 2 1 |1 2 2 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 |-14 -12 -13 -20  -6  -7 |1 3 0 2 0 |1 3 2
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 |  9   3   7  15   7   7 |0 2 1 2 1 |2 1 2 1
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 | -2  -3  -9 -12 -15  -1 |0 4 1 0 1 |4 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 | -5  -5   4   0  13  -4 |2 2 0 2 0 |2 2 2
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  10  10  10  -7   4 |0 2 2 1 1 |2 2 1 1
			if( all(aRem[c(1,2)]==aRem[c(6,3)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c(12,43)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	31

		# fCutCnt.nextColVal_6
			#     10 11 12 18 24 42    | 1  1  6  6 18 |                        |0 4 1 0 1 |4 1 1
			#      2 21 28 38 42 45(1) |19  7 10  4  3 | -8  10  16  20  18   3 |1 0 2 1 2 |1 2 1 2
			#      9 18 20 24 27 36    | 9  2  4  3  9 |  7  -3  -8 -14 -15  -9 |1 1 3 1 0 |1 1 3 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -5 -11  -7   5   4   3 |2 1 1 2 0 |2 1 1 2
			#     11 17 28 30 33 35    | 6 11  2  3  2 |  7  10  15   1   2  -4 |0 2 1 3 0 |2 1 3
			#     20 25 31 32 36 43    | 5  6  1  4  7 |  9   8   3   2   3   8 |0 0 2 3 1 |2 3 1
			if( all(aZoid[c(1,6)]==c( 9,31)) ){	surviveFlg[idx]<-FALSE	;next }		# zw	22

			# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }
			# if( all(aRem[c(,)]==aRem[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 2<=sum(c(,,,,,)[c(,,)]==aZoid[c(,,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			# if( all(c(,,,,,)[c(,)]==aZoid[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

			# if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
			# ptnLst <- list( c(,) ,c(,) )
			# if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 2<=sum(c(,,,,,)[c(,,)]==aZoid[c(,,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			# if( all(c(,,,,,)[c(,)]==aZoid[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			# if( all(aRem[c(,)]==aRem[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }

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
		aRem <- aZoid %% 10

		# anaColEndPtn()
		if( 2<sum(aZoid==c(  2,34,16,NA,38,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 14,20,20,NA,41,31 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 11,22,12,NA,39,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( NA,38,17,NA,39,40 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 33,33,NA,NA,43,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }


		# cnt <- 0
		# 	if( aZoid[1]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[2]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[3]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[4]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[5]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[6]%in%c(  ) ) cnt<-cnt+1
		# if( 4<cnt ) {	surviveFlg[idx]<-FALSE	;next }

		# if( aRem[4]==c(6) ){	surviveFlg[idx]<-FALSE	;next }		# if( fCutU.remFilt(aZoid[4],c( 6,8,9  ),c( 16 )) )

		# colValSeqNext( ,pColSize=2 )
		#	3개 이상 일치.
		score <- sum(aCStep==c(  3, 6, 2,NA,18 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 20, 5, 9,NA, 4 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  5,16, 1,NA, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  3, 2, 8,NA, 4 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 16, 3, 7,NA, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  8, 6, 5,NA, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  3,12,12,NA, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( NA, 3,14,NA, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( NA,10,20,NA, 4 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( NA, 7, 6,NA, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( NA,19,11,NA, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }

		#	unique	3>[]( 5, 1,16,NA,NA )
		if( 3<=sum(aCStep==c( 5, 1,16,NA,NA ),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		# unique 3<=sum(aCStep==c( NA,11,NA,NA,32 ),na.rm=T)
		if( 3<=sum(aCStep==c( NA,11,NA,NA,32  ),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[ 3 ]*c(4,9)==aCStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }	# unique


		cnt <- 0
			# ...
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		#        *  *  *     *  *  *     *  *  *     *  *  *
		# [  1]             11 16 18                        
		# [  2]              1 13 33                        
		# [  3]              2  5 11                        
		# [  4]             27 34 42 
		if( all(aZoid[1:3+1]==c( 11, 16, 18 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+1]==c(  1, 13, 33 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+1]==c(  2,  5, 11 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+1]==c( 27, 34, 42 )) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext()

# UNdone		fCut.basic() 사용
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

		# anaColEndPtn()
		if( 1<sum( aCStep[ 2 ]*c(1,2,1)==aCStep[c(3,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(2,3)]*c(2,1)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		#	unique	3<=[]( NA, 4, 4, 5, 4 )
		#	unique	3<=[]( 17,11, 7,20,10 )

		cnt <- 0
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }


		# colValSeqNext( ,pColSize=3 )
		#        *  *  *     *  *  *     *  *  *
		# [  1]  5  3 14                 5  4  1
		# [  2]  9  1 24                        
		if( all(aCStep[1:3+0]==c(  5,  3, 14 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aCStep[1:3+0]==c(  9,  1, 24 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aCStep[1:3+2]==c(  5,  4,  1 )) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,b,b.. a?	
			if( 2<sum(aCStep[c( 5,2,3 )]==c( 8, 1, 6 )) ){	surviveFlg[idx]<-FALSE	;next }	# a
			if( 2<sum(aCStep[c( 5,2,3 )]==c( 4,10, 2 )) ){	surviveFlg[idx]<-FALSE	;next }	# b
		#	a,a,b.. a?
			if( 2<sum(aCStep[c( 5,1,3,4,1 )]==c(  3, 5, 1, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# a
			if( 2<sum(aCStep[c( 5,1,3,4,1 )]==c( 13, 3, 4, 8, 4 )) ){	surviveFlg[idx]<-FALSE	;next }	# b
		#	q,a,b,a.. q? 
			if( 2<sum(aCStep[c( 5,3,2 )]==c(  8, 3, 2 )) ){	surviveFlg[idx]<-FALSE	;next }	# q
			if( 2<sum(aCStep[c( 5,3,2 )]==c( 27, 1, 6 )) ){	surviveFlg[idx]<-FALSE	;next }	# a
			if( 2<sum(aCStep[c( 5,3,2 )]==c(  3, 5,11 )) ){	surviveFlg[idx]<-FALSE	;next }	# b

		# fCutCnt.basic()
			if( 1<sum( aCStep[ 2 ]*c( 1,12)==aCStep[c(3,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 13, 1, 2) ( 3, 2, 1 ) ( 4, 4,NA, 7 ) ( 4*, 4*, 6,18 )
			#			( 4*, 4*, 4, 2,23 ) ( 4, 4*, 4*, 2,23 )
			#	unique	[2:3]( 3,17)	2/3,2/17
			if( fCutU.hasPtn(c( 13, 1, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4,NA, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 6,18 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 4, 2,23 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 4, 2,23 ),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(  3,17 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum( aCStep[ 5 ]*c(4,1,6)==aCStep[c(1,2,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(2,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7, 3, 9 ) ( 5, 7, 2 )
			#	unique	[1:4]( 7*, 10*,  4,  3 )	[1:4]( 7,10*, 4*, 3 )
			#	unique	( 4, 5 ) ( 5,13 )
			if( fCutU.hasPtn(c(  7, 3, 9 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 7, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c( 7,10)) && any(aCStep[c(3,4)]==c( 4, 3)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(10, 4)) && any(aCStep[c(1,4)]==c( 7, 3)) ) {	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 4, 5) ,c( 5,13) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4 )] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 3, 8,NA,12) ( 2, 3, 5 )
			#	unique	[2:4]( 1,a,1 ) [1:4]( 8*, 1*, 8, 3 ) [c(1,5)]==c( 8, 8)
			if( fCutU.hasPtn(c(  3, 8,NA,12 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 2,3,5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,5)]==c( 8, 8 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c( 8, 1 )) && any(aCStep[c(3,4)]==c(8,3)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,5)]==c( 3, 4 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3)+0]==c(1,1)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3)+1]==c(1,1)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3)+2]==c(1,1)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[ 5 ]==c( 2 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			#	unique	( 6,NA, 1, 5 )
			#	unique	[3:5](10,13, 3)
			if( fCutU.hasPtn(c(  6,NA, 1, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:5]==c(10,13, 3)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3,5)]==c( 1, 1 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[ 2 ]*c(2,3)==aCStep[c(1,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,4)]*c(2,1)==aCStep[c(1,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,4)]*c(3,1)==aCStep[c(3,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 3, 8,NA,12 )
			#	unique	[1:4]( 4*,12*, 5, 5) [4:5]( 3, 3) # 5/3
			if( fCutU.hasPtn(c(  3, 8,NA,12 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[4:5]==c( 3,3 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c( 4,12 )) && any(aCStep[c(3,4)]==c( 5,5 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( 1<sum( aCStep[ 3 ]*c(3,1,2)==aCStep[c(1,4,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(3,4)]*c(3,2)==aCStep[c(2,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7*,10*,16, 7 )
			#	unique	[3:4]( 6, 6)
			if( fCutU.hasPtn(c( 7,10,16, 7 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c( 6,6 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,5)]==c( 6,3 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 3, 7, 14)
			if( fCutU.hasPtn(c(  3, 7, 14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( a,1,a ) ( 1, 9, 3 ) ( 3, 5, 1 )
			#	unique	[1:3]( 1, 8, 3 ) [1:3]( 5, 2, 5 ) [2:4]( 8,a,8 )
			if( fCutU.hasPtn(c( 1, 9, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3, 5, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			cenIdx <- 2		;if( aCStep[cenIdx]==1 && (aCStep[cenIdx-1]==aCStep[cenIdx+1]) ){	surviveFlg[idx]<-FALSE	;next }
			cenIdx <- 3		;if( aCStep[cenIdx]==1 && (aCStep[cenIdx-1]==aCStep[cenIdx+1]) ){	surviveFlg[idx]<-FALSE	;next }
			cenIdx <- 4		;if( aCStep[cenIdx]==1 && (aCStep[cenIdx-1]==aCStep[cenIdx+1]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c( 1, 8, 3 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c( 5, 2, 5 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,4)]==c( 8,8 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 1, 1, 8 )
			#	unique	[1:5]( 4, 4, 4*, 8*, 3)
			#	unique	[2]( 1 )  # 2/1/1  2/3/1  2/4/?
			#			[5]( 1 )	# 3/1
			#			[1:3]( 6, 6, 6 )
			if( fCutU.hasPtn(c( 1, 1, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c( 4, 8 )) && any(aCStep[c(1,2,5)]==c(4,4,3)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c( 6, 6, 6 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( any(aCStep[c(2)]==c( 1 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#	unique	( 1*, 5*, 7, 2 )	( 1, 5*, 7*, 2 )	( 1*, 5, 7*, 2 ) ( 7, 3,13 ) ( 4, 8,10 ) ( 4,13, 3 )
			#	unique	2 < [1:3]( 3,13, 8)  [1:4]( 3*,13, 8, 4*)  [1:4]( 3,13*, 8, 4*)  [1:4]( 3,13, 8*, 4*)
			#			[3:5]( 4, 1, 5*)
			if( fCutU.hasPtn(c( 1, 5, 7, 2 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 1, 5, 7, 2 ),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 1, 5, 7, 2 ),aCStep,thld=3,fixIdx=c(1,3)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 7, 3,13 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 4, 8,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 4,13, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c( 3,13, 8 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,4)]==c(  3, 4 )) && any(aCStep[c(2,3)]==c( 13, 8)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,4)]==c( 13, 4 )) && any(aCStep[c(1,3)]==c( 13, 8)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3,4)]==c(  8, 4 )) && any(aCStep[c(1,2)]==c(  3,13)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[5]==c( 5 )) && any(aCStep[c(3,4)]==c( 4, 1)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 3 ]*c(2,5,1,6)==aCStep[c(1,2,4,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(3,1)==aCStep[c(5,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(3,5)==aCStep[c(5,2)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	if( 1<sum( aCStep[c(2,3)]*c(5,6)==aCStep[c(2,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7,10*, 2*,NA,23)
			#	unique	[]( 4,10*, 2*, 2,12 )	[1:3]( 2, 2,12 )
			if( fCutU.hasPtn(c(  7,10, 2,NA,23 ),aCStep,thld=3,fixIdx=2:3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c( 2, 2,12 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c( 10, 2 )) && any(aCStep[c(1,4,5)]==c( 4, 2,12)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			#	unique	( 11,NA, 4, 3 )
			#	unique	[1:4]( 6, 1, 4*, 7* )
			if( fCutU.hasPtn(c( 11,NA, 4, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c( 4,7 )) && any(aCStep[c(1,2)]==c( 6,1 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[ 4 ]==c( 5 )) ) {	surviveFlg[idx]<-FALSE	;next }

			# if( fCutU.hasPtn(c( ,, ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			# if( all(aCStep[:]==c( , )) ) {	surviveFlg[idx]<-FALSE	;next }
			# if( all(aCStep[:]==c( , )) && any(aCStep[c(,)]==c( ,)) ) {	surviveFlg[idx]<-FALSE	;next }
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
		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		#	if( all(aFStep[:]==c(,)) && any(aFStep[c(,)]==c(,))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(1,3)==aFStep[c(4,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		#	if( all(aFStep[:]==c(,)) && any(aFStep[c(,)]==c(,))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(1,-3)==aFStep[c(6,5)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			#	unique	( -11,-18,-20, -2*, -2*) ( 1*, 3*, -1, -5,-16)
			#	unique	[2:6](-12,   1,   5,   7*,   5*)
			if( fCutU.hasPtn(c( -11,-18,-20, -2, -2 ),aFStep,thld=3,fixIdx=4:5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   1, 3, -1, -5,-16 ),aFStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[5:6]==c(7,5)) && any(aFStep[c(2,3,4)]==c(-12, 1, 5))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[c(4,5)]*c( 2,-1)==aFStep[c(1,2)] ) ) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( 1<sum( aFStep[ 4 ]*c(2,4)==aFStep[c(1,5)] ) ) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(4,6)]*c(-2,-1)==aFStep[c(1,2)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-13,-16, 3*, 1*)
			if( fCutU.hasPtn(c( -13,-16, 3, 1 ),aFStep,thld=3,fixIdx=3:4 ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(2,5)]*c(-2, 1)==aFStep[c(3,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(2,4)]*c(-2,-2)==aFStep[c(3,1)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(5,4)]*c( 1,-2)==aFStep[c(6,1)] ) ) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			#	unique	( 2, -3, -6 )
			if( fCutU.hasPtn(c(  2,-3, 6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[ 1 ]*c(3,3,3)==aFStep[c(2,3,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 2 ]*c(1,1)==aFStep[c(3,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,2)]*c(3,1)==aFStep[c(3,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-10,NA, -8, -9 )
			if( fCutU.hasPtn(c( -10,NA, -8, -9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[c(5,6)]*c(1,2)==aFStep[c(3,1)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			#	unique	(  4*,  6*, -14, -15 ) ( 9,14,15 )
			#	unique	[]( 10,  4*,  6*,  0,  6,  5)  [3:6](  1,  4*,  6*, 14 )
			if( fCutU.hasPtn(c(  4,  6, -14, -15 ),aFStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,14,15 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[2:3]==c(4,6)) && any(aFStep[c(1,4,5,6)]==c(10, 0, 6, 5))) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[4:5]==c(4,6)) && any(aFStep[c(3,6)]==c( 1,14))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(1,1)==aFStep[c(3,4)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,2)]*c(-1, 1)==aFStep[c(5,3)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,2)]*c(-1, 1)==aFStep[c(5,4)] ) ) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(1,3)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 4 ]*c(4,4)==aFStep[c(2,6)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(2,3)]*c(1,1)==aFStep[c(6,5)] ) ) {	surviveFlg[idx]<-FALSE	;next }
			# if( 1<sum( aFStep[c(2,3)]*c(3,4)==aFStep[c(1,2)] ) ) {	surviveFlg[idx]<-FALSE	;next }

		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		#	if( all(aFStep[:]==c(,)) && any(aFStep[c(,)]==c(,))) {	surviveFlg[idx]<-FALSE	;next }

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
			if( fCutU.hasPtn(c( 16,23,23,20,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,26,NA,NA,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 19,NA,33,35 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  5,11,22,23,26,42 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,20,31,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextZW
			if( fCutU.hasPtn(c( 11,20,NA,NA,NA,36 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,NA,27,40,40 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 15,16,30,31,42,33 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,40,45,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,13,24 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 21,NA,21,33,34 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,35,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  6,17,36,39 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextQuo10
			if( fCutU.hasPtn(c(  9,36,28,29,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,12,NA,NA,19 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 13,25,28 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,29,32,42 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 25,38,26,35,43 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextBin
			if( fCutU.hasPtn(c(  1,NA,26,30,34,37 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,NA,13,30,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1,NA,26,30,34,37 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,28,33 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,16,34 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1, 8,28,42 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,18,26,41,45 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextRebNum
			if( fCutU.hasPtn(c(  2, 8,11,28,31,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,NA,13,14,25 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 13,NA,14,27 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,18,19,31 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 24,24,NA,25,38,35 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,18,21,21,28,35 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 21,19,31,27,NA,36 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextCStepBin
			if( fCutU.hasPtn(c( 11,18,21 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,17,23,44 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,17,23,44 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,19,NA,24 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,18,29 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextFStepBin
			if( fCutU.hasPtn(c(  3, 9,15,11,39,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,11,15,18 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,NA,18,13,32 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,11,15,18 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,22,20,35 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,18,31,NA,37 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12, 4, 4,37,38 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_1
			if( fCutU.hasPtn(c(  6,NA,20,43 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,36,NA,43,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  2, 8,12 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,NA,29,35,38 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,NA,29,35,38 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,32,42 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9, 7,24,NA,NA,43 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_2
			if( fCutU.hasPtn(c( 13,35,NA,45 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,29,NA,NA,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 19,20,37,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 24,25,21,37,41 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 17,34,31,38 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,23,41 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,23,NA,43 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,29,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_3
			if( fCutU.hasPtn(c(  7,23,26,NA,45 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,18,20 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 29,31,38 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 37,25,26,39 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_4
			if( fCutU.hasPtn(c(  1,11,14,34,38 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1,11,14,34,38 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,32,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,25,33,33 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4, 6,10,25,28 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,35,33,38 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_5
			if( fCutU.hasPtn(c(  2,NA,NA,36,39,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,10,12,36,32,38 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,14,16,NA,16 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,14,16 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18, 9, 7,10,27 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,10,12,36,32,38 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_6
			if( fCutU.hasPtn(c(  4,13,NA,37 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 17, 7,23,29 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7, 7,22,NA,NA,43 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

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

		# if( all(quoSize==c( ,,, )) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.basic
			#     16 20 24 28 36 39    | 4  4  4  8  3 |                        |0 1 3 2 0 |1 3 2
			#     14 18 22 26 31 44    | 4  4  4  5 13 | -2  -2  -2  -2  -5   5 |0 2 2 1 1 |2 2 1 1
			#     11 17 28 30 33 35    | 6 11  2  3  2 | -3  -1   6   4   2  -9 |0 2 1 3 0 |2 1 3
			#      2  8 23 26 27 44    | 6 15  3  1 17 | -9  -9  -5  -4  -6   9 |2 0 3 0 1 |2 3 1
			#     20 25 31 32 36 43    | 5  6  1  4  7 | 18  17   8   6   9  -1 |0 0 2 3 1 |2 3 1
			#      8 15 17 19 43 44(1) | 7  2  2 24  1 |-12 -10 -14 -13   7   1 |1 3 0 0 2 |1 3 2
				#	unique	(0 2 3 1) rebind as reverse. (2 1 3 0) reverse again?
			if( all(quoSize[2:5]==c(0,3,1,2)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum (zw)
			#      5  7 20 22 37 42    | 2 13  2 15  5 |                        |2 0 2 1 1 |2 2 1 1
			#      1  2  4 23 31 34    | 1  2 19  8  3 | -4  -5 -16   1  -6  -8 |3 0 1 2 0 |3 1 2
			#     10 20 33 36 41 44    |10 13  3  5  3 |  9  18  29  13  10  10 |0 1 1 2 2 |1 1 2 2
			#      7 10 17 29 33 44(3) | 3  7 12  4 11 | -3 -10 -16  -7  -8   0 |1 2 1 1 1 |1 2 1 1 1
			#     15 27 33 35 43 45(1) |12  6  2  8  2 |  8  17  16   6  10   1 |0 1 1 2 2 |1 1 2 2
			#     11 24 32 33 35 40(2) |13  8  1  2  5 | -4  -3  -1  -2  -8  -5 |0 1 1 3 1 |1 1 3 1
				#	unique	(0 1 1 2 2) rebind. ban (0 1 1 2 2),(2 0 2 1 1)
			if( all(quoSize==c(0,1,1,2,2)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(quoSize==c(2,0,2,1,1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			#      7 22 24 31 34 36    |15  2  7  3  2 |                        |1 0 2 3 0 |1 2 3
			#     10 15 21 35 38 43    | 5  6 14  3  5 |  3  -7  -3   4   4   7 |0 2 1 2 1 |2 1 2 1
			#     17 25 28 37 43 44(1) | 8  3  9  6  1 |  7  10   7   2   5   1 |0 1 2 1 2 |1 2 1 2
			#      5 10 13 21 39 43(1) | 5  3  8 18  4 |-12 -15 -15 -16  -4  -1 |1 2 1 1 1 |1 2 1 1 1
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 |  8   6  11   4  -6  -7 |0 2 2 2 0 |2 2 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 | -8  -3  -7   4   1   3 |1 2 1 2 0 |1 2 1 2
					#	unique (1 2 1 2) rebind. ban (2 1 1 1)
			if( all(quoSize[1:4]==c(2,1,1,1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -2  -6  -9  -7  -3 |1 1 2 2 0 |1 1 2 2
			#      5  6 13 16 27 28    | 1  7  3 11  1 |  2  -4 -10  -8  -4 -11 |2 2 2 0 0 |2 2 2
			#      6  7 18 19 30 38(1) | 1 11  1 11  8 |  1   1   5   3   3  10 |2 2 0 2 0 |2 2 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  18  15  19  10   7 |0 1 1 2 2 |1 1 2 2
			#     13 16 24 25 33 36(3) | 3  8  1  8  3 | -3  -9  -9 -13  -7  -9 |0 2 2 2 0 |2 2 2
					#	(2 2 2 0) rebind. ban (2 2 0 2)
			if( all(quoSize[2:5]==c(2,2,0,2)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#      3  6 10 30 34 37    | 3  4 20  4  3 |                        |2 1 0 3 0 |2 1 3
			#      3  4 16 20 28 44(1) | 1 12  4  8 16 |  0  -2   6 -10  -6   7 |2 1 2 0 1 |2 1 2 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  3  14  15  14  10   1 |1 1 0 3 1 |1 1 3 1
			#      2  6  7 12 19 45(2) | 4  1  5  7 26 | -4 -12 -24 -22 -19   0 |3 2 0 0 1 |3 2 1
			#      3 10 13 26 34 38    | 7  3 13  8  4 |  1   4   6  14  15  -7 |1 2 1 2 0 |1 2 1 2
			#     13 14 19 26 40 43(2) | 1  5  7 14  3 | 10   4   6   0   6   5 |0 3 1 0 2 |3 1 2
					#	unique (2 1 2 0) rebind.  ban (3 2 0 0)
			if( all(quoSize[2:5]==c(3,2,0,0)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.
			# if( all(quoSize==c(,,,,)) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	table( sapply( dbgLst,function(str){str}) )
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( 28,37 )","nextColVal_1(  )","nextColVal_5(  )","nextFStepBin( )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 28,37 ) )]

	# unique
	# zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	# allIdxF <- allIdxF[!( zw%in%c( 29 ) )]

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






