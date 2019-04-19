# toZ855_H.R 최종접근
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
			if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 24       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 26,29,31 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 32       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44,39,42 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <17>
			if( fCutU.hasPtn(c(    17,27       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,17,NA,26,27 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c( 20,26          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,NA,NA,28,33 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c( 20,26          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 26,NA,44 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(  2,10,28 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(    24,31 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 24,30,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(          27,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,27,31,NA,38,39 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 26,NA,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 24,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,3        ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,9        ),c( 24       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,9        ),c( 26,29,31 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,2,1        ),c( 32       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,9,2,6,8    ),c( 44,39,42 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4, 5, 6, 3   ) ) cnt<-cnt+1	# (5/3) (5/3) (5/?)
			if( aCStep[2]%in%c( 15, 1, 2      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 6, 4      ) ) cnt<-cnt+1	# unique 4 (3/1/4) & (3/1/?)
			if( aCStep[4]%in%c(  1, 4       ) ) cnt<-cnt+1	# 4/4
			if( aCStep[5]%in%c(  5, 2      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5, 6 )) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  6, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  8, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 4*, 4*, 4, 2, 23 ) ( 4, 4*, 4*, 2, 23 ) 
			#	unique	[1:5]( 5*,6*,1,4,7 ) [1:5]( 5,6*,1*,4,7 )
			#			3>sum[1:4]( 3, 6, 4, 4 )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (2)   3 (3)   4 (8)   5 (4)   6 (3)   8 (2) 

			cnt.w2 <- 0
			if( aCStep[1]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#  5 13 17 29 34 39    | 8  4 12  5  5 |                        |1 2 1 2 0 |1 2 1 2
			# 16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2
			# 14 18 22 26 31 44    | 4  4  4  5 13 | -2  -2  -2  -2  -5   5 |0 2 2 1 1 |2 2 1 1
			# 11 17 28 30 33 35    | 6 11  2  3  2 | -3  -1   6   4   2  -9 |0 2 1 3 0 |2 1 3
			#  2  8 23 26 27 44    | 6 15  3  1 17 | -9  -9  -5  -4  -6   9 |2 0 3 0 1 |2 3 1
			# 20 25 31 32 36 43    | 5  6  1  4  7 | 18  17   8   6   9  -1 |0 0 2 3 1 |2 3 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -5       ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  9      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -7       ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (3)   -5 (2)   -2 (4)   -1 (3)   2 (2)   6 (2)   7 (2)   9 (2) 

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

					# [1]   13
					# [2]   16
					# [3]   34 37 35 26 17
					# [4]   43 31 23
					# [5]   25 21
					# [6]   43 41 44 40 35 40

					if( 1<sum(aZoid==c( 13,16,34,43,25,43 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,37,31,21,41 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,35,23,NA,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,26,NA,NA,40 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,17,NA,NA,35 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(          ) ) score<-score+1
					if( aZoid[2]%in%c(          ) ) score<-score+1
					if( aZoid[3]%in%c(          ) ) score<-score+1	
					if( aZoid[4]%in%c(          ) ) score<-score+1
					if( aZoid[5]%in%c(          ) ) score<-score+1
					if( aZoid[6]%in%c( 41,42    ) ) score<-score+1	
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
					if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 29,28  ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 34     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(        ) ) cnt<-cnt+1

					# if( all(aZoid[c( , )]==c( , )) ) cnt<-cnt+1
					# if( all(aZoid[1:2+ ]==c( , )) ) cnt<-cnt+1
					# if( all(aZoid[c( , )]==c( , )) ) cnt<-cnt+1	# -

					# [  1] 26 33    15 23    15 17    30 33    40 44
					# [  2] 10 12    24 27    37 38    31 32    32 34
					# [  3]  7 15     5 11    14 22    42 45    23 36
					# [  4] 32 37    19 20    34 45    34 42    29 30
					# [  5] 22 25     7 18    17 23    29 42    43 44
					# [  6] 27 28    22 34    22 25    37 41         
					# [  7] 37 43             27 34    14 25         
					# [  8]                   31 42    21 29         
					# [  9]                   32 40    25 33         
					# [ 10]                   17 21                  


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 7       ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 4,6     ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c(         ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 6,8     ),c( 29,28 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 4,2,1   ),c( 34    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 4       ),c(       )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( , ),c( )) ) remCnt <- remCnt+1
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

					score  <- sum(aCStep==c(  7, 8, 2, 3, 4 ),na.rm=T)
					matCnt <- sum(aCStep==c(  2, 3, 1, 1, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  8, 6, 8, 3,13 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5, 1,11, 8, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,11, 6,13, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1,12, 3, 4,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  6,NA, 7,11,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,11, 8,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 8, 8,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  7   2   8   5   3   1   6 
					#	[2]  8   3   6   1  11  12 
					#	[3]  2   1   8  11   6   3   7  11   8   4   7   7   3   6  22 
					#	[4]  3   1   3   8  13   4  11   8   8 
					#	[5]  4   2  13   1   1 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1 # -
						if( fCutU.hasPtn(c(  2, 3 ),aCStep) ) cnt<-cnt+1
						if( 1<sum(aCStep[1:2+2]==c(  7, 8 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  8, 8  ),aCStep) ) cnt<-cnt+1 # -
						if( fCutU.hasPtn(c(  8, 1 ),aCStep) ) cnt<-cnt+1

						#	unique	( 8, 8, 1 ) ( 2, 3, 7 )
						#	unique	[3:4]( 11, 8 ) [3:5]( 7, 8, 2)
						#			[1:5]( 3,(2,1) ,(7,4) ,8 ,13 )
						#	unique	aCStep[2] == (1+aCStep[1]) 
						#			aCStep[5] == (1+aCStep[4])

						if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  3      ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(  3, 4     ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  8, 2, 1  ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  3      ) ) cnt<-cnt+1


						if( 1<sum( aCStep[ 3 ]*c(4,2)==aCStep[c(2,4)] ) )	cnt<-cnt+1
						if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt<-cnt+1
						if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3)]) )	cnt<-cnt+1	# 10 
						if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt<-cnt+1	# 11
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

					# [1]*  12  5  4  7  2  6  4  3 12  1  3 13 14  1 22  2  7  8  6  1  1  6  4  1  3  8
					# [2]*   8  2  1  1  2  6  4  1
					# [3]*   4 18  1  1  1  3  5  8  1  1 21  7  4  2 11  8  2  7  7 11  7  7  5 13 12  6  3 16  9 ...
					# [4]*   5  9 26 11  3 24  1  8  7  8  3  9  2  3  1 17  6  1  1  7  7  6  1 16 17 10 20 11  5 ...
					# [5]    7

					tCnt <- 0
						if( aCStep[1]%in%c(        ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[2]%in%c(  3     ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[3]%in%c(        ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[4]%in%c(        ) ) tCnt<-tCnt+1	# double(  )
						if( aCStep[5]%in%c(        ) ) tCnt<-tCnt+1	# double(  )

						if( 1<sum( aCStep[ 3 ]*c(3,2)==aCStep[c(1,2)] ) )	cnt<-cnt+1
						if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt<-cnt+1
						if( sum(aCStep[c(2,3)])==sum(aCStep[c(4,5)]) )	cnt<-cnt+1	# 12

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					if( fCutU.hasPtn(c(  1, 1 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+1]==c(  8, 4 )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1	# -
					if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1

					#	unique	( 1,1,1 )
					#	unique	3>[1:5]( 3,(5,2) ,5 ,12 , 4)

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
					# [  1]  3  6     3  6     7  3    11  2
					# [  2]  1 11     6  3    10  7     3  5
					# [  3]  2  1     9  1    11  3    21  7
					# [  4] 17  5     6 18     4  3     3  3
					# [  5]  4 18     3  6     4  3     1  5
					# [  6]  5  3     5  2    16  4     9  8
					# [  7] 12  5     2 12     2  8     4  5
					# [  8]  2  5     6 17     7  3     8  2
					# [  9]  9  1     9  5    15  9    13  7
					# [ 10]  6  8    12  3    10  2     1  4
					# [ 11]  5  6     8  9    15  1     1  7
					# [ 12]  6  7     6  1     6  1    19  3


					if( aCStep[1]%in%c(            ) ) cnt<-cnt+1  # double( )
					if( aCStep[2]%in%c(  6, 5      ) ) cnt<-cnt+1  # double( )
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1  # double( )
					if( aCStep[4]%in%c(  3, 2      ) ) cnt<-cnt+1  # double( 3 )
					if( aCStep[5]%in%c(  6         ) ) cnt<-cnt+1  # double( )

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  6, 9 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  8, 5 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+1]==c(  3, 6 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  3,16 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  4, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  5, 1 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  8,10 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c( 12, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  1,12 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+2]==c(  4, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  4, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  4, 5 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  7, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  7, 6 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  7, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  4, 7 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+3]==c(  1, 6 )) ) cnt<-cnt+1

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
					if( aCStep[2]%in%c(  6     ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  4, 3  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  3     ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(        ) ) cnt<-cnt+1

					#        *  *  *     *  *  *     *  *  *
					# [  1] 17  5  2     5  3  7     6  3  6
					# [  2]  6  8  9     4  3  9            
					# [  3]  5  6  1     3  3  3     

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
					if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,2,0)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+1]==c(1,3,0)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[5]%in%c( 31    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,39 ) ) cnt<-cnt+1
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
			if( fCutU.hasPtn(c(       14,NA,33 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c( 12,18,NA,24 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       14,NA,33 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 23,25,32,23,29,44 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(     ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,1 ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,9 ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,0 ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,4 ),c( 31    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,9 ),c( 43,39 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 14 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  6, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1*, 5*, 1,29, 4) ( 6, 2,NA,15 )
			#	unique	[1:5]( 6*, 1*, 17,  4, 11)	[1:3+0]( 8,4,8 ) [1:3+1]( 8,4,8 ) [1:3+2]( 8,4,8 )
			#	unique	3>sum[1:4](18,18,18, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   4 (6)   5 (3)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     10 14 16 18 27 28    | 4  2  2  9  1 |                        |0 4 2 0 0 |4 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -5  -8  -5  -4  -6  13 |2 2 1 0 1 |2 2 1 1
			#      2 10 11 19 35 39(1) | 8  1  8 16  4 | -3   4   0   5  14  -2 |1 3 0 2 0 |1 3 2
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 |  3   1   1  10  -2   5 |1 2 1 1 1 |1 2 1 1 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  2  -3   1 -14   0   1 |2 2 0 1 1 |2 2 1 1
			#     14 18 22 26 31 44    | 4  4  4  5 13 |  7  10   9  11  -2  -1 |0 2 2 1 1 |2 2 1 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( 14     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  2,  9 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 14, 11 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 12,-2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -1, 1 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 11,19,17,12*,-2*,-7)
			#	unique	[]()
			# -------------------------------------------------------------------------------------
			#     FV :    -5 (2)   -3 (2)   -2 (3)   0 (2)   1 (4)   5 (2)   10 (2) 
			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,6)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 9
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,3,6)]) )	cnt.w2<-cnt.w2+1	# 18

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
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+1]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,1,2
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
			if( aZoid[2]%in%c( 25,30 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 39,31 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,16, 9,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 24,25,29,26,32,41 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       33,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       33,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 25,28,33       ),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(       33,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          39,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 25,28,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			# <43>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,3,1 ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,0   ),c( 25,30 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,4   ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,7   ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1     ),c( 39,31 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,9   ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9, 7 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 1 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 3 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3, 1 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 7,15,21)
			#	unique	[3:4]( 5, 7 )	[3:4]( 5, 8 )	# 1 주변패턴
			#	unique	[1:5]( 7, 1, 5, 8, 2 )	# 6/1 5/1  4/8 7/2
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (4)   4 (3)   5 (4)   6 (2)   7 (2)   8 (2)   15 (2) 

			cnt.w2 <- 0
			if( aCStep[1]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 11

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      3  4  9 24 25 33    | 1  5 15  1  8 |                        |3 0 2 1 0 |3 2 1
			#      1  7 22 33 37 40(1) | 6 15 11  4  3 | -2   3  13   9  12   7 |2 0 1 2 1 |2 1 2 1
			#      6  7 19 21 41 43(1) | 1 12  2 20  2 |  5   0  -3 -12   4   3 |2 1 1 0 2 |2 1 1 2
			#     11 30 34 35 42 44    |19  4  1  7  2 |  5  23  15  14   1   1 |0 1 0 3 2 |1 3 2
			#     16 25 33 38 40 45    | 9  8  5  2  5 |  5  -5  -1   3  -2   1 |0 1 1 2 2 |1 1 2 2
			#     20 25 31 32 36 43(1) | 5  6  1  4  7 |  4   0  -2  -6  -4  -2 |0 0 2 3 1 |2 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  5, 3, 4, 1   ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  3      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -3       ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -2       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0,-1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3, 0 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 3*, 0*, -1, 0, -12,-7)	( 3, 0*, -1*, 0, -12,-7)
			# -------------------------------------------------------------------------------------
			#     FV :    -2 (4)   0 (2)   1 (3)   3 (3)   4 (2)   5 (3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c(-1, 1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 3 ]*c(3,2,1)==aFStep[c(4,5,6)] ) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+0]==c(2,2,1)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(2,1,0)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+0]==c(1,0,1)) ) return(FALSE)	# next rebind of 2,2,1
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
			if( aZoid[1]%in%c( 10,43      ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,43,35 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 5          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA, 8,NA,30 ),aZoid) ) cnt<-cnt+1
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3, 8,NA,15,44 ),aZoid) ) cnt<-cnt+1
			# <10>
			# <12>
			if( fCutU.hasPtn(c(  7,NA,12,35 ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,19,44,34    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <21>
			# <22>
			if( fCutU.hasPtn(c( 17,NA,22,23,32,40 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(          30,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,26,19,30       ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(          35,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    17,27,35    ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 34,28,35,40,42 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 14,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    29,36,NA,43 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 16,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,2,3        ),c( 10,43    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,6        ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,1        ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2,5        ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,3        ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,3        ),c( 45,43,35 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6, 9      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  8       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  9, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  9, 9 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  6, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 10, 9 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 20,NA, 7, 5 ) ( 10, 9*, 5*,18) ( 10*, 9*, 5,18) ( 5, 4,15)
			#	unique	[1:3]( 2,10,2 )	# (1,6)/(2,8,2)
			#			[2:4]( 6, 2,24)	[2:5]( 2, 9*, 9*, 5)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (4)   5 (3)   6 (4)   8 (3)   9 (3)   12 (2)   15 (2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      8 16 25 30 42 43    | 8  9  5 12  1 |                        |1 1 1 1 2 |1 1 1 1 2
			#      3 16 22 37 38 44(1) |13  6 15  1  6 | -5   0  -3   7  -4   1 |1 1 1 2 1 |1 1 1 2 1
			#     15 27 33 35 43 45    |12  6  2  8  2 | 12  11  11  -2   5   1 |0 1 1 2 2 |1 1 2 2
			#      1  3 12 21 26 41    | 2  9  9  5 15 |-14 -24 -21 -14 -17  -4 |2 1 2 0 1 |2 1 2 1
			#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   1  -2  -9   2   4 |2 2 1 0 1 |2 2 1 1
			#     10 21 22 30 35 42(1) |11  1  8  5  7 |  9  17  12  18   7  -3 |0 1 2 2 1 |1 2 2 1
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
			if( fCutU.hasPtn(c(  7,-2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( )
			#	unique	[]()
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -14 (2)   -4 (2)   -3 (2)   -2 (2)   0 (2)   1 (3)   7 (2)   11 (2)   12 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-3,-4,-6)==aFStep[c(1,3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,6)]*c(2,-4)==aFStep[c(4,3)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 24

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
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 3,1,1
					if( all(quoSize[1:3+0]==c(3,1,0)) ) return(FALSE)	# next rebind of 0,2,1
					if( all(quoSize[1:3+1]==c(0,3,0)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(0,3,0)) ) return(FALSE)	# next rebind of 1,1,2
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
			if( aZoid[1]%in%c(  9          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 22,18,15    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 17          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28,21,17    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44,38,34,21 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,13,16 ),aZoid) ) cnt<-cnt+1
			# <10>
			# <15>
			if( fCutU.hasPtn(c( 15,17          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 15,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    17,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,41 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 22,24    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    24,27 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c(    24,27 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,NA,27 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(       21,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,34    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       34,41 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c( 15,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       34,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c(       22,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,32,NA,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 9        ),c(  9          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4        ),c(             )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,8,1,5  ),c( 22,18,15    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,2,1,7  ),c( 17          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,1,7    ),c( 28,21,17    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,8,1    ),c( 44,38,34,21 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 5,17 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4, 2    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 13, 4    ) ) cnt<-cnt+1	# unique 13  3/7/13 3/7/?

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 13, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  3,13 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3,13 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 13, 7 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  9, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  4, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4,14 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 4*, 2*, 6,10 ) ( 6*, 3*, 3,22) ( 6, 3*, 3*,22) ( 4, 4, 3 ) ( 4*, 1*,23,10)
			#			( 2, 4,13*, 7*) ( 2, 4*,13*, 7)
			#	unique	[1:5]( 9*, 4*, 3, 3, 4 )	[1:4]( 2,13*, 7*, 3) [1:4]( 3, 3*,13*, 7 )
			#			[2:5]( 5, 3, 3*,13*)
			#			3>sum[1:5](NA, 7, 7, 3,13)
			# -------------------------------------------------------------------------------------
			#     FV :    2 (4)   3 (7)   4 (6)   7 (2)   13 (2)   14 (2) 

			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      7  9 10 12 26 38    | 2  1  2 14 12 |                        |2 2 1 1 0 |2 2 1 1
			#      7 11 13 33 37 43(1) | 4  2 20  4  6 |  0   2   3  21  11   5 |1 2 0 2 1 |1 2 2 1
			#      3  6 20 24 27 44    | 3 14  4  3 17 | -4  -5   7  -9 -10   1 |2 0 3 0 1 |2 3 1
			#      8 17 21 24 27 31(2) | 9  4  3  3  4 |  5  11   1   0   0 -13 |1 1 3 1 0 |1 1 3 1
			#     15 19 21 34 41 44(1) | 4  2 13  7  3 |  7   2   0  10  14  13 |0 2 1 1 2 |2 1 1 2
			#     10 15 18 21 34 41(4) | 5  3  3 13  7 | -5  -4  -3 -13  -7  -3 |0 3 1 1 1 |3 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3, 0 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 3, 3, 0 )
			#	unique	(-4,-5) ( 0, 2 ) ( 11, 5 )
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (2)   -5 (2)   -4 (2)   -3 (2)   0 (4)   1 (2)   2 (2)   5 (2)   7 (2)   11 (2) 
			cnt.w2 <- 0
			if( aFStep[4]==sum(aFStep[c(3,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,3,6]) )	cnt.w2<-cnt.w2+1	# -11

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
					if( (aZoid[6]-aZoid[1]) %in% c( 44 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,2,1
					#	unique (2 1 2 0 1) 2step rebind. (0 1 2 2 1) again?
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
			if( aZoid[1]%in%c(  1,10     ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 12      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 28      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,43      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 5          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA, 8, 3,30 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,NA,NA,28    ),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,15 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,12    ),aZoid) ) cnt<-cnt+1
			# <10>
			# <12>
			if( fCutU.hasPtn(c(  6,NA,12    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       12,30 ),aZoid) ) cnt<-cnt+1
			# <21>
			# <22>
			if( fCutU.hasPtn(c( 14,NA,22,32,25 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(  1,NA,NA,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             28,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     2, 3,NA,28    ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(  8, 5,14,41 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(             28,45 ),aZoid) ) cnt<-cnt+1

			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,0        ),c(  1,10 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 1,4        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,0,7,9        ),c( 12    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2,4,3        ),c( 28    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,4,3        ),c( 28    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,3,6        ),c( 45,43 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1, 6    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5, 6, 3 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 17       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(  2, 9*, 9*, 5,15 ) (20,NA, 7, 5)
			#	unique	[2:4]( 9,19,15)	# (1,3,4)/(9,5,15)
			#			[1:3]( 1, 7, 4)		# (6,2,16)/(1,8,5)
			#			[2:3]( 1,7 )	# (5,NA)/(16,17)/( 5, 7)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (2)   3 (2)   4 (2)   5 (4)   6 (3)   9 (2)   11 (3)   17 (3) 

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      1  6 17 22 28 45    | 5 11  5  6 17 |                        |2 1 2 0 1 |2 1 2 1
			#      6 16 37 38 41 45(2) |10 21  1  3  4 |  5  10  20  16  13   0 |1 1 0 2 2 |1 1 2 2
			#      1  3 12 21 26 41(1) | 2  9  9  5 15 | -5 -13 -25 -17 -15  -4 |2 1 2 0 1 |2 1 2 1
			#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   1  -2  -9   2   4 |2 2 1 0 1 |2 2 1 1
			#     10 21 22 30 35 42(1) |11  1  8  5  7 |  9  17  12  18   7  -3 |0 1 2 2 1 |1 2 2 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -5 -10 -10  -1  -2   2 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -1     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -2,-13 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -5 (2)   -2 (2)   0 (2)   2 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(2,2)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(2,5)]*c( 1,-1)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 6 ]*c(-5,-5,-1)==aFStep[c(2,3,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# -3

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,1,3
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
			if( aZoid[2]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 25    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 28    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41,44 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,17,30,21 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c(  6,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,17,30,21 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c(       20,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,16,20,22,2     ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <24>
			# <27>
			if( fCutU.hasPtn(c( 27,30 ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 12,NA,32 ),aZoid) ) cnt<-cnt+1
			# <35>
			# <40>
			if( fCutU.hasPtn(c(       20,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,35,NA,39,NA,40 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,4        ),c( 24    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2        ),c( 12    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1        ),c( 25    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,6,8        ),c( 28    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6,4,1        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4        ),c( 41,44 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3, 2 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  9    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 7 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  8, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 8, 1, 2 ) (19, 1,11) (17, 6,NA, 1*, 5*)
			#	unique	[3:5](13, 8, 1)	[1:3](2,2,2) [2:4](1,1,1) [3:5](8,8,8)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (2)   3 (2)   5 (2)   7 (4)   8 (2)   9 (2)   10 (2)   11 (2)   12 (2)
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(10, 4, 3)==aCStep[c(1,4,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     13 14 15 26 35 39    | 1  1 11  9  4 |                        |0 3 1 2 0 |3 1 2
			#      1 10 20 32 35 40(1) | 9 10 12  3  5 |-12  -4   5   6   0   1 |1 1 1 2 1 |1 1 1 2 1
			#      3  6 13 23 24 35(1) | 3  7 10  1 11 |  2  -4  -7  -9 -11  -5 |2 1 2 1 0 |2 1 2 1
			#      6 13 20 27 28 40(2) | 7  7  7  1 12 |  3   7   7   4   4   5 |1 1 3 0 1 |1 1 3 1
			#     11 24 32 33 35 40(1) |13  8  1  2  5 |  5  11  12   6   7   0 |0 1 1 3 1 |1 1 3 1
			#      7 27 29 30 38 44    |20  2  1  8  6 | -4   3  -3  -3   3   4 |1 0 2 2 1 |1 2 2 1

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
			if( fCutU.hasPtn(c(  2, 4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5, 7 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 3,-13,-13, 2*, 4*) ( 5, 7,-4)
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (3)   -3 (2)   0 (2)   3 (3)   4 (3)   5 (3)   6 (2)   7 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2,3)]*c(-1, 1, 1)==aFStep[c(6,5,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(2,4)]*c(-1,-1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1

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
			if( aZoid[2]%in%c( 33    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  8    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 36    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 8>
			# <30>
			if( fCutU.hasPtn(c(       13,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    18,NA,30,33 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c( 33 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,8        ),c(  8 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,5        ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6        ),c( 36 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 25      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2, 3, 1) ( 6,25, 2*, 2*, 4 )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(3,4)]*c(3,2)==aCStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[ 3 ]*c(3,1)==aCStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[ 4 ]*c(1,2)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      5  7  8 15 30 43    | 2  1  7 15 13 |                        |3 1 0 1 1 |3 1 1 1
			#      8 13 14 30 38 39(2) | 5  1 16  8  1 |  3   6   6  15   8  -4 |1 2 0 3 0 |1 2 3
			#      2  8 33 35 37 41(1) | 6 25  2  2  4 | -6  -5  19   5  -1   2 |2 0 0 3 1 |2 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(   4    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(  3,  6*,  6*, 15,  8, -4 ) ()
			# -------------------------------------------------------------------------------------
			#     FV :    6 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(6,4)]*c(-3,-1)==aFStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,2,5,3)]) )	cnt.w2<-cnt.w2+1	# 7

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
					if( (aZoid[6]-aZoid[1]) %in% c( 29, ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,2,1)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 2,2,1
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
			if( aZoid[1]%in%c(  8, 9 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 37,44 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41,38 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <10>
			if( fCutU.hasPtn(c( 10,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,31,32,42 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			# <18>
			# <29>
			if( fCutU.hasPtn(c(    29,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,29,34,NA,NA,37 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 24,15,30,33,45 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 13,NA,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       33,37 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(          35,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    17,27,35    ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(    29,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,34,29,39 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 10,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    31,32,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  1,24,29,31,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 9,1        ),c(  8, 9 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,7        ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 7,4        ),c( 37,44 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,8        ),c( 41,38 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 10      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1, 3   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 16      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3      ) ) cnt<-cnt+1	# (6,2)/6  (3,2)/?

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 11, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 12, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  8, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 15, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1, 6, 11) (15, 3, 3) ( 1, 6*, 6*,18) ( 11*, 1*, 9, 8,10)
			#	unique	[1:4](1,  8*,  5*,  7) [1:2](12*, 6*, 2, 8, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (4)   3 (3)   5 (2)   6 (4)   7 (2)   8 (2)   11 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     18 29 30 37 39 43    |11  1  7  2  4 |                        |0 1 1 3 1 |1 1 3 1
			#     12 29 32 33 39 40(2) |17  3  1  6  1 | -6   0   2  -4   0  -3 |0 1 1 3 1 |1 1 3 1
			#     15 27 33 35 43 45(1) |12  6  2  8  2 |  3  -2   1   2   4   5 |0 1 1 2 2 |1 1 2 2
			#     10 11 12 18 24 42    | 1  1  6  6 18 | -5 -16 -21 -17 -19  -3 |0 4 1 0 1 |4 1 1
			#     10 21 22 30 35 42(2) |11  1  8  5  7 |  0  10  10  12  11   0 |0 1 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38    | 5  3 16  3  2 | -1  -7  -5   3   1  -4 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -2    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   2    ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0,-4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-4, 6, 0*,-4*) (  2, 2*, 2*, 12, 10)
			# -------------------------------------------------------------------------------------
			#     FV :    -5 (2)   -4 (2)   -3 (2)   0 (4)   1 (2)   2 (2)   3 (2)   10 (2) 

			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,3,6,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# -4
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,6,5)]) )	cnt.w2<-cnt.w2+1	# -8

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
					if( (aZoid[6]-aZoid[1]) %in% c( 32,33,34 ) ) return( FALSE )
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
			if( aZoid[1]%in%c(  4          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 15          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17,14, 6    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 17,23,19,21 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 16          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 36          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,15          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,13,NA,40 ),aZoid) ) cnt<-cnt+1
			# < 6>
			# <14>
			if( fCutU.hasPtn(c(    14,17       ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(  5,NA,15          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    15,18       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,15,NA,29,42 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,19    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,NA,36 ),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c(    14,17       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,17,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			# <23>
			# <36>
			if( fCutU.hasPtn(c( 16,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,11,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 17,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,7        ),c(  4          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5        ),c( 15          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,4,6        ),c( 17,14, 6    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,3,9,1        ),c( 17,23,19,21 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,6        ),c( 16          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6,7,4        ),c( 36          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  8, 7      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4,10      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 8, 3   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1       ) ) cnt<-cnt+1	# 8, 4, 2, ?
			if( aCStep[5]%in%c(  3       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  8, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 8 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 8*, 4*,10,15) (10, 3, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (2)   4 (5)   5 (2)   6 (2)   8 (4)   10 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(2,1)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      5 13 17 23 28 36    | 8  4  6  5  8 |                        |1 2 2 1 0 |1 2 2 1
			#      1  4  8 23 33 42(1) | 3  4 15 10  9 | -4  -9  -9   0   5   6 |3 0 1 1 1 |3 1 1 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 13  11   8  -6   5   3 |0 4 0 1 1 |4 1 1
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
			#      6 11 15 17 23 40    | 5  4  2  6 17 |  4   1   1  -5  -9   4 |1 3 1 0 1 |1 3 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 | -1  -5   1   1  14  -2 |2 2 0 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( 11     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1,-2      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -4, 1,-12      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  1,-5      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  5      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  1, 1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+2]==c( -5, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -4,-9 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 4,-4,-4*,-9*) (-14, 1*, 1*,NA, 5)
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (4)   -6 (2)   -5 (3)   -2 (2)   1 (4)   4 (2)   5 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(-1,-1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1	# -3

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
					if( (aZoid[6]-aZoid[1]) %in% c( 34,29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[1]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 16          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 21          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 36,42,40,24 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,14,12,25,22 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			# <17>
			if( fCutU.hasPtn(c( 17,23 ),aZoid) ) cnt<-cnt+1
			# <19>
			# <22>
			if( fCutU.hasPtn(c( 21,22,40,41 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <23>
			if( fCutU.hasPtn(c( 17,23 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(  4,13,30,36 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 8        ),c(             )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2        ),c( 12          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6,5,0        ),c( 16          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1        ),c( 21          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,3        ),c(             )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6,2,0,4,1  ),c( 36,42,40,24 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(   8, 1  ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(   4, 2  ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(   3     ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(   8     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  8, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 3, 5, 2 ) ( 2, 5,28) ( 8*, 4*,10,15 ) ( 1, 7, 5 ) (10, 3, 8) ( 7,12,10*, 5*)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (4)   3 (3)   4 (5)   5 (2)   6 (2)   7 (2)   8 (4)   10 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,3)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,2,3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      1  2  6 16 19 42    | 1  4 10  3 23 |                        |3 2 0 0 1 |3 2 1
			#      9 12 19 20 39 41(1) | 3  7  1 19  2 |  8  10  13   4  20  -1 |1 2 1 1 1 |1 2 1 1 1
			#      5 13 17 23 28 36    | 8  4  6  5  8 | -4   1  -2   3 -11  -5 |1 2 2 1 0 |1 2 2 1
			#      7 22 24 31 34 36(1) |15  2  7  3  2 |  2   9   7   8   6   0 |1 0 2 3 0 |1 2 3
			#      2 10 14 22 32 36(2) | 8  4  8 10  4 | -5 -12 -10  -9  -2   0 |1 2 1 2 0 |1 2 1 2
			#      6 11 15 17 23 40    | 5  4  2  6 17 |  4   1   1  -5  -9   4 |1 3 1 0 1 |1 3 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -6, -9   ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   4, -9,  7 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -5 (3)   -2 (2)   0 (2)   1 (3)   4 (3)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(1,1)==aFStep[c(6,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(2,6,5)]) )	cnt.w2<-cnt.w2+1	# -4
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(2,1,5)]) )	cnt.w2<-cnt.w2+1	# -4

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
	# 	rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 41 ) ) return( FALSE )
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
			if( aZoid[1]%in%c(  1, 2 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,31 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# < 3>
			if( fCutU.hasPtn(c(  3, 6, 8, 8 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			# <14>
			# <18>
			if( fCutU.hasPtn(c( 13,18    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    18,22 ),aZoid) ) cnt<-cnt+1
			# <22>
			if( fCutU.hasPtn(c(    18,22 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,22 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c( 15,NA,35,NA,39 ),aZoid) ) cnt<-cnt+1
			# <41>
			# <43>
			# <44>
			if( fCutU.hasPtn(c(  8, 9,11,19,44 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,5,2,4 ),c(  1, 2 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,2,6   ),c(  8    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,4,6   ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,0,1   ),c( 42,31 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6,27    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 6, 4 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2, 4    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  6, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  1, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  1, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 4 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 6, 2, 1 ) ( 7, 5, 4 )
			#	unique	[1:3]( 1  3  4 ) ( 6, 2, 1*, 1*, 6 )
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (7)   4 (6)   6 (5)   27 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(4,1,2)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(4,1,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(4,2,3)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      1 28 35 41 43 44    |27  7  6  2  1 |                        |1 0 1 1 3 |1 1 1 3
			#      2  8 33 35 37 41(2) | 6 25  2  2  4 |  1 -20  -2  -6  -6  -3 |2 0 0 3 1 |2 3 1
			#      4 10 14 15 18 22    | 6  4  1  3  4 |  2   2 -19 -20 -19 -19 |1 4 1 0 0 |1 4 1
			#      1  3 12 14 16 43(1) | 2  9  2  2 27 | -3  -7  -2  -1  -2  21 |2 3 0 0 1 |2 3 1
			#      3  9 11 12 13 19(2) | 6  2  1  1  6 |  2   6  -1  -2  -3 -24 |2 4 0 0 0 |2 4
			#     14 18 22 26 31 44    | 4  4  4  5 13 | 11   9  11  14  18  25 |0 2 2 1 1 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2, 9  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( 11     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( 18     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -1,-2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+2]==c( 14,18 )) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#	unique	[1:5]( 9, 11, 14*, 18*, 25)
			# -------------------------------------------------------------------------------------
			#     FV :    -20 (2)   -19 (3)   -6 (2)   -3 (3)   -2 (4)   -1 (2)   2 (3)   11 (2)
			cnt.w2 <- 0
			if( aFStep[6]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+0]==c(0,2,3)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(2,3,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(3,1,0)) ) return(FALSE)	# next rebind of 1,1,1
					#	unique (1 2 1 1 1) rebind. (0 2 3 1 0) again?
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
			if( aZoid[2]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 15    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 28,21 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c( 11,NA,26,31,34 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <14>
			# <28>
			if( fCutU.hasPtn(c(  4, 5,NA,28 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 10,NA,30       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,30,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 30,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 13,18,NA,NA,38 ),aZoid) ) cnt<-cnt+1
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
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2       ),c( 12    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5       ),c( 15    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8,1     ),c( 28,21 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 7,2     ),c( 12    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 3   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3,12   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 17      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 1 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  4,15 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 15,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 4,15,11)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (3)   3 (5)   6 (2)   7 (2)   9 (2)   10 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[4]*c(2,1)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#      8 11 19 21 36 45    | 3  8  2 15  9 |                        |1 2 1 1 1 |1 2 1 1 1
			#     11 30 34 35 42 44(1) |19  4  1  7  2 |  3  19  15  14   6  -1 |0 1 0 3 2 |1 3 2
			#     14 15 25 28 29 30(1) | 1 10  3  1  1 |  3 -15  -9  -7 -13 -14 |0 2 3 1 0 |2 3 1
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 | -2   3   5  11  12  12 |0 2 0 2 2 |2 2 2
			#      9 10 13 28 38 45    | 1  3 15 10  7 | -3  -8 -17 -11  -3   3 |1 2 1 1 1 |1 2 1 1 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 | -4   1   1   2  -5  -7 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(   -5   ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -4, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -3 (2)   1 (2)   3 (4)   12 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(1,2)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(3,4)]*c( 1,-2)==aFStep[c(2,1)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# -3
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# -3
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,4,6)]) )	cnt.w2<-cnt.w2+1	# -4

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

#	static과 custom 분리필요
fltCntMtx		<- function( ccObjLst ,allIdxF ){
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

	allIdxF.len <- length(allIdxF)
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)
	for( aIdx in seq_len(allIdxF.len) ){

		# # -[rawMtx]-----------------------------------------------------------
			fndIdx <- which(rawMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw001" )
			fndIdx <- which(rawMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 2< length(fndIdx) )	fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw011" )
				if( 2==length(fndIdx) ){
					# if( all(c("nextQuo10","nextColVal_6")==names(fndIdx)) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
					fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
				}
				if( 1==length(fndIdx) ){
					banCol <- c("nextRebNum")
					if( banCol %in% names(fndIdx) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw013" )
				}
			}
			fndIdx <- which(rawMtx[aIdx,]==1)
			if( 5<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw021" )
		# # -[remMtx]-----------------------------------------------------------
			fndIdx <- which(remMtx[aIdx,]>3)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem001" )
				if( any(names(fndIdx)%in%c("nextColVal_2"))  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem002" )
			}
			fndIdx <- which(remMtx[aIdx,]==3)
			if(0<length(fndIdx)){
				if( 2<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem011" )
				if( 1==length(fndIdx) ){
					banCol <- c("nextColVal_3","nextQuo10","nextColVal_6")
					if( names(fndIdx) %in% banCol ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem012" )
				}
			}
			fndIdx <- which(remMtx[aIdx,]==2)
			if( 4<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem021" )
			fndIdx <- which(remMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem031" )
			fndIdx <- which( 0<remMtx[aIdx,] & remMtx[aIdx,]<3 )
			if( 10<length(fndIdx) || length(fndIdx)<4 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem041" )
		# # -[cStepMtx]-----------------------------------------------------------
			fndIdx <- which(cStepMtx[aIdx,]>2)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep001" )
				banCol <- c("nextColVal_4")
				if( any(names(fndIdx)%in%banCol)  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep002" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 3<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep011" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep021" )
		# # -[fStepMtx]-----------------------------------------------------------
			fndIdx <- which(fStepMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep001" )
			fndIdx <- which(fStepMtx[aIdx,]==2)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep011" )
			fndIdx <- which(fStepMtx[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep021" )

		# # -[cStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:001" )
			fndIdx <- which(cStepMtx.w1[aIdx,]==1)
			if(4<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:011" )
		# # -[cStepMtx.w2]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w2[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:001" )
			fndIdx <- which(cStepMtx.w2[aIdx,]==1)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:011" )
			if( 1==length(fndIdx) ) {
				banCol <- c("nextRebNum","nextQuo10","nextFStepBin","nextColVal_1","nextColVal_6")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:021" )
			}

		# # -[fStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(2<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextRebNum","basic","nextColVal_5")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:021" )
			}
		# # -[fStepMtx.w2]-----------------------------------------------------------
			# fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			# if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextZW","nextColVal_2","nextQuo10","nextFStepBin")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:021" )
			}


		# # -[raw.w1.w2]-----------------------------------------------------------
			cntVal <- rawMtx[aIdx,] + cStepMtx.w1[aIdx,] + cStepMtx.w2[aIdx,] + fStepMtx.w1[aIdx,] + fStepMtx.w2[aIdx,]
			if(0==sum(cntVal)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw.w1.w2:000" )
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="fStep021")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	# fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( list( allIdxF=allIdxF ,fltCnt=fltCnt ) )

} # fltCntMtx()

#	static과 custom 분리필요
fltScoreMtx.static		<- function( ccObjLst ,allIdxF ){
	allIdxF.len <- length( allIdxF )
	scoreMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx })
	phName <- attributes(scoreMtxLst)$names
	colName <- colnames(scoreMtxLst[[1]])

	scoreMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx) <- phName	;colnames(scoreMtx)=colName
	scoreMtx.evt <- scoreMtx

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx[,] <- 0		;scoreMtx.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx[phIdx,] <- scoreMtxLst[[phIdx]][aIdx,]
			#	scoreMtx.evt[phIdx,] <- scoreMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3 w1CStep.cnt w1FStep.cnt w1CStep.matLen w1FStep.matLen
		}

		evtCnt <- apply( scoreMtx.evt ,1 ,sum )

		colSum <- apply( scoreMtx ,2 ,sum )
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 0==sum(0<colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 4<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }
		workCol <- c("nbor","spanM","quoAll","quoPtn","zw")
		if( 3<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01.a") }
		workCol <- c("remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 3<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01.b") }


		if( 4<colSum["nbor"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"nbor.00") }
		if( 4<colSum["spanM"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"nbor.00") }
		if( 5<colSum["quoAll"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"quoAll.00") }
		if( 3<colSum["quoPtn"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"quoPtn.00") }
		if( 2<colSum["zw"]		){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"zw.00") }
		if( 1<colSum["remH0"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remH0.00") }
		if( 1<colSum["remH1"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remH1.00") }
		if( 1<sum(colSum[c("remH0","remH1")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"remHn.00") }
		if( 2<colSum["cStep2"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"cStep2.00") }
		if( 1<colSum["cStep3"]	){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"cStep3.00") }
		if( 2<colSum["w1CStep.cnt"] ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"w1CStep.cnt.00") }
		if( 4<colSum["w1FStep.cnt"] ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"w1FStep.cnt.00") }

		ceilThld <- c( 4,4, 4,3, 2, 1,1, 2,1, 2,4 )
		names(ceilThld) <- c("nbor","spanM","quoAll","quoPtn","zw","remH0","remH1","cStep2","cStep3","w1CStep.cnt","w1FStep.cnt")
		if( 3<sum(ceilThld<=colSum[names(ceilThld)]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"ceilThld.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="remHn.00")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx.static()

#	static과 custom 분리필요
fltScoreMtx2.static	<- function( ccObjLst ,allIdxF ){
	
	allIdxF.len <- length( allIdxF )
	scoreMtx2Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx2 })
	phName <- attributes(scoreMtx2Lst)$names
	colName <- colnames(scoreMtx2Lst[[1]])

	scoreMtx2 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx2) <- phName	;colnames(scoreMtx2)=colName
	scoreMtx2.evt <- scoreMtx2

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx2[,] <- 0		;scoreMtx2.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx2[phIdx,] <- scoreMtx2Lst[[phIdx]][aIdx,]
			scoreMtx2.evt[phIdx,] <- scoreMtx2[phIdx,] >= c( 3, 2, 2, 1, 1, 100,100, 2, 2 )	# rebL.cnt,rebR.cnt은 100으로
			#	"rebV" "rebC" "rebC2" "rebL" "rebR" "rebL.cnt" "rebR.cnt" "inc.raw" "inc.cStep"
		}

		evtCnt <- apply( scoreMtx2.evt ,1 ,sum )
		if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		colSum <- apply( scoreMtx2 ,2 ,sum )
		workCol <- c("inc.raw", "inc.cStep")
		if( 1<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx2 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("rebC","rebC2","inc.raw", "inc.cStep")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

		colSum <- apply( scoreMtx2 ,2 ,function(cv){any(cv>1)} )
		evtCnt <- rep( 0, 6 )
			evtCnt[1] <- sum(scoreMtx2[,"rebV"]>1)
			if( any(scoreMtx2[,"rebV"]>=3) ) evtCnt[1] <- 100
			evtCnt[2] <- sum(scoreMtx2[,"rebC"]>1)
			evtCnt[3] <- sum(scoreMtx2[,"rebC2"]>1)
			evtCnt[4] <- sum(scoreMtx2[,c("rebL","rebR")]>0)
			evtCnt[5] <- sum(scoreMtx2[,"inc.raw"]>1)
			evtCnt[6] <- sum(scoreMtx2[,"inc.cStep"]>1)
		if( 3<sum(evtCnt>=c(3,1,1,1,1,1)) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01") }
		if( 0==sum(evtCnt>=c(3,1,1,1,1,1)) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.a") }


		# # -[rebV]--------------------------------------------------------------
		if( 7>sum(scoreMtx2[,"rebV"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0030") }
		flg <- scoreMtx2[,"rebV"] >= 3
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0040") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0050") }
		flg <- scoreMtx2[,"rebV"] == 2
			if( 5 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0060") }

		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070") }
		flg <- scoreMtx2[,"rebC"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin") # 검토필요.
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090") }
		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC2"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070.2") }
		flg <- scoreMtx2[,"rebC2"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080.2") }
			# banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			# if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090.2") }

		# # -[rebL]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebL"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0100") }

		# # -[rebR]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebR"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0110") }

		# # -[inc.raw]-----------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.raw"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0120") }
		flg <- scoreMtx2[,"inc.raw"] >= 2
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0130") }

		# # -[inc.cStep]---------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.cStep"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0140") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx2.static()

fltScoreMtx3.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx3Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx3 })
	phName <- attributes(scoreMtx3Lst)$names
	colName <- colnames(scoreMtx3Lst[[1]])

	scoreMtx3 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx3) <- phName	;colnames(scoreMtx3)=colName
	scoreMtx3.evt <- scoreMtx3

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx3[,] <- 0		;scoreMtx3.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx3[phIdx,] <- scoreMtx3Lst[[phIdx]][aIdx,]
			scoreMtx3.evt[phIdx,] <- scoreMtx3[phIdx,] >= c( 1, 1, 2, 2, 2, 2 )	# 
			#	rebPtn.1 rebPtn.n rebC.C1 rebC.F1 rebC.C2 rebC.F2
		}

		evtCnt <- apply( scoreMtx3.evt ,1 ,sum )
		# if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		# if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		colSum <- apply( scoreMtx3 ,2 ,sum )
		workCol <- c("rebPtn.1","rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){sum(cv==1)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if(  6>sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01") }
		if(  2>sum(colSum[c("rebC.C1","rebC.F1")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.a") }
		if(  2>sum(colSum[c("rebC.C2","rebC.F2")]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.01.b") }
		if( 17<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.02") }

		colSum <- apply( scoreMtx3 ,2 ,function(cv){sum(cv>=2)} )
		workCol <- c("rebC.C1","rebC.F1","rebC.C2","rebC.F2")
		if(  3<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.10") }

		# -[rebPtn.1]--------------------------------------------------------------
		if( 2<sum(scoreMtx3[,"rebPtn.1"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebPtn.1.00") }

		# # -[rebC.X1 : rebC.C1, rebC.F1]--------------------------------------------------------------
		workCol <- c("rebC.C1","rebC.F1")
		if( 2>sum(scoreMtx3[,"rebC.C1"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebC.X1.00") }

		# # -[rebC.X2 : rebC.C2, rebC.F2]--------------------------------------------------------------
		workCol <- c("rebC.C2","rebC.F2")
		if( 2>sum(scoreMtx3[,"rebC.C2"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"rebC.X2.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx3.static()

fltScoreMtx4.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx4Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx4 })
	phName <- attributes(scoreMtx4Lst)$names
	colName <- colnames(scoreMtx4Lst[[1]])

	scoreMtx4 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx4) <- phName	;colnames(scoreMtx4)=colName
	scoreMtx4.evt <- scoreMtx4

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx4[,] <- 0		;scoreMtx4.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx4[phIdx,] <- scoreMtx4Lst[[phIdx]][aIdx,]
			scoreMtx4.evt[phIdx,] <- scoreMtx4[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	incRaw3     incC3     incF3   incRaw2     incC2     incF2     (1,6) nextVal.r nextVal.c nextVal.f 
		}

		evtCnt <- apply( scoreMtx4.evt ,1 ,sum )

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incRaw2","incC2","incF2","nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }

		colSum <- apply( scoreMtx4 ,2 ,sum )
		workCol <- c("incRaw2","incC2","incF2","(1,6)","nextVal.r","nextVal.c","nextVal.f")
		if( 1>sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01a") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){any(cv>1)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if( 2<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.10") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){sum(cv>=2)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if( 3<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.gt") }

		colSum <- apply( scoreMtx4 ,2 ,function(cv){sum(cv==1)} )
		workCol <- c("nextVal.r","nextVal.c","nextVal.f")
		if(  4>sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.lt") }
		if( 17<sum(colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.11.gt") }


		# # -[incX2 : incRaw2 ,incC2 ,incF2]--------------------------------------------------------------
		workCol <- c("incRaw2","incC2","incF2")
		if( 1>sum(scoreMtx4[,workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"incX2.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx4.static()

fltCStepValMtx.static	<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	cStepValMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$cStepValMtx })
	phName <- attributes(cStepValMtxLst)$names
	colName <- colnames(cStepValMtxLst[[1]])

	cStepValMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(cStepValMtx) <- phName	;colnames(cStepValMtx)=colName
	cStepValMtx.evt <- cStepValMtx

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		cStepValMtx[,] <- 0		;cStepValMtx.evt[,] <- 0
		for( phIdx in phName ){
			cStepValMtx[phIdx,] <- cStepValMtxLst[[phIdx]][aIdx,]
			# cStepValMtx.evt[phIdx,] <- cStepValMtx[phIdx,] >= c( 1,1,1,  1,1,1,  1, 2,2,2 )	# 
			#	c31 c32 c33 c34 	c21 c22 c23 c24 c25 	max2 min2
		}

		evtCnt <- apply( cStepValMtx.evt ,1 ,sum )

		colSum <- apply( cStepValMtx ,2 ,sum )
		workCol <- c("c21","c22","c23","c24","c25")
		# if( 2<sum(0==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.00") }

		colSum <- apply( cStepValMtx ,2 ,sum )
		workCol <- c("c21","c22","c23","c24","c25")
		if( 2<sum(1==colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"voidCol.01") }

		colSum <- apply( cStepValMtx ,2 ,function(cv){any(cv>1)} )
		workCol <- c("c21","c22","c23","c24","c25")
		if( 3<sum(0<colSum[workCol]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"evtCol.00") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="voidCol.01")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltCStepValMtx.static()


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

		# -	a,b,b.. a?	nextColVal_1 nextColVal_2 nextColVal_5
			if( 2<sum(aZoid[c( 6,6,1,2 )]==c( 36,42, 2,10 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aZoid[c( 6,6,1,2 )]==c( 38,45,12,18 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	a,a,b.. b?	nextBin nextColVal_2
			if( 2<sum(aZoid[c( 1,2 )]==c( 15,16 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	q,a,b,a.. q?	basic nextZW nextColVal_3
			if( 2<sum(aZoid[c( 4,6,2,1,4 )]==c( 28,39, 9, 3,20 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. b?
			if( 2<sum(aZoid[c( 4,6,2,1,4 )]==c( 30,35,10, 2,12 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. a?
			if( 2<sum(aZoid[c( 4,6,2,1,4 )]==c( 26,44,30, 8,25 )) ){	surviveFlg[idx]<-FALSE	;next }
		# -	a,a+1,b,a+2?	nextQuo10 nextBin nextFStepBin nextColVal_2
			if( 2<sum(aZoid[c( 5,2,6,3 )]==c( 43,12,38,14 )) ){	surviveFlg[idx]<-FALSE	;next }

		#	bin pattern
		if( all(aBin[ -4]==c( 0,0,1,1,0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextRebNum
		if( all(aBin[ -4]==c( 0,1,0,0,1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextRebNum symm
		if( all(aBin[ -5]==c( 0,1,0,0,1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextFStepBin
		if( all(aBin[ -5]==c( 0,0,0,0,1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextFStepBin symm
		if( all(aBin[ -4]==c( 1,0,1,0,1 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_2
		if( all(aBin[ -4]==c( 0,0,0,1,0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_2	symm
		if( all(aBin[1:6]==c( 0,0,0,0,1,0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_5

		# fCutCnt.basic()
			#      1  2 16 22 38 39    | 1 14  6 16  1 |                        |2 1 1 2 0 |2 1 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 |  4  11   1   7  -4   0 |1 2 1 2 0 |1 2 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2
			#     14 18 22 26 31 44    | 4  4  4  5 13 | -2  -2  -2  -2  -5   5 |0 2 2 1 1 |2 2 1 1
			#     11 17 28 30 33 35    | 6 11  2  3  2 | -3  -1   6   4   2  -9 |0 2 1 3 0 |2 1 3
			#      2  8 23 26 27 44    | 6 15  3  1 17 | -9  -9  -5  -4  -6   9 |2 0 3 0 1 |2 3 1
			if( 1<sum( aZoid[ 1 ]*c(4,13,22)==aZoid[c(2,4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(4,6)]==c(28,29)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(4,6)]==c(30,35)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(4,6)]==c(26,44)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c(16,39)) ){	surviveFlg[idx]<-FALSE	;next }	# zw

		# fCutCnt.nextZW
			#      2 10 16 19 34 45    | 8  6  3 15 11 |                        |1 3 0 1 1 |1 3 1 1
			#      2  6 13 16 29 30(2) | 4  7  3 13  1 |  0  -4  -3  -3  -5 -15 |2 2 1 1 0 |2 2 1 1
			#      7  9 12 14 23 28    | 2  3  2  9  5 |  5   3  -1  -2  -6  -2 |2 2 2 0 0 |2 2 2
			#      9 30 34 35 39 41(1) |21  4  1  4  2 |  2  21  22  21  16  13 |1 0 0 4 1 |1 4 1
			#      6 10 17 18 21 29    | 4  7  1  3  8 | -3 -20 -17 -17 -18 -12 |1 3 2 0 0 |1 3 2
			#     11 30 34 35 42 44    |19  4  1  7  2 |  5  20  17  17  21  15 |0 1 0 3 2 |1 3 2

		# fCutCnt.nextQuo10
			#      1  8 17 34 39 45    | 7  9 17  5  6 |                        |2 1 0 2 1 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -4  -8 -10 -14 -12 |3 0 2 1 0 |3 2 1
			#      1  7 22 33 37 40(1) | 6 15 11  4  3 | -2   3  13   9  12   7 |2 0 1 2 1 |2 1 2 1
			#      6  7 19 21 41 43(1) | 1 12  2 20  2 |  5   0  -3 -12   4   3 |2 1 1 0 2 |2 1 1 2
			#     11 30 34 35 42 44    |19  4  1  7  2 |  5  23  15  14   1   1 |0 1 0 3 2 |1 3 2
			#     16 25 33 38 40 45    | 9  8  5  2  5 |  5  -5  -1   3  -2   1 |0 1 1 2 2 |1 1 2 2

		# fCutCnt.nextBin
			#      2 16 17 32 39 45    |14  1 15  7  6 |                        |1 2 0 2 1 |1 2 2 1
			#      5  6 11 17 38 44(1) | 1  5  6 21  6 |  3 -10  -6 -15  -1  -1 |2 2 0 1 1 |2 2 1 1
			#      7  8 20 29 33 38(1) | 1 12  9  4  5 |  2   2   9  12  -5  -6 |2 0 2 2 0 |2 2 2
			#      2 10 16 19 34 45    | 8  6  3 15 11 | -5   2  -4 -10   1   7 |1 3 0 1 1 |1 3 1 1
			#      2 11 19 25 28 32(2) | 9  8  6  3  4 |  0   1   3   6  -6 -13 |1 2 2 1 0 |1 2 2 1
			#     15 19 21 34 41 44(1) | 4  2 13  7  3 | 13   8   2   9  13  12 |0 2 1 1 2 |2 1 1 2
			if( all(aRem[c(3,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c(15,44)) ){	surviveFlg[idx]<-FALSE	;next }	# zw
			if( all(aZoid[c(1,6)]==c( 2,32)) ){	surviveFlg[idx]<-FALSE	;next }	# zw

		# fCutCnt.nextRebNum
			#      5 12 14 32 34 42    | 7  2 18  2  8 |                        |1 2 0 2 1 |1 2 2 1
			#     19 28 31 38 43 44    | 9  3  7  5  1 | 14  16  17   6   9   2 |0 1 1 2 2 |1 1 2 2
			#      3  5  8 19 38 42(2) | 2  3 11 19  4 |-16 -23 -23 -19  -5  -2 |3 1 0 1 1 |3 1 1 1
			#      8 21 28 31 36 45(1) |13  7  3  5  9 |  5  16  20  12  -2   3 |1 0 2 2 1 |1 2 2 1
			#      2  4  5 17 27 32    | 2  1 12 10  5 | -6 -17 -23 -14  -9 -13 |3 1 1 1 0 |3 1 1 1
			#      8 10 13 36 37 40    | 2  3 23  1  3 |  6   6   8  19  10   8 |1 2 0 2 1 |1 2 2 1
			if( all(aZoid[c(1,6)]==c( 3,42)) ){	surviveFlg[idx]<-FALSE	;next }	# zw

		# fCutCnt.nextCStepBin
			#      7  8 20 29 33 38    | 1 12  9  4  5 |                        |2 0 2 2 0 |2 2 2
			#      7 17 19 30 36 38(2) |10  2 11  6  2 |  0   9  -1   1   3   0 |1 2 0 3 0 |1 2 3
			#      2 10 16 19 34 45(1) | 8  6  3 15 11 | -5  -7  -3 -11  -2   7 |1 3 0 1 1 |1 3 1 1
			#      2 11 19 25 28 32(2) | 9  8  6  3  4 |  0   1   3   6  -6 -13 |1 2 2 1 0 |1 2 2 1
			#     15 19 21 34 41 44(1) | 4  2 13  7  3 | 13   8   2   9  13  12 |0 2 1 1 2 |2 1 1 2
			#      3  8 16 32 34 43(1) | 5  8 16  2  9 |-12 -11  -5  -2  -7  -1 |2 1 0 2 1 |2 1 2 1
			if( 1<sum( aZoid[ 2 ]*c(2,4)==aZoid[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#     10 14 22 24 28 37    | 4  8  2  4  9 |                        |0 2 3 1 0 |2 3 1
			#     13 14 17 32 41 42(1) | 1  3 15  9  1 |  3   0  -5   8  13   5 |0 3 0 1 2 |3 1 2
			#      6  7 10 16 38 41(1) | 1  3  6 22  3 | -7  -7  -7 -16  -3  -1 |2 2 0 1 1 |2 2 1 1
			#      5 13 17 23 28 36    | 8  4  6  5  8 | -1   6   7   7 -10  -5 |1 2 2 1 0 |1 2 2 1
			#      4  5  6 12 25 37(1) | 1  1  6 13 12 | -1  -8 -11 -11  -3   1 |3 1 1 1 0 |3 1 1 1
			#     14 26 32 36 39 42    |12  6  4  3  3 | 10  21  26  24  14   5 |0 1 1 3 1 |1 1 3 1
			if( all(aRem[c(2,3)]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			#      2  7 19 25 29 36    | 5 12  6  4  7 |                        |2 1 2 1 0 |2 1 2 1
			#     10 15 21 35 38 43    | 5  6 14  3  5 |  8   8   2  10   9   7 |0 2 1 2 1 |2 1 2 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 |  5   4   2  -8 -15  -9 |0 1 4 1 0 |1 4 1
			#      9 14 17 33 36 38(2) | 5  3 16  3  2 | -8  -7  -8   7   9   2 |1 2 0 3 0 |1 2 3
			#      5 11 14 30 33 38(3) | 6  3 16  3  5 | -4  -3  -3  -3  -3   0 |1 2 0 3 0 |1 2 3
			if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c( 5,38)) ){	surviveFlg[idx]<-FALSE	;next }	# zw
			if( all(aZoid[c(1,6)]==c(17,36)) ){	surviveFlg[idx]<-FALSE	;next }	# zw
		# fCutCnt.nextColVal_2
			#      5 21 27 34 44 45    |16  6  7 10  1 |                        |1 0 2 1 2 |1 2 1 2
			#     13 14 26 28 30 36    | 1 12  2  2  6 |  8  -7  -1  -6 -14  -9 |0 2 2 2 0 |2 2 2
			#      7 22 24 31 34 36(1) |15  2  7  3  2 | -6   8  -2   3   4   0 |1 0 2 3 0 |1 2 3
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -5 -12 -12   0  -1   6 |1 2 0 2 1 |1 2 2 1
			#      9 10 13 28 38 45(1) | 1  3 15 10  7 |  7   0   1  -3   5   3 |1 2 1 1 1 |1 2 1 1 1
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -8   6  16   5   2   0 |1 1 1 1 2 |1 1 1 1 2
			if( all(aZoid[c(1,6)]==c( 1,45)) ){	surviveFlg[idx]<-FALSE	;next }	# zw
			if( all(aZoid[c(1,6)]==c( 2,42)) ){	surviveFlg[idx]<-FALSE	;next }	# zw
		# fCutCnt.nextColVal_3
			#      4 12 24 33 38 45    | 8 12  9  5  7 |                        |1 1 1 2 1 |1 1 1 2 1
			#      4  8 18 19 39 44(1) | 4 10  1 20  5 |  0  -4  -6 -14   1  -1 |2 2 0 1 1 |2 2 1 1
			#      2  3 12 20 27 38    | 1  9  8  7 11 | -2  -5  -6   1 -12  -6 |2 1 2 1 0 |2 1 2 1
			#      4  6 15 25 26 33    | 2  9 10  1  7 |  2   3   3   5  -1  -5 |2 1 2 1 0 |2 1 2 1
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -3  -2  -5 -13   2  12 |2 2 1 0 1 |2 2 1 1
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  5   6   8  13   6 -10 |1 2 1 2 0 |1 2 1 2
			#	unique	: 29,44,29... zh==44 && aZoid[c(1,6)] 체크
			if( all(aZoid[c(1,6)]==c( 1,45)) ){	surviveFlg[idx]<-FALSE	;next }	# zw

		# fCutCnt.nextColVal_4
			#      1 10 13 26 32 36    | 9  3 13  6  4 |                        |1 2 1 2 0 |1 2 1 2
			#      3 12 13 18 31 32(2) | 9  1  5 13  1 |  2   2   0  -8  -1  -4 |1 3 0 2 0 |1 3 2
			#     12 18 19 29 31 39(3) | 6  1 10  2  8 |  9   6   6  11   0   7 |0 3 1 2 0 |3 1 2
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   5   0   3  -1 |2 0 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 |  5   9   6  10   7   4 |0 2 0 2 2 |2 2 2
			#     11 17 28 30 33 35(1) | 6 11  2  3  2 | -1  -1  -2  -9  -8  -7 |0 2 1 3 0 |2 1 3

		# fCutCnt.nextColVal_5
			#     23 27 28 38 42 43    | 4  1 10  4  1 |                        |0 0 3 1 2 |3 1 2
			#      6  7 11 17 33 44    | 1  4  6 16 11 |-17 -20 -17 -21  -9   1 |2 2 0 1 1 |2 2 1 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  5   9   7   2  -9  -5 |0 4 1 1 0 |4 1 1
			#      2 10 11 19 35 39(3) | 8  1  8 16  4 | -9  -6  -7   0  11   0 |1 3 0 2 0 |1 3 2
			#     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
			#     12 18 24 26 39 40(3) | 6  6  2 13  1 |  0   0   5  -3   8   1 |0 2 2 1 1 |2 2 1 1
			if( all(aZoid[c(1,2)]==c( 2,10)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,2)]==c(12,18)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[c(1,6)]==c(12,40)) ){	surviveFlg[idx]<-FALSE	;next }	# zw

		# fCutCnt.nextColVal_6
			#      5  7 11 16 41 45    | 2  4  5 25  4 |                        |2 2 0 0 2 |2 2 2
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 |  5   4   1   2 -17  -3 |0 4 1 0 1 |4 1 1
			#      2 21 28 38 42 45(1) |19  7 10  4  3 | -8  10  16  20  18   3 |1 0 2 1 2 |1 2 1 2
			#      9 18 20 24 27 36    | 9  2  4  3  9 |  7  -3  -8 -14 -15  -9 |1 1 3 1 0 |1 1 3 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -5 -11  -7   5   4   3 |2 1 1 2 0 |2 1 1 2
			#     11 17 28 30 33 35    | 6 11  2  3  2 |  7  10  15   1   2  -4 |0 2 1 3 0 |2 1 3

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
		if( 2<sum(aZoid==c( 17,14,NA,16,18,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(  4,26,NA,15,29,33 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(  5,19,NA,25,20,42 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(  5, 4,NA,21,29,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		if( aZoid[2]%in%c(  6    ) ) {	surviveFlg[idx]<-FALSE	;next } # unique 6  10,a,9,a,8,a,7,a,..?



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
		score <- sum(aCStep==c(  1, 5, 8, 2, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  1, 3, 3, 3,12 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 16, 2,13, 5, 1 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  2,NA, 1, 1,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  1,NA, 1,12,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  8,NA, 6, 5,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  2,NA, 3,13,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  5,NA, 3, 6,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  4,NA, 4, 2,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  3,NA, 3, 2,NA ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }

		if( fCutU.hasPtn(c( 1, 7,13, 1),aCStep,thld=3,fixIdx=1) ) {	surviveFlg[idx]<-FALSE	;next }	# unique
		if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(3,2)] ) )	{	surviveFlg[idx]<-FALSE	;next }



		cnt <- 0
			# ...
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		#        *  *  *     *  *  *     *  *  *     *  *  *
		# [  1]  1  9 12                            29 32 44
		if( all(aZoid[1:3+0]==c(  1, 9,12 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+3]==c( 29,32,44 )) ){	surviveFlg[idx]<-FALSE	;next }

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
		# if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum( aCStep[c(1,3)]*c(4,4)==aCStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		if( aCStep[1]%in%c(  3  ) ) {	surviveFlg[idx]<-FALSE	;next }	# double( 3 )
		if( 1<sum( aCStep[ 2 ]*c(1,6)==aCStep[c(3,4)] ) )	{	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		ptnLst <- list( c(1,4) ,c(5,2) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		#        *  *  *     *  *  *     *  *  *   
		# [  1]             12 13  1     5 17  6
		if( all(aZoid[1:3+1]==c( 12,13, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+2]==c(  5,17, 6 )) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,b,b.. a?	basic nextZW nextRebNum nextColVal_1 nextColVal_4 nextColVal_5
			if( 2<sum(aCStep[c( 1,3,1, 2,3,4 ,1,1 )]==c( 4, 2,13,  4, 1, 1, 2,8)) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aCStep[c( 1,3,1, 2,3,4 ,1,1 )]==c( 6, 1, 2,  3,16, 3, 6,6)) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b.. b?	nextQuo10 nextFStepBin nextColVal_5
			if( 2<sum(aCStep[c( 5,3,2 )]==c( 2,6,1 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( 3<sum(aCStep[c( 5,3,2 )]==c( 5,4,6 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. q? nextQuo10 nextCStepBin nextColVal_6
			if( 2<sum(aCStep[c( 2,5,2,4 )]==c( 3,5,6,4 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aCStep[c( 2,5,2,4 )]==c( 7,8,2,2 )) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.basic()
			if( 1<sum( aCStep[ 3 ]*c(2,5)==aCStep[c(1,2)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	(  4, 4*, 6*, 18) ( 4,NA, 3, 5 )
			#	unique	[2:5](  6*,15*, 3, 1)  [2:5](  6,15*, 3*, 1)
			#			[1:4]( 15*, 3*, 1, 17)
			#			[1:6](  6*,15*, 3, 1,17) [1:6](  6,15*, 3*, 1,17)
			if( fCutU.hasPtn(c(  4, 4, 6, 18 ),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA, 3, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(  6,15 )) && any(aCStep[c(4,5)]==c(3, 1)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c( 15, 3 )) && any(aCStep[c(2,5)]==c(6, 1)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c( 15, 3 )) && any(aCStep[c(3,4)]==c(1,17)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(  6,15 )) && any(aCStep[3:5]==c( 3, 1,17)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c( 15, 3 )) && any(aCStep[c(1,4,5)]==c( 6, 1,17)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			#	unique	( 4, 7*, 3*,13, 1)
			#	unique	[1:6]( 17, 4*, 1*, 10, 2) 
			#			[3:5]( 7,NA, 2)  <-- 3/(NA, 4, 1)/( 4,NA, 1) ...  3/(NA, 7, 2)/( 7,NA, 2)
			#			[1:6]( 4, 7,NA,NA,15)	[1:6](21*, 4*, 1, 4, 2)
			#			[c(2,3,5)]( 3, 2, 5)
			if( fCutU.hasPtn(c( 4,7,3,13, 1),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 4,7,NA,NA,15),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(21, 4)) && any(aCStep[3:5]==c(1,4,2)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3,5)]==c(7,2)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(4, 1)) && any(aCStep[c(1,4,5)]==c(17,10, 2)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,3,5)]==c( 3,2,5 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,5)]==c( 7,8 )) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(2,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7,15,21 )
			if( fCutU.hasPtn(c(  7,15,21 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			#	unique	( 8, 6, 3 ) ( 4, 2,13) ( 2,13, 7) (13, 7, 3)
			#	unique	[2:5]( 4*, 2*,13, 7) [2:5]( 4, 2*,13*, 7)	[4:5](13, 7)
			#			[3:5]( 3,NA, 3) <-- (7,6)/(6,*,6) ... (7,3)/(3,*,3)
			if( fCutU.hasPtn(c(  8, 6, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 2,13 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,13, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13, 7, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[4:5]==c( 13, 7 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3,5)]==c(  3, 3 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(4, 2)) && any(aCStep[c(4,5)]==c(13, 7)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c(2,13)) && any(aCStep[c(2,5)]==c(4,7)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			#	unique	( 2, 3,NA,NA, 2)
			#	unique	[1:5](13*,  7*,  3,  5,  9)
			if( fCutU.hasPtn(c( 2, 3,NA,NA, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(13, 7)) && any(aCStep[3:5]==c(3,5,9)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[ 4 ]*c(4,8)==aCStep[c(2,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 18, 8, 3) ( 8, 6, 3) ( 5, 8,16) ( 8,16, 2) (16, 2, 9)
			#	unique	[2:5]( 5*, 8*,16, 2) [2:5]( 5, 8*,16*, 2)
			#	unique	( 8,16) ( 6, 2) ( 7, 3)
			if( fCutU.hasPtn(c(18, 8, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 8, 6, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5, 8,16),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 8,16, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(16, 2, 9),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(5, 8)) && any(aCStep[c(4,5)]==c(16, 2)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c(8,16)) && any(aCStep[c(2,5)]==c(5,2)) ) {	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 8,16) ,c( 6, 2) ,c( 7, 3) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,5)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,2)]*c(1,2)==aCStep[c(4,1)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	(  1, 3,NA,NA, 5)
			#	unique	[1:5](12*, 6*, 4, 3, 3)
			if( fCutU.hasPtn(c( 1, 3,NA,NA, 5),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(12, 6)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(12, 6)) && any(aCStep[c(3,4,5)]==c(4,3,3)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7*, 3*,16*, 3*, 8) ( 7,NA,18, 3*, 5*) ( 5*, 6*, 8,13, 1)
			#	unique	[1:5]( 5,  6, 11,  8*,  3*)	[1:5](  6*, 3*,16, 3, 5)
			#	unique	( 4, 4, a, a)
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(1,2)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(2,3)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(3,4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(1,3)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(2,4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,16, 3, 8 ),aCStep,thld=3,fixIdx=c(1,4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA,18, 3, 5 ),aCStep,thld=3,fixIdx=c(4,5)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 6, 8,13, 1 ),aCStep,thld=3,fixIdx=c(1,2)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[4:5]==c(8,3)) && any(aCStep[c(1,2,3)]==c( 5, 6,11)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(6,3)) && any(aCStep[c(3,4,5)]==c(16, 3,5)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:2]==c(4,4)) && (aCStep[3]==aCStep[4]) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(4,4)) && (aCStep[4]==aCStep[5]) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2

		# fCutCnt.nextColVal_3
			#	unique	( 14, 5, 8*, 1*)	( 14,10, 1 )	( 8, 7, 7 )
			#	unique	[2:5]( 3, 6*, 2*, 16)	[1:4]( 9,10*, 1*, 7)
			#	unique	( 9, 1) ( 5, 7) ( 8,12)
			if( fCutU.hasPtn(c( 14, 5, 8, 1 ),aCStep,thld=3,fixIdx=3:4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14,10, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 7, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c( 6, 2)) && any(aCStep[c(2,5)]==c( 3,16)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(10, 1)) && any(aCStep[c(1,4)]==c(9,7)) ) {	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c(9,1) ,c(5,7) ,c( 8,12) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( 1<sum( aCStep[c(4,5)]*c(2,1)==aCStep[c(1,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 27,NA, 4, 4) ( 6*,10*,NA, 4, 3) ( 9*, 3*,13, 6, 4)
			#	unique	[2:4]( 2,a,2 ) 
			if( fCutU.hasPtn(c( 27,NA, 4, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10,NA, 4, 3 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 3,13, 6, 4 ),aCStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,4)]==c(2,2)) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 3 ]*c(3,3)==aCStep[c(1,2)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 6, 3,16 ) (12, 3, 8) ( 8, 1*,10*,NA,15 )
			#	unique	[1:6]( 5,  2*,  1*,  5, 15)
			#	unique	2>sum(  6, 6,13,NA,(13, 6) )
			if( fCutU.hasPtn(c(  6, 3,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 3, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 1,10,NA,15 ),aCStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[2:3]==c(2,1)) && any(aCStep[c(1,4,5)]==c(5,5,15)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aCStep==c( 6, 6,13,NA,13 ),na.rm=T) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aCStep==c( 6, 6,13,NA, 6 ),na.rm=T) ) {	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( 1<sum( aCStep[c(4,5)]*c(2,1)==aCStep[c(1,3)] ) )	{	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 5, 3, 1 ) ( 11,NA, 4, 3 )
			#	unique	[1:4]( 11, 2, 3*, 2*) [1:5](11,NA, 3,NA,NA) [1:5](NA,NA, 3,NA, 3)
			if( fCutU.hasPtn(c(  5, 3, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,NA, 4, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3,5)]==c( 3, 3 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3)]==c(11, 3 )) ) {	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[3:4]==c( 3, 2 )) && any(aCStep[c(1,2)]==c(11, 2)) ) {	surviveFlg[idx]<-FALSE	;next }

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
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[ 1 ]*c(4,3)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,3)]*c(4,1)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,3)]*c(3,1)==aFStep[c(6,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(-1,-1)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 3 ]*c(4,6)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,3)]*c(1,4)==aFStep[c(5,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 14, 2*, 9*,14)
			#	unique	[2:6](13, 8*, 2*, 9,13)
			if( fCutU.hasPtn(c( 14, 2, 9,14 ),aFStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[2:3]==c(8,2)) && any(aFStep[c(1,4,5)]==c(13, 9,13))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			#	unique	( 6, 6, 7)
			if( 1<sum( aFStep[c(1,3)]*c(1,1)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 6,6,7 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			#	unique	( 1*, 1*, 3, 12) ( 1, 1*, 3*, 12)
			#	unique	[1:4](  2,  9*, 13*,  12)
			if( fCutU.hasPtn(c(  1, 1, 3, 12 ),aFStep,thld=3,fixIdx=1:2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 1, 3, 12 ),aFStep,thld=3,fixIdx=2:3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[2:3]==c( 9,13)) && any(aFStep[c(1,4)]==c( 2,12))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(1,1,1)==aFStep[c(3,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(2,3)]*c(1,1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(2,4)]*c(1,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(11, 4, 2)
			#	unique	[1:3]( -3, -3, 0)
			if( fCutU.hasPtn(c( 11, 4, 2 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[1:3]==c(-3, -3, 0)) ) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(-4, 3, 8)==aFStep[c(1,2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(-2, 1)==aFStep[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( -4,-6*,-6*,16,NA,-11) (-11, -3, -5) ( -2*,-5*,-20, 3)
			#	unique	[2:6]( -3*, -2*, -5,-13,  2)
			if( fCutU.hasPtn(c(  -4,-6,-6,16,NA,-11 ),aFStep,thld=3,fixIdx=2:3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11, -3, -5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -2,-5,-20, 3 ),aFStep,thld=3,fixIdx=1:2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[2:3]==c(-3,-2)) && any(aFStep[c(4,5,6)]==c(-5,-13, 2))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(1,4)==aFStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(  9*, 6*,14, 3, 8) (  1,12, 6*, 9*,14, 1)
			#	unique	[2:6](  5*, 9*, 6,10, 7)
			if( fCutU.hasPtn(c(  9, 6,14, 3, 8 ),aFStep,thld=3,fixIdx=1:2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,12, 6, 9,14, 1 ),aFStep,thld=3,fixIdx=3:4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[2:3]==c(5,9)) && any(aFStep[4:6]==c( 6,10, 7))) {	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			#	unique	( 15,NA, 1*, 2*,11 ) (5, 4, 5)
			#	unique	[4:6]( 7,-3,-8)
			if( fCutU.hasPtn(c( 15,NA, 1, 2,11 ),aFStep,thld=3,fixIdx=3:4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5, 4, 5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aFStep[4:6]==c( 7,-3,-8)) ) {	surviveFlg[idx]<-FALSE	;next }

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

		# fCutCnt.nextRebNum
			#      5 12 14 32 34 42    | 7  2 18  2  8 |                        |1 2 0 2 1 |1 2 2 1
			#     19 28 31 38 43 44    | 9  3  7  5  1 | 14  16  17   6   9   2 |0 1 1 2 2 |1 1 2 2
			#      3  5  8 19 38 42(2) | 2  3 11 19  4 |-16 -23 -23 -19  -5  -2 |3 1 0 1 1 |3 1 1 1
			#      8 21 28 31 36 45(1) |13  7  3  5  9 |  5  16  20  12  -2   3 |1 0 2 2 1 |1 2 2 1
			#      2  4  5 17 27 32    | 2  1 12 10  5 | -6 -17 -23 -14  -9 -13 |3 1 1 1 0 |3 1 1 1
			#      8 10 13 36 37 40    | 2  3 23  1  3 |  6   6   8  19  10   8 |1 2 0 2 1 |1 2 2 1
			if( all(quoSize==c(0,1,1,2,2)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
		#      2  7 19 25 29 36    | 5 12  6  4  7 |                        |2 1 2 1 0 |2 1 2 1
		#     10 15 21 35 38 43    | 5  6 14  3  5 |  8   8   2  10   9   7 |0 2 1 2 1 |2 1 2 1
		#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
		#     17 21 25 26 27 36(1) | 4  4  1  1  9 |  5   4   2  -8 -15  -9 |0 1 4 1 0 |1 4 1
		#      9 14 17 33 36 38(2) | 5  3 16  3  2 | -8  -7  -8   7   9   2 |1 2 0 3 0 |1 2 3
		#      5 11 14 30 33 38(3) | 6  3 16  3  5 | -4  -3  -3  -3  -3   0 |1 2 0 3 0 |1 2 3
			if( all(quoSize==c(1,2,0,3,0)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(quoSize==c(0,1,4,1,0)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
		#      4 12 24 33 38 45    | 8 12  9  5  7 |                        |1 1 1 2 1 |1 1 1 2 1
		#      4  8 18 19 39 44(1) | 4 10  1 20  5 |  0  -4  -6 -14   1  -1 |2 2 0 1 1 |2 2 1 1
		#      2  3 12 20 27 38    | 1  9  8  7 11 | -2  -5  -6   1 -12  -6 |2 1 2 1 0 |2 1 2 1
		#      4  6 15 25 26 33    | 2  9 10  1  7 |  2   3   3   5  -1  -5 |2 1 2 1 0 |2 1 2 1
		#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -3  -2  -5 -13   2  12 |2 2 1 0 1 |2 2 1 1
		#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  5   6   8  13   6 -10 |1 2 1 2 0 |1 2 1 2
			if( all(quoSize==c(1,2,1,2,0)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#      1 10 13 26 32 36    | 9  3 13  6  4 |                        |1 2 1 2 0 |1 2 1 2
			#      3 12 13 18 31 32(2) | 9  1  5 13  1 |  2   2   0  -8  -1  -4 |1 3 0 2 0 |1 3 2
			#     12 18 19 29 31 39(3) | 6  1 10  2  8 |  9   6   6  11   0   7 |0 3 1 2 0 |3 1 2
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   5   0   3  -1 |2 0 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 |  5   9   6  10   7   4 |0 2 0 2 2 |2 2 2
			#     11 17 28 30 33 35(1) | 6 11  2  3  2 | -1  -1  -2  -9  -8  -7 |0 2 1 3 0 |2 1 3
			if( all(quoSize[2:5]==c(0,2,1,3)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5 (zw)
			#     23 27 28 38 42 43    | 4  1 10  4  1 |                        |0 0 3 1 2 |3 1 2
			#      6  7 11 17 33 44    | 1  4  6 16 11 |-17 -20 -17 -21  -9   1 |2 2 0 1 1 |2 2 1 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  5   9   7   2  -9  -5 |0 4 1 1 0 |4 1 1
			#      2 10 11 19 35 39(3) | 8  1  8 16  4 | -9  -6  -7   0  11   0 |1 3 0 2 0 |1 3 2
			#     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
			#     12 18 24 26 39 40(3) | 6  6  2 13  1 |  0   0   5  -3   8   1 |0 2 2 1 1 |2 2 1 1
			if( all(quoSize[1:4]==c(4,1,1,0)) ){	surviveFlg[idx]<-FALSE	;next }

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
	#	"nextRebNum( 37 )","nextColVal_1(  )","nextColVal_5( 29,26 )","nextFStepBin(  )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 37,29,26 ) )]

	# unique	nextBin
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 29 ) )]

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






