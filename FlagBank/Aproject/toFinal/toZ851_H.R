# toZ850_H.R 최종접근
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

# UNdone
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
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,1,3
					if( all(quoSize[1:3+2]==c(3,2,0)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c(  1       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 13       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 27,30    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39,42,36 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA, 3,11,36,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5, 8,NA,17,25,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <16>
			# <28>
			if( fCutU.hasPtn(c( 20,24,22,28,42,36 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 18,29       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    29,35    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    29,NA,38 ),aZoid) ) cnt<-cnt+1
			# <30>
			# <39>
			if( fCutU.hasPtn(c(          27,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,27,31,NA,38,39 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(       31,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,20,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,7      ),c(  1       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c( 13       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 8        ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,0,5    ),c( 27,30    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,9      ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,2,6    ),c( 39,42,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 12        ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4,14,15, 3      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4, 1      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(4)   4(6)   5(3)   8(2)   12(3)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1 16 29 33 40 45    |15 13  4  7  5 |                        |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 |-11 -14 -10  -6   8  -3 |2 1 1 2 0 |2 1 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 |  4  11   1   7  -4   0 |1 2 1 2 0 |1 2 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  7, 8     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  0,-3      ) ) cnt<-cnt+1
			#	unique aFStep[1:2] (7,7)

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -13(2)   -4(2)   -3(2)   0(3)   1(2)   2(2)   4(2)   7(4)   8(2)   11(2) 
			cnt.w2 <- 0
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1	# 13

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
					# [1]   12  2  7  4 18  5  2
					# [2]   10 23  2 11  1  9
					# [3]    8 11 32
					# [4]   
					# [5]   29 26 42 45
					# [6]*  28 44 26 45 30 43 43 44 44

					if( 1<sum(aZoid==c( 12,10, 8,NA,29,28 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  2,23,11,NA,26,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  7, 2,32,NA,42,26 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  4,11,NA,NA,NA,45 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 18,NA,NA,NA,NA,30 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  5, 9,NA,NA,NA,43 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  2,NA,NA,NA,NA,43 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(         ) ) score<-score+1
					if( aZoid[2]%in%c(         ) ) score<-score+1
					if( aZoid[3]%in%c(         ) ) score<-score+1
					if( aZoid[4]%in%c(         ) ) score<-score+1
					if( aZoid[5]%in%c(         ) ) score<-score+1
					if( aZoid[6]%in%c( 43      ) ) score<-score+1
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
					if( aZoid[1]%in%c(  1, 3          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  3             ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 15,18,14,20,13 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 26             ) ) cnt<-cnt+1	# 
					if( aZoid[5]%in%c( 31             ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(                ) ) cnt<-cnt+1

					if( all(aZoid[1:2+1]==c( 1, 4)) ) cnt<-cnt+1

					# [  1]  1  4     4 16    14 25    31 33    36 41
					# [  2]  4  5    16 17    14 27    35 41    23 28
					# [  3]  4 37    15 20    20 23    27 29    32 45
					# [  4]  1  8     9 16    15 28    20 26    18 30
					# [  5]  3  8    22 24             34 39    35 40
					# [  6]  9 12    12 18             34 40    33 34
					# [  7]  8 11    12 29             40 45    40 42
					# [  8] 17 20     8 27             24 26    35 36
					# [  9]  7  8                      13 36         


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 1, 3      ),c(  1, 3          )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 3         ),c(  3             )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 5,8,4,0,3 ),c( 15,18,14,20,13 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 6,8       ),c( 26             )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 5,1       ),c( 31             )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 6         ),c(                )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
							if( aZoid[2]==12   && fCutU.remFilt(aZoid[3],c(7),c( 15,18,14,20,13 )) ) remCnt <- remCnt+1
						# grp (1:2+2)
						# grp (1:2+3)
							if( aZoid[4]==34   && fCutU.remFilt(aZoid[5],c( 8 ),c( 31 )) ) remCnt <- remCnt+1
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

					score  <- sum(aCStep==c(  3,12,11, 2, 5 ),na.rm=T)
					matCnt <- sum(aCStep==c(  1, 1,13, 6, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 33, 5, 3, 2,13 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  7, 7,13, 6,12 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5, 2,NA, 5, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 6,NA, 6, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,17,NA, 5, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,19,NA, 2, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1,NA,NA,23,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  3   1  33   7   5   3   3   3   1 
					#	[2] 12   1   5   7   2   6  17  19 
					#	[3] 11  13   3  13 
					#	[4]  2   6   2   6   5   6   5   2  23 
					#	[5]  5   5  13  12   5   1   2   1 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( aCStep[1]==aCStep[2] )	cnt<-cnt+1
						if( 1<sum(aCStep[1:2+2]==c( 13, 6 )) )	cnt<-cnt+1

						if( aCStep[1]%in%c( 11     ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 13     ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  6, 2  ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  5,13  ) ) cnt<-cnt+1

						if( aCStep[5]==sum(aCStep[c(1,4)]) )	cnt<-cnt+1
						if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,4)]) )	cnt<-cnt+1	# 14

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
					# [  1]  1  4 16                14 27 29            
					# [  2]  7  8 27         
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

					# [1]*  23  3 11  4 21 10 20  2  3  1 16  4  1  4  9  8  9  3  2  9  2  2  4 ...
					# [2]   13  9  4  2  6  1  2
					# [3]*   2 13  5  5  4  3  6  7  2 17  5
					# [4]    9  8  3  2
					# [5]*   7  3  2 11  7 12  5 30  1 14  6 17  6  1 14  1  7 15 14 12 14  4 11 ...

					tCnt <- 0
						if( aCStep[1]%in%c(  2,12,13 ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(        ) ) tCnt<-tCnt+1

						if( aCStep[4]==sum(aCStep[c(3,5)]) )	cnt<-cnt+1
						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					if( fCutU.hasPtn(c(  1, 9 ),aCStep) )	cnt<-cnt+1
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
					# [  1]  5  6     1  5     3  4    10  2
					# [  2]  4  1     6  1     7  4     8  7
					# [  3]  3 12     1  6     6 13     8  5
					# [  4]  2  3     6  2     9  3    30  1
					# [  5]  7 14    18  1    13  2     7  6
					# [  6]  2  8    14 15     4  8     6  1
					# [  7]  8  3             11 13     2  3
					# [  8]  7 12              2  6     7  8
					# [  9]  7 13              7  1     2  1
					# [ 10]  5  3             17  1     1  7
					# [ 11]  7  4              2  1    11  3

					if( aCStep[1]%in%c(  6, 4       ) ) cnt<-cnt+1 # unique 2,3,4,5,..6?
					if( aCStep[2]%in%c(  6,14       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(             ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  4,13, 5,10 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  7, 5       ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  1, 4 )) ) cnt<-cnt+1

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

					# [  1]                         17  1  5
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.colValSeqNext.cStep()

# UNdone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )
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
			if( aZoid[1]%in%c(  5,17  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 14,13  ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33,35  ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 46     ) ) cnt<-cnt+1
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
			if( fCutU.hasPtn(c(       14,NA,33 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 15,NA,27 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       14,NA,33 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(          35,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,11,35    ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,7,3        ),c(  5,17 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,3        ),c( 14,13 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,2        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,5        ),c( 33,35 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6        ),c( 46    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#		u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5, 2     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  8, 4      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 13, 3,19   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(3)   3(2)   4(4)   5(3)   6(2)   8(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 19

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     17 23 27 35 38 43    | 6  4  8  3  5 |                        |0 1 2 2 1 |1 2 2 1
			#     10 14 16 18 27 28(1) | 4  2  2  9  1 | -7  -9 -11 -17 -11 -15 |0 4 2 0 0 |4 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -5  -8  -5  -4  -6  13 |2 2 1 0 1 |2 2 1 1
			#      2 10 11 19 35 39(1) | 8  1  8 16  4 | -3   4   0   5  14  -2 |1 3 0 2 0 |1 3 2
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 |  3   1   1  10  -2   5 |1 2 1 1 1 |1 2 1 1 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  2  -3   1 -14   0   1 |2 2 0 1 1 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  1,-7     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(           ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1, 0,-14 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  1,-3     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(           ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -8        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -5(2)   -3(2)   -2(2)   0(2)   1(4)   5(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(6,1)]*c( 1,-7)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextZW()

# UNdone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 28,43 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(0,3,1)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+0]==c(1,0,0)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[1]%in%c(  2,13,15 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,41      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,10,17 ),aZoid) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c( 13,NA,20          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,NA,28,NA,45 ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 14,18 ),aZoid) ) cnt<-cnt+1
			# <19>
			# <21>
			# <38>
			if( fCutU.hasPtn(c( 11,19,38 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(  4, 4,15,41 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,3,4,5,7 ),c(  2,13,15 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0         ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,9       ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(           ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 7         ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1       ),c( 45,41    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3,10       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  7, 6      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3, 1      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   3(6)   4(2)   5(3)   6(3)   7(4)   14(2)
			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     13 15 18 24 27 41    | 2  3  6  3 14 |                        |0 3 2 0 1 |3 2 1
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -8  -9  -7 -10  -6   0 |2 2 1 0 1 |2 2 1 1
			#      2  6  7 12 19 45(1) | 4  1  5  7 26 | -3   0  -4  -2  -2   4 |3 2 0 0 1 |3 2 1
			#     14 20 23 31 37 38    | 6  3  8  6  1 | 12  14  16  19  18  -7 |0 1 2 3 0 |1 2 3
			#      2 21 28 38 42 45(1) |19  7 10  4  3 |-12   1   5   7   5   7 |1 0 2 1 2 |1 2 1 2
			#     13 14 19 26 40 43    | 1  5  7 14  3 | 11  -7  -9 -12  -2  -2 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(   2    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -11    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -12(2)   -9(2)   -7(3)   -2(4)   0(2)   5(2)   7(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(6,1)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(4,5,6)]) )	cnt.w2<-cnt.w2+1	# -16

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextQuo10()

# UNdone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 35 ) ) return( FALSE )	# unique	35   35   25   35...
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(3,1,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,1,2
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
			if( aZoid[3]%in%c( 21       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 23,16    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 24,18,19 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA, 9,13,10,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5,19          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,17,10,25 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,14,17,24 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,NA,21,30 ),aZoid) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c( 20,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c(       21,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,21,NA,28    ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 10,17,24,39 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(  7,26,NA,40 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       19,NA,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13, 8,NA,20,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2       ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1       ),c( 21       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,4,6   ),c( 23,16    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,4,9   ),c( 24,18,19 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c( 39       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6,16   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(2)   3(4)   4(3)   5(4)   6(2)   11(2)   13(3)   15(2) 

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,3)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  5 21 25 38 41    | 4 16  4 13  3 |                        |2 0 2 1 1 |2 2 1 1
			#     15 20 23 26 39 44    | 5  3  3 13  5 | 14  15   2   1   1   3 |0 1 3 1 1 |1 3 1 1
			#      7  9 15 26 27 42(2) | 2  6 11  1 15 | -8 -11  -8   0 -12  -2 |2 1 2 0 1 |2 1 2 1
			#      5 20 21 24 33 40    |15  1  3  9  7 | -2  11   6  -2   6  -2 |1 0 3 1 1 |1 3 1 1
			#      9 14 20 22 33 34(2) | 5  6  2 11  1 |  4  -6  -1  -2   0  -6 |1 1 2 2 0 |1 1 2 2
			#      1  2 15 19 24 36    | 1 13  4  5 12 | -8 -12  -5  -3  -9   2 |2 2 1 1 0 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -2     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -4,-3  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -12(2)   -8(3)   -6(2)   -2(5)   0(2)   1(2)   2(2)   6(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-4,-6)==aFStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 4 ]*c( 4, 3)==aFStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(6,4)]*c(-4,3)==aFStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# -17
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextBin()

# UNdone
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
					#	unique (0 1 1 3 1) 중복 발생 (0 1 3 2 0)도 중복발생 가능할까?
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
			if( aZoid[2]%in%c( 24    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 28,12    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 33,30    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33,27    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31　　 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			# <16>
			# <30>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,28,30    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       30,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 19,NA,33,35 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(          36,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 26,22,24,36,39 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(          36,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 26,22,24,NA,39 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 24,16,28,30,29,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,5,4        ),c( 24    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2        ),c( 28,12 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,0,7        ),c( 33,30 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,9,7        ),c( 33,27 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,6        ),c( 39    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4,18    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4, 2, 5 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  9       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# 			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(2)   3(6)   4(5)   5(3)   6(2)   8(2)   12(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 15   4  -5  -5  -4  -6 |0 1 3 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   0    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -6(2)   -5(4)   0(2)   4(2)   6(2)   15(2)   18(2) 

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
					if( (aZoid[6]-aZoid[1]) %in% c( 24 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,1)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(1,2,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(3,1,1)) ) return(FALSE)	# next rebind of 3,1,1
					#	unique (2 1 2 0 1) 중복 발생, (2 2 1 1 0)도 중복?
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
			if( aZoid[2]%in%c( 20, 9 ) ) cnt<-cnt+1
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
			# < 9>
			if( fCutU.hasPtn(c(  9,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,14,17,24 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,NA,21,30 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c( 19,23 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       19,NA,33 ),aZoid) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c( 20,26,NA,28 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(    18,24    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,24,45 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c(    23,27 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 21,NA,27 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(       19,NA,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13, 8,NA,20,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(          28,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 29,31,33,NA,35,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,3     ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,9     ),c( 20, 9 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6       ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,9     ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6, 4, 1     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3,11, 5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6, 4, 5     ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 13        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(4)   3(2)   4(4)   5(2)   6(3)   11(4)   15(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7  9 15 26 27 42    | 2  6 11  1 15 |                        |2 1 2 0 1 |2 1 2 1
			#      4  8 19 25 27 45(1) | 4 11  6  2 18 | -3  -1   4  -1   0   3 |2 1 2 0 1 |2 1 2 1
			#      5 20 21 24 33 40    |15  1  3  9  7 |  1  12   2  -1   6  -5 |1 0 3 1 1 |1 3 1 1
			#      9 14 20 22 33 34(2) | 5  6  2 11  1 |  4  -6  -1  -2   0  -6 |1 1 2 2 0 |1 1 2 2
			#     18 20 24 27 31 42(1) | 2  4  3  4 11 |  9   6   4   5  -2   8 |0 1 3 1 1 |1 3 1 1
			#      1  2 15 19 24 36(1) | 1 13  4  5 12 |-17 -18  -9  -8  -7  -6 |2 2 1 1 0 |2 2 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  -5    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -6(3)   -2(2)   -1(4)   0(2)   4(3)   6(2) 
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 0,2,1
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 2,0,1
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
			if( aZoid[2]%in%c( 26    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42,45 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,35,30,39,43    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 7>
			# <11>
			if( fCutU.hasPtn(c(  6,NA,11 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     5,11 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 21,24,32 ),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c( 24,31,NA,40 ),aZoid) ) cnt<-cnt+1
			# <41>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6       ),c( 26    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0       ),c( 40    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,4     ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,5,0   ),c( 42,45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 20, 3       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3,10       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4,11       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3, 5    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   3(6)   4(3)   5(2)   7(2)   11(2)   13(2)   20(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  3 17 20 31 44    | 2 14  3 11 13 |                        |2 1 1 1 1 |2 1 1 1 1
			#      1  7 22 33 37 40(1) | 6 15 11  4  3 |  0   4   5  13   6  -4 |2 0 1 2 1 |2 1 2 1
			#      4  7 11 24 42 45(1) | 3  4 13 18  3 |  3   0 -11  -9   5   5 |2 1 1 0 2 |2 1 1 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 |  1  -1   0 -10 -21  -4 |2 2 1 0 1 |2 2 1 1
			#      8  9 18 21 28 40(1) | 1  9  3  7 12 |  3   3   7   7   7  -1 |2 1 2 0 1 |2 1 2 1
			#      1 21 26 36 40 41(2) |20  5 10  4  1 | -7  12   8  15  12   1 |1 0 2 1 2 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  3      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  9      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  9      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 23     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -4(2)   -1(2)   0(3)   1(2)   3(3)   5(3)   7(3)   12(2) 
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
					if( (aZoid[6]-aZoid[1]) %in% c( 32 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,0)) ) return(FALSE)	# next rebind of 0,1,2
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
			if( aZoid[2]%in%c( 11    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 33    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,NA,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,15, 7,37    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			# <14>
			# <21>
			if( fCutU.hasPtn(c( 11,21 ),aZoid) ) cnt<-cnt+1
			# <22>
			if( fCutU.hasPtn(c(       22,NA,34 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,22,37    ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(       23,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,33,NA,33,35 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(             36,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,30,30,28,NA,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 1       ),c( 11 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,2     ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2,6     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,7     ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,3     ),c( 33 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 30, 1, 2      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1, 2, 7     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 6      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(5)   4(3)   5(3)   7(3)   8(3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 13 20 22 23 36    | 5  7  2  1 13 |                        |1 1 3 1 0 |1 1 3 1
			#      1  5  9 21 27 35    | 4  4 12  6  8 | -7  -8 -11  -1   4  -1 |3 0 2 1 0 |3 2 1
			#      9 14 15 17 31 33(1) | 5  1  2 14  2 |  8   9   6  -4   4  -2 |1 3 0 2 0 |1 3 2
			#      5 12 14 32 34 42(1) | 7  2 18  2  8 | -4  -2  -1  15   3   9 |1 2 0 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  2  25  24   7   6   2 |1 0 0 3 2 |1 3 2
			#     10 21 22 30 35 42    |11  1  8  5  7 |  3 -16 -16  -9  -5  -2 |0 1 2 2 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(   4    ) ) cnt<-cnt+1
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
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -16(2)   -4(2)   -2(3)   -1(3)   2(2)   3(2)   4(2)   6(2)   9(2) 

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
					if( all(quoSize[1:3+1]==c(3,1,0)) ) return(FALSE)	# next rebind of 3,1,0
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
			if( aZoid[2]%in%c( 18,38 ) ) cnt<-cnt+1
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
			# < 5>
			if( fCutU.hasPtn(c(  5,24,25 ),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6, 9          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,19       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,12,24 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,17 ),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  7,NA,18       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     5,18,31,45 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 18,26 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(  6,NA,19       ),aZoid) ) cnt<-cnt+1
			# <23>
			# <26>
			if( fCutU.hasPtn(c( 18,26 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(       24,NA,35 ),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(  4, 4,10,17,40 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,6,4        ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5,3,8        ),c( 18,38 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,8        ),c( 18    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,6        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,1        ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3, 7, 5    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6,10       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  5, 3   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(2)   3(4)   4(5)   5(2)   6(3)   7(4)   8(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 23 27 29 33 41    | 7  4  2  4  8 |                        |0 1 3 1 1 |1 3 1 1
			#      4 20 26 28 35 40    |16  6  2  7  5 |-12  -3  -1  -1   2  -1 |1 0 3 1 1 |1 3 1 1
			#     12 15 19 26 40 43(2) | 3  4  7 14  3 |  8  -5  -7  -2   5   3 |0 3 1 0 2 |3 1 2
			#      5  6 11 17 38 44    | 1  5  6 21  6 | -7  -9  -8  -9  -2   1 |2 2 0 1 1 |2 2 1 1
			#      2  5 15 18 19 23(1) | 3 10  3  1  4 | -3  -1   4   1 -19 -21 |2 3 1 0 0 |2 3 1
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  4   5   3   7  15  12 |1 2 1 2 0 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  6, 3  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -9(2)   -7(2)   -3(2)   -2(2)   -1(4)   1(2)   3(2)   4(2)   5(2) 
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
					if( all(quoSize[1:3+1]==c(1,0,0)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33,34,35 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 4 ),aZoid) ) cnt<-cnt+1
			# < 8>
			# <12>
			if( fCutU.hasPtn(c(  7,NA,12,16,23 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 14,15,23,38,30 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <24>
			# <29>
			if( fCutU.hasPtn(c(          29,32 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,13,NA,29    ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(       31,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,12,31    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  2, 7, 3,27,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  9,NA,NA,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             32,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    21,18,33,NA,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 7,1,9,0  ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3        ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3        ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,2      ),c( 12       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,4,5,1  ),c( 33,34,35 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4        ),c( 44       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(   5, 1  ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(   7     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(   3     ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(4)   4(4)   5(5)   6(2)   10(2)   12(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     17 20 30 31 33 45    | 3 10  1  2 12 |                        |0 1 1 3 1 |1 1 3 1
			#      1  2 15 19 24 36    | 1 13  4  5 12 |-16 -18 -15 -12  -9  -9 |2 2 1 1 0 |2 2 1 1
			#      1  3  8 12 42 43(1) | 2  5  4 30  1 |  0   1  -7  -7  18   7 |3 1 0 0 2 |3 1 2
			#      7  9 24 29 34 38    | 2 15  5  5  4 |  6   6  16  17  -8  -5 |2 0 2 2 0 |2 2 2
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 | -3  -4  -9  -2   0   6 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2,-5      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -4       ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -3       ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  0,-9      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  1,-1      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -9(3)   -7(2)   -3(2)   0(3)   1(2)   6(4) 
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
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+0]==c(2,2,2)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c( 16, 2   ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(         ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 38      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 25      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 28,40   ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,21,30 ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(    11,14       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,11,NA,34,38 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(    11,14       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,14,34,38 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,19,43    ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(          25,28 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4, 6,10,NA,28 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 33,38 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  2,11,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4,2,6    ),c( 16, 2 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3,5      ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,1      ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,8,6    ),c( 38    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5        ),c( 25    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,0,9    ),c( 28,40 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  5, 1, 3,16, 2 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(5)   3(3)   4(3)   5(4)   6(2)   8(2)   9(2)   16(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     10 14 16 18 27 28    | 4  2  2  9  1 |                        |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 |  4   2   3  16   7  10 |1 2 0 3 0 |1 2 3
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 | -4  -9   2  -8   5   1 |2 1 1 2 0 |2 1 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  3,  4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1,  2     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  2,-15      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(3)   3(2)   4(2)   5(3)   13(2) 

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,0,3)) ) return(FALSE)	# next rebind of 2,3,0
					if( all(quoSize[1:3+1]==c(0,0,3)) ) return(FALSE)	# next rebind of 0,0,3
					if( all(quoSize[1:3+2]==c(0,0,0)) ) return(FALSE)	# next rebind of 0,0,1
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
			if( aZoid[1]%in%c(  5, 6    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 10      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 12,10      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 12,13      ) ) cnt<-cnt+1
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
			# <13>
			# <14>
			if( fCutU.hasPtn(c(   2,NA,14    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(        14,17 ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 15,16 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c( 15,NA,35,NA,39 ),aZoid) ) cnt<-cnt+1
			# <41>
			# <43>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,5 ),c(  5, 6 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(     ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,1 ),c( 10    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3   ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,0 ),c( 12,10 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,3 ),c( 12,13 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 2, 3     ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2, 8      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2, 4      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  5       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(8)   3(3)   4(3)   6(5)   27(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3 13 16 24 26 29    |10  3  8  2  3 |                        |1 2 3 0 0 |1 2 3
			#      1 28 35 41 43 44    |27  7  6  2  1 | -2  15  19  17  17  15 |1 0 1 1 3 |1 1 1 3
			#      2  8 33 35 37 41(2) | 6 25  2  2  4 |  1 -20  -2  -6  -6  -3 |2 0 0 3 1 |2 3 1
			#      4 10 14 15 18 22    | 6  4  1  3  4 |  2   2 -19 -20 -19 -19 |1 4 1 0 0 |1 4 1
			#      1  3 12 14 16 43(1) | 2  9  2  2 27 | -3  -7  -2  -1  -2  21 |2 3 0 0 1 |2 3 1
			#      3  9 11 12 13 19(2) | 6  2  1  1  6 |  2   6  -1  -2  -3 -24 |2 4 0 0 0 |2 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  1      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -1,-19       ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -2,  0,-19     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -3       ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -4,-2, 2      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -20(2)   -19(3)   -6(2)   -3(3)   -2(5)   -1(2)   2(3)   15(2)   17(2) 

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 0,1,3
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 3,2,0
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
			if( aZoid[1]%in%c(  6     ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 27,24      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39,43      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,15,23 ),aZoid) ) cnt<-cnt+1
			# <12>
			# <13>
			if( fCutU.hasPtn(c( 13,NA,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,20,39,42    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <19>
			# <39>
			if( fCutU.hasPtn(c(          27,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 27,27,31,NA,38,39 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 24,33,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 7,6,4   ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,5,4   ),c( 27,24 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,9     ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,3,6   ),c( 39,43 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4, 5, 3     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4, 1      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   3(3)   4(6)   5(4)   7(2)   8(3)   12(2)   26(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  6  7 12 19 45    | 4  1  5  7 26 |                        |3 2 0 0 1 |3 2 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  1   3   5   1   6  -2 |2 2 1 0 1 |2 2 1 1
			#      4  5 31 35 43 45(1) | 1 26  4  8  2 |  1  -4  19  22  18   2 |2 0 0 2 2 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   9 -12  -9  -3  -2 |0 3 1 0 2 |3 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 | -8  -1  -2   3  -6  -4 |1 2 1 2 0 |1 2 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  1     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -4(2)   -2(3)   -1(2)   1(3)   2(2)   3(2)   7(2)   9(2) 
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

fltScoreMtx		<- function( ccObjLst ,allIdxF ){

} # fltScoreMtx()

fltScoreMtx2		<- function( ccObjLst ,allIdxF ){

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
			scoreMtx2.evt[phIdx,] <- scoreMtx2[phIdx,] >= c( 3, 2, 1, 1, 100,100, 2, 2 )	# rebL.cnt,rebR.cnt은 100으로
		}

		evtCnt <- apply( scoreMtx2.evt ,1 ,sum )
		if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		# # -[rebV]--------------------------------------------------------------
		if( 7>sum(scoreMtx2[,"rebV"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0030") }
		flg <- scoreMtx2[,"rebV"] >= 3
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0040") }
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0050") }
		flg <- scoreMtx2[,"rebV"] == 2
			if( 5 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0060") }

		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070") }
		flg <- scoreMtx2[,"rebC"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080") }
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090") }

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
	rObj <- list( allIdxF=allIdxF ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx2()

fltCStepValMtx		<- function( ccObjLst ,allIdxF ){

} # fltCStepValMtx()

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

		#	a,b,b.. a?	basic nextRebNum nextCStepBin nextColVal_1 nextColVal_4 nextColVal_5
			if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 42,42,33,39, 8,41 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 39,45,34,42, 4,33 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b.. b?	nextColVal_2 nextColVal_3
			if( 2<sum(aZoid[c( 6,6 )]==c( 44,19 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. q? nextZW nextRebNum nextColVal_3
			if( 2<sum(aZoid[c( 6,4,3 )]==c( 43,36,26 )) ){	surviveFlg[idx]<-FALSE	;next }

		#	bin pattern
		if( all(aBin[c(1,2,3,4,5)]==c(  1,  1,  1,  0,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10
		if( all(aBin[c(1,2,3,4,5)]==c(  1,  0,  1,  0,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10
		if( all(aBin[c(1,3,4,5)]==c(  1,  1,  0,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10
		if( all(aBin[c(1,3,4,5)]==c(  0,  0,  0,  0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10
		if( all(aBin[c(1,2,3,4,6)]==c(  1,  0,  1,  1,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextRebNum
		if( all(aBin[c(1,2,3,4,6)]==c(  1,  1,  0,  1,  0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextRebNum
		if( all(aBin[ 1:6 ]==c(  1, 1, 0, 1, 0, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextCStepBin
		if( all(aBin[ 1:6 ]==c(  0, 1, 1, 1, 1, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextCStepBin
		if( all(aBin[c(1,2,3,4,6)]==c(  0, 0, 0, 0, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_1
		if( all(aBin[c(1,2,3,4,6)]==c(  1, 0, 0, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_1
		if( all(aBin[ 3:6 ]==c(  1, 0, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_3
		if( all(aBin[ 3:6 ]==c(  0, 0, 0, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_3
		if( all(aBin[ 1:6 ]==c(  0, 1, 1, 1, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_4
		if( all(aBin[ 1:6 ]==c(  0, 1, 1, 1, 1, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextColVal_4

		# fCutCnt.basic()
			#      7  8 13 15 33 45    | 1  5  2 18 12 |                        |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 |-11 -14 -10  -6   8  -3 |2 1 1 2 0 |2 1 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 |  4  11   1   7  -4   0 |1 2 1 2 0 |1 2 1 2
			if( aZoid[6] %in% c(42,39) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aZoid[c(2,3)]*c(3,2)==aZoid[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(c( 5,13,17,29,34,39)[c(5,6)]==aZoid[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( 2<=sum(c( 5,13,17,29,34,39)[c(6,3,2)]==aZoid[c(6,5,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextZW
			#     11 17 21 26 36 45    | 6  4  5 10  9 |                        |0 2 2 1 1 |2 2 1 1
			#      4 10 14 15 18 22    | 6  4  1  3  4 | -7  -7  -7 -11 -18 -23 |1 4 1 0 0 |1 4 1
			#     10 22 27 31 42 43(2) |12  5  4 11  1 |  6  12  13  16  24  21 |0 1 2 1 2 |1 2 1 2
			#     10 15 21 35 38 43(2) | 5  6 14  3  5 |  0  -7  -6   4  -4   0 |0 2 1 2 1 |2 1 2 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
			#      5 10 13 21 39 43    | 5  3  8 18  4 | -7  -7 -10 -13  -3  -2 |1 2 1 1 1 |1 2 1 1 1
			if( 1<sum( aZoid[c(1,3)]*c(2,3)==aZoid[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(c( 5,10,13,21,39,43)[c(2,6)]==aZoid[c(1,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 5,10,13,21,39,43)[c(1,6)]==aZoid[c(1,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextQuo10
			#      5 22 31 32 39 45    |17  9  1  7  6 |                        |1 0 1 3 1 |1 1 3 1
			#     12 17 23 34 42 45(1) | 5  6 11  8  3 |  7  -5  -8   2   3   0 |0 2 1 1 2 |2 1 1 2
			#     10 11 12 18 24 42(2) | 1  1  6  6 18 | -2  -6 -11 -16 -18  -3 |0 4 1 0 1 |4 1 1
			#      3 12 13 18 31 32(2) | 9  1  5 13  1 | -7   1   1   0   7 -10 |1 3 0 2 0 |1 3 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 12   9  18  14  10  11 |0 1 1 2 2 |1 1 2 2
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 |  2   0  -6  -6 -14  -7 |0 1 4 1 0 |1 4 1
			if( all(aRem[c(1,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c(17,21,25,26,27,36)[c(6,2)]==aZoid[c(6,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c(17,21,25,26,27,36)[c(1,5)]==aZoid[c(3,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c(17,21,25,26,27,36)[c(3,4)]==aZoid[c(2,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c(17,21,25,26,27,36)[c(5,6)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextBin
			#      7 11 26 28 29 44    | 4 15  2  1 15 |                        |1 1 3 0 1 |1 1 3 1
			#      2 20 33 35 37 40    |18 13  2  2  3 | -5   9   7   7   8  -4 |1 0 1 3 1 |1 1 3 1
			#      3 10 20 26 35 43(2) | 7 10  6  9  8 |  1 -10 -13  -9  -2   3 |1 1 2 1 1 |1 1 2 1 1
			#      3  5 14 20 42 44(2) | 2  9  6 22  2 |  0  -5  -6  -6   7   1 |2 1 1 0 2 |2 1 1 2
			#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  9   9   1   4 -15 -12 |0 3 2 1 0 |3 2 1
			#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
			if( 2<=sum(c( 6,13,20,27,28,40)[c(3,5)]==aZoid[c(2,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 6,13,20,27,28,40)[c(2,4)]==aZoid[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 6,13,20,27,28,40)[c(1,3)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextRebNum
			#      4  5 31 35 43 45    | 1 26  4  8  2 |                        |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			if( 2<=sum(c( 1,16,29,33,40,45)[c(2,3,6,4)]==aZoid[c(1,1,6,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 1,16,29,33,40,45)[c(5,6)]==aZoid[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextCStepBin
			#     12 14 15 24 27 32    | 2  1  9  3  5 |                        |0 3 2 1 0 |3 2 1
			#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 | -3  -9 -11  -3  -3  -7 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 12  12   6   4   0   4 |0 1 1 3 1 |1 1 3 1
			if( 2<=sum(c(19,21,30,33,34,42)[c(5,6,2,5)]==aZoid[c(4,5,1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextFStepBin
			#      8 17 21 24 27 31    | 9  4  3  3  4 |                        |1 1 3 1 0 |1 1 3 1
			#      7 15 20 25 33 43    | 8  5  5  8 10 | -1  -2  -1   1   6  12 |1 1 2 1 1 |1 1 2 1 1
			#     17 20 30 31 33 45(2) | 3 10  1  2 12 | 10   5  10   6   0   2 |0 1 1 3 1 |1 1 3 1
			#      4  8  9 16 17 19(1) | 4  1  7  1  2 |-13 -12 -21 -15 -16 -26 |3 3 0 0 0 |3 3
			#      5  9 12 30 39 43(1) | 4  3 18  9  4 |  1   1   3  14  22  24 |2 1 0 2 1 |2 1 2 1
			#      3 10 23 24 31 39(1) | 7 13  1  7  8 | -2   1  11  -6  -8  -4 |1 1 2 2 0 |1 1 2 2
			if( 1<sum( aZoid[ 1 ]*c(8,13)==aZoid[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 3,10,23,24,31,39)[c(1,3,5)]==aZoid[c(5,2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 3,10,23,24,31,39)[c(3,5)]==aZoid[c(2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_1
			#      1 10 13 26 32 36    | 9  3 13  6  4 |                        |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2
			#     14 26 32 36 39 42(1) |12  6  4  3  3 | 11  16  16  17   8   3 |0 1 1 3 1 |1 1 3 1
			#     12 16 26 28 30 42(2) | 4 10  2  2 12 | -2 -10  -6  -8  -9   0 |0 2 2 1 1 |2 2 1 1
			if( all(aRem[c(1,2)]==aRem[c(6,3)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c(12,16,26,28,30,42)[c(6,6)]==aZoid[c(5,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn
			if( (sum(aZoid%in%c(12,16,26,28,30,42))==1) && (aZoid[5]==42) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn 6->5 만 2번..
			if( all(c(12,16,26,28,30,42)[c(5,6)]==aZoid[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c(12,16,26,28,30,42)[c(2,6)]==aZoid[c(3,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_2
			#      1  7 19 26 27 35    | 6 12  7  1  8 |                        |2 1 2 1 0 |2 1 2 1
			#      6  7 15 16 20 31(1) | 1  8  1  4 11 |  5   0  -4 -10  -7  -4 |2 2 1 1 0 |2 2 1 1
			#      1  4 16 26 40 41(1) | 3 12 10 14  1 | -5  -3   1  10  20  10 |2 1 1 0 2 |2 1 1 2
			#      5  6 26 27 38 39(1) | 1 20  1 11  1 |  4   2  10   1  -2  -2 |2 0 2 2 0 |2 2 2
			#      1 11 15 17 25 39(1) |10  4  2  8 14 | -4   5 -11 -10 -13   0 |1 3 1 1 0 |1 3 1 1
			#     10 28 31 33 41 44    |18  3  2  8  3 |  9  17  16  16  16   5 |0 1 1 2 2 |1 1 2 2
			if( 2<=sum(c(10,28,31,33,41,44)[c(2,4,4,6)]==aZoid[c(2,3,3,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( (sum(aZoid%in%c(10,28,31,33,41,44))==1) && (aZoid[3]==33) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn 6->5 만 2번..

		# fCutCnt.nextColVal_3
			#      3  4  6 10 28 30    | 1  2  4 18  2 |                        |3 1 1 1 0 |3 1 1 1
			#     13 15 18 24 27 41    | 2  3  6  3 14 | 10  11  12  14  -1  11 |0 3 2 0 1 |3 2 1
			#     13 14 26 28 30 36(1) | 1 12  2  2  6 |  0  -1   8   4   3  -5 |0 2 2 2 0 |2 2 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -8  -8 -15 -14  -9   5 |2 2 1 0 1 |2 2 1 1
			#      6 21 35 36 37 41(3) |15 14  1  1  4 |  1  15  24  22  16   0 |1 0 1 3 1 |1 1 3 1
			#      3  9 11 12 13 19    | 6  2  1  1  6 | -3 -12 -24 -24 -24 -22 |2 4 0 0 0 |2 4
			if( 1<sum( aZoid[ 1 ]*c(3,4)==aZoid[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 3, 9,11,12,13,19)[c(1,2)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 3, 9,11,12,13,19)[c(2,5,6)]==aZoid[c(1,2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_4
			#     17 23 27 35 38 43    | 6  4  8  3  5 |                        |0 1 2 2 1 |1 2 2 1
			#      3 10 14 16 36 38(1) | 7  4  2 20  2 |-14 -13 -13 -19  -2  -5 |1 3 0 2 0 |1 3 2
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  0  -1  -2  -3 -11   5 |2 2 1 0 1 |2 2 1 1
			#      8 15 21 31 33 38    | 7  6 10  2  5 |  5   6   9  18   8  -5 |1 1 1 3 0 |1 1 1 3
			#      4  7 13 29 31 39(1) | 3  6 16  2  8 | -4  -8  -8  -2  -2   1 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			if( 1<sum( aZoid[ 2 ]*c(7,9)==aZoid[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aRem[ 2 ]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 4, 5,31,35,43,45)[c(5,1,4)]==aZoid[c(6,1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 4, 5,31,35,43,45)[c(1,5)]==aZoid[c(1,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_5
			#      6 21 35 36 37 41    |15 14  1  1  4 |                        |1 0 1 3 1 |1 1 3 1
			#     15 17 19 21 27 45(1) | 2  2  2  6 18 |  9  -4 -16 -15 -10   4 |0 3 2 0 1 |3 2 1
			#      1 21 26 36 40 41(1) |20  5 10  4  1 |-14   4   7  15  13  -4 |1 0 2 1 2 |1 2 1 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 14   0   5  -4   1   2 |0 1 1 2 2 |1 1 2 2
			#      8 15 21 31 33 38(3) | 7  6 10  2  5 | -7  -6 -10  -1  -8  -5 |1 1 1 3 0 |1 1 1 3
			#      7  8 13 15 33 45(3) | 1  5  2 18 12 | -1  -7  -8 -16   0   7 |2 2 0 1 1 |2 2 1 1
			if( aZoid[2]==7 ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aRem[c(3,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 7, 8,13,15,33,45)[c(2,4)]==aZoid[c(4,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 7, 8,13,15,33,45)[c(2,6)]==aZoid[c(2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 7, 8,13,15,33,45)[c(1,2,3)]==aZoid[c(2,3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3
			if( all(c( 7, 8,13,15,33,45)[c(1,2,5)]==aZoid[c(2,3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3

		# fCutCnt.nextColVal_6
			#      4  6 15 25 26 33    | 2  9 10  1  7 |                        |2 1 2 1 0 |2 1 2 1
			#      2  6  7 12 19 45(1) | 4  1  5  7 26 | -2   0  -8 -13  -7  12 |3 2 0 0 1 |3 2 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  1   3   5   1   6  -2 |2 2 1 0 1 |2 2 1 1
			#      4  5 31 35 43 45(1) | 1 26  4  8  2 |  1  -4  19  22  18   2 |2 0 0 2 2 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   9 -12  -9  -3  -2 |0 3 1 0 2 |3 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 | -8  -1  -2   3  -6  -4 |1 2 1 2 0 |1 2 1 2
			if( 1<sum( aZoid[c(2,3)]*c(3,2)==aZoid[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 5,13,17,29,34,39)[c(2,4,6,5,1)]==aZoid[c(2,3,5,6,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

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
		if( 1<sum(aZoid==c( 12,10, 8,NA,29,28 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  2,23,11,NA,26,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  7, 2,32,NA,42,26 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  4,11,NA,NA,NA,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  5, 9,NA,NA,NA,43 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		if( all(aZoid[1:2+1]==c( 1, 4)) ){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			if( aZoid[1]%in%c(  1, 3          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  3             ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 15,18,14,20,13 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 26             ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 31             ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(                ) ) cnt<-cnt+1
		if( 4<cnt ) {	surviveFlg[idx]<-FALSE	;next }

		# if( aRem[4]==c(6) ){	surviveFlg[idx]<-FALSE	;next }		# if( fCutU.remFilt(aZoid[4],c( 6,8,9  ),c( 16 )) )

		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  3,12,11, 2, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c(  1, 1,13, 6, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c( 33, 5, 3, 2,13 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c(  7, 7,13, 6,12 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c(  5, 2,NA, 5, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c(  3, 6,NA, 6, 1 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score  <- sum(aCStep==c(  3,17,NA, 5, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  3,19,NA, 2, 1 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		# [  1]  1  4 16                14 27 29            
		# [  2]  7  8 27         
		if( all(aZoid[1:3+0]==c(  1,  4, 16 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+0]==c(  7,  8, 27 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+2]==c( 14, 27, 29 )) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,b,b.. a?	nextZW nextQuo10 nextCStepBin nextColVal_2 nextColVal_3 
			if( 2<sum(aZoid[c( 1,3,1,3,4,3,4 )]==c( 12, 5, 1, 1,11, 3, 7 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aZoid[c( 1,3,1,3,4,3,4 )]==c(  5, 1, 2, 2, 8, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b.. b?	nextZW nextBin nextFStepBin nextColVal_1 nextColVal_4 nextColVal_6  
			if( 2<sum(aZoid[c( 2,1,1,2,2,4,1 )]==c(  3, 7, 7,10,26, 8, 8 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. q? 
			# if( 2<sum(aZoid[c( ,, )]==c( ,, )) ){	surviveFlg[idx]<-FALSE	;next }


		# fCutCnt.basic()
			if( 1<sum( aCStep[ 2 ]*c(2,3)==aCStep[c(1,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,4)]*c(2,1)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all( aCStep[4:5]==c( 5, 5 ) ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all( aCStep[4:5]==c(16, 1 ) ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,23,10,14 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA, 2,22 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 2, 2 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,14, 8, 8 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,NA,20, 3, 5 ),aCStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 4,17, 5 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 6,12 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum(aCStep[1:2+0]==c(  5, 3 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( 1<sum(aCStep[1:2+0]==c(  5, 3 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( 1<sum( aCStep[c(2,5)]*c(6,2)==aCStep[c(4,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,18, 1 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,31, 4 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,NA, 5,28, 5 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 6, 8,13, 1 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA, 8,33 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 8,11,15 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }


		# fCutCnt.nextQuo10
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA, 1,NA,16 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA, 1, 9 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 1, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 1, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 1,NA,20 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 1,12 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 1,15 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA,12,NA, 8 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 1, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 4, 8,NA,16 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 4,NA,15 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( fCutU.hasPtn(c( 18,20, 4,11, 7 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA, 3 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[c(1,3)]*c(4,3)==aCStep[c(5,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA,NA, 1, 8 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,NA, 1, 8 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 3, 2,NA,12 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 9, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 29, 5,NA, 7 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 7, 7 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( fCutU.hasPtn(c(  4, 1, 2 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21, 1, 7,15 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,19, 1,12 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 3,33,14 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,26,17, 6 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 5,29,17, 6 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18,NA, 9, 8 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( 1<sum( aCStep[ 4 ]*c(2,5,1,6)==aCStep[c(1,2,3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(3,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,14,NA, 2,16 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 4, 2 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 2, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 6,NA,22, 8 ),aCStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 6, 4 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,17, 1 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 17, 6, 5 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			if( 1<sum(aCStep[1:2+2]==c( 2, 8 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( 1<sum( aCStep[ 3 ]*c(9,4)==aCStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,3)]*c(1,4)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 28,NA, 8, 1 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2, 2, 8 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2, 2, 8 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,NA, 6,14 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 4, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			if( all( aCStep[3:4]==c(1,1) ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all( aCStep[3:4]==c(3,7) ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 1, 8 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 3, 8 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( 1<sum( aCStep[ 5 ]*c(13,2,4)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,2)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,11,20 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 8,12, 1, 5 ),aCStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,22, 2,11 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 8,18,NA, 8 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 8, 1 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 1, 2 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 9,19, 4 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10, 3, 1 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10, 3, 1 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( 1<sum( aCStep[ 2 ]*c(2,3)==aCStep[c(1,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,4)]*c(2,1)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 5, 7, 2 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 5, 7, 2 ),aCStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,NA,10,20, 4 ),aCStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,16, 8 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23, 5, 3 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13, 7,12 ),aCStep ) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,b,b,..a
		#	a,b,b,..b
		#	a,a,b,..b		basic(0,0,-3) nextZW( 6, 6,-12)
			# if( 2<=sum(aZoid[c(6,3)]==c( 39, 6)) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,c..q

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			#	unique	( -6:-6, 8,-22) (-4:-5,16,-4,13) (-3:-16, 1,NA,-3) ( 0&1&4: 4,20, 1, 6,-11, 0)
			#			( 7: 7,-6, 4) ( 8:-18,-22,-13, 8,-9)
			if( fCutU.hasPtn(c( -6, 8,-22 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -5,16,-4,13 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -16, 1,NA,-3 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  4,20, 1, 6,-11, 0 ),aFStep,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  4,20, 1, 6,-11, 0 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  4,20, 1, 6,-11, 0 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  7,-6, 4 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -18,-22,-13, 8,-9 ),aFStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(1,6)]*c(2,5)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14, -7,-14,NA,-2,-4 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -7,-8,NA,NA,-6 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  4,11, 4, 4, 8 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[c(3,6)]*c(1,2)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   2,-6,-1,-12, 4 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -10,-6,-17, 2 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(   2,-3,-12 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[c(2,4)]*c(-1,-2)==aFStep[c(5,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -6, 4, 3, 5 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -5,21,-19, 7, -6 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  1,-3, 1,12 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  1, 7,12, 3 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -7,-21, 7,-5 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -5,-21,-19, 7,-6 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  3, 1,-1,-5, 8 ),aFStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 23, 9,-5, 1 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(-3, 2)==aFStep[c(4,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -17,NA, 8,13 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -11,-10,11,-5,10 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  -5,  6, 1 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(   7, 17,18, 8,-6 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(1,4)]*c(1,1)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(4,6)]*c(3,3)==aFStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 3 ]*c(2,2)==aFStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 7,-7 ),aFStep,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  8, 5,21,21, 1 ),aFStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -1, 2,NA,16,-9 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  3, 5,-4 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  1, 3, 3 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  7, 1, 1,-15, 8 ),aFStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 11,12,11,14 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 12,23,18 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 1 ]*c(3,4,2)==aFStep[c(4,5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,6)]*c(3,2)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2, 3,21 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -5, 1,19 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 11,19, 6,-12 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[ 1 ]*c(5,3,4)==aFStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7,-10,-12,-16 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  0,-16,-6,-16,-13, 6 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -15,-13, 10,-5,-16 ),aFStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -3,NA,-15,-6,-18, 0 ),aFStep,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -19,-15,-19, -8, 0 ),aFStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[ 3 ]*c(1,1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -13, 10,-18,-10,-19, 4 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -4,20,-15,-16 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  9,23, 1,-14 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 11, 3,10 ),aFStep,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			cnt <- 0
				if( 1<sum( aFStep[ 3 ]*c(1,1)==aFStep[c(4,5)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[ 2 ]*c(2,2,2)==aFStep[c(3,4,5)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[ 1 ]*c(4,8)==aFStep[c(2,3)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[ 1 ]*c(4,8)==aFStep[c(2,4)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[ 1 ]*c(4,8)==aFStep[c(2,5)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,3)]*c(4,2)==aFStep[c(2,4)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,3)]*c(4,2)==aFStep[c(2,5)] ) ) cnt <- cnt+1
			if( cnt>1 ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14,-1, 5 ),aFStep,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(-3,-3)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 6 ]*c(3,1,2)==aFStep[c(3,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(4,6)]*c(3,2)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,-2,NA,11 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  0,-3,NA,15,NA, 7 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -6,NA,18, 4 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[c(2,3)]*c(-1, 2)==aFStep[c(6,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -12,-16,15,21 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  2, 3,-10, -6 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  8,-13,-8 ),aFStep,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -7,-10,-22, 1 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -15,-3,-4 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  -1,-6,-11 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -18, -4, 7 ),aFStep ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( 1<sum( aFStep[ 3 ]*c(4,3,2)==aFStep[c(1,5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(3,4)]*c(2,-2)==aFStep[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -8,11, 3, -6 ),aFStep,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -5, 3, -17,-9 ),aFStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }		# 

		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

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

		# fCutCnt.nextQuo10
			#      5 22 31 32 39 45    |17  9  1  7  6 |                        |1 0 1 3 1 |1 1 3 1
			#     12 17 23 34 42 45(1) | 5  6 11  8  3 |  7  -5  -8   2   3   0 |0 2 1 1 2 |2 1 1 2
			#     10 11 12 18 24 42(2) | 1  1  6  6 18 | -2  -6 -11 -16 -18  -3 |0 4 1 0 1 |4 1 1
			#      3 12 13 18 31 32(2) | 9  1  5 13  1 | -7   1   1   0   7 -10 |1 3 0 2 0 |1 3 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 12   9  18  14  10  11 |0 1 1 2 2 |1 1 2 2
			#     17 21 25 26 27 36(1) | 4  4  1  1  9 |  2   0  -6  -6 -14  -7 |0 1 4 1 0 |1 4 1
			if( all(quoSize[1:2]==quoSize[5:4]) ){	surviveFlg[idx]<-FALSE	;next }
    		#	unique 좌우대칭 또 재발?

		# fCutCnt.nextBin
			#      7 11 26 28 29 44    | 4 15  2  1 15 |                        |1 1 3 0 1 |1 1 3 1
			#      2 20 33 35 37 40    |18 13  2  2  3 | -5   9   7   7   8  -4 |1 0 1 3 1 |1 1 3 1
			#      3 10 20 26 35 43(2) | 7 10  6  9  8 |  1 -10 -13  -9  -2   3 |1 1 2 1 1 |1 1 2 1 1
			#      3  5 14 20 42 44(2) | 2  9  6 22  2 |  0  -5  -6  -6   7   1 |2 1 1 0 2 |2 1 1 2
			#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  9   9   1   4 -15 -12 |0 3 2 1 0 |3 2 1
			#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
			#	unique	(1 1 3 0 1) 패턴 존재. (1 0 1 3 1) 재발 가능? 
			if( all(quoSize==c(1,0,1,3,1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			#      4  5 31 35 43 45    | 1 26  4  8  2 |                        |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#	unique (0 1 1 3 1)재발 발생, (1 1 1 1 2)도 근시간내에 재발될까?
			if( all(quoSize==c(1,1,1,1,2)) ){	surviveFlg[idx]<-FALSE	;next }

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
	#	"nextRebNum( NA )","nextColVal_1( 28,37 )","nextColVal_5(  )","nextFStepBin(  )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 28,37 ) )]

	# unique	nextColVal_2
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 34,40 ) )]

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






