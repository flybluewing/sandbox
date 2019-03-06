# toZ849_H.R 최종접근
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
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(0,1,3)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[2]%in%c( 16,21    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 30       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 28,16, 5 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 36       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA, 3,11,36,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <16>
			# <30>
			if( fCutU.hasPtn(c( 30,43 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 19,NA,33,35 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(  5,11,22,23,26,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(       31,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,20,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,3        ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,1        ),c( 16,21    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6,0,2        ),c( 30       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8,6,5        ),c( 28,16, 5 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,4,2        ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,0        ),c( 36       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 14      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3,16   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 16, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 1,23, 9) ( 2: 9, 2, 2 ) ( 2: 8,NA, 2,22) (13:13,20,15)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (6)   4 (2)   5 (2)   12 (3)   13 (2) 
			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(1,5,2)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     19 21 30 33 34 42    | 2  9  3  1  8 |                        |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 |-11 -14 -10  -6   8  -3 |2 1 1 2 0 |2 1 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -3,  0 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -6, 8 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+3]==c(  4, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(-14,-13),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	(-13: 4,-13,-9,12) (-13:-14,-13, 11) (-3:-29,-26,-16, 1,NA,-3) ( 0:14,-4,-14,-2,-1, 0)
			#			(  3:21,21,20,17, 3) ( 8:-22,-13, 8,-9)
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (3)   -6 (2)   -3 (2)   0 (2)   3 (2)   7 (2)   8 (3)
			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(3,4,6,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# -17

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
					# [1]   17 12  3 11
					# [2]    1 12
					# [3]   15 25 21 20
					# [4]*  32 32 37
					# [5]   45 40 25 32
					# [6]*  44 44 34 21 31 45 45 34 35 45 36 43 40

					#	unique	aZoid[c(4,6)] == c(32,44)
					#	unique	aRem[c(3,4,6)] == c(5,2,4) aRem[c(3,4,6)] == c(1,7,1)
					if( 1<sum(aZoid==c( 17,NA,15,32,NA,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 12,12,25,32,40,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  3,NA,21,37,25,34 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 11,NA,20,NA,32,21 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(         ) ) score<-score+1
					if( aZoid[2]%in%c(         ) ) score<-score+1
					if( aZoid[3]%in%c(         ) ) score<-score+1
					if( aZoid[4]%in%c( 32,37   ) ) score<-score+1
					if( aZoid[5]%in%c(         ) ) score<-score+1
					if( aZoid[6]%in%c( 44,34   ) ) score<-score+1
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
					if( aZoid[2]%in%c( 23,27   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 39,20   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 16      ) ) cnt<-cnt+1	# 
					if( aZoid[5]%in%c( 19      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 36      ) ) cnt<-cnt+1

					if( all(aZoid[3:4]==c(19,26)) ) cnt<-cnt+1

					# [  1] 13 15    15 40    33 45    17 33    30 35
					# [  2]  3 23    24 41    16 18    18 27    31 36
					# [  3] 24 27    14 20    19 26    39 44    33 35
					# [  4] 15 27    18 21    19 26    28 35         
					# [  5] 19 20    15 19    14 28    26 31         
					# [  6] 26 27    16 20    17 30    29 36         
					# [  7] 17 20             10 11    37 40         
					# [  8] 11 17              9 25    31 34         
					# [  9]                   20 29    31 40         
					# [ 10]                   33 35    30 33         

					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 3,4    ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 3,7,6  ),c( 23,27 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 1,0    ),c( 39,20 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 6,8,9  ),c( 16    )) )	remCnt <- remCnt+1	# unique 6
						if( fCutU.remFilt(aZoid[5],c(        ),c( 19    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 4      ),c( 36    )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
							if( aZoid[2]==27   && fCutU.remFilt(aZoid[1],c(3),c( )) ) remCnt <- remCnt+1	# unique
						# grp (1:2+1)
						# grp (1:2+2)
							if( aZoid[3]==19   && fCutU.remFilt(aZoid[4],c(6),c( 16    )) ) remCnt <- remCnt+1
							if( aZoid[4]==26   && fCutU.remFilt(aZoid[3],c(9),c( 39,20 )) ) remCnt <- remCnt+1
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

					score  <- sum(aCStep==c(  2,25,12,16, 5 ),na.rm=T)
					matCnt <- sum(aCStep==c( 20,17, 2, 9, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 6, 7, 5, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 12, 3, 7, 7,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1, 4,14, 5,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1, 4,13, 7,NA ),na.rm=T)	# unique ( 1&4:1, 4,15, 3)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3,NA, 1, 3,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  6,NA,16, 3,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 9, 9,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 2, 3,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA, 2, 4,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  2  20   3  12   1   1   3   6 
					#	[2] 25  17   6   3   4   4 
					#	[3] 12   2   7   7  14  13   1  16   9   2   2 
					#	[4] 16   9   5   7   5   7   3   3   9   3   4   5 
					#	[5]  5   5   2 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1

						if( aCStep[1]%in%c(       ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(       ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(       ) ) cnt<-cnt+1
						if( aCStep[4]%in%c( 9     ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 5, 2  ) ) cnt<-cnt+1

						if( 1<sum( aCStep[ 1 ]*c(6,8)==aCStep[c(3,4)] ) )	cnt<-cnt+1
						if( 1<sum( aCStep[c(1,5)]*c(6,5)==aCStep[c(3,2)] ) )	cnt<-cnt+1

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
					# [  1] 13 15 40                16 18 27    30 33 35

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

					# [1]*   1  3  9 15  2 12  5  5  1  5
					# [2]    3  2  1
					# [3]*   9 12  4  7  6  6 14  2  7  4 11  5  1  6  8  8  4 11  7  2  2 16
					# [4]*   1 13  6  9  2  2  9  1  1  2 10  2  6  7  7  4 10 19  9 13  3  5  1  4  ...
					# [5]    2  1  6  4  1 12  5  1  3  6  1

					tCnt <- 0
						if( aCStep[1]%in%c(  4     ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  4     ) ) tCnt<-tCnt+1	# unique 1,2,3...4?
						if( aCStep[3]%in%c(  3, 1  ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(  3     ) ) tCnt<-tCnt+1
						#	unique	aCStep[4:5]==c(9,1)	aCStep[4:5]==c(2,12)

						if( 1<sum( aCStep[c(1,2)]*c(1,3)==aCStep[c(4,3)] ) )	cnt<-cnt+1
						if( aCStep[5]==sum(aCStep[c(1,4)]) )	cnt<-cnt+1
						if( aCStep[2]==sum(aCStep[c(1,5)]) )	cnt<-cnt+1
						if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt<-cnt+1
						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					if( 1<sum(aCStep[1:2+0]==c( 1, 1 )) )	cnt<-cnt+1	# 1,3
					if( fCutU.hasPtn(c( 9, 1 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+2]==c( 3, 2 )) )	cnt<-cnt+1

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
					# [  1]  2 25     6  3     2 12     1  4
					# [  2]  1  5     1  8     1  7     1  8
					# [  3]  4 19     1 12     5  2     5  8
					# [  4]  5  9    13  4     4  6     1  8
					# [  5] 14 11     2  9     2  9     6  7
					# [  6] 11  4     1  8     2 20     6 11
					# [  7]  7  6     1 18     6  9     4 11
					# [  8]  2  4     1 20     2  8    15  3
					# [  9]  3  5     9 10     1  6    13  8
					# [ 10]  4 16     3  4     2  9    16  6
					# [ 11]           5  2     3 14     3  2

					if( aCStep[1]%in%c( 3         ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6, 1, 9   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(20, 3      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 8, 1, 5, 6, 4 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 8,11       ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  1,18 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  1, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(  5, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  1, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  2,10 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  4,11 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  5, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  5, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  4, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  1, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 11, 9 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  3,12 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  1, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  4, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 13, 8 )) ) cnt<-cnt+1

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[1]%in%c(  2       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12,11    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 14,19,33 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 44       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,36    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,22,45,40 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,NA,41    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,14          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,21,NA,30 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 11,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,32       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 20,27,33,38 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(  6, 9,12,10,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c( 23,25,30,40 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 11,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			# <43>
			# <45>
			if( fCutU.hasPtn(c( 21,39,NA,44,45 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,3     ),c(  2       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,0     ),c( 12,11    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,3,4   ),c( 14,19,33 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,0,9   ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c( 44       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,6,2   ),c( 45,36    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3,18   ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  9, 4   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3, 8 )) )	cnt.w1<-cnt.w1+1
			#	unique	( 2&8: 8, 2,11,16) ( 3: 3*, 3,NA,26) ( 4:17,20, 4) (13:27,13, 2,10)
			# -------------------------------------------------------------------------------------
			#     FV :    2 (4)   3 (4)   4 (5)   8 (2)   9 (4)   13 (2)   15 (2) 
			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 20
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 22

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      9 33 36 40 42 43(2) |24  3  4  2  1 |  4  24  24  10   3   0 |1 0 0 2 3 |1 2 3
			#      2 10 12 31 33 42(2) | 8  2 19  2  9 | -7 -23 -24  -9  -9  -1 |1 2 0 2 1 |1 2 2 1
			#      1 10 13 26 32 36(1) | 9  3 13  6  4 | -1   0   1  -5  -1  -6 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -7   5  10  12   4   0 |1 1 1 1 2 |1 1 1 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 12     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  4,-1,18 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-7:-7,NA,NA,NA,17 1) (-5:15, 2,11, -5, 9,15) (-1:21,24,11,-1, -1*) ( 1:14, 1,26,-9, 14)
			#			( 4:-21, 9,14,NA, 4, -9) (10:-14, 10,21, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -7 (2)   -5 (2)   -1 (3)   0 (3)   1 (2)   4 (3)   10 (2)   24 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,5)]*c(2,3)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,1,2,5)]) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 42 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,1,1
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
			if( aZoid[2]%in%c(  4    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,41 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 5          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA, 8, 3,30 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,42,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4, 5       ),aZoid) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  1, 5       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4, 5       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     5,45,43 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,14    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,28 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  7,NA,12,35 ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 21,NA,41 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 10,NA,28 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       14,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,NA,NA,28,31 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c( 21,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,18,41 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 14,15,NA,21,33,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0        ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4        ),c(  4    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0        ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,6,1   ),c( 29    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1     ),c( 45,41 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 3, 4, 2 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique	( 1: 1,NA,26,12,12) ( 2: 5,10, 2) ( 7: 3,27, 7, 7*) (10: 5,18,10, 9)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (4)   3(4)   5(3)   7 (2)   9 (2)   10 (2)   15 (2) 
			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,3)])==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  4  5 17 27 32    | 2  1 12 10  5 |                        |3 1 1 1 0 |3 1 1 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  11  13   4   7   9 |0 3 1 1 1 |3 1 1 1
			#      1  3 12 21 26 41(2) | 2  9  9  5 15 | -9 -12  -6   0  -8   0 |2 1 2 0 1 |2 1 2 1
			#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   1  -2  -9   2   4 |2 2 1 0 1 |2 2 1 1
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  3   1  21  23  15   0 |2 0 0 2 2 |2 2 2
			#      9 10 13 28 38 45(1) | 1  3 15 10  7 |  5   5 -18  -7  -5   0 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  5      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  0,  4 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-9:-9,16,14) ( 0: 7, 9,NA,NA,-25, 0) ( 1: 6, 1,NA,NA,28, -4) ( 4:-12,-29,-9, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   0 (5)   1 (2)   4 (2)   5 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c( 1,-1)==aFStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+1]==c(2,1,3)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[1]%in%c( 26    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 24,19 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,36 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			# <17>
			if( fCutU.hasPtn(c( 17,NA,NA,38    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,29,30,NA,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 10,NA,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,26 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 12,NA,24,NA,25,27 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(       21,26 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,26 ),aZoid) ) cnt<-cnt+1
			# <36>
			# <43>
			if( fCutU.hasPtn(c( 32,NA,32,33,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 20,25,18,11,29,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 7,1,6,5        ),c( 26    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,9        ),c( 24,19 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,4        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,5        ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,7,6,3        ),c( 45,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5,15       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5,11       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2,13, 7, 5 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  4, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  9, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 1: 2,15, 4, 1) ( 2:13, 2,12) ( 2:23,NA,NA, 4, 2) ( 3: 9, 3,16) ( 4: 4, 7,12,17)
			#			( 5: 4, 7, 5) ( 8: 8, 5,13) (15:15,NA,12, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (4)   3 (3)   4 (2)   5 (3)   6 (2)   7 (3)   8 (3)   9 (3)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3 10 19 24 32 45    | 7  9  5  8 13 |                        |1 2 1 1 1 |1 2 1 1 1
			#      5  9 12 20 21 26    | 4  3  8  1  5 |  2  -1  -7  -4 -11 -19 |2 1 3 0 0 |2 1 3
			#      2  9 24 41 43 45(1) | 7 15 17  2  2 | -3   0  12  21  22  19 |2 0 1 0 3 |2 1 3
			#     11 17 21 26 36 45(1) | 6  4  5 10  9 |  9   8  -3 -15  -7   0 |0 2 2 1 1 |2 2 1 1
			#      7 22 24 31 34 36(1) |15  2  7  3  2 | -4   5   3   5  -2  -9 |1 0 2 3 0 |1 2 3
			#     17 25 28 37 43 44    | 8  3  9  6  1 | 10   3   4   6   9   8 |0 1 2 1 2 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -5, 3,-15     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(   3     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(   5     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   7, 3    ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  9, 8 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+4]==c( -4, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( -7: -8,NA, -7, 4) (-4:-4,21,24) ( 3:15, 3*, 3,14,27)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -4 (2)   -3 (2)   0 (2)   3 (2)   5 (2)   8 (2)   9 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(2,3)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(2,3)]*c(2,2)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 13
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1	# 14
			if( sum(aFStep[c(4,5)])==sum(aFStep[c(2,3,6)]) )	cnt.w2<-cnt.w2+1	# 15
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 16

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
					if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )	# unique
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+2]==c(4,0,1)) ) return(FALSE)	# next rebind of 0,2,2
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
			if( aZoid[5]%in%c( 41    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c(  3,11       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,NA,29 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,27,34 ),aZoid) ) cnt<-cnt+1
			# <28>
			# <29>
			if( fCutU.hasPtn(c(    11,NA,29 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,29 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 18,26,30,44,45 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <38>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,5     ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2       ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,8     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1       ),c( 41 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,9,2   ),c( 44 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2      ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 8   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique	( 1: 5,32, 1) ( 3:36, 1, 3) ( 4:31, 4, 2) ( 4: 9,NA,14, 4,19) ( 6: 6,23, 1) (17:13,17,NA,17)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (3)   3 (3)   4 (4)   6 (2)   7 (3)   17 (2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 24 28 35 38 40    |16  4  7  3  2 |                        |1 0 2 2 1 |1 2 2 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 | -5 -18 -18  -5  -4  -3 |2 1 0 3 0 |2 1 3
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 11   9   6 -13   4   8 |0 4 0 1 1 |4 1 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -9  -4  -4  12  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 |  7   7  18  10   8  -2 |0 2 0 2 2 |2 2 2
			#      2  4 11 28 29 43    | 2  7 17  1 14 |-10 -14 -19 -11 -12   1 |2 1 2 0 1 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( 11     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -3,-4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -4,-5 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-5:-3,10,NA,-5, 2) (-4:-13, -4,-5) (-4: 0,-3,-4,27) ( 8: 5, 8,NA,16, 8*)
			# -------------------------------------------------------------------------------------
			#     FV :    -18 (2)   -5 (3)   -4 (3)   7 (2)   8 (2) 

			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,5)])==sum(aFStep[c(1,2,6)]) )	cnt.w2<-cnt.w2+1	# -23
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1	# -33

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
					if( all(quoSize[1:3+0]==c(2,2,2)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[5]%in%c( 40,10 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,45 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,11             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,14          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,NA,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,11,NA,43 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <21>
			if( fCutU.hasPtn(c( 10,NA,21    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,21,43 ),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(       24,NA,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       24,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 25,NA,35 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(       24,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             32,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,19,NA,36 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(  5,NA,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,NA,39    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          39,43 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  5,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          39,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3, 1,14,41,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1        ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,7        ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,3        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,8        ),c( 40,10 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,5        ),c( 43,45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5, 4, 8, 6 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(   3,  8     ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(             ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 3, 8    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 8, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 5, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 8, 1 )) )	cnt.w1<-cnt.w1+1
			#	unique	( 3: 3, 8,NA,12) ( 4: 6, 3,NA,27, 4) ( 5: 5,NA, 2,33, 3) ( 9:13,NA, 9, 8)
			#			(18: 2,13,18)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (2)   3 (7)   4 (3)   5 (3)   6 (2)   8 (4)   9 (2)   18 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      7 22 24 31 34 36    |15  2  7  3  2 |  2  13  12   1  -5  -7 |1 0 2 3 0 |1 2 3
			#     10 15 21 35 38 43    | 5  6 14  3  5 |  3  -7  -3   4   4   7 |0 2 1 2 1 |2 1 2 1
			#     17 25 28 37 43 44(1) | 8  3  9  6  1 |  7  10   7   2   5   1 |0 1 2 1 2 |1 2 1 2
			#      5 10 13 21 39 43(1) | 5  3  8 18  4 |-12 -15 -15 -16  -4  -1 |1 2 1 1 1 |1 2 1 1 1
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 |  8   6  11   4  -6  -7 |0 2 2 2 0 |2 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( NA     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	( 4:13,19,25, 4,-16,-21) ( 4:23,15,18, 4,-19) ( 7:10,16, 7)
			# -------------------------------------------------------------------------------------
			#     FV :    -15 (2)   -7 (3)   1 (2)   2 (2)   4 (3)   7 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,4)]*c(-1, 2)==aFStep[c(5,1)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(4,3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,3,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,4,2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )	# unique
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+0]==c(1,2,2)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 1,2,2
					#	unique (0 1 1 2 2) 이 2번 나타남. --> 0 1 1 2 2, 1 1 2 1 1
					#	unique (2 2 0 1 1/2 2 0 1 1) 패턴이 2번 반복. 뭔가 조건이 생길지도..
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
			if( aZoid[1]%in%c(  2    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 26    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 43,36 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <13>
			if( fCutU.hasPtn(c( 12,13,36,34,29 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(  6,NA,16    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    15,16    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,32 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  9, 4,31,42 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(    13,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,NA,32,35,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(    26,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,35,32,43 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,3        ),c(  2    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,0        ),c( 26    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,4,9      ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,8        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,6,1      ),c( 43,36 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,6        ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 12, 4, 3 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 13       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 8, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  4, 7 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 14, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 2:20, 5, 2, 1) ( 3: 9, 3,19, 7, 7) ( 6:14, 6, 6)
			#	unique	( 5, 3 ) ( 7, 2 )
			# -------------------------------------------------------------------------------------
			#     FV :    2 (4)   3 (3)   4 (2)   5 (3)   6 (2)   7 (4)   9 (5)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 13 20 29 36 41    | 9  7  9  7  5 |                        |1 1 2 1 1 |1 1 2 1 1
			#      6  8 13 16 30 43(1) | 2  5  3 14 13 |  2  -5  -7 -13  -6   2 |2 2 0 1 1 |2 2 1 1
			#     19 28 31 38 43 44(1) | 9  3  7  5  1 | 13  20  18  22  13   1 |0 1 1 2 2 |1 1 2 2
			#      3  7 14 16 31 40(1) | 4  7  2 15  9 |-16 -21 -17 -22 -12  -4 |2 2 0 1 1 |2 2 1 1
			#     15 27 33 35 43 45    |12  6  2  8  2 | 12  20  19  19  12   5 |0 1 1 2 2 |1 1 2 2
			#      1 10 13 26 32 36    | 9  3 13  6  4 |-14 -17 -20  -9 -11  -9 |1 2 1 2 0 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  11    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  20    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  20    ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  11    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  18    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 11,20 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 20,20 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-17:-7,-17,-18,-6,-18) (20:11,20*,20,16,11, 9)
			# -------------------------------------------------------------------------------------
			#     FV :    -17 (2)   -9 (2)   2 (2)   12 (2)   13 (2)   19 (2)   20 (2) 
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(6,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# -31

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
					if( (aZoid[6]-aZoid[1]) %in% c( 27 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 0,1,3
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
			if( aZoid[2]%in%c( 30,27      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,36,30      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <18>
			if( fCutU.hasPtn(c(  1,18,40 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 25,28 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 25,28 ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(  8,30,NA,41 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 11,32,NA,43 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c(             41,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,35,40,33,NA,44 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  8,11,32,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(  3,8,4   ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(  0,7     ),c( 30,27    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  4       ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  2       ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  3,7,4,1 ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(  5,6,0,3 ),c( 45,36,30 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 10, 2    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2, 1    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 2    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2,19    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1	# -	unique 4,3,2..1?
			if( fCutU.hasPtn(c( 1, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 9, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	( 2:19, 2, 1) ( 3: 4,NA, 3,15) ( 4: 4, 5,11) ( 9: 9, 3, 3,NA,17)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (5)   3 (4)   4 (2)   9 (4)   12 (2)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(1,6)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     17 25 28 37 43 44    | 8  3  9  6  1 |                        |0 1 2 1 2 |1 2 1 2
			#      3 12 13 18 31 32    | 9  1  5 13  1 |-14 -13 -15 -19 -12 -12 |1 3 0 2 0 |1 3 2
			#     11 30 34 35 42 44    |19  4  1  7  2 |  8  18  21  17  11  12 |0 1 0 3 2 |1 3 2
			#      9 18 20 24 27 36    | 9  2  4  3  9 | -2 -12 -14 -11 -15  -8 |1 1 3 1 0 |1 1 3 1
			#      2 25 28 30 33 45    |23  3  2  3 12 | -7   7   8   6   6   9 |1 0 2 2 1 |1 2 2 1
			#      5 18 30 41 43 45(2) |13 12 11  2  2 |  3  -7   2  11  10   0 |1 1 0 1 3 |1 1 1 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -7     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 11     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  8     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-14&-15:-14, -9,-15, 3) (-12:15,-12,-16) (-7:-7,-3,14,14,-6) ( 8: 8,-6,-9, 1)
			#			( 11:-12,NA,-13, 11, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    -15 (2)   -14 (2)   -12 (3)   -7 (2)   6 (2)   8 (2)   11 (2) 
			cnt.w2 <- 0
			if( aFStep[1]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 13

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
					if( (aZoid[6]-aZoid[1]) %in% c( 40 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c(  3,15  ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  5     ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(        ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,34  ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,29 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,20,NA,36    ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 16,19    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,NA,28 ),aZoid) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c(  1, 2,18,20 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 15,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    21,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,21,36    ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 16,NA,28 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 19,28 ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 11, 8,21,44 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,5,6        ),c(  3,15  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,6,5        ),c(        )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5       ),c(  5     )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(        )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(        )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,6,4,7 ),c( 43,34  )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2,12       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  8       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique	( 1: 1*,19, 1,10) ( 8:20, 7, 8,26)
			#			( 3: 3, 1, 3) ( 4: 4,15,24) 
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   3 (5)   4 (4)   6 (4)   8 (2)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(3,2,4)==aCStep[c(2,4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(3,4)]*c(3,2)==aCStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 20

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  8 25 27 37 41    | 4 17  2 10  4 |                        |2 0 2 1 1 |2 2 1 1
			#      1  5  6 14 20 39    | 4  1  8  6 19 | -3  -3 -19 -13 -17  -2 |3 1 1 1 0 |3 1 1 1
			#      9 15 16 21 28 34    | 6  1  5  7  6 |  8  10  10   7   8  -5 |1 2 2 1 0 |1 2 2 1
			#     15 18 21 32 35 44(2) | 3  3 11  3  9 |  6   3   5  11   7  10 |0 2 1 2 1 |2 1 2 1
			#      3 12 33 36 42 45    | 9 21  3  6  3 |-12  -6  12   4   7   1 |1 1 0 2 2 |1 1 2 2
			#      3  4 16 20 28 44(1) | 1 12  4  8 16 |  0  -8 -17 -16 -14  -1 |2 1 2 0 1 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  6,-14 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep

			#	unique	( 7:-15,19,-3, 7,-8) (10:14, 4,10)
			# -------------------------------------------------------------------------------------
			#     FV :    -17 (2)   -3 (2)   7 (3)   8 (2)   10 (3) 
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 36,38 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,0)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,0,2
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
			if( aZoid[1]%in%c(  3, 6, 4,14       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 10, 7, 5,18,19,27    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 22                ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 15,41             ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 31,33,36          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39,44             ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,10             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA, 9,14       ),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,19,21,37,23 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,33,31,29,24 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c(  3,10             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <16>
			if( fCutU.hasPtn(c(  7,16,NA,34 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(       19,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,12,19,21    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(       24,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,24,NA,44 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       31,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,14,31    ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       31,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,14,NA,39 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,5,6,4        ),c(  3, 6, 4,14       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,7,5,9        ),c( 10, 7, 5,18,19,27 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5        ),c( 22                )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,4        ),c( 15,41             )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,3,6        ),c( 31,33,36          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,4        ),c( 39,44             )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7, 1    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  8       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  8,11    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  3,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  7,14 )) )	cnt.w1<-cnt.w1+1
			#	unique	( 3: 7, 22, 3) ( 6:10, 6,NA,13) ( 8: 7,NA, 5,17, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (2)   3 (4)   6 (2)   7 (6)   8 (2)   11 (2) 
			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8 
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  7 11 17 33 44    | 1  4  6 16 11 |                        |2 2 0 1 1 |2 2 1 1
			#      7 22 24 31 34 36(1) |15  2  7  3  2 |  1  15  13  14   1  -8 |1 0 2 3 0 |1 2 3
			#      3 10 23 24 31 39(2) | 7 13  1  7  8 | -4 -12  -1  -7  -3   3 |1 1 2 2 0 |1 1 2 2
			#      5  6 13 16 27 28    | 1  7  3 11  1 |  2  -4 -10  -8  -4 -11 |2 2 2 0 0 |2 2 2
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 | -2   4   3   3   4  11 |1 3 0 2 0 |1 3 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 | 10   4   3   7   9   4 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  5      ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4, -4,  3    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  3,-10      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  2      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 3 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( 4, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -4,-10 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5,  3 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-8:-9,-22,-21, -8) (-4:-4, -8,-15, -1,-19) 
			#			( 3&4:22, 4, 3,11,14,-3) ( 3:16, 5, 3,10, 7)
			#			( 4:17, 4,-5) ( 4:10, 2,11,15, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    -8 (2)   -4 (3)   1 (2)   3 (4)   4 (4) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,3)]*c(1,3)==aFStep[c(6,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(6,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(5,2)]) )	cnt.w2<-cnt.w2+1	# 13
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# 13
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1	# 17

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
					if( (aZoid[6]-aZoid[1]) %in% c( 33,27,44 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(0,1,3)) ) return(FALSE)	# next rebind of 2,1,1
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
			if( aZoid[1]%in%c( 12, 3     ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 22,34,30      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,30      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,12,NA,27 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,22          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,20,NA,42       ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(  6,NA,28    ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c(  2, 5,NA,32 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 10,10,20,34,39 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 12,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    22,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 20,NA,42,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,3   ),c( 12, 3    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7     ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,4,0 ),c( 22,34,30 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2     ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,6   ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,0,2 ),c( 45,30    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2,11, 3 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6,18,12 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 8    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  5, 5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 2: 2,16,NA,14) ( 2:19, 2, 12, 4) ( 3: 8,10,20,NA, 3) ( 5: 5, 7,21)
			#			( 8: 8, 2,20, 8*) (11: 8, 4,11)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (8)   3 (2)   5 (3)   8 (3)   11 (4) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aCStep[c(3,5)])==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1 12 13 21 32 45    |11  1  8 11 13 |                        |1 2 1 1 1 |1 2 1 1 1
			#      5  7  9 11 32 35(1) | 2  2  2 21  3 |  4  -5  -4 -10   0 -10 |3 1 0 2 0 |3 1 2
			#      6  8 28 33 38 39    | 2 20  5  5  1 |  1   1  19  22   6   4 |2 0 1 3 0 |2 1 3
			#      3  4  6 10 28 30(2) | 1  2  4 18  2 | -3  -4 -22 -23 -10  -9 |3 1 1 1 0 |3 1 1 1
			#     12 14 24 26 34 45    | 2 10  2  8 11 |  9  10  18  16   6  15 |0 2 2 1 1 |2 2 1 1
			#     12 17 23 34 42 45(3) | 5  6 11  8  3 |  0   3  -1   8   8   0 |0 2 1 1 2 |2 1 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( 17     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  6     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-10: -1,-4,NA,NA,-10) (-10:-12,NA,NA,-10,-18) ( 0: 2, 3,20,NA, 0)
			#			(  6:17,19,17,10, 6,26)
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (3)   -4 (2)   0 (3)   1 (2)   4 (2)   6 (2)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(5,6)]*c(1,1)==aFStep[c(4,1)] ) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 33 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,2,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,0,0
					if( all(quoSize[1:3+0]==c(0,4,0)) ) return(FALSE)	# next rebind of 2,2,0
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
			if( aZoid[4]%in%c(  9,15 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 38    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,25,22,19,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			# <11>
			# <14>
			if( fCutU.hasPtn(c( 14,37,40 ),aZoid) ) cnt<-cnt+1
			# <16>
			# <18>
			if( fCutU.hasPtn(c(  4, 5,18,NA,32,36 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 10,NA,NA,NA,28 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 21,22,NA,38 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(          18,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 23,23,21,NA,35,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 8,0     ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5       ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7       ),c(  9,15 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1       ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,9     ),c( 38    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 2, 3  ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(           ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1, 5, 3  ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 13        ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  9, 2, 1  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique	( 1: 9, 1,15, 9) ( 2: 2, 2,19) ( 3:15,NA, 3,17) ( 7:13,25,NA, 7) (12: 7, 1,NA,12)
			#	unique	( 1,11) ( 1, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (3)   3 (2)   4 (2)   7 (4)   8 (3)   11 (2)   12 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(4,1,6)==aCStep[c(1,2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(2,3)]*c(4,4)==aCStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  7 11 16 41 45    | 2  4  5 25  4 |                        |2 2 0 0 2 |2 2 2
			#      8  9 18 21 28 40    | 1  9  3  7 12 |  3   2   7   5 -13  -5 |2 1 2 0 1 |2 1 2 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  6   6  -2  -4  10   5 |0 4 0 1 1 |4 1 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 |-11  -5   7   7  -7  -6 |1 1 2 2 0 |1 1 2 2
			#      6  7 18 19 30 38    | 1 11  1 11  8 |  3  -3  -5  -5  -1  -1 |2 2 0 2 0 |2 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 | -5   2  -7  -5  -4 -10 |2 2 2 0 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  5       ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1       ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -5, 7,-6 ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -5, 6    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -3       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -7,-4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -5,-3 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	(-7:-17,-3,-7-4) (-4:-2,-20,-8,-4) ( 2:-13, 2,-21,-15, 5,-15) ( 3: 3,-8,-17,-15,11, 3)
			#			(-5:-5, 9,-13,-9) (-5:-5, 5,-13) (-5: 1,-11,-5,-3,-19) (-5:-13, 7,-9,-5,-7,-19)
			#			( 5:-7,-10,13, 5) ( 7:-12, 7, 9,-1,-7) ( 7:-13,12, 7,-19, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -5 (6)   -4 (2)   -1 (2)   2 (2)   3 (2)   5 (2)   6 (2)   7 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c( 1, 2)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 2 ]*c(-2,-5)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,2)]*c( 1,-2)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1	# -12
			if( sum(aFStep[c(4,3)])==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1	# -12

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
					if( (aZoid[6]-aZoid[1]) %in% c( 41 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,0)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+0]==c(0,0,2)) ) return(FALSE)	# next rebind of 2,0,0
					if( all(quoSize[1:3+1]==c(2,0,0)) ) return(FALSE)	# next rebind of 0,0,2
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
			if( aZoid[2]%in%c(  4    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 26    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# < 6>
			# <12>
			# <13>
			if( fCutU.hasPtn(c( 13,NA,26 ),aZoid) ) cnt<-cnt+1
			# <19>
			# <25>
			if( fCutU.hasPtn(c( 14,18,NA,25 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c( 13,NA,26 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 31,36       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 31,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 24,33,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 31,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 3,4,0,5,9 ),c(  4 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,6,4     ),c( 26 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,2       ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0         ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,4       ),c( 45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 6    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3,10    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  9       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 1,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 3, 1 )) )	cnt.w1<-cnt.w1+1
			#	unique	( 1: 1,NA,10,20, 4) ( 5&7: 1, 5, 7, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (2)   3 (3)   4 (3)   5 (2)   7 (4)   12 (2)   26 (2) 
			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8 
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15


			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 18 31 34 38 45    |12 13  3  4  7 |                        |1 1 0 3 1 |1 1 3 1
			#      4  6 15 25 26 33(1) | 2  9 10  1  7 | -2 -12 -16  -9 -12 -12 |2 1 2 1 0 |2 1 2 1
			#      2  6  7 12 19 45(1) | 4  1  5  7 26 | -2   0  -8 -13  -7  12 |3 2 0 0 1 |3 2 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  1   3   5   1   6  -2 |2 2 1 0 1 |2 2 1 1
			#      4  5 31 35 43 45(1) | 1 26  4  8  2 |  1  -4  19  22  18   2 |2 0 0 2 2 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   9 -12  -9  -3  -2 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  9     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( 12     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique	(-12:20,-12,-2, 3, 8) (-9:20,NA,-8,-9, 6, 8) (-2:17,15,NA,-19,-12, -2)
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (4)   -9 (2)   -2 (4)   1 (3)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c( 1,-1)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 5 ]*c( 4, 3)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(6,5)]*c(6,3)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,2)]) )	cnt.w2<-cnt.w2+1

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

flagScoreMtx2		<- function( ccObjLst ,allIdxF ){

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

} # flagScoreMtx2()

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
		aBin <- aZoid %%  2
		aRem <- aZoid %% 10

		#	a,b,b.. a?	nextRebNum nextFStepBin nextColVal_2 nextColVal_5
			if( 2<sum(aZoid[c( 1,1,1,2,3,4 )]==c(  7, 8, 6,14,12,31 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aZoid[c( 1,1,1,2,3,4 )]==c( 13, 3, 5,10,13,26 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b.. b?	basic nextColVal_1 nextColVal_3
			if( 2<sum(aZoid[c( 6,2,3,4 )]==c( 42, 8,31,32 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. q? nextBin nextFStepBin nextColVal_1 nextColVal_2 nextColVal_6
			if( 2<sum(aZoid[c( 1,2,6,2,6,2 )]==c(  1,24,45,17,41,12 )) ){	surviveFlg[idx]<-FALSE	;next }

		#	bin pattern
		if( 3<=sum(aBin[ 2:4 ]==c(  0, 0, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextZW
		if( 3<=sum(aBin[ 1:6 ]==c(  1, 0, 1, 0, 0, 0 )) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_5

		# fCutCnt.basic()
			#     14 26 32 36 39 42    |12  6  4  3  3 |                        |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			if( aZoid[6] %in% c(42,45) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c(12,16,26,28,30,42)[c(6,4,3)]==aZoid[c(6,5,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c(12,16,26,28,30,42)[c(5,6)]==aZoid[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(aRem[c(1,2)]==aRem[c(6,3)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			#     11 24 32 33 35 40    |13  8  1  2  5 |                        |0 1 1 3 1 |1 1 3 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  0 -12  -3   0   3   2 |0 2 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  0   4 -11 -14 -14  -3 |0 4 1 1 0 |4 1 1
			#     13 16 24 25 33 36(2) | 3  8  1  8  3 |  2   0   6   6   9  -3 |0 2 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   2   6  14   8   6 |0 2 0 2 2 |2 2 2
			#      6  8 18 35 42 43(2) | 2 10 17  7  1 | -6 -10 -12  -4   1   1 |2 1 0 1 2 |2 1 1 2	
			if( 2<=sum(c( 6, 8,18,35,42,43)[c(1)]==aZoid[c(1)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 6, 8,18,35,42,43)[c(1,4)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 6, 8,18,35,42,43)[c(2,5)]==aZoid[c(2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 6, 8,18,35,42,43)[c(2,6)]==aZoid[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( 1<sum( aZoid[ 1 ]*c(3,7)==aZoid[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum(aZoid[c(5,6)]==c(43,44)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  3   3   7  -9  -8  -8 |1 2 1 2 0 |1 2 1 2
			#     17 25 28 37 43 44    | 8  3  9  6  1 |  9  13   9  16  12   9 |0 1 2 1 2 |1 2 1 2
			#     14 15 25 28 29 30(2) | 1 10  3  1  1 | -3 -10  -3  -9 -14 -14 |0 2 3 1 0 |2 3 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -7  -6  -1   1   5   8 |2 0 2 2 0 |2 2 2
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -6   7   5   4   6   7 |1 1 1 1 2 |1 1 1 1 2
			if( 2<=sum(c( 1,16,29,33,40,45)[c(3,5,4)]==aZoid[c(2,4,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 1,16,29,33,40,45)[c(2,3)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextBin
			#     26 27 28 42 43 45    | 1  1 14  1  2 |                        |0 0 3 0 3 |3 3
			#      1  2  6 16 20 33    | 1  4 10  4 13 |-25 -25 -22 -26 -23 -12 |3 1 1 1 0 |3 1 1 1
			#      1 23 28 30 34 35(1) |22  5  2  4  1 |  0  21  22  14  14   2 |1 0 2 3 0 |1 2 3
			#     12 24 33 38 40 42    |12  9  5  2  2 | 11   1   5   8   6   7 |0 1 1 2 2 |1 1 2 2
			#      1  5 27 30 34 36    | 4 22  3  4  2 |-11 -19  -6  -8  -6  -6 |2 0 1 3 0 |2 1 3
			#     12 17 23 34 42 45(1) | 5  6 11  8  3 | 11  12  -4   4   8   9 |0 2 1 1 2 |2 1 1 2
			if( 2<=sum(c(12,17,23,34,42,45)[c(1,5)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextRebNum
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2  8 13 25 28 37(3) | 6  5 12  3  9 |  0   0  -2   3   3  -4 |2 1 2 1 0 |2 1 2 1
			#      5 13 17 23 28 36(2) | 8  4  6  5  8 |  3   5   4  -2   0  -1 |1 2 2 1 0 |1 2 2 1
			#      7  8 10 19 21 31    | 1  2  9  2 10 |  2  -5  -7  -4  -7  -5 |2 2 1 1 0 |2 2 1 1
			#     13 16 24 25 33 36    | 3  8  1  8  3 |  6   8  14   6  12   5 |0 2 2 2 0 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  0  -2  -5   1   7   7 |0 3 1 0 2 |3 1 2
			if( 2<=sum(c(13,14,19,26,40,43)[c(1)]==aZoid[c(1)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c(13,14,19,26,40,43)[c(3,5)]==aZoid[c(2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c(13,14,19,26,40,43)[c(1,2,5)]==aZoid[c(1,2,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3

		# fCutCnt.nextCStepBin
			#     16 17 23 24 29 44    | 1  6  1  5 15 |                        |0 2 3 0 1 |2 3 1
			#      6  7 12 28 38 40    | 1  5 16 10  2 |-10 -10 -11   4   9  -4 |2 1 1 1 1 |2 1 1 1 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 | -3   4   2 -13  -6  -4 |1 3 0 2 0 |1 3 2
			#      4  5  8 16 21 29    | 1  3  8  5  8 |  1  -6  -6   1 -11  -7 |3 1 2 0 0 |3 1 2
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7  11  10   3   3  10 |0 4 1 1 0 |4 1 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  1   1   5  15  18   6 |0 2 1 1 2 |2 1 1 2
			if( 1<sum(aZoid[c(1,2)]==c(13,18)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#      4  5  7 18 20 25    | 1  2 11  2  5 |                        |3 1 2 0 0 |3 1 2
			#      4  8 11 18 37 45(2) | 4  3  7 19  8 |  0   3   4   0  17  20 |2 2 0 1 1 |2 2 1 1
			#      8 24 28 35 38 40(1) |16  4  7  3  2 |  4  16  17  17   1  -5 |1 0 2 2 1 |1 2 2 1
			#      8 10 23 24 35 43(3) | 2 13  1 11  8 |  0 -14  -5 -11  -3   3 |1 1 2 1 1 |1 1 2 1 1
			#      3 11 13 21 33 37    | 8  2  8 12  4 | -5   1 -10  -3  -2  -6 |1 2 1 2 0 |1 2 1 2
			#      3 10 14 16 36 38(1) | 7  4  2 20  2 |  0  -1   1  -5   3   1 |1 3 0 2 0 |1 3 2
			if( 2<=sum(c( 3,10,14,16,36,38)[c(2,1)]==aZoid[c(1,1)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 3,10,14,16,36,38)[c(1,4)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 3,10,14,16,36,38)[c(1,2,4)]==aZoid[c(1,4,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3

		# fCutCnt.nextColVal_1
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  4   0  12  16   9  -5 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
			if( 2<=sum(c( 6, 8,18,35,42,43)[c(6,4,2)]==aZoid[c(5,3,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextColVal_2
			#     13 19 28 37 38 43    | 6  9  9  1  5 |                        |0 2 1 2 1 |2 1 2 1
			#      5 10 13 27 37 41(2) | 5  3 14 10  4 | -8  -9 -15 -10  -1  -2 |1 2 1 1 1 |1 2 1 1 1
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -3   7   6  -3   0   0 |1 2 1 1 1 |1 2 1 1 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  4   1  12  10   1   4 |1 1 0 3 1 |1 1 3 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -1  -7 -19  -5  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  0   7  18  12  10   1 |1 1 0 1 3 |1 1 1 3
			if( all(c( 5,18,30,41,43,45)[c(1,4)]==aZoid[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 5,18,30,41,43,45)[c(5,6)]==aZoid[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( 1<sum(aZoid[c(2,6)]==c(17,41)) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum(aZoid[c(2,6)]==c(11,44)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			#     16 21 26 31 36 43    | 5  5  5  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 | -9  16  12   8   4   1 |1 0 0 3 2 |1 3 2
			#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  0 -28 -28 -26  -9  -9 |2 2 0 2 0 |2 2 2
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
			#     10 14 16 18 27 28(2) | 4  2  2  9  1 |  9   7   0   0  -7 -10 |0 4 2 0 0 |4 2
			#      5 22 31 32 39 45    |17  9  1  7  6 | -5   8  15  14  12  17 |1 0 1 3 1 |1 1 3 1
			if( 2<=sum(c( 5,22,31,32,39,45)[c(1,1)]==aZoid[c(1,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 5,22,31,32,39,45)[c(3,4)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(aRem[c(1,2)]==aRem[c(6,4)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum(aZoid[1:2+2]==c(31,32)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#      7 15 20 25 33 43    | 8  5  5  8 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#     10 14 16 18 27 28    | 4  2  2  9  1 |  3  -1  -4  -7  -6 -15 |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 |  4   2   3  16   7  10 |1 2 0 3 0 |1 2 3
			if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(c( 5,11,14,30,33,38)[c(3,4)]==aZoid[c(2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2 (*2)

		# fCutCnt.nextColVal_5
			#      2 19 25 26 27 43    |17  6  1  1 16 |                        |1 1 3 0 1 |1 1 3 1
			#      3 10 14 16 36 38    | 7  4  2 20  2 |  1  -9 -11 -10   9  -5 |1 3 0 2 0 |1 3 2
			#     10 14 16 18 27 28(3) | 4  2  2  9  1 |  7   4   2   2  -9 -10 |0 4 2 0 0 |4 2
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -8  -4  -4  13   6  14 |1 2 0 2 1 |1 2 2 1
			#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   1  -5   1  -4 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -2   0   0   0  -2  -2 |1 2 1 2 0 |1 2 1 2
			if( 2<=sum(c( 1,10,13,26,32,36)[c(2,1)]==aZoid[c(2,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 1,10,13,26,32,36)[c(2,3,4)]==aZoid[c(1,2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3
			if( all(c( 1,10,13,26,32,36)[c(2,3,4)]==aZoid[c(2,3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 3
			if( fCutU.hasPtn(c( 10,14,26 ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }	# 10 14 26, 10 13 26
			if( fCutU.hasPtn(c( 10,13,26 ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }	# 10 14 26, 10 13 26
			if( fCutU.hasPtn(c( 10,12,26 ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }	# 10 14 26, 10 13 26
			if( 1<sum(aZoid[1:3+1]==c(10,12,31)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			#      2  7 19 25 29 36    | 5 12  6  4  7 |                        |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 13  13  12  -2  -8  -1 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			if( 2<=sum(c( 7, 8,13,15,33,45)[c(5,4)]==aZoid[c(6,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(aRem[c(3,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }

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
		if( 1<sum(aZoid==c( 17,NA,15,32,NA,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 12,12,25,32,40,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  3,NA,21,37,25,34 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 11,NA,20,NA,32,21 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		if( aRem[4]==c(6) ){	surviveFlg[idx]<-FALSE	;next }		# if( fCutU.remFilt(aZoid[4],c( 6,8,9  ),c( 16 )) )

		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  2,25,12,16, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 20,17, 2, 9, 5 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  3, 6, 7, 5, 2 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 12, 3, 7, 7,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  1, 4,14, 5,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  1, 4,13, 7,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  3,NA, 1, 3,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  6,NA,16, 3,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }

		if( fCutU.hasPtn(c(1, 4,15, 3),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(1, 4,15, 3),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		if( 1<sum( aCStep[ 1 ]*c(6,8)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(1,5)]*c(6,5)==aCStep[c(3,2)] ) ){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
    	# [  1] 13 15 40                16 18 27    30 33 35
		if( all(aZoid[1:3+0]==c( 13, 15, 40 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+2]==c( 16, 18, 27 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+3]==c( 30, 33, 35 )) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,b,b,..a
		if( 2<sum(aCStep[c(1,1,4,5,5,2,3,5)]==c(1,6,7,3,10,3,13,4)) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b,..b
		if( 2<sum(aCStep[c(1,1,4,5,5,2,3,5)]==c(5,2,2,1,3,2,19,9)) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b,..b
		if( 2<sum(aCStep[c(1,3,4,5)]==c(1,1,2,3)) ){	surviveFlg[idx]<-FALSE	;next }	# nextColVal_6 nextColVal_3 nextColVal_2 nextBin

		# fCutCnt.basic()
			if( fCutU.hasPtn(c( 2, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }	# unique 2,2
			if( fCutU.hasPtn(c( 18,20, 4,11, 7 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[ 3 ]*c(2,5,1,6)==aCStep[c(1,2,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[ 4 ]*c(2,5,1,6)==aCStep[c(1,2,3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum(aCStep[1:2+1]==c(  8, 1 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( 1<sum(aCStep[1:2+3]==c(  2, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,25,12, 1 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,NA,NA, 5,26 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 1,14, 1 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			if( fCutU.hasPtn(c(  2,20, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 7, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,NA, 9, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15,NA, 3,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( fCutU.hasPtn(c(  1, 7, 6, 7,24 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 6, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,20 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 22, 1, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c(4,2) ,c(5,6) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( fCutU.hasPtn(c(  1, 2,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,13,20, 3 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 7,21, 3 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 7,25 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum(aCStep[1:2+3]==c(  1, 3 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique 1,3
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( 1<sum(aCStep[1:2+2]==c( 1, 3 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique 1,3
			if( fCutU.hasPtn(c(  1, 5,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 7,22 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 3, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,21,11 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 1,NA, 5,22 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 6,21,11 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,19, 8, 1 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 4,17, 8 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,NA, 1, 5,15 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( 1<sum( aCStep[ 5 ]*c(2,1,10)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,20, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 5, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( fCutU.hasPtn(c( 28,11, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 5,29, 9 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,13, 6, 5, 4 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,13, 6, 5, 4 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 5, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 4,NA, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 4,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			if( 1<sum( aCStep[ 5 ]*c(6,1)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 1,29 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21, 3,NA, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,22, 7,17 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,20,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 9, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			if( 1<sum(aCStep[1:2+1]==c( 7,6 )) ){	surviveFlg[idx]<-FALSE	;next }	# unique
			if( fCutU.hasPtn(c(  1, 1, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA,2, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 29,13,NA, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 5, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 3,20, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 8, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA,27, 4, 5 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA, 4,19 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,3,5)]==c( 3,13, 4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,3,5)]==c( 2,19, 9)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,3,4)]==c(10,13,26)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 2,NA,NA,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 2,29, 3 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 3,13, 4, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 6, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 5,24,NA, 6 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

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
		if( 2<=sum(aZoid[c(6,3)]==c( 39, 6)) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,c..q

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[c(2,4)]*c(2,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14,-13,11 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( -13, -9,12 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 19,-2,-5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c( 14,-4,-14,-2,-1,0 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }		# 
			if( fCutU.hasPtn(c(  8, 8,-10,-18 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }		# 

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(1,5)]*c(2,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(4,5)]*c(3,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -12, -5, 2,-1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,-4,23,26,29, -3 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4, 4, 6,NA,10,15 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2,-2, 6,NA,19 ),aFStep,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(-1, 1)==aFStep[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -9,NA,-13, -9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -6,15, 9, 3, 4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11,13, 5, 0 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 9, 7 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -15, 7,19,16,20 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 4 ]*c(3,-1,2)==aFStep[c(2,3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(3,4)]*c(-3, 2)==aFStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,-9, 3, 8,12 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,23,-13, 0, 10, 11 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( NA,NA,-6,NA,-6,-6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( fCutU.hasPtn(c( -6,-5, 9,18,21 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 3,-5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,-13,-17,-4 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -4, -2,-10,  3 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 7,12 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(1,3,6)]*c(1,3,3)==aFStep[c(2,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-6, 6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,18,15,NA,NA, -4 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 8,16,29 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,13,17 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-1,1)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5, 5,12 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 7,-9,-3,-7 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  0,12, 7, 1, 9, -2 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 1, 0, 9, 4 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 3,13,-8, 3 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA,17,-18 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA,17,14 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(1,-1)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,-6,NA,NA,21 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 4,-3, 0 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,13, 8, 1,-23 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( all(aFStep[1:3+3]==c(-19, -5, -5)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -6,NA, 5, 0, -1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA,12,10, 1 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA,12,10, 1 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA,12,10, 1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 7,NA,NA,20, 2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( fCutU.hasPtn(c(  0,14, 8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(2,8,5)==aFStep[c(1,4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,2)]*c(4,5)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 2, 3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,-1, 4,29 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12,13,-7,-1,-10 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -2   0   0   0  -2  -2 |1 2 1 2 0 |1 2 1 2
			#	unique	3개 같은 값이 또 나올까?
			if( any(3==table(aFStep)) ) {	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 1 ]*c(1,1)==aFStep[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 2 ]*c(1,1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,13,-7,-10 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-9,-9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,10,-7,-5 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2, 6,-4 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5, 0,-1, 5,-5, 0 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 0,-1, 1,-5 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 0,-5, 1 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 9,13, 0,-7,-3 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, -1,13, 2 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-4,-6)==aFStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4, 7,23,NA, -1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-4, 1,-3, 1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15,13,25 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

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

		# fCutCnt.nextColVal_1
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  4   0  12  16   9  -5 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
			#	unique	반복이 나올까? 2 2 1 0 1에서 반복 나왔음.
			if( all(quoSize==c(2, 1, 0, 1, 2)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			#     13 19 28 37 38 43    | 6  9  9  1  5 |                        |0 2 1 2 1 |2 1 2 1
			#      5 10 13 27 37 41(2) | 5  3 14 10  4 | -8  -9 -15 -10  -1  -2 |1 2 1 1 1 |1 2 1 1 1
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -3   7   6  -3   0   0 |1 2 1 1 1 |1 2 1 1 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  4   1  12  10   1   4 |1 1 0 3 1 |1 1 3 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -1  -7 -19  -5  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  0   7  18  12  10   1 |1 1 0 1 3 |1 1 1 3
			#	unique	1 2 1 1 1, 0 2 1 2 1
			#			1 1 0 1 3 이 반복될까?
			if( all(quoSize==c( 1, 2, 1, 1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(quoSize==c( 0, 2, 1, 2, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(quoSize==c( 1, 1, 0, 1, 3 )) ){	surviveFlg[idx]<-FALSE	;next }
					
		# fCutCnt.nextColVal_3
			#     16 21 26 31 36 43    | 5  5  5  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 | -9  16  12   8   4   1 |1 0 0 3 2 |1 3 2
			#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  0 -28 -28 -26  -9  -9 |2 2 0 2 0 |2 2 2
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
			#     10 14 16 18 27 28(2) | 4  2  2  9  1 |  9   7   0   0  -7 -10 |0 4 2 0 0 |4 2
			#      5 22 31 32 39 45    |17  9  1  7  6 | -5   8  15  14  12  17 |1 0 1 3 1 |1 1 3 1
			#	unique 1 0 1 3 1 재발 가능? (2 2 0 2 0)
			if( all(quoSize==c(1, 0, 1, 3, 1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			#      2 19 25 26 27 43    |17  6  1  1 16 |                        |1 1 3 0 1 |1 1 3 1
			#      3 10 14 16 36 38    | 7  4  2 20  2 |  1  -9 -11 -10   9  -5 |1 3 0 2 0 |1 3 2
			#     10 14 16 18 27 28(3) | 4  2  2  9  1 |  7   4   2   2  -9 -10 |0 4 2 0 0 |4 2
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -8  -4  -4  13   6  14 |1 2 0 2 1 |1 2 2 1
			#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   1  -5   1  -4 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -2   0   0   0  -2  -2 |1 2 1 2 0 |1 2 1 2
			#	unique	1 2 1 2 0 재발 확률은...  1 2 . 2 . 패턴도 3연속째이다. 다음도 가능?
			if( all(quoSize[c(1,2,3)]==c(1,2,2)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(quoSize[c(1,2,3)]==c(0,4,0)) ){	surviveFlg[idx]<-FALSE	;next }

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
	#	"nextRebNum( NA )","nextColVal_1(  )","nextColVal_5( 35,40,39 )","nextFStepBin( 36,32 )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 35,40,39, 36,32 ) )]

	# unique	nextCStepBin
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 33 ) )]

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






