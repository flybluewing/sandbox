# to20180707_H.R 최종접근

# 공용
# done.
fCut.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF	;zMtxLen <- nrow(zMtx)
	mi <- fCutU.getMtxInfo( zMtx )	# matrix info
	#	mtxLen, lastZoid, rem, quoTbl, quoSize, cStep, fStep, rawTail, cStepTail

	# col[1] 은 1~9
	flag <- gEnv$allZoidMtx[allIdxF,1]<10	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# <recycle> 2개 이상 재현은 제외시키자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid %in% mi$lastZoid) )
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

    # rem 동일값 연속  rx,rx,r3,r3,rx,rx - 잘라내기엔 좀 위험하다..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    aCode <- aZoid%%10
					return( !any(aCode[2:6]==aCode[1:5]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    # rem 재현 2 이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( mi$rem==(aZoid%%10) )
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    # rem ptn 재현 3이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){
						fnd <- fCutU.hasPtn( mi$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    # rem ptn 재현 4이상 - hIdx-1 에서.
	tgtZoidRem <- zMtx[mi$mtxLen-1,] %% 10
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
						fnd <- fCutU.hasPtn( mi$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( mi$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# quo 4개 이상은 제외(사실 기본 필터에 포함되어있다.)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 4>max(table(aZoid%/%10)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )
} # fCut.default()
# QQE working
fCutCnt.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )

	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

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

	# lastZW 가 19라서... allIdxF 상에 존재하지도 않는다..
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    # flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				return( ... )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
	# flgCnt[!flag] <- flgCnt[!flag] + 1


	return( flgCnt )

} # fCutCnt.default()

# done
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail


	# 810  5 10 13 21 39 43
	# 811  8 11 19 21 36 45
	# 812  1  3 12 14 16 43
	# 813 11 30 34 35 42 44
	# 814  2 21 28 38 42 45
	# 815 17 21 25 26 27 36
	# 동일 컬럼에 같은 값 재현은 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid==stdMI$lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	1,xx,2,xx,?
	flag <- !(gEnv$allZoidMtx[allIdxF,1] %in% c(3))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	30,21,21,?
	flag <- !(gEnv$allZoidMtx[allIdxF,2] %in% c(21,30))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	col 21,21,14 가 또 재발?
	flag <- !(gEnv$allZoidMtx[allIdxF,2] %in% c(14))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# 810  5  3  8 18  4
	# 811  3  8  2 15  9
	# 812  2  9  2  2 27
	# 813 19  4  1  7  2
	# 814 19  7 10  4  3
	# 815  4  4  1  1  9
	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( 1<sum(aCStep==1) ) return(FALSE)
					if( 1<sum(aCStep==4) ) return(FALSE)
					if( any(aCStep[2:5]==aCStep[1:4]) ) return(FALSE)
					if( sum(aCStep[c(1,2,3)])==aCStep[5] ) return( FALSE )
					if( sum(aCStep[c(1,2,4)])==aCStep[5] ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# QuoTbl : 이전 패턴 반복은 제외.(H814 tbl)
	# 1Quo가 17 하나 뿐이거나, 3Quo가 36 하나 뿐인 것 제외.
	banQuo <- fCutU.getQuoObj( gEnv$zhF["814",] )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoVal <- aZoid%/%10
					aQuoTbl <- table(aQuoVal)
					if( banQuo$sameTbl(aQuoTbl) ) return( FALSE )

					quoSeg <- aZoid[aQuoVal==1]
					if( 1==length(quoSeg) && quoSeg==17 ) return( FALSE )
					quoSeg <- aZoid[aQuoVal==3]
					if( 1==length(quoSeg) && quoSeg==36 ) return( FALSE )

					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# 현재의 특이한(1,4,1) Quo 마지막 재발 패턴이 반복되긴 어렵겠지.(딱 한번 있긴 했다..)
	stdQuoLst <- apply( gEnv$zhF ,1 ,fCutU.getQuoObj)
	quoMatFlag <- sapply( stdQuoLst ,function(quo){
						stdMI$quo10$sameTbl( quo$tbl )
					})
	quoMatIdx <- which(quoMatFlag)
	lastQuoIdx <- quoMatIdx[length(quoMatIdx)-1]
	nextQuo <- stdQuoLst[[ lastQuoIdx+1 ]]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table( aZoid%/%10 )
					return( !nextQuo$sameTbl(aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	# cStep 과 fStep 관계가 있다... 둘 다 1,9로 끝났음.
	#	44,45,36(1,-9)    26,37,36(1,9)	 다만, fStep 값은 abs()로 하자.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					aFStep <- aZoid - stdMI$lastZoid
					cnt <- 0
					if( aCStep[5]==abs(aFStep[6]) ) return( FALSE )
					if( aCStep[4]==abs(-9) ) return( FALSE )
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	allIdxF <- allIdxF[flag]



	return( allIdxF )

} # fCut.basic()

#
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

	flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
				aRem <- aZoid%%10
				cnt <- 0
				if( aRem[1]%in%c(3  ) ) cnt <- cnt+1
				if( aRem[2]%in%c(1,0) ) cnt <- cnt+1
				if( aRem[6]%in%c(7  ) ) cnt <- cnt+1

				if( aRem[3]%in%c(0  ) ) cnt <- cnt+1
				if( aRem[4]%in%c(5  ) ) cnt <- cnt+1
				if( aRem[6]%in%c(6  ) ) cnt <- cnt+1

				if( aRem[1]==aRem[5] ) cnt <- cnt+1
				if( aRem[4]==aRem[6] ) cnt <- cnt+1
				return( cnt<2 )
			})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 810  5  3  8 18  4
	# 811  3  8  2 15  9
	# 812  2  9  2  2 27
	# 813 19  4  1  7  2
	# 814 19  7 10  4  3
	# 815  4  4  1  1  9
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[1]%in%c( 4   ) ) cnt <- cnt+1
					if( aCStep[2]%in%c( 7   ) ) cnt <- cnt+1
					if( aCStep[3]%in%c(10   ) ) cnt <- cnt+1
					if( 1<sum(aCStep==c(4,4,1,1,9)) ) cnt <- cnt+1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# fStep :  15   0  -3 -12 -15  -9  ( gEnv$zhF[815,] - gEnv$zhF[814,] )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid - stdMI$lastZoid
					cnt <- 0
					if( aFStep[1]==(-aFStep[5]) ) cnt <- cnt + 1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.basic()

# done
fCutCnt.colValSeqNext <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					cnt <- 0
					if( 1<sum(aRem[banVal.idx]==c(4,3,1,5,5)) ) cnt <- cnt + 1
					return( 1>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !( aZoid[5]%in%c(35) ) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
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
	# <remove>
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
	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 3>sum(aCStep==c(1,1,4,3,13)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional rem과 val의 조건이 겹치기 때문에 한 군데에서 처리.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){

				# rem : 한 개 그룹에서 완전 일치는 아니지만, 전체 일치 수가 2이상
					aRem <- aZoid %% 10
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aRem[cIdx+0:1]==banCodeLst[[cIdx]])
					}
					if( cnt>2 ) return(FALSE)

				# fndMtx 내에서 일치가 1개 이상.
					cnt <- 0
					for( cIdx in banCode.span ){
						fndMtx <- cvSeqNextLst[[cIdx]]$fndMtx
						for( rIdx in 1:nrow(fndMtx) ){
							if( all(aZoid[cIdx+0:1]==fndMtx[rIdx,]) ){
								cnt <- cnt+1
							}
						}
					}
					if( cnt>0 ) return( FALSE )

				# 한 개 그룹에서 완전 일치는 아니지만, 전체 일치 수가 2이상
					cnt <- 0
					for( cIdx in banCode.span ){
						fndVal <- cvSeqNextLst[[cIdx]]$fndMtx[1,]
						if( all(aZoid[cIdx+0:1]==fndVal) ) next

						cnt <- cnt + sum(aZoid[cIdx+0:1]==fndVal)
					}
					if( cnt>1 ) return( FALSE )

					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid %% 10
					if( aRem[1]%in%c(0  ) ) cnt <- cnt+1	#
					if( aRem[2]%in%c(6,2) ) cnt <- cnt+1
					if( aRem[2]%in%c(8  ) ) cnt <- cnt+1	#
					if( aRem[3]%in%c(9  ) ) cnt <- cnt+1
					if( aRem[3]%in%c(4  ) ) cnt <- cnt+1	#
					if( aRem[4]%in%c(6  ) ) cnt <- cnt+1	#
					if( aRem[5]%in%c(3,6) ) cnt <- cnt+1
					if( aRem[5]%in%c(2,3,5,8) ) cnt <- cnt+1	#
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	flag <- !(gEnv$allZoidMtx[allIdxF,2]%in%c( 8))	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	flag <- !(gEnv$allZoidMtx[allIdxF,3]%in%c(14))	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	flag <- !(gEnv$allZoidMtx[allIdxF,5]%in%c(20,26,38))	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	#  1   4   2   3   1  13  13   5   4 
	#  1   2   7  12   6   2   9  12   2 
	#  4   1   3  16   9   7   3   1   7   5   3 
	#  3   5   1  12   5  13   6   2   2   2   3   5   6  12  11   3   9 
	# 13   1   3   2   3 
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 2>sum(aCStep==c(1,1,4,3,13)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[1]%in%c(1,5) ) cnt <- cnt+1
					if( aCStep[4]%in%c(4  ) ) cnt <- cnt+1
					if( all(aCStep[c(3,5)]==c(4,13)) ) cnt <- cnt+1
					return( cnt<1 )
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
					aRem <- aZoid %% 10
					for( cIdx in banCode.span ){
						if( 1<sum(aRem[cIdx+0:2]==banCodeLst[[cIdx]]) ) return(FALSE)
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aZoid[cIdx+0:2]==cvSeqNextLst[[cIdx]]$fndMtx[1,])
					}
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					for( cIdx in banCode.span ){
						remFlag <- 1 < sum(aRem[cIdx+0:2]==banCodeLst[[cIdx]])
						valFlag <- 0 < sum(aZoid[cIdx+0:2]==cvSeqNextLst[[cIdx]]$fndMtx[1,])
						
						if( remFlag && valFlag ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
}

# done
fCutCnt.nextZW <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail
	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(stdMI$lastZoid%in%aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoTbl <- table(aZoid%/%10)
					return( !stdMI$quo10$sameTbl(quoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
		# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# 37,39 패턴이 2번이나 나왔다.
					return( !fCutU.hasPtn(c(37,39),aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c(17,20) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(20,28) ) cnt <- cnt + 1	# * 28
					if( aZoid[5]%in%c(37   ) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(37,22) ) cnt <- cnt + 1	# * 37
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(9    ) ) cnt <- cnt + 1 # * 9
					if( aRem[2]%in%c(7,0,1) ) cnt <- cnt + 1
					if( aRem[3]%in%c(9    ) ) cnt <- cnt + 1 # * 9
					if( aRem[4]%in%c(0    ) ) cnt <- cnt + 1
					if( aRem[5]%in%c(7    ) ) cnt <- cnt + 1 # * 7
					if( 1<sum(aRem==0) ) cnt <- cnt + 1
					# 기타 고려사항 : rem에서의 순차적 증가가 많이 나타났다.(390,408)
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(1) ) cnt <- cnt + 1	# *1
					if( aCStep[2]%in%c(11) ) cnt <- cnt + 1	# *1
					if( aCStep[4]%in%c(2) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(7) ) cnt <- cnt + 1	# *7
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( !all(aCStep[c(1,2)]==c(1,11)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.nextZW()

# done
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail
	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(stdMI$lastZoid%in%aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoTbl <- table(aZoid%/%10)
					return( !stdMI$quo10$sameTbl(quoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c( 2,15) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(41   ) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(45,34,33) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(42,43),aZoid) ) cnt <- cnt+1
					if( fCutU.hasPtn(c(37,40),aZoid) ) cnt <- cnt+1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(5,1,3) ) cnt <- cnt + 1
					if( aRem[2]%in%c(8   ) ) cnt <- cnt + 1
					if( aRem[3]%in%c(0,2 ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(1,3 ) ) cnt <- cnt + 1
					if( aRem[5]%in%c(1   ) ) cnt <- cnt + 1
					if( aRem[6]%in%c(5,1,4) ) cnt <- cnt + 1
					# aRem[3:4] == c(0,1)
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(10) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 5,10, 3) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 11) ) cnt <- cnt + 1
					if( aCStep[5]%in%c( 8) ) cnt <- cnt + 1
					if( 6==(aCStep[3]-aCStep[2]) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					idx <- which(aZoid[1:5]==26)
					if( length(idx)<1 ) return( TRUE )

					return( (aZoid[idx+1]%%10)!=7 )	# *26,37
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# 141  4 17  2 11  1
	# 235  1  4  1  4  6
	# 373 11 11  5  1  2
	# 724  6 25  2  2  4
	# 731  5  6 12 17  3
	# 767 10  5 11  3  8



	return( flgCnt )
} # fCutCnt.nextQuo10()

# done
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(stdMI$lastZoid%in%aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoTbl <- table(aZoid%/%10)
					return( !stdMI$quo10$sameTbl(quoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:3 ){
						fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c(15   ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(26   ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(16,19),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(3,4  ) ) cnt <- cnt + 1
					if( aRem[2]%in%c(5,9  ) ) cnt <- cnt + 1
					if( aRem[3]%in%c(6,5  ) ) cnt <- cnt + 1
					if( aRem[5]%in%c(8,4,1) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(1) ) cnt <- cnt + 1
					if( aCStep[3]%in%c(5,10) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(3,3),aCStep) ) cnt <- cnt + 1
					if( all(aCStep[2:3]==c(1,15)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.nextBin()

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================
# fCutU.getNextBin
# done
fCutCnt.cust.getNextBin <- function( gEnv ,allIdxF ){ # < official >
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))

	flgCnt <- rep( 0 ,length(allIdxF) )


	# <recycle> 동일 컬럼 값 재발생
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# conditinoal Quo10 tbl 동일
	stdQuoTbl <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(stdQuoTbl)!=length(aQuoTbl) ) return(TRUE)

					return( !all(stdQuoTbl==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# conditinoal Quo10 tbl 동일 - h-1, h-3
	stdQuoTbl <- table( zMtx[nrow(zMtx)-1,] %/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(stdQuoTbl)!=length(aQuoTbl) ) return(FALSE)

					return( !all(stdQuoTbl==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 392  1  3  7  8 24 42
	# 418 11 13 15 26 28 34
	# 456  1  7 12 18 23 27
	# 538  6 10 18 31 32 34
	# 586  2  7 12 15 21 34
	# 592  2  5  6 13 28 44		# 특이점이 너무 많아 불안하네...
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid%%10
					cnt <- 0
					if( aZoid[1]%in%c(6,2) ) cnt <- cnt +1
					if( aZoid[2]%in%c(7) ) cnt <- cnt +1
					if( aZoid[3]%in%c(12) ) cnt <- cnt +1
					if( aZoid[6]%in%c(44) ) cnt <- cnt +1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	hSpan <- nrow(zMtx) - 1:2
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:4 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:2] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	return(flgCnt)

} # fCutCnt.cust.getNextBin( )

#========================================================================
# fCutU.getNextRebNumPtn
#		lastZoid가 반복되지 않기 위해, rebNum이 0,0,1 패턴 다음의 hZoid들을 수집
#	done
fCutCnt.cust.getNextRebNum <- function( gEnv ,allIdxF ,numPtn=c(0,0,1) ){	# < half official >

	lastZoid.std <- gEnv$zhF[nrow(gEnv$zhF),]
	flgCnt <- rep( 0 ,length(allIdxF) )

	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))

	# <recycle> 동일 컬럼 값 재발생 (2개 이상)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2

	# <recycle> 동일 컬럼 값 재발생
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid==lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# conditinoal Quo10 tbl 동일
	stdQuoTbl <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(stdQuoTbl)!=length(aQuoTbl) ) return(TRUE)

					return( !all(stdQuoTbl==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# quoTbl 뒷부분 1,1 이 너무 오래 나왔다.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					chkSpan <- length(aQuoTbl)-1:0
					return( !all(aQuoTbl[chkSpan]==c(1,1)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# ZW
	lastZW <- lastZoid[6]-lastZoid[1]	# ... 25  34  31  24  30  38 
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aZW <- aZoid[6]-aZoid[1]
					return( !(aZW%in%c(38,29)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 동일반복 3이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(aCStep==cStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 647  5 16 21 23 24 30
	# 709 10 18 30 36 39 44
	# 722 12 14 21 30 39 43
	# 727  7  8 10 19 21 31
	# 732  2  4  5 17 27 32
	# 790  3  8 19 27 30 41
	# 2,3,4?  7+3=c+?
	flag <- !(gEnv$allZoidMtx[allIdxF,1] %in% c(4,8) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 14,8,4,8,?
	flag <- !(gEnv$allZoidMtx[allIdxF,2] %in% c(4,14) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 27,27
	flag <- !(gEnv$allZoidMtx[allIdxF,3] %in% c(27) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 27~27 ,30~30?
	flag <- !(gEnv$allZoidMtx[allIdxF,4] %in% c(30) )	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid%%10
					return( !all(aRem[1:2]==c(2,4)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid%%10
					return( !all(aRem[4:5]==c(7,7)) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2]%in%c(4  ) ) cnt <- cnt+1
					if( aRem[3]%in%c(7,1) ) cnt <- cnt+1
					if( aRem[4]%in%c(7,9) ) cnt <- cnt+1
					if( aRem[6]%in%c(2,3) ) cnt <- cnt+1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 733 13  8  1  2  5
	# 751  1 12  4  8 16
	# 768 20  2  1  8  6
	# 787  1  7  3 11  1
	# 805  9  1  5 13  1
	# 810  5  3  8 18  4
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCstep <- aZoid[2:6]-aZoid[1:5]
					if( aCstep[1]==8 ) cnt <- cnt+1
					if( aCstep[5]==4 ) cnt <- cnt+1

					if( aCstep[3]==sum(aCstep[1:2]) ) cnt <- cnt+1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.cust.getNextRebNum()

#========================================================================
# zoid[1] - 3,5,7,9
# done
fCutCnt.cust.colval1_03 <- function( gEnv ,allIdxF ){ # < official >
	cutCol.val<-3	;cutCol.col<-1
	applyFlag <- gEnv$allZoidMtx[allIdxF,1]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,1]==cutCol.val,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	# 751  3  4 16 20 28 44
	# 763  3  8 16 32 34 43
	# 784  3 10 23 24 31 39
	# 790  3  8 19 27 30 41
	# 795  3 10 13 26 34 38
	# 805  3 12 13 18 31 32

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2	# 필수이다 싶으면 2 증가시키자.
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	lastRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					return( 3>sum(aRem[2:6]==lastRem[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

    # val ptn 재현 2이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 2:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastZw <- lastZoid[6]-lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZw!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
	flag <- !(gEnv$allZoidMtx[allIdxT,2]%in% c(10	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	flag <- !(gEnv$allZoidMtx[allIdxT,3]%in% c(13 ,19))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	flag <- !(gEnv$allZoidMtx[allIdxT,6]%in% c(37    ))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# conditional zoid[3,5] qr,xx,rq 패턴 발생  13,18,31
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuo <- aZoid %/% 10
					aRem <- aZoid %% 10
					return( !( (aQuo[3]==aRem[5])&&(aRem[3]==aQuo[5]) ) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#  1,11, 2,-> 3  fStep 1	;tail(gEnv$zhF[,1])
	#	fStep 1이 한번에 3개 나오지는 않겠지.
	banVal <- lastZoid[2:6]+1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 751  1 12  4  8 16
	# 763  5  8 16  2  9
	# 784  7 13  1  7  8
	# 790  5 11  8  3 11
	# 795  7  3 13  8  4
	# 805  9  1  5 13  1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid-lastZoid	;aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[2]==aCStep[5] ) cnt <- cnt +1	# cStep 9  1  5 13  1 

					if( aCStep[1] %in% c(7    ) ) cnt <- cnt +1
					if( aCStep[1] %in% c(13,11) ) cnt <- cnt +1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	# allIdxF <- setdiff( allIdxF ,allIdxT[flgCnt>0] )
	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.cust.colval1_03( )
# done
fCutCnt.cust.colval1_05 <- function( gEnv ,allIdxF ){ # < official >
	cutCol.val<-5	;cutCol.col<-1
	applyFlag <- gEnv$allZoidMtx[allIdxF,1]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,1]==cutCol.val,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2	# 필수이다 싶으면 2 증가시키자.
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					return( 3>sum(aRem[2:6]==lastRem[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현 2이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 2:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastZw <- lastZoid[6]-lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZw!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 769  5  7 11 16 41 45
	# 772  5  6 11 14 21 41
	# 787  5  6 13 16 27 28
	# 797  5 22 31 32 39 45
	# 803  5  9 14 26 30 43
	# 810  5 10 13 21 39 43
	# 9, 10, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,2]%in% c(11	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 13,xx,14,xx,?
	flag <- !(gEnv$allZoidMtx[allIdxT,3]%in% c(15	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 16,xx,26,xx,?
	flag <- !(gEnv$allZoidMtx[allIdxT,4]%in% c(36	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 39,30,39,?
	flag <- !(gEnv$allZoidMtx[allIdxT,5]%in% c(30	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 45,43,43,?
	flag <- !(gEnv$allZoidMtx[allIdxT,5]%in% c(43,45))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[4]==6 ) cnt <- cnt +1	#16,x,16,x,26,x,?
					if( aRem[2]==0 ) cnt <- cnt +1	#39,26,13,?
					if( aRem[6]==6 ) cnt <- cnt +1	#6,31,26,39,?
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#  1,11, 2,-> 5  fStep 3	;tail(gEnv$zhF[,1])
	#	fStep 3이 한번에 3개 나오지는 않겠지.
	banVal <- lastZoid[2:6] + 3
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 769  2  4  5 25  4
	# 772  1  5  3  7 20
	# 787  1  7  3 11  1
	# 797 17  9  1  7  6
	# 803  4  5 12  4 13
	# 810  5  3  8 18  4
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid-lastZoid	;aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[3]==(aCStep[1]+aCStep[2]) ) cnt <- cnt +1	# cStep  5  3  8 18  4 
					if( aCStep[3]==(2) ) cnt <- cnt +1	# cStep  4,3,?
					if( aCStep[1]==(3) ) cnt <- cnt +1	# cStep  5~5,  3~3?
					if( aCStep[5]==(18) ) cnt <- cnt +1	# cStep  4~4,18~18?
					if( (-aFStep[2])==aFStep[3] ) cnt <- cnt +1				# fStep  0  1 -1 -5  9  0 
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	# allIdxF <- setdiff( allIdxF ,allIdxT[flgCnt>0] )
	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.cust.colval1_05( )
# done
fCutCnt.cust.colval1_07 <- function( gEnv ,allIdxF ){ # < official >
	cutCol.val<-7	;cutCol.col<-1
	applyFlag <- gEnv$allZoidMtx[allIdxF,1]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,1]==cutCol.val,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2	# 필수이다 싶으면 2 증가시키자.
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					return( 3>sum(aRem[2:6]==lastRem[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현 2이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 2:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastZw <- lastZoid[6]-lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZw!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#  1,11, 2,-> 7  fStep 5	;tail(gEnv$zhF[,1])
	#	fStep 3이 한번에 3개 나오지는 않겠지.
	banVal <- lastZoid[2:6] + 5
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 694  7 15 20 25 33 43
	# 727  7  8 10 19 21 31
	# 739  7 22 29 33 34 35
	# 747  7  9 12 14 23 28
	# 764  7 22 24 31 34 36
	# 768  7 27 29 30 38 44
	# 22,xx,22,xx, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,2]%in% c(22	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 31,30, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,4]%in% c(29	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 34,xx,34,xx, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,5]%in% c(34	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 34,xx,34,xx, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,6]%in% c(37	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2]==7 ) cnt <- cnt +1	# r5,r3,r1,r9?
					if( aRem[3]==6 ) cnt <- cnt +1	# r8,r4,r0,r6?
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 694  8  5  5  8 10
	# 727  1  2  9  2 10
	# 739 15  7  4  1  1
	# 747  2  3  2  9  5
	# 764 15  2  7  3  2
	# 768 20  2  1  8  6
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid-lastZoid	;aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[4]==(aCStep[2]+aCStep[5]) ) cnt <- cnt +1	# cStep  20  2  1  8  6
					if( aCStep[5]==(3) ) cnt <- cnt +1	# cStep  1,xx, 2,xx,?
					# if( aFStep[5]==(aFStep[3]+aFStep[4]) ) cnt <- cnt +1		# fStep  0  5  5 -1  4  8 
					# if( aFStep[2]==aFStep[3] ) cnt <- cnt +1		# fStep  0  5  5 -1  4  8 

					if( aCStep[2] %in% c(2,3) ) cnt <- cnt +1
					if( aCStep[3] %in% c(1  ) ) cnt <- cnt +1
					if( aCStep[4] %in% c(1,2) ) cnt <- cnt +1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	# allIdxF <- setdiff( allIdxF ,allIdxT[flgCnt>0] )
	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.cust.colval1_07( )
# done
fCutCnt.cust.colval1_09 <- function( gEnv ,allIdxF ){ # < official >
	cutCol.val<-9	;cutCol.col<-1
	applyFlag <- gEnv$allZoidMtx[allIdxF,1]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,1]==cutCol.val,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2	# 필수이다 싶으면 2 증가시키자.
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(aZoid[2:6]==lastZoid[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastRem <- lastZoid %% 10
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					return( 3>sum(aRem[2:6]==lastRem[2:6]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현 2이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 2:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
	lastZw <- lastZoid[6]-lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZw!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	#  1,11, 2,-> 9  fStep 7	;tail(gEnv$zhF[,1])
	#	fStep 3이 한번에 3개 나오지는 않겠지.
	banVal <- lastZoid[2:6] + 7
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[2:6]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 593  9 10 13 24 33 38
	# 622  9 15 16 21 28 34
	# 633  9 12 19 20 39 41
	# 674  9 10 14 25 27 31
	# 759  9 33 36 40 42 43
	# 766  9 30 34 35 39 41
	# 33,34, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,4]%in% c(35	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 33,34, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,5]%in% c(34	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# 40,39, ?
	flag <- !(gEnv$allZoidMtx[allIdxT,6]%in% c(38	))	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2] %in% c(3,2,4) ) cnt <- cnt +1
					if( aRem[3] %in% c(6,9) ) cnt <- cnt +1
					if( aRem[6] %in% c(3,1) ) cnt <- cnt +1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 593  1  3 11  9  5
	# 622  6  1  5  7  6
	# 633  3  7  1 19  2
	# 674  1  4 11  2  4
	# 759 24  3  4  2  1
	# 766 21  4  1  4  2	
	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aFStep <- aZoid-lastZoid	;aCStep <- aZoid[2:6]-aZoid[1:5]
					cnt <- 0
					if( aCStep[2]==aCStep[4] ) cnt <- cnt +1	# cStep 21  4  1  4  2 
					if( aFStep[2]==aFStep[5] ) cnt <- cnt +1	# fStep 0 -3 -2 -5 -3 -2
					if( aFStep[3]==aFStep[6] ) cnt <- cnt +1	# fStep 0 -3 -2 -5 -3 -2

					if( aCStep[2] %in% c(3,7) ) cnt <- cnt +1
					if( aCStep[4] %in% c(  4) ) cnt <- cnt +1
					if( aCStep[5] %in% c(4,3) ) cnt <- cnt +1
					if( all(aCStep[c(4,5)]==c(1,4)) ) cnt <- cnt +1
					if( all(aCStep[c(2,4)]==c(1,1)) ) cnt <- cnt +1
					return( cnt<2 )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1


	# allIdxF <- setdiff( allIdxF ,allIdxT[flgCnt>0] )
	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.cust.colval1_09( )

#========================================================================
# zoid[cutCol.idx] - 36(772),39(1315),44(1342)
# done
fCutCnt.colValStd <- function( gEnv ,allIdxF ,cutCol.idx ,cutCol.val ){ # < official >

	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[-cutCol.idx]==lastZoid[-cutCol.idx]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2
	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현(3) - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:4 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:2] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현(2) 2개 이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( hIdx in hSpan ){
						for( cIdx in 1:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) cnt <- cnt + 1
						}
					}
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# 
	lastZw <- lastZoid[6]-lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZw!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colValStd()

#========================================================================
# zoid width
# zWidth <- 32	# 460
# done
fCutCnt.zWidthStd <- function( gEnv ,allIdxF ,zWidth ){ # < official >

	applyFlag <- zWidth == (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[zWidth==(gEnv$zhF[,6]-gEnv$zhF[,1]), ]	# rptObj<-anaQuoTbl( zMtx )
	lastZoid <- zMtx[nrow(zMtx),]
	cStep <- lastZoid[2:6] - lastZoid[1:5]
	fStep <- lastZoid - zMtx[nrow(zMtx)-1,]
	zCodeMtx <- t(apply(zMtx,1,function(aZoid){aZoid[2:6]-aZoid[1:5]}))

	#
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[-cutCol.idx]==lastZoid[-cutCol.idx]) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 2
	#
	lastQuo <- table(lastZoid%/%10)
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aQuoTbl <- table(aZoid%/%10)
					if( length(lastQuo)!=length(aQuoTbl) ) return( TRUE )
					return( !all(lastQuo==aQuoTbl) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
	# cStep 3 이상 일치.
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					return( 3>sum(cStep==aCStep) )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현(3) - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					for( hIdx in hSpan ){
						for( cIdx in 1:4 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:2] ,aZoid )
							if( fnd ) return( FALSE )
						}
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1
    # val ptn 재현(2) 2개 이상 - hIdx-1:1 에서
	hSpan <- nrow(zMtx) - 1:1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( hIdx in hSpan ){
						for( cIdx in 1:5 ){
							fnd <- fCutU.hasPtn( zMtx[hIdx,][cIdx+0:1] ,aZoid )
							if( fnd ) cnt <- cnt + 1
						}
					}
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidthStd()



QQE binary rem 으로 fCut 추가.
	lastZoid[c(2,5)] 일치값에 대한 fCut 추가.
	cStep 3개, fStep 3개 일치값에 대한 fCut 추가.(컬럼고정)

finalFlt.cust <- function( gEnv ,allIdxF ){
	
	lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]

	fltCnt <- rep( 0 ,length(allIdxF) )

	# Quo
	zQuo <- lastZoid %/% 10
	flag <- apply( gEnv$allZoidMtx[allIdxF,]%/%10 ,1 ,function(aCode){ any(aCode!=zQuo) } )
	kIdx <- head(which(flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# zWidth : 38  37  36(?)
	flag <- !( (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]) %in% c(36,37) )
	kIdx <- head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# --------------------------------------------
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=F )
	# colPtnLst[[1]]$val  12 15 15  7  4 19  1
	# colPtnLst[[2]]$val  15  7  7 17
	# colPtnLst[[3]]$val  20  4
	# colPtnLst[[4]]$val  36 31 18
	# colPtnLst[[5]]$val  37 23 24 34 42 30 28
	# colPtnLst[[6]]$val  41 40 42 35 43 45 42 42 44 42 42

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- ifelse( aZoid[1]==12 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[2]==15 ,1 ,0 )
					cnt <- cnt + ifelse( aZoid[6]==42 ,1 ,0 )
					return( cnt>0 )
				})	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# --------------------------------------------
	# colValSeqNext()
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	#	hold point
	#		cvSeqNextLst[[1]] zoid[1:2] - 13 21 ( 8,17  21,25 )
	#		cvSeqNextLst[[2]] zoid[2:3] -  7 12 (11,20   9,27 )
	#		cvSeqNextLst[[3]] zoid[3:4] - 18 19 (18,28  15,18)
	#		cvSeqNextLst[[4]] zoid[4:5] - 18 24
	#		cvSeqNextLst[[5]] zoid[5:6] - 18 22 (33,44)
	step <- sapply( cvSeqNextLst ,function(p){ ifelse(0<nrow(p$fndMtx),p$fndMtx[,2]-p$fndMtx[,1],-1) })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( step[idx] == (aZoid[idx+1]-aZoid[idx]) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	banValLst <- lapply( cvSeqNextLst ,function(p){ if(0<nrow(p$fndMtx)) p$fndMtx[1,]%%10 else c(-1,-1)  })
    cnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(banValLst[[idx]]==(aZoid[idx:(idx+1)]%%10)) ){
							cnt <- cnt+1
						}
					}
					return( cnt )
				})	
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[1]]$fndMtx
	flag <- gEnv$allZoidMtx[allIdxF,2]!=8	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[2]]$fndMtx
	flag <- gEnv$allZoidMtx[allIdxF,2]!=20	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- gEnv$allZoidMtx[allIdxF,3]!=22	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[3]]$fndMtx
	flag <- (gEnv$allZoidMtx[allIdxF,3]%%10)!=8	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- (gEnv$allZoidMtx[allIdxF,3]%%10)!=9	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- (gEnv$allZoidMtx[allIdxF,3])!=19	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]
	flag <- apply( gEnv$allZoidMtx[allIdxF,3:4]%%10 ,1 ,function(aCode){ 
					return( !all(aCode==c(8,8)) )
				})	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst[[4]]$fndMtx
	# cvSeqNextLst[[5]]$fndMtx
	flag <- (gEnv$allZoidMtx[allIdxF,6])!=44	;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	# cvSeqNextLst -> fStep
	banValLst <- lapply( cvSeqNextLst ,function( p ){
					if( 2 > nrow(p$fndMtx) ) {
						return( c(-1,-1) )
					} else {
						return( p$fndMtx[1,] + (p$fndMtx[1,]-p$fndMtx[2,]) )
					}
				})
	#	by Pattern
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						if( all(aZoid[idx:(idx+1)] == banValLst[[idx]]) ){
							cnt <- cnt + 1
						}
					}
					return( cnt )
				})
	flag <- cnt<1 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	#	by Value
	cnt <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
					cnt <- 0
					for( idx in 1:5 ){
						cnt <- cnt + sum(aZoid[idx:(idx+1)] == banValLst[[idx]])
					}
					return( cnt )
				})
	flag <- cnt<2 ;kIdx<-head(which(!flag))
	fltCnt[!flag] <- 1+fltCnt[!flag]

	rObj <- list( fltCnt=fltCnt )
	return( rObj )
} # finalFlt.cust()

testFunc.fCut.seqRebCnt <- function( gEnv ,allIdxF ){

	testSpan <- 300:nrow(gEnv$zhF)

	rstLst <- list()
	for( hIdx in testSpan ){
		stdZoid <- gEnv$zhF[hIdx,]
		rstObj <- fCut.seqRebCnt( gEnv$zhF[1:(hIdx-1),] ,stdZoid ,pRowLen=100 ,pLen=3 )
		rstLst[[1+length(rstLst)]] <- rstObj
	}
	fnd <- sapply( rstLst ,function(p){ p$cnt })


} # testFunc.fCut.seqRebCnt









# pastBanLst 에 해당하는 allIdxF 파트를 전달받고, 또 전달해주도록 해야 한다.
# global : uAnaCutDataLst.c ,uAnaLstGrp ,pPhase
pPhase <- "colVal"	;pValCnt=FALSE
grpIdx <- 2	;uIdx <- 1
pBanLst <- initValLst( uAnaCutDataLst.c[[grpIdx-1]][[uIdx]][[pPhase]] )
pAllIdxF <- allIdxF

# pAllIdxF=tAllIdxF;pBanLst=NULL;grpIdx=1;pPhase="colVal";pLog=F;gEnv=tEnv
# grpIdx.bak=grpIdx;uIdx.bak=uIdx;pBanLst.bak=pBanLst;pAllIdxF.bak=pAllIdxF;fltPos.bak=fltPos;banMtxLst.bak=banMtxLst
bak.1.1 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)
bak.1.2 <- list(grpIdx=grpIdx,uIdx=uIdx,pBanLst=pBanLst,pAllIdxF=pAllIdxF,fltPos=fltPos,banMtxLst=banMtxLst)

Obj <- bak.1.1
grpIdx=Obj$grpIdx;uIdx=Obj$uIdx;pBanLst=Obj$pBanLst;pAllIdxF=Obj$pAllIdxF;fltPos=Obj$fltPos;banMtxLst=Obj$banMtxLst

banValScan.grp <- function( pAllIdxF ,pBanLst=NULL ,grpIdx ,pPhase="colVal" ,pLog=F ,gEnv ,pValCnt=FALSE ) {

	initValLst <- function( banLst ){
		valLst <- lapply( banLst ,function(banMtx){
							val <- sort(unique(banMtx[,"banVal"]))
							tbl <- table(banMtx[,"banVal"])[as.character(val)]
							if( !pValCnt ){
								tbl[] <- 1
							}
							return( cbind(val,tbl) )
						})
		return( valLst )
	} # initValLst()
	mergeValLst <- function( valLst ,banLst ){
		valLst.work <- initValLst( banLst )
		valLst.f <- lapply( 1:length(valLst) ,function(vIdx){
						vMtx <- valLst[[vIdx]]
						vMtx.work <- valLst.work[[vIdx]]
						dupVal <- intersect( vMtx.work[,"val"] ,vMtx[,"val"] )
						if( 0<length(dupVal) ){
							vMtx[vMtx[,"val"]%in%dupVal ,"tbl"] <- 
								vMtx[vMtx[,"val"]%in%dupVal ,"tbl"] + vMtx.work[vMtx.work[,"val"]%in%dupVal ,"tbl"]
						}
						vMtx.f <- rbind( vMtx ,vMtx.work[!vMtx.work[,"val"]%in%vMtx[,"val"] ,] )
						return(vMtx.f)
					})
		return( valLst.f )
	} # mergeValLst()

	#==========================================================================
	logIndent <- paste(rep(" ",grpIdx) ,collapse="" )

	fltPos <- rep( 0 ,length(pAllIdxF) )	;names(fltPos) <- pAllIdxF
	if( grpIdx > length(uAnaCutDataLst.c) ){
		return( list(fltPos=fltPos) )
	}

	# sapply( uAnaLstGrp[[grpIdx]] ,function(p){p$idStr} )
	for( uIdx in seq_len( length(uAnaCutDataLst.c[[grpIdx]]) ) ){

		banMtxLst <- NULL
		if( is.null(pBanLst) ){
			banMtxLst <- initValLst( uAnaCutDataLst.c[[grpIdx]][[uIdx]][[pPhase]] )
		} else {
			banMtxLst <- mergeValLst( pBanLst ,uAnaCutDataLst.c[[grpIdx]][[uIdx]][[pPhase]] )
		}

		banValLst <- lapply( banMtxLst ,function(banMtx){
							if( 0==nrow(banMtx) ){ return( integer(0) )
							} else {
								return(banMtx[banMtx[,"tbl"]>1 ,"val"])
							}
						})

		# uIdx에 해당하는 pAllIdxF 부분 추출
		flag.u <- apply( gEnv$allZoidMtx[pAllIdxF,,drop=F] ,1 ,function(aZoid){
						uAnaLstGrp[[grpIdx]][[uIdx]]$isTarget( aZoid )
					})

		flag <- apply( gEnv$allZoidMtx[pAllIdxF[flag.u],,drop=F] ,1 ,function(aZoid){
						for( idx in 1:6 ){
							if( aZoid[idx]%in%banValLst[[idx]] ){
								return( TRUE )
							}
						}
						return( FALSE )
					})

		idx.target <- which(flag.u)
		if( length(idx.target[flag])>0 ){
			fltPos[idx.target[flag]] <- grpIdx
			#	cat(sprintf("uIdx:%d %d\n",uIdx,sum(fltPos!=0)))
		}

		if( pLog ){
			k.FLogStr(sprintf("%s grpIdx:%d uIdx:%d flag.u[1]:%d fltPos[1]:%d"
						,logIndent ,grpIdx ,uIdx ,flag.u[1] ,fltPos[1])
					)
		}


		# banValScan.unit 투입 및 적용.
		#	아직 필터링 안된 대상 추출.
		idx.undone <- idx.target[!flag]
		if( 0<length(idx.undone) ){
			fltPos.next <- banValScan.grp( pAllIdxF=pAllIdxF[idx.undone] ,pBanLst=banMtxLst ,grpIdx=grpIdx+1 ,pLog=pLog ,gEnv=gEnv )$fltPos
				# pAllIdxF=pAllIdxF[idx.undone] ;pBanLst=banMtxLst ;grpIdx=grpIdx+1
			fltPos[idx.undone] <- ifelse( fltPos[idx.undone]==0 ,fltPos.next ,fltPos[idx.undone] )
		}

	}

	rObj <- list( fltPos=fltPos )
	return( rObj )

} # banValScan()




#====================================================================================
#====================================================================================
#====================================================================================
uAnaLstGrp <- getUAnaLstGrp( gEnv ,allIdxF ,pDefaultCut=FALSE ,pReport=TRUE )

# uAnaLstGrp에서 uAnaCutData 수집.
uAnaCutDataLst.c <- list()	# uAnaCutDataLst custom
for( nIdx in attributes(uAnaLstGrp)$names ){	# nIdx <- "uAnaLst.rebCnt"
	uAnaLst <- uAnaLstGrp[[nIdx]]
	cutDataLst <- list()
	for( uIdx in 1:length(uAnaLst) ){
		cutData <- list( )
		cutData$colVal		<- uAnaLst[[uIdx]]$uAnaCutData$colVal
		cutData$colVal.f	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.f
		cutData$colVal.c	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.c
		cutDataLst[[uAnaLst[[uIdx]]$idStr]] <- cutData
	} # uIdx
	uAnaCutDataLst.c[[nIdx]] <- cutDataLst
} # nIdx

# 상황에 맞게 제외조건 추가. to2018nnnn_H.R에서 정의
customizeCutData <- function( uAnaCutDataLst.c ){ 
	return(uAnaCutDataLst.c) 
} # customizeCutData()
uAnaCutDataLst.c <- customizeCutData( uAnaCutDataLst.c )

	# "uAnaLst.rawData" "uAnaLst.rebCnt"  
	# "uAnaLst.colVal1" "uAnaLst.colVal3" "uAnaLst.colVal4" "uAnaLst.colVal6" 
	# "uAnaLst.nextZW"  "uAnaLst.zw" 
