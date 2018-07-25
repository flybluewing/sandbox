# to20180707_H.R 최종접근

# 공용
# 
fCut.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF	;zMtxLen <- nrow(zMtx)	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	;rptObj<-anaMtx( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

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
    # rem 동일값 연속  rx,rx,r3,r3,rx,rx - 잘라내기엔 좀 위험하다..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    aCode <- aZoid%%10
					return( !any(aCode[2:6]==aCode[1:5]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
    # rem 재현 2 이상
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
					return( 4>max(table(aZoid%/%10)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )
} # fCut.default()
# 
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

# done
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	# rptObj<-anaMtx( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	# stdMI$rawTail				u0.zoidMtx_ana(stdMI$rawTail)
	# 동일 컬럼에 같은 값 재현은 제외.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( !any(aZoid==stdMI$lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	[same    ] 45(?), ., .,45, ., .,45     col
	flag <- !(gEnv$allZoidMtx[allIdxF,6] %in% c(45))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	[desc1   ] 19(?),18,17
	flag <- !(gEnv$allZoidMtx[allIdxF,3] %in% c(19))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	[desc(-4) ] 33(?),29,25,21
	flag <- !(gEnv$allZoidMtx[allIdxF,5] %in% c(33))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
	#	[desc(-4) ] 33(?),29,25,21 --> -10,-4,-4,-10?
	flag <- !(gEnv$allZoidMtx[allIdxF,5] %in% c(39))	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	stdQuo <- fCutU.chkRowPtnReb(stdMI$quoTail)
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj(aZoid)$size
					return( !stdQuo$filt(quoSize)$filted )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )

} # fCut.basic()

#
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	# rptObj<-anaMtx( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	fltCnt.raw <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
				cnt <- 0
				if( aZoid[2]%in%c( 3) ) cnt <- cnt+1	# rebVal19
				if( aZoid[3]%in%c(12) ) cnt <- cnt+1	# rebVal19
				if( aZoid[4]%in%c(14) ) cnt <- cnt+1	# rebVal19

				if( all(aZoid[5:6]==c(14,16)) ) cnt <- cnt+1	# rebVal36
				return( cnt )
			})	;kIdx<-anaFlagFnd(!flag,rpt)

	#	rptObj<-anaMtx( stdMI$rawTail %% 10 )
	fltCnt.rem <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
				aRem <- aZoid %% 10
				aRem.cStep <- aRem[2:6]-aRem[1:5]
				cnt <- 0
				if( aRem[1]%in%c(1,2  ) ) cnt<-cnt+1	# (?2721,?8582)
				if( aRem[2]%in%c(0,1,8) ) cnt<-cnt+1	# (?101,?11,?811)
				if( aRem[3]%in%c(6,7,9) ) cnt<-cnt+1	# (?54,?89,?87)
				if( aRem[4]%in%c(7    ) ) cnt<-cnt+1	# (?65,)
				
				if( aRem.cStep[2]%in%c(4,8,6) ) cnt<-cnt+1	# (?448 ,?78)
				if( aRem.cStep[3]%in%c(1,-8) ) cnt<-cnt+1	# (?11-8 ,)

				if( aRem[2]%in%c(1) ) cnt <- cnt+1	# (21\78\2?,)
				if( aRem[4]==aRem[6] ) cnt <- cnt+1		# *
				return( cnt )
			})	;kIdx<-anaFlagFnd(!flag,rpt)

    fltCnt.cStep <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
				aCStep <- aZoid[2:6]-aZoid[1:5]
				cnt <- 0
				if( aCStep[1]%in%c(1,4) ) cnt<-cnt+1	# (?114)
				if( aCStep[2]%in%c(4,8,10) ) cnt<-cnt+1	# (?448,11/10?)
				if( aCStep[3]%in%c(1,2) ) cnt<-cnt+1	# (?112)
				if( aCStep[4]%in%c(3  ) ) cnt<-cnt+1	# (?21)
				if( aCStep[5]%in%c(10 ) ) cnt<-cnt+1	# (?89)

				if( aCStep[3]%in%c(1) ) cnt<-cnt+1	# (129-?29)
				return( cnt )
			})	;kIdx<-anaFlagFnd(!flag,rpt)


	# fStep : -5 -3 -6  3  4  3  ( gEnv$zhF[816,] - gEnv$zhF[815,] )
    fltCnt.fStep <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
				aFStep <- aZoid - stdMI$lastZoid
				cnt <- 0
				if( aFStep[4]==aFStep[6] ) cnt<-cnt+1
				if( (-aFStep[3])==(aFStep[4]+aFStep[6]) ) cnt<-cnt+1
				return( cnt )
			})	;kIdx<-anaFlagFnd(!flag,rpt)

	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( fltCnt.raw[idx]>2 )		return( 2 )
					if( fltCnt.rem[idx]>2 )		return( 2 )
					if( fltCnt.cStep[idx]>2 )	return( 2 )
					if( fltCnt.fStep[idx]>2 )	return( 2 )

					if( fltCnt.raw[idx]==2 )	return( 1 )
					if( fltCnt.rem[idx]==2 )	return( 1 )
					if( fltCnt.cStep[idx]==2 )	return( 1 )
					if( fltCnt.fStep[idx]==2 )	return( 2 )

					if( fltCnt.raw[idx]>0 
							&& (fltCnt.rem[idx]>0 || fltCnt.cStep[idx]>0 || fltCnt.fStep[idx]>0 )
						)
					{
						return( 1 )
					}		
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( flgCnt )
} # fCutCnt.basic()

# 
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

# 
fCutCnt.colValSeqNext.cStep <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}) )
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( zMtx ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]
	#	<remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 3>sum(aCStep[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2
	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 2>sum(aCStep[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[2]%in%c( 1,11 ) ) cnt <- cnt+1
					if( aCStep[3]%in%c( 9    ) ) cnt <- cnt+1
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
					for( cIdx in banCode.span ){
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

	# =========================================================
	# colValSeqNext( ,pColSize=3 )
	cvSeqNextLst <- colValSeqNext( zMtx ,pColSize=3 )
	banCodeLst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,] else integer(0)
					})
	banCode.span <- which( sapply( banCodeLst ,function(p){length(p)>0}) )
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					for( cIdx in banCode.span ){
						cnt <- cnt + sum(aCStep[cIdx+0:2]==cvSeqNextLst[[cIdx]]$fndMtx[1,])
					}
					return( cnt<3 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					for( cIdx in banCode.span ){
						if( 1<sum(aCStep[cIdx+0:2]==banCodeLst[[cIdx]]) ) return(FALSE)
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	return( flgCnt )

} # fCutCnt.colValSeqNext.cStep()

# 
fCutCnt.nextZW <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

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
	if( 1<stdMI$mtxLen ){
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
	}
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
					if( aZoid[3]%in%c(37,17) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(20,28,34,35) ) cnt <- cnt + 1	# ** 28
					if( aZoid[5]%in%c(37,21) ) cnt <- cnt + 1	# * 37
					if( aZoid[6]%in%c(37,22) ) cnt <- cnt + 1	# * 37

					if( fCutU.hasPtn(c(37,39),aZoid) ) cnt <- cnt + 1	# *
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

# 
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

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
	if( 1<stdMI$mtxLen ){
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
	}
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

	# 141  8 12 29 31 42 43
	# 235 21 22 26 27 31 37
	# 373 15 26 37 42 43 45
	# 724  2  8 33 35 37 41
	# 731  2  7 13 25 42 45
	# 767  5 15 20 31 34 42
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c( 2,15) ) cnt <- cnt + 1 # *2
					if( aZoid[3]%in%c(32   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(43   ) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(41   ) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(45,34,33) ) cnt <- cnt + 1	# *33
					if( fCutU.hasPtn(c(41,45),aZoid) ) cnt <- cnt+1 # *
					if( fCutU.hasPtn(c(42,43),aZoid) ) cnt <- cnt+1	# *
					if( fCutU.hasPtn(c(37,40),aZoid) ) cnt <- cnt+1 # *
					# 26,r7
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

# 
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )	#	rptObj<-anaMtx( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	# 	if( fCutU.hasPtn(c(,),aXxxx) ) cnt <- cnt + 1
	# 	if( all(aXxxx[c(,)]==c(,)) ) cnt <- cnt + 1
    fltCnt.raw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[ ]%in%c(     ) ) cnt <- cnt + 1
					return( cnt )
				})	;kIdx<-anaFlagFnd(fltCnt.raw)
    fltCnt.rem <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[ ]%in%c(     ) ) cnt <- cnt + 1
					return( cnt )
				})	;kIdx<-anaFlagFnd(fltCnt.rem)
    fltCnt.cStep <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[ ]%in%c(  ) ) cnt <- cnt + 1
					return( cnt )
				})	;kIdx<-anaFlagFnd(fltCnt.cStep)

	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( fltCnt.raw[idx]>2 )		return( 2 )
					if( fltCnt.rem[idx]>2 )		return( 2 )
					if( fltCnt.cStep[idx]>2 )	return( 2 )

					if( fltCnt.raw[idx]==2 )	return( 1 )
					if( fltCnt.rem[idx]==2 )	return( 1 )
					if( fltCnt.cStep[idx]==2 )	return( 1 )

					if( fltCnt.raw[idx]>0 && (fltCnt.rem[idx]>0 || fltCnt.cStep[idx]>0 ) )		return( 1 )
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( flgCnt )
} # fCutCnt.nextBin()

# 
fCutCnt.nextRebNum <- function( gEnv ,allIdxF ,numPtn=c(0,0,1,1) ,rpt=FALSE ){	# < half official >

	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

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
	if( 1<stdMI$mtxLen ){
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
	}
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

	# 605  1  2  7  9 10 38
	# 632 15 18 21 32 35 44
	# 648 13 19 28 37 38 43
	# 692  3 11 14 15 32 36
	# 734  6 16 37 38 41 45
	# 788  2 10 11 19 35 39
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c( 6       ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(16, 3   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(37,11,25,39) ) cnt <- cnt + 1 # *37
					if( aZoid[5]%in%c(14,38) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(43) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(37,38),aZoid) ) cnt <- cnt + 1	# 32,37,38
					if( fCutU.hasPtn(c(32,37),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(9    ) ) cnt <- cnt + 1
					if( aRem[2]%in%c(3    ) ) cnt <- cnt + 1
					if( aRem[3]%in%c(6    ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(9    ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(5    ) ) cnt <- cnt + 1
					if( aRem[6]%in%c(7    ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8, 6 ) ) cnt <- cnt + 1
					if( aCStep[2]%in%c( 8    ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 8    ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c( 3, 4, 5 ) ) cnt <- cnt + 1
					if( all(aCStep[4:5]==c(16, 4)) ) cnt <- cnt + 1
					if( aCStep[1]==aCStep[3] ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.nextRebNum()

# 
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	# rawVal, cStep 모두 특이하다. 주의할 것.
	stdMI <- fCutU.getMtxInfo( zMtx )	
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# <remove> 동일 컬럼 rebind가 2개짜리도 툭툭 튀어나온다..
    # flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				return( 2>sum(stdMI$lastZoid%in%aZoid) )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # flgCnt[!flag] <- flgCnt[!flag] + 2

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
	# 599  5 12 17 29 34 35
	# 653  5  6 26 27 38 39
	# 709 10 18 30 36 39 44
	# 744 10 15 18 21 34 41
	# 753  2 17 19 24 37 41
	# 808 15 21 31 32 41 43
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c(24,21,16   ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(37,19   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(41) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(41,43,34) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(43) ) cnt <- cnt + 1

					if( all(aZoid[3:4]==c(32,41)) ) cnt <- cnt + 1
					if( all(aZoid[4:5]==c(41,43)) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(37,41),aZoid) ) cnt <- cnt + 1
					# ** c( r2,17,r9 )
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(9,6  ) ) cnt <- cnt + 1
					if( aRem[2]%in%c(1    ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(1    ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[3]%in%c(4) ) cnt <- cnt + 1
					if( aCStep[4]%in%c(9) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(3) ) cnt <- cnt + 1
					if( 2<sum(aCStep[5]==c(6,10,1,9,2)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.nextCStepBin()

#
fCutCnt.stdFiltedCnt_0 <- function( gEnv ,allIdxF ,stdFiltedCnt ,fCnt ,rpt=FALSE ){
	# stdFiltedCnt <- allIdxLst$stdFiltedCnt	;fCnt <- 0
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF[stdFiltedCnt==fCnt, ,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	# rawVal, cStep 모두 특이하다. 주의할 것.
	stdMI <- fCutU.getMtxInfo( zMtx )	
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

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


	# 789  2  6  7 12 19 45
	# 794  6  7 18 19 30 38
	# 804  1 10 13 26 32 36
	# 805  3 12 13 18 31 32
	# 806 14 20 23 31 37 38
	# 813 11 30 34 35 42 44
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1   ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(33   ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# 참... 체크해 볼 만한 게 없네.

	return( flgCnt )

} # fCutCnt.stdFiltedCnt_0()

# 
fCutCnt.colValStd <- function( gEnv ,allIdxF ,cutCol.idx ,cutCol.val ,rpt=FALSE ){
	# cutCol.idx=1 ;cutCol.val=1
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid[-cutCol.idx]==stdMI$lastZoid[-cutCol.idx]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid[-cutCol.idx]==aZoid[-cutCol.idx]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					quoTbl <- table(aZoid%/%10)
					return( !stdMI$quo10$sameTbl(quoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	if( 1<stdMI$mtxLen ){
		tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
		flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
						aCode <- aZoid%%10
						for( cIdx in 1:3 ){
							fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
							if( fnd ) return( FALSE )
						}
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} #fCutCnt.colValStd()
# 
fCutCnt.zWidthStd <- function( gEnv ,allIdxF ,zWidth ,rpt=FALSE ){
	applyFlag <- zWidth == (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[zWidth==(gEnv$zhF[,6]-gEnv$zhF[,1]), ,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==stdMI$lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 2

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					quoTbl <- table(aZoid%/%10)
					return( !stdMI$quo10$sameTbl(quoTbl) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	# 이미 각 케이스에서 확인하므로 제거.
	# lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    # flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
	# 				return( lastZW!=(aZoid[6]-aZoid[1]) )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	if( 1<stdMI$mtxLen ){
		tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
		flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
						aCode <- aZoid%%10
						for( cIdx in 1:3 ){
							fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
							if( fnd ) return( FALSE )
						}
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidthStd()
# 
fCutCnt.quoTblStd <- function( gEnv ,allIdxF ,tblStr ,rpt=FALSE ){

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})

	applyFlag <- aQuoTblStr==tblStr
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zQuoTblStr <- sapply( fCutU.getQuoTblLst(gEnv$zhF) ,function(quo){quo$valStr} )
	zMtx <- gEnv$zhF[zQuoTblStr==tblStr, ,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 2>sum(aZoid==stdMI$lastZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    # flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
	# 				quoTbl <- table(aZoid%/%10)	# 같은 quoTbl 끼리니까 당연히 적용 안됨.
	# 				return( !stdMI$quo10$sameTbl(quoTbl) )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # flgCnt[!flag] <- flgCnt[!flag] + 1
	lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					return( lastZW!=(aZoid[6]-aZoid[1]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	if( 1<stdMI$mtxLen ){
		tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
		flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
						aCode <- aZoid%%10
						for( cIdx in 1:3 ){
							fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
							if( fnd ) return( FALSE )
						}
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid[2:6] - aZoid[1:5]
					for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
						fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.quoTblStd()

# 
fCutCnt.colVal_1_x <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.colVal_1_1( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_1_2( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_1_4( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_1_7( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_1_8( gEnv ,allIdxF ,rpt )

	return( flgCnt )
}
# 
fCutCnt.colVal_1_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 1	;cutCol.val <- 1
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# <remove>
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					return( !(aZoid[3]%in%c(26)) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[3]%in%c(11) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(16,12) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[3]%in%c(6  ) ) cnt <- cnt + 1	# 6
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[3]%in%c( 9,10,13) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 1      ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(9,1),aCStep) ) cnt <- cnt + 1
					# if( all(aCStep[2:3]==c(1,15)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_1_1()
# 
fCutCnt.colVal_1_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 1	;cutCol.val <- 2
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c(10) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(16) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(31,36) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(45,42,33) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2]%in%c(0  ) ) cnt <- cnt + 1
					if( aRem[3]%in%c(6  ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(3  ) ) cnt <- cnt + 1
					if( aRem[5]%in%c(1  ) ) cnt <- cnt + 1
					if( aRem[6]%in%c(9,2 ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c( 9, 5),aRem) ) cnt <- cnt + 1	# * 19,r5
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8, 6   ) ) cnt <- cnt + 1	# *6
					if( aCStep[2]%in%c( 7,10, 6) ) cnt <- cnt + 1	# *6
					if( aCStep[3]%in%c( 4,6    ) ) cnt <- cnt + 1	# *
					if( aCStep[4]%in%c(10,3    ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c( 5      ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(10,4),aCStep) ) cnt <- cnt + 1	# *
					if( fCutU.hasPtn(c(6,10),aCStep) ) cnt <- cnt + 1
					# if( all(aCStep[2:3]==c(1,15)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_1_2()
# 
fCutCnt.colVal_1_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 1	;cutCol.val <- 4
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c( 5,6,7) ) cnt <- cnt + 1	# *6
					if( aZoid[3]%in%c(16    ) ) cnt <- cnt + 1	#
					if( aZoid[4]%in%c(26    ) ) cnt <- cnt + 1	#
					if( aZoid[6]%in%c(24    ) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[2]%in%c(6  ) ) cnt <- cnt + 1
					if( aRem[4]%in%c(7  ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(0,3),aRem) ) cnt <- cnt + 1	# *r0,43
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1      ) ) cnt <- cnt + 1
					if( aCStep[2]%in%c( 9      ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 10     ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(  7,18  ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(9,1),aCStep) ) cnt <- cnt + 1
					# if( all(aCStep[2:3]==c(1,15)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_1_4()
# 
fCutCnt.colVal_1_7 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 1	;cutCol.val <- 7
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c(22) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(12) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(29) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(34) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(37) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(34,37),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(15      ) ) cnt <- cnt + 1
					if( aCStep[2]%in%c( 2,3    ) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 1,2    ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c( 3,9    ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_1_7()
# 
fCutCnt.colVal_1_8 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 1	;cutCol.val <- 8
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[3]%in%c(18,13) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(21,36) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(21) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(40,34) ) cnt <- cnt + 1

					if( fCutU.hasPtn(c(36,45),aZoid) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(18,21),aZoid) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(19,21),aZoid) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[2]%in%c( 7      ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 1,3,23 ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_1_8()

# 
fCutCnt.colVal_6_x <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.colVal_6_40( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_6_41( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_6_42( gEnv ,allIdxF ,rpt )

	return( flgCnt )
}
# 
fCutCnt.colVal_6_40 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 6	;cutCol.val <- 40
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 6, 1 ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c( 8  ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(14  ) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(36,16) ) cnt <- cnt + 1	# *r6
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[5]%in%c(6   ) ) cnt <- cnt + 1	# *r6
					return( 2>cnt )	# Free pass
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3      ) ) cnt <- cnt + 1	# * 3
					if( aCStep[2]%in%c( 2,10   ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 1      ) ) cnt <- cnt + 1	# * 1
					if( aCStep[4]%in%c( 5,13   ) ) cnt <- cnt + 1
					# if( fCutU.hasPtn(c(9,1),aCStep) ) cnt <- cnt + 1
					# if( all(aCStep[2:3]==c(1,15)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_6_40()

fCutCnt.colVal_6_41 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 6	;cutCol.val <- 41
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c(12,25) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(26   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(24   ) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(20,30,40) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )	# Free pass
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4      ) ) cnt <- cnt + 1
					if( aCStep[2]%in%c(20,13   ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 3,5    ) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 7,5,9  ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(11      ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(5,10),aCStep) ) cnt <- cnt + 1	# *
					if( all(aCStep[2:3]==c(8,3)) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_6_41()

fCutCnt.colVal_6_42 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 6	;cutCol.val <- 42
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c( 2    ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c( 9,12,29) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(28,18) ) cnt <- cnt + 1	# * r8
					if( fCutU.hasPtn(c(31,32),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(11,12),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(31,32),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1      ) ) cnt <- cnt + 1	# *
					if( aCStep[3]%in%c( 5      ) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 5      ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(10      ) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_6_42()

# 
fCutCnt.colVal_5_x <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.colVal_5_29( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_5_39( gEnv ,allIdxF ,rpt )

	return( flgCnt )
}

fCutCnt.colVal_5_29 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 5	;cutCol.val <- 29
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 2, 7    ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c( 6, 2    ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(13,16    ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(26      ) ) cnt <- cnt + 1

					if( fCutU.hasPtn(c(14,16),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4,6  ) ) cnt <- cnt + 1	# *4
					if( aCStep[2]%in%c( 4    ) ) cnt <- cnt + 1	# *6
					if( aCStep[3]%in%c( 6,5  ) ) cnt <- cnt + 1	# *6,5
					if( aCStep[4]%in%c( 3,6  ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(10,4,6) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_5_29()

fCutCnt.colVal_5_39 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 5	;cutCol.val <- 39
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5, 1    ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(21,31    ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(12       ) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(45,41    ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(12,21),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(9,12),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[2]%in%c( 9,4   ) ) cnt <- cnt + 1	# *9
					if( aCStep[3]%in%c( 1,17  ) ) cnt <- cnt + 1	# 
					if( aCStep[4]%in%c( 1,9   ) ) cnt <- cnt + 1	# 
					if( aCStep[5]%in%c( 6,2   ) ) cnt <- cnt + 1	# 
					if( fCutU.hasPtn(c(16, 9),aCStep) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_5_39()

# 
fCutCnt.colVal_3_x <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.colVal_3_13( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.colVal_3_23( gEnv ,allIdxF ,rpt )

	return( flgCnt )
}

fCutCnt.colVal_3_13 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 3	;cutCol.val <- 13
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3    ) ) cnt <- cnt + 1	#
					if( aZoid[2]%in%c(10,12 ) ) cnt <- cnt + 1	#

					if( fCutU.hasPtn(c(31,32),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(26,30),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[3]%in%c( 6  ) ) cnt <- cnt + 1	# 
					if( aCStep[5]%in%c( 4  ) ) cnt <- cnt + 1	# 

					if( all(aCStep[c(2,5)]==c( 3,4 )) ) cnt <- cnt + 1	#
					if( all(aCStep[c(2,5)]==c( 1,1 )) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_3_13()

fCutCnt.colVal_3_23 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	cutCol.idx <- 3	;cutCol.val <- 23
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 9    ) ) cnt <- cnt + 1	#
					if( aZoid[2]%in%c(10    ) ) cnt <- cnt + 1	#
					if( aZoid[4]%in%c(35,24,42) ) cnt <- cnt + 1	#
					if( aZoid[6]%in%c(39) ) cnt <- cnt + 1	#
					# if( fCutU.hasPtn(c(26,30),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5,7,3  ) ) cnt <- cnt + 1	# 
					if( aCStep[2]%in%c( 1  ) ) cnt <- cnt + 1
					if( aCStep[3]%in%c( 6  ) ) cnt <- cnt + 1
					if( aCStep[4]%in%c( 3  ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(11  ) ) cnt <- cnt + 1
					# if( all(aCStep[c(2,5)]==c( 1,1 )) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.colVal_3_23()

# 
fCutCnt.zWidth <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.zWidth_33( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.zWidth_36( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.zWidth_39( gEnv ,allIdxF ,rpt )
	return( flgCnt )
}

fCutCnt.zWidth_33 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	zWidth <- 33
	applyFlag <- (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])==zWidth
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[(gEnv$zhF[,6]-gEnv$zhF[,1])==zWidth,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# zoid[1] 들이 거의 10 이상... 그냥 스킵하는게?
	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 9,10, 3,12 ) ) cnt <- cnt + 1	#
					if( aZoid[3]%in%c(35    ) ) cnt <- cnt + 1	#
					if( aZoid[4]%in%c(34,31) ) cnt <- cnt + 1	#
					if( aZoid[5]%in%c(42,38) ) cnt <- cnt + 1	#
					if( aZoid[6]%in%c(43,42,45) ) cnt <- cnt + 1	#
					# if( fCutU.hasPtn(c(26,30),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 19  ) ) cnt <- cnt + 1 
					if( aCStep[2]%in%c(  4,14,7  ) ) cnt <- cnt + 1	# *7
					if( aCStep[3]%in%c(  5, 3  ) ) cnt <- cnt + 1
					if( aCStep[4]%in%c(  6  ) ) cnt <- cnt + 1
					if( aCStep[5]%in%c(  4,10  ) ) cnt <- cnt + 1
					if( all(aCStep[c(1,2)]==c(5,6)) ) cnt <- cnt + 1
					if( all(aCStep[c(1,2)]==c(12,5)) ) cnt <- cnt + 1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidth_33()

fCutCnt.zWidth_36 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	zWidth <- 36
	applyFlag <- (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])==zWidth
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[(gEnv$zhF[,6]-gEnv$zhF[,1])==zWidth,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3,9 ) ) cnt <- cnt + 1	#
					if( aZoid[2]%in%c( 3,6,12 ) ) cnt <- cnt + 1	#
					if( aZoid[4]%in%c( 41,31 ) ) cnt <- cnt + 1	#
					if( aZoid[5]%in%c( 39 ) ) cnt <- cnt + 1	# *39
					if( fCutU.hasPtn(c(10,33),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5,7  ) ) cnt <- cnt + 1 
					if( aCStep[2]%in%c( 3  ) ) cnt <- cnt + 1 
					if( aCStep[3]%in%c( 1,7  ) ) cnt <- cnt + 1 
					if( aCStep[4]%in%c( 7,10,1  ) ) cnt <- cnt + 1 
					if( aCStep[5]%in%c( 7  ) ) cnt <- cnt + 1 
					if( all(aCStep[c(1,2)]==c(5,3)) ) cnt <- cnt + 1	# *
					if( all(aCStep[c(3,4)]==c(7,8)) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidth_36()

fCutCnt.zWidth_39 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	zWidth <- 39
	applyFlag <- (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])==zWidth
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[(gEnv$zhF[,6]-gEnv$zhF[,1])==zWidth,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 4 ) ) cnt <- cnt + 1	# *4
					if( aZoid[2]%in%c(19,18,24) ) cnt <- cnt + 1	# *19
					if( aZoid[5]%in%c(39,33) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(43) ) cnt <- cnt + 1	# *43
					if( fCutU.hasPtn(c(37,41),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(38,41),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 12    ) ) cnt <- cnt + 1 
					if( aCStep[2]%in%c(  3, 5 ) ) cnt <- cnt + 1 
					if( aCStep[3]%in%c(  4    ) ) cnt <- cnt + 1 	# *4
					if( aCStep[4]%in%c(  4,7  ) ) cnt <- cnt + 1 	#
					if( aCStep[5]%in%c(  5,10, 4,13 ) ) cnt <- cnt + 1 	#
					if( all(aCStep[c(2,3,4)]==c(3,4,7)) ) cnt <- cnt + 1	# *
					if( fCutU.hasPtn(c(13, 2),aCStep) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidth_39()

fCutCnt.quoTbl <- function( gEnv ,allIdxF ,rpt=FALSE ){
	flgCnt <- rep( 0 ,length(allIdxF) )

	flgCnt <- flgCnt + fCutCnt.quoTbl_1221( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.quoTbl_2121( gEnv ,allIdxF ,rpt )
	flgCnt <- flgCnt + fCutCnt.quoTbl_2211( gEnv ,allIdxF ,rpt )
	return( flgCnt )
}

fCutCnt.quoTbl_1221 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	quoStr <- "1 2 2 1"
	applyFlag <- apply(gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
						return( quoStr == fCutU.getQuoObj( aZoid )$valStr )
					})

	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	stdQuoFlag <- apply( gEnv$zhF ,1 ,function( zoid ){
						return( quoStr == fCutU.getQuoObj(zoid)$valStr )
					})
	zMtx <- gEnv$zhF[stdQuoFlag,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 8 ) ) cnt <- cnt + 1	# *8
					if( aZoid[2]%in%c(27 ) ) cnt <- cnt + 1	#
					if( aZoid[3]%in%c(11,39 ) ) cnt <- cnt + 1	# 39는 무리다. ㅋㅋ
					if( aZoid[4]%in%c(32,37 ) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(36,28 ) ) cnt <- cnt + 1
					if( fCutU.hasPtn(c(10,11),aZoid) ) cnt <- cnt + 1	# *
					if( fCutU.hasPtn(c(38,45),aZoid) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3, 2    ) ) cnt <- cnt + 1 
					if( aCStep[2]%in%c( 3, 2,20 ) ) cnt <- cnt + 1 # *2
					if( aCStep[4]%in%c( 2,19    ) ) cnt <- cnt + 1 
					if( aCStep[5]%in%c( 2, 3    ) ) cnt <- cnt + 1 
					if( all(aCStep[c(4,5)]==c(20,2)) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.quoTbl_1221()

fCutCnt.quoTbl_2121 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	quoStr <- "2 1 2 1"
	applyFlag <- apply(gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
						return( quoStr == fCutU.getQuoObj( aZoid )$valStr )
					})

	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	stdQuoFlag <- apply( gEnv$zhF ,1 ,function( zoid ){
						return( quoStr == fCutU.getQuoObj(zoid)$valStr )
					})
	zMtx <- gEnv$zhF[stdQuoFlag,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5, 7 ) ) cnt <- cnt + 1	#
					if( aZoid[2]%in%c(19,15 ) ) cnt <- cnt + 1	#
					if( aZoid[3]%in%c(25,14,20 ) ) cnt <- cnt + 1	# *25
					if( aZoid[4]%in%c(26,35,21 ) ) cnt <- cnt + 1	#
					if( aZoid[5]%in%c(28,27,30,38 ) ) cnt <- cnt + 1	# *28,38
					if( aZoid[6]%in%c(40,42 ) ) cnt <- cnt + 1	# *28
					if( fCutU.hasPtn(c(38,44),aZoid) ) cnt <- cnt + 1	#
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5, 2, 6, 1 ) ) cnt <- cnt + 1 # *2
					if( aCStep[2]%in%c(14,10) ) cnt <- cnt + 1 # *2
					if( aCStep[3]%in%c( 7   ) ) cnt <- cnt + 1 #
					if( aCStep[4]%in%c( 2,10 ) ) cnt <- cnt + 1 #
					if( aCStep[5]%in%c( 5 ) ) cnt <- cnt + 1 #
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.quoTbl_2121()

fCutCnt.quoTbl_2211 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	quoStr <- "2 2 1 1"
	applyFlag <- apply(gEnv$allZoidMtx[allIdxF,] ,1 ,function(aZoid){
						return( quoStr == fCutU.getQuoObj( aZoid )$valStr )
					})

	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	stdQuoFlag <- apply( gEnv$zhF ,1 ,function( zoid ){
						return( quoStr == fCutU.getQuoObj(zoid)$valStr )
					})
	zMtx <- gEnv$zhF[stdQuoFlag,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 6    ) ) cnt <- cnt + 1	#
					if( aZoid[2]%in%c( 7     ) ) cnt <- cnt + 1	#
					if( aZoid[3]%in%c(10,3   ) ) cnt <- cnt + 1	#
					if( aZoid[5]%in%c(24      ) ) cnt <- cnt + 1	#
					if( aZoid[6]%in%c(36,43   ) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c( 5,11),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(11,17),aZoid) ) cnt <- cnt + 1	#
					if( fCutU.hasPtn(c(27,26,32),aZoid) ) cnt <- cnt + 1	# *
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1,3,2 ) ) cnt <- cnt + 1 #
					if( aCStep[2]%in%c( 7,3,4 ) ) cnt <- cnt + 1 #
					if( aCStep[3]%in%c( 1,2,7,4,3 ) ) cnt <- cnt + 1 # *2
					if( aCStep[4]%in%c( 7,5 ) ) cnt <- cnt + 1 #
					if( aCStep[5]%in%c( 20 ) ) cnt <- cnt + 1 #
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.quoTbl_2211()

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================

fCut.finalApproach <- function( gEnv ,allIdxF ,rpt=FALSE ){

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( aZoid[4]%in%c(28) ) return(FALSE)
					if( aZoid[5]%in%c(37) ) return(FALSE)
					if( aZoid[6]%in%c(37) ) return(FALSE)

					if( aZoid[2]%in%c( 2) ) return(FALSE)
					if( aZoid[6]%in%c(33) ) return(FALSE)

					if( aZoid[4]%in%c(37) ) return(FALSE)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( fCutU.hasPtn(c(37,39),aZoid) ) return(FALSE)

					if( fCutU.hasPtn(c(41,45),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(42,43),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(37,40),aZoid) ) return(FALSE)

					if( fCutU.hasPtn(c(14,18),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(24,16),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(16,19),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(18,21),aZoid) ) return(FALSE)

					if( fCutU.hasPtn(c(37,38),aZoid) ) return(FALSE)
					if( fCutU.hasPtn(c(32,37),aZoid) ) return(FALSE)

					if( all(aZoid[3:4]==c(32,41)) ) return(FALSE)
					if( all(aZoid[4:5]==c(41,43)) ) return(FALSE)
					if( fCutU.hasPtn(c(37,41),aZoid) ) return(FALSE)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c(17,20) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(37,17) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(20,28,34,35) ) cnt <- cnt + 1	# ** 28
					if( aZoid[5]%in%c(37,21) ) cnt <- cnt + 1	# * 37
					if( aZoid[6]%in%c(37,22) ) cnt <- cnt + 1	# * 37

					if( aZoid[1]%in%c( 5   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c( 2,15) ) cnt <- cnt + 1 # *2
					if( aZoid[3]%in%c(32   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(43   ) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(41   ) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(45,34,33) ) cnt <- cnt + 1	# *33

					if( aZoid[1]%in%c( 3   ) ) cnt <- cnt + 1
					if( aZoid[2]%in%c(15   ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(26,34) ) cnt <- cnt + 1

					if( aZoid[2]%in%c( 6       ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(16, 3   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(37,11,25,39) ) cnt <- cnt + 1 # *37
					if( aZoid[5]%in%c(14,38) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(43) ) cnt <- cnt + 1

					if( aZoid[2]%in%c(24,21,16   ) ) cnt <- cnt + 1
					if( aZoid[3]%in%c(37,19   ) ) cnt <- cnt + 1
					if( aZoid[4]%in%c(41) ) cnt <- cnt + 1
					if( aZoid[5]%in%c(41,43,34) ) cnt <- cnt + 1
					if( aZoid[6]%in%c(43) ) cnt <- cnt + 1

					return( 4>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# 26,r7
					fndIdx <- which(aZoid==26)	;fndIdx <- setdiff(fndIdx,6)
					for( cIdx in fndIdx ){
						if( 7 == aZoid[cIdx+1]%%10 ) return( FALSE )
					}
					# r2,17,r9
					fndIdx <- which(aZoid==17)	;fndIdx <- setdiff(fndIdx,c(1,6))
					for( cIdx in fndIdx ){
						rem <- aZoid[cIdx+c(-1,1)]%%10
						if( all(rem==c(2,9)) ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )

} # fCut.finalApproach()



