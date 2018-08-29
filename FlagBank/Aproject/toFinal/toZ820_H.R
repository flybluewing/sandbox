# toZ820_H.R 최종접근
cntThld <- c(2,2,3,2,2)	;names(cntThld) <- c("raw","rawFV","rem","cStep","fStep")

# 공용
# done
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
# done
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

# undone
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	# 동일 컬럼에 같은 값 재현은 제외.
    # flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				return( !any(aZoid==stdMI$lastZoid) )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # allIdxF <- allIdxF[flag]

	#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
	#     11 30 34 35 42 44    |19  4  1  7  2 |                        |0 1 0 3 2 |1 3 2
	#      2 21 28 38 42 45(1) |19  7 10  4  3 | -9  -9  -6   3   0   1 |1 0 2 1 2 |1 2 1 2
	#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 15   0  -3 -12 -15  -9 |0 1 4 1 0 |1 4 1
	#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
	#      3  9 12 13 25 43(1) | 6  3  1 12 18 | -9  -9  -7 -16  -6   4 |2 2 1 0 1 |2 2 1 1
	#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 11   6  13  15   4 -13 |0 2 3 1 0 |2 3 1
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[5]==1 ) return( FALSE )
					if( 1<sum(aCStep==1) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	stdQuo <- fCutU.chkRowPtnReb(stdMI$quoTail)	# stdQuo$matValMtx
	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj(aZoid)$size
					return( !stdQuo$filt(quoSize)$filted )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]

	return( allIdxF )

} # fCut.basic()

# done
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2 ]==c(3,2,0)) ) return(FALSE)	# next rebind of 1,2,2 (reverse)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
	cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0	# fCut.basic() 의 내용과 조율할 것.
					if( aZoid[1]%in%c( 4,25,13 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(12       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(27       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(         ) ) cnt<-cnt+1

					if( aZoid[3]%in%c(33       ) ) cnt<-cnt+1
					if( all(aZoid[3:5]==c(12,14,26)) ) cnt<-cnt+1	# 25 28 29 / 25 26 27
					return( cnt )
			})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=2 )	# <12>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <21>
					if( fCutU.hasPtn(c(21,22,NA,NA,27),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <25>
					if( fCutU.hasPtn(c(17,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <28>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <29>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <38>
					if( fCutU.hasPtn(c(38,NA,45),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <45>
					if( fCutU.hasPtn(c(38,NA,45),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(30,29,38,NA,38,45),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(4    ,5,3 ),c( 4,25,13 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(5,9  ,7   ),c(12       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(      7,6 ),c(         )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(8,3,7,0   ),c(27       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(          ),c(         )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(      8   ),c(         )) ) cnt<-cnt+1 # 6

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c(12,18  )) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+2]==c( 1,12,18)) ) cnt<-cnt+1	# 2

					if( all(aCStep[1:2]==c(10, 3)) ) cnt<-cnt+1	#  1,10
					if( fCutU.hasPtn(c(10, 4),aZoid) ) cnt<-cnt+1
					if( all(aCStep[3:4]==c( 8, 5)) ) cnt<-cnt+1	#  3, 1
					if( all(aCStep[4:5]==c( 1,12)) ) cnt<-cnt+1	#  1, 1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      2 21 28 38 42 45    |19  7 10  4  3 |                        |1 0 2 1 2 |1 2 1 2
		#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 15   0  -3 -12 -15  -9 |0 1 4 1 0 |1 4 1
		#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
		#      3  9 12 13 25 43(1) | 6  3  1 12 18 | -9  -9  -7 -16  -6   4 |2 2 1 0 1 |2 2 1 1
		#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 11   6  13  15   4 -13 |0 2 3 1 0 |2 3 1
		#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  2  10   8  10  11  15 |0 1 1 2 2 |1 1 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  9, 10  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  4      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+4]==c(  2, 10    )) ) cnt<-cnt+1 # 11
					if( 1<sum(aFStep[1:3+3]==c( 10,  8, 10)) ) cnt<-cnt+1 # 15

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )
} # fCutCnt.basic()

# done
fCutCnt.colValSeqNext <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

	# =========================================================
	# anaColEndPtn()
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	banVal <- sapply( colPtnLst ,function( p ){
					return( if(length(p$val)>0) p$val[1] else 0 )
				})
	banVal.idx <- which( banVal!=0 )
	banVal <- banVal[banVal.idx]

	# <remove>
    score <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum(aZoid[banVal.idx]==banVal)
					return( ifelse(cnt>=2,2,cnt) )
				})	;kIdx<-anaFltCnt(score,rpt)
    flgCnt <- flgCnt + score
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid %% 10
					cnt <- 0
					if( 1<sum(aRem[banVal.idx]==(banVal%%10)) ) cnt <- cnt + 1
					return( 1>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[2]%in%c( 18 ) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3,5)]==c(12,16,35,41)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3  )]==c(36, 3,36   )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1  ,3  )]==c( 9   ,31   )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1  ,3  )]==c(10   ,27   )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1  ,3  )]==c(14   ,39   )) ) cnt<-cnt+1
					return( 1>cnt )
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

    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aRem <- aZoid%%10
					cnt <- 0
					if( aZoid[1]%in%c( 8,11       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(13,27       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(34,25,20    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   43,34,27 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(            ) ) cnt<-cnt+1

					if( all(aZoid[1:2+0]==c(25,32)) ) cnt<-cnt+1
					if( all(aZoid[1:2+1]==c(40,44)) ) cnt<-cnt+1
					if( all(aZoid[1:2+3]==c(32,35)) ) cnt<-cnt+1
					if( all(aZoid[1:2+3]==c(39,41)) ) cnt<-cnt+1
					# if( all(aZoid[ 1:2]==c( 1, 8)) ) cnt<-cnt+1 #  3, 8  2, 8

					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(8,1    ),c( 8,11   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c(3,7,1  ),c(13,27   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c(6,4,2,8),c(        )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c(4,5,0  ),c(34,25,20)) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c(3,4,7  ),c(43,34,27)) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c(3,0    ),c(        )) )	remCnt <- remCnt+1					
						# grp 1
						if( aZoid[1]==24 && fCutU.remFilt(aZoid[2],c(2),c(32)) ) remCnt <- remCnt+1	
						# grp 2
						if( aZoid[3]==44 && fCutU.remFilt(aZoid[2],c(1),c(  )) ) remCnt <- remCnt+1 
						# grp 4
						if( aZoid[4]==20 && fCutU.remFilt(aZoid[5],c(4),c(  )) ) remCnt <- remCnt+1 
						if( aZoid[5]==41 && fCutU.remFilt(aZoid[4],c(9),c(39)) ) remCnt <- remCnt+1 
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# <remove>	cStep 
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					score <- 0
					score <- score + sum(aCStep==c( 5, 8, 7,18, 3)) # last cStep 2개이상 자름
					if( 1<sum(aCStep==c( 5, 1, 1,18,17)) ){	# h-2 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c( 5, 1, 1,18,17))-1)
					}
					if( 1<sum(aCStep==c(16, 6, 5,14, 4)) ){	# h-3 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c(16, 6, 5,14, 4))-1)
					}
					if( 1<sum(aCStep==c( 2, 3, 9, 3, 8)) ){	# h-4 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c( 2, 3, 9, 3, 8))-1)
					}
					if( 1<sum(aCStep==c( 8, 2, 3, 4, 2)) ){	# h-5 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c( 8, 2, 3, 4, 2))-1)
					}
					if( 1<sum(aCStep==c( 9, 4, 7, 6, 6)) ){	# h-6 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c( 9, 4, 7, 6, 6))-1)
					}
					if( 1<sum(aCStep[2:5]==c(12, 4, 4, 8)) ){	# h-7 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep[2:5]==c(12, 4, 4, 8))-1)
					}

					cnt <- 0
					if( aCStep[1]%in%c( 5,16   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(        ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      19) ) cnt<-cnt+1
					if( aCStep[4]%in%c(18,14   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(        ) ) cnt<-cnt+1

					if( 2<sum(aCStep[1:3+0]==c( 5, 8, 7 )) ) cnt<-cnt+1 # 5
					if( 2<sum(aCStep[1:3+1]==c( 2, 3, 9 )) ) cnt<-cnt+1 # 8
					if( 2<sum(aCStep[1:3+2]==c( 3, 4, 2 )) ) cnt<-cnt+1 # 7
					if( 2<sum(aCStep[1:3+2]==c( 6, 5, 4 )) ) cnt<-cnt+1 # 3
					score <- score + ifelse(cnt>1,1,0)

					return( score )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt <- flgCnt + fltCnt

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

	return( flgCnt )
} # fCutCnt.colValSeqNext()                     

# done
fCutCnt.colValSeqNext.cStep <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
	#	<remove>
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					return( 2>sum(aCStep[banVal.idx]==banVal) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 2

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2,10       ) ) cnt<-cnt+1	# 마지막 값 연속도 포함.
					if( aCStep[2]%in%c(16      ,10 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 9, 3, 5    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(12, 8   ,10 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3, 4       ) ) cnt<-cnt+1

					if( 2<sum(aCStep==c(10, 8, 4, 8, 2)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c(12, 2, 1,17,11)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 2, 1, 5, 8, 7)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c(19, 4, 1, 1,12)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 2, 5, 4,20, 5)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 3, 4, 5, 2,30)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 4, 7, 5, 2, 1)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 7,16, 4, 8,14)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 3, 3, 1, 6)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 7, 8, 4,17)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c(16,16, 1, 6)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 2, 2, 6, 1)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c(11, 6, 9,14)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 4, 4, 2, 1)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 2,13,10, 7)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 2, 3,20,15)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c(13, 6, 6,14)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 8, 1, 8,12)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 3, 5,12,14)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c(20,12, 2, 4)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c(24, 5, 2,11)) ) cnt<-cnt+1
					if( 2<sum(aCStep[c(1,3,4,5)]==c( 9, 5, 3, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 5, 6,13)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c(11, 5, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 3, 2, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 7, 5, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 4, 8, 3)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c(10, 5, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 6, 7, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 1,24, 2)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c( 6,13,10)) ) cnt<-cnt+1
					if( all(aCStep[1:3+2]==c(11, 1,11)) ) cnt<-cnt+1

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

	# conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					cnt <- 0
					if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(     ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(10, 3) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4,12) ) cnt<-cnt+1

					if( all(aCStep[1:2+1]==c( 2, 6)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 4, 4)) ) cnt<-cnt+1

					if( all(aCStep[1:2+2]==c( 2, 7)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 4, 7)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 8,11)) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c( 3, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 3, 6)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 8,11)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 9, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(18, 6)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(21, 4)) ) cnt<-cnt+1

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
	# -- conditional
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6] - aZoid[1:5]
					cnt <- 0
					# if( all(aCStep[3:5]==c( 8, 5, 1)) ) cnt<-cnt+1	# next of 2
					# if( all(aCStep[3:5]==c( 2, 8,15)) ) cnt<-cnt+1	# inc of 2
					# if( all(aCStep[3:5]==c( 5, 6, 1)) ) cnt<-cnt+1	# inc of 1

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

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,1,2 reverse
					if( all(quoSize[1:3+2]==c(2,2,2)) ) return(FALSE)	# next rebind of 1,2,2 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(12,16      ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(22,16,31,24) ) cnt<-cnt+1
					if( aZoid[3]%in%c(           ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(33         ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(           ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(           ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-3 )	# <12>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <14>
					if( fCutU.hasPtn(c(14,NA,NA,32   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,NA,NA,NA,38),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <15>
					if( fCutU.hasPtn(c(15,26,42,40),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <20>
					if( fCutU.hasPtn(c(20,22   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(20,NA,32),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=2 )	# <24>
					if( fCutU.hasPtn(c(20,NA,24,34),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <26>
					if( fCutU.hasPtn(c(12,NA,26),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff= 1)	# <30>
					if( fCutU.hasPtn(c(17,18,14,20,30),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <31>
					if( fCutU.hasPtn(c(10,19,31,NA,44),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# <43>
					if( fCutU.hasPtn(c(40,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(29,39,NA,NA,NA,43),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(2,6,4   ),c(12,16      )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(2,6,1,4 ),c(22,16,31,24)) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(        ),c(           )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3,0,2   ),c(33         )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(4  ,1   ),c(           )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(    1,6 ),c(           )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6, 3, 4    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(          1 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(          1 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 3   ,10 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 6,10, 1)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+0]==c( 1,12, 2)) ) cnt<-cnt+1	#10
					if( 1<sum(aCStep[1:3+1]==c( 6, 3, 8)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 2, 5, 4)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 5, 4,30)) ) cnt<-cnt+1	# 2

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#     12 14 24 26 34 45    | 2 10  2  8 11 |                        |0 2 2 1 1 |2 2 1 1
		#     13 14 26 28 30 36(2) | 1 12  2  2  6 |  1   0   2   2  -4  -9 |0 2 2 2 0 |2 2 2
		#      1  3  8 12 42 43    | 2  5  4 30  1 |-12 -11 -18 -16  12   7 |3 1 0 0 2 |3 1 2
		#     12 15 16 20 24 30(1) | 3  1  4  4  6 | 11  12   8   8 -18 -13 |0 3 2 1 0 |3 2 1
		#     14 20 23 31 37 38(1) | 6  3  8  6  1 |  2   5   7  11  13   8 |0 1 2 3 0 |1 2 3
		#     15 21 31 32 41 43(1) | 6 10  1  9  2 |  1   1   8   1   4   5 |0 1 1 2 2 |1 1 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  0      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  9      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  9      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-12,-11,-18)) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+1]==c(-12,-11,-18)) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+1]==c(  5,  7, 11)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+2]==c(  7, 11, 13)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+3]==c(-12,-11,-18)) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+3]==c( 16,-12, -7)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:2+4]==c(  1,  1   )) ) cnt<-cnt+1 #  5

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})

    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextZW()

# done
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(15,12) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(37   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(     ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# < 4>
					if( fCutU.hasPtn(c( 4,NA,17,31,32,23),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4, 8),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# < 6>
					if( fCutU.hasPtn(c( 6, 7,NA,NA,13),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# < 8>
					if( fCutU.hasPtn(c( 4, 8),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <13>
					if( fCutU.hasPtn(c( 6,NA,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   12,13,35,NA,39),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <15>
					if( fCutU.hasPtn(c( 8,NA,15,NA,20),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <23>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,23),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,18,20,23),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <33>
					if( fCutU.hasPtn(c(11,26,NA,29,33),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(3    ),c( 3   )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(5,3  ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(5,2,8),c(15,12)) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(5,3,4),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(3,5,7),c(37   )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5, 4, 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2      , 3 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(14      , 5 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1          ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 3, 2)) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:2+0]==c( 9, 5   )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+2]==c( 5, 4, 2)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 3,14,10)) ) cnt<-cnt+1	# 6

					if( fCutU.hasPtn(c( 5, 5),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5, 2),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 2, 6)) ) cnt<-cnt+1	# 2, 9

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      1  4  8 23 33 42    | 3  4 15 10  9 |                        |3 0 1 1 1 |3 1 1 1
		#      4  8 13 19 20 43(2) | 4  5  6  1 23 |  3   4   5  -4 -13   1 |2 2 1 0 1 |2 2 1 1
		#      5 10 13 27 37 41(1) | 5  3 14 10  4 |  1   2   0   8  17  -2 |1 2 1 1 1 |1 2 1 1 1
		#      7  9 12 14 23 28    | 2  3  2  9  5 |  2  -1  -1 -13 -14 -13 |2 2 2 0 0 |2 2 2
		#      4  6 15 25 26 33    | 2  9 10  1  7 | -3  -3   3  11   3   5 |2 1 2 1 0 |2 1 2 1
		#      6 11 15 17 23 40(2) | 5  4  2  6 17 |  2   5   0  -8  -3   7 |1 3 1 0 1 |1 3 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3, -3,  3)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+1]==c(  2,  0,  8)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+1]==c( -1, -1,-13)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+2]==c(  1, 13, 14)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+3]==c(  2,  5,  0)) ) cnt<-cnt+1 # -3

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextQuo10()

# done
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(39) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,2,1 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1,15   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 5      ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(42,12,27) ) cnt<-cnt+1
					if( aZoid[6]%in%c(45,44   ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 1>
					if( fCutU.hasPtn(c( 1, 5),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA, 8,NA,30),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# < 3>
					if( fCutU.hasPtn(c( 3,NA,20),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3, 8,NA,15,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <12>
					if( fCutU.hasPtn(c( 7,NA,12,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <16>
					if( fCutU.hasPtn(c(16,NA,NA,NA,45),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(16,19,44,34   ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <25>
					if( fCutU.hasPtn(c( 5,20,25,28,40,41),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <43>
					if( fCutU.hasPtn(c(14,NA,NA,NA,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   29,36,NA,43),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <44>
					if( fCutU.hasPtn(c(21,32,NA,NA,44),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <45>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(1,5    ),c( 1,15   )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(5      ),c( 5      )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(2,5    ),c(        )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3      ),c(        )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(2  ,7,1),c(42,12,27)) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(5,4    ),c(45,44   )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(15,17) ) cnt<-cnt+1
					if( aCStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 9, 9)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+1]==c( 9, 9, 5)) ) cnt<-cnt+1	# 2
					
					if( fCutU.hasPtn(c( 6, 2),aZoid) ) cnt<-cnt+1	# ++
					if( all(aCStep[1:2+2]==c( 6, 2 )) ) cnt<-cnt+1	# 9, 5

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#     11 12 25 32 44 45    | 1 13  7 12  1 |                        |0 2 1 1 2 |2 1 1 2
		#      8 16 25 30 42 43(1) | 8  9  5 12  1 | -3   4   0  -2  -2  -2 |1 1 1 1 2 |1 1 1 1 2
		#      3 16 22 37 38 44(1) |13  6 15  1  6 | -5   0  -3   7  -4   1 |1 1 1 2 1 |1 1 1 2 1
		#     15 27 33 35 43 45    |12  6  2  8  2 | 12  11  11  -2   5   1 |0 1 1 2 2 |1 1 2 2
		#      1  3 12 21 26 41    | 2  9  9  5 15 |-14 -24 -21 -14 -17  -4 |2 1 2 0 1 |2 1 2 1
		#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   1  -2  -9   2   4 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 11, 11, -2)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:2+1]==c(-17, -4    )) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+3]==c(  9, -2, -4)) ) cnt<-cnt+1 #  4

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextBin()

# done
fCutCnt.nextRebNum <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(41) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,0,3)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 35 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 27 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 29 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 31 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)				
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 1>
					if( fCutU.hasPtn(c( 1, 7 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA,16,42,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# < 5>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,38,NA,44   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# < 6>
					if( fCutU.hasPtn(c(  6, 8,36,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <11>
					if( fCutU.hasPtn(c( 7,11      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   11,42,41),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-3 )	# <14>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# <35>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <39>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,39),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(   1  ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(   5  ),c( 35 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 8,2  ),c( 27 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 9    ),c( 29 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,1,2),c( 31 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(   5  ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4  ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 9  ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 2, 4)) ) cnt<-cnt+1	# 17
					if( 1<sum(aCStep[1:1+0]==c( 6, 8   )) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+1]==c( 1, 3, 3)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 8, 6,19)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+2]==c(25, 2, 2)) ) cnt<-cnt+1	#  6

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      5 11 14 27 29 36    | 6  3 13  2  7 |                        |1 2 2 1 0 |1 2 2 1
		#      1  5  6 14 20 39(2) | 4  1  8  6 19 | -4  -6  -8 -13  -9   3 |3 1 1 1 0 |3 1 1 1
		#     14 15 18 21 26 35(1) | 1  3  3  5  9 | 13  10  12   7   6  -4 |0 3 2 1 0 |3 2 1
		#      1  6 11 28 34 42    | 5  5 17  6  8 |-13  -9  -7   7   8   7 |2 1 1 1 1 |2 1 1 1 1
		#      2  8 33 35 37 41    | 6 25  2  2  4 |  1   2  22   7   3  -1 |2 0 0 3 1 |2 3 1
		#      5 22 31 32 39 45    |17  9  1  7  6 |  3  14  -2  -3   2   4 |1 0 1 3 1 |1 1 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  7      ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  1      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c(  2,  4    )) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:3+1]==c( -3,-14,  2)) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+2]==c(  3, -2, -4)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+3]==c(  3, 14, -2)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( -7, -8, -7)) ) cnt<-cnt+1 #  4

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})

    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextRebNum()

# done
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	# rawVal, cStep 모두 특이하다. 주의할 것.
	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )
	
	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  1, 6,11 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  5       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 20       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 42,12,16 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 45       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 1>
					if( fCutU.hasPtn(c( 1, 5         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA, 8,NA,30),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA,NA,NA,28   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# < 6>
					if( fCutU.hasPtn(c( 6,15),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <12>
					if( fCutU.hasPtn(c( 7,NA,12,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=2 )	# <26>
					if( fCutU.hasPtn(c(13,NA,26),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <27>
					if( fCutU.hasPtn(c( 4,27,NA,29),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# <28>
					if( fCutU.hasPtn(c( 1,NA,NA,NA,28   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            28,45),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    2, 3,NA,28   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <41>
					if( fCutU.hasPtn(c(  8, 5,14,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <45>
					if( fCutU.hasPtn(c(            28,45),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,6      ),c(  1, 6,11 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5    ,0  ),c(  5       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 8        ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3,0,9    ),c( 20       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 2    ,6,3),c( 42,12,16 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5        ),c( 45       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(17 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 5,15   )) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+0]==c( 1, 3, 4)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c( 3, 6, 2)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c( 1, 3, 4)) ) cnt<-cnt+1	# 17

					if( fCutU.hasPtn(c( 2,10),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#     11 18 26 27 40 41    | 7  8  1 13  1 |                        |0 2 2 0 2 |2 2 2
		#     15 27 33 35 43 45(1) |12  6  2  8  2 |  4   9   7   8   3   4 |0 1 1 2 2 |1 1 2 2
		#      1  6 17 22 28 45(1) | 5 11  5  6 17 |-14 -21 -16 -13 -15   0 |2 1 2 0 1 |2 1 2 1
		#      6 16 37 38 41 45(2) |10 21  1  3  4 |  5  10  20  16  13   0 |1 1 0 2 2 |1 1 2 2
		#      1  3 12 21 26 41(1) | 2  9  9  5 15 | -5 -13 -25 -17 -15  -4 |2 1 2 0 1 |2 1 2 1
		#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   1  -2  -9   2   4 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(-15      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1
					# 편차가 너무 커서 건질 게 없다.
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})

    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextCStepBin()

# done
fCutCnt.nextFStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+1]==c(1,1,0)) ) return(FALSE)	# next rebind of 2,0,2
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 0,2,0
					if( all(quoSize[1:3+1]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,1,0 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  7    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  9,18 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 43,38 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )

					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 6>
					if( fCutU.hasPtn(c(  6, 7 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  6,NA,17 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  6,NA,NA,17,19,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 7>
					if( fCutU.hasPtn(c(  6, 7 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  7,17 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  7,NA,17,19,33),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 9>
					if( fCutU.hasPtn(c( 9,14,33),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=3 )	# <11>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=1 )	# <18>
					if( fCutU.hasPtn(c(   18,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,18,NA,24   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <19>
					if( fCutU.hasPtn(c( 8,NA,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      19,39,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <21>
					if( fCutU.hasPtn(c(20,21),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <31>
					if( fCutU.hasPtn(c(11,14,13,15,31),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <41>
					if( fCutU.hasPtn(c(    7,NA,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8,NA,27,11,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <43>
					if( fCutU.hasPtn(c(11, 5,25,37,43),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7,3   ),c(  7    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 9,8,6 ),c(  9,18 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,3   ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3,6,7 ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(       ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3,8   ),c( 43,38 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2   ,11,20 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 6,11,12 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(  8, 3,11 )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+0]==c(  1,11, 8 )) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c(  8, 3,11 )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 11, 1,11 )) ) cnt<-cnt+1	# 11

					if( fCutU.hasPtn(c( 1,10 ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[c(2,4)]==c( 1, 1)) ) cnt<-cnt+1
					if( all(aCStep[c(4,5)]==c(14, 4)) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#     11 18 21 36 37 43    | 7  3 15  1  6 |                        |0 2 1 2 1 |2 1 2 1
		#      3  4  7 11 31 41(1) | 1  3  4 20 10 | -8 -14 -14 -25  -6  -2 |3 1 0 1 1 |3 1 1 1
		#      7  9 10 13 31 35(2) | 2  1  3 18  4 |  4   5   3   2   0  -6 |2 2 0 2 0 |2 2 2
		#      6  7 19 21 41 43(1) | 1 12  2 20  2 | -1  -2   9   8  10   8 |2 1 1 0 2 |2 1 1 2
		#      1  9 12 23 39 43(1) | 8  3 11 16  4 | -5   2  -7   2  -2   0 |2 1 1 1 1 |2 1 1 1 1
		#      6  7 18 19 30 38    | 1 11  1 11  8 |  5  -2   6  -4  -9  -5 |2 2 0 2 0 |2 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  2      ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -2,  9,  8)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+0]==c( -4, -9, -5)) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+0]==c( -8,-10, -8)) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+3]==c(  1,  2, -9)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c( -2,  7, -2)) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:2+4]==c(  1,  2    )) ) cnt<-cnt+1 # -5

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

}	# fCutCnt.nextFStepBin( )

# done
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )


	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(37,24) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,1,3)) ) return(FALSE)	# next rebind of  0,3,2 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  8 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 39 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# < 5>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,33),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,15, 7,37 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# < 9>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <14>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <22>
					if( fCutU.hasPtn(c( 6,15,NA,22,NA,33  ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=-2 )	# <39>
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7   ),c(  8 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3   ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 9   ),c( 39 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4, 1 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 8, 4 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 7, 2,18)) ) cnt<-cnt+1	#   1
					if( 1<sum(aCStep[1:3+1]==c( 7, 2,18)) ) cnt<-cnt+1	#   1
					if( 1<sum(aCStep[1:3+2]==c( 7, 2,18)) ) cnt<-cnt+1	#   1
					if( 1<sum(aCStep[1:2+3]==c( 5, 1   )) ) cnt<-cnt+1	#   4
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     10 11 18 22 28 39    | 1  7  4  6 11 |                        |0 3 2 1 0 |3 2 1
		#      8 13 20 22 23 36(1) | 5  7  2  1 13 | -2   2   2   0  -5  -3 |1 1 3 1 0 |1 1 3 1
		#      1  5  9 21 27 35    | 4  4 12  6  8 | -7  -8 -11  -1   4  -1 |3 0 2 1 0 |3 2 1
		#      9 14 15 17 31 33(1) | 5  1  2 14  2 |  8   9   6  -4   4  -2 |1 3 0 2 0 |1 3 2
		#      5 12 14 32 34 42(1) | 7  2 18  2  8 | -4  -2  -1  15   3   9 |1 2 0 2 1 |1 2 2 1
		#      7 37 38 39 40 44    |30  1  1  1  4 |  2  25  24   7   6   2 |1 0 0 3 2 |1 3 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  2      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -8      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -8,-11, -1)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( -8, -9, -6)) ) cnt<-cnt+1 #  7
					if( 1<sum(aFStep[1:3+3]==c( -2, -1, 15)) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+3]==c( -7, -8,-11)) ) cnt<-cnt+1 #  2

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_1()

# undone
fCutCnt.nextColVal_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )


	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c( , , )) ) return(FALSE)	# next rebind of  , , 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 11) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )

					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=2 )	# <12>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=3 )	# <18>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <20>
					if( fCutU.hasPtn(c(20,NA,NA,37),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <29>
					if( fCutU.hasPtn(c(   29,NA,NA,39 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,29,34,NA,NA,37),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# <30>
					if( fCutU.hasPtn(c(16,NA,30,39),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <33>
					if( fCutU.hasPtn(c( 1,22,33),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-2 )	# <35>
					if( fCutU.hasPtn(c(28,NA,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <39>
					if( fCutU.hasPtn(c(   29,NA,NA,39 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,NA,34,29,39 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <43>
					if( fCutU.hasPtn(c( 1,24,29,31,43),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(1     ),c( 11)) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(9,7   ),c(   )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(1,3,0 ),c(   )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3     ),c(   )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(5     ),c(   )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(3     ),c(   )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(13    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 2,12 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 8, 2)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+0]==c( 6, 2, 8)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 1, 1, 6)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c( 1, 1, 6)) ) cnt<-cnt+1	#  6

					if( fCutU.hasPtn(c(6,3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 2, 8)) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      4 19 20 26 30 35    |15  1  6  4  5 |                        |1 1 2 2 0 |1 1 2 2
		#      8 13 20 22 23 36(1) | 5  7  2  1 13 |  4  -6   0  -4  -7   1 |1 1 3 1 0 |1 1 3 1
		#     18 29 30 37 39 43    |11  1  7  2  4 | 10  16  10  15  16   7 |0 1 1 3 1 |1 1 3 1
		#     12 29 32 33 39 40(2) |17  3  1  6  1 | -6   0   2  -4   0  -3 |0 1 1 3 1 |1 1 3 1
		#     15 27 33 35 43 45(1) |12  6  2  8  2 |  3  -2   1   2   4   5 |0 1 1 2 2 |1 1 2 2
		#     10 11 12 18 24 42    | 1  1  6  6 18 | -5 -16 -21 -17 -19  -3 |0 4 1 0 1 |4 1 1
		# -<standard zoid>---------------------------------------------------------------------
		#     10 21 22 30 35 42(2) |11  1  8  5  7 |  0  10  10  12  11   0 |0 1 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(-19      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  6,  0, -2)) ) cnt<-cnt+1 #-16
					if( 1<sum(aFStep[1:3+3]==c(  2,  4,  5)) ) cnt<-cnt+1 # -3
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_2()

# done
fCutCnt.nextColVal_3 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )


	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,1,1)) ) return(FALSE)	# next rebind of  2,2,0 
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of  2,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  8 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 14 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 17 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 36 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# < 6>
					if( fCutU.hasPtn(c( 6,NA,21,NA,38),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# < 7>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <11>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <14>
					if( fCutU.hasPtn(c(13,14),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=3 )	# <15>
					if( fCutU.hasPtn(c(15,37,39),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <33>
					if( fCutU.hasPtn(c(33,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <35>
					if( fCutU.hasPtn(c(33,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <41>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <43>
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(8,9,5 ),c(  8 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(3     ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(4     ),c( 14 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3     ),c( 17 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(0,6   ),c( 36 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(1     ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 2, 1, 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 9, 5)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+0]==c( 6,25, 2)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+1]==c(12, 2,20)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:2+3]==c(12, 6,  )) ) cnt<-cnt+1	# 5

					if( fCutU.hasPtn(c(3,3),aCStep) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      5  6 11 17 38 44    | 1  5  6 21  6 |                        |2 2 0 1 1 |2 2 1 1
		#     15 27 33 35 43 45    |12  6  2  8  2 | 10  21  22  18   5   1 |0 1 1 2 2 |1 1 2 2
		#      3 11 14 15 32 36(1) | 8  3  1 17  4 |-12 -16 -19 -20 -11  -9 |1 3 0 2 0 |1 3 2
		#      2  8 33 35 37 41    | 6 25  2  2  4 | -1  -3  19  20   5   5 |2 0 0 3 1 |2 3 1
		#      6  7 19 21 41 43(1) | 1 12  2 20  2 |  4  -1 -14 -14   4   2 |2 1 1 0 2 |2 1 1 2
		#      7  9 12 14 23 28(1) | 2  3  2  9  5 |  1   2  -7  -7 -18 -15 |2 2 2 0 0 |2 2 2

		# -<standard zoid>---------------------------------------------------------------------
		#     10 21 22 30 35 42    |11  1  8  5  7 |  3  12  10  16  12  14 |0 1 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( -7      ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -2,  7,  7)) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+0]==c(-18,-15    )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( 19, 20, 11)) ) cnt<-cnt+1 #-18

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_3()

# done
fCutCnt.nextColVal_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of  2,1,1 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  6 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 20 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 12 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 37 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# < 7>
					if( fCutU.hasPtn(c( 7,NA,NA,32),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,30,38),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 8>
					if( fCutU.hasPtn(c( 8,21,28,18,16,23),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <13>
					if( fCutU.hasPtn(c(12,13,40,44,44),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <14>
					if( fCutU.hasPtn(c(14,40,42),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <17>
					if( fCutU.hasPtn(c(17,21),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(17,NA,26   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(17,NA,NA,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <21>
					if( fCutU.hasPtn(c(17,21),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   21,26   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   21,NA,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <27>
					if( fCutU.hasPtn(c(27,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# <34>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <37>
					if( fCutU.hasPtn(c( 3, 6, 6,20,37),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(7,5,6   ),c(  6 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(0,2     ),c( 20 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(2       ),c( 12 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(2       ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(7    ,0 ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(7    ,8 ),c( 37 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(       4,10 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(       1, 4 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 5   , 1    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1,10       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1, 1, 9)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 4, 1, 1)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+1]==c( 1, 1, 9)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 1, 1, 9)) ) cnt<-cnt+1	# 1

					if( 1<sum(aCStep[1:3+0]==c( 4, 1, 1)) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 9 ) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(1,9)) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      1  7 14 20 34 37    | 6  7  6 14  3 |                        |2 1 1 2 0 |2 1 1 2
		#      8 13 14 30 38 39(1) | 5  1 16  8  1 |  7   6   0  10   4   2 |1 2 0 3 0 |1 2 3
		#      8 17 21 24 27 31(1) | 9  4  3  3  4 |  0   4   7  -6 -11  -8 |1 1 3 1 0 |1 1 3 1
		#      5 10 13 27 37 41(1) | 5  3 14 10  4 | -3  -7  -8   3  10  10 |1 2 1 1 1 |1 2 1 1 1
		#      7 22 29 33 34 35    |15  7  4  1  1 |  2  12  16   6  -3  -6 |1 0 2 3 0 |1 2 3
		#     17 21 25 26 27 36    | 4  4  1  1  9 | 10  -1  -4  -7  -7   1 |0 1 4 1 0 |1 4 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  -8     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(   0     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c( -3, -6    )) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+1]==c(  3,  7,  8)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+2]==c(  2, 12, 16)) ) cnt<-cnt+1 # -7
					if( 1<sum(aFStep[1:3+3]==c(  2, 12, 16)) ) cnt<-cnt+1 # -7

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_4()

# done
fCutCnt.nextColVal_5 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 41 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,3,1)) ) return(FALSE)	# next rebind of 2,2,0 
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,1,1 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 9,36 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# < 2>
					if( fCutU.hasPtn(c( 2,25,14, 7,31,37),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# < 7>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# < 9>
					if( fCutU.hasPtn(c( 9,NA,NA,19,25 ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <19>
					if( fCutU.hasPtn(c(19,23),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <24>
					if( fCutU.hasPtn(c(24,33,39),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# <31>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <32>
					if( fCutU.hasPtn(c(19,NA,32,42),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=2 )	# <41>
					if( fCutU.hasPtn(c(36,39,NA,41),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <45>
					if( fCutU.hasPtn(c( 8,35,38,23,35,45),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(        ),c(      )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(9,7,6   ),c( 9,36 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(    2   ),c(      )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(    1   ),c(      )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(    0,2 ),c(      )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(5,6     ),c(      )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 6 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,15, 9)) ) cnt<-cnt+1	# 17
					if( 1<sum(aCStep[1:2+0]==c( 4, 4   )) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+1]==c(15, 2, 5)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c(11, 6, 3)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:2+3]==c( 2, 1   )) ) cnt<-cnt+1	#  6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      2  9 24 41 43 45    | 7 15 17  2  2 |                        |2 0 1 0 3 |2 1 3
		#      3  7 14 16 31 40    | 4  7  2 15  9 |  1  -2 -10 -25 -12  -5 |2 2 0 1 1 |2 2 1 1
		#      8 19 25 28 32 36    |11  6  3  4  4 |  5  12  11  12   1  -4 |1 1 2 2 0 |1 1 2 2
		#      7  9 10 13 31 35    | 2  1  3 18  4 | -1 -10 -15 -15  -1  -1 |2 2 0 2 0 |2 2 2
		#      2 17 19 24 37 41    |15  2  5 13  4 | -5   8   9  11   6   6 |1 2 1 1 1 |1 2 1 1 1
		#      5 22 31 32 39 45    |17  9  1  7  6 |  3   5  12   8   2   4 |1 0 1 3 1 |1 1 3 1
		# -<standard zoid>---------------------------------------------------------------------
		#     10 21 22 30 35 42(1) |11  1  8  5  7 |  5  -1  -9  -2  -4  -3 |0 1 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( 13      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  7      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( 10      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c( -3, -5,-12)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+1]==c( -1,-10,-15)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c(  3,  5, 12)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+3]==c( -5,-12,-11)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( 15,  1,  1)) ) cnt<-cnt+1 #  4

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_5()

# done
fCutCnt.nextColVal_6 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,3,0)) ) return(FALSE)	# next rebind of  1,2,1
					if( all(quoSize[1:3+1]==c(3,0,0)) ) return(FALSE)	# next rebind of  2,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1, 3 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 2    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 1>
					if( fCutU.hasPtn(c( 1, 2  ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA,14,16,NA,41),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# < 3>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <10>
					if( fCutU.hasPtn(c( 6,10,NA,34),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <12>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <14>
					if( fCutU.hasPtn(c( 4,NA,14),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <17>
					if( fCutU.hasPtn(c(17,NA,22,NA,NA,28),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <25>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=1 )	# <27>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <28>
					if( fCutU.hasPtn(c(24,NA,28,29),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# <36>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <43>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(1,3,1,0    ),c( 1, 3 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(2    ,6,3  ),c( 2    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(7    ,6,4,1),c(      )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(7          ),c(      )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(8    ,3    ),c(      )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(      4    ),c(      )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1          ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 2, 8,10    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(          1 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 6, 2,16)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c( 3, 6, 2)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c( 2, 2,27)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 2, 2,27)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:2+3]==c( 4, 4   )) ) cnt<-cnt+1	#  9

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      3  8 19 27 30 41    | 5 11  8  3 11 |                        |2 1 1 1 1 |2 1 1 1 1
		#      2 10 14 22 32 36    | 8  4  8 10  4 | -1   2  -5  -5   2  -5 |1 2 1 2 0 |1 2 1 2
		#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -1  -6  -4 -10  -4   9 |2 2 1 0 1 |2 2 1 1
		#     17 25 28 37 43 44(1) | 8  3  9  6  1 | 16  21  18  25  15  -1 |0 1 2 1 2 |1 2 1 2
		#      1  3 12 14 16 43(1) | 2  9  2  2 27 |-16 -22 -16 -23 -27  -1 |2 3 0 0 1 |2 3 1
		#     17 21 25 26 27 36    | 4  4  1  1  9 | 16  18  13  12  11  -7 |0 1 4 1 0 |1 4 1
		# -<standard zoid>---------------------------------------------------------------------
		#     10 21 22 30 35 42(1) |11  1  8  5  7 | -7   0  -3   4   8   6 |0 1 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -1      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -7      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-16 -22 -16)) ) cnt<-cnt+1 # 16
					if( 1<sum(aFStep[1:3+0]==c(-22 -16 -23)) ) cnt<-cnt+1 # 18

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<sum(cntMtx[idx,c("raw","rawFV")]) ){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextColVal_6()

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



