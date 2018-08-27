# toZ822_H.R 최종접근
cntThld <- c(2,2,3,2,2)	;names(cntThld) <- c("raw","rawFV","rem","cStep","fStep")

# 공용
# undone
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

# done *
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 0,1,2
					if( all(quoSize[1:3+1]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(0,1,1)) ) return(FALSE)	# next rebind of 2,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 30 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 28 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <12>
					if( fCutU.hasPtn(c(12,13   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,NA,23),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <13>
					if( fCutU.hasPtn(c(12,13   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,23),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <25>
					if( fCutU.hasPtn(c(17,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# <29>
					if( fCutU.hasPtn(c( 9,NA,20,29),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# <30>
					if( fCutU.hasPtn(c(14,15,30),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(2,8  ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(3,0  ),c( 30 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(4,5,3),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(7    ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(0    ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c( 28 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 11, 9    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  1, 8,11 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 11       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  5, 2    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  5       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(11,  1, 11)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+0]==c(11,  1, 11)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c(11,  1, 11)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c(11,  5, 15)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c( 11, 1 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  8, 5 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  3, 1 ),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+2]==c( 1,11 )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     12 18 19 29 31 39    | 6  1 10  2  8 |                        |0 3 1 2 0 |3 1 2
				#      3  9 12 13 25 43(1) | 6  3  1 12 18 | -9  -9  -7 -16  -6   4 |2 2 1 0 1 |2 2 1 1
				#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 11   6  13  15   4 -13 |0 2 3 1 0 |2 3 1
				#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  2  10   8  10  11  15 |0 1 1 2 2 |1 1 2 2
				#     10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1
				#      1 12 13 24 29 44    |11  1 11  5 15 | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -10 ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  -7 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  -7 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 11,   6,  13 )) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+0]==c( 11,   6,  13 )) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+1]==c( 11,   6,  13 )) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+3]==c( -9,  -9,  -9 )) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:2+4]==c( -9,  -9      )) ) cnt<-cnt+1 # -6

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

# undone
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
    score <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					# if( 1<sum(aZoid[c(1,2,3,4,5,6)]==c( 1,32,26,13,23,34)) ) cnt<-cnt+1
					# if( 1<sum(aZoid[c(1,2,  4,5,6)]==c( 3,32,   23,40,21)) ) cnt<-cnt+1
					# if( 1<sum(aZoid[c(1,2,  4,5,6)]==c(11,24,   41,20,31)) ) cnt<-cnt+1
					# if( 1<sum(aZoid[c(  2,  4,5,6)]==c(   31,   27,41,45)) ) cnt<-cnt+1
					# if( 1<sum(aZoid[c(        5,6)]==c(            34,45)) ) cnt<-cnt+1

					return( 1>cnt )
				})	;kIdx<-anaFltCnt(score,rpt)
    flgCnt <- flgCnt + score

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
					if( aZoid[1]%in%c(  ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(  ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(  ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(  ) ) cnt<-cnt+1

					if( fCutU.hasPtn(c(23,27),aZoid) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[ idx ]]$fndMtx )
					if( all(aZoid[1:2+1]==c(17,20)) ) cnt<-cnt+1
					if( all(aZoid[1:2+3]==c(22,37)) ) cnt<-cnt+1

					remCnt <- 0
						# if( fCutU.remFilt(aZoid[1],c(     ),c(   )) )	remCnt <- remCnt+1
						# if( fCutU.remFilt(aZoid[2],c(     ),c(   )) )	remCnt <- remCnt+1
						# if( fCutU.remFilt(aZoid[3],c(     ),c(   )) )	remCnt <- remCnt+1
						# if( fCutU.remFilt(aZoid[4],c( 1   ),c(   )) )	remCnt <- remCnt+1
						# if( fCutU.remFilt(aZoid[5],c( 9,8 ),c(   )) )	remCnt <- remCnt+1
						# if( fCutU.remFilt(aZoid[6],c(     ),c(   )) )	remCnt <- remCnt+1					
						# # grp 1
						# # grp 2
						# if( aZoid[3]==20 && fCutU.remFilt(aZoid[2],c( 7),c(17)) ) remCnt <- remCnt+1 
						# # grp 5
						# if( aZoid[6]==37 && fCutU.remFilt(aZoid[4],c( 2),c(22)) ) remCnt <- remCnt+1 
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	[1]
					#	[2]  3   4  10   3   4  20   7   5 
					#	[3]  1   2 
					#	[4]  7   4   4   2 
					#	[5]  4  14   6   1  13 
					aCStep <- aZoid[2:6]-aZoid[1:5]

					# score <- sum(aCStep==c(NA, 3, 1, 7, 4),na.rm=T)

					# matCnt <- sum(aCStep==c(NA, 4, 2, 4,14),na.rm=T)
					# score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					# matCnt <- sum(aCStep==c(NA,10,NA, 4, 6),na.rm=T)
					# score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					# matCnt <- sum(aCStep==c(NA, 3,NA, 2, 1),na.rm=T)
					# score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					# matCnt <- sum(aCStep==c(NA, 4,NA,NA,13),na.rm=T)
					# score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )


					cnt <- 0
						# if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1
						# if( aCStep[2]%in%c( 2   ) ) cnt<-cnt+1
						# if( aCStep[3]%in%c(     ) ) cnt<-cnt+1
						# if( aCStep[4]%in%c( 7   ) ) cnt<-cnt+1
						# if( aCStep[5]%in%c(     ) ) cnt<-cnt+1

						if( 2<sum(aCStep[1:3+2]==c( 3, 1, 7 )) ) cnt<-cnt+1 # 4
					score <- score + ifelse(cnt>1,1,0)

					return( score )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1


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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					# [  1]             21 31 32                17 19 20
					if( all(aCStep[1:2+3]==c(1,1)) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1


	return( flgCnt )
} # fCutCnt.colValSeqNext()                     

# undone
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

					tCnt <- 0
						if( aCStep[1]%in%c( 4, 1,15 ) ) tCnt<-tCnt+1	# 마지막 값 연속도 포함.
						if( aCStep[2]%in%c( 4, 2    ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c( 4       ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(         ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c( 4, 6    ) ) tCnt<-tCnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# [1]*   4  4  1  4  7
					# [2]*   4  4  2 12 16  7 12  2  8  8  5  4  6  1 12  4  2  2  8  8  5  8  5  3  6  9 13 11  2  2  7
					# [3]*   7  4  5 19  7  8  9 14  7  8  2  1  9  2  2 10  5  1 16  3  3  7  3  3
					# [4]*  12  4 15 11  1  9 10  9  1 19 22  1  8  2 14  8  5  2  6  2 20  1  3  5  2  1  4  1  1  9  4 11  3
					# [5]*  13  3  2  2 10  3  6  6 16  6 13  7  7  3  7  2
					matCnt<-sum(aCStep==c( 4, 4, 7,12,13),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 4, 4, 4, 4, 3),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 1, 2, 5,15, 2),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 4,12,19,11, 2),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 7,16, 7, 1,10),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 7, 8, 9, 3),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA,12, 9,10, 6),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 2,14, 9, 6),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 8, 7, 1,16),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 8, 8,19, 6),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 5, 2,22,13),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 4, 1, 1, 7),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 6, 9, 8, 7),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 1, 2, 2, 3),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA,12, 2,14, 7),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 2, 5, 5,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 2, 1, 2,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 8,16, 6,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 8, 3, 2,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 5, 3,20,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 8, 7, 1,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 5, 3, 3,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(NA, 3, 3, 5,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)

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
					if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1,14, 4 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 6       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1       ) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[1]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[2]]$fndMtx )
					if( all(aCStep[1:2+1]==c( 2,13)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 4, 3)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 4, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 1, 3)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 2, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(14,13)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[3]]$fndMtx )
					if( all(aCStep[1:2+2]==c( 2, 6)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 2, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 4, 4)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[4]]$fndMtx )
					# if( all(aCStep[1:2+3]==c( , )) ) cnt<-cnt+1

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

# done *
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
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(1,4,1)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,4,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(23      ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 8,18,25) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(25      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(21      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(27      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# < 8>
					if( fCutU.hasPtn(c( 8,28,35,39),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=2 )	# <10>
					if( fCutU.hasPtn(c(10,20      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,NA,12,26),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-3 )	# <17>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <19>
					if( fCutU.hasPtn(c( 6,NA,NA,29),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,19,33   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-3 )	# <21>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <27>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <30>
					if( fCutU.hasPtn(c( 4,24,NA,30,43),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=2 )	# <36>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <39>
					if( fCutU.hasPtn(c(39,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,10,12,24,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt) 
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(         ),c( 23       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(8,5      ),c(  8,18,25 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(9,0      ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(5,8,6    ),c( 25       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(1        ),c( 21       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(1,4,7,5,6),c( 27       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3   ,10 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(12, 5    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c( 5, 11,  8)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 5, 11,  8)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 2,  1, 12)) ) cnt<-cnt+1	#  9

					if( (aCStep[1]==aCStep[2]) && (aCStep[3]==aCStep[4]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     10 18 30 36 39 44    | 8 12  6  3  5 |                        |0 2 0 3 1 |2 3 1
				#     12 14 21 30 39 43(2) | 2  7  9  9  4 |  2  -4  -9  -6   0  -1 |0 2 1 2 1 |2 1 2 1
				#      7  8 10 19 21 31(1) | 1  2  9  2 10 | -5  -6 -11 -11 -18 -12 |2 2 1 1 0 |2 2 1 1
				#      2  4  5 17 27 32    | 2  1 12 10  5 | -5  -4  -5  -2   6   1 |3 1 1 1 0 |3 1 1 1
				#      3  8 19 27 30 41(1) | 5 11  8  3 11 |  1   4  14  10   3   9 |2 1 1 1 1 |2 1 1 1 1
				#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 14  13   6  -1  -3  -5 |0 1 4 1 0 |1 4 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  12 ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -12 ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  6,  -1,  -3)) ) cnt<-cnt+1 # 14
					if( 1<sum(aFStep[1:3+1]==c( 10,   3,   9)) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+1]==c(-11, -18, -12)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(  1,   3,   5)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+3]==c(  1,   4,  14)) ) cnt<-cnt+1 # -5
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

# done *
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
					if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  6,12 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 12    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 23    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 29,41 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# < 1>
					if( fCutU.hasPtn(c( 1,NA,NA,NA,30),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,16,16,35   ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 7>
					if( fCutU.hasPtn(c( 7,27,34,17,27),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <11>
					if( fCutU.hasPtn(c(10,11,12),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=-1 )	# <13>
					if( fCutU.hasPtn(c(13,20,16),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <19>
					if( fCutU.hasPtn(c(19,29,24,31),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <29>
					if( fCutU.hasPtn(c(      23,29),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(17, 7,NA,29),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <41>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# <44>
					if( fCutU.hasPtn(c( 5, 5,14,17,44),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(0     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(6,2   ),c(  6,12 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(2     ),c( 12    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3,6   ),c( 23    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(9,1   ),c( 29,41 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(8     ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  6    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 12,11 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 10    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  6    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  4    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+2]==c(12,  6,  4)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 9,  3,  9)) ) cnt<-cnt+1	# 15

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      1  8 10 13 28 42    | 7  2  3 15 14 |                        |2 2 1 0 1 |2 2 1 1
				#      4 11 20 23 32 39    | 7  9  3  9  7 |  3   3  10  10   4  -3 |1 1 2 2 0 |1 1 2 2
				#     15 19 21 34 41 44    | 4  2 13  7  3 | 11   8   1  11   9   5 |0 2 1 1 2 |2 1 1 2
				#      5  7 11 16 41 45(1) | 2  4  5 25  4 |-10 -12 -10 -18   0   1 |2 2 0 0 2 |2 2 2
				#      2  7 19 25 29 36(1) | 5 12  6  4  7 | -3   0   8   9 -12  -9 |2 1 2 1 0 |2 1 2 1
				#      1 12 13 24 29 44(1) |11  1 11  5 15 | -1   5  -6  -1   0   8 |1 2 2 0 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 10,  18,   0)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+2]==c( 12,  10,  18)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c( -1,   5,  -6)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+3]==c( -1,   5,  -6)) ) cnt<-cnt+1 #  8
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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 0,1,2 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 37 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <12>
					if( fCutU.hasPtn(c(12,NA,28,NA,27),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <14>
					if( fCutU.hasPtn(c(14,16,20,28,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <17>
					if( fCutU.hasPtn(c(17,25,32,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <19>
					if( fCutU.hasPtn(c(      19,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11,NA,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,19      ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-2 )	# <20>
					if( fCutU.hasPtn(c( 9,20,NA,21),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <23>
					if( fCutU.hasPtn(c(15,NA,23      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      23,NA,38),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <24>
					if( fCutU.hasPtn(c(      24,25   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7, 7,24,NA,37),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=-2 )	# <31>
					if( fCutU.hasPtn(c( 7,16,19,31),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <39>
					if( fCutU.hasPtn(c(      19,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,NA,NA,39),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4    ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,4  ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 1    ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(      ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 1,4  ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 7    ),c( 37 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8, 9, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 8, 2, 1 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 8       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  8, 11)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+1]==c( 8,  4,  6)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+1]==c( 3,  8,  6)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+2]==c(10,  2,  8)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:2+4]==c( 6,  3    )) ) cnt<-cnt+1	# 1

					if( fCutU.hasPtn(c( 3, 9),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      8 17 21 24 27 31    | 9  4  3  3  4 |                        |1 1 3 1 0 |1 1 3 1
				#      9 12 19 20 39 41    | 3  7  1 19  2 |  1  -5  -2  -4  12  10 |1 2 1 1 1 |1 2 1 1 1
				#      5 13 17 23 28 36    | 8  4  6  5  8 | -4   1  -2   3 -11  -5 |1 2 2 1 0 |1 2 2 1
				#     12 14 24 26 34 45    | 2 10  2  8 11 |  7   1   7   3   6   9 |0 2 2 1 1 |2 2 1 1
				#      2 10 11 19 35 39    | 8  1  8 16  4 |-10  -4 -13  -7   1  -6 |1 3 0 2 0 |1 3 2
				#     14 20 23 31 37 38    | 6  3  8  6  1 | 12  10  12  12   2  -1 |0 1 2 3 0 |1 2 3
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  3 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c(-11,  -5     )) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:2+1]==c(-11,  -5     )) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+1]==c(  3, -11,  -5)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c(  3, -11,  -5)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+3]==c( -1,  -7,  -3)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c(-10,  -4     )) ) cnt<-cnt+1 # -1

					if( fCutU.hasPtn(c(12,10),aFStep) ) cnt<-cnt+1

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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,0 reverse
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,0,2 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1, 5 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 2,15 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(16    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(42    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 2>
					if( fCutU.hasPtn(c( 2, 7         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,NA,NA,NA,43),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# < 5>
					if( fCutU.hasPtn(c( 5,24,31),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 7>
					if( fCutU.hasPtn(c( 2, 7         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    7,NA,NA,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    7,NA,NA,41),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# < 9>
					if( fCutU.hasPtn(c( 8, 9         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    9,NA,11,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <11>
					if( fCutU.hasPtn(c( 7, 8,NA,11,NA,33),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <13>
					if( fCutU.hasPtn(c( 4,13,19),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-3 )	# <15>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <31>
					if( fCutU.hasPtn(c(         31,33),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,20,27,31   ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <41>
					if( fCutU.hasPtn(c( 7,NA,NA,41),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <42>
					if( fCutU.hasPtn(c(28,33,NA,NA,42),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(1,5  ),c(  1, 5 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(5,7  ),c(  2,15 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(7,0  ),c( 16    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(7,3  ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(1,3,2),c( 42    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(5,1,3),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(10       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4,10    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(10, 3, 5 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(16, 8    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 8, 2    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+1]==c(10  5  )) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:2+2]==c( 3  8  )) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	#  8

					if( fCutU.hasPtn(c( 1, 3),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3  4  7 11 31 41    | 1  3  4 20 10 |                        |3 1 0 1 1 |3 1 1 1
				#      5  6  9 11 15 37(1) | 1  3  2  4 22 |  2   2   2   0 -16  -4 |3 2 0 1 0 |3 2 1
				#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  2   3   1   2  16  -2 |2 2 0 2 0 |2 2 2
				#      2  7 27 33 41 44(1) | 5 20  6  8  3 | -5  -2  17  20  10   9 |2 0 1 1 2 |2 1 1 2
				#      2  7 13 25 42 45(2) | 5  6 12 17  3 |  0   0 -14  -8   1   1 |2 1 1 0 2 |2 1 1 2
				#      5 15 20 31 34 42(1) |10  5 11  3  8 |  3   8   7   6  -8  -3 |1 1 1 2 1 |1 1 1 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  -8     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -5,  -2     )) ) cnt<-cnt+1 # 3
					if( 1<sum(aFStep[1:3+0]==c( -7,  -6,   8)) ) cnt<-cnt+1 # 8
					if( 1<sum(aFStep[1:3+3]==c(  7,   6,  -8)) ) cnt<-cnt+1 #-8
					if( 1<sum(aFStep[1:2+4]==c( -5,  -2     )) ) cnt<-cnt+1 #-3
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

# done *
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
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 0,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  2    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 20    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 24,27 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 36,10 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 37,38 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# < 2>
					if( fCutU.hasPtn(c( 2,17,13,26),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# < 3>
					if( fCutU.hasPtn(c( 3,11      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,NA,25),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <10>
					if( fCutU.hasPtn(c(   10,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,10            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,NA,14,39   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-4 )	# <12>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <14>
					if( fCutU.hasPtn(c(14,16,20,28,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# <19>
					if( fCutU.hasPtn(c(12, 9,19,NA,35),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=-3 )	# <20>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <23>
					if( fCutU.hasPtn(c(23,NA,NA,37),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(23,38,43   ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <24>
					if( fCutU.hasPtn(c( 8,NA,24,36,44),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <31>
					if( fCutU.hasPtn(c(      22,31   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,17,NA,31,35),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <39>
					if( fCutU.hasPtn(c(   10,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,NA,NA,14,NA,39),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(2,4     ),c(  2    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(0,4     ),c( 20    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(3,4     ),c( 24,27 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(        ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(6,2,1,0 ),c( 36,10 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(7,8     ),c( 37,38 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 8, 1 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  8, 11)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c( 2,  8, 11)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c( 3,  8,  6)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:2+3]==c( 2,  8    )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:2+3]==c( 6,  3    )) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 6, 3),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 1),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4  8 13 19 20 43    | 4  5  6  1 23 |                        |2 2 1 0 1 |2 2 1 1
				#      1  2  3  9 12 23    | 1  1  6  3 11 | -3  -6 -10 -10  -8 -20 |4 1 1 0 0 |4 1 1
				#     12 14 24 26 34 45(1) | 2 10  2  8 11 | 11  12  21  17  22  22 |0 2 2 1 1 |2 2 1 1
				#      3 10 23 24 31 39(1) | 7 13  1  7  8 | -9  -4  -1  -2  -3  -6 |1 1 2 2 0 |1 1 2 2
				#      2 10 11 19 35 39(2) | 8  1  8 16  4 | -1   0 -12  -5   4   0 |1 3 0 2 0 |1 3 2
				#     14 20 23 31 37 38    | 6  3  8  6  1 | 12  10  12  12   2  -1 |0 1 2 3 0 |1 2 3
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -4  -1  -2)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+0]==c(-12 -21 -17)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+0]==c(-21 -17 -22)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+1]==c( -9  -4  -1)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c( -9  -4  -1)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+3]==c( 12   5  -4)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( -1   0 -12)) ) cnt<-cnt+1 # -1

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

# done *
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
					if( all(quoSize[1:3+2]==c(2,3,1)) ) return(FALSE)	# next rebind of 2,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 16       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 38,25    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 44,28,12 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 43       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <12>
					if( fCutU.hasPtn(c( 9,12,NA,NA,26,29),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <15>
					if( fCutU.hasPtn(c(15,26         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,NA,25,26,27),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <17>
					if( fCutU.hasPtn(c(17,NA,NA,37),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <25>
					if( fCutU.hasPtn(c(25,28),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=1 )	# <28>
					if( fCutU.hasPtn(c(25,28),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <33>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-2 )	# <37>
					if( fCutU.hasPtn(c(17,NA,NA,37),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# <44>
					if( fCutU.hasPtn(c(30,NA,38,NA,NA,44),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(         ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5,2     ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 6       ),c( 16       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 9,8,5,6 ),c( 38,25    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4,8,2   ),c( 44,28,12 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3,9     ),c( 43       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 7,11       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1          ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 6,20, 3, 6 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 3, 9    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 3,  6,  3)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+0]==c( 9,  7,  1)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+1]==c( 1, 10,  3)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c(21,  3,  6)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:2+3]==c( 3,  6    )) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 1, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 3, 6)) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4  8 18 19 39 44    | 4 10  1 20  5 |                        |2 2 0 1 1 |2 2 1 1
				#     15 24 31 32 33 40    | 9  7  1  1  7 | 11  16  13  13  -6  -4 |0 1 1 3 1 |1 1 3 1
				#      3 12 33 36 42 45(1) | 9 21  3  6  3 |-12 -12   2   4   9   5 |1 1 0 2 2 |1 1 2 2
				#      6 12 17 21 34 37(1) | 6  5  4 13  3 |  3   0 -16 -15  -8  -8 |1 2 1 2 0 |1 2 1 2
				#     17 25 28 37 43 44(2) | 8  3  9  6  1 | 11  13  11  16   9   7 |0 1 2 1 2 |1 2 1 2
				#     14 15 25 28 29 30(2) | 1 10  3  1  1 | -3 -10  -3  -9 -14 -14 |0 2 3 1 0 |2 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-11, -13, -11)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+2]==c(-11, -13, -11)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+2]==c( -9, -14, -14)) ) cnt<-cnt+1 # -9

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

# done *
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 31 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 45 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-2 )	# <17>
					if( fCutU.hasPtn(c(17,32,35,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=0 )	# <18>
					if( fCutU.hasPtn(c(14, 9,18,41,35),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <30>
					if( fCutU.hasPtn(c(   30,34      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   30,NA,35   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,30,NA,NA,45),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=2 )	# <31>
					if( fCutU.hasPtn(c(31,32),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=2 )	# <32>
					if( fCutU.hasPtn(c(31,32),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <34>
					if( fCutU.hasPtn(c(   30,34      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      34,35   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,NA,34,NA,45),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <35>
					if( fCutU.hasPtn(c(   30,NA,35   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      34,35   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,NA,NA,35,45),aZoid,thld=3,fixIdx) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <39>
					if( fCutU.hasPtn(c( 1,14,28,29,39),aZoid,thld=3,fixIdx5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <44>
					if( fCutU.hasPtn(c(            41,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,35,40,33,NA,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt) 
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(       ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 2,0   ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5     ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 2     ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3,1,5 ),c( 31 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,2,6 ),c( 45 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1, 8 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c(17,  9,  1)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+1]==c(19,  4,  1)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 9,  6,  1)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+2]==c( 1,  3,  8)) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 4, 1 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 7 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9, 1 ),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+0]==c( 1, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 7, 1)) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      9 30 34 35 39 41    |21  4  1  4  2 |                        |1 0 0 4 1 |1 4 1
				#      6 10 17 18 21 29    | 4  7  1  3  8 | -3 -20 -17 -17 -18 -12 |1 3 2 0 0 |1 3 2
				#      5 22 31 32 39 45    |17  9  1  7  6 | -1  12  14  14  18  16 |1 0 1 3 1 |1 1 3 1
				#     17 25 28 37 43 44    | 8  3  9  6  1 | 12   3  -3   5   4  -1 |0 1 2 1 2 |1 2 1 2
				#      3 12 13 18 31 32    | 9  1  5 13  1 |-14 -13 -15 -19 -12 -12 |1 3 0 2 0 |1 3 2
				#     11 30 34 35 42 44    |19  4  1  7  2 |  8  18  21  17  11  12 |0 1 0 3 2 |1 3 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  5,   4,  -1)) ) cnt<-cnt+1 # 18
					if( 1<sum(aFStep[1:3+2]==c(-12, -14, -14)) ) cnt<-cnt+1 # 17
					if( 1<sum(aFStep[1:3+2]==c(-14, -14, -18)) ) cnt<-cnt+1 # 17
					if( 1<sum(aFStep[1:2+4]==c( 12,   3     )) ) cnt<-cnt+1 # 12
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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,3,0 reverse
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 13, 5,21 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 17       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 20       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 21,14    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 26,28    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff= )	# < 9>
					if( fCutU.hasPtn(c( 9,24,28,33),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff= )	# <12>
					if( fCutU.hasPtn(c(12,NA,NA,28),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,16      ),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff= )	# <14>
					if( fCutU.hasPtn(c(14,17,18),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff= )	# <15>
					if( fCutU.hasPtn(c(15,16),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff= )	# <18>
					if( fCutU.hasPtn(c( 4, 3,18,NA,22,38),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff= )	# <21>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff= )	# <23>
					if( fCutU.hasPtn(c(16,NA,23,34),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff= )	# <28>
					if( fCutU.hasPtn(c(12,NA,NA,28),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,21,24,28),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff= )	# <37>
					if( fCutU.hasPtn(c(   19,NA,NA,37),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(22,NA,11,26,37),aZoid,thld=3,fixIdx5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,5,0     ),c( 13, 5,21 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 7,2       ),c( 17       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 0         ),c( 20       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,6,4     ),c( 21,14    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 7,8,6,0,8 ),c( 26,28    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(           ),c(          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
     tgt.col banVal                               descript tgt.dir
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3, 4    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1, 4    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 5, 6, 1 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 7, 12    )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c(14,  1,  1)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+0]==c( 3,  7, 12)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 8,  6,  1)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c( 2,  2,  2)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 2,  2,  6)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 3, 9),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7  9 12 14 23 28    | 2  3  2  9  5 |                        |2 2 2 0 0 |2 2 2
				#     12 15 18 28 34 42(2) | 3  3 10  6  8 |  5   6   6  14  11  14 |0 3 1 1 1 |3 1 1 1
				#      8  9 18 21 28 40(2) | 1  9  3  7 12 | -4  -6   0  -7  -6  -2 |2 1 2 0 1 |2 1 2 1
				#      6 21 35 36 37 41(1) |15 14  1  1  4 | -2  12  17  15   9   1 |1 0 1 3 1 |1 1 3 1
				#     15 17 19 21 27 45(1) | 2  2  2  6 18 |  9  -4 -16 -15 -10   4 |0 3 2 0 1 |3 2 1
				#     14 20 23 31 37 38    | 6  3  8  6  1 | -1   3   4  10  10  -7 |0 1 2 3 0 |1 2 3
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 10,  10,  -7)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+2]==c(-10, -10,   7)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+3]==c(-10, -10,   7)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+3]==c( 12,  17,  15)) ) cnt<-cnt+1 # -7

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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,2,0)) ) return(FALSE)	# next rebind of 2,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 19 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 18 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 35 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <11>
					if( fCutU.hasPtn(c(   11,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,11,NA,NA,33),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <12>
					if( fCutU.hasPtn(c(12,NA,NA,36,30),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <14>
					if( fCutU.hasPtn(c(14,16,20,28,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <19>
					if( fCutU.hasPtn(c(   11,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,NA,19,NA,33),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=2 )	# <21>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=-1 )	# <26>
					if( fCutU.hasPtn(c(18,26,38,35),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <31>
					if( fCutU.hasPtn(c(16,27,28,31,42),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=1 )	# <36>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=0 )	# <45>
					if( fCutU.hasPtn(c( 4, 8,14,16,38,45),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4     ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 2,1,9 ),c( 19 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 2,9   ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,8   ),c( 18 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 5     ),c( 35 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1     ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8,13 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1,11 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 8,  2, 15)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+0]==c( 8,  2, 15)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+1]==c( 1,  8, 16)) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 3, 8),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     12 14 24 26 34 45    | 2 10  2  8 11 |                        |0 2 2 1 1 |2 2 1 1
				#      2 10 11 19 35 39    | 8  1  8 16  4 |-10  -4 -13  -7   1  -6 |1 3 0 2 0 |1 3 2
				#      1 21 26 36 40 41    |20  5 10  4  1 | -1  11  15  17   5   2 |1 0 2 1 2 |1 2 1 2
				#      3 12 13 18 31 32    | 9  1  5 13  1 |  2  -9 -13 -18  -9  -9 |1 3 0 2 0 |1 3 2
				#     14 20 23 31 37 38(1) | 6  3  8  6  1 | 11   8  10  13   6   6 |0 1 2 3 0 |1 2 3
				#      8 11 19 21 36 45    | 3  8  2 15  9 | -6  -9  -4 -10  -1   7 |1 2 1 1 1 |1 2 1 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  8      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 11,   8,  10)) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+1]==c( -1,  11,  15)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c( -1,  11,  15)) ) cnt<-cnt+1 #-10
					if( 1<sum(aFStep[1:2+4]==c(  2,  -9     )) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(-11, -15, -17)) ) cnt<-cnt+1 #  7
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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+1]==c(2,2,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 25    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 25,30 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=2 )	# <15>
					if( fCutU.hasPtn(c(15,33,33,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <17>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <21>
					if( fCutU.hasPtn(c(21,28),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <26>
					if( fCutU.hasPtn(c( 7, 9,18,NA,26),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=3 )	# <27>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=1 )	# <28>
					if( fCutU.hasPtn(c(   25,28      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,NA,28,NA,30),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt) 
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,5   ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5     ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7     ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 5,1,7 ),c( 25    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 5,0   ),c( 25,30 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 4     ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  1, 2, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 12       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  2,10    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  6,10    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  3       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  9, 10)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 9, 10,  1)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c(10,  1,  7)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 2,  2,  6)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+0]==c( 2,  6, 18)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+1]==c( 9,  7,  5)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+1]==c( 7,  5,  2)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1, 12,  2)) ) cnt<-cnt+1	#  7

					if( fCutU.hasPtn(c( 2, 9),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,11),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 6),aCStep) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     23 27 28 38 42 43    | 4  1 10  4  1 |                        |0 0 3 1 2 |3 1 2
				#      2  8 17 24 29 31    | 6  9  7  5  2 |-21 -19 -11 -14 -13 -12 |2 1 2 1 0 |2 1 2 1
				#     13 14 26 28 30 36    | 1 12  2  2  6 | 11   6   9   4   1   5 |0 2 2 2 0 |2 2 2
				#      1  3 12 21 26 41(1) | 2  9  9  5 15 |-12 -11 -14  -7  -4   5 |2 1 2 0 1 |2 1 2 1
				#     15 17 19 21 27 45(1) | 2  2  2  6 18 | 14  14   7   0   1   4 |0 3 2 0 1 |3 2 1
				#      4  6 15 25 26 33(1) | 2  9 10  1  7 |-11 -11  -4   4  -1 -12 |2 1 2 1 0 |2 1 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(       ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( 6,  5 ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(       ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(       ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( 1     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( 3     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 14,   7,   0)) ) cnt<-cnt+1 # -11
					if( 1<sum(aFStep[1:3+0]==c(-11, -11,  -4)) ) cnt<-cnt+1 # -11
					if( 1<sum(aFStep[1:3+1]==c(  0,   1,   4)) ) cnt<-cnt+1 #  -4
					if( 1<sum(aFStep[1:3+1]==c(  4,  -1, -12)) ) cnt<-cnt+1 #   4
					if( 1<sum(aFStep[1:3+2]==c( -4,   1,  12)) ) cnt<-cnt+1 #  -1
					if( 1<sum(aFStep[1:3+3]==c(  4,   1,   5)) ) cnt<-cnt+1 # -12
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

# done *
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 15    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 38,36 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <13>
					if( fCutU.hasPtn(c(13,NA,NA,15,16),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <15>
					if( fCutU.hasPtn(c( 7,15,NA,29,32),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-2 )	# <25>
					if( fCutU.hasPtn(c(13,25,38,44),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <28>
					if( fCutU.hasPtn(c(27,28),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=-1 )	# <33>
					if( fCutU.hasPtn(c(      33,37   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(23,27,33,NA,36),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <38>
					if( fCutU.hasPtn(c(17,NA,NA,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   29,31,38),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=1 )	# <40>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <44>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,NA,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    5,31,26,37,44),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5,4   ),c( 15    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,8,6 ),c( 38,36 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 5,0   ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,1,0 ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 4     ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1, 3 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 5, 6 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1, 12,  2)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+0]==c( 1, 12,  2)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+0]==c( 5,  2,  5)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c(14,  3,  5)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c( 5,  2,  5)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c( 9, 8),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+0]==c( 1,12)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 2, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 3, 6)) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3 13 15 40 41 44    |10  2 25  1  3 |                        |1 2 0 0 3 |1 2 3
				#      4  9 23 33 39 44(1) | 5 14 10  6  5 |  1  -4   8  -7  -2   0 |2 0 1 2 1 |2 1 2 1
				#      2 11 19 25 28 32    | 9  8  6  3  4 | -2   2  -4  -8 -11 -12 |1 2 2 1 0 |1 2 2 1
				#     13 14 26 28 30 36(1) | 1 12  2  2  6 | 11   3   7   3   2   4 |0 2 2 2 0 |2 2 2
				#     10 15 21 35 38 43    | 5  6 14  3  5 | -3   1  -5   7   8   7 |0 2 1 2 1 |2 1 2 1
				#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  6  10  12   3   2   2 |0 1 1 2 2 |1 1 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( -8     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -2,-11 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  2,  1 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3,  -2,  -4)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c( -5,   7,   8)) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:3+3]==c(  7,   8,   7)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c(  7,   8     )) ) cnt<-cnt+1 #  2

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

# done *
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
					if( all(quoSize[1:3+0]==c(2,0,0)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+1]==c(0,0,2)) ) return(FALSE)	# next rebind of 0,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 12,18    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41,43,42 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# < 5>
					if( fCutU.hasPtn(c( 5,NA,10),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=0 )	# <10>
					if( fCutU.hasPtn(c(10,NA,NA,NA,NA,43),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <11>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <12>
					if( fCutU.hasPtn(c(12,NA,NA,41),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=2 )	# <16>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[3,],aZoid,posDiff=1 )	# <18>
					if( fCutU.hasPtn(c(12,NA,NA,27),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    9,18,27),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[2,],aZoid,posDiff=-2 )	# <21>
					if( fCutU.hasPtn(c(21,22,35),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[4,],aZoid,posDiff=-1 )	# <41>
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=-1 )	# <42>
					if( fCutU.hasPtn(c(30,38,NA,42),aZoid) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[6,],aZoid,posDiff=0 )	# <43>
					if( fCutU.hasPtn(c( 6,NA,NA,NA,NA,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            38,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    2, 4,27,NA,43),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[5,],aZoid,posDiff=0 )	# <45>
					if( fCutU.hasPtn(c(43,45),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt) 
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 0     ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 1,7,8 ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 2,8   ),c( 12,18    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 8,6,2 ),c(          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(       ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1,3,7 ),c( 41,43,42 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(18,  9,  4)) ) cnt<-cnt+1	#   7
					if( 1<sum(aCStep[1:3+1]==c( 6,  6, 18)) ) cnt<-cnt+1	#   4
					if( 1<sum(aCStep[1:2+3]==c( 2,  4    )) ) cnt<-cnt+1	#   3

					if( fCutU.hasPtn(c( 4, 3),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c( 2, 4)) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     10 15 18 21 34 41    | 5  3  3 13  7 |                        |0 3 1 1 1 |3 1 1 1
				#      4 16 20 33 40 43    |12  4 13  7  3 | -6   1   2  12   6   2 |1 1 1 1 2 |1 1 1 1 2
				#      5  9 12 30 39 43(1) | 4  3 18  9  4 |  1  -7  -8  -3  -1   0 |2 1 0 2 1 |2 1 2 1
				#      5  7 11 16 41 45(1) | 2  4  5 25  4 |  0  -2  -1 -14   2   2 |2 2 0 0 2 |2 2 2
				#     10 11 12 18 24 42(1) | 1  1  6  6 18 |  5   4   1   2 -17  -3 |0 4 1 0 1 |4 1 1
				#      2 21 28 38 42 45(1) |19  7 10  4  3 | -8  10  16  20  18   3 |1 0 2 1 2 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( 16 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -1, -14,   2)) ) cnt<-cnt+1 #  -8
					if( 1<sum(aFStep[1:3+3]==c(-20, -18,  -3)) ) cnt<-cnt+1 #   3

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



