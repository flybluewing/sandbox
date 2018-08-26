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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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

# undone *
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
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					cnt <- cnt + neighborObj$matchCnt( aZoid )
					
					cnt <- cnt + fCutU.spanMatch(stdMI$rawTail[ ,],aZoid,posDiff= )	# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
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



