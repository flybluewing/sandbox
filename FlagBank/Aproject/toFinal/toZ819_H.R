# toZ819_H.R 최종접근
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
    # rem 동일값 연속  rx,rx,r3,r3,rx,rx - 잘라내기엔 좀 위험하다..
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    aCode <- aZoid%%10
					return( !any(aCode[2:6]==aCode[1:5]) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    allIdxF <- allIdxF[flag]
    # rem 동일위치 재현 3 이상
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                    cnt <- sum( stdMI$rem==(aZoid%%10) )
					return( cnt<3 )
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

# done
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

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=cntThld
	cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0	# fCut.basic() 의 내용과 조율할 것.
					if( aZoid[1]%in%c(13 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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
					# rptObj<-anaMtx( stdMI$rawTail ) ;u0.zoidMtx_ana.rpt( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(5,3),c(13 )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[2],c(7  ),c(   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[3],c(   ),c(   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[4],c(   ),c(   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[5],c(0,7),c(   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[6],c(1  ),c(   )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(1      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(2,3    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(1      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(    3,6) ) cnt<-cnt+1
					if( aCStep[5]%in%c(    1,4) ) cnt<-cnt+1

					if( 1<sum(aCStep[4:5]==c(10,2)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10, 4),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,10),aZoid) ) cnt<-cnt+1
					if( 1<sum(aCStep[1:3+0]==c( 1,10, 3)) ) cnt<-cnt+1	# 3
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( 4       ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[3:5]==c( -3, -4, -3)) ) cnt<-cnt+1
					if( 1<sum(aFStep[1:2]==c( -5, -3    )) ) cnt<-cnt+1
					if( 1<sum(aFStep[4:6]==c(-16, -6,  4)) ) cnt<-cnt+1
					if( 1<sum(aFStep[1:3+3]==c( -5, -3, -6)) ) cnt<-cnt+1 # 15
					
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
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
					if( aZoid[1]%in%c( 5 ) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 7,21,21,12,23,25)) ) cnt<-cnt+1
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
					if( aZoid[5]%in%c(33) ) cnt<-cnt+1
					if( all(aZoid[ 1:2]==c( 2, 7)) ) cnt<-cnt+1
					if( all(aZoid[ 1:2]==c( 1, 8)) ) cnt<-cnt+1 #  3, 8  2, 8

					remCnt <- 0
					if( aRem[1]%in%c(1,3,0 ) ) cnt<-cnt+1
					if( aRem[2]%in%c(      ) ) cnt<-cnt+1
					if( aRem[3]%in%c(0     ) ) cnt<-cnt+1
					if( aRem[4]%in%c(2,7   ) ) cnt<-cnt+1
					if( aRem[5]%in%c(      ) ) cnt<-cnt+1
					if( aRem[6]%in%c(      ) ) cnt<-cnt+1
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# <remove>	cStep 
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					score <- 0					
					score <- score + sum(aCStep==c( 7, 4, 1, 2, 5)) # last cStep 2개이상 자름
					if( 1<sum(aCStep==c(12, 9, 9, 9, 4)) ){	# h-2 cStep 에서 3개 이상 자름
						score <- score + (sum(aCStep==c(12, 9, 9, 9, 4))-1)
					}

					cnt <- 0
					if( aCStep[1]%in%c(12) ) cnt<-cnt+1
					if( aCStep[5]%in%c(12) ) cnt<-cnt+1
					# if( aCStep[ ]%in%c( ) ) cnt<-cnt+1
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
					if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1	# 마지막 값 연속도 포함.
					if( aCStep[2]%in%c(13   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(10, 8) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 7   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1   ) ) cnt<-cnt+1

					if( 2<sum(aCStep==c(11, 3, 8, 1, 6)) ) cnt<-cnt+1
					if( 2<sum(aCStep==c( 4, 3, 1,12, 3)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 8,10, 2, 6)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c(10, 1,15, 2)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c(14, 8, 3, 7)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 6, 6, 6, 2)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 5,15,11, 5)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 1, 3, 3,11)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 3, 7, 4, 7)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 1, 2,15, 2)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 9, 2, 3,14)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c(11, 5, 2, 2)) ) cnt<-cnt+1
					if( 2<sum(aCStep[1:4]==c( 2, 4, 5, 3)) ) cnt<-cnt+1

					if( all(aCStep[1:2]==c( 1, 4)) ) cnt<-cnt+1
					if( all(aCStep[c(1,3)]==c( 5,17)) ) cnt<-cnt+1
					if( all(aCStep[c(1,3)]==c( 5,11)) ) cnt<-cnt+1
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
					if( all(aCStep[1:2]==c(11, 3)) ) cnt<-cnt+1
					if( all(aCStep[1:2]==c( 9, 2)) ) cnt<-cnt+1
					if( all(aCStep[2:3]==c( 5, 7)) ) cnt<-cnt+1

					if( all(aCStep[3:4]==c(11, 4)) ) cnt<-cnt+1
					if( all(aCStep[3:4]==c(11, 7)) ) cnt<-cnt+1
					if( all(aCStep[3:4]==c( 6, 9)) ) cnt<-cnt+1
					if( all(aCStep[3:4]==c( 3,11)) ) cnt<-cnt+1

					if( all(aCStep[4:5]==c( 8, 2)) ) cnt<-cnt+1

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
					if( all(aCStep[3:5]==c( 8, 5, 1)) ) cnt<-cnt+1	# next of 2
					if( all(aCStep[3:5]==c( 2, 8,15)) ) cnt<-cnt+1	# inc of 2
					if( all(aCStep[3:5]==c( 5, 6, 1)) ) cnt<-cnt+1	# inc of 1

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
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					cnt <- 0
					if( all(quoSize[2:4]==c(0,1,3)) ) cnt<-cnt+1	# next rebind of revers 2,0,1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=cntThld

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 4 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 7 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(38 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(   ),c(    )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[2],c(   ),c(    )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[3],c(8,4),c( 4  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[4],c(3,7),c( 7  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[5],c(8  ),c(38  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[6],c(   ),c(    )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(5,3) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c( 4, 5,24)) ) cnt<-cnt+1	# 5
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

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
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
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(16   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 7   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(30   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   41) ) cnt<-cnt+1
					if( aZoid[5]%in%c(43   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(45   ) ) cnt<-cnt+1
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
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(3      ),c(16   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[2],c(7,0    ),c( 7,30)) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[3],c(9,5    ),c(     )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[4],c(1      ),c(41   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[5],c(3      ),c(43   )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[6],c(3,1,5,4),c(45   )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1   ,8) ) cnt<-cnt+1
					if( aCStep[2]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4     ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2, 3  ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,20, 2)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+1]==c(11, 4, 3)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:2+3]==c( 1, 5   )) ) cnt<-cnt+1	# 7
					if( 1<sum(aCStep[1:3+2]==c( 1, 7, 2)) ) cnt<-cnt+1	# 2
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( 5,-2    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1
					if( 1<sum(aFStep[3:5]==c( -9,-12, -7)) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  5, 23, 15)) ) cnt<-cnt+1 # 5
					if( 1<sum(aFStep[1:3+3]==c(  2, -4, -8)) ) cnt<-cnt+1 # 1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
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
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   19) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   11) ) cnt<-cnt+1
					if( aZoid[4]%in%c(35   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(2,0  ),c(    )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[2],c(9,8  ),c(19  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[3],c(     ),c(11  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[4],c(7,5  ),c(35  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[5],c(0,1  ),c( 1  )) ) cnt<-cnt+1
					if( fCutU.remFilt(aZoid[6],c(3,2,6),c(16  )) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6, 7, 8 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(12       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3, 2    ) ) cnt<-cnt+1

					if( fCutU.hasPtn(c( 8,10),aCStep) ) cnt<-cnt+1
					if( aCStep[2]==aCStep[3] ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 9, 2   )) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+0]==c(16, 7, 3)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+0]==c( 7, 3, 9)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+0]==c(11, 1, 1)) ) cnt<-cnt+1	# 9
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

					if( 1<sum(aZoid[1:2]==c( 5, 3)) ) cnt<-cnt+1	#-5,-4
					if( fCutU.hasPtn(c(-5,-5),aFStep) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c( -6, -7    )) ) cnt<-cnt+1 # -5
					if( 1<sum(aFStep[1:3+0]==c( 13,  5,  4)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+0]==c( 18,  6,  7)) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:3+3]==c(-13,-13,-16)) ) cnt<-cnt+1 #  9
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
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
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					cnt <- 0
					if( all(quoSize[1:3]==c(1,1,3)) ) cnt<-cnt+1	# 0 2 1 next rebind
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1, 2) ) cnt<-cnt+1
					if( aZoid[2]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(16   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(     ) ) cnt<-cnt+1
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
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(1,2 ),c( 1, 2)) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(2   ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(6,1 ),c(16   )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(1   ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(    ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(4,1 ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      6    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 5,10      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(           ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(           ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1,   4,10 ) ) cnt<-cnt+1

					if( aCStep[2]==aCStep[5] ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(20, 5,10)) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:3+1]==c( 2, 5, 4)) ) cnt<-cnt+1	#10
					if( 1<sum(aCStep[1:3+1]==c( 5,10, 4)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+2]==c(10, 4, 1)) ) cnt<-cnt+1	# 1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(18,25    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( fCutU.hasPtn(c(-4,10),aFStep) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+3]==c(  0, 18, 18)) ) cnt<-cnt+1 # -2
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
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
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )
	
	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					cnt <- 0
					if( all(quoSize[1:3]==c(1,1,2)) ) cnt<-cnt+1	# next rebind of reverse 1 1 0
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 9,10) ) cnt<-cnt+1
					if( aZoid[3]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(31,40) ) cnt<-cnt+1
					if( aZoid[6]%in%c(35   ) ) cnt<-cnt+1
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
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(5,2     ),c( 5   )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(9,0,7,2 ),c( 9,10)) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(2  ,9   ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(5,1     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(1,0     ),c(31,40)) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(5,3     ),c(35   )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3, 5,4  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(        7) ) cnt<-cnt+1
					if( aCStep[5]%in%c(14       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3]==c( 5, 5, 5)) ) cnt<-cnt+1
					if( 1<sum(aCStep[2:4]==c( 5, 5, 5)) ) cnt<-cnt+1
					if( 1<sum(aCStep[3:5]==c( 3,18, 4)) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c(13, 3, 4)) ) cnt<-cnt+1	# 3
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

					if( 1<sum(aFStep[4:6]==c(  -6, -5,  2)) ) cnt<-cnt+1
					if( 1<sum(aFStep[1:3]==c(   4,  6,  5)) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-12,-16,-18)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+3]==c( -6, -5,  2)) ) cnt<-cnt+1 #  7
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})

    flgCnt <- flgCnt + score

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx) )

} # fCutCnt.nextCStepBin()


# undone
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
					# if( all(quoSize[1:3+ ]==c( , , )) ) return(FALSE)	# next rebind of  , , 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

# undone
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
					# if( all(quoSize[1:3+ ]==c( , , )) ) return(FALSE)	# next rebind of  , , 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

# undone
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
					# if( all(quoSize[1:3+ ]==c( , , )) ) return(FALSE)	# next rebind of  , , 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
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



# 이번 시점에서는 보류 allIdxLst$stdFiltedCnt : ...  3 1 3 0 
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
					if( aZoid[1]%in%c( 1   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(33   ) ) cnt<-cnt+1
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

# done
fCutCnt.colValStd <- function( gEnv ,allIdxF ,cutCol.idx ,cutCol.val ,rpt=FALSE ){
	# cutCol.idx=1 ;cutCol.val=1
	applyFlag <- gEnv$allZoidMtx[allIdxF,cutCol.idx]==cutCol.val
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[gEnv$zhF[,cutCol.idx]==cutCol.val,,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )	#	rptObj<-anaMtx( stdMI$rawTail )	;u0.zoidMtx_ana( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxT ,zMtx ,pRebTwo=F )

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} #fCutCnt.colValStd()
# done
fCutCnt.zWidthStd <- function( gEnv ,allIdxF ,zWidth ,rpt=FALSE ){
	applyFlag <- zWidth == (gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1])
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zMtx <- gEnv$zhF[zWidth==(gEnv$zhF[,6]-gEnv$zhF[,1]), ,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )	#	rptObj<-anaMtx( stdMI$rawTail )	;u0.zoidMtx_ana( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxT ,zMtx ,pZWidth=F )

	flgCnt.all <- rep( 0 ,length(allIdxF) )
	flgCnt.all[applyFlag] <- flgCnt
	return( flgCnt.all )

} # fCutCnt.zWidthStd()
# done
fCutCnt.quoTblStd <- function( gEnv ,allIdxF ,tblStr ,rpt=FALSE ){

	aQuoTblObjLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,,drop=F] )	# tbl valStr quoStr idStr
	aQuoTblStr <- sapply( aQuoTblObjLst ,function(quoTbl){quoTbl$valStr})

	applyFlag <- aQuoTblStr==tblStr
	allIdxT <- allIdxF[applyFlag]
	flgCnt <- rep( 0 ,length(allIdxT) )

	zQuoTblStr <- sapply( fCutU.getQuoTblLst(gEnv$zhF) ,function(quo){quo$valStr} )
	zMtx <- gEnv$zhF[zQuoTblStr==tblStr, ,drop=F]	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )	#	rptObj<-anaMtx( stdMI$rawTail )	;u0.zoidMtx_ana( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxT ,zMtx ,pQuoTbl=F )

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
					if( aZoid[3]%in%c(11) ) cnt<-cnt+1
					if( aZoid[4]%in%c(16,12) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt<-cnt+1
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[3]%in%c(6  ) ) cnt<-cnt+1	# 6
					return( 2>cnt )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxT,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[3]%in%c( 9,10,13) ) cnt<-cnt+1
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



