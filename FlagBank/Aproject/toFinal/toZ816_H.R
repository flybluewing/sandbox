# toZ816_H.R 최종접근
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

# done *
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	# flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(44) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)
	cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0	# fCut.basic() 의 내용과 조율할 것.
					if( aZoid[1]%in%c( 3,14) ) cnt<-cnt+1
					if( aZoid[2]%in%c(21,30) ) cnt<-cnt+1
					if( aZoid[3]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(27,24) ) cnt<-cnt+1
					if( aZoid[6]%in%c(45   ) ) cnt<-cnt+1
					return( cnt )
			})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# <11>
					if( fCutU.hasPtn(c(11,NA,NA,34   ),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(21,22,NA,NA,27),aZoid) ) cnt<-cnt+1
					# <36>
					# <42>
					if( fCutU.hasPtn(c(12,22,41,42),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(11,NA,NA,43),aZoid) ) cnt<-cnt+1
					# <45>
					if( fCutU.hasPtn(c(31,37,NA,NA,45),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(3    ),c( 3,14)) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(1,0  ),c(21,30)) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(5  ,0),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(    5),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(7    ),c(27,24)) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(7,5,6),c(45   )) ) cnt<-cnt+1 # 6

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 9, 5 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 1, 9  )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 1, 1, 9)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+1]==c( 7,10, 4)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 7,10, 4)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:2+3]==c(19, 4   )) ) cnt<-cnt+1	# 9

					if( all(aCStep[1:2+1]==c( 7,10)) ) cnt<-cnt+1

					if( (aCStep[1]==aCStep[2]) && (aCStep[3]==aCStep[4]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      5 10 13 21 39 43    | 5  3  8 18  4 |                        |1 2 1 1 1 |1 2 1 1 1
		#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  3   1   6   0  -3   2 |1 2 1 1 1 |1 2 1 1 1
		#      1  3 12 14 16 43    | 2  9  2  2 27 | -7  -8  -7  -7 -20  -2 |2 3 0 0 1 |2 3 1
		#     11 30 34 35 42 44    |19  4  1  7  2 | 10  27  22  21  26   1 |0 1 0 3 2 |1 3 2
		#      2 21 28 38 42 45(1) |19  7 10  4  3 | -9  -9  -6   3   0   1 |1 0 2 1 2 |1 2 1 2
		#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 15   0  -3 -12 -15  -9 |0 1 4 1 0 |1 4 1
		# -<standard zoid>---------------------------------------------------------------------
		#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(-10      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -5      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -5      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c(-12, -15     )) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+0]==c( -7, -20,  -2)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:2+4]==c( 15,   0     )) ) cnt<-cnt+1 # -9

					if( all(aFStep[1:2+1]==c( -7,-20)) ) cnt<-cnt+1

					if( aFStep[5]==sum(aFStep[c(3,4)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )
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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5 ) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( 7,21,21,12,23,25)) ) cnt<-cnt+1
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
					# if( all(aZoid[ 1:2]==c( 2, 7)) ) cnt<-cnt+1
					# if( all(aZoid[ 1:2]==c( 1, 8)) ) cnt<-cnt+1 #  3, 8  2, 8

					remCnt <- 0
					# if( aRem[1]%in%c(1,3,0 ) ) cnt<-cnt+1
					# if( aRem[2]%in%c(      ) ) cnt<-cnt+1
					# if( aRem[3]%in%c(0     ) ) cnt<-cnt+1
					# if( aRem[4]%in%c(2,7   ) ) cnt<-cnt+1
					# if( aRem[5]%in%c(      ) ) cnt<-cnt+1
					# if( aRem[6]%in%c(      ) ) cnt<-cnt+1
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# <remove>	cStep 
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					score <- 0					
					# score <- score + sum(aCStep==c( 7, 4, 1, 2, 5)) # last cStep 2개이상 자름
					if( 1<sum(aCStep==c(12, 9, 9, 9, 4)) ){	# h-2 cStep 에서 3개 이상 자름
						# score <- score + (sum(aCStep==c(12, 9, 9, 9, 4))-1)
					}

					cnt <- 0
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
					# if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1	# 마지막 값 연속도 포함.
					# if( aCStep[2]%in%c(13   ) ) cnt<-cnt+1
					# if( aCStep[3]%in%c(10, 8) ) cnt<-cnt+1
					# if( aCStep[4]%in%c( 7   ) ) cnt<-cnt+1
					# if( aCStep[5]%in%c( 1   ) ) cnt<-cnt+1

					# if( 2<sum(aCStep==c(11, 3, 8, 1, 6)) ) cnt<-cnt+1
					# if( 2<sum(aCStep==c( 4, 3, 1,12, 3)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 8,10, 2, 6)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c(10, 1,15, 2)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c(14, 8, 3, 7)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 6, 6, 6, 2)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 5,15,11, 5)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 1, 3, 3,11)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 3, 7, 4, 7)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 1, 2,15, 2)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 9, 2, 3,14)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c(11, 5, 2, 2)) ) cnt<-cnt+1
					# if( 2<sum(aCStep[1:4]==c( 2, 4, 5, 3)) ) cnt<-cnt+1

					# if( all(aCStep[1:2]==c( 1, 4)) ) cnt<-cnt+1
					# if( all(aCStep[c(1,3)]==c( 5,17)) ) cnt<-cnt+1
					# if( all(aCStep[c(1,3)]==c( 5,11)) ) cnt<-cnt+1
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
					# if( all(aCStep[1:2]==c(11, 3)) ) cnt<-cnt+1
					# if( all(aCStep[1:2]==c( 9, 2)) ) cnt<-cnt+1
					# if( all(aCStep[2:3]==c( 5, 7)) ) cnt<-cnt+1

					# if( all(aCStep[3:4]==c(11, 4)) ) cnt<-cnt+1
					# if( all(aCStep[3:4]==c(11, 7)) ) cnt<-cnt+1
					# if( all(aCStep[3:4]==c( 6, 9)) ) cnt<-cnt+1
					# if( all(aCStep[3:4]==c( 3,11)) ) cnt<-cnt+1

					# if( all(aCStep[4:5]==c( 8, 2)) ) cnt<-cnt+1

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

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,0,3 (reverse)
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,3,0 (reverse)
					if( all(quoSize[1:3+2]==c(1,2,0)) ) return(FALSE)	# next rebind of 3,2,0 (reverse)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 16 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 17 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 3>
					if( fCutU.hasPtn(c( 3, 11),aZoid) ) cnt<-cnt+1
					# <11>
					if( fCutU.hasPtn(c( 3, 11),aZoid) ) cnt<-cnt+1
					# <15>
					# <17>
					if( fCutU.hasPtn(c(17,30,38,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(17,NA,38   ),aZoid) ) cnt<-cnt+1
					# <21>
					# <22>
					# <37>
					if( fCutU.hasPtn(c(26,27,32,37),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(37,39),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(37,39),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6   ),c( 16 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 7,1 ),c( 17 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,1 ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,0 ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 7   ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3   ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 2, 3 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 7,10 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 8,26, 2)) ) cnt<-cnt+1	#11
					if( 1<sum(aCStep[1:3+0]==c( 2, 9,10)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+1]==c( 2, 9,10)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:2+3]==c( 5, 7   )) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:2+3]==c( 1,11   )) ) cnt<-cnt+1	# 7

					if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3 11 13 14 17 21    | 8  2  1  3  4 |                        |1 4 1 0 0 |1 4 1
				#      4 15 17 26 36 37(1) |11  2  9 10  1 |  1   4   4  12  19  16 |1 2 1 2 0 |1 2 1 2
				#      3 11 37 39 41 43(1) | 8 26  2  2  2 | -1  -4  20  13   5   6 |1 1 0 2 2 |1 1 2 2
				#     10 15 22 24 27 42    | 5  7  2  3 15 |  7   4 -15 -15 -14  -1 |0 2 3 0 1 |2 3 1
				#     16 17 28 37 39 40    | 1 11  9  2  1 |  6   2   6  13  12  -2 |0 2 1 2 1 |2 1 2 1
				#      9 20 21 22 30 37(1) |11  1  1  8  7 | -7   3  -7 -15  -9  -3 |1 0 3 2 0 |1 3 2
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39    | 6  1 10  2  8 |  3  -2  -2   7   1   2 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( 13     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -4     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -6, -2, -6)) ) cnt<-cnt+1 # -7
					if( 1<sum(aFStep[1:3+1]==c( -6, -2, -6)) ) cnt<-cnt+1 # -7
					if( 1<sum(aFStep[1:3+2]==c(  2,  6, 13)) ) cnt<-cnt+1 #-15
					if( 1<sum(aFStep[1:3+3]==c(  6, 13, 12)) ) cnt<-cnt+1 #-15

					if( (aFStep[1]==aFStep[3]) && (aFStep[2]==-aFStep[6]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextZW()

# done *
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,0,3)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+0]==c(3,0,0)) ) return(FALSE)	# next rebind of 1,1,1 reverse
					if( all(quoSize[1:3+1]==c(4,2,0)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,1,1 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 5      ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(41      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(45,43,24) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 2>
					if( fCutU.hasPtn(c( 2, 6),aZoid) ) cnt<-cnt+1
					# < 8>
					if( fCutU.hasPtn(c( 8,NA,NA,NA,40),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8,NA,41,43   ),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,NA,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,NA,25,26   ),aZoid) ) cnt<-cnt+1
					# <26>
					# <31>
					if( fCutU.hasPtn(c( 4,13,31),aZoid) ) cnt<-cnt+1
					# <37>
					if( fCutU.hasPtn(c(37,40),aZoid) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(28,33,NA,NA,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(         42,43),aZoid) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(         42,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,23,NA,NA,43),aZoid) ) cnt<-cnt+1
					# <45>
					if( fCutU.hasPtn(c(41,45),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(5,1,3),c( 5      )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(8,5  ),c(        )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(0    ),c(        )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(1,3  ),c(        )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(1    ),c(41      )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(5,3,4),c(45,43,24)) ) cnt<-cnt+1 # 6

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(           ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(10, 2, 3, 5) ) cnt<-cnt+1
					if( aCStep[4]%in%c(11         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4,13      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c(10, 5,11)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+1]==c( 6,25, 2)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c( 6,25, 2)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c(25, 2, 2)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c(11, 3, 8)) ) cnt<-cnt+1	#  3

					if( aCStep[5]==sum(aCStep[c(2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      8 12 29 31 42 43    | 4 17  2 11  1 |                        |1 1 1 1 2 |1 1 1 1 2
				#     21 22 26 27 31 37(1) | 1  4  1  4  6 | 13  10  -3  -4 -11  -6 |0 0 4 2 0 |4 2
				#     15 26 37 42 43 45(2) |11 11  5  1  2 | -6   4  11  15  12   8 |0 1 1 1 3 |1 1 1 3
				#      2  8 33 35 37 41(1) | 6 25  2  2  4 |-13 -18  -4  -7  -6  -4 |2 0 0 3 1 |2 3 1
				#      2  7 13 25 42 45(1) | 5  6 12 17  3 |  0  -1 -20 -10   5   4 |2 1 1 0 2 |2 1 1 2
				#      5 15 20 31 34 42(1) |10  5 11  3  8 |  3   8   7   6  -8  -3 |1 1 1 2 1 |1 1 1 2 1
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39(1) | 6  1 10  2  8 |  7   3  -1  -2  -3  -3 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  7      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-11,-15,-12)) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:2+0]==c( -6, -4    )) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+1]==c( 20, 10, -5)) ) cnt<-cnt+1 #  7
					if( 1<sum(aFStep[1:3+2]==c( 10, -5, -4)) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+3]==c(  7,  6,  4)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+0]==c( -6,  4, 11)) ) cnt<-cnt+1 # -3

					if( all(aFStep[1:2]==-aFStep[6:5]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextQuo10()

# done *
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(34) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)
    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(15   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(26,16) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(     ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 3>
					if( fCutU.hasPtn(c( 3, 8,10,29,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 6>
					if( fCutU.hasPtn(c( 6,39,31,NA,43),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,NA,NA,NA,34   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,19,NA,NA,NA,38),aZoid) ) cnt<-cnt+1
					# <11>
					# <16>
					if( fCutU.hasPtn(c(16,19),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(14,18   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   18,21),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(   18,21),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,NA,21),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c( 1,26,NA,42,34),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c(10,NA,NA,NA,34   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   19,10,11,34,38),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(3,4  ),c( 3   )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(4,5,9),c(15   )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(6,5  ),c(26,16)) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(4,1  ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(10, 1,11)) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:3+1]==c( 1,20, 1)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+2]==c( 1,20, 1)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:2+3]==c( 1,20   )) ) cnt<-cnt+1	# 7

					if( all(aCStep[1:2+0]==c(10, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 1,15)) ) cnt<-cnt+1

					if( aCStep[5]==sum(aCStep[c(2,3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  4 16 17 19 20    | 1 12  1  2  1 |                        |2 3 1 0 0 |2 3 1
			#      8 11 14 16 18 21(1) | 3  3  2  2  3 |  5   7  -2  -1  -1   1 |1 4 1 0 0 |1 4 1
			#     10 11 26 31 34 44(1) | 1 15  5  3 10 |  2   0  12  15  16  23 |0 2 1 2 1 |2 1 2 1
			#      3  6 13 23 24 35    | 3  7 10  1 11 | -7  -5 -13  -8 -10  -9 |2 1 2 1 0 |2 1 2 1
			#      5  6 26 27 38 39(1) | 1 20  1 11  1 |  2   0  13   4  14   4 |2 0 2 2 0 |2 2 2
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  5   9  -8  -6  -4   2 |0 3 1 1 1 |3 1 1 1
			# -<standard zoid>---------------------------------------------------------------------
			#     12 18 19 29 31 39(1) | 6  1 10  2  8 |  2   3   1   8  -3  -2 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  2      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  0      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( 14      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  2,  0, 12)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:2+0]==c(-14, -4    )) ) cnt<-cnt+1 #  9
					if( 1<sum(aFStep[1:3+1]==c( 13,  4, 14)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+3]==c(  8,  6,  4)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c( -2,  0,-12)) ) cnt<-cnt+1 #  2

					if( aFStep[4]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextBin()

# done *
fCutCnt.nextRebNum <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,3,0
					if( all(quoSize[1:3+1]==c(1,0,2)) ) return(FALSE)	# next rebind of 3,0,2
					if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of 0,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 36 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 39 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 2>
					if( fCutU.hasPtn(c( 2,13,13,28,32),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					# <11>
					# <15>
					# <19>
					if( fCutU.hasPtn(c( 9 ,19,42,41),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <32>
					if( fCutU.hasPtn(c(         32,37),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,10, 9,32   ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <35>
					if( fCutU.hasPtn(c( 2, 1, 6,35),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <37>
					if( fCutU.hasPtn(c(   37,38   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,37,NA,39),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(   37,38   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,16,37,38,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(0,6  ),c( 36 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(6    ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(0,9  ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(5    ),c( 39 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(7    ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 8       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 5, 3 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(10,21, 1)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+0]==c( 1, 8,16)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c(10,21, 1)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 8,16, 4)) ) cnt<-cnt+1	#  4

					if( aCStep[4]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      1  2  7  9 10 38    | 1  5  2  1 28 |                        |4 1 0 1 0 |4 1 1
		#     15 18 21 32 35 44    | 3  3 11  3  9 | 14  16  14  23  25   6 |0 2 1 2 1 |2 1 2 1
		#     13 19 28 37 38 43    | 6  9  9  1  5 | -2   1   7   5   3  -1 |0 2 1 2 1 |2 1 2 1
		#      3 11 14 15 32 36    | 8  3  1 17  4 |-10  -8 -14 -22  -6  -7 |1 3 0 2 0 |1 3 2
		#      6 16 37 38 41 45    |10 21  1  3  4 |  3   5  23  23   9   9 |1 1 0 2 2 |1 1 2 2
		#      2 10 11 19 35 39    | 8  1  8 16  4 | -4  -6 -26 -19  -6  -6 |1 3 0 2 0 |1 3 2
		# -<standard zoid>---------------------------------------------------------------------
		#     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 23,  9,  9)) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:3+3]==c( 23,  9,  9)) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:2+4]==c( 23,  9    )) ) cnt<-cnt+1 # -6

					if( all(aFStep[2]==aFStep[5:6]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextRebNum()

# done *
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	# rawVal, cStep 모두 특이하다. 주의할 것.
	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )
	
	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c(32) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,2 (reverse)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   39 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(16    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(19    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   41 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(34    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(43    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 5>
					if( fCutU.hasPtn(c( 5,NA,35,25,42,43),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,12,NA,NA,29,38),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,NA,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,24,41,30,41),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <17>
					# <18>
					if( fCutU.hasPtn(c(18,NA,32,43),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(12,21,28,23),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c(      19,NA,34),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,18,NA,NA,34),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(14,10,33,34,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(15,NA,NA,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,23,38,27,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )	;u0.zoidMtx_ana( stdMI$rawTail )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(  9   ),c(   39 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(6,4,1 ),c(16    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(9,0   ),c(19    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(  1   ),c(   41 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(4     ),c(34    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(3     ),c(43    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 3,13, 7)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+0]==c( 8,12, 6)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+0]==c( 6, 3, 5)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:2+3]==c( 6,10,  )) ) cnt<-cnt+1	# 2

					if( aCStep[2]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(1,3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
		#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
		#      5 12 17 29 34 35    | 7  5 12  5  1 |                        |1 2 1 2 0 |1 2 1 2
		#      5  6 26 27 38 39(1) | 1 20  1 11  1 |  0  -6   9  -2   4   4 |2 0 2 2 0 |2 2 2
		#     10 18 30 36 39 44(1) | 8 12  6  3  5 |  5  12   4   9   1   5 |0 2 0 3 1 |2 3 1
		#     10 15 18 21 34 41(2) | 5  3  3 13  7 |  0  -3 -12 -15  -5  -3 |0 3 1 1 1 |3 1 1 1
		#      2 17 19 24 37 41(1) |15  2  5 13  4 | -8   2   1   3   3   0 |1 2 1 1 1 |1 2 1 1 1
		#     15 21 31 32 41 43(1) | 6 10  1  9  2 | 13   4  12   8   4   2 |0 1 1 2 2 |1 1 2 2
		# -<standard zoid>---------------------------------------------------------------------
		#     12 18 19 29 31 39(1) | 6  1 10  2  8 | -3  -3 -12  -3 -10  -4 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  5      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3,-12,-15)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+1]==c(  0, -3,-12)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+3]==c(-13, -4,-12)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+3]==c( -3,-12,-15)) ) cnt<-cnt+1 #  4

					if( aFStep[4]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(2,4)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(5,4)]) ) cnt<-cnt+1
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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextCStepBin()

# done *
fCutCnt.nextFStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 33,32 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(4,2,0)) ) return(FALSE)	# next rebind of  2,1,1 
					if( all(quoSize[1:3+2]==c(0,2,4)) ) return(FALSE)	# next rebind of  1,1,2
					if( all(quoSize[1:3+1]==c(2,2,1)) ) return(FALSE)	# next rebind of  1,2,1 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  3    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 21    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 36,42 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 2>
					if( fCutU.hasPtn(c( 2,NA,19   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,NA,NA,26),aZoid) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 7,26,22,20,32),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					# <12>
					# <19>
					if( fCutU.hasPtn(c( 2,NA,19),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(19,26   ),aZoid) ) cnt<-cnt+1
					# <28>
					# <29>
					if( fCutU.hasPtn(c(10,28,NA,29,40),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c( 3,NA,NA,NA,NA,36),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   12,23,31,34,36),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(14,15,17,36,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(2,0,3   ),c(  3    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(7,4  ,1 ),c( 21    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(        ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(3,7,5   ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(      1 ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(6,2,4   ),c( 36,42 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  5, 4, 6, 9 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 11          ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  7, 9       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  3,10       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(             ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 5,  6, 11)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 6, 11,  8)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c( 5, 16, 10)) ) cnt<-cnt+1	#  3

					if( fCutU.hasPtn(c( 5,12 ),aCStep) ) cnt<-cnt+1

					if( aCStep[3]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(1,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4 10 19 29 32 42    | 6  9 10  3 10 |                        |1 2 1 1 1 |1 2 1 1 1
				#      6  7 12 28 38 40    | 1  5 16 10  2 |  2  -3  -7  -1   6  -2 |2 1 1 1 1 |2 1 1 1 1
				#      1  2 15 19 24 36    | 1 13  4  5 12 | -5  -5   3  -9 -14  -4 |2 2 1 1 0 |2 2 1 1
				#     10 14 16 18 27 28    | 4  2  2  9  1 |  9  12   1  -1   3  -8 |0 4 2 0 0 |4 2
				#      2  7 19 25 29 36    | 5 12  6  4  7 | -8  -7   3   7   2   8 |2 1 2 1 0 |2 1 2 1
				#     12 17 23 34 42 45    | 5  6 11  8  3 | 10  10   4   9  13   9 |0 2 1 1 2 |2 1 1 2
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39(1) | 6  1 10  2  8 |  0   1  -4  -5 -11  -6 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  3,  5  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( 10      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -1,   3,  -8)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+3]==c( -8,  -7,   3)) ) cnt<-cnt+1 #  9
					if( 1<sum(aFStep[1:3+3]==c(-12,  -1,   1)) ) cnt<-cnt+1 #  9

					if( fCutU.hasPtn(c( 5,12 ),aFStep) ) cnt<-cnt+1

					if( (aFStep[1]==aFStep[2])&&(aFStep[4]==aFStep[6]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(3,4)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1

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
	
	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

}	# fCutCnt.nextFStepBin( )

# done *
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  3       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 17       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 18,27,25 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 2>
					# <10>
					# <18>
					if( fCutU.hasPtn(c( 9,18,29),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,24   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,NA,42),aZoid) ) cnt<-cnt+1
					# <23>
					# <24>
					if( fCutU.hasPtn(c(   24,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,24   ),aZoid) ) cnt<-cnt+1
					# <28>
					# <40>
					if( fCutU.hasPtn(c(11,24,17,29,18,40),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(   24,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,NA,42),aZoid) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c(28,38,NA,30,44),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6,3   ),c(  3       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 2     ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,0   ),c( 17       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 8,7,5 ),c( 18,27,25 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0     ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1     ),c(          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  2 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  8 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  4 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 6,18  )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+0]==c( 6, 6,18)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 1,21, 2)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c(21, 2,13)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c( 1,21, 2)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c(21, 2,13)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+2]==c(12, 6, 6)) ) cnt<-cnt+1	# 18

					if( fCutU.hasPtn(c( 6, 6),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,18),aCStep) ) cnt<-cnt+1

					if( (aCStep[1]==aCStep[2]) && (aCStep[3]==aCStep[4]) ) cnt<-cnt+1
					if( all( (aCStep[3]*c(1,3)) == aCStep[4:5] ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     16 18 24 42 44 45    | 2  6 18  2  1 |                        |0 2 1 0 3 |2 1 3
				#     10 22 28 34 36 44(1) |12  6  6  2  8 | -6   4   4  -8  -8  -1 |0 1 2 2 1 |1 2 2 1
				#      1  2 23 25 38 40    | 1 21  2 13  2 | -9 -20  -5  -9   2  -4 |2 0 2 1 1 |2 2 1 1
				#      6 13 20 27 28 40(1) | 7  7  7  1 12 |  5  11  -3   2 -10   0 |1 1 3 0 1 |1 1 3 1
				#      2  5 15 18 19 23    | 3 10  3  1  4 | -4  -8  -5  -9  -9 -17 |2 3 1 0 0 |2 3 1
				#     10 11 12 18 24 42(1) | 1  1  6  6 18 |  8   6  -3   0   5  19 |0 4 1 0 1 |4 1 1
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39(2) | 6  1 10  2  8 |  2   7   7  11   7  -3 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  -5     ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  -9     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -8,  -6,   3)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+1]==c( -8,  -5,  -9)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+1]==c( -9,  -9, -17)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:2+4]==c(  4,   8     )) ) cnt<-cnt+1 #  5

					if( aFStep[5]==sum(aFStep[c(1,3)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_1()

# done *
fCutCnt.nextColVal_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 33 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(4,1,0)) ) return(FALSE)	# next rebind of  1,3,1 
					if( all(quoSize[1:3+1]==c(1,4,1)) ) return(FALSE)	# next rebind of  3,1,0
					if( all(quoSize[1:3+2]==c(0,1,4)) ) return(FALSE)	# next rebind of  1,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  6, 7 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 10    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 6>
					if( fCutU.hasPtn(c( 6,NA,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,10            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,NA,11,10,12   ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c( 9,10 ),aZoid) ) cnt<-cnt+1
					# <17>
					# <36>
					# <40>
					if( fCutU.hasPtn(c(   12,NA,NA,NA,40),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,NA,17,NA, 9,40),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(6,7,8     ),c(  6, 7 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(1,2,0,3   ),c( 10    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(5,1       ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(5         ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(2    ,7,4 ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(6    ,7,1 ),c( 39    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  3, 4    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  4, 9, 1 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  5   , 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 4,  1,  1)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c( 4,  4,  1)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+1]==c( 4,  2,  6)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 4,  2,  6)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:2+0]==c( 5,  4    )) ) cnt<-cnt+1	#  9

					if( fCutU.hasPtn(c( 3, 4),aCStep) ) cnt<-cnt+1

					if( (aCStep[1]==aCStep[2]) && (aCStep[3]==aCStep[4]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7 10 17 29 33 44    | 3  7 12  4 11 |                        |1 2 1 1 1 |1 2 1 1 1
				#      8 10 13 36 37 40(1) | 2  3 23  1  3 |  1   0  -4   7   4  -4 |1 2 0 2 1 |1 2 2 1
				#      6 12 19 24 34 41    | 6  7  5 10  7 | -2   2   6 -12  -3   1 |1 2 1 1 1 |1 2 1 1 1
				#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
				#      6 11 15 17 23 40    | 5  4  2  6 17 |  1 -11 -16 -15 -16  -5 |1 3 1 0 1 |1 3 1 1
				#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 11  10  10   9   4  -4 |0 1 4 1 0 |1 4 1
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  2      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -3      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-10, -10,  -9)) ) cnt<-cnt+1 # 11
					if( 1<sum(aFStep[1:3+0]==c(  1, -11, -16)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+1]==c(  1, -11, -16)) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+2]==c(-15, -16,  -5)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+3]==c(-12,  -3,   1)) ) cnt<-cnt+1 # -4

					if( fCutU.hasPtn(c( 4,-4),aFStep) ) cnt<-cnt+1

					if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,3)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_2()

# done *
fCutCnt.nextColVal_3 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

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
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 2,1,0 
					if( all(quoSize[1:3+2]==c(2,3,0)) ) return(FALSE)	# next rebind of 2,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 13    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 27,15 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 6>
					if( fCutU.hasPtn(c( 6,NA,17,NA,NA,22),aZoid) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 7,21,22,34,27),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,NA,33),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,32,34   ),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(18,19),aZoid) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(18,19),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(21,12,NA,27,35),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 8         ),c(  8    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 8,6       ),c(  8    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 3         ),c( 13    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(           ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(           ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2,1,7,5,6 ),c( 27,15 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 8 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(10    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4, 9 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 3,  5    )) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 2,  1,  9)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1,  1,  4)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 8,  1,  4)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 1,  4, 11)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,10),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8, 3),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+1]==c( 2, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 8, 2)) ) cnt<-cnt+1

					if( aCStep[3]==sum(aCStep[c(2,4,5)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  6 18 21 33 34    | 4 12  3 12  1 |                        |2 1 1 2 0 |2 1 1 2
			#     10 17 18 19 23 27(1) | 7  1  1  4  4 |  8  11   0  -2 -10  -7 |0 4 2 0 0 |4 2
			#      6  8 13 30 35 40    | 2  5 17  5  5 | -4  -9  -5  11  12  13 |2 1 0 2 1 |2 1 2 1
			#      6  7 15 16 20 31(1) | 1  8  1  4 11 |  0  -1   2 -14 -15  -9 |2 2 1 1 0 |2 2 1 1
			#      7 18 19 27 29 42(1) |11  1  8  2 13 |  1  11   4  11   9  11 |1 2 2 0 1 |1 2 2 1
			#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  5  -4  -4  -3  -2 -10 |0 3 2 1 0 |3 2 1
			# -<standard zoid>---------------------------------------------------------------------
			#     12 18 19 29 31 39(1) | 6  1 10  2  8 |  0   4   4   5   4   7 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  11     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -2,  14,  15)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+1]==c(  0,  -1,   2)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+2]==c(  0,  -1,   2)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c( -5,  11,  12)) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+3]==c( -5,  11,  12)) ) cnt<-cnt+1 #-10

					if( fCutU.hasPtn(c( -2, -10),aFStep) ) cnt<-cnt+1

					if( aFStep[6]==sum(aFStep[c(2,3,5)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,2,3)]) ) cnt<-cnt+1

					if( aFStep[4]==sum(aFStep[c(1,2,4)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,3,4)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_3()

# done *
fCutCnt.nextColVal_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 39,35 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,2,0)) ) return(FALSE)	# next rebind of  2,1,2
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of  1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  1, 3,11 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 13,26    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 30,39    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 36,44,31 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 1>
					if( fCutU.hasPtn(c( 1,NA,NA,16,24,31),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 3>
					if( fCutU.hasPtn(c( 3,11            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,11,NA,21,20),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,12),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,30   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,NA,13,NA,NA,28),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,20),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(10,11,18,40),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(19, 5,26,28,32),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <32>
					if( fCutU.hasPtn(c(23,16,23,NA,32),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(25,31,NA,36),aZoid) ) cnt<-cnt+1
					# <40>
					if( fCutU.hasPtn(c(40,41),aZoid) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(40,41),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,3     ),c(  1, 3,11 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 1    ,6 ),c( 13,26    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 3,6,7   ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 8       ),c(          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0       ),c( 30,39    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 6,4  ,1 ),c( 36,44,31 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 9,20       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(       4    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 7          ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(12   ,13, 5 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 9,  1,  5)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:0+0]==c( 6,  4    )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 9,  3, 13)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 1,  5, 13)) ) cnt<-cnt+1	# 13
					if( 1<sum(aCStep[1:3+2]==c(13,  6,  4)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 1, 3 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 4 ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(20, 5)) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3 13 15 40 41 44    |10  2 25  1  3 |                        |1 2 0 0 3 |1 2 3
			#      4 10 14 15 18 22(1) | 6  4  1  3  4 |  1  -3  -1 -25 -23 -22 |1 4 1 0 0 |1 4 1
			#      1  2 15 19 24 36(1) | 1 13  4  5 12 | -3  -8   1   4   6  14 |2 2 1 1 0 |2 2 1 1
			#      1 21 26 36 40 41(2) |20  5 10  4  1 |  0  19  11  17  16   5 |1 0 2 1 2 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 |  0 -11 -13 -10  -8  -5 |1 2 1 2 0 |1 2 1 2
			#      3 12 13 18 31 32(2) | 9  1  5 13  1 |  2   2   0  -8  -1  -4 |1 3 0 2 0 |1 3 2
			# -<standard zoid>---------------------------------------------------------------------
			#     12 18 19 29 31 39(3) | 6  1 10  2  8 |  9   6   6  11   0   7 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  2      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -8,  5  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -3      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+2]==c(  2,   2,   0)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+2]==c( -8,  -1,  -4)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+3]==c( -8,   1,   4)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(-19, -11, -17)) ) cnt<-cnt+1 # -4

					if( aFStep[6]==sum(aFStep[c(1,2,4)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_4()

# done *
fCutCnt.nextColVal_5 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,0,2)) ) return(FALSE)	# next rebind of  1,3,0 reverse
					if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of  1,1,0 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 6       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(11,28, 9 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(19,17    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(      20 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(39,44,43 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# <11>
					if( fCutU.hasPtn(c(11,NA,18),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(11,NA,18),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   17,18),aZoid) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(19,NA,39),aZoid) ) cnt<-cnt+1
					# <24>
					# <33>
					# <35>
					if( fCutU.hasPtn(c( 5,35,38),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(19,NA,39),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(3       ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(5,6     ),c(  6       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(8,1,9   ),c( 11,28, 9 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(9,7,0,1 ),c( 19,17    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(6,0     ),c(       20 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(9,4,3   ),c( 39,44,43 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6, 1,16 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3, 5, 8 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(17       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 6       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1, 10,  4)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+0]==c( 1,  8, 16)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 4,  1, 10)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 1,  5, 15)) ) cnt<-cnt+1	# 16
					if( 1<sum(aCStep[1:2+3]==c( 5,  2    )) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 8, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 1,10 )) ) cnt<-cnt+1

					if( aCStep[4]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
					if( all((aCStep[5]*c(2,2))==aCStep[c(1,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  8 11 15 18 45    | 7  3  4  3 27 |                        |2 3 0 0 1 |2 3 1
			#     11 24 32 33 35 40(1) |13  8  1  2  5 | 10  16  21  18  17  -5 |0 1 1 3 1 |1 1 3 1
			#     23 27 28 38 42 43    | 4  1 10  4  1 | 12   3  -4   5   7   3 |0 0 3 1 2 |3 1 2
			#      6  7 11 17 33 44    | 1  4  6 16 11 |-17 -20 -17 -21  -9   1 |2 2 0 1 1 |2 2 1 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  5   9   7   2  -9  -5 |0 4 1 1 0 |4 1 1
			#      2 10 11 19 35 39(3) | 8  1  8 16  4 | -9  -6  -7   0  11   0 |1 3 0 2 0 |1 3 2
			# -<standard zoid>---------------------------------------------------------------------
			#     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  7      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  1      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( 11      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  1      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c( 11,   0     )) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+1]==c( -6,  -7,   0)) ) cnt<-cnt+1 # -7

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_5()

# done *
fCutCnt.nextColVal_6 <- function( gEnv ,allIdxF ,rpt=FALSE ){
	# 경고 : 동일 컬럼 동일 값 반복 상태인 경우 신중할 것.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,0)) ) return(FALSE)	# next rebind of  0,2,1 
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
					if( aZoid[3]%in%c( 12    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 42,28 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 42    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0

					
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,14     ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,16,NA,NA,19),aZoid) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,16),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,NA,NA,28,22),aZoid) ) cnt<-cnt+1
					# <16>
					if( fCutU.hasPtn(c(16,NA,26),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(10,10,18,35,36),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <28>
					# <42>
					if( fCutU.hasPtn(c(23,31,38,NA,42),aZoid,thld=3,fixIdx5) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(19,27,34,NA,34,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(4,3,1 ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(6,2,5 ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(3,1,2 ),c( 12    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(6     ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(2,8,0 ),c( 42,28 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(2,5,3 ),c( 42    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 9   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 7) ) cnt<-cnt+1
					if( aCStep[3]%in%c(11   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c(30,  1   )) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:2+0]==c( 3,  5   )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 9,  1,  5)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c(14,  3,  5)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 9, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5, 6),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(30, 1)) ) cnt<-cnt+1

					if( sum(aCStep[c(2,4)])==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
					if( sum(aCStep[c(5,4)])==sum(aCStep[c(1,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3  4 16 20 28 44    | 1 12  4  8 16 |                        |2 1 2 0 1 |2 1 2 1
				#     10 14 16 18 27 28(2) | 4  2  2  9  1 |  7  10   0  -2  -1 -16 |0 4 2 0 0 |4 2
				#      1  3  8 12 42 43    | 2  5  4 30  1 | -9 -11  -8  -6  15  15 |3 1 0 0 2 |3 1 2
				#     10 15 21 35 38 43(1) | 5  6 14  3  5 |  9  12  13  23  -4   0 |0 2 1 2 1 |2 1 2 1
				#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
				#      3 12 13 18 31 32(1) | 9  1  5 13  1 | -9  -5 -10 -16 -11 -13 |1 3 0 2 0 |1 3 2
				# -<standard zoid>---------------------------------------------------------------------
				#     12 18 19 29 31 39(3) | 6  1 10  2  8 |  9   6   6  11   0   7 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  9,  12,  13)) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+1]==c(  9,  11,   8)) ) cnt<-cnt+1 #-10
					if( 1<sum(aFStep[1:3+1]==c( -6,  15,  15)) ) cnt<-cnt+1 #-16
					if( 1<sum(aFStep[1:3+3]==c(  9,  12,  13)) ) cnt<-cnt+1 #-11
					if( 1<sum(aFStep[1:3+3]==c( -2,  -2,  -2)) ) cnt<-cnt+1 #-13

					if( aFStep[4]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx ,cccMtx=cccObj$scoreMtx ) )

} # fCutCnt.nextColVal_6()



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



