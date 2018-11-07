# toZ821_H.R 최종접근
cntMtx.colName <- c( "raw","rawFV","rem","cStep","fStep"
						,"raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2"
					)

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

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	#	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,2)) ) return(FALSE)	# next rebind of 0,1,2 reverse
					if( all(quoSize[1:3+1]==c(2,2,1)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 1,2,2 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 25 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <12>
			# <21>
			if( fCutU.hasPtn(c( 3,21,NA,34,43),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c(17, 25),aZoid) ) cnt<-cnt+1
			# <29>
			# <30>
			if( fCutU.hasPtn(c(14,15,30),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 1       ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,1,4,0 ),c( 25 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,5     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3       ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,9     ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(18       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 5       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 1, 8    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 5,10, 4 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 8, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3, 1),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(10, 4),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 1,10),aCStep) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			# 17 21 25 26 27 36    | 4  4  1  1  9 |                        |0 1 4 1 0 |1 4 1
			# 12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
			#  3  9 12 13 25 43(1) | 6  3  1 12 18 | -9  -9  -7 -16  -6   4 |2 2 1 0 1 |2 2 1 1
			# 14 15 25 28 29 30(1) | 1 10  3  1  1 | 11   6  13  15   4 -13 |0 2 3 1 0 |2 3 1
			# 16 25 33 38 40 45(1) | 9  8  5  2  5 |  2  10   8  10  11  15 |0 1 1 2 2 |1 1 2 2
			# 10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( fCutU.hasPtn(c( -5, -3),aFStep) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,6)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					if( 1<sum(aZoid[c(1,2,3,4,5,6)]==c( 1,32,26,13,23,34)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,  4,5,6)]==c( 3,32,   23,40,21)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,  4,5,6)]==c(11,24,   41,20,31)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(  2,  4,5,6)]==c(   31,   27,41,45)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(        5,6)]==c(            34,45)) ) cnt<-cnt+1

					return( 1>cnt )
				})	;kIdx<-anaFltCnt(!flag,rpt)
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
						if( fCutU.remFilt(aZoid[1],c(     ),c(   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c(     ),c(   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c(     ),c(   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 1   ),c(   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 9,8 ),c(   )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c(     ),c(   )) )	remCnt <- remCnt+1					
						# grp 1
						# grp 2
						if( aZoid[3]==20 && fCutU.remFilt(aZoid[2],c( 7),c(17)) ) remCnt <- remCnt+1 
						# grp 5
						if( aZoid[6]==37 && fCutU.remFilt(aZoid[4],c( 2),c(22)) ) remCnt <- remCnt+1 
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

					score <- sum(aCStep==c(NA, 3, 1, 7, 4),na.rm=T)

					matCnt <- sum(aCStep==c(NA, 4, 2, 4,14),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA,10,NA, 4, 6),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 3,NA, 2, 1),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 4,NA,NA,13),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )


					cnt <- 0
						if( aCStep[1]%in%c( 4   ) ) cnt<-cnt+1
						if( aCStep[2]%in%c( 2   ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(     ) ) cnt<-cnt+1
						if( aCStep[4]%in%c( 7   ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(     ) ) cnt<-cnt+1

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

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[2]%in%c(  8       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 15       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 26,21,18 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 34,25    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 37       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c( 5,NA,NA,NA,NA,44),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,NA, 8,21,26   ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c( 6,12),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 6,12),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(15,21         ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(15,NA,NA,34   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(15,NA,NA,NA,43),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c(      21,34   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 9,15,21,NA,33),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(   15,26),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 8,NA,26),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(15,NA,NA,34   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(         34,39),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 8, 9,31,34   ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <37>
			if( fCutU.hasPtn(c( 8,19,28,30,NA,37),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2       ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,2     ),c(  8       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 5,8     ),c( 15       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,1,0   ),c( 26,21,18 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,5,3,0 ),c( 34,25    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7       ),c( 37       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 11, 4 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 13    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 4,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 2,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c(13, 8)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==c(13, 3)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aCStep[5]==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  4  5  6 12 25 37    | 1  1  6 13 12 |                        |3 1 1 1 0 |3 1 1 1
			# 15 19 21 34 41 44    | 4  2 13  7  3 | 11  14  15  22  16   7 |0 2 1 1 2 |2 1 1 2
			#  5 15 20 31 34 42(2) |10  5 11  3  8 |-10  -4  -1  -3  -7  -2 |1 1 1 2 1 |1 1 1 2 1
			#  6 12 17 21 34 37(1) | 6  5  4 13  3 |  1  -3  -3 -10   0  -5 |1 2 1 2 0 |1 2 1 2
			#  3 10 13 26 34 38(1) | 7  3 13  8  4 | -3  -2  -4   5   0   1 |1 2 1 2 0 |1 2 1 2
			#  5  9 14 26 30 43(1) | 4  5 12  4 13 |  2  -1   1   0  -4   5 |2 1 1 1 1 |2 1 1 1 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  0, -5 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  7,19 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 22,11 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c( 7,27,34,17,27),aZoid,thld=3,fixIdx1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(10,11,12),aZoid) ) cnt<-cnt+1
			# <13>
			# <19>
			if( fCutU.hasPtn(c(19,29,24,31),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <20>
			if( fCutU.hasPtn(c( 2, 9,20,NA,36,38),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 3,NA,NA,28,44),aZoid) ) cnt<-cnt+1
			# <41>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,4,2,0 ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,9     ),c(  7,19 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,9,1   ),c( 22,11 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,9     ),c( 29    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7       ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 2    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 7, 4 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 6    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 3    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 7, 7),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[c(1,3)]==c( 7, 3)) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    2 (3)   3 (3)   4 (4)   5 (2)   7 (8)   9 (2)   12 (2) 

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 4,5   )]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aCStep[c( 1,2 )])==sum(aCStep[c( 3,4,5 )]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 13 20 27 28 40    | 7  7  7  1 12 |                        |1 1 3 0 1 |1 1 3 1
			#      1  8 10 13 28 42(2) | 7  2  3 15 14 | -5  -5 -10 -14   0   2 |2 2 1 0 1 |2 2 1 1
			#      4 11 20 23 32 39    | 7  9  3  9  7 |  3   3  10  10   4  -3 |1 1 2 2 0 |1 1 2 2
			#     15 19 21 34 41 44    | 4  2 13  7  3 | 11   8   1  11   9   5 |0 2 1 1 2 |2 1 1 2
			#      5  7 11 16 41 45(1) | 2  4  5 25  4 |-10 -12 -10 -18   0   1 |2 2 0 0 2 |2 2 2
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 | -3   0   8   9 -12  -9 |2 1 2 1 0 |2 1 2 1
			# -<standard zoid>---------------------------------------------------------------------
			#      1 12 13 24 29 44(1) |11  1 11  5 15 | -1   5  -6  -1   0   8 |1 2 2 0 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( fCutU.hasPtn(c( -12, -8 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(   0,  0 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -10 (3)   -5 (2)   -3 (2)   0 (3)   1 (2)   3 (2)   8 (2)   9 (2)   10 (2)   11 (2)
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,4)]*c( 4,-1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					if( (aZoid[6]-aZoid[1]) %in% c( 32 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(  3,10 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  4    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  5    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 13    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 37,45 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c( 4,NA,NA,NA,17,29),aZoid) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c( 5, 6      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,NA,15,39),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c( 5, 6      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,NA,15,39),aZoid) ) cnt<-cnt+1
			# <15>
			# <16>
			if( fCutU.hasPtn(c(16,NA,NA,NA,42),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(16,23,44      ),aZoid) ) cnt<-cnt+1
			# <33>
			# <37>
			if( fCutU.hasPtn(c( 3,NA,NA,NA,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    4,NA,NA,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(         13,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       3,NA,35,37),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(16,NA,NA,NA,42),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 9, 2,26,24,45),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,0     ),c(  3,10 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,7,4,0 ),c(  4    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,5     ),c(  5    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,2,6,4 ),c( 13    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,3,4   ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,5     ),c( 37,45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 1, 9, 3 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 1       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 1       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 1       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			cnt.w2 <- 0
			if( aCStep[5]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			# 16 17 34 36 42 45    | 1 17  2  6  3 |                        |0 2 0 2 2 |2 2 2
			#  4 15 28 33 37 40    |11 13  5  4  3 |-12  -2  -6  -3  -5  -5 |1 1 1 2 1 |1 1 1 2 1
			# 10 16 20 39 41 42    | 6  4 19  2  1 |  6   1  -8   6   4   2 |0 2 1 1 2 |2 1 1 2
			#  4 13 18 31 33 45    | 9  5 13  2 12 | -6  -3  -2  -8  -8   3 |1 2 0 2 1 |1 2 2 1
			#  5  6  9 11 15 37    | 1  3  2  4 22 |  1  -7  -9 -20 -18  -8 |3 2 0 1 0 |3 2 1
			#  4  5  6 12 25 37(3) | 1  1  6 13 12 | -1  -1  -3   1  10   0 |3 1 1 1 0 |3 1 1 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -10 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0
			if( aFStep[6]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					if( (aZoid[6]-aZoid[1]) %in% c( 27 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,0)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(  3, 2       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 10,18,14,13 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 14,26       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 15          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 18          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(             ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# <10>
			if( fCutU.hasPtn(c(10,14),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(10,14),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(   14,15   ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(11,NA,15   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(   14,15   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(      15,19),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c(         17,31   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(         17,NA,31),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 1, 6,NA,17      ),aZoid) ) cnt<-cnt+1
			# <18>
			# <22>
			if( fCutU.hasPtn(c( 5,10,22),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(         17,31   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(      17,NA,31),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 1, 6,NA,NA,31),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 4,11,13,32),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(19,NA,33),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,2       ),c(  3, 2       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,8,4,3,2 ),c( 10,18,14,13 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,6,7,2   ),c( 14,26       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5    ,2   ),c( 15          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8    ,3   ),c( 18          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,3       ),c(             )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 7, 6, 1, 4 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 4, 8       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 1, 2       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 3          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 4, 1       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 1, 2),aCStep) )	cnt.w1<-cnt.w1+1
			cnt.w2 <- 0
			if( all(aCStep[c(1,2)]==aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  9 14 15 17 31 33    | 5  1  2 14  2 |                        |1 3 0 2 0 |1 3 2
			#  1 12 13 21 32 45    |11  1  8 11 13 | -8  -2  -2   4   1  12 |1 2 1 1 1 |1 2 1 1 1
			#  5 10 16 17 31 32(1) | 5  6  1 14  1 |  4  -2   3  -4  -1 -13 |1 3 0 2 0 |1 3 2
			#  4 18 26 33 34 38    |14  8  7  1  4 | -1   8  10  16   3   6 |1 1 1 3 0 |1 1 1 3
			#  4 10 14 15 18 22(2) | 6  4  1  3  4 |  0  -8 -12 -18 -16 -16 |1 4 1 0 0 |1 4 1
			#  2 10 14 22 32 36(3) | 8  4  8 10  4 | -2   0   0   7  14  14 |1 2 1 2 0 |1 2 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  0  ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,1,1)) ) return(FALSE)	# next rebind of 1,2,0 reverse
					if( all(quoSize[1:3+2]==c(1,1,0)) ) return(FALSE)	# next rebind of 0,2,1
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,1,0
					if( all(quoSize[1:3+1]==c(3,0,0)) ) return(FALSE)	# next rebind of 1,0,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(  1, 2, 3 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 35       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  9       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 12, 4    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c( 4,11,12,20, 9),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c( 5, 6 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,NA,15,39),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c( 5, 6 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,NA,15,39),aZoid) ) cnt<-cnt+1
			# < 8>
			# < 9>
			if( fCutU.hasPtn(c( 3,NA, 9 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       9,21,19),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(12,NA,NA,42),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(12,29,30   ),aZoid) ) cnt<-cnt+1
			# <17>					# <33>
			# <37>
			if( fCutU.hasPtn(c( 3,NA,NA,NA,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    4,NA,NA,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(         13,NA,37),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       3,NA,35,37),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(   12,NA,NA,42),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(17,NA,29,30,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 5,16,NA,38,NA,45),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,2,3 ),c(  1, 2, 3 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 5     ),c( 35       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,1,0 ),c(  9       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,2,4 ),c( 12, 4    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,2   ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,7,0 ),c( 45       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 7, 9, 1 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 1       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 1       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  5  6  9 11 15 37    | 1  3  2  4 22 |                        |3 2 0 1 0 |3 2 1
			#  1  8 17 34 39 45    | 7  9 17  5  6 | -4   2   8  23  24   8 |2 1 0 2 1 |2 1 2 1
			#  4  5  6 12 25 37    | 1  1  6 13 12 |  3  -3 -11 -22 -14  -8 |3 1 1 1 0 |3 1 1 1
			#  4  8  9 16 17 19(1) | 4  1  7  1  2 |  0   3   3   4  -8 -18 |3 3 0 0 0 |3 3
			#  3 12 33 36 42 45    | 9 21  3  6  3 | -1   4  24  20  25  26 |1 1 0 2 2 |1 1 2 2
			#  2 10 12 31 33 42(3) | 8  2 19  2  9 | -1  -2 -21  -5  -9  -3 |1 2 0 2 1 |1 2 2 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -1,  0 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -3     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c(1,4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,1,2 reverse
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,2 reverse
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[4]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 44,43 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c( 7,19,36,40),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(15,20),aZoid) ) cnt<-cnt+1
			# <23>
			if( fCutU.hasPtn(c( 7,19,22,23),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c( 5,18,NA,26),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(22,27,40,39,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 5,NA,NA,NA,42),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    8,33,NA,42),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,2,1,3 ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,8     ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,8,0   ),c( 40    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,0     ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,3,8,1 ),c( 44,43 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 7, 4,18,10 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(10          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(10          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 9, 1),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5,17),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(17, 7),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3,16),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==c(10, 1)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+1]==c( 1, 9)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c( 9, 2)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,3)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( all(aCStep[c(1,2)]== (aCStep[5]*c(3,5)) ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  7  9 10 12 26 38    | 2  1  2 14 12 |                        |2 2 1 1 0 |2 2 1 1
			#  3  7 14 23 26 42(2) | 4  7  9  3 16 | -4  -2   4  11   0   4 |2 1 2 0 1 |2 1 2 1
			#  2  8 15 22 25 41    | 6  7  7  3 16 | -1   1   1  -1  -1  -1 |2 1 2 0 1 |2 1 2 1
			#  5 13 18 23 40 45    | 8  5  5 17  5 |  3   5   3   1  15   4 |1 2 1 0 2 |1 2 1 2
			#  1  6 11 28 34 42    | 5  5 17  6  8 | -4  -7  -7   5  -6  -3 |2 1 1 1 1 |2 1 1 1 1
			# 15 21 31 32 41 43    | 6 10  1  9  2 | 14  15  20   4   7   1 |0 1 1 2 2 |1 1 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  3  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  9  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( aFStep[2]==aFStep[3] )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(2,4,6)]) )	cnt.w1<-cnt.w1+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(3,6)]) )	cnt.w1<-cnt.w1+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

}	# fCutCnt.nextFStepBin( )

# done *
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,2,0)) ) return(FALSE)	# next rebind of 2,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c(  6, 3, 4, 5 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  7, 4, 9    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 24          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 30,42       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(             ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c( 3,NA,NA,22),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 3,14,15   ),aZoid) ) cnt<-cnt+1
			# < 4>
			if( fCutU.hasPtn(c( 4,NA,NA,23),aZoid) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c( 6, 7 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 6,NA,25,21,27,32),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c( 6, 7 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    7,11   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 7,25,14,18,31),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c( 7, 9,16,29),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(    7,11   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 2,NA,11,31),aZoid) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c( 5,10,13,24),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 4,11,10,NA,30),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 8, 5,NA, 9,33),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,3,4,5,0 ),c(  6, 3, 4, 5 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,4,9     ),c(  7, 4, 9    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(           ),c(             )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4         ),c( 24          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,2       ),c( 30,42       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(           ),c(             )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 3 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3,13 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 13    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 18, 1 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 4, 13),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c(11, 1)) )		cnt.w1<-cnt.w1+1
			if( all(aCStep[c(1,3,5)]==c( 1, 6,11)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[c(1,3,5)]==c( 4,12,13)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[c(1,3,5)]==c( 1, 1, 8)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==aCStep[1:2+2]) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  3  4  9 24 25 33    | 1  5 15  1  8 |                        |3 0 2 1 0 |3 2 1
			#  1  2  3  9 12 23(2) | 1  1  6  3 11 | -2  -2  -6 -15 -13 -10 |4 1 1 0 0 |4 1 1
			#  6  7 11 17 33 44    | 1  4  6 16 11 |  5   5   8   8  21  21 |2 2 0 1 1 |2 2 1 1
			#  4  7 11 24 42 45(2) | 3  4 13 18  3 | -2   0   0   7   9   1 |2 1 1 0 2 |2 1 1 2
			#  6  7 18 19 30 38(1) | 1 11  1 11  8 |  2   0   7  -5 -12  -7 |2 2 0 2 0 |2 2 2
			#  5  9 14 26 30 43(1) | 4  5 12  4 13 | -1   2  -4   7   0   5 |2 1 1 1 1 |2 1 1 1 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  6, -3 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  8,  2 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  7,  0 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  5     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( fCutU.hasPtn(c( 0, 7),aFStep) )	cnt.w1<-cnt.w1+1
			if( all(aFStep[1:2+1]==c( -4,  7)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			#   5   5   8   8  21  21
			if( all(aFStep[c(1,3,5)+0]==aFStep[c(1,3,5)+1]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,3,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_1()

# done *
fCutCnt.nextColVal_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,3,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[2]%in%c( 20,12 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 19    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 35    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 40    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c( 6,10            ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 6,NA,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 6,NA,11,10,12,39),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(12,17         ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(12,NA,14,24,21),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <17>
			# <19>
			if( fCutU.hasPtn(c(19,34,28,37),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <31>			# <36>
			# <39>
			if( fCutU.hasPtn(c(      30,39),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(16,27,NA,39),aZoid) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c(   12,NA,NA,NA,40),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 4,NA,17,NA,NA,40),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,2,8,7 ),c( 20,12 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9       ),c( 19    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5       ),c( 35    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0,7     ),c( 40    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 1, 6    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 5, 1    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 1,23    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 7,12, 3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 1,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 7, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( aCStep[3]%in%c( 2 ) )	cnt.w1<-cnt.w1+1	# 10, 7   10, 2

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( all(aCStep[c(1,3,5)]== aCStep[4]*c(3,5,4) ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  8 10 13 36 37 40    | 2  3 23  1  3 |                        |1 2 0 2 1 |1 2 2 1
			#  6 12 19 24 34 41    | 6  7  5 10  7 | -2   2   6 -12  -3   1 |1 2 1 1 1 |1 2 1 1 1
			#  5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
			#  6 11 15 17 23 40    | 5  4  2  6 17 |  1 -11 -16 -15 -16  -5 |1 3 1 0 1 |1 3 1 1
			# 17 21 25 26 27 36(1) | 4  4  1  1  9 | 11  10  10   9   4  -4 |0 1 4 1 0 |1 4 1
			# 12 18 19 29 31 39    | 6  1 10  2  8 | -5  -3  -6   3   4   3 |0 3 1 2 0 |3 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(            ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( 10         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  2         ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 10         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  4,-16,  3 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  8         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0
			if( all(aFStep[c(4,6)]== aFStep[2]*c(-1,-1) ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_2()

# done *
fCutCnt.nextColVal_3 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,3,0)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+2]==c(1,1,3)) ) return(FALSE)	# next rebind of 1,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[3]%in%c( 20, 7 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 31    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c( 7,NA,NA,41),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c(15,NA,NA,27   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(15,NA,19,NA,38),aZoid) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(18,20      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(18,NA,28,39),aZoid) ) cnt<-cnt+1
			# <22>
			if( fCutU.hasPtn(c( 5,12),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 5,12,22,NA,NA,30),aZoid) ) cnt<-cnt+1
			# <31>
			# <41>
			if( fCutU.hasPtn(c( 7,NA,NA,41),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(       ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,4   ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 0,8,7 ),c( 20, 7 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,3   ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6,1   ),c( 31    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2     ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 9          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 1, 2, 6,10 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 7          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 4          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 1, 3),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 6,20),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(10, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 2, 7),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==c(5,20)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+1]==c(3, 5)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c(6, 8)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  7 18 22 24 31 34    |11  4  2  7  3 |                        |1 1 2 2 0 |1 1 2 2
			#  6 15 22 23 25 32(1) | 9  7  1  2  7 | -1  -3   0  -1  -6  -2 |1 1 3 1 0 |1 1 3 1
			# 14 15 18 21 26 35(1) | 1  3  3  5  9 |  8   0  -4  -2   1   3 |0 3 2 1 0 |3 2 1
			#  3  4  7 11 31 41    | 1  3  4 20 10 |-11 -11 -11 -10   5   6 |3 1 0 1 1 |3 1 1 1
			#  7 17 19 30 36 38(1) |10  2 11  6  2 |  4  13  12  19   5  -3 |1 2 0 3 0 |1 2 3
			#  2  7 27 33 41 44(1) | 5 20  6  8  3 | -5 -10   8   3   5   6 |2 0 1 1 2 |2 1 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( 1    ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(      ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 5, 1 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( 3    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( fCutU.hasPtn(c( 5, 6),aFStep) )	cnt.w1<-cnt.w1+1
			if( all(aFStep[1:2+4]==c(5,-3)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_3()

# done *
fCutCnt.nextColVal_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					if( (aZoid[6]-aZoid[1]) %in% c( 40 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,0,1)) ) return(FALSE)	# next rebind of 2,2,0 reverse
					if( all(quoSize[1:3+1]==c(0,0,2)) ) return(FALSE)	# next rebind of 2,0,0 reverse
					if( all(quoSize[1:3+2]==c(2,0,0)) ) return(FALSE)	# next rebind of 0,0,2
					if( all(quoSize[1:3+0]==c(0,0,1)) ) return(FALSE)	# next rebind of 1,0,0 reverse
					if( all(quoSize[1:3+1]==c(0,2,2)) ) return(FALSE)	# next rebind of 0,0,2 
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[1]%in%c( 18,36    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 40       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,42,44 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c(11,15),aZoid) ) cnt<-cnt+1
			# <16>
			# <17>
			if( fCutU.hasPtn(c( 6,17,NA,NA,27),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(33,37      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(33,NA,44,40),aZoid) ) cnt<-cnt+1
			# <36>					# <39>
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,6,8,7 ),c( 18,36    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,0,2   ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3       ),c(          )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5       ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,6,7   ),c( 40       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,2,4,0 ),c( 45,42,44 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 4,10    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 3, 1, 5 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 6, 2    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(25       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 4, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 1, 4),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 2, 1),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 1, 8),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+1]==c( 3, 4)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c( 4, 2)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c( 2, 4)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==c(24, 3)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+2]==c( 4, 2)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c( 2, 1)) )	cnt.w1<-cnt.w1+1
			cnt.w2 <- 0
			if( all(aCStep[c(2,5)]== aCStep[1]*c(2,2) ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  2 16 17 32 39 45    |14  1 15  7  6 |                        |1 2 0 2 1 |1 2 2 1
			# 24 25 33 34 38 39(1) | 1  8  1  4  1 | 22   9  16   2  -1  -6 |0 0 2 4 0 |2 4
			# 20 30 33 35 36 44(1) |10  3  2  1  8 | -4   5   0   1  -2   5 |0 0 1 4 1 |1 4 1
			# 11 17 21 26 36 45(1) | 6  4  5 10  9 | -9 -13 -12  -9   0   1 |0 2 2 1 1 |2 2 1 1
			#  9 33 36 40 42 43(1) |24  3  4  2  1 | -2  16  15  14   6  -2 |1 0 0 2 3 |1 2 3
			#  5  7 11 16 41 45    | 2  4  5 25  4 | -4 -26 -25 -24  -1   2 |2 2 0 0 2 |2 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  0  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			cnt.w2 <- 0
			if( aFStep[2]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_4()

# done *
fCutCnt.nextColVal_5 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[2]%in%c( 16,17       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 12, 6       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,41,37,16 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c( 2,NA,NA,NA,NA,45),aZoid) ) cnt<-cnt+1
			# < 6>
			# <12>
			if( fCutU.hasPtn(c( 5,NA,19),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(12,19),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(   16,NA,NA,NA,45),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(10,16,NA,44,43   ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(16,17),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c(16,17),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c( 5,NA,19),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(12,19),aZoid) ) cnt<-cnt+1
			# <37>					# <38>
			# <39>
			if( fCutU.hasPtn(c(16, 8,21, 8,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(37,41),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 2,NA,NA,NA,NA,45),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(   16,NA,NA,NA,45),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5         ),c(             )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,2,7     ),c( 16,17       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,9,2,3,6 ),c( 12, 6       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 8         ),c(             )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,2       ),c(             )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1,7,0,6 ),c( 45,41,37,16 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1,19 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1,14 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 1, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 7),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+1]==c( 8,12)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+2]==c( 7, 6)) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+3]==c( 3, 5)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			# 16 17 22 31 34 37    | 1  5  9  3  3 |                        |0 2 1 3 0 |2 1 3
			#  4 12 24 33 38 45    | 8 12  9  5  7 |-12  -5   2   2   4   8 |1 1 1 2 1 |1 1 1 2 1
			#  2 16 17 32 39 45(1) |14  1 15  7  6 | -2   4  -7  -1   1   0 |1 2 0 2 1 |1 2 2 1
			#  9 12 19 20 39 41(1) | 3  7  1 19  2 |  7  -4   2 -12   0  -4 |1 2 1 1 1 |1 2 1 1 1
			#  6 16 37 38 41 45(1) |10 21  1  3  4 | -3   4  18  18   2   4 |1 1 0 2 2 |1 1 2 2
			#  2  6  7 12 19 45(2) | 4  1  5  7 26 | -4 -10 -30 -26 -22   0 |3 2 0 0 1 |3 2 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -5, -4 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			if( all(aFStep[1:2+4]==c(-12, 0)) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_5()

# done *
fCutCnt.nextColVal_6 <- function( gEnv ,allIdxF ,rpt=FALSE ){

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+1]==c(2,1,0)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

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
			if( aZoid[3]%in%c( 19    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 27,20 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28,31 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c( 7,NA,NA,28   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 7,11,21,NA,34),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c( 2, 9,10,31,32),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c(      19,NA,28   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(         20,28   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(15,12,NA,NA,28,35),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c(27,NA,29,39),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(      29,NA,38   ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c(11, 1,23,30),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(      29,NA,38   ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(15,NA,29,36,38,40),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3,2,9,1,0 ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,3       ),c( 12    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,7       ),c( 19    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7,0       ),c( 27,20 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,0,9,6   ),c( 28,31 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(           ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 3, 4             ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(                  ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 5                ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 4, 7, 3, 8,12, 1 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(                  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]
			if( fCutU.hasPtn(c( 4, 5),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+0]==c( 3, 7)) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5,12),aCStep) )	cnt.w1<-cnt.w1+1
			if( all(aCStep[1:2+1]==c( 4, 5)) )	cnt.w1<-cnt.w1+1

			if( all(aCStep[1:2+0]==c(  5,13 )) )	cnt.w1<-cnt.w1+1	# 
			if( fCutU.hasPtn(c( 5, 7),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   4 (5)   5 (5)   6 (3)   7 (2)   12 (3)   17 (2) 
			cnt.w2 <- 0
			if( aCStep[5]==sum(aCStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 2,3 )])==sum(aCStep[c( 1,5 )]) )	cnt.w2<-cnt.w2+1	# 17
			if( sum(aCStep[c( 2,3 )])==sum(aCStep[c( 4,5 )]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  6 17 22 28 45    | 5 11  5  6 17 |                        |2 1 2 0 1 |2 1 2 1
			#      7 27 29 30 38 44    |20  2  1  8  6 |  6  21  12   8  10  -1 |1 0 2 2 1 |1 2 2 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  4 -15   0   3   0  -2 |0 2 1 2 1 |2 1 2 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 | -3  -3 -11 -12 -10  -2 |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			# -<standard zoid>---------------------------------------------------------------------
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( 3,  2         ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(               ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( 2,  1, -2     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 5             ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 1,-10,  2, -3 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(               ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]
			# -------------------------------------------------------------------------------------
			#     FV :    -3 (2)   -2 (3)   0 (2)   1 (4)   3 (2)   4 (2) 
			cnt.w2 <- 0
			if( aFStep[6]==sum(aFStep[c(1,2,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( all(aFStep[c(2,4)]== aFStep[5]*c(2,1) ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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



