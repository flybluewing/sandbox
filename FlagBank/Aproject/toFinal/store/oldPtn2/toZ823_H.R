# toZ823_H.R 최종접근
cntThld <- c(2,2,3,2,2)	;names(cntThld) <- c("raw","rawFV","rem","cStep","fStep")

# 공용
# undone
fCut.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF	;zMtxLen <- nrow(zMtx)	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	;rptObj<-anaMtx( stdMI$rawTail )

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

# done
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	#=====================================================================================
	#	fCutCnt.basic() 에서 정책적으로 자르기.
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	if( TRUE ){	# 소스코드 접을 수 있으려고..
		stdMI <- fCutU.getMtxInfo( zMtx )
		# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

		# raw
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( any(aZoid==stdMI$lastZoid) ) return(FALSE)
						if( 1<sum(aZoid%in%stdMI$lastZoid) ) return(FALSE)
						if( 1<sum(aZoid[c(2,5,6)]== aZoid[1]*c(2,3,4) )) return( FALSE )
						if( 1<sum( aZoid[2:6] %in% (aZoid[1]*c(2,3,4)) )) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# cStep 패턴
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]

						if( fCutU.hasPtn(c( 2, 3 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c( 3, 1 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1

						if( all(aCStep[1:2+0]==c( 9, 2)) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c( 2, 4)) ) cnt<-cnt+1

						if( aCStep[1]==sum(aCStep[2:4]) ) cnt<-cnt+10
						if( aCStep[5]==sum(aCStep[2:4]) ) cnt<-cnt+10
						if( all(aCStep[c(1,5)]== aCStep[4]*c(3,3) ) ) cnt<-cnt+10

						return( 2>cnt )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# fStep 패턴
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid

						if( aFStep[4]==sum(aFStep[c(1,6)]) ) cnt<-cnt+10
						if( aFStep[2]==sum(aFStep[c(1,5)]) ) cnt<-cnt+10
						if( aFStep[5]==sum(aFStep[c(2,6)]) ) cnt<-cnt+10

						return( 2>cnt )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# auxZW, auxQuo
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 3,1,0
						if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,2,2
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	}

	#=====================================================================================
	#	fCutCnt.nextZW() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )	
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# auxZW, auxQuo
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						if( all(quoSize[1:3+2]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,0,1
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 2 < sum(score>0) ) return( FALSE )
						if( 0 < sum(score[c("spanM","quoPtn")]) ) return( FALSE )
						
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.getNextZW( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextQuo10() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextQuo10( gEnv )$zMtx
	
	#=====================================================================================
	#	fCutCnt.nextBin() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )
		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < score["quoPtn"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextBin( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextRebNum() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# auxZW, auxQuo
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# rawFV
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# anaMtx.freqVal( stdMI$rawTail )
						cnt <- 0
						# < 3>
						if( fCutU.hasPtn(c( 3,20,10,16,34,20),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# < 5>
						# <13>
						if( fCutU.hasPtn(c( 7, 8,13,24),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
						# <16>
						if( fCutU.hasPtn(c( 3,16   ),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(   16,19),aZoid) ) cnt<-cnt+1
						# <21>					# <27>					# <30>
						# <44>
						if( fCutU.hasPtn(c(11,NA,NA,40,NA,44),aZoid) ) cnt<-cnt+1

						return( 0==cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextRebNum( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextCStepBin() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# aFStep
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid
						if( aFStep[1]%in%c(      ) ) cnt<-cnt+1
						if( aFStep[2]%in%c(      ) ) cnt<-cnt+1
						if( aFStep[3]%in%c(      ) ) cnt<-cnt+1
						if( aFStep[4]%in%c(      ) ) cnt<-cnt+1
						if( aFStep[5]%in%c(      ) ) cnt<-cnt+1
						if( aFStep[6]%in%c(   4  ) ) cnt<-cnt+1

						if( 1<sum(aFStep[1:3+1]==c(  5,   4,   6)) ) cnt<-cnt+1 # -4
						if( 1<sum(aFStep[1:3+0]==c(  5,  14,  -5)) ) cnt<-cnt+1 # -6
						if( 1<sum(aFStep[1:3+3]==c(  2,   3,   6)) ) cnt<-cnt+1 #-14

						if( (aFStep[1]==aFStep[4]) && (aFStep[4]==-aFStep[6]) ) cnt<-cnt+1
						if( aFStep[5]==sum(aFStep[c(1,2,4)]) ) cnt<-cnt+1
						if( aFStep[1]==sum(aFStep[c(2,3,6)]) ) cnt<-cnt+1
						if( aFStep[4]==sum(aFStep[c(2,3,6)]) ) cnt<-cnt+1

						return( 2>cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]			

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextCStepBin( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextFStepBin() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextFStepBin( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_1() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_1( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_2() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# aFStep(*)
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid
						if( aFStep[5]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
						if( aFStep[2]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
						if( aFStep[2]==sum(aFStep[c(4,6)]) ) cnt<-cnt+1
						if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,3,4)]) ) cnt<-cnt+1
						return( 0==cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("nbor","zw","remH0","remH1")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_2( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_3() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# auxZW, auxQuo
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]					
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						if( all(quoSize[1:3+1]==c(1,1,3)) ) return(FALSE)	# next rebind of 0,2,2 REVERSE
						if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,2,1
						if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,1
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# rawFV
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# anaMtx.freqVal( stdMI$rawTail )
						cnt <- 0
						# < 4>
						# <11>
						if( fCutU.hasPtn(c(11,12,NA,23,38),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <17>
						# <18>
						if( fCutU.hasPtn(c(18,26         ),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(18,NA,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <19>
						if( fCutU.hasPtn(c(19,28,31),aZoid) ) cnt<-cnt+1
						# <26>
						if( fCutU.hasPtn(c(18,26         ),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(   26,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <27>
						# <38>
						if( fCutU.hasPtn(c(28,NA,NA,38),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(   25,26,38),aZoid) ) cnt<-cnt+1
						# <41>
						if( fCutU.hasPtn(c(16,NA,NA,NA,41),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(   12,21,34,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

						return( 0==cnt )
					})	;kIdx<-anaFltCnt( flag,rpt)
		allIdxF <- allIdxF[flag]
		# aCStep
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]
						if( aCStep[1]%in%c( 16, 6    ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  2, 5    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  1       ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  4, 1, 8 ) ) cnt<-cnt+1

						if( 1<sum(aCStep[1:3+0]==c( 20,  2,  1 )) ) cnt<-cnt+1	#  2
						if( 1<sum(aCStep[1:3+1]==c(  5, 13,  4 )) ) cnt<-cnt+1	#  1
						if( 1<sum(aCStep[1:3+2]==c(  4,  5,  6 )) ) cnt<-cnt+1	#  8
						if( 1<sum(aCStep[1:3+2]==c( 15,  2,  5 )) ) cnt<-cnt+1	#  6

						if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1

						if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
						if( aCStep[1]== 2*sum(aCStep[c(2,4)]) ) cnt<-cnt+1

						return( 3>cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("nbor","zw","spanM")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_3( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_4() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# aFStep
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid
						if( aFStep[1]%in%c( -10     ) ) cnt<-cnt+1
						if( aFStep[2]%in%c( -11, 6  ) ) cnt<-cnt+1
						if( aFStep[3]%in%c(  -1, 4  ) ) cnt<-cnt+1
						if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
						if( aFStep[5]%in%c(  -4     ) ) cnt<-cnt+1
						if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

						if( 1<sum(aFStep[1:3+1]==c( -14, -14,  -7 )) ) cnt<-cnt+1 # 12
						if( 1<sum(aFStep[1:3+0]==c(   0,   1,   4 )) ) cnt<-cnt+1 #  5
						if( 1<sum(aFStep[1:3+2]==c(  -1,   1,   3 )) ) cnt<-cnt+1 # -1
						if( 1<sum(aFStep[1:3+3]==c(   4,  -1, -12 )) ) cnt<-cnt+1 #  1

						if( aFStep[2]==sum(aFStep[c(1,3,4,6)]) ) cnt<-cnt+1
						if( sum(aFStep[c(1,4)])==sum(aFStep[c(5,6)]) ) cnt<-cnt+1
						if( sum(aFStep[c(3,4)])==sum(aFStep[c(5,6)]) ) cnt<-cnt+1

						return( 2>cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("zw")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_4( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_5() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# rem
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# u0.zoidMtx_ana( stdMI$rawTail%%10 )
						cnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 2,1  ),c( 11    )) ) cnt<-cnt+1 # 1
						if( fCutU.remFilt(aZoid[2],c( 9,4  ),c( 19,24 )) ) cnt<-cnt+1 # 2
						if( fCutU.remFilt(aZoid[3],c( 1,2  ),c( 11,32 )) ) cnt<-cnt+1 # 3
						if( fCutU.remFilt(aZoid[4],c( 9,7  ),c( 29    )) ) cnt<-cnt+1 # 4
						if( fCutU.remFilt(aZoid[5],c( 3    ),c(       )) ) cnt<-cnt+1 # 5
						if( fCutU.remFilt(aZoid[6],c( 9,4  ),c( 39    )) ) cnt<-cnt+1 # 6
						return( 3>cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		# aCStep
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]
						if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
						if( aCStep[2]%in%c( 1, 2 ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 1    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(16, 2 ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 1    ) ) cnt<-cnt+1

						if( 1<sum(aCStep[1:3+0]==c( 1,  5, 15)) ) cnt<-cnt+1	#  6
						if( 1<sum(aCStep[1:3+0]==c( 6,  1, 10)) ) cnt<-cnt+1	#  1
						if( 1<sum(aCStep[1:3+1]==c( 4,  6, 16)) ) cnt<-cnt+1	# 10
						if( 1<sum(aCStep[1:3+2]==c( 8,  1,  8)) ) cnt<-cnt+1	#  2
						if( 1<sum(aCStep[1:3+2]==c( 6,  1, 10)) ) cnt<-cnt+1	#  8

						if( all(aCStep[c(1,5)]==(aCStep[4]*3:4)) ) cnt<-cnt+1
						if( aCStep[3]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

						return( 3>cnt )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_5( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_6() 에서 정책적으로 자르기.
	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("cStep2","cStep3")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_6( gEnv )$zMtx


	save( allIdxF ,file="Obj_fCut.basic.save" )

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
					if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 3,1,0
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,2,2
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
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 24,30    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 43,41,23 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 9>
					if( fCutU.hasPtn(c( 9,NA,NA,23),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,24,27   ),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,13   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,NA,23),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c(12,13   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,23),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(17,NA,NA,24,25,28),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c(17,25),aZoid) ) cnt<-cnt+1
					# <29>
					if( fCutU.hasPtn(c( 9, 1,20,29),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(14,15,30),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(       ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3     ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 3,2   ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,0   ),c( 24,30    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 5     ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3,1,8 ),c( 43,41,23 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 9, 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2   , 3 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(11,  1,  8)) ) cnt<-cnt+1	# 9
					if( 1<sum(aCStep[1:3+0]==c( 8,  5,  7)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 8,  5,  2)) ) cnt<-cnt+1	# 3


					if( fCutU.hasPtn(c( 2, 3 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3, 1 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1

					if( all(aCStep[1:2+0]==c( 9, 2)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 2, 4)) ) cnt<-cnt+1

					if( aCStep[1]==sum(aCStep[2:4]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[2:4]) ) cnt<-cnt+1
					if( all(aCStep[c(1,5)]== aCStep[4]*c(3,3) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3  9 12 13 25 43    | 6  3  1 12 18 |                        |2 2 1 0 1 |2 2 1 1
				#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 11   6  13  15   4 -13 |0 2 3 1 0 |2 3 1
				#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  2  10   8  10  11  15 |0 1 1 2 2 |1 1 2 2
				#     10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1
				#      1 12 13 24 29 44    |11  1 11  5 15 | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
				#      9 18 20 24 27 36(1) | 9  2  4  3  9 |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-11,  -8,  -5)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+0]==c(  2,  10,   8)) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:2+4]==c( -6,  -4     )) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+3]==c( -9,  -9,  -6)) ) cnt<-cnt+1 # -8

					if( aFStep[4]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(2,6)]) ) cnt<-cnt+1

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

# done
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
	# # -- conditional
    # flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				aRem <- aZoid %% 10
	# 				cnt <- 0
	# 				if( 1<sum(aRem[banVal.idx]==(banVal%%10)) ) cnt <- cnt + 1
	# 				return( 1>cnt )
	# 			})	;kIdx<-anaFlagFnd(!flag,rpt)
    # flgCnt[!flag] <- flgCnt[!flag] + 1

	# # -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( 1<sum(aZoid[c(1,2,3,4,5,6)]==c( 5,13,14,28,13,25)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3,4,5,6)]==c( 7,10,19,25,31,32)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3,4,5  )]==c(14, 7,11,29,21   )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,  3,4,5  )]==c( 6,   17,34,25   )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,  3,4    )]==c( 1,   18,24      )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,  3,4    )]==c( 3,   18,11      )) ) cnt<-cnt+1

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
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 31,33,23,32 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 40,39 ) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(23,27),aZoid) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[ idx ]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[ 2 ]]$fndMtx )
					if( all(aZoid[1:2+1]==c(28,37)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[ 4 ]]$fndMtx )
					if( all(aZoid[1:2+1]==c(27,28)) ) cnt<-cnt+1

					# [  1] 12 17    22 27    16 27    38 42    31 39
					# [  2]  8 11    16 23    20 32    23 27    25 38
					# [  3] 10 14    12 13    23 27    29 33    24 25
					# [  4] 14 19    28 37    17 31    23 31    34 37
					# [  5]  1  5    18 29    29 31    42 43    28 30
					# [  6]          32 33    18 30    32 35    37 40
					# [  7]           9 12    26 30    27 29         
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(       ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 0     ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 7,9   ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 1,3,2 ),c( 31,33,23,32 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 6     ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 0,9   ),c( 40,39 )) )	remCnt <- remCnt+1
						# grp 1
						# grp 2
						#	if( aZoid[3]==20 && fCutU.remFilt(aZoid[2],c( 7),c(17)) ) remCnt <- remCnt+1 
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	[1]  5   3   4   5   4 
					#	[2]  5   7   1   9  11   1   3   1   1   9   1 
					#	[3] 11  12   4  14   2  12   4  10   5   4   3   9   2   1
					#	[4]  4   4   4   8   1   3   2   8   4   3   1  11   2   1  18   7   3   1 
					#	[5]  8  13   1   3   2   3 
					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c( 5, 5,11, 4, 8 ),na.rm=T)
					matCnt <- sum(aCStep==c( 3, 7,12, 4,13 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 4, 1, 4, 4, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 5, 9,14, 8, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 4,11, 2, 1, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 1,12, 3, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 3, 4, 2,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 1,10, 8,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 1, 5, 4,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 9, 4, 3,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(NA, 1, 3, 1,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )

					cnt <- 0
						if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
						if( aCStep[2]%in%c( 1    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(10    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c( 4, 5 ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

						# if( 2<sum(aCStep[1:3+2]==c( 3, 1, 7 )) ) cnt<-cnt+1 # 
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
	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCStep <- aZoid[2:6]-aZoid[1:5]
					# [  1]             21 31 32                17 19 20
					# if( all(aCStep[1:2+3]==c(1,1)) ) return( FALSE )
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
	# anaColEndPtn() - 문제 많다. 개량필요.
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
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional custom
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]

					tCnt <- 0
						if( aCStep[1]%in%c( 1    ) ) tCnt<-tCnt+1	# 마지막 값 연속도 포함.
						if( aCStep[2]%in%c( 1, 4 ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c( 1    ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c( 3    ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c( 1    ) ) tCnt<-tCnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# [1]    7  1  4  1  6 13 12  8
					# [2]*   5 12  8 17  8  1 20  2  2  8  1  4  1  6  9  2 10 20  6  8 12  2  2  3  6  5  2  5  8  5  7 24 13  1  9  3  7 11  1  8  6 14  1  5  8  3  8 12 11  8 15  1  1  1 11 16  3  1  1 10  4 10  5  3  1 14  1 11  2  1 15  6  1  5  1  1  1 14
					# [3]    1 17 27 10  5 16  5 15 14 10  4  2 13  3
					# [4]*   2  1  8  6  2  4  8  4  1  6  3  1  8  2 14  1  8 25  6  1  7 26  5  1  1  1  1  1  4  1  3
					# [5]    2  3  4 21  1
					matCnt<-sum(aCStep==c( 7, 5, 1, 2, 2),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 1,12,17, 1, 3),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 4, 8,27, 8, 4),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 1,17,10, 6,21),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 6, 8, 5, 2, 1),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(13, 1,16, 4,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c(12,20, 5, 8,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					matCnt<-sum(aCStep==c( 8, 2,15, 4,NA),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)

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
					# [  1]  4  1     8  3     1  9     1  6
					# [  2]  6 17    20  2    24  3    10  2
					# [  3] 20  4     7  5    10  9     6  1
					# [  4]  3  3     3 27     8  3     2  3
					# [  5]  7  1    11 19    17  5     1  1
					# [  6]  5  1     1  8     2 19    19  2
					# [  7]  4  6    12  5     1 14     1  3
					# [  8]  1 11     6  1     3  5     3  2
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3,19 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2, 1 ) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[1]]$fndMtx )
					if( all(aCStep[1:2+0]==c( 5, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 8,15)) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(14, 9)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[2]]$fndMtx )
					if( all(aCStep[1:2+1]==c( 1, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(12, 4)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[3]]$fndMtx )
					if( all(aCStep[1:2+2]==c( 1, 2)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 2, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 4, 2)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 8, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(10, 9)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 4, 5)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[4]]$fndMtx )
					if( all(aCStep[1:2+3]==c( 1, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 5, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 3, 3)) ) cnt<-cnt+1

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
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6, 7 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					return( cnt<2 )
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
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 15,10    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 12,18, 8 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 13       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 26,14    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 44,43,25 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <12>
					if( fCutU.hasPtn(c( 12,NA,NA,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 12,NA,26   ),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,33,43),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(10,NA,18   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    9,18,20),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(   12,NA,24   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      17,24   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(20,NA,NA,24,45),aZoid) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 2, 3,25),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c( 1,10,39),aZoid) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c( 8, 7, 6, 8,14,42),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c(38,44),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(           ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5,0       ),c( 15,10    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 2,8       ),c( 12,18, 8 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3         ),c( 13       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 6,3,5,2,4 ),c( 26,14    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2,4,3,5   ),c( 44,43,25 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(  3       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  7, 1    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  7, 3    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  1, 3    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 18, 8, 9 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1, 12, 18)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+0]==c( 1,  1,  6)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+1]==c( 6,  3,  1)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 1, 12, 18)) ) cnt<-cnt+1	#18

					if( fCutU.hasPtn(c( 1, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10, 5),aCStep) ) cnt<-cnt+1

					if( all(aCStep[4:5] == (aCStep[1]*2:3) ) ) cnt<-cnt+1

					if( aCStep[5]==sum(aCStep[c(1,5)]) ) cnt<-cnt+1
					if( aCStep[5]==(aCStep[1]*aCStep[2]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      6 15 22 23 25 32    | 9  7  1  2  7 |                        |1 1 3 1 0 |1 1 3 1
				#      7 37 38 39 40 44    |30  1  1  1  4 |  1  22  16  16  15  12 |1 0 0 3 2 |1 3 2
				#      2 12 19 24 39 44(2) |10  7  5 15  5 | -5 -25 -19 -15  -1   0 |1 2 1 1 1 |1 2 1 1 1
				#     12 15 18 28 34 42(1) | 3  3 10  6  8 | 10   3  -1   4  -5  -2 |0 3 1 1 1 |3 1 1 1
				#     10 11 12 18 24 42(3) | 1  1  6  6 18 | -2  -4  -6 -10 -10   0 |0 4 1 0 1 |4 1 1
				#      3  9 12 13 25 43(1) | 6  3  1 12 18 | -7  -2   0  -5   1   1 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -2      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -7, -4  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  0,  2  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c( -7,  -2,   0)) ) cnt<-cnt+1 #  -2
					if( 1<sum(aFStep[1:3+0]==c( -5,   1,   1)) ) cnt<-cnt+1 #   0
					if( 1<sum(aFStep[1:3+2]==c(-10, -10,   0)) ) cnt<-cnt+1 #  -5
					if( 1<sum(aFStep[1:2+4]==c( -5, -25     )) ) cnt<-cnt+1 #   1

					if( aFStep[5]==aFStep[6] ) cnt<-cnt+1 #   1

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

# done
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
					if( all(quoSize[1:3+0]==c(0,4,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(0,4,0)) ) return(FALSE)	# next rebind of 0,1,1 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 14,13    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 13       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 45,39,18 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 2>
					if( fCutU.hasPtn(c( 2,NA,13      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,15,NA,26,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 6>
					if( fCutU.hasPtn(c( 6, 8,NA,10,27,37),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <14>					# <15>
					# <16>
					if( fCutU.hasPtn(c(16,NA,38),aZoid) ) cnt<-cnt+1
					# <17>
					# <19>
					if( fCutU.hasPtn(c(19,25),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(16,NA,38),aZoid) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(18,22,11,30,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <45>
					if( fCutU.hasPtn(c(22,NA,NA,NA,35,45),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   14,NA,NA,NA,45),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 0       ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 4,3,5   ),c( 14,13    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5       ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6,3     ),c( 13       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3,6     ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,9,7,8 ),c( 45,39,18 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1, 2 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 1,  4    )) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:2+0]==c(10,  7    )) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+1]==c( 3,  1,  4)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 1, 21,  7)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+2]==c( 5, 10,  7)) ) cnt<-cnt+1	#  4

					if( (aCStep[1]==aCStep[3]) ) cnt<-cnt+1

					if( all(aCStep[1:2]==aCStep[c(3,5)]) ) cnt<-cnt+1
					if( aCStep[1]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[3]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#      1 11 15 17 25 39    |10  4  2  8 14 |                        |1 3 1 1 0 |1 3 1 1
					#      2  5 15 18 19 23(1) | 3 10  3  1  4 |  1  -6   0   1  -6 -16 |2 3 1 0 0 |2 3 1
					#      6 16 37 38 41 45    |10 21  1  3  4 |  4  11  22  20  22  22 |1 1 0 2 2 |1 1 2 2
					#      6 12 19 24 34 41(2) | 6  7  5 10  7 |  0  -4 -18 -14  -7  -4 |1 2 1 1 1 |1 2 1 1 1
					#     14 15 16 17 38 45    | 1  1  1 21  7 |  8   3  -3  -7   4   4 |0 4 0 1 1 |4 1 1
					#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -2 ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -1 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -5 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+3]==c(  4,  11,  22 )) ) cnt<-cnt+1 #  -6
					if( 1<sum(aFStep[1:3+3]==c( 20,  22,  22 )) ) cnt<-cnt+1 #  -6

					if( aFStep[5]==aFStep[6] ) cnt<-cnt+1

					if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(3,4,5,6 )]) ) cnt<-cnt+1

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

# done
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


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 13    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 15, 9 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <15>
					# <17>
					if( fCutU.hasPtn(c(   17,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,17,31,27   ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c(25,33),aZoid) ) cnt<-cnt+1
					# <28>
					if( fCutU.hasPtn(c(25,25,28,39),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <32>
					# <39>
					if( fCutU.hasPtn(c(   17,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,NA,31,27,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <40>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 0,4     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3,5     ),c( 13    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7       ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,0,5,9 ),c( 15, 9 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 7,5,2   ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 9       ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 11, 3    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  4       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  3, 6,25 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  1, 5    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1,  5,  7)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+0]==c(25,  1,  3)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c( 2, 25,  1)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c(10, 11,  1)) ) cnt<-cnt+1	#  3

					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 6),aCStep) ) cnt<-cnt+1

					if( aCStep[5]==sum(aCStep[c(2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      5 17 25 31 39 40    |12  8  6  8  1 |                        |1 1 1 2 1 |1 1 1 2 1
				#      1 10 20 32 35 40(1) | 9 10 12  3  5 | -4  -7  -5   1  -4   0 |1 1 1 2 1 |1 1 1 2 1
				#      2  7 17 28 29 39    | 5 10 11  1 10 |  1  -3  -3  -4  -6  -1 |2 1 2 1 0 |2 1 2 1
				#      9 15 16 21 28 34(1) | 6  1  5  7  6 |  7   8  -1  -7  -1  -5 |1 2 2 1 0 |1 2 2 1
				#      6 15 22 23 25 32(1) | 9  7  1  2  7 | -3   0   6   2  -3  -2 |1 1 3 1 0 |1 1 3 1
				#      3 13 15 40 41 44(1) |10  2 25  1  3 | -3  -2  -7  17  16  12 |1 2 0 0 3 |1 2 3
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  7, -3  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -1      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -3      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3,  -2,  -7)) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:2+0]==c( 16,  12     )) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+0]==c(  0,   6,   2)) ) cnt<-cnt+1 # -7

					if( sum(aFStep[c(3,4)])==sum(aFStep[c(2,6)]) ) cnt<-cnt+1

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

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 39 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 25 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 40 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 42 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,20,10,16,34,20),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 5>
					# <13>
					if( fCutU.hasPtn(c( 7, 8,13,24),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <16>
					if( fCutU.hasPtn(c( 3,16   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   16,19),aZoid) ) cnt<-cnt+1
					# <21>					# <27>					# <30>
					# <44>
					if( fCutU.hasPtn(c(11,NA,NA,40,NA,44),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 9    ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 2,5  ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5    ),c( 25 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 2    ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(10    ),c( 40 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2    ),c( 42 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 5, 4 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4,20 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1,  8,  6)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c( 2,  1,  8)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c( 8, 18,  4)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 8, 18,  4)) ) cnt<-cnt+1	#  1

					if( (aCStep[1]==aCStep[2])&&(aCStep[3]==aCStep[4]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      5 16 21 23 24 30    |11  5  2  1  6 |                        |1 1 3 1 0 |1 1 3 1
				#      3  4 16 20 28 44(1) | 1 12  4  8 16 | -2 -12  -5  -3   4  14 |2 1 2 0 1 |2 1 2 1
				#      7 27 29 30 38 44(1) |20  2  1  8  6 |  4  23  13  10  10   0 |1 0 2 2 1 |1 2 2 1
				#      3 12 13 18 31 32    | 9  1  5 13  1 | -4 -15 -16 -12  -7 -12 |1 3 0 2 0 |1 3 2
				#      5 10 13 21 39 43(1) | 5  3  8 18  4 |  2  -2   0   3   8  11 |1 2 1 1 1 |1 2 1 1 1
				#     17 21 25 26 27 36(1) | 4  4  1  1  9 | 12  11  12   5 -12  -7 |0 1 4 1 0 |1 4 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3,  -8, -11)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:2+0]==c(-12,  -7     )) ) cnt<-cnt+1 # 11
					if( 1<sum(aFStep[1:3+1]==c(  0,  -3,  -8)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c(-23, -13, -10)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+3]==c(  0,   3,   8)) ) cnt<-cnt+1 #-12
					if( 1<sum(aFStep[1:3+3]==c(  0,   3,   8)) ) cnt<-cnt+1 # -7

					if( all(aFStep[c(1,3)]== aFStep[5]*c(-1,-1) ) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(4,5)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
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

} # fCutCnt.nextRebNum()

# done
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


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 17 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 26 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,11   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,13),aZoid) ) cnt<-cnt+1
					# < 6>
					if( fCutU.hasPtn(c( 6,NA,14,15,35,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <15>
					# <16>
					if( fCutU.hasPtn(c(13,16),aZoid) ) cnt<-cnt+1
					# <32>
					if( fCutU.hasPtn(c(11,29,32),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3   ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 4   ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c( 17 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 6,3 ),c( 26 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 8   ),c(    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 10   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  1   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  3   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  4,2 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1, 11,  1)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c(10,  2, 25)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+2]==c( 1, 11,  1)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:2+3]==c( 1, 11    )) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 1,11 ),aCStep) ) cnt<-cnt+1

					if( (aCStep[1]==aCStep[3]) && (aCStep[3]==aCStep[4]) ) cnt<-cnt+1
					if( all(aCStep[3:4]== aCStep[1]*c(1,1) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      9 15 16 21 28 34    | 6  1  5  7  6 |                        |1 2 2 1 0 |1 2 2 1
				#      6 15 22 23 25 32(1) | 9  7  1  2  7 | -3   0   6   2  -3  -2 |1 1 3 1 0 |1 1 3 1
				#      3 13 15 40 41 44(1) |10  2 25  1  3 | -3  -2  -7  17  16  12 |1 2 0 0 3 |1 2 3
				#      2  4  5 17 27 32    | 2  1 12 10  5 | -1  -9 -10 -23 -14 -12 |3 1 1 1 0 |3 1 1 1
				#      6  7 18 19 30 38    | 1 11  1 11  8 |  4   3  13   2   3   6 |2 2 0 2 0 |2 2 2
				#      1  3 12 14 16 43    | 2  9  2  2 27 | -5  -4  -6  -5 -14   5 |2 3 0 0 1 |2 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(   4  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c(  5,   4,   6)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+0]==c(  5,  14,  -5)) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:3+3]==c(  2,   3,   6)) ) cnt<-cnt+1 #-14

					if( (aFStep[1]==aFStep[4]) && (aFStep[4]==-aFStep[6]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,2,4)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(2,3,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(2,3,6)]) ) cnt<-cnt+1

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

# done
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
					if( all(quoSize[1:3+1]==c(0,0,3)) ) return(FALSE)	# next rebind of 2,1,1 reverse
					if( all(quoSize[1:3+2]==c(2,1,3)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  7          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(             ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 37,20,28,13 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(             ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 36,29       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 42          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 5>
					# <12>
					if( fCutU.hasPtn(c(   12,20      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,12,NA,31,32),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c(13,NA,29),aZoid) ) cnt<-cnt+1
					# <20>
					if( fCutU.hasPtn(c(   12,20      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,20,31,32),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(19,26),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(      27,NA,NA,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(25,NA,27,40      ),aZoid) ) cnt<-cnt+1
					# <28>
					if( fCutU.hasPtn(c(15, 9, 3,28,45),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <29>
					if( fCutU.hasPtn(c(11, 7,13,24,29,36),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(      28,NA,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,NA,NA,NA,38),aZoid) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(11, 8,10,20,32,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7       ),c(  7          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 6       ),c(             )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,0,8,3 ),c( 37,20,28,13 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6,5,6   ),c(             )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 6,7,9   ),c( 36,29       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2       ),c( 42          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2, 8 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 5,  2, 13)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:2+2]==c( 4,  3    )) ) cnt<-cnt+1	# 13
					if( 1<sum(aCStep[1:3+1]==c( 7,  4,  3)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+2]==c(12,  2,  7)) ) cnt<-cnt+1	#  6

					if( aCStep[3]==sum(aCStep[c(1,2,5)]) ) cnt<-cnt+1
					if( sum(aCStep[c(2,3)])==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      5 18 20 36 42 43    |13  2 16  6  1 |                        |1 1 1 1 2 |1 1 1 1 2
				#      5  9 12 20 21 26(2) | 4  3  8  1  5 |  0  -9  -8 -16 -21 -17 |2 1 3 0 0 |2 1 3
				#      1 17 27 28 29 40    |16 10  1  1 11 | -4   8  15   8   8  14 |1 1 3 0 1 |1 1 3 1
				#      6 12 20 26 29 38(1) | 6  8  6  3  9 |  5  -5  -7  -2   0  -2 |1 1 3 1 0 |1 1 3 1
				#     13 25 27 34 38 41(1) |12  2  7  4  3 |  7  13   7   8   9   3 |0 1 2 2 1 |1 2 2 1
				#      8 13 15 28 37 43(1) | 5  2 13  9  6 | -5 -12 -12  -6  -1   2 |1 2 1 1 1 |1 2 1 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  8 ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( 10 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 13,   7,   8)) ) cnt<-cnt+1 #  -5
					if( 1<sum(aFStep[1:3+3]==c(  8,   9,   3)) ) cnt<-cnt+1 #   2

					if( all(aFStep[2:4]== aFStep[6]*c(-6,-6,-3)) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(4,5,6)]) ) cnt<-cnt+1

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

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 20    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 31,15 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 8>
					# <10>
					if( fCutU.hasPtn(c(10,33,42,42),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <11>
					if( fCutU.hasPtn(c( 6,11,18,16),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 6,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,35,NA,45),aZoid) ) cnt<-cnt+1
					# <15>
					# <20>
					if( fCutU.hasPtn(c(18,20,35),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c( 8,24,27),aZoid) ) cnt<-cnt+1
					# <31>
					if( fCutU.hasPtn(c(      31,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8,13,31      ),aZoid) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(20,18,35,37,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <45>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)

    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 0       ),c( 20    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 8       ),c(  8    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 0       ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,5     ),c( 31,15 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8       ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1,3,2,4 ),c( 41    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 12, 8, 6 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  5, 3,11 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 11, 3    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  8, 3    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  2, 6    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(10,  5, 11)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+1]==c(11,  3,  8)) ) cnt<-cnt+1	# 11
					if( 1<sum(aCStep[1:3+2]==c(11,  3,  8)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 3,  4,  3)) ) cnt<-cnt+1	#  8

					if( fCutU.hasPtn(c( 6,11 ),aCStep) ) cnt<-cnt+1

					if( aCStep[3]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1
					if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      2  8 13 25 28 37    | 6  5 12  3  9 |                        |2 1 2 1 0 |2 1 2 1
				#      7 13 30 39 41 45(1) | 6 17  9  2  4 |  5   5  17  14  13   8 |1 1 0 2 2 |1 1 2 2
				#      4 10 11 12 20 27    | 6  1  1  8  7 | -3  -3 -19 -27 -21 -18 |1 3 2 0 0 |1 3 2
				#      1  8 11 15 18 45(1) | 7  3  4  3 27 | -3  -2   0   3  -2  18 |2 3 0 0 1 |2 3 1
				#     10 22 27 31 42 43    |12  5  4 11  1 |  9  14  16  16  24  -2 |0 1 2 1 2 |1 2 1 2
				#      5 15 20 31 34 42(2) |10  5 11  3  8 | -5  -7  -7   0  -8  -1 |1 1 1 2 1 |1 1 1 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  0 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+2]==c( 14,  16,  16 )) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+2]==c( 27,  21,  18 )) ) cnt<-cnt+1 # -8

					if( aFStep[5]==sum(aFStep[c(2,6)]) ) cnt<-cnt+1
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

} # fCutCnt.nextColVal_1()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,1,1)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  2, 1 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 16,23 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 16    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 34    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 33    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,NA,NA,25),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,14,15      ),aZoid) ) cnt<-cnt+1
					# < 4>
					if( fCutU.hasPtn(c( 4, 9         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,NA,25      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,NA,NA,17,27),aZoid) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 4, 9         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,23,26,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 9>
					if( fCutU.hasPtn(c( 2, 9 ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,NA,13),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 9,NA,13),aZoid) ) cnt<-cnt+1
					# <16>
					# <25>
					if( fCutU.hasPtn(c( 3,NA,NA,NA,25),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,15,NA,25),aZoid) ) cnt<-cnt+1
					# <33>
					# <38>
					if( fCutU.hasPtn(c(14,NA,NA,38),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 2,1     ),c(  2, 1 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3,6     ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 6,3     ),c( 16,23 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6,5     ),c( 16    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4       ),c( 34    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2,8,7,3 ),c( 33    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2, 6, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1,15    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1,  5, 15)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c( 9,  2, 16)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c( 3,  1, 12)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 9,  2, 16)) ) cnt<-cnt+1	# 18

					if( all( aCStep[4:5]==(aCStep[1]*2:3)) ) cnt<-cnt+1

					if( all(aCStep[c(1,4)]== aCStep[2]*c(2,4) ) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4  9 23 33 39 44    | 5 14 10  6  5 |                        |2 0 1 2 1 |2 1 2 1
				#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  3   0 -13 -20  -8  -9 |2 2 0 2 0 |2 2 2
				#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
				#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3  -7   6  -9  -5 |3 0 2 1 0 |3 2 1
				#     14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
				#      3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(   8 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -13 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 11,   6,   4 )) ) cnt<-cnt+1 # -11
					if( 1<sum(aFStep[1:3+1]==c(  2,  -3,  -7 )) ) cnt<-cnt+1 #  -6
					if( 1<sum(aFStep[1:3+3]==c( -2,   6,   5 )) ) cnt<-cnt+1 # -13
					if( 1<sum(aFStep[1:2+4]==c(  2,  -3      )) ) cnt<-cnt+1 #  -2

					if( aFStep[5]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(4,6)]) ) cnt<-cnt+1
					if( sum(aFStep[c(2,5)])==sum(aFStep[c(1,3,4)]) ) cnt<-cnt+1

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

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,3)) ) return(FALSE)	# next rebind of 0,2,2 REVERSE
					if( all(quoSize[1:3+2]==c(1,0,2)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 27    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 16,17 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 19    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 39,41 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 4>
					# <11>
					if( fCutU.hasPtn(c(11,12,NA,23,38),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <17>
					# <18>
					if( fCutU.hasPtn(c(18,26         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(18,NA,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(19,28,31),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(18,26         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   26,39,28,35),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <27>
					# <38>
					if( fCutU.hasPtn(c(28,NA,NA,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   25,26,38),aZoid) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(16,NA,NA,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   12,21,34,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7     ),c( 27    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 7,8,6 ),c( 16,17 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,3   ),c( 19    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 5     ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0,9   ),c( 39,41 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(       ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 16, 6    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  2, 5    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  1       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  4, 1, 8 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 20,  2,  1 )) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c(  5, 13,  4 )) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c(  4,  5,  6 )) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 15,  2,  5 )) ) cnt<-cnt+1	#  6

					if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1

					if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[1]== 2*sum(aCStep[c(2,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     11 18 26 27 40 41    | 7  8  1 13  1 |                        |0 2 2 0 2 |2 2 2
				#      1 11 15 17 25 39(1) |10  4  2  8 14 |-10  -7 -11 -10 -15  -2 |1 3 1 1 0 |1 3 1 1
				#      4 18 26 33 34 38    |14  8  7  1  4 |  3   7  11  16   9  -1 |1 1 1 3 0 |1 1 1 3
				#      4  8 13 19 20 43(1) | 4  5  6  1 23 |  0 -10 -13 -14 -14   5 |2 2 1 0 1 |2 2 1 1
				#      2 17 19 24 37 41(1) |15  2  5 13  4 | -2   9   6   5  17  -2 |1 2 1 1 1 |1 2 1 1 1
				#      7 27 29 30 38 44    |20  2  1  8  6 |  5  10  10   6   1   3 |1 0 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  6,-10  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -3      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  6,   1,   3 )) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+0]==c(  2,  -9,  -6 )) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+1]==c(  2,  -9,  -6 )) ) cnt<-cnt+1 # 10
					if( 1<sum(aFStep[1:3+2]==c( 10,  10,   6 )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+2]==c( 14,  14,  -5 )) ) cnt<-cnt+1 #  1

					if( aFStep[2]==sum(aFStep[c(4,5,6)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(4,5,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( all(aFStep[2:3]== aFStep[1]*c(2,2) ) ) cnt<-cnt+1

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

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 31,34 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,1,0
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
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 23       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 26,28,29 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <15>
					if( fCutU.hasPtn(c(15,33,33,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <17>
					# <21>
					if( fCutU.hasPtn(c(21,28),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(16,NA,23,24,25,41),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c( 7, 9,18,NA,26),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(    21,NA,27),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 19,NA,NA,27),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c( 5,22,14,20,24,36),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4       ),c(          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 6       ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(         ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3       ),c( 23       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 6,9,8,3 ),c( 26,28,29 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3       ),c(          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 9, 2 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 9    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(11, 2 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  4,  3)) ) cnt<-cnt+1	# 9
					if( 1<sum(aCStep[1:3+1]==c( 9,  2,  4)) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:2+3]==c( 9,  2    )) ) cnt<-cnt+1	# 9

					if( fCutU.hasPtn(c( 2, 2 ),aCStep) ) cnt<-cnt+1

					if( aCStep[1]==sum(aCStep[c(2,3,4)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(2,3,4)]) ) cnt<-cnt+1
					if( all(aCStep[c(1,5)]== aCStep[4]*c(3,3) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      2  8 17 24 29 31    | 6  9  7  5  2 |                        |2 1 2 1 0 |2 1 2 1
				#     13 14 26 28 30 36    | 1 12  2  2  6 | 11   6   9   4   1   5 |0 2 2 2 0 |2 2 2
				#      1  3 12 21 26 41(1) | 2  9  9  5 15 |-12 -11 -14  -7  -4   5 |2 1 2 0 1 |2 1 2 1
				#     15 17 19 21 27 45(1) | 2  2  2  6 18 | 14  14   7   0   1   4 |0 3 2 0 1 |3 2 1
				#      4  6 15 25 26 33(1) | 2  9 10  1  7 |-11 -11  -4   4  -1 -12 |2 1 2 1 0 |2 1 2 1
				#      9 18 20 24 27 36    | 9  2  4  3  9 |  5  12   5  -1   1   3 |1 1 3 1 0 |1 1 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -10     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -11, 6  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  -1, 4  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  -4     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c( -14, -14,  -7 )) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+0]==c(   0,   1,   4 )) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+2]==c(  -1,   1,   3 )) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(   4,  -1, -12 )) ) cnt<-cnt+1 #  1

					if( aFStep[2]==sum(aFStep[c(1,3,4,6)]) ) cnt<-cnt+1
					if( sum(aFStep[c(1,4)])==sum(aFStep[c(5,6)]) ) cnt<-cnt+1
					if( sum(aFStep[c(3,4)])==sum(aFStep[c(5,6)]) ) cnt<-cnt+1

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

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 27,36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+1]==c(0,1,1)) ) return(FALSE)	# next rebind of 3,1,2
					if( all(quoSize[1:3+0]==c(1,1,4)) ) return(FALSE)	# next rebind of 1,3,0 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 11    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 19,24 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 11,32 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 29    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <11>
					# <18>
					if( fCutU.hasPtn(c(   18,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8,18,NA,34,23),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(   18,19      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,NA,19,23,23),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <24>					# <33>
					# <35>
					if( fCutU.hasPtn(c( 5,NA,38),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(22,26,27,NA,27,39),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 2,1  ),c( 11    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 9,4  ),c( 19,24 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 1,2  ),c( 11,32 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 9,7  ),c( 29    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3    ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 9,4  ),c( 39    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 2 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(16, 2 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1,  5, 15)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c( 6,  1, 10)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 4,  6, 16)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+2]==c( 8,  1,  8)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c( 6,  1, 10)) ) cnt<-cnt+1	#  8

					if( all(aCStep[c(1,5)]==(aCStep[4]*3:4)) ) cnt<-cnt+1
					if( aCStep[3]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     11 24 32 33 35 40    |13  8  1  2  5 |                        |0 1 1 3 1 |1 1 3 1
				#     23 27 28 38 42 43    | 4  1 10  4  1 | 12   3  -4   5   7   3 |0 0 3 1 2 |3 1 2
				#      6  7 11 17 33 44    | 1  4  6 16 11 |-17 -20 -17 -21  -9   1 |2 2 0 1 1 |2 2 1 1
				#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  5   9   7   2  -9  -5 |0 4 1 1 0 |4 1 1
				#      2 10 11 19 35 39(3) | 8  1  8 16  4 | -9  -6  -7   0  11   0 |1 3 0 2 0 |1 3 2
				#     12 18 19 29 31 39(2) | 6  1 10  2  8 | 10   8   8  10  -4   0 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  0,  -5 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+3]==c(-20, -17, -21)) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c( 10,  -4,   0)) ) cnt<-cnt+1 #  0

					if( (aFStep[1]==aFStep[4]) && (aFStep[2]==aFStep[3]) ) cnt<-cnt+1

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

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,0)) ) return(FALSE)	# next rebind of 0,3,1
					if( all(quoSize[1:3+2]==c(0,3,1)) ) return(FALSE)	# next rebind of 1,2,0 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  3,10 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 20    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 30,19 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 31,42 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,14      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,16,NA,NA,19),aZoid) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,16),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,NA,20      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,NA,NA,NA,30),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,23,NA,27   ),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(11,NA,NA,26),aZoid) ) cnt<-cnt+1
					# <31>
					if( fCutU.hasPtn(c(21,24,25,NA,31),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(23,31,38,NA,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(19,27,34,NA,34,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,3,0,4),c(  3,10 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0      ),c( 20    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9      ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 0,9    ),c( 30,19 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 1,2    ),c( 31,42 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1      ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 6 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 7    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1,  5, 13)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:3+0]==c( 6,  1, 10)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:2+3]==c( 5,  6    )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 1,  5, 13)) ) cnt<-cnt+1	# 8

					if( fCutU.hasPtn(c( 9, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5, 6),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 3, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 6, 1)) ) cnt<-cnt+1

					if( all(aCStep[c(1,5)]==(aCStep[4]*3:4)) ) cnt<-cnt+1
					if( aCStep[3]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     10 14 16 18 27 28    | 4  2  2  9  1 |                        |0 4 2 0 0 |4 2
				#      1  3  8 12 42 43    | 2  5  4 30  1 | -9 -11  -8  -6  15  15 |3 1 0 0 2 |3 1 2
				#     10 15 21 35 38 43(1) | 5  6 14  3  5 |  9  12  13  23  -4   0 |0 2 1 2 1 |2 1 2 1
				#     12 17 23 34 42 45    | 5  6 11  8  3 |  2   2   2  -1   4   2 |0 2 1 1 2 |2 1 1 2
				#      3 12 13 18 31 32(1) | 9  1  5 13  1 | -9  -5 -10 -16 -11 -13 |1 3 0 2 0 |1 3 2
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

					if( 1<sum(aFStep[1:3+0]==c(   2,   2,   2 )) ) cnt<-cnt+1 #  9
					if( 1<sum(aFStep[1:3+0]==c( -13, -23,   4 )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+1]==c( -13, -23,   4 )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+2]==c(  11,   0,   7 )) ) cnt<-cnt+1 # 11
					if( 1<sum(aFStep[1:3+2]==c(  -1,   4,   2 )) ) cnt<-cnt+1 #  0

					if( (aFStep[2]==aFStep[3]) ) cnt<-cnt+1

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



