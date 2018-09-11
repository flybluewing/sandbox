# toZ824_H.R 최종접근
cntThld <- c(2,2,3,2,2)	;names(cntThld) <- c("raw","rawFV","rem","cStep","fStep")

# 공용
# done
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

# undone
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
						# if( 1<sum(aZoid[c(2,5,6)]== aZoid[1]*c(2,3,4) )) return( FALSE )
						# if( 1<sum( aZoid[2:6] %in% (aZoid[1]*c(2,3,4)) )) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# cStep 패턴
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]

						# if( fCutU.hasPtn(c( 2, 3 ),aCStep) ) cnt<-cnt+1
						# if( fCutU.hasPtn(c( 3, 1 ),aCStep) ) cnt<-cnt+1
						# if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
						# if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1

						# if( all(aCStep[1:2+0]==c( 9, 2)) ) cnt<-cnt+1
						# if( all(aCStep[1:2+2]==c( 2, 4)) ) cnt<-cnt+1

						# if( aCStep[1]==sum(aCStep[2:4]) ) cnt<-cnt+10
						# if( aCStep[5]==sum(aCStep[2:4]) ) cnt<-cnt+10
						# if( all(aCStep[c(1,5)]== aCStep[4]*c(3,3) ) ) cnt<-cnt+10

						return( 2>cnt )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# fStep 패턴
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid

						# if( aFStep[4]==sum(aFStep[c(1,6)]) ) cnt<-cnt+10
						# if( aFStep[2]==sum(aFStep[c(1,5)]) ) cnt<-cnt+10
						# if( aFStep[5]==sum(aFStep[c(2,6)]) ) cnt<-cnt+10

						return( 2>cnt )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# auxZW, auxQuo
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						# if( all(quoSize[1:3+2]==c(1,2,2)) ) return(FALSE)	# next rebind of 3,1,0
						# if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,2,2
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						# if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < score["quoAll"] ) return( FALSE )
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
					if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,1)) ) return(FALSE)	# next rebind of 0,2,2 reverse
					if( all(quoSize[1:3+1]==c(2,0,1)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 3,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  8          ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 18,12,24,29 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 25          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 26          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 29          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(             ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <12>
					if( fCutU.hasPtn(c(12,NA,24      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,23,NA,23,34),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(15,18,28,28,NA,44),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c(      24,25   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,16,24,NA,42),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c(17,25),aZoid) ) cnt<-cnt+1
					# <29>
					if( fCutU.hasPtn(c( 9, 1,20,29),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(14,15,30),aZoid) ) cnt<-cnt+1
					# <40>
					if( fCutU.hasPtn(c(23,NA,NA,40),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(         ),c(  8          )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 8,2,4,9 ),c( 18,12,24,29 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5,7     ),c( 25          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6       ),c( 26          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 9,5,0   ),c( 29          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(         ),c(             )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  3, 1 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  2,11 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+1]==c( 6,  6,  2 )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:2+3]==c( 9,  2     )) ) cnt<-cnt+1	# 1

					if( fCutU.hasPtn(c(11, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 6, 6)) ) cnt<-cnt+1

					if( all(aCStep[1:2]== aCStep[3]*c(3,3) ) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(1,2,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     14 15 25 28 29 30    | 1 10  3  1  1 |                        |0 2 3 1 0 |2 3 1
				#     16 25 33 38 40 45(1) | 9  8  5  2  5 |  2  10   8  10  11  15 |0 1 1 2 2 |1 1 2 2
				#     10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1
				#      1 12 13 24 29 44    |11  1 11  5 15 | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
				#      9 18 20 24 27 36(1) | 9  2  4  3  9 |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
				#     12 18 24 26 39 40(2) | 6  6  2 13  1 |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  4,   2,  12 )) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:2+1]==c(  9,   9      )) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+1]==c(  0,  -2,  -8 )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c(  9,   9      )) ) cnt<-cnt+1 #  4

					if( (aFStep[5]== (aFStep[1]*aFStep[3]) ) ) cnt<-cnt+1
					if( (aFStep[5]== (aFStep[1]*aFStep[6]) ) ) cnt<-cnt+1
					if( all(aFStep[c(3,6)]== (aFStep[4]*c(2,2)) ) ) cnt<-cnt+1

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
						# [1]   21 25 24
						# [2]   11 14
						# [3]   32 29 21 29
						# [4]   
						# [5]   
						# [6]*  31 31 33
					# if( 1<sum(aZoid[c(1,2,3,4,5,6)]==c(  ,  ,  ,  ,  ,  )) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3    ,6)]==c(21,11,32,      31)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1,2,3    ,6)]==c(25,14,29,      31)) ) cnt<-cnt+1
					if( 1<sum(aZoid[c(1  ,3    ,6)]==c(24   ,21,      33)) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 17, 8,35,NA,NA,31 ),aZoid) ) cnt<-cnt+1

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
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 29 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 29 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 33 ) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(23,27),aZoid) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[ idx ]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[ 5 ]]$fndMtx )
					if( all(aZoid[1:2+4]==c(18,23)) ) cnt<-cnt+1

						# [  1]  3  9    26 30    15 19    39 40    28 32
						# [  2] 14 26    28 38    33 41    22 35    16 31
						# [  3] 18 23    31 32    28 30    19 39    19 23
						# [  4]  9 15     9 15    12 14             28 36
						# [  5]  9 18    27 29     8 19             16 38
						# [  6]  3  4     1  8    27 31             31 42
						# [  7] 25 39     2  6    30 33             26 42
						# [  8] 15 17     7 11    15 27             28 39
						# [  9]                   24 33             38 45
						# [ 10]                                     36 44
						# [ 11]                                     20 23
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 2, 0    ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 7, 2, 1 ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 1, 4, 9 ),c( 29 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 7,      ),c( 29 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c(         ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 4, 3    ),c( 33 )) )	remCnt <- remCnt+1
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
					#	[1]  6  12   5   6   9   1  14   2 
					#	[2]  4  10   1   6   2   7   4   4 
					#	[3]  4   8   2   2  11   4   3  12   9 
					#	[4]  1  13  20 
					#	[5]  4  15   4   8  22  11  16  11   7   8   3 
					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c( 6, 4, 4, 1, 4 ),na.rm=T)
					matCnt <- sum(aCStep==c(12,10, 8,13,15 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 5, 1, 2,20, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 6, 6, 2,NA, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 9, 2,11,NA, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 1, 7, 4,NA,11 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(14, 4, 3,NA,16 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 2, 4,12,NA,11 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )

					cnt <- 0
						if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 6 ) ) cnt<-cnt+1

						if( (aCStep[2]==aCStep[3]) && (aCStep[2]==aCStep[5]) ) cnt<-cnt+1
						# if( 2<sum(aCStep[1:3+2]==c( 3, 1, 7 )) ) cnt<-cnt+1 # 
					score <- score + ifelse(cnt>1,1,0)

					return( score )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt=>2] <- flgCnt[fltCnt=>2] + 1

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
					# [  1] 14 26 30     1  8 19 
					if( aZoid[3]==sum(aZoid[1:2]) ) return( FALSE )
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

					tCnt <- 0
						if( aCStep[1]%in%c(         ) ) tCnt<-tCnt+1	# 마지막 값 연속도 포함.
						if( aCStep[2]%in%c( 7       ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(         ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c( 5, 4    ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c( 2, 3, 7 ) ) tCnt<-tCnt+1

						if( aCStep[1]==sum(aCStep[3:5]) ) cnt<-cnt+1
						if( aCStep[2]==sum(aCStep[3:5]) ) cnt<-cnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

						# [1]*  18  4 12  6  3  8  1  1  8  3
						# [2]*  18  1  3  7  5 19  6  3  3  1  2  9  7  6  3 16 11  7 10  7 11  9  4 15 16  3  2 22 12  5 12
						# [3]*   6  9 10  3  6 20  3  7  6  3  5  1  9  3  3  1 15  4 10 11 18  2  2  1  2  8  1 13  4 13  1  1  5 13  5  9  3  1  1
						# [4]*   5  5  4 12  9 12  2  7  1  9  9
						# [5]*   7  2  2  2  3  3  7  3 11 11  1  8  1  7  3  1  5  2  1  7  4  1  4  4  2  7  4  3 13
					# matCnt<-sum(aCStep==c(18,18, 6, 5, 7),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
					if( is.null(valMtx) ){
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
						# [  1] 12  4     3  9     3  3     6  1
						# [  2]  4 13     9  9     6  5     9 10
						# [  3]  5  2     3  1     8 12     3  4
						# [  4]  4  6     4  1     2  2    18  4
						# [  5]  4  4     1 16     2 10     7  7
						# [  6]  1 10     4  7     1 12    12  9
						# [  7] 14  1     6  3     5  6     9 11
						# [  8] 13  6     2  4     4  3     4  6
					if( aCStep[1]%in%c( 4, 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3, 9, 2 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 9       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(12       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[1]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[2]]$fndMtx )
					if( all(aCStep[1:2+1]==c( 5, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 5, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 3, 5)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[3]]$fndMtx )
					if( all(aCStep[1:2+2]==c( 2, 8)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 2, 2)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 2, 3)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 1, 4)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[4]]$fndMtx )
					if( all(aCStep[1:2+3]==c( 7, 7)) ) cnt<-cnt+1

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
					# if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
					# if( aCStep[2]%in%c( 6, 7 ) ) cnt<-cnt+1
					# if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					# if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					# if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

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
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(2,2,1)) ) return(FALSE)	# next rebind of 3,1,0
					if( all(quoSize[1:3+2]==c(1,0,3)) ) return(FALSE)	# next rebind of 0,3,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 6, 4 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 2>
					if( fCutU.hasPtn(c( 2,17,23,32,37,41),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 6>
					# <11>
					if( fCutU.hasPtn(c(10,11,NA,NA,18),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(      15,16   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,NA,15,NA,27),aZoid) ) cnt<-cnt+1
					# <17>
					if( fCutU.hasPtn(c(      14,17   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,13,NA,17,27),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(18,43,45),aZoid) ) cnt<-cnt+1
					# <19>					# <23>					# <28>
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(13,35,NA,38),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6,4,1,0 ),c( 6, 4 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 9,1,6,5 ),c(      )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 6       ),c(      )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4       ),c(      )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(         ),c(      )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,2     ),c(      )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2, 5 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1, 5 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4, 1 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  6, 17 )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 3,  4,  7 )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 1, 10,  4 )) ) cnt<-cnt+1	# 6

					if( fCutU.hasPtn(c( 4, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3, 4),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(5,4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(1,2)) ) cnt<-cnt+1

					if( aCStep[4]==sum(aCStep[c(2,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[ 1:4  ]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  5 15 18 19 23    | 3 10  3  1  4 |                        |2 3 1 0 0 |2 3 1
			#      2 11 19 25 28 32(2) | 9  8  6  3  4 |  0   6   4   7   9   9 |1 2 2 1 0 |1 2 2 1
			#     23 27 28 38 42 43(1) | 4  1 10  4  1 | 21  16   9  13  14  11 |0 0 3 1 2 |3 1 2
			#      4  8  9 16 17 19    | 4  1  7  1  2 |-19 -19 -19 -22 -25 -24 |3 3 0 0 0 |3 3
			#      6 18 31 34 38 45    |12 13  3  4  7 |  2  10  22  18  21  26 |1 1 0 3 1 |1 1 3 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  0  -7 -16 -17 -15  -5 |1 3 1 0 1 |1 3 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( 23 ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 21,  16,   9)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+0]==c( -9, -13, -14)) ) cnt<-cnt+1 # -7
					if( 1<sum(aFStep[1:3+1]==c( 19,  19,  19)) ) cnt<-cnt+1 #-16

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
					if( (aZoid[6]-aZoid[1]) %in% c( 16 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,1,2)) ) return(FALSE)	# next rebind of 0,1,2 reverse
					if( all(quoSize[1:3+1]==c(0,2,3)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(2,3,1)) ) return(FALSE)	# next rebind of 2,1,2
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
					if( aZoid[4]%in%c( 25, 8 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 28,12 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 45,34 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <12>
					if( fCutU.hasPtn(c( 7,12),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,31,32,22),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(14, 9,19,NA,26),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c(25,28),aZoid) ) cnt<-cnt+1
					# <28>
					if( fCutU.hasPtn(c(25,28),aZoid) ) cnt<-cnt+1
					# <30>
					# <43>
					if( fCutU.hasPtn(c(25,38,26,35,43),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c(31,NA,40,NA,NA,44),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5,2   ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(       ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 9,5,8 ),c( 25, 8 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,2   ),c( 28,12 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,4,9 ),c( 45,34 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(10,11   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3,13   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1,10   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3, 4, 1) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 4   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 4,  3, 18)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+0]==c( 9,  6,  1)) ) cnt<-cnt+1	#10
					if( 1<sum(aCStep[1:3+1]==c( 1, 10,  3)) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+1]==c( 3,  1,  1)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 3,  1,  1)) ) cnt<-cnt+1	# 1

					if( fCutU.hasPtn(c(10, 2),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(11, 4)) ) cnt<-cnt+1

					if( aCStep[3]==sum(aCStep[c(1,4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      1  2 15 19 24 36    | 1 13  4  5 12 |                        |2 2 1 1 0 |2 2 1 1
				#      3  4 16 20 28 44    | 1 12  4  8 16 |  2   2   1   1   4   8 |2 1 2 0 1 |2 1 2 1
				#      5  9 12 30 39 43    | 4  3 18  9  4 |  2   5  -4  10  11  -1 |2 1 0 2 1 |2 1 2 1
				#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  3   3   7  -9  -8  -8 |1 2 1 2 0 |1 2 1 2
				#     17 25 28 37 43 44    | 8  3  9  6  1 |  9  13   9  16  12   9 |0 1 2 1 2 |1 2 1 2
				#     14 15 25 28 29 30(2) | 1 10  3  1  1 | -3 -10  -3  -9 -14 -14 |0 2 3 1 0 |2 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( 10 ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( 13 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -9, -13,  -9 )) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+0]==c( -7,   9,   8 )) ) cnt<-cnt+1 #-10
					if( 1<sum(aFStep[1:3+1]==c( -9, -13,  -9 )) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+2]==c(  9,  16,  12 )) ) cnt<-cnt+1 # -9

					if( all(aFStep[c(3,4)]== aFStep[1]*c(1,3) ) ) cnt<-cnt+1
					if( (aFStep[1]==aFStep[3])&&(aFStep[5]==aFStep[6]) ) cnt<-cnt+1

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,0,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 11 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 33 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 42 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 7>
					if( fCutU.hasPtn(c( 7,NA,12,41),aZoid) ) cnt<-cnt+1
					# <11>
					if( fCutU.hasPtn(c( 4,11,19,41),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(10,15         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   15,NA,NA,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   15,31,44   ),aZoid) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 7,NA,25,31),aZoid) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c( 8, 5,NA, 9,33),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <40>
					if( fCutU.hasPtn(c( 4, 8,13,14,40),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(22,27,24,NA,43),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4,9     ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 1,3,0,5 ),c( 11 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,8     ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3       ),c( 33 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3       ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3,5,2   ),c( 42 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1, 3    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3, 6, 4 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 7       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(17       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(  1,  4,  6 )) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+0]==c(  1,  5, 15 )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 15,  1,  8 )) ) cnt<-cnt+1	# 6

					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13, 2),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,13),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 1, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(16,11)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(16,11)) ) cnt<-cnt+1

					if( sum(aCStep[c(1,4)])==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      8 11 28 30 43 45    | 3 17  2 13  2 |                        |1 1 1 1 2 |1 1 1 1 2
				#      7 20 22 25 38 40    |13  2  3 13  2 | -1   9  -6  -5  -5  -5 |1 0 3 1 1 |1 3 1 1
				#     12 15 19 26 40 43(1) | 3  4  7 14  3 |  5  -5  -3   1   2   3 |0 3 1 0 2 |3 1 2
				#     10 11 15 25 35 41(1) | 1  4 10 10  6 | -2  -4  -4  -1  -5  -2 |0 3 1 1 1 |3 1 1 1
				#      3  4  9 24 25 33(1) | 1  5 15  1  8 | -7  -7  -6  -1 -10  -8 |3 0 2 1 0 |3 2 1
				#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -8 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  4,  1,  5 )) ) cnt<-cnt+1 # 3
					if( 1<sum(aFStep[1:3+0]==c(  4,  4,  1 )) ) cnt<-cnt+1 # 3
					if( 1<sum(aFStep[1:3+1]==c( -1, -5, -2 )) ) cnt<-cnt+1 # 2
					if( 1<sum(aFStep[1:3+2]==c(  3,  3,  2 )) ) cnt<-cnt+1 #-7

					if( aFStep[5]==sum(aFStep[c(1,2,3)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,3)) ) return(FALSE)	# next rebind of 0,1,2
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
					if( aZoid[3]%in%c( 29,30 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <23>
					# <40>
					if( fCutU.hasPtn(c(      27,40),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(30,28,NA,40),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,4   ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 4,0   ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,0   ),c( 29,30 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,6,2 ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3     ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(       ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 6,  1    )) ) cnt<-cnt+1	# 3
					if( 1<sum(aCStep[1:3+3]==c( 9, 13,  3)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+2]==c(13,  3,  2)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+2]==c( 3,  2, 12)) ) cnt<-cnt+1	# 1

					if( fCutU.hasPtn(c( 8, 7 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 2 ),aCStep) ) cnt<-cnt+1

					if( all(aCStep[c(1,4)]== aCStep[2]*c(1,4) ) ) cnt<-cnt+1
					if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
					if( sum(aCStep[c(3,5)])==sum(aCStep[c(2,4)]) ) cnt<-cnt+1
					if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#     16 24 29 40 41 42    | 8  5 11  1  1 |                        |0 1 2 0 3 |1 2 3
					#      1 10 23 26 28 40(1) | 9 13  3  2 12 |-15 -14  -6 -14 -13  -2 |1 1 3 0 1 |1 1 3 1
					#      5  7  9 11 32 35    | 2  2  2 21  3 |  4  -3 -14 -15   4  -5 |3 1 0 2 0 |3 1 2
					#     14 20 23 31 37 38    | 6  3  8  6  1 |  9  13  14  20   5   3 |0 1 2 3 0 |1 2 3
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  6  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 15,  -4,   5 )) ) cnt<-cnt+1 #13
					if( 1<sum(aFStep[1:3+1]==c(-13, -14, -20 )) ) cnt<-cnt+1 #14
					if( 1<sum(aFStep[1:3+2]==c(-20,  -5,  -3 )) ) cnt<-cnt+1 # 5
					if( 1<sum(aFStep[1:2+4]==c( -9, -13      )) ) cnt<-cnt+1 # 3

					if( aFStep[3]==sum(aFStep[c( 1, 5)]) ) cnt<-cnt+1
					if( sum(aFStep[c( 1, 2)])==sum(aFStep[c( 3, 5, 6 )]) ) cnt<-cnt+1

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+2]==c(1,2,3)) ) return(FALSE)	# next rebind of 0,1,1
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 12       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(  9       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 24,25,33 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 34,39    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA, 4,28, 8,22),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 6>
					# <11>
					if( fCutU.hasPtn(c( 4,11,19,41),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <14>
					# <15>
					if( fCutU.hasPtn(c(      15,23      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,NA,15,NA,19,23),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <20>
					if( fCutU.hasPtn(c(   12,20),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,NA,20),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c( 3,24,NA,34),aZoid) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 7,NA,25,31),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(11,16,27),aZoid) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c( 5,NA, 9,33),aZoid) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c( 9, 9, 8,14,24,44),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt) 
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4,2,7 ),c( 12       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 4,1   ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9     ),c(  9       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,5,3 ),c( 24,25,33 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(       ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,4,2 ),c( 34,39    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1, 7 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3,   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(16, 7 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(17    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 1  4  6)) ) cnt<-cnt+1	# 1
					if( 1<sum(aCStep[1:3+0]==c( 2  9  6)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+1]==c( 1  9  3)) ) cnt<-cnt+1	# 6

					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7, 7),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(2,9)) ) cnt<-cnt+1

					if( aCStep[4]==sum(aCStep[c(1,2,5)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,2,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     10 11 15 25 35 41    | 1  4 10 10  6 |                        |0 3 1 1 1 |3 1 1 1
				#      3  5 14 20 42 44    | 2  9  6 22  2 | -7  -6  -1  -5   7   3 |2 1 1 0 2 |2 1 1 2
				#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  9   9   1   4 -15 -12 |0 3 2 1 0 |3 2 1
				#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
				#      3  4  9 24 25 33    | 1  5 15  1  8 | -3  -9 -11  -3  -3  -7 |3 0 2 1 0 |3 2 1
				#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(   ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -3,  -3,  -7 )) ) cnt<-cnt+1 # 3
					if( 1<sum(aFStep[1:3+0]==c(-11,  -3,  -3 )) ) cnt<-cnt+1 # 3
					if( 1<sum(aFStep[1:3+1]==c( -7,   8,  11 )) ) cnt<-cnt+1 #-7
					if( 1<sum(aFStep[1:3+2]==c( -3,  -3,  -7 )) ) cnt<-cnt+1 # 8
					if( 1<sum(aFStep[1:3+3]==c( -3,  -3,  -2 )) ) cnt<-cnt+1 #11

					if( (aFStep[1]==aFStep[2]) && (aFStep[5]== aFStep[3]*4 ) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,2,3)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 26 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 13,19 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 14    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 24,21 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 44    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 8>
					if( fCutU.hasPtn(c( 8,10,17,12,25),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,24,25,38,30),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <14>
					if( fCutU.hasPtn(c(14,NA,18,43),aZoid) ) cnt<-cnt+1
					# <17>
					if( fCutU.hasPtn(c(17,22),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(         21,32   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,16,NA,21,NA,33),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <24>					# <29>					# <31>
					if( fCutU.hasPtn(c(            31,36),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,15,28,13,31   ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <37>
					if( fCutU.hasPtn(c(18,26,27,36,37),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,1     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3,9     ),c( 13,19 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 4       ),c( 14    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,1     ),c( 24,21 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3       ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 6,4,5,1 ),c( 44    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 8, 5 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 5    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 3,  7, 16 )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 1,  5, 15 )) ) cnt<-cnt+1	# 7
					if( 1<sum(aCStep[1:3+1]==c( 1,  6,  1 )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:2+3]==c( 1,  6     )) ) cnt<-cnt+1	#10
					if( 1<sum(aCStep[1:3+2]==c( 3,  7, 16 )) ) cnt<-cnt+1	# 4

					if( fCutU.hasPtn(c( 5, 7),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 5,15)) ) cnt<-cnt+1

					if( (aCStep[4]== 9) && (aCStep[3]==aCStep[5]) ) cnt<-cnt+1
					if( all(aCStep[c(1,5)]== aCStep[3]*c(2,2)) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(1,3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      6  8 14 21 30 37    | 2  6  7  9  7 |                        |2 1 1 2 0 |2 1 1 2
				#      4  9 10 29 31 34    | 5  1 19  2  3 | -2   1  -4   8   1  -3 |2 1 1 2 0 |2 1 1 2
				#      3 13 20 24 33 37    |10  7  4  9  4 | -1   4  10  -5   2   3 |1 1 2 2 0 |1 1 2 2
				#      2 12 14 17 24 40(1) |10  2  3  7 16 | -1  -1  -6  -7  -9   3 |1 3 1 0 1 |1 3 1 1
				#     16 17 23 24 29 44(2) | 1  6  1  5 15 | 14   5   9   7   5   4 |0 2 3 0 1 |2 3 1
				#      8 12 19 21 31 35    | 4  7  2 10  4 | -8  -5  -4  -3   2  -9 |1 2 1 2 0 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(   6  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(   8  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(   5  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  5,  -2,  -3 )) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+0]==c( -6,  -7,  -9 )) ) cnt<-cnt+1 # -5
					if( 1<sum(aFStep[1:3+1]==c(  4,  10,  -5 )) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+1]==c( -5,   2,   3 )) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:3+3]==c( -7,  -9,   3 )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c(  9,   7,   5 )) ) cnt<-cnt+1 # -9

					if( aFStep[6]==sum(aFStep[c(2,3)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(2,4)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,4,5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1
					if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,6)]) ) cnt<-cnt+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 22 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,2,1)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+2]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,0,1
					if( all(quoSize[1:3+0]==c(2,2,2)) ) return(FALSE)	# next rebind of 2,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 12,27 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 13    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 14    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(  9    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 12    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 1>
					if( fCutU.hasPtn(c( 1, 6, 5, 5,32),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(    8,12),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,NA,12),aZoid) ) cnt<-cnt+1
					# <13>					# <28>
					# <33>
					if( fCutU.hasPtn(c(   28,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,NA,33,41),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c( 5,13,36),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,0     ),c(  8    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 6,2,7   ),c( 12,27 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 3       ),c( 13    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,1,7,5 ),c( 14    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,6     ),c(  9    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 2       ),c( 12    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 6    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4, 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 6,  3,  1)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c( 6,  3,  1)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 3,  6,  2)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+0]==c( 2, 16, 17)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 6, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4, 5),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 1,12)) ) cnt<-cnt+1

					if( 1<sum(aCStep[c(1,4,5)]== aCStep[2]*c(2,4,6)) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#     20 30 33 35 36 44    |10  3  2  1  8 |                        |0 0 1 4 1 |1 4 1
					#      1  2 15 19 24 36(1) | 1 13  4  5 12 |-19 -28 -18 -16 -12  -8 |2 2 1 1 0 |2 2 1 1
					#     11 12 29 33 38 42    | 1 17  4  5  4 | 10  10  14  14  14   6 |0 2 1 2 1 |2 1 2 1
					#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
					#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
					#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  1,-14  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -5,  -2,  -1 )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+1]==c( -2,  -5,  -2 )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+2]==c(  1,  -3,  -2 )) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+3]==c(  5,   2,   1 )) ) cnt<-cnt+1 # -3
					if( 1<sum(aFStep[1:2+4]==c(  2,   5      )) ) cnt<-cnt+1 # -2

					if( aFStep[2]==sum(aFStep[c(1,3,4)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(3,5)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(4,5)]) ) cnt<-cnt+1

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
					if( all(quoSize[1:3+0]==c(2,0,3)) ) return(FALSE)	# next rebind of 0,2,2 reverse
					if( all(quoSize[1:3+1]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(1,0,1)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 3, 7 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(27    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(25,31 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,NA,NA,25),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,14,15      ),aZoid) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 7,23,26,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 9>
					if( fCutU.hasPtn(c( 2, 9),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,NA,13),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,NA,23),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 9,NA,13),aZoid) ) cnt<-cnt+1
					# <16>					# <18>
					# <24>
					if( fCutU.hasPtn(c(      24,27   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(20,NA,24,NA,45),aZoid) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 3,NA,NA,NA,25),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14,15,NA,25),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(   14,NA,NA,38),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,7,1 ),c(  3, 7 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 7,3,5 ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,6   ),c( 27    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 2     ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 5,1   ),c( 25,31 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3,5,2 ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6, 1 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 2, 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(14, 1 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(  6,  6,  2 )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c(  6,  6,  2 )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c(  5, 15,  1 )) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c(  6,  6,  2 )) ) cnt<-cnt+1	#  1

					if( all(aCStep[1:2]== aCStep[3]*c(2,2) ) ) cnt<-cnt+1
					if( sum(aCStep[c(1,2,3)])==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7  9 10 13 31 35    | 2  1  3 18  4 |                        |2 2 0 2 0 |2 2 2
				#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
				#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3  -7   6  -9  -5 |3 0 2 1 0 |3 2 1
				#     14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
				#      3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
				#     12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   9  12  13  14  -3 |0 2 2 1 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  -3     ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  -5     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-13, -12     )) ) cnt<-cnt+1 #  9
					if( 1<sum(aFStep[1:3+1]==c(-13, -12     )) ) cnt<-cnt+1 #  9
					if( 1<sum(aFStep[1:3+0]==c( -4, -13,  -2)) ) cnt<-cnt+1 # 12
					if( 1<sum(aFStep[1:3+2]==c( -4, -13,  -2)) ) cnt<-cnt+1 # 13
					if( 1<sum(aFStep[1:2+4]==c( 11,  11     )) ) cnt<-cnt+1 # -3

					if( all(aFStep[1:2]== aFStep[6]*c(-3,-3) ) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,0)) ) return(FALSE)	# next rebind of 3,1,0
					if( all(quoSize[1:3+2]==c(1,1,0)) ) return(FALSE)	# next rebind of 1,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1,17) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 4   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(23,32) ) cnt<-cnt+1
					if( aZoid[6]%in%c(35   ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 1>
					if( fCutU.hasPtn(c( 1, 4),aZoid) ) cnt<-cnt+1
					# < 5>
					# <12>
					if( fCutU.hasPtn(c( 1,10,12),aZoid) ) cnt<-cnt+1
					# <17>
					if( fCutU.hasPtn(c(17,NA,26,27),aZoid) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c( 9,15,24,42),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(11,16,30),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,7,5 ),c(  1,17 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 4,7   ),c(  4    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 4     ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(       ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3,2,6 ),c( 23,32 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5     ),c( 35    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 3, 1, 7, 6 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3          ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4, 1       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1,12       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 5, 12    )) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 4, 30,  1)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+1]==c( 5,  4, 30)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+2]==c( 1, 13   4)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 5,12 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 6 ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 1, 6)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(30, 1)) ) cnt<-cnt+1

					if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      5 12 17 29 34 35    | 7  5 12  5  1 |                        |1 2 1 2 0 |1 2 1 2
				#      5 16 21 23 24 30(1) |11  5  2  1  6 |  0   4   4  -6 -10  -5 |1 1 3 1 0 |1 1 3 1
				#      4  5  6 12 25 37(1) | 1  1  6 13 12 | -1 -11 -15 -11   1   7 |3 1 1 1 0 |3 1 1 1
				#     17 20 30 31 33 45    | 3 10  1  2 12 | 13  15  24  19   8   8 |0 1 1 3 1 |1 1 3 1
				#      1  2 15 19 24 36    | 1 13  4  5 12 |-16 -18 -15 -12  -9  -9 |2 2 1 1 0 |2 2 1 1
				#      1  3  8 12 42 43(1) | 2  5  4 30  1 |  0   1  -7  -7  18   7 |3 1 0 0 2 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(-15      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(-13      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -1, -11, -15)) ) cnt<-cnt+1 # 0
					if( 1<sum(aFStep[1:3+0]==c( 19,   8,   8)) ) cnt<-cnt+1 # 1
					if( 1<sum(aFStep[1:3+0]==c(-19,  -8,  -8)) ) cnt<-cnt+1 #-7
					if( 1<sum(aFStep[1:3+1]==c(-19,  -8,  -8)) ) cnt<-cnt+1 #-7
					if( 1<sum(aFStep[1:3+3]==c(  0,  -1,   7)) ) cnt<-cnt+1 #18
					if( 1<sum(aFStep[1:3+3]==c( 19,   8,   8)) ) cnt<-cnt+1 # 7

					if( all(aFStep[6]== aFStep[3:4]*c(-1,-1) ) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(4,6)]) ) cnt<-cnt+1

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,3,0)) ) return(FALSE)	# next rebind of 0,3,1	reverse
					if( all(quoSize[1:3+2]==c(0,2,0)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 10,20 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 12,19 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 30    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 31,32 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 1>
					if( fCutU.hasPtn(c( 1,NA,NA,16,24,31),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,12),aZoid) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,NA,20      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,NA,NA,NA,30),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,23,NA,27   ),aZoid) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c( 5,NA,NA,NA,30   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,NA,13,NA,NA,28),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,20),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(11,18),aZoid) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(19,34,26),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(19, 5,26,28,32),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <31>
					if( fCutU.hasPtn(c(21,24,25,NA,31),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <32>
					if( fCutU.hasPtn(c(23,16,23,NA,32),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(25,31,NA,36),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1    ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,3  ),c( 10,20 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 2,9  ),c( 12,19 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 0,7  ),c( 30    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 1,2  ),c( 31,32 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 3    ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 6       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 3, 2 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 4    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c(13,  1    )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c( 6,  1, 10)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 3, 13,  6)) ) cnt<-cnt+1	# 10

					if( fCutU.hasPtn(c( 6, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 9, 3)) ) cnt<-cnt+1

					if( all(aCStep[c(1,5)]== aCStep[4]*c(3,4) ) ) cnt<-cnt+1
					if( aCStep[3]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4 10 14 15 18 22    | 6  4  1  3  4 |                        |1 4 1 0 0 |1 4 1
				#      1  2 15 19 24 36(1) | 1 13  4  5 12 | -3  -8   1   4   6  14 |2 2 1 1 0 |2 2 1 1
				#      1 21 26 36 40 41(2) |20  5 10  4  1 |  0  19  11  17  16   5 |1 0 2 1 2 |1 2 1 2
				#      1 10 13 26 32 36(3) | 9  3 13  6  4 |  0 -11 -13 -10  -8  -5 |1 2 1 2 0 |1 2 1 2
				#      3 12 13 18 31 32(2) | 9  1  5 13  1 |  2   2   0  -8  -1  -4 |1 3 0 2 0 |1 3 2
				#     12 18 19 29 31 39(3) | 6  1 10  2  8 |  9   6   6  11   0   7 |0 3 1 2 0 |3 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  6  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 17,  16,   5 )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+1]==c( 17,  16,   5 )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+2]==c(-11, -13, -10 )) ) cnt<-cnt+1 # 11
					if( 1<sum(aFStep[1:3+3]==c(  6,   6,  11 )) ) cnt<-cnt+1 #  0

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
					if( (aZoid[6]-aZoid[1]) %in% c( 22 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(0,2,3)) ) return(FALSE)	# next rebind of 2,1,1 reverse
					if( all(quoSize[1:3+2]==c(1,3,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+0]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 12,11 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 20,36 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 30    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <10>
					if( fCutU.hasPtn(c(10,11,26,43,43),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <20>
					if( fCutU.hasPtn(c(20,32   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(20,NA,35),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(      20,21    ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 5,NA,21,43 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c(33,37      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(33,NA,44,40),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c( 6, 8,16,10,36),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <42>
					if( fCutU.hasPtn(c(21, 7,26,28,42),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4     ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,2,1 ),c( 12,11 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 0     ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 0,3,6 ),c( 20,36 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0,3   ),c( 30    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 4     ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8, 3 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3, 2 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 8    ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 8 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c(10,  4    )) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+0]==c( 8,  2, 15)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+1]==c(11,  3,  8)) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 3, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4, 9),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 2,15 )) ) cnt<-cnt+1

					if( 2<=sum(aCStep[4:5]== aCStep[1]*c(5,3)) ) cnt<-cnt+1
					if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#     20 30 33 35 36 44    |10  3  2  1  8 |                        |0 0 1 4 1 |1 4 1
					#      9 33 36 40 42 43(2) |24  3  4  2  1 |-11   3   3   5   6  -1 |1 0 0 2 3 |1 2 3
					#      5 15 20 31 34 42(1) |10  5 11  3  8 | -4 -18 -16  -9  -8  -1 |1 1 1 2 1 |1 1 1 2 1
					#      6 10 17 18 21 29    | 4  7  1  3  8 |  1  -5  -3 -13 -13 -13 |1 3 2 0 0 |1 3 2
					#      2 10 14 22 32 36(1) | 8  4  8 10  4 | -4   0  -3   4  11   7 |1 2 1 2 0 |1 2 1 2
					#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  6   1   5  -1   4   9 |1 2 1 1 1 |1 2 1 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -4      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  6      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -2      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  4, -3  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -8,  -1     )) ) cnt<-cnt+1 #  6
					if( 1<sum(aFStep[1:3+1]==c( -4,   0,  -3)) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+1]==c(-16,  -9,  -8)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+1]==c(-13, -13, -13)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(  5,  -1,   4)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+3]==c(  5,   3,  13)) ) cnt<-cnt+1 #  9

					if( aFStep[5]==sum(aFStep[c(3,4)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(1,4)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(3,2)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(5,2)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(3,5)]) ) cnt<-cnt+1

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
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 6, 7, 4 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 9       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(21,34,14 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(45,39    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 6>
					if( fCutU.hasPtn(c( 6, 8,NA,NA,27,29),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 7>
					# <12>
					if( fCutU.hasPtn(c( 5,12             ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   12,19,18,43,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c(15,NA,17,25),aZoid) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c( 4,NA,NA,21   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      21,34   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    8, 9,21,44),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c(      21,34),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,15,NA,34),aZoid) ) cnt<-cnt+1
					# <37>
					# <41>
					if( fCutU.hasPtn(c(            41,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            41,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(24,22, 5,30,41   ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <43>
					if( fCutU.hasPtn(c(  3, 8,11,18,NA,43),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c(         35,NA,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            41,44),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(28,31,NA,NA,NA,44),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6,7,4 ),c(  6, 7, 4 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3     ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,5,1 ),c(  9       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1,4   ),c( 21,34,14 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8     ),c(          )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5,9   ),c( 45,39    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4, 3    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 5, 4, 3 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 3,  8, 18 )) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c(18,  4     )) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c( 1,  3,  4 )) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+1]==c( 6,  8,  3 )) ) cnt<-cnt+1	# 18
					if( 1<sum(aCStep[1:3+2]==c( 5,  3,  8 )) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 5, 2),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 5,20 )) ) cnt<-cnt+1

					if( aCStep[3]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7 12 15 24 25 43    | 5  3  9  1 18 |                        |1 2 2 0 1 |1 2 2 1
				#      2  7 27 33 41 44(1) | 5 20  6  8  3 | -5  -5  12   9  16   1 |2 0 1 1 2 |2 1 1 2
				#      6 16 37 38 41 45(1) |10 21  1  3  4 |  4   9  10   5   0   1 |1 1 0 2 2 |1 1 2 2
				#     15 19 21 34 41 44(1) | 4  2 13  7  3 |  9   3 -16  -4   0  -1 |0 2 1 1 2 |2 1 1 2
				#      6 12 17 21 34 37(2) | 6  5  4 13  3 | -9  -7  -4 -13  -7  -7 |1 2 1 2 0 |1 2 1 2
				#      5 10 13 21 39 43(1) | 5  3  8 18  4 | -1  -2  -4   0   5   6 |1 2 1 1 1 |1 2 1 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -2     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -4,-16 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c(  -2,  -4,   0 )) ) cnt<-cnt+1 #  -4
					if( 1<sum(aFStep[1:3+2]==c( -13,  -7,  -7 )) ) cnt<-cnt+1 #   0
					if( 1<sum(aFStep[1:3+3]==c( -16,  -4,   0 )) ) cnt<-cnt+1 #   5

					if( all(aFStep[c(3,6)]== aFStep[2]*c( 2,-3) ) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(2,3,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,3,5)]) ) cnt<-cnt+1

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






