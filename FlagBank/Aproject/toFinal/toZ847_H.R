# toZ847_H.R 최종접근
cntMtx.colName <- c( "raw","rawFV","rem","cStep","fStep"
						,"raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2"
					)

#	centerVal 값이 aCode내에 존재하고, 좌우 값이 있으면 좌우 값 반환.
#		centerVal 값은 aCode내에 다수 존재할 수도 있다.
getSideValues <- function( aCode ,centerVal ){
	# aCode<-1:5	;centerVal<-4
	indices <- which( aCode==centerVal )
	indices <- indices[ !(indices %in% c(1,length(aCode)) ) ]

	rLst <- list()
	for( idx in indices ){
		rLst[[1+length(rLst)]] <- aCode[c(idx-1,idx+1)]
	}

	return( rLst )

} # getSideValues()

# 공용
# undone
fCut.default <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- gEnv$zhF	;zMtxLen <- nrow(zMtx)	# rptObj<-anaQuoTbl( zMtx )
	stdMI <- fCutU.getMtxInfo( zMtx )	;rptObj<-anaMtx( stdMI$rawTail )

	# col[1] 은 1~15
	flag <- gEnv$allZoidMtx[allIdxF,1]<=15	;kIdx<-anaFlagFnd(!flag,rpt)
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

# UNdone - working
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	allIdxF <- rmvRaw( gEnv ,allIdxF )
	allIdxF <- rmvColValSeqNext( gEnv ,allIdxF )
	allIdxF <- rmvColValSeqNext.cStep( gEnv ,allIdxF )

	allIdxF <- rmvCStep( gEnv ,allIdxF )
	allIdxF <- rmvFStep( gEnv ,allIdxF )

	allIdxF <- rmvQuo10( gEnv ,allIdxF )
	allIdxF <- rmvZW( gEnv ,allIdxF )
	allIdxF <- rmvFV3( gEnv ,allIdxF )
	save( allIdxF ,file="Obj_allIdxF.fCut.basic.save")

	# // \\ 재현

	save( allIdxF ,file="Obj_fCut.basic.save" )

	return( allIdxF )

} # fCut.basic()

# UNdone
fCutCnt.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,1,1)) ) return(FALSE)	# next rebind of 1,1,0
					#	unique	1 1 0 1 3 (0 1 1 3 1 이라는 동일 패턴 발생이 근 시기에 있었는 데, 또 나올까?)
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

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
			if( aZoid[2]%in%c( 30          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 31,28,12,42 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 33,30       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,42       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			# <14>
			if( fCutU.hasPtn(c( 14,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 14,22,NA,34 ),aZoid) ) cnt<-cnt+1
			# <30>
			# <33>
			if( fCutU.hasPtn(c( 19,NA,33,35 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 24,16,28,30,29,42 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(       31,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,20,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 7        ),c(             )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,1,4,0  ),c( 30          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2        ),c( 31,28,12,42 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,0      ),c( 33,30       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,4,9    ),c( 33          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,2,3,1  ),c( 45,42       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 11      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(4)   3(5)   4(2)   5(3)   6(2)   12(3)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(6,1)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  9     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  0,  3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#     FV :    -5(2)   0(3)   3(2)   4(2)   6(2)   8(2)   18(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(2,4)==aFStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3,5 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 5
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
					# [1]    2  7  4 18  5  2
					# [2]*  15 15 23 23
					# [3]   27 25 36 28
					# [4]   43 29 25
					# [5]   45 42 36 24 43 45 38 32 44 31 27 36
					# [6]   45 44 39 23 44 31 45 44 35 36 44 45 36 45

					if( 1<sum(aZoid==c(  2,15,27,43,NA,45 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  7,15,25,29,42,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  4,23,36,25,36,39 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 18,23,28,NA,24,23 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  5,NA,NA,NA,43,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  2,NA,NA,NA,38,45 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,32,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,44,45 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,31,36 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,27,44 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,NA,NA,NA,36,45 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c( 15    ) ) score<-score+1
					if( aZoid[2]%in%c( 15,23 ) ) score<-score+1
					if( aZoid[3]%in%c(       ) ) score<-score+1
					if( aZoid[4]%in%c(       ) ) score<-score+1
					if( aZoid[5]%in%c(       ) ) score<-score+1
					if( aZoid[6]%in%c(       ) ) score<-score+1
					if( score>1 ) cnt<-cnt+1

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
					if( aZoid[1]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 28     ) ) cnt<-cnt+1	# 28 35, 28 36 다음 패턴 발생?
					if( aZoid[5]%in%c( 37     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(        ) ) cnt<-cnt+1

					# [  1]  5  9     8 18    33 42    23 40    37 38
					# [  2] 19 26     4  9     8 34    34 44    32 36
					# [  3]           7 10    33 40    30 39    24 30
					# [  4]          33 38             23 24    37 40
					# [  5]          31 38             28 36    39 45
					# [  6]          24 25             38 43    34 39
					# [  7]           7  8             25 28    25 34
					# [  8]                            44 45    31 39
					# [  9]                            37 38    38 41
					# [ 10]                            28 35    28 38
					# [ 11]                                     31 42
					# [ 12]                                     36 37


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(         ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 5       ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 7,0     ),c(    )) )	remCnt <- remCnt+1	# unique 7은 안 나올거 같다.
						if( fCutU.remFilt(aZoid[4],c( 2,5     ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 4,3,8   ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c(         ),c(    )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
						# grp (1:2+3)
							if( aZoid[4]==28   && fCutU.remFilt(aZoid[5],c( 7 ),c( )) ) remCnt <- remCnt+1
							if( aZoid[6]==38   && fCutU.remFilt(aZoid[5],c( 6 ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+4)
					if(remCnt>2) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){

					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c(  4,10, 9,17, 1 ),na.rm=T)
					matCnt <- sum(aCStep==c(  7, 5,26,10, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 3, 7, 9, 6 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 5,NA, 1, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 7,NA, 8, 6 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 1,NA, 5, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA, 1,NA, 3, 9 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 1, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 1, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 7,10 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]   4   7 
					#	[2]  10   5   3   5   7   1   1 
					#	[3]   9  26   7 
					#	[4]  17  10   9   1   8   5   3   1   1   7 
					#	[5]   1   4   6   3   6   5   9   8   3  10  11   1 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1

						if( aCStep[1]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  5, 1  ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  5, 7  ) ) cnt<-cnt+1

						if( aCStep[2]==sum(aCStep[c(3,5)]) )	cnt<-cnt+1
						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 
					score <- score + ifelse(cnt>1,1,0)

					return( score )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt>=2] <- flgCnt[fltCnt>=2] + 1

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
					# [  1]              7  8 34                23 24 30

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

					# [1]*   1  1
					# [2]*   4  3  5  5  8 19 11
					# [3]*  16 27  2 11  6 13  2  2  2
					# [4]*  12  3  2  2  1  3  8 16 13  5  1  1  2  1  7  2  4  3  1 14  2 16  3 10  3  7
					# [5]*   8  1  4  3  9 13  8  3 16 13  7  2 13  3  4  6  1  5 20  7  3  5  5 12  3 13

					tCnt <- 0
						if( aCStep[1]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  5     ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(        ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  4     ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(        ) ) tCnt<-tCnt+1

						if( 1<sum( aCStep[ 2 ]*c(4,3,2)==aCStep[c(3,4,5)] ) )	cnt<-cnt+1
						if( 1<sum( aCStep[c(2,5)]*c(3,2)==aCStep[c(4,3)] ) )	cnt<-cnt+1
						if( sum(aCStep[c( 2,3 )])==sum(aCStep[c( 4,5 )]) )	cnt<-cnt+1	# 20

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					if( fCutU.hasPtn(c( 2, 1 ),aCStep) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 5 ),aCStep) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,NA, 2, 5 ),aCStep) )	cnt<-cnt+1

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1

					if( is.null(valMtx) ){
						# matCnt 작업을 루프문으로 바꾼 것.
						# 	matCnt<-sum(aCStep==c( 5, 6, 5, 8, 9 ),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
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
					# [  1] 12  8     1 26     1  2     1  7
					# [  2]  4  2              4  3     5  2
					# [  3]  9  1              1  7     4  6
					# [  4]                    1 14     2  9
					# [  5]                    5  7     2 20
					# [  6]                    9 14     6  9
					# [  7]                    3  1     2  8
					# [  8]                    1 10     1  6
					# [  9]                    2  9     2  9
					# [ 10]                             3 14
					# [ 11]                             5  7

					if( aCStep[1]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  1         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  1, 3      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  2, 6      ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  1, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  4,11 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  5, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  5, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  5, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  4, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  3, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c(  1, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( 11, 9 )) ) cnt<-cnt+1

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
					if( aCStep[1]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(            ) ) cnt<-cnt+1

					# [  1]              1  4  3     6  7  1
					return( cnt<1 )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )

} # fCutCnt.colValSeqNext.cStep()

# UNdone
fCutCnt.nextZW <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )	
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 34, 29 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,2,3
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

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
			if( aZoid[1]%in%c(  2, 7,16 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 22       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  9       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 33       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 36       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  2:2   7:2   14:2   22:3   25:2   29:2   31:2   32:2   36:3   39:2
			# <  2>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#                2 , 10 , 14 , 22 , 32 , 36 
			#          -->   2*, 13 ,  9 , 19 , 35 , 36!
			# <  7>   1      7 , 22 , 24 , 31 , 34 
			#                7 , 19 , 25 , 29 , 36 
			#          -->   7*, 16 , 26!, 27 , 38 
			# < 14>  -2     14 , 22 , 32 , 36 
			#               14 , 15 , 25 , 28 
			#          -->  14*, NA , 18 , 20 
			# < 22>   2      5 , 22 , 31 , 32 
			#               14 , 22 , 32 , 36 
			#          -->  NA , 22*, 33!, 40 
			# < 25>  -1      7 , 19 , 25 , 29 , 36 
			#               14 , 15 , 25 , 28 , 29 
			#          -->  21 , 11 , 25*, 27!, NA 
			# < 29>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#               14 , 15 , 25 , 28 , 29 , 30 
			#          -->  26 , 23 , NA , NA , 29*, NA 
			# < 31>  -1     22 , 24 , 31 , 34 , 36 
			#                5 , 22 , 31 , 32 , 39 
			#          -->  NA , 20 , 31*, NA , 42 
			# < 32>   1      5 , 22 , 31 , 32 , 39 
			#               10 , 14 , 22 , 32 , 36 
			#          -->  15 ,  6 , 13 , 32*, 33 
			# < 36>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#                2 , 10 , 14 , 22 , 32 , 36 
			#          -->   2!, 13 ,  9 , 19 , 35 , 36*
			# < 39>   0      1 ,  9 , 12 , 23 , 39 , 43 
			#                5 , 22 , 31 , 32 , 39 , 45 
			#          -->   9 , 35 , NA , NA , 39*, NA 
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,7,6    ),c(  2, 7,16 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2        ),c( 22       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6        ),c(  9       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,8      ),c( 33       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9        ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 6,4      ),c( 36       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#       tgt.col banVal                     descript tgt.dir
			#      1       1      9 [desc1   ]  9(?),xx, 8,xx, 7  Slide/
			#      2       4      2       [desc1   ]  2(?), 3, 4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(2)   3(3)   4(4)   6(2)   7(3)   8(3)   10(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4,5 )]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 22 24 31 34 36    |15  2  7  3  2 |                        |1 0 2 3 0 |1 2 3
			#      1  9 12 23 39 43    | 8  3 11 16  4 | -6 -13 -12  -8   5   7 |2 1 1 1 1 |2 1 1 1 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 |  1  -2   7   2 -10  -7 |2 1 2 1 0 |2 1 2 1
			#      5 22 31 32 39 45    |17  9  1  7  6 |  3  15  12   7  10   9 |1 0 1 3 1 |1 1 3 1
			#      2 10 14 22 32 36(2) | 8  4  8 10  4 | -3 -12 -17 -10  -7  -9 |1 2 1 2 0 |1 2 1 2
			#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 12   5  11   6  -3  -6 |0 2 3 1 0 |2 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -12(2)   -10(2)   -7(2)   -6(2)   -3(2)   5(2)   7(3)   12(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 5 ]*c(-4,-2, 2)==aFStep[c(1,4,6)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 6 ]*c(-2,-1)==aFStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextZW()

# UNdone
fCutCnt.nextQuo10 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1 13 20 22 25 28    |12  7  2  3  3 |                        |1 1 4 0 0 |1 1 4
			#      4  8 18 19 39 44    | 4 10  1 20  5 |  3  -5  -2  -3  14  16 |2 2 0 1 1 |2 2 1 1
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -3  -1  -2  -1  -5  -6 |2 2 0 2 0 |2 2 2
			#      1 28 35 41 43 44(1) |27  7  6  2  1 |  0  21  19  23   9   6 |1 0 1 1 3 |1 1 1 3
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 | 11 -14 -14 -11  -4  -1 |0 2 1 2 1 |2 1 2 1
			#     13 16 24 25 33 36    | 3  8  1  8  3 |  1   2   3  -5  -6  -7 |0 2 2 2 0 |2 2 2
			#   dup number  1:3   13:2   16:2   18:2   25:2   28:2   39:2   43:2   44:2
			#   zoid width  ... 27   40   37   43   31   23 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                               descript tgt.dir
			#  721       1      1     [same    ]  1(?), ., ., 1, ., ., 1     col
			#  826       1     14                 [desc1   ] 14(?),13,12     col
			#  722       1     12 [seqReb  ] 12(?), .,12, ., 1, ., 1,...     col

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  1>   0      1 ,  7 , 16 , 18 , 34 , 38 
			#                1 , 28 , 35 , 41 , 43 , 44 
			#          -->   1*, NA , NA , NA , NA , NA 
			# < 13>  -1     13 , 20 , 22 , 25 , 28 
			#               13 , 16 , 24 , 25 , 33 
			#          -->  13*, NA , 26 , 25!, 38 
			# < 16>  -1      7 , 16 , 18 , 34 , 38 
			#               13 , 16 , 24 , 25 , 33 
			#          -->  NA , 16*, 30 , NA , 28 
			# < 18>   1      4 ,  8 , 18 , 19 , 39 
			#                7 , 16 , 18 , 34 , 38 
			#          -->  10 , NA , 18*, NA , 37!
			# < 25>  -1     13 , 20 , 22 , 25 , 28 
			#               13 , 16 , 24 , 25 , 33 
			#          -->  13!, 12 , NA , 25*, 38 
			# < 28>  -4     25 , 28 
			#                1 , 28 
			#          -->  NA , 28*
			# < 39>   0      4 ,  8 , 18 , 19 , 39 , 44 
			#               12 , 14 , 21 , 30 , 39 , 43 
			#          -->  20 , 20 , 24 , NA , 39*, 42!
			# < 43>   1      1 , 28 , 35 , 41 , 43 
			#               14 , 21 , 30 , 39 , 43 
			#          -->  27 , 14 , 25 , 37 , 43*
			# < 44>   0      4 ,  8 , 18 , 19 , 39 , 44 
			#                1 , 28 , 35 , 41 , 43 , 44 
			#          -->  NA , NA , NA , NA , NA , 44*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 721        1      1     [same    ]  1(?), ., ., 1, ., ., 1     col
			# 826        1      4              [desc1   ]  4(?), 3, 2, 1     col
			# 722        1      3           [desc1   ]  3(?),xx, 2,xx, 1     col
			# 7221       1      2 [seqReb  ]  2(?), ., 2, ., 1, ., 1,...     col
			# 7211       4      0     [desc1   ]  0(?),xx,xx, 1,xx,xx, 2     col
			# 696        5      4           [symm    ]  4(?), 3, 9, 3, 4     col
			# 7222       6      3 [seqReb  ]  3(?), ., 3, ., 8, ., 8,...     col
			# 1          1      6          [seqReb  ]  6(?), 6, 1, 1,...  Slide/
			# 11         4      3                 [same    ]  3(?), 3, 3  Slide/
			# 12         4      4                 [same    ]  4(?), 4, 4 Slide\\
			# 13         4      1              [sameEnd ]  1(?), 4, 4, 1 Slide\\
			# 14         5      1           [same    ]  1(?), ., 1, ., 1 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                               descript tgt.dir
			#      826        1      4                 [desc1   ]  4(?), 3, 2     col
			#      721        2      7     [same    ]  7(?), ., ., 7, ., ., 7     col
			#      8261       2      9                 [desc1   ]  9(?), 8, 7     col
			#      8262       2      8          [seqReb  ]  8(?), 8, 7, 7,...     col
			#      722        3      9 [seqReb  ]  9(?), ., 9, ., 2, ., 2,...     col
			#      8263       4      7                 [desc1   ]  7(?), 8, 9     col
			#      7211       4      1     [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			#      7221       5      4           [same    ]  4(?), ., 4, ., 4     col
			#      394        5      3     [sameEnd ]  3(?),xx, 4,xx, 4,xx, 3     col
			#      8264       5      2                 [desc1   ]  2(?), 3, 4     col
			#      1          1      7                 [desc1   ]  7(?), 8, 9  Slide/
			#      11         5      7                 [desc1   ]  7(?), 8, 9 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(4)   3(4)   4(3)   6(2)   7(3)   8(2)   9(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  1 13 20 22 25 28    |12  7  2  3  3 |                        |1 1 4 0 0 |1 1 4
			#  4  8 18 19 39 44    | 4 10  1 20  5 |  3  -5  -2  -3  14  16 |2 2 0 1 1 |2 2 1 1
			#  1  7 16 18 34 38(1) | 6  9  2 16  4 | -3  -1  -2  -1  -5  -6 |2 2 0 2 0 |2 2 2
			#  1 28 35 41 43 44(1) |27  7  6  2  1 |  0  21  19  23   9   6 |1 0 1 1 3 |1 1 1 3
			# 12 14 21 30 39 43(1) | 2  7  9  9  4 | 11 -14 -14 -11  -4  -1 |0 2 1 2 1 |2 1 2 1
			# 13 16 24 25 33 36    | 3  8  1  8  3 |  1   2   3  -5  -6  -7 |0 2 2 2 0 |2 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      1        5     -3 [desc1   ] -3(?),xx,-4,xx,-5     col
			#      E4       3     -6       [desc1   ] -6(?),-5,-4  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -14(2)   -6(2)   -5(3)   -3(2)   -2(2)   -1(3)   3(2) 
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# -4
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# -5
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# -3
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# -4
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# -4
			#	unique	: 1,2,3... -5,-6,-7 처럼 간격이 동일한 패턴이 과연 반복될 수 있을까?

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextQuo10()

# UNdone
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 18 21 26 38 43    | 7  3  5 12  5 |                        |0 2 2 1 1 |2 2 1 1
			#      1  8 11 15 18 45(2) | 7  3  4  3 27 |-10 -10 -10 -11 -20   2 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2
			#   dup number  5:2   11:2   15:3   18:3   19:2   26:2   34:2   43:2
			#   zoid width  ... 32   44   21   34   37   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                      descript tgt.dir
			#  832        2     13        [desc1   ] 13(?),14,15     col
			#  8321       3     18        [desc1   ] 18(?),19,20     col
			#  8322       5     40 [seqReb  ] 40(?),40,34,34,...     col
			#  8323       6     44        [desc1   ] 44(?),43,42     col
			#  1          1     21  [desc1   ] 21(?),xx,20,xx,19  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  5>  -1      5 , 15 , 18 , 19 , 23 
			#                5 , 15 , 20 , 31 , 34 
			#          -->   5*, 15!, 22 , 43 , 45 
			# < 11>   2     11 , 18 , 21 , 26 
			#               11 , 15 , 18 , 45 
			#          -->  11*, 12 , 15 , NA 
			# < 15>  -1      5 , 15 , 18 , 19 , 23 
			#                5 , 15 , 20 , 31 , 34 
			#          -->   5!, 15*, 22 , 43 , 45 
			# < 18>  -1      8 , 11 , 15 , 18 , 45 
			#                2 ,  5 , 15 , 18 , 19 
			#          -->  NA , NA , 15!, 18*, NA 
			# < 19>  -2     15 , 18 , 19 , 23 
			#               13 , 14 , 19 , 26 
			#          -->  11 , 10 , 19*, 29 
			# < 26>   0     11 , 18 , 21 , 26 , 38 , 43 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  15 , 10 , 17 , 26*, 42 , 43!
			# < 34>   0      3 ,  6 , 10 , 30 , 34 , 37 
			#                5 , 15 , 20 , 31 , 34 , 42 
			#          -->   7 , 24 , 30 , 32!, 34*, NA 
			# < 43>   0     11 , 18 , 21 , 26 , 38 , 43 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  15 , 10 , 17 , 26!, 42 , 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 713        1      2           [symm    ]  2(?), 3, 5, 3, 2     col
			# 767        2      5           [same    ]  5(?), ., 5, ., 5     col
			# 571        2      8     [sameEnd ]  8(?),xx, 5,xx, 5,xx, 8     col
			# 832        2      3              [desc1   ]  3(?), 4, 5, 6     col
			# 728        3     -1     [desc1   ] -1(?),xx,xx, 0,xx,xx, 1     col
			# 8321       3      9          [seqReb  ]  9(?), 9, 0, 0,...     col
			# 8322       5      0          [seqReb  ]  0(?), 0, 4, 4,...     col
			# 8323       6      4                 [desc1   ]  4(?), 3, 2     col
			# 7671       6      1           [desc1   ]  1(?),xx, 2,xx, 3     col
			# 7672       6      2 [seqReb  ]  2(?), ., 2, ., 3, ., 3,...     col
			# 1          1      4          [seqReb  ]  4(?), 4, 0, 0,...  Slide/
			# 11         3      3                 [desc1   ]  3(?), 4, 5 Slide\\
			# 12         5      2           [symm    ]  2(?), 6, 0, 6, 2 Slide\\
			# 13         6     -1                 [desc1   ] -1(?), 0, 1 Slide\\
			# 14         6      5           [symm    ]  5(?), 0, 1, 0, 5 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                           descript tgt.dir
			#      832        2      5             [same    ]  5(?), 5, 5     col
			#      728        2      4          [sameEnd ]  4(?), 5, 5, 4     col
			#      7281       2      5 [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			#      713        5      4       [symm    ]  4(?), 3, 8, 3, 4     col
			#      1          2      7      [seqReb  ]  7(?), 7, 3, 3,...  Slide/
			#      11         4      9         [desc(-2) ]  9(?), 7, 5, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   3(9)   4(4)   5(4)   7(3)   10(2) 
			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 18 21 26 38 43    | 7  3  5 12  5 |                        |0 2 2 1 1 |2 2 1 1
			#      1  8 11 15 18 45(2) | 7  3  4  3 27 |-10 -10 -10 -11 -20   2 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        1      3 [desc1   ]  3(?),xx, 2,xx, 1     col
			#      2        5     -1 [desc1   ] -1(?),xx, 0,xx, 1     col
			#      E5       4      7       [desc1   ]  7(?), 6, 5  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -10(3)   -5(2)   -1(2)   1(6)   2(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1	# 7
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1	# 7

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextBin()

# UNdone
fCutCnt.nextRebNum <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     13 18 26 31 34 44    | 5  8  5  3 10 |                        |0 2 1 2 1 |2 1 2 1
			#      6  7 10 16 38 41    | 1  3  6 22  3 | -7 -11 -16 -15   4  -3 |2 2 0 1 1 |2 2 1 1
			#      9 10 13 24 33 38(2) | 1  3 11  9  5 |  3   3   3   8  -5  -3 |1 2 1 2 0 |1 2 1 2
			#     15 24 31 32 33 40(2) | 9  7  1  1  7 |  6  14  18   8   0   2 |0 1 1 3 1 |1 1 3 1
			#      1 11 21 23 34 44    |10 10  2 11 10 |-14 -13 -10  -9   1   4 |1 1 2 1 1 |1 1 2 1 1
			#      3 10 16 19 31 39    | 7  6  3 12  8 |  2  -1  -5  -4  -3  -5 |1 3 0 2 0 |1 3 2
			#   dup number  10:3   13:2   16:2   24:2   31:3   33:2   34:2   38:2   44:2
			#   zoid width  ... 31   35   29   25   43   36 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  831        2      9             [desc1   ]  9(?),10,11     col
			#  726        2     12       [desc1   ] 12(?),xx,11,xx,10     col
			#  7261       4     22       [desc1   ] 22(?),xx,23,xx,24     col
			#  643        4     33 [desc1   ] 33(?),xx,xx,32,xx,xx,31     col
			#  7262       5     35       [desc1   ] 35(?),xx,34,xx,33     col
			#  6431       5     32 [desc1   ] 32(?),xx,xx,33,xx,xx,34     col
			#  1          1     -1         [desc(11) ] -1(?),10,21,32  Slide/
			#  11         6     10       [symm    ] 10(?),31,23,31,10 Slide\\

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# < 10>   0      9 , 10 , 13 , 24 , 33 , 38 
			#                3 , 10 , 16 , 19 , 31 , 39 
			#          -->  NA , 10*, 19 , 14 , 29 , 40!
			# < 13>   2     13 , 18 , 26 , 31 
			#               13 , 24 , 33 , 38 
			#          -->  13*, 30 , 40 , 45 
			# < 16>  -1      7 , 10 , 16 , 38 , 41 
			#                3 , 10 , 16 , 19 , 31 
			#          -->  NA , 10!, 16*, NA , 21 
			# < 24>  -2     13 , 24 , 33 , 38 
			#               15 , 24 , 31 , 32 
			#          -->  17 , 24*, 29 , 26 
			# < 31>   2     15 , 24 , 31 , 32 
			#               16 , 19 , 31 , 39 
			#          -->  17!, 14 , 31*, NA 
			# < 33>   0      9 , 10 , 13 , 24 , 33 , 38 
			#               15 , 24 , 31 , 32 , 33 , 40 
			#          -->  21 , NA , NA , NA , 33*, 42 
			# < 34>   0     13 , 18 , 26 , 31 , 34 , 44 
			#                1 , 11 , 21 , 23 , 34 , 44 
			#          -->  NA ,  4 , 16 , 15 , 34*, 44!
			# < 38>   1      6 ,  7 , 10 , 16 , 38 
			#               10 , 13 , 24 , 33 , 38 
			#          -->  14 , 19 , NA , NA , 38*
			# < 44>   0     13 , 18 , 26 , 31 , 34 , 44 
			#                1 , 11 , 21 , 23 , 34 , 44 
			#          -->  NA ,  4 , 16 , 15 , 34!, 44*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 831        2     -1             [desc1   ] -1(?), 0, 1     col
			# 726        2      2       [desc1   ]  2(?),xx, 1,xx, 0     col
			# 8311       3      6      [seqReb  ]  6(?), 6, 1, 1,...     col
			# 7261       4      2       [desc1   ]  2(?),xx, 3,xx, 4     col
			# 643        4      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			# 7262       5      5       [desc1   ]  5(?),xx, 4,xx, 3     col
			# 6431       5      2 [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			# 1          1     -1       [desc1   ] -1(?), 0, 1, 2, 3  Slide/
			# 11         2      6      [seqReb  ]  6(?), 6, 3, 3,...  Slide/
			# 12         3     -1             [desc1   ] -1(?), 0, 1 Slide\\
			# 13         6      0       [symm    ]  0(?), 1, 3, 1, 0 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                           descript tgt.dir
			#      643        2      6 [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			#      831        3      4          [desc1   ]  4(?), 3, 2, 1     col
			#      8311       4     13             [desc1   ] 13(?),12,11     col
			#      1          5      3       [desc1   ]  3(?),xx, 2,xx, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   3(5)   5(3)   6(2)   7(3)   8(2)   9(2)   10(4)   11(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			# 13 18 26 31 34 44    | 5  8  5  3 10 |                        |0 2 1 2 1 |2 1 2 1
			#  6  7 10 16 38 41    | 1  3  6 22  3 | -7 -11 -16 -15   4  -3 |2 2 0 1 1 |2 2 1 1
			#  9 10 13 24 33 38(2) | 1  3 11  9  5 |  3   3   3   8  -5  -3 |1 2 1 2 0 |1 2 1 2
			# 15 24 31 32 33 40(2) | 9  7  1  1  7 |  6  14  18   8   0   2 |0 1 1 3 1 |1 1 3 1
			#  1 11 21 23 34 44    |10 10  2 11 10 |-14 -13 -10  -9   1   4 |1 1 2 1 1 |1 1 2 1 1
			#  3 10 16 19 31 39    | 7  6  3 12  8 |  2  -1  -5  -4  -3  -5 |1 3 0 2 0 |1 3 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -5(3)   -3(3)   2(2)   3(3)   4(2)   8(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c(-2, 1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,1)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3,6)])==sum(aFStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1	# -8
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1	# -9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextRebNum()

# UNdone
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 18 19 27 29 42    |11  1  8  2 13 |                        |1 2 2 0 1 |1 2 2 1
			#      1  8 11 15 18 45(1) | 7  3  4  3 27 | -6 -10  -8 -12 -11   3 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2
			#   dup number  5:2   15:3   18:3   19:3   34:2   42:2
			#   zoid width  ... 35   44   21   34   37   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                      descript tgt.dir
			#  832        2     13        [desc1   ] 13(?),14,15     col
			#  8321       3     18        [desc1   ] 18(?),19,20     col
			#  8322       5     40 [seqReb  ] 40(?),40,34,34,...     col
			#  8323       6     44        [desc1   ] 44(?),43,42     col
			#  1          1     21  [desc1   ] 21(?),xx,20,xx,19  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			# <  5>  -1      5 , 15 , 18 , 19 , 23 
			#                5 , 15 , 20 , 31 , 34 
			#          -->   5*, 15!, 22 , 43 , 45 
			# < 15>  -1      5 , 15 , 18 , 19 , 23 
			#                5 , 15 , 20 , 31 , 34 
			#          -->   5!, 15*, 22 , 43 , 45 
			# < 18>  -1      8 , 11 , 15 , 18 , 45 
			#                2 ,  5 , 15 , 18 , 19 
			#          -->  NA , NA , 15!, 18*, NA 
			# < 19>  -2     15 , 18 , 19 , 23 
			#               13 , 14 , 19 , 26 
			#          -->  11 , 10 , 19*, 29 
			# < 34>   0      3 ,  6 , 10 , 30 , 34 , 37 
			#                5 , 15 , 20 , 31 , 34 , 42 
			#          -->   7 , 24 , 30 , 32!, 34*, NA 
			# < 42>   0      7 , 18 , 19 , 27 , 29 , 42 
			#                5 , 15 , 20 , 31 , 34 , 42 
			#          -->   3 , 12 , 21!, 35 , 39 , 42*
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#         tgt.col banVal                      descript tgt.dir
			#      832       2      5        [same    ]  5(?), 5, 5     col
			#      728       2      4     [sameEnd ]  4(?), 5, 5, 4     col
			#      713       5      4  [symm    ]  4(?), 3, 8, 3, 4     col
			#      1         2      7 [seqReb  ]  7(?), 7, 3, 3,...  Slide/
			#      11        4      9    [desc(-2) ]  9(?), 7, 5, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   3(8)   4(4)   5(2)   7(2)   8(2)   10(2)   11(2) 
			cnt.w2 <- 0
			if( sum(aCStep[c(1,3)])==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# 8
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 18 19 27 29 42    |11  1  8  2 13 |                        |1 2 2 0 1 |1 2 2 1
			#      1  8 11 15 18 45(1) | 7  3  4  3 27 | -6 -10  -8 -12 -11   3 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      1        1      3 [desc1   ]  3(?),xx, 2,xx, 1     col
			#      2        5     -1 [desc1   ] -1(?),xx, 0,xx, 1     col
			#      E5       4      7       [desc1   ]  7(?), 6, 5  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -5(2)   -1(2)   1(6)   3(2)
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1	# 7
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1	# 7

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextCStepBin()

# UNdone
fCutCnt.nextFStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  5 20 34 35 44    | 2 15 14  1  9 |                        |2 0 1 2 1 |2 1 2 1
			#      8 13 20 22 23 36(1) | 5  7  2  1 13 |  5   8   0 -12 -12  -8 |1 1 3 1 0 |1 1 3 1
			#     12 15 19 26 40 43    | 3  4  7 14  3 |  4   2  -1   4  17   7 |0 3 1 0 2 |3 1 2
			#     11 18 21 26 38 43(2) | 7  3  5 12  5 | -1   3   2   0  -2   0 |0 2 2 1 1 |2 2 1 1
			#      5 12 14 32 34 42    | 7  2 18  2  8 | -6  -6  -7   6  -4  -1 |1 2 0 2 1 |1 2 2 1
			#      1  8 17 34 39 45(1) | 7  9 17  5  6 | -4  -4   3   2   5   3 |2 1 0 2 1 |2 1 2 1
			#   dup number  5:2   8:2   12:2   20:2   26:2   34:3   43:2
			#   zoid width  ... 41   28   31   32   37   44 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  571        3     22 [desc1   ] 22(?),xx,xx,21,xx,xx,20     col
			#  578        6     41 [desc1   ] 41(?),xx,42,xx,43,xx,44     col
			#  5711       6     42 [desc1   ] 42(?),xx,xx,43,xx,xx,44     col
			#  1          3     34             [same    ] 34(?),34,34  Slide/
			#  11         3     43          [sameEnd ] 43(?),34,34,43  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  5>  -1      5 , 20 , 34 , 35 , 44 
			#                5 , 12 , 14 , 32 , 34 
			#          -->   5*, NA , NA , 29 , 24 
			# <  8>   1      8 , 13 , 20 , 22 , 23 
			#                8 , 17 , 34 , 39 , 45 
			#          -->   8*, 21 , NA , NA , NA 
			# < 12>   1     12 , 15 , 19 , 26 , 40 
			#               12 , 14 , 32 , 34 , 42 
			#          -->  12*, 13!, 45 , 42 , 44 
			# < 20>   0      3 ,  5 , 20 , 34 , 35 , 44 
			#                8 , 13 , 20 , 22 , 23 , 36 
			#          -->  13 , NA , 20*, NA , NA , 28 
			# < 26>   0     12 , 15 , 19 , 26 , 40 , 43 
			#               11 , 18 , 21 , 26 , 38 , 43 
			#          -->  10!, 21 , 23 , 26*, 36 , 43!
			# < 34>  -1     12 , 14 , 32 , 34 , 42 
			#                1 ,  8 , 17 , 34 , 39 
			#          -->  NA ,  2 ,  2 , 34*, 36 
			# < 43>   0     12 , 15 , 19 , 26 , 40 , 43 
			#               11 , 18 , 21 , 26 , 38 , 43 
			#          -->  10!, 21 , 23 , 26!, 36 , 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 558        1      2           [symm    ]  2(?), 1, 5, 1, 2     col
			# 578        2      2 [seqReb  ]  2(?), ., 2, ., 5, ., 5,...     col
			# 5581       2      5           [symm    ]  5(?), 8, 2, 8, 5     col
			# 571        3      2     [desc1   ]  2(?),xx,xx, 1,xx,xx, 0     col
			# 676        3     10             [desc(-3) ] 10(?), 7, 4, 1     col
			# 5781       6      1     [desc1   ]  1(?),xx, 2,xx, 3,xx, 4     col
			# 5711       6      2     [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			# 1          2      1           [desc1   ]  1(?),xx, 2,xx, 3  Slide/
			# 11         3      4                 [same    ]  4(?), 4, 4  Slide/
			# 12         3      3              [sameEnd ]  3(?), 4, 4, 3  Slide/
			# 13         5      4                 [same    ]  4(?), 4, 4 Slide\\
			# 14         5      8              [sameEnd ]  8(?), 4, 4, 8 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                      descript tgt.dir
			#      676        1      7     [same    ]  7(?), 7, 7, 7     col
			#      558        1      3  [sameEnd ]  3(?), 7, 7, 7, 3     col
			#      6761       1      7 [seqReb  ]  7(?), 7, 7, 7,...     col
			#      6762       3     16        [desc1   ] 16(?),17,18     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(4)   3(3)   5(4)   7(5)   9(2)   14(2) 
			cnt.w2 <- 0
			if( sum(aCStep[c( 3,4 )])==sum(aCStep[c( 2,3,5 )]) )	cnt.w2<-cnt.w2+1	# 22

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  3  5 20 34 35 44    | 2 15 14  1  9 |                        |2 0 1 2 1 |2 1 2 1
			#  8 13 20 22 23 36(1) | 5  7  2  1 13 |  5   8   0 -12 -12  -8 |1 1 3 1 0 |1 1 3 1
			# 12 15 19 26 40 43    | 3  4  7 14  3 |  4   2  -1   4  17   7 |0 3 1 0 2 |3 1 2
			# 11 18 21 26 38 43(2) | 7  3  5 12  5 | -1   3   2   0  -2   0 |0 2 2 1 1 |2 2 1 1
			#  5 12 14 32 34 42    | 7  2 18  2  8 | -6  -6  -7   6  -4  -1 |1 2 0 2 1 |1 2 2 1
			#  1  8 17 34 39 45(1) | 7  9 17  5  6 | -4  -4   3   2   5   3 |2 1 0 2 1 |2 1 2 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      E4       2      5 [desc1   ]  5(?),xx, 6,xx, 7  Slide/
			#      E5       6      4       [desc1   ]  4(?), 5, 6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -12(2)   -6(2)   -4(3)   -1(3)   0(3)   2(3)   3(3)   4(2)   5(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 4 ]*c(-2,-2)==aFStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,3)]*c(1,1)==aFStep[c(2,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(6,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

}	# fCutCnt.nextFStepBin( )

# UNdone
fCutCnt.nextColVal_1 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 10 14 22 32 36    | 8  4  8 10  4 |                        |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -1   0  -1   4   0   0 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2
			#     14 26 32 36 39 42(1) |12  6  4  3  3 | 11  16  16  17   8   3 |0 1 1 3 1 |1 1 3 1
			#   dup number  10:3   13:2   14:2   19:2   26:2   31:2   32:3   36:4   39:3
			#   zoid width  ... 34   35   37   35   36   28 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  831        2      9       [desc1   ]  9(?),xx,10,xx,11     col
			#  828        3     12 [desc1   ] 12(?),xx,xx,13,xx,xx,14     col
			#  8281       5     30 [desc1   ] 30(?),xx,xx,31,xx,xx,32     col
			#  842        5     39      [seqReb  ] 39(?),39,31,31,...     col
			#  8421       6     42      [seqReb  ] 42(?),42,39,39,...     col
			#  1          4     39             [same    ] 39(?),39,39  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# < 10>   0      1 , 10 , 13 , 26 , 32 , 36 
			#                3 , 10 , 16 , 19 , 31 , 39 
			#          -->   5 , 10*, 19 , 12 , 30!, 42 
			# < 13>   0      1 , 10 , 13 , 26 , 32 , 36 
			#                4 ,  7 , 13 , 29 , 31 , 39 
			#          -->   7 ,  4 , 13*, 32 , 30!, 42 
			# < 14>  -2     14 , 22 , 32 , 36 
			#               14 , 26 , 32 , 36 
			#          -->  14*, 30 , 32!, 36!
			# < 19>   1      8 , 11 , 19 , 21 , 36 
			#               10 , 16 , 19 , 31 , 39 
			#          -->  12 , NA , 19*, 41 , 42 
			# < 26>  -2     13 , 26 , 32 , 36 
			#               14 , 26 , 32 , 36 
			#          -->  15!, 26*, 32!, 36!
			# < 31>   0      4 ,  7 , 13 , 29 , 31 , 39 
			#                3 , 10 , 16 , 19 , 31 , 39 
			#          -->   2!, 13 , 19 ,  9 , 31*, 39!
			# < 32>  -2     13 , 26 , 32 , 36 
			#               14 , 26 , 32 , 36 
			#          -->  15!, 26!, 32*, 36!
			# < 36>  -1     11 , 19 , 21 , 36 , 45 
			#               14 , 26 , 32 , 36 , 39 
			#          -->  17 , 33 , NA , 36*, NA 
			# < 39>  -1     10 , 16 , 19 , 31 , 39 
			#               14 , 26 , 32 , 36 , 39 
			#          -->  18 , 36 , NA , NA , 39*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 842        1      5             [desc1   ]  5(?), 4, 3     col
			# 811        1      8       [symm    ]  8(?), 4, 3, 4, 8     col
			# 831        2     -1       [desc1   ] -1(?),xx, 0,xx, 1     col
			# 828        3      2 [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			# 8421       4      6      [seqReb  ]  6(?), 6, 9, 9,...     col
			# 8281       5      0 [desc1   ]  0(?),xx,xx, 1,xx,xx, 2     col
			# 8422       5      9      [seqReb  ]  9(?), 9, 1, 1,...     col
			# 8423       6      2      [seqReb  ]  2(?), 2, 9, 9,...     col
			# 1          1      6             [same    ]  6(?), 6, 6  Slide/
			# 11         1      9          [sameEnd ]  9(?), 6, 6, 9  Slide/
			# 12         1      6       [same    ]  6(?), ., 6, ., 6  Slide/
			# 13         1      9   [ptnReb   ]  9(?), 6, 6, 9, 6, 6  Slide/
			# 14         4      9             [same    ]  9(?), 9, 9  Slide/
			# 15         5      6             [same    ]  6(?), 6, 6 Slide\\
			# 16         5      7          [sameEnd ]  7(?), 6, 6, 7 Slide\\
			# 17         6      9             [same    ]  9(?), 9, 9 Slide\\
			# 18         6      3          [sameEnd ]  3(?), 9, 9, 3 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                      descript tgt.dir
			#      842        2      6     [same    ]  6(?), 6, 6, 6     col
			#      811        2      8  [sameEnd ]  8(?), 6, 6, 6, 8     col
			#      8421       2      6 [seqReb  ]  6(?), 6, 6, 6,...     col
			#      8422       3      5        [desc1   ]  5(?), 4, 3     col
			#      831        3      4  [desc1   ]  4(?),xx, 3,xx, 2     col
			#      8311       5      7  [desc1   ]  7(?),xx, 8,xx, 9     col
			#      8423       5      3 [seqReb  ]  3(?), 3, 8, 8,...     col
			#      1          3      5        [desc1   ]  5(?), 6, 7 Slide\\
			#      11         5      3        [same    ]  3(?), 3, 3 Slide\\
			#      12         5      6     [sameEnd ]  6(?), 3, 3, 6 Slide\\
			#      13         5      3  [same    ]  3(?), ., 3, ., 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    2(2)   3(6)   4(4)   6(4)   8(5)   9(2)   12(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 10 14 22 32 36    | 8  4  8 10  4 |                        |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -1   0  -1   4   0   0 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2
			#     14 26 32 36 39 42(1) |12  6  4  3  3 | 11  16  16  17   8   3 |0 1 1 3 1 |1 1 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      E3       1      2 [desc1   ]  2(?),xx, 3,xx, 4  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -6(2)   -5(2)   -4(2)   -1(3)   0(5)   3(3)   4(2)   8(2)   16(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 5 ]*c(2,2)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# 19
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# 19

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_1()

# UNdone
fCutCnt.nextColVal_2 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  4  9 24 25 33    | 1  5 15  1  8 |                        |3 0 2 1 0 |3 2 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
			#      3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
			#     12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   9  12  13  14  -3 |0 2 2 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
			#   dup number  3:2   9:3   12:2   18:2   24:3   25:2   38:2   43:2
			#   zoid width  ... 30   31   40   28   31   37 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  834        1      5                 [desc1   ]  5(?), 6, 7     col
			#  824        1      7 [seqReb  ]  7(?), ., 7, ., 3, ., 3,...     col
			#  8241       2      9           [same    ]  9(?), ., 9, ., 9     col
			#  710        2      4     [sameEnd ]  4(?),xx, 9,xx, 9,xx, 4     col
			#  8341       2      7                 [desc1   ]  7(?), 8, 9     col
			#  8342       3     18          [seqReb  ] 18(?),18,24,24,...     col
			#  8242       5     34 [seqReb  ] 34(?), .,34, .,25, .,25,...     col
			#  1          1     23           [desc1   ] 23(?),xx,24,xx,25  Slide/
			#  11         3     36                 [desc1   ] 36(?),35,34  Slide/
			#  12         3      9                 [desc1   ]  9(?), 8, 7 Slide\\

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  3>   0      3 ,  4 ,  9 , 24 , 25 , 33 
			#                3 ,  9 , 12 , 13 , 25 , 43 
			#          -->   3*, 14 , 15 , NA , 25!, NA 
			# <  9>   0      3 ,  9 , 12 , 13 , 25 , 43 
			#                7 ,  9 , 24 , 29 , 34 , 38 
			#          -->  NA ,  9*, 36 , 45 , 43 , 33 
			# < 12>  -2     12 , 13 , 25 , 43 
			#               12 , 18 , 24 , 26 
			#          -->  12*, 23 , 23!, NA 
			# < 18>   1     12 , 18 , 24 , 26 , 39 
			#                8 , 18 , 35 , 42 , 43 
			#          -->   4 , 18*, NA , NA , NA 
			# < 24>   0     12 , 18 , 24 , 26 , 39 , 40 
			#                7 ,  9 , 24 , 29 , 34 , 38 
			#          -->   2 , NA , 24*, 32 , 29 , 36 
			# < 25>   0      3 ,  4 ,  9 , 24 , 25 , 33 
			#                3 ,  9 , 12 , 13 , 25 , 43 
			#          -->   3!, 14 , 15 ,  2 , 25*, NA 
			# < 38>   1     14 , 15 , 16 , 17 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->   4 , 33 , NA , NA , 38*
			# < 43>   0      3 ,  9 , 12 , 13 , 25 , 43 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   9 ,  7!, 24 , NA , NA , 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                               descript tgt.dir
			# 834        1      5                 [desc1   ]  5(?), 6, 7     col
			# 823        1      1     [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			# 824        1      7 [seqReb  ]  7(?), ., 7, ., 3, ., 3,...     col
			# 8241       2      9           [same    ]  9(?), ., 9, ., 9     col
			# 710        2      4     [sameEnd ]  4(?),xx, 9,xx, 9,xx, 4     col
			# 8341       2      7                 [desc1   ]  7(?), 8, 9     col
			# 817        2      9           [symm    ]  9(?), 8, 9, 8, 9     col
			# 8342       3      8          [seqReb  ]  8(?), 8, 4, 4,...     col
			# 8242       5      3           [desc1   ]  3(?),xx, 4,xx, 5     col
			# 8243       5      4 [seqReb  ]  4(?), ., 4, ., 5, ., 5,...     col
			# 8244       6      8 [seqReb  ]  8(?), ., 8, ., 3, ., 3,...     col
			# 1          1      3           [desc1   ]  3(?),xx, 4,xx, 5  Slide/
			# 11         2      7                 [desc1   ]  7(?), 8, 9  Slide/
			# 12         2      8          [seqReb  ]  8(?), 8, 9, 9,...  Slide/
			# 13         3      6                 [desc1   ]  6(?), 5, 4  Slide/
			# 14         3      9                 [desc1   ]  9(?), 8, 7 Slide\\
			# 15         4      7                 [desc1   ]  7(?), 8, 9 Slide\\
			# 16         5      6                 [desc1   ]  6(?), 5, 4 Slide\\
			# 17         5      5           [desc1   ]  5(?),xx, 4,xx, 3 Slide\\
			# 18         6      9           [same    ]  9(?), ., 9, ., 9 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                           descript tgt.dir
			#      834        1      2             [same    ]  2(?), 2, 2     col
			#      823        1      6          [sameEnd ]  6(?), 2, 2, 6     col
			#      8231       2      7 [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			#      817        5     18       [symm    ] 18(?), 1, 4, 1,18     col
			#      1          5      4       [desc1   ]  4(?),xx, 5,xx, 6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(8)   2(3)   5(3)   6(3)   7(2)   15(2) 
			cnt.w2 <- 0
			if( aCStep[3]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  3  4  9 24 25 33    | 1  5 15  1  8 |                        |3 0 2 1 0 |3 2 1
			# 14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
			#  3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
			# 12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   9  12  13  14  -3 |0 2 2 1 1 |2 2 1 1
			#  7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      1        6     -2 [same    ] -2(?), .,-2, .,-2     col
			#      E2       1     -2       [desc1   ] -2(?),-1, 0  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -6(2)   -5(2)   -4(2)   -2(2)   -1(2)   9(2)   11(2)   12(2)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c( 1,-1)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_2()

# UNdone
fCutCnt.nextColVal_3 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6  7 10 16 38 41    | 1  3  6 22  3 |                        |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#      6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  1   0  -5 -20  -9   2 |2 2 0 1 1 |2 2 1 1
			#   dup number  6:2   7:3   8:2   15:2   18:2   19:2   33:2   35:2
			#   zoid width  ... 35   34   30   21   37   38 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                      descript tgt.dir
			#  844        1      8        [desc1   ]  8(?), 7, 6     col
			#  8441       2      8        [same    ]  8(?), 8, 8     col
			#  713        2      5     [sameEnd ]  5(?), 8, 8, 5     col
			#  1          1      8 [seqReb  ]  8(?), 8,18,18,...  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  6>   0      6 ,  7 , 10 , 16 , 38 , 41 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   6*,  9!, 26 , NA , NA , 45 
			# <  7>  -1      7 , 19 , 26 , 27 , 35 
			#                7 ,  8 , 13 , 15 , 33 
			#          -->   7*, NA , NA , NA , 31 
			# <  8>   0      6 ,  8 , 18 , 35 , 42 , 43 
			#                7 ,  8 , 13 , 15 , 33 , 45 
			#          -->  NA ,  8*, NA , NA , 24 , NA 
			# < 15>   1      2 ,  5 , 15 , 18 , 19 
			#                8 , 13 , 15 , 33 , 45 
			#          -->  14 , NA , 15*, NA , NA 
			# < 18>  -1      5 , 15 , 18 , 19 , 23 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->   7!,  1 , 18*, NA , NA 
			# < 19>   2      1 ,  7 , 19 , 26 
			#               15 , 18 , 19 , 23 
			#          -->  NA , NA , 19*, 20 
			# < 33>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  8 , 13 , 15 , 33 
			#          -->  10 ,  7!,  2 ,  5 , 33*
			# < 35>  -2     19 , 26 , 27 , 35 
			#                6 ,  8 , 18 , 35 
			#          -->  NA , NA ,  9 , 35*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                            descript tgt.dir
			# 844        1      8              [desc1   ]  8(?), 7, 6     col
			# 8441       2      8              [same    ]  8(?), 8, 8     col
			# 713        2      5           [sameEnd ]  5(?), 8, 8, 5     col
			# 834        3      7        [desc1   ]  7(?),xx, 8,xx, 9     col
			# 8442       4      5              [same    ]  5(?), 5, 5     col
			# 7131       4      8           [sameEnd ]  8(?), 5, 5, 8     col
			# 8341       4      6        [desc1   ]  6(?),xx, 5,xx, 4     col
			# 8443       5      4              [desc1   ]  4(?), 3, 2     col
			# 7132       5     10  [desc1   ] 10(?),xx,xx, 9,xx,xx, 8     col
			# 8342       5     -1 [desc( 3) ] -1(?),xx, 2,xx, 5,xx, 8     col
			# 8343       6      3        [same    ]  3(?), ., 3, ., 3     col
			# 585        6      1  [sameEnd ]  1(?),xx, 3,xx, 3,xx, 1     col
			# 8444       6      5       [seqReb  ]  5(?), 5, 3, 3,...     col
			# 5851       6      1  [symm    ]  1(?), 5, 3, 3, 3, 5, 1     col
			# 1          1      8           [same    ]  8(?), 8, 8, 8  Slide/
			# 11         1      5        [sameEnd ]  5(?), 8, 8, 8, 5  Slide/
			# 12         1      8       [seqReb  ]  8(?), 8, 8, 8,...  Slide/
			# 13         4      3              [same    ]  3(?), 3, 3  Slide/
			# 14         5      3        [symm    ]  3(?), 5, 8, 5, 3 Slide\\
			# 15         6      6        [desc1   ]  6(?),xx, 5,xx, 4 Slide\\
			# 16         6      3       [seqReb  ]  3(?), 3, 5, 5,... Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                               descript tgt.dir
			#      834        1      3           [desc1   ]  3(?),xx, 2,xx, 1     col
			#      8341       1      2 [seqReb  ]  2(?), ., 2, ., 1, ., 1,...     col
			#      8441       2      5          [seqReb  ]  5(?), 5,10,10,...     col
			#      624        2     12        [symm    ] 12(?), 5,10,10, 5,12     col
			#      713        5      5     [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			#      1          5     19                 [desc1   ] 19(?),18,17 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(7)   2(2)   3(4)   5(2)   6(2)   7(2)   8(2)   10(2)   12(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# 19

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  6  7 10 16 38 41    | 1  3  6 22  3 |                        |2 2 0 1 1 |2 2 1 1
			#  1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#  3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#  2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#  6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2
			#  7  8 13 15 33 45(1) | 1  5  2 18 12 |  1   0  -5 -20  -9   2 |2 2 0 1 1 |2 2 1 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      E3       5      4 [desc1   ]  4(?),xx, 3,xx, 2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -6(3)   -5(2)   -2(3)   0(2)   1(2)   2(2)   3(2) 
			cnt.w2 <- 0

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..
} # fCutCnt.nextColVal_3()

# UNdone
fCutCnt.nextColVal_4 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      3  7  8 34 39 41    | 4  1 26  5  2 |                        |3 0 0 2 1 |3 2 1
			#      7 12 16 34 42 45(2) | 5  4 18  8  3 |  4   5   8   0   3   4 |1 2 0 1 2 |1 2 1 2
			#      2  9 15 23 34 40(1) | 7  6  8 11  6 | -5  -3  -1 -11  -8  -5 |2 1 1 1 1 |2 1 1 1 1
			#      1  9 12 28 36 41(1) | 8  3 16  8  5 | -1   0  -3   5   2   1 |2 1 1 1 1 |2 1 1 1 1
			#      5 16 21 23 24 30    |11  5  2  1  6 |  4   7   9  -5 -12 -11 |1 1 3 1 0 |1 1 3 1
			#     12 14 21 30 39 43(2) | 2  7  9  9  4 |  7  -2   0   7  15  13 |0 2 1 2 1 |2 1 2 1
			#   dup number  7:2   9:2   12:3   16:2   21:2   23:2   30:2   34:3   39:2   41:2
			#   zoid width  ... 38   38   38   40   25   31 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  722        3     21             [same    ] 21(?),21,21     col
			#  517        3     12          [sameEnd ] 12(?),21,21,12     col
			#  647        4     23       [same    ] 23(?), .,23, .,23     col
			#  160        4     34 [sameEnd ] 34(?),xx,23,xx,23,xx,34     col
			#  5171       6     41 [same    ] 41(?), ., .,41, ., .,41     col
			#  1          1      7         [desc( 7) ]  7(?),14,21,28  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  7>  -1      7 ,  8 , 34 , 39 , 41 
			#                7 , 12 , 16 , 34 , 42 
			#          -->   7*, 16 , NA , 29 , 43!
			# <  9>   0      2 ,  9 , 15 , 23 , 34 , 40 
			#                1 ,  9 , 12 , 28 , 36 , 41 
			#          -->  NA ,  9*, NA , 33 , 38 , 42!
			# < 12>  -2     12 , 28 , 36 , 41 
			#               12 , 14 , 21 , 30 
			#          -->  12*, NA , NA , 19 
			# < 16>  -1     12 , 16 , 34 , 42 , 45 
			#                5 , 16 , 21 , 23 , 24 
			#          -->  NA , 16*, NA , NA , NA 
			# < 21>   0      5 , 16 , 21 , 23 , 24 , 30 
			#               12 , 14 , 21 , 30 , 39 , 43 
			#          -->  19 , 12 , 21*, 37 , NA , NA 
			# < 23>   0      2 ,  9 , 15 , 23 , 34 , 40 
			#                5 , 16 , 21 , 23 , 24 , 30 
			#          -->   8 , NA , NA , 23*, NA , NA 
			# < 30>  -2     21 , 23 , 24 , 30 
			#               12 , 14 , 21 , 30 
			#          -->   3 ,  5 , 18 , 30*
			# < 34>   1      7 , 12 , 16 , 34 , 42 
			#                9 , 15 , 23 , 34 , 40 
			#          -->  11 , 18 , 30 , 34*, 38 
			# < 39>   0      3 ,  7 ,  8 , 34 , 39 , 41 
			#               12 , 14 , 21 , 30 , 39 , 43 
			#          -->  21 , 21 , 34 , 26 , 39*, 45 
			# < 41>   0      3 ,  7 ,  8 , 34 , 39 , 41 
			#                1 ,  9 , 12 , 28 , 36 , 41 
			#          -->  NA , 11 , 16 , 22 , 33 , 41*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 722        3      1             [same    ]  1(?), 1, 1     col
			# 517        3      2          [sameEnd ]  2(?), 1, 1, 2     col
			# 647        4      3       [same    ]  3(?), ., 3, ., 3     col
			# 160        4      4 [sameEnd ]  4(?),xx, 3,xx, 3,xx, 4     col
			# 6471       5      4       [same    ]  4(?), ., 4, ., 4     col
			# 1601       5      9 [sameEnd ]  9(?),xx, 4,xx, 4,xx, 9     col
			# 6472       6      0       [same    ]  0(?), ., 0, ., 0     col
			# 1602       6      1 [sameEnd ]  1(?),xx, 0,xx, 0,xx, 1     col
			# 5171       6      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			# 1          3      3             [desc1   ]  3(?), 4, 5 Slide\\
			# 11         5     -1             [desc1   ] -1(?), 0, 1 Slide\\
			# 12         5      0       [desc1   ]  0(?),xx, 1,xx, 2 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#          tgt.col banVal                           descript tgt.dir
			#      647        2      4       [desc1   ]  4(?),xx, 5,xx, 6     col
			#      722        2      9         [desc(-2) ]  9(?), 7, 5, 3     col
			#      6471       5      6       [same    ]  6(?), ., 6, ., 6     col
			#      160        5      2 [sameEnd ]  2(?),xx, 6,xx, 6,xx, 2     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(3)   3(2)   4(3)   5(4)   6(3)   7(2)   8(4)   9(2)   11(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(1,3)]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aCStep[c(2,5)])==sum(aCStep[c(1,4)]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  3  7  8 34 39 41    | 4  1 26  5  2 |                        |3 0 0 2 1 |3 2 1
			#  7 12 16 34 42 45(2) | 5  4 18  8  3 |  4   5   8   0   3   4 |1 2 0 1 2 |1 2 1 2
			#  2  9 15 23 34 40(1) | 7  6  8 11  6 | -5  -3  -1 -11  -8  -5 |2 1 1 1 1 |2 1 1 1 1
			#  1  9 12 28 36 41(1) | 8  3 16  8  5 | -1   0  -3   5   2   1 |2 1 1 1 1 |2 1 1 1 1
			#  5 16 21 23 24 30    |11  5  2  1  6 |  4   7   9  -5 -12 -11 |1 1 3 1 0 |1 1 3 1
			# 12 14 21 30 39 43(2) | 2  7  9  9  4 |  7  -2   0   7  15  13 |0 2 1 2 1 |2 1 2 1
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                     descript tgt.dir
			#      E4       2     -5 [same    ] -5(?), .,-5, .,-5  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -5(3)   -3(2)   -1(2)   0(3)   4(3)   5(2)   7(3) 
			cnt.w2 <- 0
			if( aFStep[6]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1	# 20

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_4()

# UNdone
fCutCnt.nextColVal_5 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 16 21 23 24 30    |11  5  2  1  6 |                        |1 1 3 1 0 |1 1 3 1
			#     17 23 27 35 38 43(1) | 6  4  8  3  5 | 12   7   6  12  14  13 |0 1 2 2 1 |1 2 2 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 |-14 -12 -13 -20  -6  -7 |1 3 0 2 0 |1 3 2
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 |  9   3   7  15   7   7 |0 2 1 2 1 |2 1 2 1
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 | -2  -3  -9 -12 -15  -1 |0 4 1 0 1 |4 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 | -5  -5   4   0  13  -4 |2 2 0 2 0 |2 2 2
			#   dup number  5:2   11:2   12:2   14:2   16:2   18:2   21:2   23:2   24:2   30:2   38:2   43:2
			#   zoid width  ... 25   26   33   31   32   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                            descript tgt.dir
			#  802        2     11        [same    ] 11(?), .,11, .,11     col
			#  647        2     16  [sameEnd ] 16(?),xx,11,xx,11,xx,16     col
			#  722        3     21  [same    ] 21(?), ., .,21, ., .,21     col
			#  830        4     18              [same    ] 18(?),18,18     col
			#  7221       4     30           [sameEnd ] 30(?),18,18,30     col
			#  8021       6     48 [desc(-6) ] 48(?),xx,42,xx,36,xx,30     col

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  5>   0      5 , 16 , 21 , 23 , 24 , 30 
			#                5 ,  6 , 16 , 18 , 37 , 38 
			#          -->   5*, NA , 11 , 13 , NA , NA 
			# < 11>   0      3 , 11 , 14 , 15 , 32 , 36 
			#               10 , 11 , 12 , 18 , 24 , 42 
			#          -->  NA , 11*, NA , 21 , 16 , NA 
			# < 12>   2     12 , 14 , 21 , 30 
			#               12 , 18 , 24 , 42 
			#          -->  12*, 22 , 27 , NA 
			# < 14>  -1     11 , 14 , 15 , 32 , 36 
			#               12 , 14 , 21 , 30 , 39 
			#          -->  13!, 14*, 27 , 28 , 42 
			# < 16>   1      5 , 16 , 21 , 23 , 24 
			#                6 , 16 , 18 , 37 , 38 
			#          -->   7!, 16*, NA , NA , NA 
			# < 18>   0     10 , 11 , 12 , 18 , 24 , 42 
			#                5 ,  6 , 16 , 18 , 37 , 38 
			#          -->  NA ,  1 , NA , 18*, NA , 34 
			# < 21>   0      5 , 16 , 21 , 23 , 24 , 30 
			#               12 , 14 , 21 , 30 , 39 , 43 
			#          -->  19 , 12 , 21*, 37 , NA , NA 
			# < 23>  -2     21 , 23 , 24 , 30 
			#               17 , 23 , 27 , 35 
			#          -->  13 , 23*, 30 , 40 
			# < 24>   0      5 , 16 , 21 , 23 , 24 , 30 
			#               10 , 11 , 12 , 18 , 24 , 42 
			#          -->  15 ,  6 ,  3 , 13 , 24*, NA 
			# < 30>  -2     21 , 23 , 24 , 30 
			#               12 , 14 , 21 , 30 
			#          -->   3 ,  5 , 18 , 30*
			# < 38>   1     17 , 23 , 27 , 35 , 38 
			#                6 , 16 , 18 , 37 , 38 
			#          -->  NA ,  9 ,  9 , NA , 38*
			# < 43>   0     17 , 23 , 27 , 35 , 38 , 43 
			#               12 , 14 , 21 , 30 , 39 , 43 
			#          -->   7 ,  5 , 15 , 25 , 40!, 43*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 802        2      1       [same    ]  1(?), ., 1, ., 1     col
			# 647        2      6 [sameEnd ]  6(?),xx, 1,xx, 1,xx, 6     col
			# 722        3      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			# 830        4      8             [same    ]  8(?), 8, 8     col
			# 7221       4      0          [sameEnd ]  0(?), 8, 8, 0     col
			# 1          1      2       [same    ]  2(?), ., 2, ., 2  Slide/
			# 11         5      1       [desc1   ]  1(?),xx, 2,xx, 3 Slide\\
			# 12         6      6             [desc1   ]  6(?), 7, 8 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#         tgt.col banVal                  descript tgt.dir
			#      830       1      1    [same    ]  1(?), 1, 1     col
			#      722       1      2 [sameEnd ]  2(?), 1, 1, 2     col
			#      1         3     20    [desc1   ] 20(?),19,18  Slide/
			#      11        4      3    [desc1   ]  3(?), 2, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(3)   3(2)   4(3)   5(2)   6(4)   8(2)   9(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(1,5)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  5 16 21 23 24 30    |11  5  2  1  6 |                        |1 1 3 1 0 |1 1 3 1
			# 17 23 27 35 38 43(1) | 6  4  8  3  5 | 12   7   6  12  14  13 |0 1 2 2 1 |1 2 2 1
			#  3 11 14 15 32 36    | 8  3  1 17  4 |-14 -12 -13 -20  -6  -7 |1 3 0 2 0 |1 3 2
			# 12 14 21 30 39 43(1) | 2  7  9  9  4 |  9   3   7  15   7   7 |0 2 1 2 1 |2 1 2 1
			# 10 11 12 18 24 42(1) | 1  1  6  6 18 | -2  -3  -9 -12 -15  -1 |0 4 1 0 1 |4 1 1
			#  5  6 16 18 37 38(1) | 1 10  2 19  1 | -5  -5   4   0  13  -4 |2 2 0 2 0 |2 2 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#        tgt.col banVal                        descript tgt.dir
			#      E4       6    -12 [same    ] -12(?), .,-12, .,-12 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -12(2)   -5(2)   7(4)   12(2)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c( 1,-1)==aFStep[c(2,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_5()

# UNdone
fCutCnt.nextColVal_6 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+0]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+1]==c(,,)) ) return(FALSE)	# next rebind of ,,
					if( all(quoSize[1:3+2]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     10 21 22 30 35 42    |11  1  8  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      5  6 16 18 37 38    | 1 10  2 19  1 | -5 -15  -6 -12   2  -4 |2 2 0 2 0 |2 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 | -4   3  -5  -4 -11 -10 |2 2 2 0 0 |2 2 2
			#      9 14 17 33 36 38(2) | 5  3 16  3  2 |  8   5   6  19  10  10 |1 2 0 3 0 |1 2 3
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -8   2  12   0   4   7 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#   dup number  1:2   5:2   9:2   14:2   16:2   18:2   30:2   33:2   38:2   45:2
			#   zoid width  ... 32   33   27   29   44   40 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  845        1      1       [same    ]  1(?), ., 1, ., 1     col
			#  820        1     10 [sameEnd ] 10(?),xx, 1,xx, 1,xx,10     col
			#  838        1      8 [desc1   ]  8(?),xx,xx, 9,xx,xx,10     col
			#  8201       1     10 [symm    ] 10(?), 5, 1, 9, 1, 5,10     col
			#  846        2     20         [desc(-2) ] 20(?),18,16,14     col
			#  8461       3     31             [desc1   ] 31(?),30,29     col
			#  8462       4     41      [seqReb  ] 41(?),41,33,33,...     col
			#  8381       5     37 [desc1   ] 37(?),xx,xx,36,xx,xx,35     col
			#  8463       6     45             [same    ] 45(?),45,45     col
			#  8382       6     38          [sameEnd ] 38(?),45,45,38     col
			#  1          2     27         [desc( 3) ] 27(?),30,33,36  Slide/
			#  11         3     42             [desc1   ] 42(?),41,40  Slide/

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
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			# <  1>   0      1 ,  9 , 11 , 14 , 26 , 28 
			#                1 , 16 , 29 , 33 , 40 , 45 
			#          -->   1*, 23 , NA , NA , NA , NA 
			# <  5>   0      5 ,  6 , 16 , 18 , 37 , 38 
			#                5 , 18 , 30 , 41 , 43 , 45 
			#          -->   5*, 30 , 44 , NA , NA , NA 
			# <  9>  -1      9 , 11 , 14 , 26 , 28 
			#                9 , 14 , 17 , 33 , 36 
			#          -->   9*, 17 , 20 , 40 , 44 
			# < 14>  -2     11 , 14 , 26 , 28 
			#                9 , 14 , 17 , 33 
			#          -->   7 , 14*, NA , 38 
			# < 16>  -1      6 , 16 , 18 , 37 , 38 
			#                1 , 16 , 29 , 33 , 40 
			#          -->  NA , 16*, 40 , 29 , 42 
			# < 18>  -2     16 , 18 , 37 , 38 
			#                5 , 18 , 30 , 41 
			#          -->  NA , 18*, 23 , 44 
			# < 30>  -1     21 , 22 , 30 , 35 , 42 
			#                5 , 18 , 30 , 41 , 43 
			#          -->  NA , 14 , 30*, NA , 44!
			# < 33>   0      9 , 14 , 17 , 33 , 36 , 38 
			#                1 , 16 , 29 , 33 , 40 , 45 
			#          -->  NA , 18 , NA , 33*, 44 , NA 
			# < 38>   0      5 ,  6 , 16 , 18 , 37 , 38 
			#                9 , 14 , 17 , 33 , 36 , 38 
			#          -->  13 , 22 , 18!, NA , 35!, 38*
			# < 45>   0      1 , 16 , 29 , 33 , 40 , 45 
			#                5 , 18 , 30 , 41 , 43 , 45 
			#          -->   9 , 20 , 31!, NA , NA , 45*
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(  )) ) cnt<-cnt+1 # 6
			#      tgt.col banVal                           descript tgt.dir
			# 845        1      1       [same    ]  1(?), ., 1, ., 1     col
			# 820        1      0 [sameEnd ]  0(?),xx, 1,xx, 1,xx, 0     col
			# 8201       1      0 [symm    ]  0(?), 5, 1, 9, 1, 5, 0     col
			# 846        2     10         [desc(-2) ] 10(?), 8, 6, 4     col
			# 8451       4      2       [desc1   ]  2(?),xx, 3,xx, 4     col
			# 8461       4      1      [seqReb  ]  1(?), 1, 3, 3,...     col
			# 838        5      7 [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			# 8462       6      5             [same    ]  5(?), 5, 5     col
			# 8381       6      8          [sameEnd ]  8(?), 5, 5, 8     col
			# 1          1      7             [desc1   ]  7(?), 8, 9  Slide/
			# 11         2     -3         [desc( 3) ] -3(?), 0, 3, 6  Slide/
			# 12         3      2             [desc1   ]  2(?), 1, 0  Slide/
			# 13         6      3             [same    ]  3(?), 3, 3 Slide\\
			# 14         6      7          [sameEnd ]  7(?), 3, 3, 7 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#         tgt.col banVal                     descript tgt.dir
			#      846       2     11       [desc1   ] 11(?),12,13     col
			#      845       3      5 [desc1   ]  5(?),xx, 4,xx, 3     col
			#      836       5      2 [symm    ]  2(?), 2, 5, 2, 2     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(6)   3(3)   5(3)   7(2)   8(2)   11(2)   12(2)   13(2)
			cnt.w2 <- 0
			if( aCStep[1]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			# 10 21 22 30 35 42    |11  1  8  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#  5  6 16 18 37 38    | 1 10  2 19  1 | -5 -15  -6 -12   2  -4 |2 2 0 2 0 |2 2 2
			#  1  9 11 14 26 28    | 8  2  3 12  2 | -4   3  -5  -4 -11 -10 |2 2 2 0 0 |2 2 2
			#  9 14 17 33 36 38(2) | 5  3 16  3  2 |  8   5   6  19  10  10 |1 2 0 3 0 |1 2 3
			#  1 16 29 33 40 45(1) |15 13  4  7  5 | -8   2  12   0   4   7 |1 1 1 1 2 |1 1 1 1 2
			#  5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique	()
			#         tgt.col banVal                     descript tgt.dir
			#      1         2      2       [same    ]  2(?), 2, 2     col
			#      2         2      5    [sameEnd ]  5(?), 2, 2, 5     col
			#      3         2      1 [desc1   ]  1(?),xx, 2,xx, 3     col
			#      4         5      2       [desc1   ]  2(?), 3, 4     col
			#      E3        2      2       [desc1   ]  2(?), 1, 0  Slide/
			#      E31       4      0       [desc1   ]  0(?), 1, 2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -5(2)   -4(3)   0(2)   2(3)   3(2)   4(2)   8(2)   10(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(2,4)==aFStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_6()


#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================

fltCntMtx		<- function( ccObjLst ,allIdxF ){
	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })
	phName <- attributes(cntMtxLst)$names

	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF
	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawCnt <- apply( rawMtx ,1 ,function(rData){ 
					if( any(rData>2) ) return( 100 )

					evtCnt <- sum(rData>1)
					if( evtCnt >2 ) return( 100 ) else return( evtCnt )
	})
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	rawFVCnt <- apply( rawFVMtx ,1 ,function(rData){ 
					if( any(rData>2) ) return( 100 )

					evtCnt <- sum(rData>1)
					if( evtCnt >1 ) return( 100 ) else return( evtCnt )
	})
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )
	remCnt <- apply( remMtx ,1 ,function(rData){ 
					if( any(rData>3) ) return( 100 )

					evtCnt <- sum(rData>2)
					if( evtCnt >2 ) return( 100 ) else return( evtCnt )
	})
	
	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )
	cStepCnt <- rep( 0 ,nrow(cStepMtx) )
	for( rIdx in 1:nrow(cStepMtx) ){
		cStep <- cStepMtx[rIdx,]
		cStep.w <- cStepMtx.w1[rIdx,] + cStepMtx.w2[rIdx,]
		cStep.all <- cStep + cStep.w

		if( any(cStep>2) || any(cStep.w>2) || (cStep.all>3) ){
			cStepCnt[rIdx] <- 100
			next
		}



	}


	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )
	fStepCnt <- rep( 0 ,nrow(fStepMtx) )



	fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( fltFlag )

} # fltCntMtx()

flagScoreMtx		<- function( ccObjLst ,allIdxF ){

} # flagScoreMtx()

flagCStepValMtx		<- function( ccObjLst ,allIdxF ){

} # flagCStepValMtx()

# UNdone		fCut.basic() 사용
rmvRaw <- function( gEnv ,allIdxF ){
    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		# fCutCnt.basic()


		if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c(,) ,c(,) )
		if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }


		if( any(aZoid[c(4,2,3,2,1)]==stdMI$lastZoid[c(5,1,3,3,3)]) ){	surviveFlg[idx]<-FALSE	;next }	# 1개 중복 기존 패턴. h-5,h-4,h-3,h-2,h-1
		if( any(aZoid%in%c(14)) ){	surviveFlg[idx]<-FALSE	;next }	# 11 중복 3번, 14도 3번?
		if( any(aZoid%in%c(32)) ){	surviveFlg[idx]<-FALSE	;next }	# col 3에서의 중복 3번 연속, 다음에도?
		aRem <- aZoid%%10
		if( all(aRem[c(2,3)]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 

		# fCutCnt.nextZW
		aRem <- aZoid%%10
		if( all(aRem[2:3]==aRem[4:5]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 너무 이쁘장한 패턴.
		if( 2<sum(aZoid==c( 8, 7,33,41,NA,36),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextQuo10
		if( 2<sum(aZoid[c(1,6)]==c(12,18,24,26,39,40)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
		if( 2<sum(aZoid==c(22,26,34,30,NA,44),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextBin
		if( 2<sum(aZoid==c( 8,11,37,41,35,32),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 2<sum(aZoid[c(1,5)]==c( 7, 9,24,29,34,38)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(1 두개가 동시발생.)

		# fCutCnt.nextRebNum
		if( 2<sum(aZoid[c(1,3)]==c(14,26,32,36,39,42)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)

		# fCutCnt.nextCStepBin
		if( 2<sum(aZoid[c(1,5)]==c( 7, 9,24,29,34,38)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(1 두개가 동시발생.)
		if( 2<sum(aZoid==c( 8,11,37,41,35,32),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextFStepBin
		if( 2<sum(aZoid[c(2,3)]==c( 5,11,14,30,33,38)[c(1,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(NA, 7,10,39,32,35),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_1
		if( 2<sum(aZoid[c(1,2)]==c(16,25,33,38,40,45)[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 2<sum(aZoid[c(4,5)]==c(16,25,33,38,40,45)[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		#	rebind 결과 컬럼이 계속 2 이긴...
		rebColIdx <- which( aZoid %in% c(16,25,33,38,40,45) )
		if( 0<length(rebColIdx) && any(rebColIdx==2) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextColVal_2
		if( 2<sum(aZoid[c(1,6)]==c( 1, 8, 9,17,29,32)[c(1,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( 2<sum(aZoid[c(4,6)]==c( 1, 8, 9,17,29,32)[c(2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)
		if( all(aZoid[c(4,6)]==c(17,32) ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
		if( aZoid[1:2]==c( 9,25) && 1<sum(aZoid%in%c( 6,16,37,38,41,45)) ) {	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. & 2reb

		# fCutCnt.nextColVal_4
		aRem <- aZoid%%10
		if( all(aRem[c(1,2)]==aRem[c(6,4)]) ){	surviveFlg[idx]<-FALSE	;next }	# aRem 
		if( 2<sum(aZoid[c(2,5)]==c( 5,22,31,32,39,45)[c(1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn(2개 패턴)

		# fCutCnt.nextColVal_5
		if( 2<sum(aZoid==c( 6, 7,29,37,32,31),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_6
		if( 2<sum(aZoid==c(11, 4,23,NA,NA,42),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvRaw()

# UNdone		fCut.basic() 사용
rmvColValSeqNext <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		# anaColEndPtn()
		if( 1<sum(aZoid==c(  8,40,10,26,42,41 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  4,13,NA,19,16,37 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 20,NA,NA,42,31,15 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  1,NA,NA,21,35,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  3,13, 1, 1, 3 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 13,NA, 3, 2, 4 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  2,NA, 8, 2, 1 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  3,NA,13,12, 5 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 13,NA, 4,11,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  8,NA,11,15,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  2,NA, 8,12,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }

		if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		# [  1]             18 31 32                35 37 40
		# [  2]                                     30 38 43 		
		if( all(aZoid[2:4]==c(18,31,32)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[4:6]==c(35,37,40)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[4:6]==c(30,38,43)) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext()

# (no Custom)done		fCut.basic() 사용 - custom 부분이 없을 듯.
rmvColValSeqNext.cStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

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

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		if( !is.null(valMtx) ){
			# matCnt 작업을 루프문으로 바꾼 것.
			# 	matCnt<-sum(aCStep==c( 5, 6, 5, 8, 9 ),na.rm=T)	;cnt<-cnt+ifelse(matCnt>2,1,0)
			matCnt <- apply( valMtx ,1 ,function(val){ sum(aCStep==val,na.rm=T) })
			if( any(matCnt>2) ){	surviveFlg[idx]<-FALSE	;next }
		}

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext.cStep()

# UNdone		fCut.basic() 사용
rmvCStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}


	zMtx <- gEnv$zhF
	stdMI <- fCutU.getMtxInfo( zMtx )

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]

		# fCutCnt.basic()
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 8,NA, 1,27),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(13,NA, 1,22),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 6, 5,NA, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(18, 9,NA, 3, 1),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 7, 3,16, 3, 7),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(2,4)]==c(3,3)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[ 2:4  ]==c( 7,17, 1)) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aCStep,16) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 1, 4,NA, 3),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA,10, 1),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 7, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 16,23, 3, 4 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 6, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 7, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			#	UNIQUE ( 1:) ( 4:) ( 6:) (10:)
			if( 1<sum( aCStep[ 3 ]*c(3,3)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,25, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,15,17, 4 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 1,11,10, 1 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3,NA,NA,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 2,22,16 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 5, 4) ,c(13, 2) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( aCStep[3]==c(4) ){	surviveFlg[idx]<-FALSE	;next }						# unique 6,6,4?
			if( fCutU.hasPtn(c( 18, 9,NA, 3, 2 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23,NA, 4,NA, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3, 2,10 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,NA,16, 4, 2 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3,4)]==c(2,5,5)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3,NA,NA,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA, 1, 7, 3 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10,20 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aCStep,16) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aZoid[2:4]==c(30, 1, 1)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,17,15,10 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,NA,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 3,19 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,NA,18, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,16, 3, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( 1<sum( aCStep[c(4,5)]*c(4,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 5, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 7,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 8, 3,NA, 9 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			if( fCutU.hasPtn(c(  2,15,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 3,15 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12,19, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,NA,NA, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 1, 1) ,c( 2, 2) ,c( 2, 8) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			if( fCutU.hasPtn(c( 17, 2, 7,24 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 4,29 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( fCutU.hasPtn(c( 29, 3, 1, 6 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 2, 5, 3 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 5, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,13, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA,13, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 4 ]*c(3,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,12, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,12, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,15, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 26, 2,NA, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 20, 5,NA, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 8,NA,26 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA,22, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 3, 8) ,c( 3, 4) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( fCutU.hasPtn(c(  1,13, 7,23 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA,18, 4,19 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18, 5,18 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,12, 2,22 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 19,17,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			ptnLst <- list( c( 5,15) ,c( 5,12) ,c( 7,12) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# if( fCutU.hasPtn(c( ,, ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fltRst <- sapply( getSideValues(aCStep,11) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		# if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	dbgZoid[,2:6]-dbgZoid[,1:5]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvCStep()

# UNdone		fCut.basic() 사용
rmvFStep <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)
	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMILst <- fCutU.getStdMI( gEnv )

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 9,10, 4),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(-6, 1)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,11, 8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,10,11 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(2,1)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(1,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,17,11,NA, 4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12,13,NA,NA,10, 4 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7,-5,-1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -13,-10, -8, -1,-15,-14 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -2, 2,NA,NA,-9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(3,1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(2,1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -3,-10,-6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  18,NA,18, 6, 0, 2 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 6,-3)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,6)]*c(1,-2)==aFStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 2,NA,16,-9 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3, 3,-15 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 1, 1,-15, 8 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(2,6)]*c(1,-3)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[c(1,2)]*c(1,1)==aFStep[c(6,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  0,-24,-10,-5,-19 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,  6,-13, 1,  0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c( 5, 3,-2)==aFStep[c( 1, 3, 5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -5, 5,-4, 0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   1,17,10,15,-1 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( fCutU.hasPtn(c( 17, 2, 7,24 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 4,29 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(3,2)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(5,6)]*c(2,2)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(5,6)]*c(2,3)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14, 5,12, 6,-6,-2 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  12, 1,-2,-2 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -1,17,17, 6,-1,-5 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -1, 3, 2,20,32 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -9,10,13, 3 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -17,-6, 4, 4,11 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(-4, 1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -9, 5,-3,21,NA,27 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1,-8, 1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,15, 1,21 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,10, 1, 4, 7 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -8,-5, 5,17,-8,-23 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( fCutU.hasPtn(c( -4, 7,23,NA,-1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4, 7,23,NA,-1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,-2,-3,-2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-4, 1,-3, 1 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-4, 1,-3, 1 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvFStep()

# UNdone		fCut.basic() 사용
rmvFV3 <- function( gEnv ,allIdxF ){

	# Error in if (!is.null(fixIdx) && !matFlag[fixIdx]) { : 
	# TRUE/FALSE가 필요한 곳에 값이 없습니다  <--- fixIdx가 NA를 가리킨 경우.

    initSize <- length(allIdxF)

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		#	fCutCnt.basic
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11,NA,31,23 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 14,22,31,34 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			# <30>
			if( fCutU.hasPtn(c(  8,NA,NA,30,33,31 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 13,14,NA,33,40 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 14,35,31,36,40 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextZW
			# < 6>
			# < 9>
			if( fCutU.hasPtn(c(  6, 9,32,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 10,11,NA,NA,18 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <17>
			if( fCutU.hasPtn(c(  4,13,14,17,27 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>			# <23>			# <28>
			# <34>
			if( fCutU.hasPtn(c( 12,30,27,34,38 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 12,30,27,34,38 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextQuo10
			# < 2>
			if( fCutU.hasPtn(c(  2,15,13,26,45 ),aZoid,thld=3,fixIdx= 1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6, 8,NA,10,27,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c( 12,17,24,18,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>			# <15>			# <16>
			# <18>
			if( fCutU.hasPtn(c(  9,18,29,29 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c(  6,19,25 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 12,17,24,NA,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 24,16,NA,38 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <41>
			if( fCutU.hasPtn(c( 18,22,11,30,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c( 22,14,NA,NA,35,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextBin
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,31,25,24 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,24,33,35 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  4,11,19,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <15>
			if( fCutU.hasPtn(c( 10,15,31,44,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 10, 9,24,33,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c(  7,NA,25,31 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  5,NA, 9,33 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c( 11,28,36,NA,38 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <40>
			if( fCutU.hasPtn(c(  4, 8,13,14,40 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextRebNum
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,17,21 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 14,22,31,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>
			if( fCutU.hasPtn(c(  7,26,28,27 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 11,31,NA,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c( 21,NA,35,NA,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>			# <45>

		#	fCutCnt.nextCStepBin
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,NA,28, 8,22 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			# < 7>
			# < 9>
			if( fCutU.hasPtn(c(  9,24,33,35 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			# <20>
			if( fCutU.hasPtn(c(  7,12,20 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c( 10, 9,24,33,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <27>
			if( fCutU.hasPtn(c( 11,16,27 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  8, 5,NA, 9,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <44>
			if( fCutU.hasPtn(c(  9, 9, 8,14,24,44 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextFStepBin
			# < 4>
			if( fCutU.hasPtn(c(  4,20,20,30 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <20>
			if( fCutU.hasPtn(c(  5,NA,20,31 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  8,19,16,NA,NA,38 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <40>

		#	fCutCnt.nextColVal_1
			# < 2>
			if( fCutU.hasPtn(c(  2,36,39,44,NA,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  9,10,13,26,37,31 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <18>
			if( fCutU.hasPtn(c(  9,18,29,32,33 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <22>
			# <25>
			if( fCutU.hasPtn(c( 14,25,32,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 10,23,33,40,43 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  9, 9,36,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			if( fCutU.hasPtn(c(  9, 9,36,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_2
			# < 4>
			if( fCutU.hasPtn(c(  4,25,NA,NA,45,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,21,40,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>			# <13>
			# <17>
			if( fCutU.hasPtn(c(  3, 3,17,33,30 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c(  1,14,29 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 14,17,28,32 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c( 18, 9,NA, 2,17,34 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_3
			# < 7>
			if( fCutU.hasPtn(c(  7, 8,24,25,44 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c(  6,14,28,22 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>			# <27>			# <31>			# <35>			# <37>
			# <40>
			if( fCutU.hasPtn(c(  5,14, 9,17,40,44 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c(  9,12,42 ),aZoid,thld=3,fixIdx= ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>
			if( fCutU.hasPtn(c(  3,34,40,41,30,43 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_4
			# < 4>
			if( fCutU.hasPtn(c(  4,13,42,38,41 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 5>
			if( fCutU.hasPtn(c(  5,38,NA,39,41 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6,12,13 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <12>
			if( fCutU.hasPtn(c(  3,12,24,25,40 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>
			if( fCutU.hasPtn(c(  9,20,NA,24,43 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c(  2, 3,12,NA,25,29 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <41>
			if( fCutU.hasPtn(c(  9,19,27,34,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_5
			# < 9>
			if( fCutU.hasPtn(c(  9,15,22,28,34 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c( 10,11,26,43,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c(  6, 5,20,21,43 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			# <34>
			if( fCutU.hasPtn(c(  9, 3,28,27,34 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  6, 8,16,10,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 21, 7,26,28,42 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_6
			# < 8>
			# < 9>
			if( fCutU.hasPtn(c(  2, 9,10,31,32 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c( 12,NA,15,20 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <18>
			if( fCutU.hasPtn(c(  4, 7,18 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c( 17, 7,23,29 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c(  3,32,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>
			if( fCutU.hasPtn(c(  7, 7,22,NA,NA,43 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )
}	# rmvFV3()

# UNdone		fCut.basic() 사용
rmvQuo10 <- function( gEnv ,allIdxF ){

    initSize <- length(allIdxF)

	surviveFlg <- rep( TRUE ,length(allIdxF) )			;dbgLst <- list()
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		quoSize <- fCutU.getQuoObj( aZoid )$size
		dbgLst[[1+length(dbgLst)]] <- paste( quoSize,collapse="," )

		# fCutCnt.basic
			#      2 25 28 30 33 45    |23  3  2  3 12 |                        |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
		if( 3==quoSize[4] && all(quoSize[3]==quoSize[5]) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(quoSize==c(2,1,2,0,1)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#      4 20 26 28 35 40    |16  6  2  7  5 |                        |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  6  33  18  16  11  -1 |1 0 0 3 2 |1 3 2
			#      2  3 12 20 27 38(1) | 1  9  8  7 11 | -5 -34 -26 -19 -13  -6 |2 1 2 1 0 |2 1 2 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1
			#      5 11 14 30 33 38    | 6  3 16  3  5 | -5  -4  -4   9  -1  -3 |1 2 0 3 0 |1 2 3
		if( all(quoSize[1:4]==c(0,0,3,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 0 3 1 1 --*--> 0 0 3 2
		if( all(quoSize[2:5]==c(1,0,0,3)) ){	surviveFlg[idx]<-FALSE	;next }	# 2 0 3 0 --*--> 1 0 0 3

		# fCutCnt.nextColVal_1
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2 22 27 33 36 37(2) |20  5  6  3  1 |  0  14  12  11  11  -4 |1 0 2 3 0 |1 2 3
			#     11 18 21 36 37 43(2) | 7  3 15  1  6 |  9  -4  -6   3   1   6 |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -8   2 -12  -6  -4 |1 1 2 2 0 |1 1 2 2
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  3   0  -5   1   3  -4 |1 2 1 2 0 |1 2 1 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  15  15  13   6  10 |0 1 1 2 2 |1 1 2 2
		if( all(quoSize[2:5]==c(1,2,1,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 1 1 2 2 -> 1 2 1 2
		if( all(quoSize[2:5]==c(0,2,1,2)) ){	surviveFlg[idx]<-FALSE	;next }	# 2 1 2 0 -> 0 2 1 2

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	table( sapply( dbgLst,function(str){str}) )
	allIdxF <- allIdxF[surviveFlg]


	# 0,1,1,1,3 0,1,2,0,3 0,1,2,1,2 0,1,2,3,0 0,1,3,0,2 0,1,3,1,1 0,1,3,2,0 0,2,0,1,3 0,2,0,2,2 0,2,1,0,3 0,2,1,1,2 0,2,1,2,1 0,2,1,3,0 0,2,2,0,2 0,2,2,1,1 0,2,2,2,0 0,2,3,0,1 0,2,3,1,0 0,3,0,0,3 0,3,0,1,2 0,3,0,2,1 0,3,0,3,0 0,3,1,0,2 
	#        55        58       612       803       300       641      1202        54       125       117       562       646       709       374       864      1819       194       525        23       249       306       342       270 
	# 0,3,1,1,1 0,3,1,2,0 0,3,2,0,1 0,3,2,1,0 1,0,1,1,3 1,0,1,2,2 1,0,2,0,3 1,0,2,1,2 1,0,2,3,0 1,0,3,0,2 1,0,3,1,1 1,0,3,2,0 1,1,0,1,3 1,1,0,3,1 1,1,1,0,3 1,1,1,1,2 1,1,1,2,1 1,1,1,3,0 1,1,2,0,2 1,1,2,1,1 1,1,2,2,0 1,1,3,0,1 1,1,3,1,0 
	#       778      1782       332      1394       272       642       318      2275      1135      1087      1804      1507       283       198       538      3346      3169      1705      2411      3595      2955       242       364 
	# 1,2,0,0,3 1,2,0,1,2 1,2,0,2,1 1,2,1,0,2 1,2,1,1,1 1,2,1,2,0 1,2,2,0,1 1,2,2,1,0 1,2,3,0,0 1,3,0,0,2 1,3,0,1,1 1,3,0,2,0 1,3,1,0,1 1,3,1,1,0 1,3,2,0,0 2,0,0,1,3 2,0,0,2,2 2,0,0,3,1 2,0,1,0,3 2,0,1,1,2 2,0,1,2,1 2,0,1,3,0 2,0,2,0,2 
	#       315      2265      2132      2555      4438      4044      2268      4295      2098       610      1004       922      1156      2124      1455       294       726       397       529      3141      3282      1186      2317 
	# 2,0,2,1,1 2,0,2,2,0 2,0,3,0,1 2,0,3,1,0 2,1,0,0,3 2,1,0,1,2 2,1,0,2,1 2,1,0,3,0 2,1,1,0,2 2,1,1,1,1 2,1,1,2,0 2,1,2,1,0 2,1,3,0,0 2,2,0,0,2 2,2,0,1,1 2,2,0,2,0 2,2,1,0,1 2,2,1,1,0 2,3,0,0,1 2,3,0,1,0 2,3,1,0,0 3,0,0,0,3 3,0,0,1,2 
	#      3998      2657       687       980       497      3351      3285      1397      3654      7175      5488      5750      2978      1723      3251      2595      3255      4887       561       753       739       145      1428 
	# 3,0,0,2,1 3,0,0,3,0 3,0,1,0,2 3,0,1,1,1 3,0,1,2,0 3,0,2,0,1 3,0,2,1,0 3,0,3,0,0 3,1,0,0,2 3,1,0,1,1 3,1,0,2,0 3,1,1,0,1 3,1,1,1,0 3,1,2,0,0 3,2,0,0,1 3,2,0,1,0 3,2,1,0,0 
	#      1458       334      1491      3039      1605      1595      1984      1260      1701      3307      1695      3702      4195      4874      1600      1480      1905 

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( NA )","nextColVal_1( 29, 36 )","nextColVal_5( 34 )","nextFStepBin( 36 )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c(29,36,34) )]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )
}	# rmvZW()


cust.byOnePhase <- function( ccObj ,phName ){

    #   ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
    cntMtx <- ff0.byOnePhase.getCntMtx( ccObj )


    filtedLst <- vector("list",nrow(cntMtx) )
    filtedFlg <- rep( FALSE ,nrow(cntMtx) )

    for( rIdx in 1:nrow(cntMtx) ){
        score <- cntMtx[rIdx,c("raw","rawFV","rem","cStep","fStep","cStep.w1","cStep.w2","fStep.w1","fStep.w2","auxZW","auxQuo")]

		# QQE customizing...

    }   # table(filtedFlg)  ;dbgIdx <- head(which(filtedFlg))


    return( list(filtedLst=filtedLst ,filtedFlg=filtedFlg) )

}	# cust.byOnePhase()






