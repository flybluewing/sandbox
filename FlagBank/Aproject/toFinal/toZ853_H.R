# toZ853_H.R 최종접근
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
	# allIdxF <- rmvFV3( gEnv ,allIdxF )
	save( allIdxF ,file="Obj_allIdxF.fCut.basic.save")

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
	stdMI <- NULL 	# Copy&Paste 안전장치.

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.basic()

# UNdone
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
					# [1]   19 16 34  2
					# [2]   13 14 33  9 31 29 31 19  5  8
					# [3]   28 34
					# [4]   32 31 24 31
					# [5]   23
					# [6]   40 38 32 28 34 37 40 37 42

					if( 1<sum(aZoid==c( 19,13,28,32,23,40 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 16,14,34,31,NA,38 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 34,33,NA,24,NA,32 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  1, 9,NA,31,NA,28 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 24,31,32 ),aZoid) ) cnt<-cnt+1	# unique 32/28 --> 28,32

					score <- 0
					if( aZoid[1]%in%c(           ) ) score<-score+1
					if( aZoid[2]%in%c( 12        ) ) score<-score+1
					if( aZoid[3]%in%c( 32        ) ) score<-score+1	# 32/28/34 --> ??/28/34
					if( aZoid[4]%in%c( 31,33     ) ) score<-score+1
					if( aZoid[5]%in%c(           ) ) score<-score+1
					if( aZoid[6]%in%c( 38        ) ) score<-score+1	# 38 : 37,40,37 --> 38,49,??
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
					if( aZoid[1]%in%c(            ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(            ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(            ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(            ) ) cnt<-cnt+1	# 
					if( aZoid[5]%in%c(            ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(            ) ) cnt<-cnt+1

					# if( all(aZoid[c(3,5)]==c(33,36)) ) cnt<-cnt+1
					if( all(aZoid[1:2+1]==c( 24,35 )) ) cnt<-cnt+1	# 42,45 unique

					# [  1]  2 22    42 45    22 25    38 39    24 35
					# [  2]  9 10    15 22     7  8    21 25    42 45
					# [  3] 34 38     2  3     2 23    20 22    12 24
					# [  4] 20 26     7  8    22 43    24 32    32 37
					# [  5] 20 23    28 31     4  8    14 27         
					# [  6]  6 24     5 34             34 40         
					# [  7] 26 28                       8  9         
					# [  8] 12 17                      36 42         
					# [  9]                            25 31         
					# [ 10]                            18 26         


					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(         ),c(     )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c(         ),c(     )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 2       ),c(     )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 1       ),c(     )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 4,2     ),c(     )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 5,4     ),c(     )) )	remCnt <- remCnt+1

						# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
						#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

						# grp (1:2+0)
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( , ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
						# grp (1:2+3)
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

					score  <- sum(aCStep==c( 20, 3, 3, 1,11 ),na.rm=T)
					matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1, 7, 1, 4, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  4, 1,21, 2,12 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  6, 1,21, 8, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  3, 3, 4,13,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 18,29,NA, 6,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  2,NA,NA, 1,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5,NA,NA, 6,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1] 20   1   4   6   3  18   2   5 
					#	[2]  3   7   1   1   3  29 
					#	[3]  3   1  21  21   4 
					#	[4]  1   4   2   8  13   6   1   6   6   8 
					#	[5] 11   3  12   5 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  3, 3 ),aCStep) ) cnt<-cnt+1	# unique ( 3, 3,NA, 9)
						if( 1<sum(aCStep[1:2+1]==c(  6, 1 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 3 ),aCStep) ) cnt<-cnt+1 # -

						if( 1<sum(aCStep[1:2+2]==c( 20, 3 )) )	cnt<-cnt+1	# unique (20, 3)/1/4

						if( aCStep[1]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  2     ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(        ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  1, 6  ) ) cnt<-cnt+1

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
					#        *  *  *     *  *  *     *  *  *     *  *  *
    				# [  1] 26 28 31                            36 42 45
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} # fCutCnt.colValSeqNext()

# UNdone
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

					# [1]*   4  8 13 13  6  6  4  6 11  5 20  4  6  4  6  5  3  1 11  5 ...
					# [2]*   4  8 13 13  6  6  4  6 11  5 20  4  6  4  6  5  3  1 11  5 ...
					# [3]*   4  8 13 13  6  6  4  6 11  5 20  4  6  4  6  5  3  1 11  5 ...
					# [4]*   3  5  8  8  8  3  3  2  5  7 11  3  6  2 10  8 11
					# [5]*   5  5  5  4 12  9 12  2  7  1  9  9

					tCnt <- 0
						if( aCStep[1]%in%c(  4       ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  4       ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(          ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(          ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(  5, 4    ) ) tCnt<-tCnt+1

						if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(3,5)] ) )	cnt<-cnt+1	# unique
						if( 1<sum( aCStep[c(1,3)]*c(4,4)==aCStep[c(2,5)] ) )	cnt<-cnt+1

						# if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt<-cnt+1
						# if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt<-cnt+1
						# if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt<-cnt+1	# 

					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

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
					# [  1]  4  4     4  4     9  3     8  6
					# [  2]  6  1     6  1     3  7     7  5
					# [  3]  3 11     3 11     3 11     1 13
					# [  4]  2 13     2 13     4  8     5  2
					# [  5] 17  5    17  5     4  1     6  8
					# [  6]  1  2     1  2    11 17     6  3
					# [  7]  1  6     1  6    13  1    19  3
					# [  8] 13  2    13  2    11  6     7  1
					# [  9]  4  7     4  7     5  4         
					# [ 10] 17  2    17  2     5  3      

					if( aCStep[1]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  2         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  9, 6      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  7         ) ) cnt<-cnt+1

					if( all(aCStep[1:2+2]==c(2,6)) ) cnt<-cnt+1	# unique
					if( all(aCStep[1:2+3]==c(9,7)) ) cnt<-cnt+1 # unique

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  4, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  1, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c(  4, 7 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  4, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  1, 3 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+1]==c(  4, 7 )) ) cnt<-cnt+1

						if( all(aCStep[1:2+2]==c(  4, 8 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  4, 4 )) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(  4,11 )) ) cnt<-cnt+1

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

					if( (aCStep[5]==1)&&(aCStep[3]==aCStep[4]) ) cnt<-cnt+1	# unique

					#        *  *  *     *  *  *     *  *  *   
					# [  1]  4  4  4     1 19  4  
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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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
	# 	rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  Templete  number    | for sample    |                        |0 1 2 2 1 |1 2 2 1
			#  Templete  number    | for sample    | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
			#  Templete  number( ) | for sample    |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
			#  Templete  number( ) | for sample    |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
			#  Templete  number( ) | for sample    | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#  Templete  number( ) | for sample    |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
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
			
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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
	# 	# w1, w2는 이전에서 정산되어 있어야 한다.

	cntMtxLst <- lapply( ccObjLst ,function(p){ p$cntMtx })
	phName <- attributes(cntMtxLst)$names

	# cnt <- rep( 0 ,nrow(cntMtxLst[[1]]) )	;names(cnt) <- allIdxF
	rawMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"raw"] } ) )	#	rownames(rawMtx) <- allIdxF
	rawFVMtx <- do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rawFV"] } ) )	#	rownames(rawFVMtx) <- allIdxF
	remMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"rem"] } ) )
	
	cStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	cStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w1"] } ) )
	cStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"cStep.w2"] } ) )

	fStepMtx <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep"] } ) )	# w1, w2는 이전에서 정산되어 있어야 한다.
	fStepMtx.w1 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w1"] } ) )
	fStepMtx.w2 <-  do.call( cbind ,lapply( cntMtxLst ,function(obj){ obj[,"fStep.w2"] } ) )


    #   basic nextZW nextQuo10 nextBin nextRebNum nextCStepBin nextFStepBin 
	#	nextColVal_1 nextColVal_2 nextColVal_3 nextColVal_4 nextColVal_5 nextColVal_6 

	allIdxF.len <- length(allIdxF)
	fltLst <- vector( "list",allIdxF.len )		;names(fltLst) <- paste("a",allIdxF)
	for( aIdx in seq_len(allIdxF.len) ){

		# # -[rawMtx]-----------------------------------------------------------
			fndIdx <- which(rawMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw001" )
			fndIdx <- which(rawMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 2< length(fndIdx) )	fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw011" )
				if( 2==length(fndIdx) ){
					# if( all(c("nextQuo10","nextColVal_6")==names(fndIdx)) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
					fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw012" )
				}
				if( 1==length(fndIdx) ){
					banCol <- c("nextRebNum")
					if( banCol %in% names(fndIdx) )		fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw013" )
				}
			}
			fndIdx <- which(rawMtx[aIdx,]==1)
			if( 5<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw021" )
		# # -[remMtx]-----------------------------------------------------------
			fndIdx <- which(remMtx[aIdx,]>3)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem001" )
				if( any(names(fndIdx)%in%c("nextColVal_2"))  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem002" )
			}
			fndIdx <- which(remMtx[aIdx,]==3)
			if(0<length(fndIdx)){
				if( 2<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem011" )
				if( 1==length(fndIdx) ){
					banCol <- c("nextColVal_3","nextQuo10","nextColVal_6")
					if( names(fndIdx) %in% banCol ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem012" )
				}
			}
			fndIdx <- which(remMtx[aIdx,]==2)
			if( 4<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem021" )
			fndIdx <- which(remMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem031" )
			fndIdx <- which( 0<remMtx[aIdx,] & remMtx[aIdx,]<3 )
			if( 10<length(fndIdx) || length(fndIdx)<4 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"rem041" )
		# # -[cStepMtx]-----------------------------------------------------------
			fndIdx <- which(cStepMtx[aIdx,]>2)
			if(0<length(fndIdx)){
				if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep001" )
				banCol <- c("nextColVal_4")
				if( any(names(fndIdx)%in%banCol)  ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep002" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==2)
			if(0<length(fndIdx)){
				if( 3<length(fndIdx) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep011" )
			}
			fndIdx <- which(cStepMtx[aIdx,]==1)
			if( 8<length(fndIdx) || length(fndIdx)<2 ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStep021" )
		# # -[fStepMtx]-----------------------------------------------------------
			fndIdx <- which(fStepMtx[aIdx,]>2)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep001" )
			fndIdx <- which(fStepMtx[aIdx,]==2)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep011" )
			fndIdx <- which(fStepMtx[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStep021" )

		# # -[cStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:001" )
			fndIdx <- which(cStepMtx.w1[aIdx,]==1)
			if(4<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW1:011" )
		# # -[cStepMtx.w2]-----------------------------------------------------------
			fndIdx <- which(cStepMtx.w2[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:001" )
			fndIdx <- which(cStepMtx.w2[aIdx,]==1)
			if(1<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:011" )
			if( 1==length(fndIdx) ) {
				banCol <- c("nextRebNum","nextQuo10","nextFStepBin","nextColVal_1","nextColVal_6")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"cStepW2:021" )
			}

		# # -[fStepMtx.w1]-----------------------------------------------------------
			fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(2<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextRebNum","basic","nextColVal_5")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW1:021" )
			}
		# # -[fStepMtx.w2]-----------------------------------------------------------
			# fndIdx <- which(fStepMtx.w1[aIdx,]>1)
			# if(0<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:001" )
			fndIdx <- which(fStepMtx.w1[aIdx,]==1)
			if(3<length(fndIdx)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:011" )
			if(1==length(fndIdx)) {
				banCol <- c("nextZW","nextColVal_2","nextQuo10","nextFStepBin")
				if( any(banCol%in%names(fndIdx)) ) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"fStepW2:021" )
			}


		# # -[raw.w1.w2]-----------------------------------------------------------
			cntVal <- rawMtx[aIdx,] + cStepMtx.w1[aIdx,] + cStepMtx.w2[aIdx,] + fStepMtx.w1[aIdx,] + fStepMtx.w2[aIdx,]
			if(0==sum(cntVal)) fltLst[[aIdx]] <- c( fltLst[[aIdx]] ,"raw.w1.w2:000" )
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="fStep021")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	# fltFlag <- rep( FALSE ,nrow(cntMtxLst[[1]]) )	;names(fltFlag) <- allIdxF
	return( list( allIdxF=allIdxF ,fltCnt=fltCnt ) )

} # fltCntMtx()

fltScoreMtx		<- function( ccObjLst ,allIdxF ){
	
	allIdxF.len <- length( allIdxF )
	scoreMtxLst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx })
	phName <- attributes(scoreMtxLst)$names
	colName <- colnames(scoreMtxLst[[1]])

	scoreMtx <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx) <- phName	;colnames(scoreMtx)=colName


	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		fndIdx <- which(fStepMtx[aIdx,]>2)
	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx()

fltScoreMtx2		<- function( ccObjLst ,allIdxF ){

	allIdxF.len <- length( allIdxF )
	scoreMtx2Lst <- lapply( ccObjLst ,function(p){ p$cccObj$scoreMtx2 })
	phName <- attributes(scoreMtx2Lst)$names
	colName <- colnames(scoreMtx2Lst[[1]])

	scoreMtx2 <- matrix( NA, nrow=length(phName), ncol=length(colName) )
	rownames(scoreMtx2) <- phName	;colnames(scoreMtx2)=colName
	scoreMtx2.evt <- scoreMtx2

	fltLst <- vector( "list",allIdxF.len )
	for( aIdx in seq_len(allIdxF.len) ){
		scoreMtx2[,] <- 0		;scoreMtx2.evt[,] <- 0
		for( phIdx in phName ){
			scoreMtx2[phIdx,] <- scoreMtx2Lst[[phIdx]][aIdx,]
			scoreMtx2.evt[phIdx,] <- scoreMtx2[phIdx,] >= c( 3, 2, 1, 1, 100,100, 2, 2 )	# rebL.cnt,rebR.cnt은 100으로
		}

		evtCnt <- apply( scoreMtx2.evt ,1 ,sum )
		if( any(3<=evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0010") }
		if( 5<sum(evtCnt) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0020") }

		# # -[rebV]--------------------------------------------------------------
		if( 7>sum(scoreMtx2[,"rebV"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0030") }
		flg <- scoreMtx2[,"rebV"] >= 3
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0040") }
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0050") }
		flg <- scoreMtx2[,"rebV"] == 2
			if( 5 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0060") }

		# # -[rebC]--------------------------------------------------------------
		if( 0==sum(scoreMtx2[,"rebC"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0070") }
		flg <- scoreMtx2[,"rebC"] >= 2
			if( 2 < sum(flg) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0080") }
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0090") }

		# # -[rebL]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebL"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0100") }

		# # -[rebR]--------------------------------------------------------------
		if( 1<sum(scoreMtx2[,"rebR"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0110") }

		# # -[inc.raw]-----------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.raw"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0120") }
		flg <- scoreMtx2[,"inc.raw"] >= 2
			banArea <- c("basic","nextZW","nextQuo10","nextBin","nextRebNum","nextCStepBin","nextFStepBin")
			if( any(flg[banArea]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0130") }

		# # -[inc.cStep]---------------------------------------------------------
		if( 6<sum(scoreMtx2[,"inc.cStep"]) ){	fltLst[[aIdx]]<-c(fltLst[[aIdx]],"0140") }

	}
	# debug
	# table(do.call(c,fltLst,))	;fltCnt <- sapply(fltLst,length)	;table(fltCnt)	;dbgIdx <- which(fltCnt>0)
	# dbgIdx<-which(sapply(fltLst,function(p){any(p=="0130")}))		;aIdx <- dbgIdx[1]

	fltCnt <- sapply(fltLst,length)
	allIdxF <- allIdxF[fltCnt==0]
	rObj <- list( allIdxF=allIdxF ,fltCnt=fltCnt ,fltLst=fltLst )

	return( rObj )

} # fltScoreMtx2()

fltCStepValMtx		<- function( ccObjLst ,allIdxF ){

} # fltCStepValMtx()

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
		aBin <- aZoid %%  2
		aRem <- aZoid %% 10

		# #	a,b,b.. a?
		# 	if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 42,42,33,39, 8,41 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	a,b,b.. b?
		# 	if( 2<sum(aZoid[c( 6,6,5,6,1,5 )]==c( 39,45,34,42, 4,33 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	a,a,b.. b?
		# 	if( 2<sum(aZoid[c( 6,6 )]==c( 44,19 )) ){	surviveFlg[idx]<-FALSE	;next }
		# #	q,a,b,a.. q?
		# 	if( 2<sum(aZoid[c( 6,4,3 )]==c( 43,36,26 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	이번엔 죄다 5,6 컬럼들 뿐이라서..

		#	bin pattern
		# if( all(aBin[c(1,2,3,4,5)]==c(  1,  1,  1,  0,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10
		# if( all(aBin[c(1,2,3,4,5)]==c(  1,  0,  1,  0,  1 )) ){	surviveFlg[idx]<-FALSE	;next }	#	nextQuo10

		# fCutCnt.basic()
			#      1 16 29 33 40 45    |15 13  4  7  5 |                        |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 |-11 -14 -10  -6   8  -3 |2 1 1 2 0 |2 1 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 |  4  11   1   7  -4   0 |1 2 1 2 0 |1 2 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2
			if( aZoid[6] %in% c(42,39) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c(16,20,24,28,36,39)[c(6,3,2)]==aZoid[c(6,5,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextZW
			#     17 23 27 35 38 43    | 6  4  8  3  5 |                        |0 1 2 2 1 |1 2 2 1
			#     10 14 16 18 27 28(1) | 4  2  2  9  1 | -7  -9 -11 -17 -11 -15 |0 4 2 0 0 |4 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -5  -8  -5  -4  -6  13 |2 2 1 0 1 |2 2 1 1
			#      2 10 11 19 35 39(1) | 8  1  8 16  4 | -3   4   0   5  14  -2 |1 3 0 2 0 |1 3 2
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 |  3   1   1  10  -2   5 |1 2 1 1 1 |1 2 1 1 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  2  -3   1 -14   0   1 |2 2 0 1 1 |2 2 1 1
			if( all(aRem[c(3,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 7, 8,13,15,33,45)[c(3,2,3,3,5)]==aZoid[c(5,3,3,2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextQuo10
			#     13 15 18 24 27 41    | 2  3  6  3 14 |                        |0 3 2 0 1 |3 2 1
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 | -8  -9  -7 -10  -6   0 |2 2 1 0 1 |2 2 1 1
			#      2  6  7 12 19 45(1) | 4  1  5  7 26 | -3   0  -4  -2  -2   4 |3 2 0 0 1 |3 2 1
			#     14 20 23 31 37 38    | 6  3  8  6  1 | 12  14  16  19  18  -7 |0 1 2 3 0 |1 2 3
			#      2 21 28 38 42 45(1) |19  7 10  4  3 |-12   1   5   7   5   7 |1 0 2 1 2 |1 2 1 2
			#     13 14 19 26 40 43    | 1  5  7 14  3 | 11  -7  -9 -12  -2  -2 |0 3 1 0 2 |3 1 2
			if( 2<=sum(c(13,14,19,26,40,43)[c(6,2,6)]==aZoid[c(6,2,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextBin
			#      1  5 21 25 38 41    | 4 16  4 13  3 |                        |2 0 2 1 1 |2 2 1 1
			#     15 20 23 26 39 44    | 5  3  3 13  5 | 14  15   2   1   1   3 |0 1 3 1 1 |1 3 1 1
			#      7  9 15 26 27 42(2) | 2  6 11  1 15 | -8 -11  -8   0 -12  -2 |2 1 2 0 1 |2 1 2 1
			#      5 20 21 24 33 40    |15  1  3  9  7 | -2  11   6  -2   6  -2 |1 0 3 1 1 |1 3 1 1
			#      9 14 20 22 33 34(2) | 5  6  2 11  1 |  4  -6  -1  -2   0  -6 |1 1 2 2 0 |1 1 2 2
			#      1  2 15 19 24 36    | 1 13  4  5 12 | -8 -12  -5  -3  -9   2 |2 2 1 1 0 |2 2 1 1
			if( 1<sum( aZoid[ 2 ]*c(12,18)==aZoid[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(c(  1, 2,15,19,24,36)[c(1,4)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c(  1, 2,15,19,24,36)[c(2,5)]==aZoid[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextRebNum
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 15   4  -5  -5  -4  -6 |0 1 3 2 0 |1 3 2
			if( 2<=sum(c(16,20,24,28,36,39)[c(3,6,4,2)]==aZoid[c(1,6,5,1)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c(16,20,24,28,36,39)[c(5,6)]==aZoid[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextCStepBin
			#      7  9 15 26 27 42    | 2  6 11  1 15 |                        |2 1 2 0 1 |2 1 2 1
			#      4  8 19 25 27 45(1) | 4 11  6  2 18 | -3  -1   4  -1   0   3 |2 1 2 0 1 |2 1 2 1
			#      5 20 21 24 33 40    |15  1  3  9  7 |  1  12   2  -1   6  -5 |1 0 3 1 1 |1 3 1 1
			#      9 14 20 22 33 34(2) | 5  6  2 11  1 |  4  -6  -1  -2   0  -6 |1 1 2 2 0 |1 1 2 2
			#     18 20 24 27 31 42(1) | 2  4  3  4 11 |  9   6   4   5  -2   8 |0 1 3 1 1 |1 3 1 1
			#      1  2 15 19 24 36(1) | 1 13  4  5 12 |-17 -18  -9  -8  -7  -6 |2 2 1 1 0 |2 2 1 1
			if( 1<sum( aZoid[ 2 ]*c(12,18)==aZoid[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 1, 2,15,19,24,36)[c(5,3,3)]==aZoid[c(5,2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 1, 2,15,19,24,36)[c(2,5)]==aZoid[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextFStepBin
			#      1  3 17 20 31 44    | 2 14  3 11 13 |                        |2 1 1 1 1 |2 1 1 1 1
			#      1  7 22 33 37 40(1) | 6 15 11  4  3 |  0   4   5  13   6  -4 |2 0 1 2 1 |2 1 2 1
			#      4  7 11 24 42 45(1) | 3  4 13 18  3 |  3   0 -11  -9   5   5 |2 1 1 0 2 |2 1 1 2
			#      5  6 11 14 21 41(1) | 1  5  3  7 20 |  1  -1   0 -10 -21  -4 |2 2 1 0 1 |2 2 1 1
			#      8  9 18 21 28 40(1) | 1  9  3  7 12 |  3   3   7   7   7  -1 |2 1 2 0 1 |2 1 2 1
			#      1 21 26 36 40 41(2) |20  5 10  4  1 | -7  12   8  15  12   1 |1 0 2 1 2 |1 2 1 2
			if( 2<=sum(c( 1,21,26,36,40,41)[c(1,2,3,5)]==aZoid[c(1,2,3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 1,21,26,36,40,41)[c(4,6)]==aZoid[c(2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_1
			#      8 13 20 22 23 36    | 5  7  2  1 13 |                        |1 1 3 1 0 |1 1 3 1
			#      1  5  9 21 27 35    | 4  4 12  6  8 | -7  -8 -11  -1   4  -1 |3 0 2 1 0 |3 2 1
			#      9 14 15 17 31 33(1) | 5  1  2 14  2 |  8   9   6  -4   4  -2 |1 3 0 2 0 |1 3 2
			#      5 12 14 32 34 42(1) | 7  2 18  2  8 | -4  -2  -1  15   3   9 |1 2 0 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  2  25  24   7   6   2 |1 0 0 3 2 |1 3 2
			#     10 21 22 30 35 42    |11  1  8  5  7 |  3 -16 -16  -9  -5  -2 |0 1 2 2 1 |1 2 2 1
			if( 1<sum( aZoid[c(1,2)]*c(3,2)==aZoid[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aRem[c(1,3)]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			#     16 23 27 29 33 41    | 7  4  2  4  8 |                        |0 1 3 1 1 |1 3 1 1
			#      4 20 26 28 35 40    |16  6  2  7  5 |-12  -3  -1  -1   2  -1 |1 0 3 1 1 |1 3 1 1
			#     12 15 19 26 40 43(2) | 3  4  7 14  3 |  8  -5  -7  -2   5   3 |0 3 1 0 2 |3 1 2
			#      5  6 11 17 38 44    | 1  5  6 21  6 | -7  -9  -8  -9  -2   1 |2 2 0 1 1 |2 2 1 1
			#      2  5 15 18 19 23(1) | 3 10  3  1  4 | -3  -1   4   1 -19 -21 |2 3 1 0 0 |2 3 1
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  4   5   3   7  15  12 |1 2 1 2 0 |1 2 1 2
			if( 2<=sum(c( 6,10,18,25,34,35)[c(1,4)]==aZoid[c(2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(c( 6,10,18,25,34,35)[c(3,6)]==aZoid[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_3
			#     17 20 30 31 33 45    | 3 10  1  2 12 |                        |0 1 1 3 1 |1 1 3 1
			#      1  2 15 19 24 36    | 1 13  4  5 12 |-16 -18 -15 -12  -9  -9 |2 2 1 1 0 |2 2 1 1
			#      1  3  8 12 42 43(1) | 2  5  4 30  1 |  0   1  -7  -7  18   7 |3 1 0 0 2 |3 1 2
			#      7  9 24 29 34 38    | 2 15  5  5  4 |  6   6  16  17  -8  -5 |2 0 2 2 0 |2 2 2
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  1   6  -3   2  -1   0 |1 1 1 3 0 |1 1 1 3
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 | -3  -4  -9  -2   0   6 |1 2 1 1 1 |1 2 1 1 1
			if( 1<sum( aZoid[ 2 ]*c(3,4)==aZoid[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(c( 5,11,12,29,33,44)[c(1,6,5)]==aZoid[c(1,6,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextColVal_4
			#     10 14 16 18 27 28    | 4  2  2  9  1 |                        |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 |  4   2   3  16   7  10 |1 2 0 3 0 |1 2 3
			#      1  2 16 22 38 39(1) | 1 14  6 16  1 | -4  -9   2  -8   5   1 |2 1 1 2 0 |2 1 1 2
			if( all(c( 1, 2,16,22,38,39)[c(4,5)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 1, 2,16,22,38,39)[c(3,4)]==aZoid[c(2,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			#	이런 패턴이 3번이나 반복되지는..?
			if( all(c( 1, 2,16,22,38,39)[c(2,3)]==aZoid[c(3,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 1, 2,16,22,38,39)[c(5,6)]==aZoid[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_5
			#      3 13 16 24 26 29    |10  3  8  2  3 |                        |1 2 3 0 0 |1 2 3
			#      1 28 35 41 43 44    |27  7  6  2  1 | -2  15  19  17  17  15 |1 0 1 1 3 |1 1 1 3
			#      2  8 33 35 37 41(2) | 6 25  2  2  4 |  1 -20  -2  -6  -6  -3 |2 0 0 3 1 |2 3 1
			#      4 10 14 15 18 22    | 6  4  1  3  4 |  2   2 -19 -20 -19 -19 |1 4 1 0 0 |1 4 1
			#      1  3 12 14 16 43(1) | 2  9  2  2 27 | -3  -7  -2  -1  -2  21 |2 3 0 0 1 |2 3 1
			#      3  9 11 12 13 19(2) | 6  2  1  1  6 |  2   6  -1  -2  -3 -24 |2 4 0 0 0 |2 4
			if( 1<sum( aZoid[ 1 ]*c(3,4)==aZoid[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(c( 3, 9,11,12,13,19)[c(3,4)]==aZoid[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			if( all(c( 3, 9,11,12,13,19)[c(2,3)]==aZoid[c(1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

		# fCutCnt.nextColVal_6
			#      2  6  7 12 19 45    | 4  1  5  7 26 |                        |3 2 0 0 1 |3 2 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  1   3   5   1   6  -2 |2 2 1 0 1 |2 2 1 1
			#      4  5 31 35 43 45(1) | 1 26  4  8  2 |  1  -4  19  22  18   2 |2 0 0 2 2 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   9 -12  -9  -3  -2 |0 3 1 0 2 |3 1 2
			#      5 13 17 29 34 39(1) | 8  4 12  5  5 | -8  -1  -2   3  -6  -4 |1 2 1 2 0 |1 2 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 11   7   7  -1   2   0 |0 1 3 2 0 |1 3 2
			if( 2<=sum(c(16,20,24,28,36,39)[c(4,6,5,1,6)]==aZoid[c(3,5,6,2,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

			# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }
			# if( all(aRem[c(,)]==aRem[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 2<=sum(c(,,,,,)[c(,,)]==aZoid[c(,,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			# if( all(c(,,,,,)[c(,)]==aZoid[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2

			# if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
			# ptnLst <- list( c(,) ,c(,) )
			# if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }
			# if( 2<=sum(c(,,,,,)[c(,,)]==aZoid[c(,,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			# if( all(c(,,,,,)[c(,)]==aZoid[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind 2
			# if( all(aRem[c(,)]==aRem[c(,)]) ){	surviveFlg[idx]<-FALSE	;next }

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
		aRem <- aZoid %% 10

		# anaColEndPtn()
		if( 1<sum(aZoid==c(  7,24,25,32,34,39 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  8,14, 6,29,18,28 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 12,19,13,NA,38,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 11,17,NA,NA,12,26 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 17,27,NA,NA,34,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 18,15,NA,NA,43,30 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 18,15,NA,NA,45,43 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=2 )
		if( all(aZoid[c(3,5)]==c(33,36)) ){	surviveFlg[idx]<-FALSE	;next }

		# cnt <- 0
		# 	if( aZoid[1]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[2]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[3]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[4]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[5]%in%c(  ) ) cnt<-cnt+1
		# 	if( aZoid[6]%in%c(  ) ) cnt<-cnt+1
		# if( 4<cnt ) {	surviveFlg[idx]<-FALSE	;next }

		# if( aRem[4]==c(6) ){	surviveFlg[idx]<-FALSE	;next }		# if( fCutU.remFilt(aZoid[4],c( 6,8,9  ),c( 16 )) )

		# colValSeqNext( ,pColSize=2 )
		#	3개 이상 일치.
		score <- sum(aCStep==c(  3, 2, 8, 1, 1 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  2,11,15, 1, 1 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 13,12, 9, 2, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  1, 4, 6, 6, 2 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 16,14,NA,NA,12 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 15, 2,NA,NA,11 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  7,12,NA,NA,15 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c( 11, 4,NA,NA,12 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		score <- sum(aCStep==c(  1,10,NA,NA, 5 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }

		if( 1<sum(aCStep[1:2+3]==c(  1, 1 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aCStep[1:2+3]==c(  2, 2 )) ){	surviveFlg[idx]<-FALSE	;next }

		if( 1<sum( aCStep[c(2,4)]*c(4,1)==aCStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }


		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		#        *  *  *     *  *  *     *  *  *     *  *  *
		# [  1] 13 16 27                            17 23 38
		if( all(aZoid[1:3+0]==c( 13, 16, 27 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+3]==c( 17, 23, 38 )) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvColValSeqNext()

# UNdone		fCut.basic() 사용
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

		# anaColEndPtn()
		if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(1,3)]*c(4,4)==aCStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
		if( (aCStep[5]==1)&&(aCStep[3]==aCStep[4]) ){	surviveFlg[idx]<-FALSE	;next }
		# [  1]                          7  7  1
		# [  2]                         12 12  1

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

		#	a,b,b.. a?	basic nextQuo10 nextRebNum nextColVal_1 nextColVal_6
			if( 2<sum(aCStep[c( 2,5,3,2,2 )]==c( 14, 1, 2, 2, 5 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
			if( 2<sum(aCStep[c( 2,5,3,2,2 )]==c(  4, 3, 4, 1, 4 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,a,b.. b?	nextZW nextFStepBin
			if( 2<sum(aCStep[c( 2,1,3,4 )]==c(  5,20,10, 4 )) ){	surviveFlg[idx]<-FALSE	;next }
		#	q,a,b,a.. q? nextZW nextColVal_5 nextColVal_6
			if( 2<sum(aCStep[c( 2,1,3,5 )]==c(  2, 6, 2, 2 )) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.basic()
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9, 2, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA, 2,22 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA,11, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,NA,20, 3, 5 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 4,17, 5 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14, 6,12 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 2,NA,15 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 2,27,23 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 3, 9 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,NA,18, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 5, 1,29, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,26, 5,17 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA, 8,29, 3 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
			ptnLst <- list( c(5,3) ,c(3,7) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[1:3]==c(3,8,6)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 5, 7, 2 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 3, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 4,24, 3 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,13, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,18, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,11,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			#	unique aCStep[c(2,5)]==c( 6,11)	        (6,11)  (6,NA,11) ... ?
			if( all(aCStep[c(2,5)]==c( 6,11)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA,11, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,15, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 25,NA, 3, 5, 9 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 4,NA,21 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 6,NA,21 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 7, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 1, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 1,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 9, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 7, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( 1<sum( aCStep[ 2 ]*c(4,2)==aCStep[c(1,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(2,4)]*c(2,5)==aCStep[c(3,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,NA,11, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 2, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3,NA,15 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 24, 4, 7,20 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA, 6,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( all(aCStep[c(3:4)]==c( 3,7)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(3:4)]==c(10, 4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( all(aCStep[c(1,3,4)]==c(20,10, 4)) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,13, 3, 7, 4 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,13, 3, 7, 4 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,17, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 17,11, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
			if( fCutU.hasPtn(c(  1,15, 9,10 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21, 1,15, 6 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21, 1,12 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13, 2,22, 2 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13, 2,22, 2 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			#	unique	( 4, 8,-->7) (4,2,6) (4,7,5)
			if( fCutU.hasPtn(c( 4,NA, 8, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 26, 6, 8 )
			if( fCutU.hasPtn(c( 26, 6, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 1:13, 4,15, 1) ( 2:25, 8, 2*,10, 2) ( 3: 3,16,NA,NA, 5) ( 7: 5,12, 7, 4)
			if( fCutU.hasPtn(c( 13, 4,15, 1 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 25, 8, 2,10, 2 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,16,NA,NA, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,12, 7, 4 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
			#	unique aCStep[4]!= 17  (5,4|5)가 2번이나 존재. 
			if( all(aCStep[4]==c(17)) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 4: 8, 5, 4) ( 5:10, 5,NA, 5*) (10: 9,10, 3, 8) (12:16, 7, 8,12)
			if( fCutU.hasPtn(c(  8, 5, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 5,NA, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,10, 3, 8 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 16, 7, 8,12 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#	unique ( 6, 1 | 6,a,b-->6,b ) (  8,3 | 8,a,b->8,b) ( 14,16|a,b,16-->a,16)
			ptnLst <- list( c( 6, 1) ,c( 8, 3) ,c(14,16) )
			if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 1: 1,11, 8,27) ( 2:11, 2, 1) ( 3:10, 3,20, 4) ( 4: 4, 8, 6) ( 5: 3,NA,27, 4, 5) ...
			if( fCutU.hasPtn(c(  1,11, 8,27 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 3,20, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 8, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,NA,27, 4, 5 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,NA, 4,19 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 22, 9,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 1: 6,NA, 1,NA, 8) (621: 6, 2, 1) ( 4: 6,NA,NA, 4, 4) ()
			if( fCutU.hasPtn(c(  6,NA, 1,NA, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 2, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,NA, 4, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( 1<sum( aCStep[ 1 ]*c(1,1,2)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique  ( 3, 5, 3 | 5,7,a,3 --> 4,8,3) 내가 뭘 잘못봤나...
			#	unique	aCStep[1:3] (  5, 7,14 )   --- 1,5,7
			if( all(aCStep[1:3]==c( 5, 7,14)) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 7: 1, 5, 7, 2) ( 1: 1,NA,10,20, 4) ( 3: 7, 3, 1, 2, 3*) (4-8: 7,NA, 4, 8) (4-8: 4, 8, 4) ...
			if( fCutU.hasPtn(c(  1, 5, 7, 2 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,NA,10,20, 4 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 3, 1, 2, 3 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,NA, 4, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 8, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA,11, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA, 3,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 4, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23, 5, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

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

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			#	unique	(-4:-5,16,-4,13) (-3:-26,-16, 1,NA, -3) ( 0:18, 3,13,-9, 8, 0) ( 4&1: 4,20, 1, 6,-11, 0) ...
			if( fCutU.hasPtn(c( -5,16,-4,13 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -26,-16, 1,NA, -3 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18, 3,13,-9, 8, 0 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,20, 1, 6,-11, 0 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,20, 1, 6,-11, 0 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -6, 2,-1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,13, 7, 2, 4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,13, 7, 2, 4 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21, 7,18,-2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -18,-22,-13, 8, -9 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(6,1)]*c( 1,-7)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-3:-3,-2,NA,-5,-12) (-2: 2, 2,-3, 6,-2) ( 0: 5,NA, 0,-3) ...
			if( fCutU.hasPtn(c( -3,-2,NA,-5,-12 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 2,-3, 6,-2 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,NA, 0,-3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -9, 1,NA,-10, 4 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-7, 1,NA, 2,-3 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,16,-4, 5 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			#	unique fCutU.hasPtn(c(5,7,5,7),aFStep)
			if( fCutU.hasPtn(c(5,7,5,7),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 6 ]*c(6,1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-12:-12, -5,-9) (-9:-6,-9,-17, 6, 2) ( -2:-11,-18,-20,-2*,-2*) (-2:25,-14,-14,-22, -2,-8) (-2:-15,-24, 0,-2)
			if( fCutU.hasPtn(c( -12, -5,-9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -6,-9,-17, 6, 2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11,-18,-20,-2,-2 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11,-18,-20,-2,-2 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 25,-14,-14,-22, -2,-8 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -15,-24, 0,-2 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-4,-6)==aFStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 4 ]*c( 4, 3)==aFStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(6,4)]*c(-4,3)==aFStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-12:-16,-12, -8) (-8:-8,-13,-2,-6,-6, 6) (-8:-8,-24, 2, -4) ( 0: -4, 9, 4, 0*, 0)
			if( fCutU.hasPtn(c( -16,-12, -8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -8,-13,-2,-6,-6, 6 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -8,-24, 2, -4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -4, 9, 4, 0, 0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -2,-11,-18 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,-23,-8,-2,-6,-10 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,-10,-8, -2 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			#	unique (15,NA,a,a)
			fcIdx <- which(aFStep==15)
			if( 0<length(fcIdx) && fcIdx[1]<4 ){
				if( aFStep[fcIdx+2]==aFStep[fcIdx+3] ){	surviveFlg[idx]<-FALSE	;next }
			}
			#	unique	(-5: 3,-5*,-8,-5,-7) (-5:11,-5,-10) (-5:-15,-5,-6,-9) ( 0:-17,21,NA,NA,19, 0)
			if( fCutU.hasPtn(c(   3,-5,-8,-5,-7 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  11,-5,-10 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -15,-5,-6,-9 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -17,21,NA,NA,19, 0 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15,-10,-16,-16,-12 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,17,18, 8,-6 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			#	unique	( -6:-17,-14,-14, -6) (-2: 8,14,11,-2,16) (-1:-4,-14,-1,-10, 5) ( 0:11,-11, -6, -3, 0,-15)
			if( fCutU.hasPtn(c( -17,-14,-14, -6 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,14,11,-2,16 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4,-14,-1,-10, 5 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,-11, -6, -3, 0,-15 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,16,-3,18 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 19, 6,13 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			#	aFStep[3:5] (a,a,a)
			if( all(aFStep[3]*c(1,1)==aFStep[4:5]) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-4: 2,-6,-5,NA,NA,-4) ( 0:-5, 0,-9,NA,-13) ( 3: 3, 6,NA,NA, 9,-7) ( 3: 3,14,NA,NA,-7)
			if( fCutU.hasPtn(c(  2,-6,-5,NA,NA,-4 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5, 0,-9,NA,-13 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 6,NA,NA, 9,-7 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,14,NA,NA,-7 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -22, 5,-3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -18, 6, 5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-8,-8)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,2)]*c(-3, 1)==aFStep[c(4,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-4:-4,-8, 0) ( -1: 0, 7, -1,NA, 7) (-1:-7,-8,-1) ( 4:-7, 4,-3) ( 6: 5, 6, 8)
			if( fCutU.hasPtn(c( -4,-8, 0 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  0, 7, -1,NA, 7 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7,-8,-1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7, 4,-3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 6, 8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			cnt <- 0
				if( 1<sum( aFStep[ 3 ]*c(5,4)==aFStep[c(5,6)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,2)]*c(3,3)==aFStep[c(6,5)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(2,3)]*c(3,4)==aFStep[c(5,6)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,3)]*c(3,5)==aFStep[c(6,5)] ) ) cnt <- cnt+1
			if( 1<cnt ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-3:-3,-1, 9, 0) (-2:-26,-11,-11, -2,-3) (-1:-5,-1, 6, 3) ( 1: 2, 7,10, 1) ( 3:10, 5, 3)
			if( fCutU.hasPtn(c(  -3,-1, 9, 0 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -26,-11,-11, -2,-3 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -5,-1, 6, 3 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   2, 7,10, 1 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  10, 5, 3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( 1<sum( aFStep[ 4 ]*c( 2,-3)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c( 3, 2)==aFStep[c(3,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c( 3,-3)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,4)]*c(-2, 2)==aFStep[c(6,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( -9:11, 4,-9, 5) (-9: 6, 1, -9) ( -3:-3,-10,-17, -4) ( 0:-12,-5,-20,-3, 0)
			if( fCutU.hasPtn(c(  11, 4,-9, 5 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   6, 1, -9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -3,-10,-17, -4 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -12,-5,-20,-3, 0 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   1,NA, 1,NA,-9 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 3 ]*c(-2,-4)==aFStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	( 2: 3, 2, 3) ( 2:-22, 2,-19,-6,-5) ( 4: 4,-1, 4) ( 5:-23,-9,-20, 5, 0) (13:12,13,-7,-6,-10)
			if( fCutU.hasPtn(c(   3, 2, 3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -22, 2,-19,-6,-5 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(   4,-1, 4 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -23,-9,-20, 5, 0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  12,13,-7,-6,-10 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			cnt <- 0
				if( 1<sum( aFStep[ 1 ]*c(   3, -1,-12)==aFStep[c(2,4,6)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[ 5 ]*c(  -2,  8    )==aFStep[c(2,6)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,5)]*c(-1,-2)==aFStep[c(4,2)] ) ) cnt <- cnt+1
				if( 1<sum( aFStep[c(1,5)]*c( 3, 8)==aFStep[c(2,6)] ) ) cnt <- cnt+1
			if( 1<cnt ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	aFStep[3:5] c( 0,-3,-1 )
			if( all(aFStep[3:5]==c( 0,-3,-1 )) ){	surviveFlg[idx]<-FALSE	;next }
			#	unique	(-1&-2:11,14,-1,-2) (-2:15, 5,-2,-5) ( 2: 2,10,17,16,13) ( 2: 2,NA,18,15,13)
			if( fCutU.hasPtn(c( 11,14,-1,-2 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,14,-1,-2 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 5,-2,-5 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,10,17,16,13 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,NA,18,15,13 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			#	unique	(-2:-7, 1,-2) (-1:-1, 6,-3) ( 1: 1,-7,NA,NA,NA, 6) ( 2:-5,-8,-20, 2)
			if( fCutU.hasPtn(c( -7, 1,-2 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 6,-3 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-7,NA,NA,NA, 6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5,-8,-20, 2 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5, 3,-17,-9 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		#	if( fCutU.hasPtn(c( ,, ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

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
			if( fCutU.hasPtn(c( 16,23,23,20,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,26,NA,NA,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 19,NA,33,35 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  5,11,22,23,26,42 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,20,31,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextZW
			if( fCutU.hasPtn(c( 11,20,NA,NA,NA,36 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,NA,27,40,40 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 15,16,30,31,42,33 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,40,45,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,13,24 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 21,NA,21,33,34 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,35,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  6,17,36,39 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextQuo10
			if( fCutU.hasPtn(c(  9,36,28,29,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,12,NA,NA,19 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 13,25,28 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,29,32,42 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 25,38,26,35,43 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextBin
			if( fCutU.hasPtn(c(  1,NA,26,30,34,37 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,NA,13,30,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1,NA,26,30,34,37 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,28,33 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,16,34 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1, 8,28,42 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,18,26,41,45 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextRebNum
			if( fCutU.hasPtn(c(  2, 8,11,28,31,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,NA,13,14,25 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 13,NA,14,27 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,18,19,31 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 24,24,NA,25,38,35 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,18,21,21,28,35 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 21,19,31,27,NA,36 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextCStepBin
			if( fCutU.hasPtn(c( 11,18,21 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,17,23,44 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,17,23,44 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,19,NA,24 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,18,29 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextFStepBin
			if( fCutU.hasPtn(c(  3, 9,15,11,39,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,11,15,18 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,NA,18,13,32 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,11,15,18 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,22,20,35 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18,18,31,NA,37 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12, 4, 4,37,38 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_1
			if( fCutU.hasPtn(c(  6,NA,20,43 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9,36,NA,43,33 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  2, 8,12 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,NA,29,35,38 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7,NA,29,35,38 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,32,42 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  9, 7,24,NA,NA,43 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_2
			if( fCutU.hasPtn(c( 13,35,NA,45 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,29,NA,NA,45 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 19,20,37,44 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 24,25,21,37,41 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 17,34,31,38 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,23,41 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  8,23,NA,43 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4,18,29,NA,NA,45 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_3
			if( fCutU.hasPtn(c(  7,23,26,NA,45 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,18,20 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 29,31,38 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 37,25,26,39 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_4
			if( fCutU.hasPtn(c(  1,11,14,34,38 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  1,11,14,34,38 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 16,32,39 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,25,33,33 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  4, 6,10,25,28 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 12,35,33,38 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_5
			if( fCutU.hasPtn(c(  2,NA,NA,36,39,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,10,12,36,32,38 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,14,16,NA,16 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,14,16 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 10,13,26,30,34 ),aZoid,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 18, 9, 7,10,27 ),aZoid,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  3,10,12,36,32,38 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

		#	fCutCnt.nextColVal_6
			if( fCutU.hasPtn(c(  4,13,NA,37 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c( 17, 7,23,29 ),aZoid,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }	# < >
			if( fCutU.hasPtn(c(  7, 7,22,NA,NA,43 ),aZoid,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }	# < >

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

		# if( all(quoSize==c( ,,, )) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#     16 20 24 28 36 39(1) | 4  4  4  8  3 | 15   4  -5  -5  -4  -6 |0 1 3 2 0 |1 3 2
			#	unique (0 1 1 3 1) 중복 발생 (0 1 3 2 0)도 중복발생 가능할까?
			if( all(quoSize==c(0,1,3,2,0)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			#      7  9 15 26 27 42    | 2  6 11  1 15 |                        |2 1 2 0 1 |2 1 2 1
			#      4  8 19 25 27 45(1) | 4 11  6  2 18 | -3  -1   4  -1   0   3 |2 1 2 0 1 |2 1 2 1
			#      5 20 21 24 33 40    |15  1  3  9  7 |  1  12   2  -1   6  -5 |1 0 3 1 1 |1 3 1 1
			#      9 14 20 22 33 34(2) | 5  6  2 11  1 |  4  -6  -1  -2   0  -6 |1 1 2 2 0 |1 1 2 2
			#     18 20 24 27 31 42(1) | 2  4  3  4 11 |  9   6   4   5  -2   8 |0 1 3 1 1 |1 3 1 1
			#      1  2 15 19 24 36(1) | 1 13  4  5 12 |-17 -18  -9  -8  -7  -6 |2 2 1 1 0 |2 2 1 1
			#	unique (2 1 2 0 1) 중복 발생, (2 2 1 1 0)도 중복?
			if( all(quoSize==c(2,2,1,1,0)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.


			if( all(quoSize==c(1,1,1,1,2)) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	table( sapply( dbgLst,function(str){str}) )
	allIdxF <- allIdxF[surviveFlg]

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10


	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( NA )","nextColVal_1( 32 )","nextColVal_5(  )","nextFStepBin(  )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 32 ) )]

	# unique	nextBin
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 35 ) )]

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






