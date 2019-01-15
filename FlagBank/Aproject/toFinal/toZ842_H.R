# toZ842_H.R 최종접근
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
	allIdxF <- rmvCStep( gEnv ,allIdxF )
	allIdxF <- rmvFStep( gEnv ,allIdxF )
	allIdxF <- rmvFV3( gEnv ,allIdxF )
	allIdxF <- rmvQuo10( gEnv ,allIdxF )
	allIdxF <- rmvZW( gEnv ,allIdxF )

	# // \\ 재현

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

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,3)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+1]==c(4,0,0)) ) return(FALSE)	# next rebind of 2,0,3
					if( all(quoSize[1:3+2]==c(0,0,0)) ) return(FALSE)	# next rebind of 0,3,0
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+2]==c(0,4,2)) ) return(FALSE)	# next rebind of 2,0,1
					#	unique : 1 2 0 3 0   ... 별 거 없다.
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
			if( aZoid[1]%in%c( 11,12    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  9       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 11,14,31 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    11,NA,31    ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c(    14,NA,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,14,43,NA,40 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <28>
			# <30>
			if( fCutU.hasPtn(c(          30,33    ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(    14,NA,33    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,NA,33,40 ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,2        ),c( 11,12    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4,9,8      ),c(  9       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,4,0      ),c( 11,14,31 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,3        ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 9,6        ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0,3        ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  5       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  2       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 15,18, 4 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 14       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  3,16 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  3,16 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,12 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 16, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,16 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  2, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 16, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c(  1, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : (3: 2, 3,12) (3: 7, 3,16) (3: 3,16, 3) (3:16, 3, 8) (3: 5, 3,16) (6: 6, 4,NA, 5, 4)
			#	unique : (2: 6, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (6)   3 (7)   5 (2)   6 (3)   12 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  9 11 14 26 28    | 8  2  3 12  2 |                        |2 2 2 0 0 |2 2 2
			#      2 25 28 30 33 45(1) |23  3  2  3 12 |  1  16  17  16   7  17 |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  7     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 16,15 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 15,16 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -1,16 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique : (16:-1,16,15)
			# -------------------------------------------------------------------------------------
			# 	FV :    -11 (2)   -6 (2)   -5 (3)   3 (4)   7 (3)   16 (4)   17 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(3,4)]*c(1,2)==aFStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,3,4,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(2,4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 6

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
					# [1]    3 13 14 15  5 16 12  1  2  8  3
					# [2]    3  6 23 13  2  7  4  6  1
					# [3]    5 21  3 16
					# [4]*  13 13
					# [5]   
					# [6]*  38 38 28 24 42 43 37
					if( 1<sum(aZoid==c(  3, 3, 5,13,NA,38 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 13, 6,21,13,NA,38 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 14,23, 3,NA,NA,28 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 15,13,16,NA,NA,24 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  5, 2,NA,NA,NA,42 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 16, 7,NA,NA,NA,43 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 12, 4,NA,NA,NA,37 ) ,na.rm=T) ) cnt<-cnt+1
					# if( 1<sum(aZoid==c( NA,NA,NA,NA,NA,NA ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 6, 7 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(         ) ) score<-score+1
					if( aZoid[2]%in%c(         ) ) score<-score+1
					if( aZoid[3]%in%c(         ) ) score<-score+1
					if( aZoid[4]%in%c(  4      ) ) score<-score+1
					if( aZoid[5]%in%c(         ) ) score<-score+1
					if( aZoid[6]%in%c( 38,28   ) ) score<-score+1
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
					if( aZoid[4]%in%c(        ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 37     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(        ) ) cnt<-cnt+1

					# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					#	if( all(aZoid[1:2+ ]==c(    )) ) cnt<-cnt+1

					# [  1]  4  7    28 30     6 13    33 36    33 36
					# [  2] 16 25    19 21    10 16     8 33    22 30
					# [  3]  2 16     6 11             25 35    21 28
					# [  4] 29 33    16 19             22 35    40 41
					# [  5] 19 28     4  7             13 18    29 34
					# [  6]  5 10    26 37             28 33    28 37
					# [  7]  2  6    13 27             22 30    38 41
					# [  8]  2 20    10 19             36 37    27 34
					# [  9]  9 17    25 31                      39 43
					# [ 10]          23 34                           
					# [ 11]           1  2                           

					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(         ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 7,9,6,5 ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 0,1     ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 6       ),c(    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 7,4     ),c( 37 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 9       ),c(    )) )	remCnt <- remCnt+1

						# grp (1:2+0)
							if( aZoid[1]==2   && fCutU.remFilt(aZoid[2],c(6,0),c( )) ) remCnt <- remCnt+1
							#	if( aZoid[ ]==   && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1
						# grp (1:2+1)
						# grp (1:2+2)
						# grp (1:2+3)
							if( aZoid[5]==33   && fCutU.remFilt(aZoid[4],c(8),c(37)) ) remCnt <- remCnt+1
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

					score  <- sum(aCStep==c(  3, 2, 7, 3, 3 ),na.rm=T)
					matCnt <- sum(aCStep==c(  9, 2, 6,25, 8 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 14, 5,NA,10, 7 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  4, 3,NA,13, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  9,  3,NA, 5, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5,11,NA, 5, 9 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  4,14,NA, 8, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 18, 9,NA, 1, 7 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  8, 6,NA,NA, 4 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					#	matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)

					#	[1]  3   9  14   4   9   5   4  18   8 
					#	[2]  2   2   5   3   3  11  14   9   6  11   1 
					#	[3]  7   6 
					#	[4]  3  25  10  13   5   5   8   1 
					#	[5]  3   8   7   1   5   9   3   7   4 

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) cnt<-cnt+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 3 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(  2, 8 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(  9, 1 ),aCStep) ) cnt<-cnt+1

						if( aCStep[1]%in%c( 18       ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  2, 5, 1 ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(  8       ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

						if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
						if( sum(aCStep[c(2,3)])==sum(aCStep[c(1,4,5)]) )	cnt<-cnt+1	# 

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
					# [  1]  2 16 19                                    
					# [  2]  5 10 19                                    

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

					# [1]*  14 15 18  1  3  7  |  5 19  6  3  3  1  2  9  7  6  3 16 11  7 10  7 11  9  4 15 16  3  2 22 12  5 12
					# [2]    6  2 17 25  1  3  | 19  3  7  6 19 10  1  3  4  5  3 15  2  6  4  3 11
					# [3]    4 					| 
					# [4]*   5 10  8  1  2 15  |  3  6 11  3  4 15  3  2  5  5 13 10  2  8  1 14  6 16  9  6  2  8  5  9 10 17  2 12 17...
					# [5]    3 16 				| 

					tCnt <- 0
						if( aCStep[1]%in%c( 13      ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(         ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(         ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(         ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(         ) ) tCnt<-tCnt+1

						if( sum(aCStep[c(2,5)])==sum(aCStep[c(3,4)]) )	cnt<-cnt+1	# 9
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
					# [  1]  5  7     2  1     1  1    11  8
					# [  2]  1 10     5  1    11 19     1  8
					# [  3]  4  8     2 11     9  1     1 12
					# [  4]  9  5     7  2     6  2    12  4
					# [  5]  3  6    13  1     3  3     4  5
					# [  6] 10  4     6  3    22  2     6 21
					# [  7]  3  9                      15  1
					# [  8] 15  1                       4 14
					# [  9] 12  6                       6  3
					# [ 10]  1  8                       5 15

					if( aCStep[1]%in%c(        ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  7     ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(  1,11  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 19,11  ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  8,12  ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aCStep[1:2+0]==c(  1,10 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  5, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  5, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  9, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 15, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  9, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 11, 4 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c( 15, 5 )) ) cnt<-cnt+1

					#	if( all(aCStep[1:2+ ]==c(   ,   )) ) cnt<-cnt+1

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
					if( aCStep[1]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  2, 6 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(       ) ) cnt<-cnt+1

					# [  1]              2  1  1            
					# [  2]              2 11 19            
					# [  3]              6  3  3                        

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
			if( aZoid[1]%in%c(  1, 4 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 38    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 38    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 6, 5, 5,32 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 2>
			if( fCutU.hasPtn(c(  2,10          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,27,37 ),aZoid) ) cnt<-cnt+1
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,13    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,13,NA,32 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,25,14,18,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c(  2,10          ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(       19,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       19,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,14,19       ),aZoid) ) cnt<-cnt+1
			# <24>			# <28>
			# <38>
			if( fCutU.hasPtn(c( 23,26,38 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 42,45 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c( 42,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,4        ),c(   1, 4  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8,6        ),c(  38     )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 8,5        ),c(         )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,7,8,6    ),c(  38     )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0        ),c(         )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,9        ),c(  39     )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 19       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5,11, 2 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 17       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 13, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : (1: 1,11) (3: 3:24)
			#	unique : (6:11, 6, 4)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   3 (5)   4 (3)   6 (2)   7 (2)   8 (2)   11 (2)   12 (2)   13 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 15
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  2 15 19 24 36    | 1 13  4  5 12 |                        |2 2 1 1 0 |2 2 1 1
			#      4  7 11 24 42 45(1) | 3  4 13 18  3 |  3   5  -4   5  18   9 |2 1 1 0 2 |2 1 1 2
			#      6  7 18 19 30 38(1) | 1 11  1 11  8 |  2   0   7  -5 -12  -7 |2 2 0 2 0 |2 2 2
			#      1  4 10 12 28 45    | 3  6  2 16 17 | -5  -3  -8  -7  -2   7 |2 2 1 0 1 |2 2 1 1
			#      2 21 28 38 42 45(2) |19  7 10  4  3 |  1  17  18  26  14   0 |1 0 2 1 2 |1 2 1 2
			#      3 10 16 19 31 39    | 7  6  3 12  8 |  1 -11 -12 -19 -11  -6 |1 3 0 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  1, -5,  0 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -11 (2)   -7 (2)   -5 (2)   0 (2)   1 (2)   5 (2)   7 (2)   18 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(5,6)]*c(1,2)==aFStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# -18

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextZW()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 0,0,0
					if( all(quoSize[1:3+0]==c(1,0,0)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(0,0,2)) ) return(FALSE)	# next rebind of 2,1,2
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
			if( aZoid[1]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 31,12 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 43    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 42    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,10    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,10 ),aZoid) ) cnt<-cnt+1
			# < 8>
			if( fCutU.hasPtn(c(  2, 8 ),aZoid) ) cnt<-cnt+1
			# < 9>
			# <10>
			if( fCutU.hasPtn(c(  3,10    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,10 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,14,23,37,28 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,NA,NA,19 ),aZoid) ) cnt<-cnt+1
			# <12>
			# <19>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,19 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,NA,NA,19 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(           8, 9,19 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 26,31 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 42,43 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 42,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,6,0   ),c(  6    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(  8    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,1     ),c(       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,2     ),c( 31,12 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,3     ),c( 43    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2       ),c( 42    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  8       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 3, 6 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  2, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (8: 6, 8,11)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (4)   3 (2)   4 (4)   5 (2)   6 (2)   7 (2)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(1,1)==aCStep[c(3,1)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     10 28 31 33 41 44    |18  3  2  8  3 |                        |0 1 1 2 2 |1 1 2 2
			#      4  8  9 16 17 19    | 4  1  7  1  2 | -6 -20 -22 -17 -24 -25 |3 3 0 0 0 |3 3
			#     10 22 27 31 42 43    |12  5  4 11  1 |  6  14  18  15  25  24 |0 1 2 1 2 |1 2 1 2
			#      1  3  8 12 42 43(2) | 2  5  4 30  1 | -9 -19 -19 -19   0   0 |3 1 0 0 2 |3 1 2
			#      6 10 18 25 34 35    | 4  8  7  9  1 |  5   7  10  13  -8  -8 |1 2 1 2 0 |1 2 1 2
			#      3  9 11 12 13 19    | 6  2  1  1  6 | -3  -1  -7 -13 -21 -16 |2 4 0 0 0 |2 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(   4     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -24, 12 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique : (-19:-19,-19)
			# -------------------------------------------------------------------------------------
			#     FV :    -19 (3)   -8 (2)   0 (2) 
			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1	# -29
			if( sum(aFStep[c(3,4)])==sum(aFStep[c(1,2,6)]) )	cnt.w2<-cnt.w2+1	# -20

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextQuo10()

# done
fCutCnt.nextBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")

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
			if( aZoid[3]%in%c( 20      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 12      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 35      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3, 4, 8,14,28 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,25,23 ),aZoid) ) cnt<-cnt+1
			# <11>
			# <15>
			if( fCutU.hasPtn(c( 13,15 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  5, 6,18,31 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 15,19,NA,NA,32 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c( 33,35 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(         ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,0        ),c( 20 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2        ),c( 12 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,3        ),c( 33 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1        ),c( 35 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  2      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 1   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  9, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  2, 2 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  5,20 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  9, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3,10 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : ( 1:20,1) ( 2: 2, 2)
			#	unique : ( 3:13, 3,10) ( 3: 3,10,4) ( 4: 4, 5,NA,NA, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (5)   3 (4)   4 (4)   5 (3)   9 (3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# 10

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 15 20 26 31 35    | 4  5  6  5  4 |                        |0 2 2 2 0 |2 2 2
			#      1  3 16 18 30 34    | 2 13  2 12  4 |-10 -12  -4  -8  -1  -1 |2 2 0 2 0 |2 2 2
			#      2 11 12 15 23 37    | 9  1  3  8 14 |  1   8  -4  -3  -7   3 |1 3 1 1 0 |1 3 1 1
			#      5  7  9 11 32 35(1) | 2  2  2 21  3 |  3  -4  -3  -4   9  -2 |3 1 0 2 0 |3 1 2
			#     10 14 19 39 40 43    | 4  5 20  1  3 |  5   7  10  28   8   8 |0 3 0 1 2 |3 1 2
			#      3 10 13 22 31 32(1) | 7  3  9  9  1 | -7  -4  -6 -17  -9 -11 |1 2 1 2 0 |1 2 1 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( -7     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  6, 8  ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -4,-3 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( -6,-17 )) )	cnt.w1<-cnt.w1+1
			#	unique : (-4:-4,-6)
			#	unique : (-3:-2,-4,-3,-1,15)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -4 (5)   -3 (2)   -1 (2)   3 (2)   8 (3) 
			cnt.w2 <- 0
			if( aFStep[4]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(2,6)]) )	cnt.w2<-cnt.w2+1	# -15
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1	# -13
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,5,6)]) )	cnt.w2<-cnt.w2+1	# -24

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+2]==c(2,0,0)) ) return(FALSE)	# next rebind of 0,2,2
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
			if( aZoid[1]%in%c(  6, 5, 4 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 31       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 29       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,43    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,NA,17,21 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if(fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40  ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,21       ),aZoid) ) cnt<-cnt+1
			# <21>
			if( fCutU.hasPtn(c( 11,NA,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    16,21       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,35 ),aZoid) ) cnt<-cnt+1
			# <31>
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 23,24,31,36 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <43>
			# <45>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 6,5,4,1,9 ),c(  6, 5, 4 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,5,3     ),c(          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1         ),c( 31       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,5,3     ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,1       ),c( 29       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,1,3,7   ),c( 45,43    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 16          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3,17, 5, 4 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 16, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : ( 3: 5, 3,17 ) (16:16, 4, 2)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (2)   2 (2)   3 (4)   4 (2)   5 (4)   6 (4)   8 (4)   10 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 18 21 36 37 43    | 7  3 15  1  6 |                        |0 2 1 2 1 |2 1 2 1
			#      7 15 20 25 33 43(1) | 8  5  5  8 10 | -4  -3  -1 -11  -4   0 |1 1 2 1 1 |1 1 2 1 1
			#     11 17 21 26 36 45    | 6  4  5 10  9 |  4   2   1   1   3   2 |0 2 2 1 1 |2 2 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( 14     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -10, -6 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (-10: -3,-10,-6)
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -7 (2)   -5 (2)   -4 (2)   0 (2)   1 (3)   2 (2)   3 (2)   6 (3) 
			cnt.w2 <- 0
			if( aFStep[5]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,3,4)])==sum(aFStep[c(1,5,6)]) )	cnt.w2<-cnt.w2+1	# -16

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextRebNum()

# done
fCutCnt.nextCStepBin <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")


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
			# <12>
			# <24>
			# <32>
			if( fCutU.hasPtn(c( 12, 7,19,NA,32 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 36,37 ),aZoid) ) cnt<-cnt+1
			# <37>
			if( fCutU.hasPtn(c( 36,37 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,4     ),c(  )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,6     ),c(  )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,4     ),c(  )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(  )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2       ),c(  )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 0,4     ),c(  )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7, 1,18 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 7    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(13,18)) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  4, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 2, 1 )) )	cnt.w1<-cnt.w1+1
			#	unique ( 1: 1, 7, 4)
			#	unique ( 3: 3, 1)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   3 (5)   4 (6)   6 (2)   9 (2)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,2)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,5)])==sum(aCStep[c(2,3,4)]) )	cnt.w2<-cnt.w2+1	# 9

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 19 25 28 32 36    |11  6  3  4  4 |                        |1 1 2 2 0 |1 1 2 2
			#      1 12 29 34 36 37(1) |11 17  5  2  1 | -7  -7   4   6   4   1 |1 1 1 3 0 |1 1 1 3
			#      3 10 13 22 31 32    | 7  3  9  9  1 |  2  -2 -16 -12  -5  -5 |1 2 1 2 0 |1 2 1 2
			#      4  7 11 24 42 45    | 3  4 13 18  3 |  1  -3  -2   2  11  13 |2 1 1 0 2 |2 1 1 2
			#      6 21 35 36 37 41    |15 14  1  1  4 |  2  14  24  12  -5  -4 |1 0 1 3 1 |1 1 3 1
			#     12 15 16 20 24 30    | 3  1  4  4  6 |  6  -6 -19 -16 -13 -11 |0 3 2 1 0 |3 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  2     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -5     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -3     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -16,-14 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  -5, -3 ),aFStep) )	cnt.w1<-cnt.w1+1
			#	unique ( -5:  2,NA,NA,NA, -5, -3)
			# -------------------------------------------------------------------------------------
			#     FV :    -16 (2)   -7 (2)   -5 (3)   -2 (2)   1 (2)   2 (3)   4 (2)   6 (2) 
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextCStepBin()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(3,1,0)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(0,1,2)) ) return(FALSE)	# next rebind of 1,1,0
					if( all(quoSize[1:3+2]==c(0,1,2)) ) return(FALSE)	# next rebind of 2,1,0
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
			if( aZoid[1]%in%c(  3       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 17,16, 5 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 41,38    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,17,33 ),aZoid) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5,NA, 7,27 ),aZoid) ) cnt<-cnt+1
			# < 6>
			# <10>
			if( fCutU.hasPtn(c( 10,16 ),aZoid) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c( 13,20,29,24,31 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <14>
			# <16>
			if( fCutU.hasPtn(c( 10,16 ),aZoid) ) cnt<-cnt+1
			# <17>
			if( fCutU.hasPtn(c( 12,17 ),aZoid) ) cnt<-cnt+1
			# <28>
			if( fCutU.hasPtn(c( 12,NA,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       22,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          28,35 ),aZoid) ) cnt<-cnt+1
			# <37>
			# <41>
			if( fCutU.hasPtn(c(       15,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1, 6,NA,NA,41 ),aZoid) ) cnt<-cnt+1

			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3        ),c(  3       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,4      ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,6,5    ),c( 17,16, 5 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 6,1,4    ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 5,2      ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 1,8,2    ),c( 41,38    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5, 7    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  6,15    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  1, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 8 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique ( 1: 1, 9,20, 2) ( 3: 1, 3,NA,31, 5) ( 4:14, 4, 3) ( 8: 8*, 6, 8, 1)
			#	unique ( 1: 1, 6)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   3 (3)   4 (3)   6 (5)   8 (4)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 10 16 26 33 41    | 6  6 10  7  8 |                        |1 2 1 1 1 |1 2 1 1 1
			#     10 14 22 24 28 37(1) | 4  8  2  4  9 |  6   4   6  -2  -5  -4 |0 2 3 1 0 |2 3 1
			#     13 14 17 32 41 42(1) | 1  3 15  9  1 |  3   0  -5   8  13   5 |0 3 0 1 2 |3 1 2
			#      6  7 10 16 38 41(1) | 1  3  6 22  3 | -7  -7  -7 -16  -3  -1 |2 2 0 1 1 |2 2 1 1
			#      5 13 17 23 28 36    | 8  4  6  5  8 | -1   6   7   7 -10  -5 |1 2 2 1 0 |1 2 2 1
			#      4  5  6 12 25 37(1) | 1  1  6 13 12 | -1  -8 -11 -11  -3   1 |3 1 1 1 0 |3 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  -1, -7 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -12     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   6     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  13     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique : ( -7: -7, -7) 
			#	unique : (-3: 5, -9,-15, -6, -3, 3)
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -7 (3)   -5 (3)   -3 (2)   -1 (3)   6 (3)   7 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c(-1, 1)==aFStep[c(6,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

}	# fCutCnt.nextFStepBin( )

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )	# unique 3연중 37 ban...
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,3,0
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 3,0,2
					if( all(quoSize[1:3+2]==c(1,2,0)) ) return(FALSE)	# next rebind of 0,2,0
					if( all(quoSize[1:3+0]==c(3,0,2)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(2,0,3)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(1,2,0)) ) return(FALSE)	# next rebind of 1,2,0
					#	unique : 1 3 0 2 0 --> 1 2 1 2 0
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
			if( aZoid[1]%in%c(  2          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12, 7       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 13,11       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 31,36,30,37 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39,45       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2,10             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,17,25,29,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c(  2,10             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,NA,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,10,19,12,NA,42 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,32    ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(    11,19      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,11,NA,NA,33 ),aZoid) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c(       13,NA,30    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7, 4,13,32,NA,42 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(    11,19      ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,19,41,42 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  2,NA,NA,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             31,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    13,19,NA,31    ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 10,NA,NA,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    12,NA,32    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          32,36 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(          32,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6, 9,12,10,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(             31,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    13,19,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 2,4     ),c(  2          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,7,6   ),c( 12, 7       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,1     ),c( 13,11       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,1,5   ),c(             )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,6,0,7 ),c( 31,36,30,37 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,5,1   ),c( 39,45       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6, 8, 4 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 14       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  8       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			#	unique : ( 2: 9,18, 2, 1) ( 3: 3, 3,11 ) ( 3: 3, 3,NA,10) ( 3: 3, 4,NA,NA, 7) (16: 5, 4,16)
			# -------------------------------------------------------------------------------------
			#     FV :    2 (2)   3 (4)   4 (4)   6 (3)   8 (7)   9 (2)   16 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1	# 15
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 10 11 19 35 39    | 8  1  8 16  4 |                        |1 3 0 2 0 |1 3 2
			#      2 10 14 22 32 36(2) | 8  4  8 10  4 |  0   0   3   3  -3  -3 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -1   0  -1   4   0   0 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2

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
			if( fCutU.hasPtn(c(  0, 0 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+4]==c(  4, 9 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+1]==c( -1, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( -2, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (-1: -1, 6, 7,NA, 0, 0) (3,3:-2,3,3) (3,3:3,3,-17) (3,3:3,3,NA,3) (4: 3,12,-9, 4,18)
			# -------------------------------------------------------------------------------------
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -5 (2)   -4 (2)   -3 (2)   -1 (3)   0 (7)   3 (4)   4 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,5)]*c(1,1)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_1()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(3,0,0)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(0,0,3)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,3,0
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
			if( aZoid[1]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 14       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,31,39 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,11             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,18,NA,43 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,NA,21 ),aZoid) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c(  3,NA,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     4,13,37,23,35 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <14>
			# <21>
			if( fCutU.hasPtn(c( 10,NA,21 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA, 7,21 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(  4, 7,NA,31 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(  9,16,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(  5,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 11,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(          )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,5     ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4       ),c( 14       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,5     ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,1,9   ),c( 43,31,39 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  4       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  2,18    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 3    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  3, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 9 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5,12 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  9, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 12, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  4, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : ( 2:10,23, 2,14  ) ( 3: 4, 3, 1) ( 3: 8, 3,10)
			#	unique : a,9,a
			# -------------------------------------------------------------------------------------
			#     FV :    2 (7)   3 (4)   4 (3)   5 (2)   6 (2)   8 (2)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 4 ]*c(3,4)==aCStep[c(2,5)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c(2,4)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			# Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#  7  8 10 19 21 31    | 1  2  9  2 10 |                        |2 2 1 1 0 |2 2 1 1
			# 13 15 18 24 27 41    | 2  3  6  3 14 |  6   7   8   5   6  10 |0 3 2 0 1 |3 2 1
			#  5  9 14 26 30 43    | 4  5 12  4 13 | -8  -6  -4   2   3   2 |2 1 1 1 1 |2 1 1 1 1
			#  5 10 13 21 39 43(2) | 5  3  8 18  4 |  0   1  -1  -5   9   0 |1 2 1 1 1 |1 2 1 1 1
			#  1  3 12 14 16 43(1) | 2  9  2  2 27 | -4  -7  -1  -7 -23   0 |2 3 0 0 1 |2 3 1
			#  4  7 13 29 31 39    | 3  6 16  2  8 |  3   4   1  15  15  -4 |2 1 1 2 0 |2 1 1 2
		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -8     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  1     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -4, -8 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique ( -1 : -8,NA,-1,NA,NA, 0)
			# -------------------------------------------------------------------------------------
			#     FV :    -7 (2)   -4 (3)   -1 (2)   0 (3)   1 (2)   2 (2)   3 (2)   6 (2)   15 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,4)]*c(-1, 1)==aFStep[c(6,5)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c( 1,3 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# 19

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_2()

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 42 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,2
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
			if( aZoid[1]%in%c(  3    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 16    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 32    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45,36 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,14,15,24,30,30 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,11          ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c(  1,NA,11          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     4,11,41,NA,39 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c( 13,27       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,22,27 ),aZoid) ) cnt<-cnt+1
			# <26>
			if( fCutU.hasPtn(c(  4, 2,26,35 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(          34,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 18,23,NA,34    ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c(          34,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             42,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,27,33,NA,42    ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(             42,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 22,27,33,43,NA,45 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,1,3 ),c(  3    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 6,0,2 ),c(  6    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 3,6   ),c( 16    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,5   ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 2,6,3 ),c( 32    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,7,6 ),c( 45,36 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  9                ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 6, 5,18       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(                   ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(                   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3,27, 5, 4,12,10 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  9, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 10, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  8, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique ( 3:19, 9, 4) ( 5*6*3*: 5, 6,10,NA, 3) ( 8: 2,19, 8, 4) (17: 7,19,17)
			#	unique ( 5: 5, 5) ( 6:13, 6)
			# -------------------------------------------------------------------------------------
			#     FV :    3 (7)   4 (2)   5 (4)   6 (4)   8 (3)   17 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(1,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(2,4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  8 11 15 18 45    | 7  3  4  3 27 |                        |2 3 0 0 1 |2 3 1
			#      1  6 11 28 34 42(2) | 5  5 17  6  8 |  0  -2   0  13  16  -3 |2 1 1 1 1 |2 1 1 1 1
			#      3 13 16 24 26 29    |10  3  8  2  3 |  2   7   5  -4  -8 -13 |1 2 3 0 0 |1 2 3
			#      2  7 13 25 42 45(1) | 5  6 12 17  3 | -1  -6  -3   1  16  16 |2 1 1 0 2 |2 1 1 2
			#     12 17 23 34 42 45(2) | 5  6 11  8  3 | 10  10  10   9   0   0 |0 2 1 1 2 |2 1 1 2
			#      1 10 13 26 32 36    | 9  3 13  6  4 |-11  -7 -10  -8 -10  -9 |1 2 1 2 0 |1 2 1 2

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
			#	unique 
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   -8 (2)   -3 (2)   0 (4)   10 (3)   16 (3) 
			cnt.w2 <- 0
			if( sum(aFStep[c(2,3)])==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1	# -17
			if( sum(aFStep[c(2,5)])==sum(aFStep[c(4,6)]) )	cnt.w2<-cnt.w2+1	# -17
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(3,6)]) )	cnt.w2<-cnt.w2+1	# -19
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1	# -19
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1	# -18
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(5,4)]) )	cnt.w2<-cnt.w2+1	# -18

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..
} # fCutCnt.nextColVal_3()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,0,2)) ) return(FALSE)	# next rebind of 1,2,2
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,2,0
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
			if( aZoid[1]%in%c( 15 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,NA,26,30,33 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,15 ),aZoid) ) cnt<-cnt+1
			# <17>
			# <33>
			if( fCutU.hasPtn(c(    33,36    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 25,33,NA,36 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c(    33,36    ),aZoid) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 13,22,44 ),aZoid) ) cnt<-cnt+1
			# <45>
			if( fCutU.hasPtn(c(  1, 6,NA,45 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,4     ),c( 15 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 1,5,0   ),c(    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 2,7     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,7     ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,6     ),c( 45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  5,  1  ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 16, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  1,11 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  1, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique (11:11, 1) ( 2: 2, 4) ( 5,11, 5)
			#	unique ( 2: 4, 2) ( 3: 3, 4) ( 4: 4, 2)
			#	unique ( 4: 1, 4, 8)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (3)   2 (4)   3 (4)   4 (4)   5 (4)   10 (2)   11 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     20 30 33 35 36 44    |10  3  2  1  8 |                        |0 0 1 4 1 |1 4 1
			#     11 17 21 26 36 45(1) | 6  4  5 10  9 | -9 -13 -12  -9   0   1 |0 2 2 1 1 |2 2 1 1
			#      9 33 36 40 42 43(1) |24  3  4  2  1 | -2  16  15  14   6  -2 |1 0 0 2 3 |1 2 3
			#      5  7 11 16 41 45    | 2  4  5 25  4 | -4 -26 -25 -24  -1   2 |2 2 0 0 2 |2 2 2
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   5   2   8 -12  -1 |1 2 2 0 1 |1 2 2 1
			#      9 14 17 33 36 38    | 5  3 16  3  2 |  8   2   4   9   7  -6 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  8, 2,-24 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 10, 3  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  0, 6  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			#	unique ( 2:11, 2, 0)
			# -------------------------------------------------------------------------------------
			#     FV :    -12 (2)   -9 (2)   -4 (2)   -2 (2)   -1 (2)   2 (3)   8 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c( 4, 2,-3)==aFStep[c(1,3,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,2,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(3,4,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(2,4)])==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aFStep[c(1,2)])==sum(aFStep[c(4,5,6)]) )	cnt.w2<-cnt.w2+1	# 10

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_4()

# done
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
					if( (aZoid[6]-aZoid[1]) %in% c( 40 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,3,0)) ) return(FALSE)	# next rebind of 1,2,0
					if( all(quoSize[1:3+0]==c(2,0,3)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(3,0,2)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,2,0
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
			if( aZoid[1]%in%c(  5,15 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 12    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 28,33 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 13    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 37,45 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,28       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,NA,NA,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  5,13,NA,NA,27    ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  6, 7,NA,33,33,42 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,16,NA,27,29 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  5,NA,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,28       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,45 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <25>
			if( fCutU.hasPtn(c( 25,NA,36 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c(  3,NA,NA,29       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       14,29       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     3,NA,29,NA,34 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  7,16,NA,33 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          33,36 ),aZoid) ) cnt<-cnt+1
			# <36>
			if( fCutU.hasPtn(c( 25,NA,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          33,36 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2, 4, 9,NA,36 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(          32,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  9,17,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5,3,5    ),c(  5,15 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8        ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,4      ),c( 12    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 3,7,5    ),c( 28,33 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3,1      ),c( 13    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7,5      ),c( 37,45 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3, 4    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 16,17, 3 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4,16    ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(2)   3(6)   4(4)   5(2)   6(3)   8(3)   16(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c(4,5)]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -3  -2   7  -5 -10  -7 |2 1 2 1 0 |2 1 2 1
			#     13 16 24 25 33 36(2) | 3  8  1  8  3 | 11   9   5   0   4   0 |0 2 2 2 0 |2 2 2
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 | -8  -5 -12   4   0   8 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39(1) | 3  6 16  2  8 | -1  -4   1   0  -2  -5 |2 1 1 2 0 |2 1 1 2
			#      9 14 17 33 36 38    | 5  3 16  3  2 |  5   7   4   4   5  -1 |1 2 0 3 0 |1 2 3

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  0, 4  ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+ ]==c( , )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -5(3)   -2(2)   -1(2)   0(4)   4(4)   5(3)   7(2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(4,5)]*c(1,1)==aFStep[c(3,1)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,4,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,3,6)]) )	cnt.w2<-cnt.w2+1	# 10
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,4,6)]) )	cnt.w2<-cnt.w2+1	# 10

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)


	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

} # fCutCnt.nextColVal_5()

# done
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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,0)) ) return(FALSE)	# next rebind of 1,3,0
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
			if( aZoid[1]%in%c(  3,13   ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  8      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 11      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 12      ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 12      ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 40      ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < >
			if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1
			#   dup number  3:2   10:2   13:2   16:2   19:2   21:2   25:2   31:2   33:2   36:2
			# <  3>   0      3 , 10 , 16 , 19 , 31 , 39 
			#                3 ,  9 , 11 , 12 , 13 , 19 
			#          -->   3*,  8!,  6 ,  5 , NA , NA 
			# < 10>   0      6 , 10 , 18 , 25 , 34 , 35 
			#                3 , 10 , 16 , 19 , 31 , 39 
			#          -->  NA , 10*, 14 , 13 , 28 , 43 
			# < 13>   4     13 , 16 
			#               13 , 19 
			#          -->  13*, 22 
			# < 16>   1     13 , 16 , 24 , 25 , 33 
			#               10 , 16 , 19 , 31 , 39 
			#          -->   7 , 16*, NA , 37 , 45 
			# < 19>   2      3 , 10 , 16 , 19 
			#               11 , 12 , 13 , 19 
			#          -->  NA , 14 , 10 , 19*
			# < 21>   1      1 , 21 , 26 , 36 , 40 
			#               15 , 21 , 31 , 33 , 38 
			#          -->  NA , 21*, 36 , 30 , 36 
			# < 25>   0      6 , 10 , 18 , 25 , 34 , 35 
			#               13 , 16 , 24 , 25 , 33 , 36 
			#          -->  20 , 22 , NA , 25*, 32!, 37!
			# < 31>   1      8 , 15 , 21 , 31 , 33 
			#               10 , 16 , 19 , 31 , 39 
			#          -->  12 , 17!, 17 , 31*, 45 
			# < 33>   0      8 , 15 , 21 , 31 , 33 , 38 
			#               13 , 16 , 24 , 25 , 33 , 36 
			#          -->  18 , 17!, 27 , 19 , 33*, 34 
			# < 36>   2      1 , 21 , 26 , 36 
			#               24 , 25 , 33 , 36 
			#          -->  NA , 29 , NA , 36*
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
			# 839        1      3          [same    ]  3(?), 3, 3, 3     col
			# 825        1      8       [sameEnd ]  8(?), 3, 3, 3, 8     col
			# 8391       1      3      [seqReb  ]  3(?), 3, 3, 3,...     col
			# 826        4      4 [desc1   ]  4(?),xx,xx, 5,xx,xx, 6     col
			# 8251       5      3       [symm    ]  3(?), 3, 1, 3, 3     col
			# 8392       6      9             [same    ]  9(?), 9, 9     col
			# 8261       6      6          [sameEnd ]  6(?), 9, 9, 6     col
			# 831        6     10       [desc1   ] 10(?),xx, 9,xx, 8     col
			# 1          2     10       [desc1   ] 10(?),xx, 9,xx, 8  Slide/
			# 11         3      3             [desc1   ]  3(?), 2, 1  Slide/
			# 12         4      2             [desc1   ]  2(?), 1, 0 Slide\\
			# 13         5      2      [seqReb  ]  2(?), 2, 6, 6,... Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  7,20, 5, 1 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6, 5       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3,10       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 7, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 6, 1 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 7,10 ),aCStep) )	cnt.w1<-cnt.w1+1
			#	unique : 	( 1: 9,NA, 1,NA, 9) ( 1: 1,NA, 1, 4) ( 3: 3,16,15) ( 3:13, 4, 3) ( 7&6: 7, 6,NA,22,11)
			#				( 8: 9,NA,23, 8) ( 8: 2, 8,NA, 7, 5) (10: 7,10,NA, 9)
			#	unique[2] : ( 6: 1, 6) ( 7: 7, 9)
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (2)   3 (3)   4 (2)   5 (2)   6 (4)   7 (3)   8 (4)   10 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,4,5)]) )	cnt.w2<-cnt.w2+1	# 8

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1 21 26 36 40 41    |20  5 10  4  1 |                        |1 0 2 1 2 |1 2 1 2
			#      6 10 18 25 34 35    | 4  8  7  9  1 |  5 -11  -8 -11  -6  -6 |1 2 1 2 0 |1 2 1 2
			#      8 15 21 31 33 38    | 7  6 10  2  5 |  2   5   3   6  -1   3 |1 1 1 3 0 |1 1 1 3
			#     13 16 24 25 33 36(1) | 3  8  1  8  3 |  5   1   3  -6   0  -2 |0 2 2 2 0 |2 2 2
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 |-10  -6  -8  -6  -2   3 |1 3 0 2 0 |1 3 2
			#      3  9 11 12 13 19(2) | 6  2  1  1  6 |  0  -1  -5  -7 -18 -20 |2 4 0 0 0 |2 4

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(             ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  -4         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  -7         ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  -8, -7, -4 ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  -3, -6     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   3         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0, 0 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			#	unique : (-8:a,-8,a)
			#	unique : (-8:-1,-8,-1, 2) ( 5: 5,-1, 0,-11,-3)
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -8 (2)   -6 (5)   -2 (2)   -1 (2)   0 (2)   3 (4)   5 (3) 
			cnt.w2 <- 0
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(4,5)]) )	cnt.w2<-cnt.w2+1	# -25

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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

	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMI <- fCutU.getMtxInfo( gEnv$zhF )
	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		# fCutCnt.basic()
		if( any(aZoid==stdMI$lastZoid) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 9,11),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aZoid[ 1 ]*c(2,14)==aZoid[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid[1:2+1]==c(14,17)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW

		# fCutCnt.nextQuo10
		if( 1<sum( aZoid[ 1 ]*c( 3, 5)==aZoid[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
		if( 1<sum( aZoid[ 1 ]*c( 9,10)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
		if( 1<sum( aZoid[ 2 ]*c( 7, 9)==aZoid[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(c(4,31) %in% aZoid) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin

		# fCutCnt.nextFStepBin

		# fCutCnt.nextColVal_1
		if( all(c(17,36) %in% aZoid) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
		if( all(aZoid[c(1,2,5)]==c(17,28,37)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
		if( all(aZoid[c(1,4)]==c(2,11)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(c( 2,28) %in% aZoid) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4

		# fCutCnt.nextColVal_5
		if( 1<sum( aZoid[ 1 ]*c(2,3,4)==aZoid[c(2,5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aZoid[c(1,2)]*c(3,2)==aZoid[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6

		# if( fCutU.hasPtn(c( ,, ),aZoid) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum(aZoid[1:2+ ]==c( , )) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aZoid,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
		# if( 1<sum( aZoid[c(,)]*c(,)==aZoid[c(,)] ) ){	surviveFlg[idx]<-FALSE	;next }

	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]

	return( allIdxF )

}	# rmvRaw()

# UNdone		fCut.basic() 사용
rmvCStep <- function( gEnv ,allIdxF ){

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

		# fCutCnt.colValSeqNext()
		if( fCutU.hasPtn(c( 2, 2, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.colValSeqNext.cStep()
		if( fCutU.hasPtn(c( 8, 3, 9),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.basic()
		if( fCutU.hasPtn(c( 2, 3,12),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c(6,2) ,c(5,3) ,c(16,3) ,c(3,16) ,c(3,2) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
		fltRst <- sapply( getSideValues(aCStep,11) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(3,5)]*c(5,1)==aCStep[c(2,1)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
		if( fCutU.hasPtn(c( 4, 7,21),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
		if( fCutU.hasPtn(c(  1, 2,11),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 21, 2, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 8, 3) ,c( 5,11) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
		if( 1<sum( aCStep[ 5 ]*c(13,2,4)==aCStep[c(2,3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
		if( fCutU.hasPtn(c( 5,10, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[ 4 ]*c(3,5)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(4,5)]*c(3,2)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
		if( fCutU.hasPtn(c( 9, 7, 9),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
		if( fCutU.hasPtn(c(11, 8, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7,11, 8),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 5, 3) ,c(12, 6) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
    	if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
		if( fCutU.hasPtn(c(2,9,1),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3

		# fCutCnt.nextColVal_4
		if( fCutU.hasPtn(c(1,9,1),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[ 2 ]*c(4,6,1)==aCStep[c(1,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(2,3)]*c(4,4)==aCStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
		if( fCutU.hasPtn(c(13, 2, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 9, 8, 4),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[ 4 ]*c(3,3)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(5,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
		if( fCutU.hasPtn(c(15, 3, 1),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 4,15,11),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# if( fCutU.hasPtn(c( ,, ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		# ptnLst <- list( c(,) ,c(,) )
		# if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	#	dbgZoid[,2:6]-dbgZoid[,1:5]
	allIdxF <- allIdxF[surviveFlg]

	return( allIdxF )

}	# rmvCStep()

# UNdone		fCut.basic() 사용
rmvFStep <- function( gEnv ,allIdxF ){

	hasPtnCnt <- function( aCode ,ptnLst ){
		rstFlg <- sapply( ptnLst ,function(ptn){ fCutU.hasPtn(ptn,aCode) } )
		return( sum(rstFlg) )
	}

	stdMILst <- fCutU.getStdMI( gEnv )

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
		if( 1<sum( aFStep[ 2 ]*c(-1,-1)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
		if( fCutU.hasPtn(c( 3,-1,-2),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(-3, 0, 4),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[ 1 ]*c(2,4,2)==aFStep[c(2,4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[ 2 ]*c(2,1)==aFStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
		if( 1<sum( aFStep[ 1 ]*c(-1,-4)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
		if( 1<sum( aFStep[c(3,5)]*c( 2,-1)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
		if( fCutU.hasPtn(c( 2, 7, 2),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[ 6 ]*c(4,2)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
		if( fCutU.hasPtn(c(11, 4, 2),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[c(1,2)]*c( 1,-1)==aFStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[ 6 ]*c(-4,-4)==aFStep[c(1,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
		if( fCutU.hasPtn(c( 9, 8,10),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 8,11,-3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[c(4,5)]*c(2,3)==aFStep[c(3,2)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
		if( 1<sum( aFStep[ 4 ]*c(2,1)==aFStep[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
		if( fCutU.hasPtn(c( 2, 5, 3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
		if( fCutU.hasPtn(c( 3, 2, 0),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(13, 3,-3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[ 1 ]*c(1,2)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aFStep[c(1,3)]*c(1,1)==aFStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
		if( 1<sum( aFStep[ 1 ]*c( 1,-1)==aFStep[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]

	allIdxF <- allIdxF[surviveFlg]

	return( allIdxF )

}	# rmvFStep()

# UNdone		fCut.basic() 사용
rmvFV3 <- function( gEnv ,allIdxF ){
	return( allIdxF )
}	# rmvFV3()

# UNdone		fCut.basic() 사용
rmvQuo10 <- function( gEnv ,allIdxF ){

	surviveFlg <- rep( TRUE ,length(allIdxF) )
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		quoSize <- fCutU.getQuoObj( aZoid )$size

		# fCutCnt.nextZW
		# if( all(quoSize==c( 2, 1, 2, 0, 1)) ){	surviveFlg[idx]<-FALSE	;next }
		# if( all(quoSize==c( 1, 1, 2, 2, 0)) ){	surviveFlg[idx]<-FALSE	;next }
		# fCutCnt.nextQuo10
		if( all(quoSize==c( 1, 2, 1, 2, 0)) ){	surviveFlg[idx]<-FALSE	;next }
		# fCutCnt.nextBin
		if( all(quoSize==c( 0, 2, 1, 2, 1)) ){	surviveFlg[idx]<-FALSE	;next }
		# fCutCnt.nextColVal_6
		if( all(quoSize==c( 0, 1, 0, 3, 2)) ){	surviveFlg[idx]<-FALSE	;next }
	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]
	allIdxF <- allIdxF[surviveFlg]
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	# fCutCnt.nextQuo10
	#	unique 35 (29   n   31   n   33   n ... )	
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!(zw%in%35)]

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( 34 )","nextColVal_1( NA )","nextColVal_5( NA )","nextFStepBin( NA )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c(34) )]

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






