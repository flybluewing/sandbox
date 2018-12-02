# toZ836_H.R 최종접근
cntMtx.colName <- c( "raw","rawFV","rem","cStep","fStep"
						,"raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2"
					)

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

# UNdone
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	#     E1 E2 E3 E4 E5 E6
	# 830  5  6 16 18 37 38
	# 831  3 10 16 19 31 39
	# 832 13 14 19 26 40 43
	# 833 12 18 30 39 41 42
	# 834  6  8 18 35 42 43
	# 835  9 10 13 28 38 45
	for( idx in seq_len(length(allIdxF)) ){
		aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
		aCStep <- aZoid[2:6] - aZoid[1:5]
		aFStep <- aZoid - stdMI$lastZoid

		cnt.rem <- 0
		aRem <- aZoid%%10
		if( all(aRem[c(3,5)]==c(8,3)) ) aRem <- aRem+10	# 18->18, 42->42, 8->? 43->?

	}

	#=====================================================================================
	#	fCutCnt.basic() 에서 정책적으로 자르기.
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	if( TRUE ){	# 소스코드 접을 수 있으려고..
		stdMI <- fCutU.getMtxInfo( zMtx )
		# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

		# raw special
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( any(aZoid==stdMI$lastZoid) ) return(FALSE)
						# if( 1<sum(aZoid%in%stdMI$lastZoid) ) return(FALSE)
						if( 0<sum(aZoid%in%stdMI$lastZoid) ) return(FALSE)

						if( (aZoid[5]%%11==0) && all( aZoid[2:3]%%(aZoid[5]%/%11) == c(0,0) ) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# -------------------------------------------------------------------------------
		# 	basic dimPlane에서 cccObj 이외 이벤트 발생은 없다고 가정.
		# -------------------------------------------------------------------------------

		# auxCntMtx
		auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( (aZoid[6]-aZoid[1]) %in% c( 29 ) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						if( all(quoSize[1:3+1]==c(0,2,2)) ) return(FALSE)	# next rebind of 1,1,3
						if( all(quoSize[1:3+1]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,2,2
						if( all(quoSize[1:3+2]==c(2,2,1)) ) return(FALSE)	# next rebind of 2,2,0
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]


		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						if( aZoid[1]%in%c( 9    ) ) cnt<-cnt+1
						if( aZoid[2]%in%c(      ) ) cnt<-cnt+1
						if( aZoid[3]%in%c(21    ) ) cnt<-cnt+1
						if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
						if( aZoid[5]%in%c(32    ) ) cnt<-cnt+1
						if( aZoid[6]%in%c(38,40 ) ) cnt<-cnt+1
						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# anaMtx.freqVal( stdMI$rawTail )
						cnt <- 0
						# < 9>
						if( fCutU.hasPtn(c( 9,30,38,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <12>
						if( fCutU.hasPtn(c(12,NA,24      ),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(12,23,NA,23,34),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <18>
						if( fCutU.hasPtn(c(15,18,28,28,NA,44),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
						# <21>
						if( fCutU.hasPtn(c(20,21,40,36,41),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
						# <24>
						if( fCutU.hasPtn(c(12,NA,24      ),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(      24,29),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c( 2,NA,24,32,29,36),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
						# <29>
						if( fCutU.hasPtn(c(      24,29),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c( 2, 5,NA,29),aZoid) ) cnt<-cnt+1
						# <38>
						if( fCutU.hasPtn(c( 9,NA,NA,NA,NA,38),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(            32,38),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(   21,18,33,NA,38),aZoid) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# u0.zoidMtx_ana( stdMI$rawTail%%10 )
						cnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 9,6 ),c(  9    )) ) cnt<-cnt+1 # 1
						if( fCutU.remFilt(aZoid[2],c( 0,1 ),c(       )) ) cnt<-cnt+1 # 2
						if( fCutU.remFilt(aZoid[3],c( 1   ),c( 21    )) ) cnt<-cnt+1 # 3
						if( fCutU.remFilt(aZoid[4],c(     ),c(       )) ) cnt<-cnt+1 # 4
						if( fCutU.remFilt(aZoid[5],c( 2   ),c( 32    )) ) cnt<-cnt+1 # 5
						if( fCutU.remFilt(aZoid[6],c( 8,0 ),c( 38,40 )) ) cnt<-cnt+1 # 6
						return( cnt<3 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]
						if( aCStep[1]%in%c( 7   ) ) cnt<-cnt+1
						if( aCStep[2]%in%c( 2   ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 6   ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(     ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 6,2 ) ) cnt<-cnt+1

						if( 1<sum(aCStep[1:3+0]==c( 2, 15,  5)) ) cnt<-cnt+1	# 6
						if( 1<sum(aCStep[1:2+3]==c( 7,  6    )) ) cnt<-cnt+1	# 2
						if( 1<sum(aCStep[1:3+2]==c( 6, 10,  2)) ) cnt<-cnt+1	# 5

						if( fCutU.hasPtn(c( 8, 6),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(11, 1),aCStep) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c( 7, 6)) ) cnt<-cnt+1

						#
						if( all(aCStep[2:3]== aCStep[4]*c(3,5) ) ) cnt<-cnt+1
						if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) ) cnt<-cnt+1
						if( aCStep[1]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid
						if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
						if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
						if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
						if( aFStep[4]%in%c( 1, 0, 2 ) ) cnt<-cnt+1
						if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
						if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

						if( 1<sum(aFStep[1:3+0]==c(  3,   0,   4)) ) cnt<-cnt+1 #  6
						if( 1<sum(aFStep[1:3+1]==c(  3,  -2,   1)) ) cnt<-cnt+1 # -3
						if( 1<sum(aFStep[1:3+2]==c(  0,   3,  -5)) ) cnt<-cnt+1 #  2
						if( 1<sum(aFStep[1:3+3]==c(  1,   6,  -3)) ) cnt<-cnt+1 #  0

						#
						if( aFStep[1]==sum(aFStep[c(4,5)]) ) cnt<-cnt+1
						if( aFStep[5]==sum(aFStep[c(3,4)]) ) cnt<-cnt+1
						if( aFStep[4]==sum(aFStep[c(2,3,5)]) ) cnt<-cnt+1
						if( aFStep[6]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]


		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("reb","cStep3")]) ) return( FALSE )	# reb는 4연속상태
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	}

	#=====================================================================================
	#	fCutCnt.nextZW() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextZW( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )	
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		# nextZW dimPlane에서 cccObj,auxZW/auxQuo 이외 "이벤트" 발생은 없다고 가정.

    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 11, 4) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 13,12) ) cnt<-cnt+1
					if( aZoid[3]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 19   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 38   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 45   ) ) cnt<-cnt+1
					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <11>
					if( fCutU.hasPtn(c(11,20,NA,NA,NA,36),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11,NA,NA,33      ),aZoid) ) cnt<-cnt+1
					# <15>
					if( fCutU.hasPtn(c( 8,15,16   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   15,NA,34),aZoid) ) cnt<-cnt+1
					# <24>
					# <32>
					if( fCutU.hasPtn(c(25,32),aZoid) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c(11,NA,NA,33      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      26,33,41,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <35>
					# <39>
					if( fCutU.hasPtn(c(18,21,16,15,16,39),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,4   ),c( 11, 4 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3,2   ),c( 13,12 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7,0   ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3,5,9 ),c( 19    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,9,4 ),c( 38    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5     ),c( 45    )) ) cnt<-cnt+1 # 6
					return( cnt<3 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(7      ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(1      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(3,5,1,6) ) cnt<-cnt+1
					if( aCStep[4]%in%c(5,2,6  ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(6      ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 5, 15    )) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 4,  5,  4)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 5,  2,  1)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1,  5, 15)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c(  1,17),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 17, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 10, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:3+0]==c( 3,9,7 )) ) cnt<-cnt+1	#

					#
					if( all(aCStep[4:5]== aCStep[1]*c(1,3) ) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     24 25 33 34 38 39    | 1  8  1  4  1 |                        |0 0 2 4 0 |2 4
				#     20 30 33 35 36 44(1) |10  3  2  1  8 | -4   5   0   1  -2   5 |0 0 1 4 1 |1 4 1
				#      1  2  3  9 12 23    | 1  1  6  3 11 |-19 -28 -30 -26 -24 -21 |4 1 1 0 0 |4 1 1
				#      8  9 18 21 28 40(1) | 1  9  3  7 12 |  7   7  15  12  16  17 |2 1 2 0 1 |2 1 2 1
				#      6 21 35 36 37 41(1) |15 14  1  1  4 | -2  12  17  15   9   1 |1 0 1 3 1 |1 1 3 1
				#      3 10 23 24 31 39    | 7 13  1  7  8 | -3 -11 -12 -12  -6  -2 |1 1 2 2 0 |1 1 2 2
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  0,  7  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -2,-10  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  0,   4, -11)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+0]==c( -7,  -4,  -4)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+3]==c(  0,   4, -11)) ) cnt<-cnt+1 # -3

					#
					if( aFStep[4]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]


		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c("quoAll","nbor")]) ) return( FALSE )
						
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
						# if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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
						# if( 0 < sum(score[c(  )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextBin( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextRebNum() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( "reb" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						if( 0 < sum(cStepVal[c("c31")]) ) return( FALSE )
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


		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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
						# if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextFStepBin( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_1() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( "quoAll" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						if( 0 < sum(cStepVal[c("max2","c33")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_1( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_2() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("remH1")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						if( 0 < sum(cStepVal[c("max2","c33")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]


		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_2( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_3() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( , )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						if( 0 < sum(cStepVal[c("c24")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_3( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_4() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( "reb" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						# if( 0 < sum(cStepVal[c("c24")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_4( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_5() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c( "remH0" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						if( 0 < sum(cStepVal[c("c22")]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		k.FLogStr(sprintf("fCut.basic allIdxF %d\n",length(allIdxF)))
	} # fCutU.nextColVal_5( gEnv )$zMtx

	#=====================================================================================
	#	fCutCnt.nextColVal_6() 에서 정책적으로 자르기.
	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0<nrow(zMtx) ){
		stdMI <- fCutU.getMtxInfo( zMtx )

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( "reb","spanM" )]) ) return( FALSE )
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( cccObj$cStepValMtx ,1 ,function( cStepVal ){
						# if( 0 < sum(cStepVal[c("c24")]) ) return( FALSE )
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

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					if( (aZoid[6]-aZoid[1]) %in% c( 35 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag

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
			if( aZoid[3]%in%c( 17,28 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,38 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,18 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,NA,37,45 ),aZoid) ) cnt<-cnt+1
			# <13>
			# <16>
			if( fCutU.hasPtn(c(       16,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,14,16,NA,25    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  6,NA,18 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,18,40,45,45 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(       19,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,12,19,21    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 12, 4, 8,19,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(    29,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 17,NA,39 ),aZoid) ) cnt<-cnt+1
			# <42>
			# <43>
			if( fCutU.hasPtn(c( 17,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0       ),c(       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,8     ),c( 17,28 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,0     ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 8,3     ),c( 28    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,8,6   ),c( 43,38 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3, 2   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1, 7   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( 1<sum(aCStep[1:2+0]==c(  3,10 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 12,10 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (3)   3 (3)   6 (2)   7 (4)   10 (3)   12 (2) 

			cnt.w2 <- 0
			if( aCStep[4]==sum(aCStep[c(2,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 2,3 )])==sum(aCStep[c( 1,4,5 )]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  6 16 18 37 38    | 1 10  2 19  1 |                        |2 2 0 2 0 |2 2 2
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 | -2   4   0   1  -6   1 |1 3 0 2 0 |1 3 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 | 10   4   3   7   9   4 |0 3 1 0 2 |3 1 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   4  11  13   1  -1 |0 2 0 2 2 |2 2 2
			#      6  8 18 35 42 43(2) | 2 10 17  7  1 | -6 -10 -12  -4   1   1 |2 1 0 1 2 |2 1 1 2
			#      9 10 13 28 38 45    | 1  3 15 10  7 |  3   2  -5  -7  -4   2 |1 2 1 1 1 |1 2 1 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -6       ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -4     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  3, -4, 11 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  -4,  3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -4 (2)   -1 (2)   1 (5)   2 (2)   3 (2)   4 (4) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[2]*c(-2, 1)==aFStep[c(5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,2  )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c(4,6  )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,5,2)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,4  )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(2,5  )]) )	cnt.w2<-cnt.w2+1	# -2
			if( sum(aFStep[c(1,3)])==sum(aFStep[c(6,5  )]) )	cnt.w2<-cnt.w2+1	# -2
			if( sum(aFStep[c(5,6)])==sum(aFStep[c(1,2,4)]) )	cnt.w2<-cnt.w2+1	# -2
			if( sum(aFStep[c(1,5)])==sum(aFStep[c(2,6,3)]) )	cnt.w2<-cnt.w2+1	# -1

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
					# [1]    3 17
					# [2]    3 31 18
					# [3]   
					# [4]   33 24 30 44
					# [5]   36 44 45 35 45 43
					# [6]   36 38 43 28 37 31 35 40 44
					if( 1<sum(aZoid==c(  3, 3,NA,33,36,36 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 17,31,NA,24,44,38 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( NA,18,NA,30,45,43 ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 32,20 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(        ) ) score<-score+1
					if( aZoid[2]%in%c(        ) ) score<-score+1
					if( aZoid[3]%in%c(        ) ) score<-score+1
					if( aZoid[4]%in%c(        ) ) score<-score+1
					if( aZoid[5]%in%c(        ) ) score<-score+1
					if( aZoid[6]%in%c(        ) ) score<-score+1
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
					if( aZoid[1]%in%c(             ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(             ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 28,23       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 22          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 37,39       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(             ) ) cnt<-cnt+1

					# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aZoid[1:2+0]==c(  5,13 )) ) cnt<-cnt+1
					if( all(aZoid[1:2+3]==c( 22,37 )) ) cnt<-cnt+1
					if( all(aZoid[1:2+4]==c( 39,42 )) ) cnt<-cnt+1
					if( all(aZoid[1:2+4]==c( 39,41 )) ) cnt<-cnt+1
					if( all(aZoid[1:2+4]==c( 39,40 )) ) cnt<-cnt+1
					if( all(aZoid[1:2+4]==c( 41,45 )) ) cnt<-cnt+1

					# [  1]  2 16     8 27    32 34    29 44    39 41
					# [  2] 20 30    23 26             38 42    42 45
					# [  3]  4 22    25 27             23 37    42 43
					# [  4]  5 14    18 23             33 39    34 35
					# [  5] 18 24    31 32             19 20    27 32
					# [  6]  2  7     7 15             24 37    35 40
					# [  7]  5 15    27 30                      34 36
					# [  8]  2 12                               39 40
					# [  9] 17 18                               17 25
					# [ 10]                                     21 26
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(       ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c(       ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 8,3   ),c( 28,23 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 0,2   ),c( 22    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 7,9   ),c( 37,39 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 5     ),c(       )) )	remCnt <- remCnt+1
						# grp (1:2+0)
							if( aZoid[1]== 2  && fCutU.remFilt(aZoid[2],c( 5),c( )) ) remCnt <- remCnt+1 
							if( aZoid[1]== 5  && fCutU.remFilt(aZoid[2],c( 3),c(13)) ) remCnt <- remCnt+1 
						# grp (1:2+3)
							if( aZoid[5]==37  && fCutU.remFilt(aZoid[4],c( 2),c(22)) ) remCnt <- remCnt+1 
						# grp (1:2+4)
							if( aZoid[5]==34  && fCutU.remFilt(aZoid[6],c( 4),c(34)) ) remCnt <- remCnt+1 
							# if( aZoid[5]==39  && fCutU.remFilt(aZoid[6],c( 2),c(42)) ) remCnt <- remCnt+1 
							if( aZoid[6]==41  && fCutU.remFilt(aZoid[5],c( 9),c(39)) ) remCnt <- remCnt+1 
							if( aZoid[6]==45  && fCutU.remFilt(aZoid[5],c( 1),c(41)) ) remCnt <- remCnt+1 
						#	if( aZoid[ ]==  && fCutU.remFilt(aZoid[ ],c( ),c( )) ) remCnt <- remCnt+1 

					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	[1] 14  10  18   9   6   5 |10  10   1 
					#	[2] 19   3   2   5   1   8 | 3 
					#	[3]  2
					#	[4] 15   4  14   6   1  13 |
					#	[5]  2   3   1   1   5   5 | 2   1   8   5   2   2  10   4 
					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c( 14,19, 2,15, 2 ),na.rm=T)
					matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 10, 3,NA, 4, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 18, 2,NA,14, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  9, 5,NA, 6, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  6, 1,NA, 1, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5, 8,NA,13, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 10, 3,NA,NA, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 10,NA,NA,NA, 1 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )

					cnt <- 0
						# if( fCutU.hasPtn(c(   ,   ),aCStep) ) aCStep<-aCStep+1
						# if( 1<sum(aCStep[1:2+ ]==c(   ,   )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c( 10, 3 ),aCStep) ) aCStep<-aCStep+1
						if( 1<sum(aCStep[1:2+0]==c(  6, 1 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c( 20, 2 ),aCStep) ) aCStep<-aCStep+1	# -
						if( fCutU.hasPtn(c(  6, 1 ),aCStep) ) aCStep<-aCStep+1

						if( aCStep[1]%in%c( 11 ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(    ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(  1 ) ) cnt<-cnt+1

						if( 1<sum( aCStep[ 5 ]*c( 7,1 )==aCStep[c(1,3)] ) )	cnt<-cnt+1
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
					# [  1] 14 26 30     1  8 19 
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

					tCnt <- 0
						if( aCStep[1]%in%c(  1    ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(  3    ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(       ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c(  2, 5 ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(  7, 3 ) ) tCnt<-tCnt+1

						if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,3)] ) )	cnt<-cnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  7, 2 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+0]==c( 14,10 )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  2, 2 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+1]==c(  4, 2 )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  2,11 ),aCStep) )	cnt<-cnt+1
					if( 1<sum(aCStep[1:2+3]==c( 12, 7 )) )	cnt<-cnt+1

					# [1]*   7  5  1 20 10  1 | 5  5  2  4  7  4  6  3  1  3 11  1  3  6 20  2 13  6  4  5  9 19 10  1  5
					# [2]    2  1
					# [3]    2
					# [4]*   3  4  2  6  2  4 | 5  5  7 14  7  5  4  1  6 13 10  9  7 12  3  7  1  2  4 10  9  6
					# [5]*   7  2  2  1 11 10 | 6  3 12 10  2 14  4  2 10  8 15  1  1  7  3  7  9 11  9 16 15  2 10  2  6  1  2  2 ...
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
					# [  1] 10  6              4 12     5  3
					# [  2]  1 13             21  1     7 10
					# [  3]  4  3                       2 16
					# [  4]  2 14                       2 11
					# [  5] 13  1                       2  8
					# [  6]  7  1                       4  6
					# [  7]  8  3                       3  3
					# [  8]  5  8                       1  1
					# [  9]  2  2                      13  1
					# [ 10]  1  1                       8  5
					# [ 11]  6  2                       7  4
					# [ 12]  3 12                      10  6
					# [ 13] 21  5                       6  1

					if( aCStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 12      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  9      ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aCStep[1:2+3]==c(  3, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  9, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 17, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  8, 4 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  3, 8 )) ) cnt<-cnt+1
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
					if( aCStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 6  ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 1  ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(    ) ) cnt<-cnt+1

					# [  1]                          5  1 19
					# [  2]                          4  6 11
					# [  3]                          2  6  2
					# [  4]                         12  2  6
					# [  5]                          3 19  8
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 18 26 33 34 38    |14  8  7  1  4 |                        |1 1 1 3 0 |1 1 1 3
			#      1 28 35 41 43 44    |27  7  6  2  1 | -3  10   9   8   9   6 |1 0 1 1 3 |1 1 1 3
			#      2 11 17 18 21 27    | 9  6  1  3  6 |  1 -17 -18 -23 -22 -17 |1 3 2 0 0 |1 3 2
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  6   1   2   3  10   8 |1 2 1 2 0 |1 2 1 2
			#      4  6 15 25 26 33    | 2  9 10  1  7 | -4  -6  -4   4  -5  -2 |2 1 2 1 0 |2 1 2 1
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  9   8   4   1  14  10 |0 3 1 0 2 |3 1 2

			#   zoid width  ... 34   43   25   27   29   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#     tgt.col banVal                     descript tgt.dir
			#  736       3     17 [symm    ] 17(?),19,15,19,17     col
			#  832       4     27       [desc1   ] 27(?),26,25     col
			#  1         1     13       [desc1   ] 13(?),14,15  Slide/
			#  11        2     13   [desc( 6) ] 13(?),19,25,31  Slide/
			#  12        3     26       [same    ] 26(?),26,26  Slide/
			#  13        3     35    [sameEnd ] 35(?),26,26,35  Slide/
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
			#   dup number  4:2   18:2   19:2   21:2   26:3   33:2   35:2   43:2
			# <  4>   0      4 , 18 , 26 , 33 , 34 , 38 
			#                4 ,  6 , 15 , 25 , 26 , 33 
			#          -->   4*, NA , NA , 17 , 18 , 28 
			# < 18>   2      4 , 18 , 26 , 33 
			#               17 , 18 , 21 , 27 
			#          -->  NA , 18*, NA , 21 
			# < 19>   0      8 , 12 , 19 , 21 , 31 , 35 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  18 , 16 , 19*, 31 , NA , NA 
			# < 21>  -1     11 , 17 , 18 , 21 , 27 
			#                8 , 12 , 19 , 21 , 31 
			#          -->   5 ,  7 , 20!, 21*, 35 
			# < 26>  -1      6 , 15 , 25 , 26 , 33 
			#               13 , 14 , 19 , 26 , 40 
			#          -->  20 , 13!, 13 , 26*, NA 
			# < 33>   2      4 , 18 , 26 , 33 
			#               15 , 25 , 26 , 33 
			#          -->  26 , 32 , 26!, 33*
			# < 35>   3      1 , 28 , 35 
			#               21 , 31 , 35 
			#          -->  NA , 34 , 35*
			# < 43>   1      1 , 28 , 35 , 41 , 43 
			#               14 , 19 , 26 , 40 , 43 
			#          -->  27 , 10 , 17 , 39!, 43*
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
			#      tgt.col banVal                     descript tgt.dir
			# 832        1      2       [desc1   ]  2(?), 3, 4     col
			# 736        3      7 [symm    ]  7(?), 9, 5, 9, 7     col
			# 8321       4      7       [desc1   ]  7(?), 6, 5     col
			# 8322       6      3       [same    ]  3(?), 3, 3     col
			# 773        6      5    [sameEnd ]  5(?), 3, 3, 5     col
			# 1          1      3       [desc1   ]  3(?), 4, 5  Slide/
			# 11         2     13   [desc(-4) ] 13(?), 9, 5, 1  Slide/
			# 12         3      6       [same    ]  6(?), 6, 6  Slide/
			# 13         3      5    [sameEnd ]  5(?), 6, 6, 5  Slide/
			# 14         3      4       [same    ]  4(?), 4, 4 Slide\\
			# 15         5      7       [desc1   ]  7(?), 6, 5 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      773        2      6 [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			#      7731       5      4 [same    ]  4(?), ., ., 4, ., ., 4     col
			#      785        5      8       [desc1   ]  8(?),xx, 7,xx, 6     col
			#      1          1      5      [seqReb  ]  5(?), 5,10,10,...  Slide/
			#      11         5     11       [desc1   ] 11(?),xx,10,xx, 9 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(3)   3(2)   4(3)   6(3)   7(5)   9(2)   10(2)   14(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      E3       2      4       [same    ]  4(?), 4, 4  Slide/
			#      E5       2     10    [sameEnd ] 10(?), 4, 4,10  Slide/
			#      E1       5      1 [symm    ]  1(?), 1,-4, 1, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -17(2)   -4(2)   1(3)   4(2)   6(2)   8(3)   9(3)   10(3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      2 11 17 18 21 27    | 9  6  1  3  6 |                        |1 3 2 0 0 |1 3 2
			#      2  8 17 24 29 31(2) | 6  9  7  5  2 |  0  -3   0   6   8   4 |2 1 2 1 0 |2 1 2 1
			#     15 17 19 21 27 45(1) | 2  2  2  6 18 | 13   9   2  -3  -2  14 |0 3 2 0 1 |3 2 1
			#      8 11 19 21 36 45(3) | 3  8  2 15  9 | -7  -6   0   0   9   0 |1 2 1 1 1 |1 2 1 1 1
			#      1  3 12 14 16 43    | 2  9  2  2 27 | -7  -8  -7  -7 -20  -2 |2 3 0 0 1 |2 3 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 |  3   4   1  15  15  -4 |2 1 1 2 0 |2 1 1 2

			#   zoid width  ... 25   29   30   37   42   35 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                           descript tgt.dir
			#  811       2     11 [same    ] 11(?), ., .,11, ., .,11     col
			#  828       3     14             [desc1   ] 14(?),13,12     col
			#  1         2     12             [desc1   ] 12(?),13,14  Slide/

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
			#   dup number  2:2   8:2   11:2   17:3   19:2   21:3   27:2   29:2   31:2   45:2
			# <  2>   0      2 , 11 , 17 , 18 , 21 , 27 
			#                2 ,  8 , 17 , 24 , 29 , 31 
			#          -->   2*,  5 , 17!, 30 , 37 , 35 
			# <  8>  -1      8 , 17 , 24 , 29 , 31 
			#                8 , 11 , 19 , 21 , 36 
			#          -->   8*, NA , 14 , 13 , 41 
			# < 11>   0      2 , 11 , 17 , 18 , 21 , 27 
			#                8 , 11 , 19 , 21 , 36 , 45 
			#          -->  NA , 11*, 21 , 24 , NA , NA 
			# < 17>  -1      8 , 17 , 24 , 29 , 31 
			#               15 , 17 , 19 , 21 , 27 
			#          -->  NA , 17*, NA , NA , 23 
			# < 19>   0     15 , 17 , 19 , 21 , 27 , 45 
			#                8 , 11 , 19 , 21 , 36 , 45 
			#          -->   1 ,  5 , 19*, 21!, 45 , 45!
			# < 21>   0     15 , 17 , 19 , 21 , 27 , 45 
			#                8 , 11 , 19 , 21 , 36 , 45 
			#          -->   1 ,  5 , 19!, 21*, 45 , 45!
			# < 27>  -1     11 , 17 , 18 , 21 , 27 
			#               15 , 17 , 19 , 21 , 27 
			#          -->  19 , 17!, 20!, 21!, 27*
			# < 29>  -1      8 , 17 , 24 , 29 , 31 
			#                4 ,  7 , 13 , 29 , 31 
			#          -->  NA , NA ,  2 , 29*, 31!
			# < 31>  -1      8 , 17 , 24 , 29 , 31 
			#                4 ,  7 , 13 , 29 , 31 
			#          -->  NA , NA ,  2 , 29!, 31*
			# < 45>   0     15 , 17 , 19 , 21 , 27 , 45 
			#                8 , 11 , 19 , 21 , 36 , 45 
			#          -->   1 ,  5 , 19!, 21!, NA , 45*

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
			# 811        2      1  [same    ]  1(?), ., ., 1, ., ., 1     col
			# 828        3      4              [desc1   ]  4(?), 3, 2     col
			# 812        5      5        [desc1   ]  5(?),xx, 6,xx, 7     col
			# 8281       5      1       [seqReb  ]  1(?), 1, 6, 6,...     col
			# 8121       6      1 [desc( 2) ]  1(?),xx, 3,xx, 5,xx, 7     col
			# 1          2      2              [desc1   ]  2(?), 3, 4  Slide/
			# 11         2      3        [desc1   ]  3(?),xx, 4,xx, 5  Slide/
			# 12         4      3              [same    ]  3(?), 3, 3 Slide\\
			# 13         4      8           [sameEnd ]  8(?), 3, 3, 8 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      812        1      2       [same    ]  2(?), ., 2, ., 2     col
			#      736        1      9 [sameEnd ]  9(?),xx, 2,xx, 2,xx, 9     col
			#      828        1      4             [desc1   ]  4(?), 3, 2     col
			#      780        1      2       [symm    ]  2(?), 3, 2, 3, 2     col
			#      8121       3      2       [same    ]  2(?), ., 2, ., 2     col
			#      7361       3      1 [sameEnd ]  1(?),xx, 2,xx, 2,xx, 1     col
			#      811        3      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      8281       3     16      [seqReb  ] 16(?),16, 2, 2,...     col
			#      8282       4      2             [same    ]  2(?), 2, 2     col
			#      8111       4     15          [sameEnd ] 15(?), 2, 2,15     col
			#      1          5      2             [same    ]  2(?), 2, 2 Slide\\
			#      11         5      8          [sameEnd ]  8(?), 2, 2, 8 Slide\\
			#      12         5      2       [same    ]  2(?), ., 2, ., 2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    2(9)   3(3)   6(5)   8(2)   9(4) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                      descript tgt.dir
			#      1       1      3 [seqReb  ]  3(?), 3,-7,-7,...     col
			#      2       6     -6    [desc( 2) ] -6(?),-4,-2, 0     col
			# -------------------------------------------------------------------------------------
			#     FV :    -7(4)   -3(2)   -2(2)   0(5)   4(2)   9(2)   15(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      1  3 17 20 31 44    | 2 14  3 11 13 |                        |2 1 1 1 1 |2 1 1 1 1
			#      3 13 18 33 37 45(1) |10  5 15  4  8 |  2  10   1  13   6   1 |1 2 0 2 1 |1 2 2 1
			#      3  4  6 10 28 30(1) | 1  2  4 18  2 |  0  -9 -12 -23  -9 -15 |3 1 1 1 0 |3 1 1 1
			#      1 28 35 41 43 44(1) |27  7  6  2  1 | -2  24  29  31  15  14 |1 0 1 1 3 |1 1 1 3
			#      7  9 12 14 23 28(1) | 2  3  2  9  5 |  6 -19 -23 -27 -20 -16 |2 2 2 0 0 |2 2 2
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 |  5   9  18  25  18  14 |0 2 0 2 2 |2 2 2
			
			#   zoid width  ... 43   42   27   43   21   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  721        1      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			#  7211       6     44 [same    ] 44(?), ., .,44, ., .,44     col

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
			#   dup number  1:2   3:3   12:2   18:2   28:3   30:2   41:2   44:2
			# <  1>   0      1 ,  3 , 17 , 20 , 31 , 44 
			#                1 , 28 , 35 , 41 , 43 , 44 
			#          -->   1*, NA , NA , NA , NA , 44!
			# <  3>   0      3 , 13 , 18 , 33 , 37 , 45 
			#                3 ,  4 ,  6 , 10 , 28 , 30 
			#          -->   3*, NA , NA , NA , 19 , 15 
			# < 12>  -2     12 , 14 , 23 , 28 
			#               12 , 18 , 30 , 39 
			#          -->  12*, 22 , 37 , NA 
			# < 18>  -1     13 , 18 , 33 , 37 , 45 
			#               12 , 18 , 30 , 39 , 41 
			#          -->  11!, 18*, 27 , 41 , 37 
			# < 28>   4      1 , 28 
			#               23 , 28 
			#          -->  NA , 28*
			# < 30>  -3     10 , 28 , 30 
			#               12 , 18 , 30 
			#          -->  14 ,  8 , 30*
			# < 41>   1      1 , 28 , 35 , 41 , 43 
			#               18 , 30 , 39 , 41 , 42 
			#          -->  35 , 32 , NA , 41*, NA 
			# < 44>   0      1 ,  3 , 17 , 20 , 31 , 44 
			#                1 , 28 , 35 , 41 , 43 , 44 
			#          -->   1!, NA , NA , NA , NA , 44*
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
			# 833        2      7                 [desc1   ]  7(?), 8, 9     col
			# 706        2      4           [symm    ]  4(?), 8, 9, 8, 4     col
			# 7211       4      2     [desc1   ]  2(?),xx,xx, 1,xx,xx, 0     col
			# 747        4      4 [seqReb  ]  4(?), ., 4, ., 0, ., 0,...     col
			# 8331       5      1          [seqReb  ]  1(?), 1, 3, 3,...     col
			# 7212       6      4     [same    ]  4(?), ., ., 4, ., ., 4     col
			# 1          3      9                 [desc1   ]  9(?), 8, 7 Slide\\
			# 11         5      1           [desc1   ]  1(?),xx, 2,xx, 3 Slide\\
			# 12         6      4           [same    ]  4(?), ., 4, ., 4 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                      descript tgt.dir
			#      747        1      3  [desc1   ]  3(?),xx, 2,xx, 1     col
			#      7471       2      4  [desc1   ]  4(?),xx, 3,xx, 2     col
			#      706        4     18  [symm    ] 18(?), 2, 9, 2,18     col
			#      7061       5      2  [symm    ]  2(?), 1, 5, 1, 2     col
			#      1          1      2  [same    ]  2(?), ., 2, ., 2  Slide/
			#      11         1     12 [seqReb  ] 12(?),12, 2, 2,...  Slide/
			#      12         2      9        [same    ]  9(?), 9, 9  Slide/
			#      13         2      1     [sameEnd ]  1(?), 9, 9, 1  Slide/
			#      14         5      2        [same    ]  2(?), 2, 2 Slide\\
			#      15         5      7     [sameEnd ]  7(?), 2, 2, 7 Slide\\
			#      16         5      3  [desc1   ]  3(?),xx, 2,xx, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(7)   3(2)   4(2)   5(2)   6(2)   9(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                        descript tgt.dir
			#      1       1      4          [desc1   ]  4(?), 5, 6     col
			#      2       6    -17 [desc1   ] -17(?),xx,-16,xx,-15     col
			#      3       6    -15 [symm    ] -15(?),14,-16,14,-15     col
			# -------------------------------------------------------------------------------------
			#     FV :    -23(2)   -9(2)   1(2)   6(2)   14(2)   18(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      8 13 20 22 23 36    | 5  7  2  1 13 |                        |1 1 3 1 0 |1 1 3 1
			#      4 10 18 27 40 45    | 6  8  9 13  5 | -4  -3  -2   5  17   9 |1 2 1 0 2 |1 2 1 2
			#      5 13 18 23 40 45(3) | 8  5  5 17  5 |  1   3   0  -4   0   0 |1 2 1 0 2 |1 2 1 2
			#      1  2  6 16 19 42    | 1  4 10  3 23 | -4 -11 -12  -7 -21  -3 |3 2 0 0 1 |3 2 1
			#      1  4  8 23 33 42(2) | 3  4 15 10  9 |  0   2   2   7  14   0 |3 0 1 1 1 |3 1 1 1
			#      1  4 10 12 28 45(2) | 3  6  2 16 17 |  0   0   2 -11  -5   3 |2 2 1 0 1 |2 2 1 1

			#   zoid width  ... 28   41   40   41   41   44 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  800        1      1              [same    ]  1(?), 1, 1, 1     col
			#  616        1      5           [sameEnd ]  5(?), 1, 1, 1, 5     col
			#  8001       1      1          [seqReb  ]  1(?), 1, 1, 1,...     col
			#  8002       2      4                 [same    ]  4(?), 4, 4     col
			#  621        2      2              [sameEnd ]  2(?), 4, 4, 2     col
			#  704        2      4 [seqReb  ]  4(?), ., 4, .,13, .,13,...     col
			#  8003       3     12             [desc(-2) ] 12(?),10, 8, 6     col
			#  7041       4     23           [same    ] 23(?), .,23, .,23     col
			#  473        4     22     [sameEnd ] 22(?),xx,23,xx,23,xx,22     col
			#  8004       6     45          [seqReb  ] 45(?),45,42,42,...     col
			#  565        6     45        [symm    ] 45(?),45,42,42,45,45     col

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
			#   dup number  1:3   4:3   8:2   10:2   13:2   18:2   23:3   40:2   42:2   45:3
			# <  1>   0      1 ,  4 ,  8 , 23 , 33 , 42 
			#                1 ,  4 , 10 , 12 , 28 , 45 
			#          -->   1*,  4!, 12 , NA , 23 , NA 
			# <  4>   0      1 ,  4 ,  8 , 23 , 33 , 42 
			#                1 ,  4 , 10 , 12 , 28 , 45 
			#          -->   1!,  4*, 12 , NA , 23 , NA 
			# <  8>   2      8 , 13 , 20 , 22 
			#                8 , 23 , 33 , 42 
			#          -->   8*, 33 , NA , NA 
			# < 10>   1      4 , 10 , 18 , 27 , 40 
			#                4 , 10 , 12 , 28 , 45 
			#          -->   4!, 10*, NA , 29!, NA 
			# < 13>   0      8 , 13 , 20 , 22 , 23 , 36 
			#                5 , 13 , 18 , 23 , 40 , 45 
			#          -->   2 , 13*, 16 , 24!, NA , NA 
			# < 18>   0      4 , 10 , 18 , 27 , 40 , 45 
			#                5 , 13 , 18 , 23 , 40 , 45 
			#          -->   6!, 16 , 18*, 19 , 40!, 45!
			# < 23>   0      5 , 13 , 18 , 23 , 40 , 45 
			#                1 ,  4 ,  8 , 23 , 33 , 42 
			#          -->  NA , NA , NA , 23*, 26 , 39 
			# < 40>   0      4 , 10 , 18 , 27 , 40 , 45 
			#                5 , 13 , 18 , 23 , 40 , 45 
			#          -->   6!, 16 , 18!, 19 , 40*, 45!
			# < 42>   0      1 ,  2 ,  6 , 16 , 19 , 42 
			#                1 ,  4 ,  8 , 23 , 33 , 42 
			#          -->   1!,  6 , 10 , 30 , NA , 42*
			# < 45>   0      5 , 13 , 18 , 23 , 40 , 45 
			#                1 ,  4 , 10 , 12 , 28 , 45 
			#          -->  NA , NA ,  2 ,  1 , 16 , 45*

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
			# 800        1      1              [same    ]  1(?), 1, 1, 1     col
			# 616        1      5           [sameEnd ]  5(?), 1, 1, 1, 5     col
			# 8001       1      1          [seqReb  ]  1(?), 1, 1, 1,...     col
			# 8002       2      4                 [same    ]  4(?), 4, 4     col
			# 621        2      2              [sameEnd ]  2(?), 4, 4, 2     col
			# 704        2      5           [desc1   ]  5(?),xx, 4,xx, 3     col
			# 6211       2      1     [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			# 7041       2      4 [seqReb  ]  4(?), ., 4, ., 3, ., 3,...     col
			# 7042       3      8           [same    ]  8(?), ., 8, ., 8     col
			# 473        3      0     [sameEnd ]  0(?),xx, 8,xx, 8,xx, 0     col
			# 7043       4      3           [same    ]  3(?), ., 3, ., 3     col
			# 4731       4      2     [sameEnd ]  2(?),xx, 3,xx, 3,xx, 2     col
			# 8003       4      1                 [desc1   ]  1(?), 2, 3     col
			# 8004       6      5          [seqReb  ]  5(?), 5, 2, 2,...     col
			# 565        6      5        [symm    ]  5(?), 5, 2, 2, 5, 5     col
			# 1          3      1                 [desc1   ]  1(?), 2, 3  Slide/
			# 11         5      5           [symm    ]  5(?), 2, 8, 2, 5 Slide\\
			# 12         6      3           [same    ]  3(?), ., 3, ., 3 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                      descript tgt.dir
			#      800        1      3        [same    ]  3(?), 3, 3     col
			#      621        1      1     [sameEnd ]  1(?), 3, 3, 1     col
			#      704        2      3  [desc1   ]  3(?),xx, 4,xx, 5     col
			#      8001       2      6 [seqReb  ]  6(?), 6, 4, 4,...     col
			#      1          5     17        [desc1   ] 17(?),16,15 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(2)   3(3)   4(2)   5(5)   6(2)   8(2)   9(2)   10(2)   13(2)   17(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                       descript tgt.dir
			#      1         1      0         [same    ]  0(?), 0, 0     col
			#      2         1     -4      [sameEnd ] -4(?), 0, 0,-4     col
			#      3         1     -1   [desc1   ] -1(?),xx, 0,xx, 1     col
			#      4         2      1   [desc1   ]  1(?),xx, 2,xx, 3     col
			#      5         3      2         [same    ]  2(?), 2, 2     col
			#      6         3    -12    [sameEnd ] -12(?), 2, 2,-12     col
			#      7         6      0   [same    ]  0(?), ., 0, ., 0     col
			#      8         6      6     [desc(-3) ]  6(?), 3, 0,-3     col
			#      E2        3      0         [same    ]  0(?), 0, 0 Slide\\
			#      E3        4      2         [same    ]  2(?), 2, 2 Slide\\
			#      E1        4     -4      [sameEnd ] -4(?), 2, 2,-4 Slide\\
			#      E31       5      3   [desc1   ]  3(?),xx, 2,xx, 1 Slide\\
			#      E11       5      1 [symm    ]  1(?),-11, 2,-11, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -4(3)   -3(2)   0(7)   2(3)   3(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#     14 21 29 31 32 37    | 7  8  2  1  5 |                        |0 1 2 3 0 |1 2 3
			#     14 15 18 21 26 35(2) | 1  3  3  5  9 |  0  -6 -11 -10  -6  -2 |0 3 2 1 0 |3 2 1
			#      3  4  6 10 28 30    | 1  2  4 18  2 |-11 -11 -12 -11   2  -5 |3 1 1 1 0 |3 1 1 1
			#      1 28 35 41 43 44(1) |27  7  6  2  1 | -2  24  29  31  15  14 |1 0 1 1 3 |1 1 1 3
			#      7  9 12 14 23 28(1) | 2  3  2  9  5 |  6 -19 -23 -27 -20 -16 |2 2 2 0 0 |2 2 2
			#     12 18 30 39 41 42(1) | 6 12  9  2  1 |  5   9  18  25  18  14 |0 2 0 2 2 |2 2 2

			#   zoid width  ... 23   21   27   43   21   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			# [1] tgt.col  banVal   descript tgt.dir 
			#  <0 rows> (or 0-length row.names)

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
			#   dup number  12:2   14:3   18:2   21:2   28:3   30:2   35:2   41:2
			# < 12>  -2     12 , 14 , 23 , 28 
			#               12 , 18 , 30 , 39 
			#          -->  12*, 22 , 37 , NA 
			# < 14>   3     14 , 15 , 18 
			#               14 , 23 , 28 
			#          -->  14*, 31 , 38 
			# < 18>  -1     15 , 18 , 21 , 26 , 35 
			#               12 , 18 , 30 , 39 , 41 
			#          -->   9 , 18*, 39 , NA , NA 
			# < 21>   2     14 , 21 , 29 , 31 
			#               18 , 21 , 26 , 35 
			#          -->  NA , 21*, 23 , 39 
			# < 28>   4      1 , 28 
			#               23 , 28 
			#          -->  NA , 28*
			# < 30>  -3     10 , 28 , 30 
			#               12 , 18 , 30 
			#          -->  14 ,  8 , 30*
			# < 35>  -3     21 , 26 , 35 
			#                1 , 28 , 35 
			#          -->  NA , 30 , 35*
			# < 41>   1      1 , 28 , 35 , 41 , 43 
			#               18 , 30 , 39 , 41 , 42 
			#          -->  35 , 32 , NA , 41*, NA 
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
			# 833        2      7             [desc1   ]  7(?), 8, 9     col
			# 706        2      4       [symm    ]  4(?), 8, 9, 8, 4     col
			# 721        4      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			# 7211       5      4 [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			# 8331       5      1      [seqReb  ]  1(?), 1, 3, 3,...     col
			# 1          3      9             [desc1   ]  9(?), 8, 7 Slide\\
			# 11         5      1       [desc1   ]  1(?),xx, 2,xx, 3 Slide\\
			# 12         6      4       [same    ]  4(?), ., 4, ., 4 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      747        1      3       [desc1   ]  3(?),xx, 2,xx, 1     col
			#      7471       2      4       [desc1   ]  4(?),xx, 3,xx, 2     col
			#      721        2      6 [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			#      7211       4      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      706        4     18       [symm    ] 18(?), 2, 9, 2,18     col
			#      7061       5      2       [symm    ]  2(?), 1, 5, 1, 2     col
			#      1          1      2       [same    ]  2(?), ., 2, ., 2  Slide/
			#      11         1     12      [seqReb  ] 12(?),12, 2, 2,...  Slide/
			#      12         2      9             [same    ]  9(?), 9, 9  Slide/
			#      13         2      1          [sameEnd ]  1(?), 9, 9, 1  Slide/
			#      14         5      2             [same    ]  2(?), 2, 2 Slide\\
			#      15         5      7          [sameEnd ]  7(?), 2, 2, 7 Slide\\
			#      16         5      3       [desc1   ]  3(?),xx, 2,xx, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(7)   3(3)   5(3)   6(2)   7(2)   9(3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                      descript tgt.dir
			#      1       1      4        [desc1   ]  4(?), 5, 6     col
			#      2       6     -5 [symm    ] -5(?),14,-16,14,-5     col
			# -------------------------------------------------------------------------------------
			#     FV :    -11(4)   -6(2)   -2(2)   14(2)   18(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      8 16 25 30 42 43    | 8  9  5 12  1 |                        |1 1 1 1 2 |1 1 1 1 2
			#     11 18 26 27 40 41    | 7  8  1 13  1 |  3   2   1  -3  -2  -2 |0 2 2 0 2 |2 2 2
			#     11 23 28 29 30 44(1) |12  5  1  1 14 |  0   5   2   2 -10   3 |0 1 3 1 1 |1 3 1 1
			#      3  8 16 32 34 43    | 5  8 16  2  9 | -8 -15 -12   3   4  -1 |2 1 0 2 1 |2 1 2 1
			#     14 15 16 17 38 45(1) | 1  1  1 21  7 | 11   7   0 -15   4   2 |0 4 0 1 1 |4 1 1
			#     11 30 34 35 42 44    |19  4  1  7  2 | -3  15  18  18   4  -1 |0 1 0 3 2 |1 3 2

			#   zoid width  ... 35   30   33   40   31   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                            descript tgt.dir
			#  783        1     17 [desc(-3) ] 17(?),xx,14,xx,11,xx, 8     col
			#  813        3     34       [seqReb  ] 34(?),34,16,16,...     col
			#  8131       5     46       [desc(-4) ] 46(?),42,38,34,30     col
			#  763        6     43  [same    ] 43(?), ., .,43, ., .,43     col
			#  8132       6     43              [desc1   ] 43(?),44,45     col
			#  7831       6     46  [desc1   ] 46(?),xx,45,xx,44,xx,43     col
			#  1          2     44        [symm    ] 44(?),34,17,34,44  Slide/

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
			#   dup number  8:2   11:3   16:3   30:3   34:2   42:2   43:2   44:2
			# <  8>   1      8 , 16 , 25 , 30 , 42 
			#                8 , 16 , 32 , 34 , 43 
			#          -->   8*, 16!, 39 , 38 , 44!
			# < 11>   0     11 , 23 , 28 , 29 , 30 , 44 
			#               11 , 30 , 34 , 35 , 42 , 44 
			#          -->  11*, 37 , 40 , 41 , NA , 44!
			# < 16>   0      3 ,  8 , 16 , 32 , 34 , 43 
			#               14 , 15 , 16 , 17 , 38 , 45 
			#          -->  NA , NA , 16*, NA , 42 , NA 
			# < 30>  -3     29 , 30 , 44 
			#               11 , 30 , 34 
			#          -->  NA , 30*, NA 
			# < 34>  -2     16 , 32 , 34 , 43 
			#               11 , 30 , 34 , 35 
			#          -->   6 , 28 , 34*, NA 
			# < 42>   0      8 , 16 , 25 , 30 , 42 , 43 
			#               11 , 30 , 34 , 35 , 42 , 44 
			#          -->  14 , NA , NA , 40 , 42*, 45!
			# < 43>   0      8 , 16 , 25 , 30 , 42 , 43 
			#                3 ,  8 , 16 , 32 , 34 , 43 
			#          -->  NA , NA ,  7 , 34 , 26 , 43*
			# < 44>   0     11 , 23 , 28 , 29 , 30 , 44 
			#               11 , 30 , 34 , 35 , 42 , 44 
			#          -->  11!, 37 , 40 , 41 , NA , 44*
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
			# 763        3      7 [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			# 813        3      4      [seqReb  ]  4(?), 4, 6, 6,...     col
			# 7631       6      3 [same    ]  3(?), ., ., 3, ., ., 3     col
			# 8131       6      3             [desc1   ]  3(?), 4, 5     col
			# 783        6      6 [desc1   ]  6(?),xx, 5,xx, 4,xx, 3     col
			# 1          2      4       [symm    ]  4(?), 4, 7, 4, 4  Slide/
			# 11         4      3             [desc1   ]  3(?), 4, 5 Slide\\
			# 12         5      4             [desc1   ]  4(?), 5, 6 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      763        2      7 [desc1   ]  7(?),xx,xx, 8,xx,xx, 9     col
			#      813        3      1             [same    ]  1(?), 1, 1     col
			#      7631       3     16          [sameEnd ] 16(?), 1, 1,16     col
			#      7831       3      1       [same    ]  1(?), ., 1, ., 1     col
			#      618        3      5 [sameEnd ]  5(?),xx, 1,xx, 1,xx, 5     col
			#      6181       3      5 [symm    ]  5(?), 1, 1,16, 1, 1, 5     col
			#      7632       3     16   [ptnReb   ] 16(?), 1, 1,16, 1, 1     col
			#      1          3      7             [same    ]  7(?), 7, 7  Slide/
			#      11         4      1             [same    ]  1(?), 1, 1 Slide\\
			#      12         4      5          [sameEnd ]  5(?), 1, 1, 5 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(9)   2(2)   5(3)   7(3)   8(3)   9(2)   12(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                       descript tgt.dir
			#      1        5      4      [same    ]  4(?), 4, 4, 4     col
			#      2        5    -10 [sameEnd ] -10(?), 4, 4, 4,-10     col
			#      3        5      4  [seqReb  ]  4(?), 4, 4, 4,...     col
			#      4        6      1   [desc1   ]  1(?),xx, 2,xx, 3     col
			#      5        6      3   [symm    ]  3(?),-1, 2,-1, 3     col
			#      E3       5      0   [same    ]  0(?), ., 0, ., 0 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -15(2)   -3(2)   -2(2)   -1(2)   0(2)   2(4)   3(3)   4(3)   18(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      7 13 30 39 41 45    | 6 17  9  2  4 |                        |1 1 0 2 2 |1 1 2 2
			#      4 10 11 12 20 27    | 6  1  1  8  7 | -3  -3 -19 -27 -21 -18 |1 3 2 0 0 |1 3 2
			#      1  8 11 15 18 45(1) | 7  3  4  3 27 | -3  -2   0   3  -2  18 |2 3 0 0 1 |2 3 1
			#     10 22 27 31 42 43    |12  5  4 11  1 |  9  14  16  16  24  -2 |0 1 2 1 2 |1 2 1 2
			#      5 15 20 31 34 42(2) |10  5 11  3  8 | -5  -7  -7   0  -8  -1 |1 1 1 2 1 |1 1 1 2 1
			#     12 18 24 26 39 40    | 6  6  2 13  1 |  7   3   4  -5   5  -2 |0 2 2 1 1 |2 2 1 1

			#   zoid width  ... 38   23   44   33   37   28 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                               descript tgt.dir
			#  823       4     26          [seqReb  ] 26(?),26,31,31,...     col
			#  760       5     43     [desc1   ] 43(?),xx,xx,42,xx,xx,41     col
			#  767       6     42 [seqReb  ] 42(?), .,42, .,45, .,45,...     col

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
			#   dup number  10:2   11:2   12:2   15:2   18:2   20:2   27:2   31:2   39:2   42:2   45:2
			# < 10>  -1     10 , 11 , 12 , 20 , 27 
			#               10 , 22 , 27 , 31 , 42 
			#          -->  10*, 33 , 42 , 42 , NA 
			# < 11>   0      4 , 10 , 11 , 12 , 20 , 27 
			#                1 ,  8 , 11 , 15 , 18 , 45 
			#          -->  NA ,  6 , 11*, 18 , 16 , NA 
			# < 12>  -3     12 , 20 , 27 
			#               12 , 18 , 24 
			#          -->  12*, 16 , 21 
			# < 15>  -2     11 , 15 , 18 , 45 
			#                5 , 15 , 20 , 31 
			#          -->  NA , 15*, 22 , 17 
			# < 18>  -3     15 , 18 , 45 
			#               12 , 18 , 24 
			#          -->   9 , 18*, NA 
			# < 20>  -2     11 , 12 , 20 , 27 
			#                5 , 15 , 20 , 31 
			#          -->  NA , 18 , 20*, 35 
			# < 27>  -3     12 , 20 , 27 
			#               10 , 22 , 27 
			#          -->   8 , 24 , 27*
			# < 31>   0     10 , 22 , 27 , 31 , 42 , 43 
			#                5 , 15 , 20 , 31 , 34 , 42 
			#          -->  NA ,  8 , 13 , 31*, NA , 41!
			# < 39>   1      7 , 13 , 30 , 39 , 41 
			#               18 , 24 , 26 , 39 , 40 
			#          -->  29 , 35 , 22 , 39*, NA 
			# < 42>   1     10 , 22 , 27 , 31 , 42 
			#               15 , 20 , 31 , 34 , 42 
			#          -->  20 , 18 , 35 , 37 , 42*
			# < 45>   0      7 , 13 , 30 , 39 , 41 , 45 
			#                1 ,  8 , 11 , 15 , 18 , 45 
			#          -->  NA ,  3 , NA , NA , NA , 45*

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
			# 760        2      1     [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			# 823        2     11             [desc(-3) ] 11(?), 8, 5, 2     col
			# 767        3     -1           [desc1   ] -1(?),xx, 0,xx, 1     col
			# 7671       4     -3    [desc( 4) ] -3(?),xx, 1,xx, 5,xx, 9     col
			# 8231       4      6          [seqReb  ]  6(?), 6, 1, 1,...     col
			# 7601       5      3     [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			# 7672       6      2 [seqReb  ]  2(?), ., 2, ., 5, ., 5,...     col
			# 1          4      3                 [desc1   ]  3(?), 4, 5 Slide\\
			# 11         5     -1           [desc1   ] -1(?),xx, 0,xx, 1 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      823        2      7             [desc1   ]  7(?), 6, 5     col
			#      8231       2      6      [seqReb  ]  6(?), 6, 5, 5,...     col
			#      767        4      3       [same    ]  3(?), ., 3, ., 3     col
			#      623        4      2 [sameEnd ]  2(?),xx, 3,xx, 3,xx, 2     col
			#      675        5     27       [symm    ] 27(?), 1, 8, 1,27     col
			#      1          1      6      [seqReb  ]  6(?), 6,11,11,...  Slide/
			#      11         2      1             [desc1   ]  1(?), 2, 3  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(2)   3(3)   4(3)   5(2)   6(4)   7(2)   8(2)   11(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                     descript tgt.dir
			#      1       6     -3       [desc1   ] -3(?),-2,-1     col
			#      2       6     18 [symm    ] 18(?),-2,-1,-2,18     col
			# -------------------------------------------------------------------------------------
			#     FV :    -7(2)   -5(2)   -3(3)   -2(4)   0(2)   3(2)   16(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      1 21 26 36 40 41    |20  5 10  4  1 |                        |1 0 2 1 2 |1 2 1 2
			#     12 17 23 34 42 45    | 5  6 11  8  3 | 11  -4  -3  -2   2   4 |0 2 1 1 2 |2 1 1 2
			#      3 12 13 18 31 32(1) | 9  1  5 13  1 | -9  -5 -10 -16 -11 -13 |1 3 0 2 0 |1 3 2
			#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 12   9  18  14  10  11 |0 1 1 2 2 |1 1 2 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 | -7 -10 -12 -11  -5   2 |1 2 1 1 1 |1 2 1 1 1
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  5   3   0   5   4  -2 |0 3 1 0 2 |3 1 2

			#   zoid width  ... 40   33   29   28   37   30 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  808        2     21 [same    ] 21(?), ., .,21, ., .,21     col
			#  811        2     10       [desc1   ] 10(?),xx,11,xx,12     col
			#  832        3     19             [same    ] 19(?),19,19     col
			#  8081       3     31          [sameEnd ] 31(?),19,19,31     col
			#  8082       5     42 [desc1   ] 42(?),xx,xx,41,xx,xx,40     col
			#  805        6     32       [symm    ] 32(?),43,45,43,32     col

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
			#   dup number  12:2   13:2   19:2   21:3   26:2   31:2   32:2   36:2   40:2   41:2   43:2   45:2
			# < 12>   1     12 , 17 , 23 , 34 , 42 
			#               12 , 13 , 18 , 31 , 32 
			#          -->  12*, NA , 13 , 28 , 22 
			# < 13>  -2     13 , 18 , 31 , 32 
			#               13 , 14 , 19 , 26 
			#          -->  13*, NA , NA , 20 
			# < 19>   0      8 , 11 , 19 , 21 , 36 , 45 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  18 , 17 , 19*, 31 , 44 , 41 
			# < 21>   2     15 , 21 , 31 , 32 
			#               19 , 21 , 36 , 45 
			#          -->  NA , 21*, 41 , NA 
			# < 26>   1      1 , 21 , 26 , 36 , 40 
			#               14 , 19 , 26 , 40 , 43 
			#          -->  NA , 17 , 26*, 44 , NA 
			# < 31>  -2     13 , 18 , 31 , 32 
			#               15 , 21 , 31 , 32 
			#          -->  17 , 24 , 31*, 32!
			# < 32>  -2     13 , 18 , 31 , 32 
			#               15 , 21 , 31 , 32 
			#          -->  17 , 24 , 31!, 32*
			# < 36>   1      1 , 21 , 26 , 36 , 40 
			#               11 , 19 , 21 , 36 , 45 
			#          -->  21 , 17 , 16 , 36*, NA 
			# < 40>   0      1 , 21 , 26 , 36 , 40 , 41 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  25 ,  7 , 12 , 16 , 40*, 45 
			# < 41>  -1     21 , 26 , 36 , 40 , 41 
			#               15 , 21 , 31 , 32 , 41 
			#          -->   9 , 16 , 26 , 24 , 41*
			# < 43>   0     15 , 21 , 31 , 32 , 41 , 43 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  11 ,  7 ,  7 , 20 , 39!, 43*
			# < 45>   0     12 , 17 , 23 , 34 , 42 , 45 
			#                8 , 11 , 19 , 21 , 36 , 45 
			#          -->   4 ,  5 , 15 ,  8 , 30 , 45*

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
			# 808        2      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			# 811        2      0       [desc1   ]  0(?),xx, 1,xx, 2     col
			# 832        2      4      [seqReb  ]  4(?), 4, 1, 1,...     col
			# 8321       3      9             [same    ]  9(?), 9, 9     col
			# 8081       3      1          [sameEnd ]  1(?), 9, 9, 1     col
			# 8082       5      2 [desc1   ]  2(?),xx,xx, 1,xx,xx, 0     col
			# 805        6      2       [symm    ]  2(?), 3, 5, 3, 2     col
			# 1          2      0       [desc1   ]  0(?),xx, 1,xx, 2  Slide/
			# 11         2      9      [seqReb  ]  9(?), 9, 1, 1,...  Slide/
			# 12         3      6             [same    ]  6(?), 6, 6  Slide/
			# 13         3      3          [sameEnd ]  3(?), 6, 6, 3  Slide/
			# 14         6     -1             [desc1   ] -1(?), 0, 1 Slide\\
			# 15         6      0       [desc1   ]  0(?),xx, 1,xx, 2 Slide\\
			# 16         6      0      [seqReb  ]  0(?), 0, 1, 1,... Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                               descript tgt.dir
			#      832       4     13                 [desc1   ] 13(?),14,15     col
			#      808       5      3     [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      811       5      9 [seqReb  ]  9(?), ., 9, ., 1, ., 1,...     col
			#      1         1      3           [desc1   ]  3(?),xx, 2,xx, 1  Slide/
			#      11        4      6                 [desc1   ]  6(?), 7, 8 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(2)   3(3)   5(4)   6(2)   8(2)   9(3)   10(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                        descript tgt.dir
			#      E3       1    -13 [desc1   ] -13(?),xx,-12,xx,-11  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -10(2)   -5(2)   -2(2)   2(2)   4(2)   5(2)   11(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      1 21 26 36 40 41    |20  5 10  4  1 |                        |1 0 2 1 2 |1 2 1 2
			#      3 12 13 18 31 32    | 9  1  5 13  1 |  2  -9 -13 -18  -9  -9 |1 3 0 2 0 |1 3 2
			#     14 20 23 31 37 38(1) | 6  3  8  6  1 | 11   8  10  13   6   6 |0 1 2 3 0 |1 2 3
			#      8 11 19 21 36 45    | 3  8  2 15  9 | -6  -9  -4 -10  -1   7 |1 2 1 1 1 |1 2 1 1 1
			#      9 18 20 24 27 36(1) | 9  2  4  3  9 |  1   7   1   3  -9  -9 |1 1 3 1 0 |1 1 3 1
			#      4  5 31 35 43 45    | 1 26  4  8  2 | -5 -13  11  11  16   9 |2 0 0 2 2 |2 2 2

			#   zoid width  ... 40   29   24   37   27   41 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                            descript tgt.dir
			#  822       3     17 [desc( 3) ] 17(?),xx,20,xx,23,xx,26     col
			#  806       6     38        [symm    ] 38(?),45,36,45,38     col

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
			#   dup number  18:2   20:2   21:2   31:3   36:3   45:2
			# < 18>  -2     13 , 18 , 31 , 32 
			#                9 , 18 , 20 , 24 
			#          -->   5 , 18*, NA , NA 
			# < 20>   1     14 , 20 , 23 , 31 , 37 
			#               18 , 20 , 24 , 27 , 36 
			#          -->  NA , 20*, 25!, 23 , 35!
			# < 21>   2      1 , 21 , 26 , 36 
			#               19 , 21 , 36 , 45 
			#          -->  NA , 21*, NA , NA 
			# < 31>  -1     20 , 23 , 31 , 37 , 38 
			#                4 ,  5 , 31 , 35 , 43 
			#          -->  NA , NA , 31*, 33 , NA 
			# < 36>   1      8 , 11 , 19 , 21 , 36 
			#               18 , 20 , 24 , 27 , 36 
			#          -->  28 , 29 , 29 , 33 , 36*
			# < 45>   0      8 , 11 , 19 , 21 , 36 , 45 
			#                4 ,  5 , 31 , 35 , 43 , 45 
			#          -->  NA , NA , 43 , NA , NA , 45*
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
			# 811        2      1  [same    ]  1(?), ., ., 1, ., ., 1     col
			# 829        3      2              [desc1   ]  2(?), 1, 0     col
			# 822        3     -3 [desc( 3) ] -3(?),xx, 0,xx, 3,xx, 6     col
			# 8291       4      6              [desc1   ]  6(?), 5, 4     col
			# 8221       5      7        [same    ]  7(?), ., 7, ., 7     col
			# 796        5      0  [sameEnd ]  0(?),xx, 7,xx, 7,xx, 0     col
			# 8292       6      4              [desc1   ]  4(?), 5, 6     col
			# 806        6      8        [symm    ]  8(?), 5, 6, 5, 8     col
			# 1          4      1       [seqReb  ]  1(?), 1, 8, 8,... Slide\\
			# 11         6      2              [desc1   ]  2(?), 3, 4 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      822        2      1           [desc1   ]  1(?),xx, 2,xx, 3     col
			#      829        3      4                 [same    ]  4(?), 4, 4     col
			#      811        3      2              [sameEnd ]  2(?), 4, 4, 2     col
			#      8291       5      2          [seqReb  ]  2(?), 2, 9, 9,...     col
			#      8221       5      9 [seqReb  ]  9(?), ., 9, ., 1, ., 1,...     col
			#      1          2      5                 [desc1   ]  5(?), 4, 3  Slide/
			#      11         3      7                 [desc1   ]  7(?), 8, 9  Slide/
			#      12         5      6           [symm    ]  6(?), 8, 4, 8, 6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(3)   3(3)   4(3)   5(2)   6(2)   8(3)   9(4)
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        2      6 [desc1   ]  6(?),xx, 7,xx, 8     col
			#      E4       5     21  [desc(-10) ] 21(?),11, 1,-9 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -13(2)   -9(6)   1(2)   6(2)   7(2)   11(3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      7 18 19 27 29 42    |11  1  8  2 13 |                        |1 2 2 0 1 |1 2 2 1
			#      7 12 15 24 25 43(1) | 5  3  9  1 18 |  0  -6  -4  -3  -4   1 |1 2 2 0 1 |1 2 2 1
			#      7 15 20 25 33 43(4) | 8  5  5  8 10 |  0   3   5   1   8   0 |1 1 2 1 1 |1 1 2 1 1
			#     10 14 16 18 27 28    | 4  2  2  9  1 |  3  -1  -4  -7  -6 -15 |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2

			#   zoid width  ... 35   36   36   18   31   29 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  775        1     11 [seqReb  ] 11(?), .,11, ., 7, ., 7,...     col
			#  7751       2      9    [desc( 3) ]  9(?),xx,12,xx,15,xx,18     col
			#  7752       6     41           [desc1   ] 41(?),xx,42,xx,43     col
			#  1          2     33                 [same    ] 33(?),33,33  Slide/
			#  11         2     27              [sameEnd ] 27(?),33,33,27  Slide/
			#  12         3     38                 [same    ] 38(?),38,38  Slide/
			#  13         3     28              [sameEnd ] 28(?),38,38,28  Slide/

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
			#   dup number  7:3   12:2   15:2   16:2   18:2   25:3   27:2   29:2   33:3   38:2   42:2   43:2
			# <  7>   0      7 , 12 , 15 , 24 , 25 , 43 
			#                7 , 15 , 20 , 25 , 33 , 43 
			#          -->   7*, 18 , 25 , 26!, 41 , 43!
			# < 12>   0      7 , 12 , 15 , 24 , 25 , 43 
			#               11 , 12 , 29 , 33 , 38 , 42 
			#          -->  NA , 12*, 43 , 42 , NA , 41!
			# < 15>  -1     12 , 15 , 24 , 25 , 43 
			#                7 , 15 , 20 , 25 , 33 
			#          -->   2 , 15*, 16 , 25!, 23 
			# < 16>  -2     16 , 18 , 27 , 28 
			#               16 , 25 , 33 , 38 
			#          -->  16*, 32 , 39 , NA 
			# < 18>   2      7 , 18 , 19 , 27 
			#               16 , 18 , 27 , 28 
			#          -->  NA , 18*, 35 , 29!
			# < 25>  -2     20 , 25 , 33 , 43 
			#               16 , 25 , 33 , 38 
			#          -->  12 , 25*, 33!, 33 
			# < 27>   1      7 , 18 , 19 , 27 , 29 
			#               14 , 16 , 18 , 27 , 28 
			#          -->  21 , 14 , 17!, 27*, NA 
			# < 29>  -2     19 , 27 , 29 , 42 
			#               11 , 12 , 29 , 33 
			#          -->   3 , NA , 29*, NA 
			# < 33>  -1     12 , 29 , 33 , 38 , 42 
			#               16 , 25 , 33 , 38 , 40 
			#          -->  20 , 21 , 33*, 38!, 38 
			# < 38>  -1     12 , 29 , 33 , 38 , 42 
			#               16 , 25 , 33 , 38 , 40 
			#          -->  20 , 21 , 33!, 38*, NA 
			# < 42>   0      7 , 18 , 19 , 27 , 29 , 42 
			#               11 , 12 , 29 , 33 , 38 , 42 
			#          -->  15 ,  6 , 39 , 39 , NA , 42*
			# < 43>   0      7 , 12 , 15 , 24 , 25 , 43 
			#                7 , 15 , 20 , 25 , 33 , 43 
			#          -->   7!, 18 , 25 , 26!, 41 , 43*

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
			# 775        1      1 [seqReb  ]  1(?), ., 1, ., 7, ., 7,...     col
			# 7751       2     -1    [desc( 3) ] -1(?),xx, 2,xx, 5,xx, 8     col
			# 756        2      4       [ptnReb   ]  4(?), 5, 2, 4, 5, 2     col
			# 7561       4      9     [desc1   ]  9(?),xx,xx, 8,xx,xx, 7     col
			# 7752       4      1    [desc( 2) ]  1(?),xx, 3,xx, 5,xx, 7     col
			# 694        4      5           [symm    ]  5(?), 8, 3, 8, 5     col
			# 7753       6      1           [desc1   ]  1(?),xx, 2,xx, 3     col
			# 1          2      3                 [same    ]  3(?), 3, 3  Slide/
			# 11         2      7              [sameEnd ]  7(?), 3, 3, 7  Slide/
			# 12         2      3           [same    ]  3(?), ., 3, ., 3  Slide/
			# 13         3      8              [same    ]  8(?), 8, 8, 8  Slide/
			# 14         3      8          [seqReb  ]  8(?), 8, 8, 8,...  Slide/
			# 15         4      4                 [desc1   ]  4(?), 3, 2 Slide\\
			# 16         5      7                 [desc1   ]  7(?), 8, 9 Slide\\
			# 17         6     -3             [desc( 3) ] -3(?), 0, 3, 6 Slide\\

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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      756        2      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      819        3      6             [desc1   ]  6(?), 5, 4     col
			#      775        3      3       [desc1   ]  3(?),xx, 4,xx, 5     col
			#      8191       5      6             [desc1   ]  6(?), 5, 4     col
			#      1          2      5             [same    ]  5(?), 5, 5  Slide/
			#      11         2      1          [sameEnd ]  1(?), 5, 5, 1  Slide/
			#      12         5      8       [symm    ]  8(?), 2, 4, 2, 8 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(4)   4(3)   5(6)   8(4)   9(3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        1      2 [desc1   ]  2(?),xx, 1,xx, 0     col
			#      E2       1     13       [same    ] 13(?),13,13  Slide/
			#      E4       1     -7    [sameEnd ] -7(?),13,13,-7  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -6(2)   -4(3)   0(3)   1(3)   3(3)   5(3)   13(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#     15 27 33 35 43 45    |12  6  2  8  2 |                        |0 1 1 2 2 |1 1 2 2
			#      5  7 11 16 41 45(1) | 2  4  5 25  4 |-10 -20 -22 -19  -2   0 |2 2 0 0 2 |2 2 2
			#      8  9 18 21 28 40    | 1  9  3  7 12 |  3   2   7   5 -13  -5 |2 1 2 0 1 |2 1 2 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  6   6  -2  -4  10   5 |0 4 0 1 1 |4 1 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 |-11  -5   7   7  -7  -6 |1 1 2 2 0 |1 1 2 2
			#      6  7 18 19 30 38    | 1 11  1 11  8 |  3  -3  -5  -5  -1  -1 |2 2 0 2 0 |2 2 2

			#   zoid width  ... 30   40   32   31   36   32 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  783        1     13 [desc1   ] 13(?),xx,xx,14,xx,xx,15     col
			#  784        2     11       [desc1   ] 11(?),xx,10,xx, 9     col
			#  794        5     29             [desc1   ] 29(?),30,31     col
			#  7831       6     45 [same    ] 45(?), ., .,45, ., .,45     col
			#  7941       6     37             [desc1   ] 37(?),38,39     col
			#  7841       6     38       [desc1   ] 38(?),xx,39,xx,40     col

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
			#   dup number  7:2   15:2   16:2   18:2   38:2   45:3
			# <  7>   0      5 ,  7 , 11 , 16 , 41 , 45 
			#                6 ,  7 , 18 , 19 , 30 , 38 
			#          -->  NA ,  7*, 25 , 22 , 19 , 31 
			# < 15>   1     15 , 27 , 33 , 35 , 43 
			#               15 , 16 , 17 , 38 , 45 
			#          -->  15*, NA , NA , 41 , NA 
			# < 16>  -1      7 , 11 , 16 , 41 , 45 
			#               14 , 15 , 16 , 17 , 38 
			#          -->  NA , NA , 16*, NA , 31 
			# < 18>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                6 ,  7 , 18 , 19 , 30 , 38 
			#          -->   4 ,  5 , 18*, NA , 32 , 36 
			# < 38>   1     14 , 15 , 16 , 17 , 38 
			#                7 , 18 , 19 , 30 , 38 
			#          -->  NA , 21 , 22 , NA , 38*
			# < 45>   0      5 ,  7 , 11 , 16 , 41 , 45 
			#               14 , 15 , 16 , 17 , 38 , 45 
			#          -->  23 , 23 , 21 , 18!, 35 , 45*
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
			# 783        1      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 794        5     -1             [desc1   ] -1(?), 0, 1     col
			# 7831       6      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			# 7941       6      7             [desc1   ]  7(?), 8, 9     col
			# 1          1      8       [symm    ]  8(?), 7, 3, 7, 8  Slide/
			# 11         2      0       [symm    ]  0(?), 8, 4, 8, 0  Slide/
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      776        1      1       [symm    ]  1(?), 1, 7, 1, 1     col
			#      794        3      1          [same    ]  1(?), 1, 1, 1     col
			#      7761       3      3       [sameEnd ]  3(?), 1, 1, 1, 3     col
			#      7941       3      1      [seqReb  ]  1(?), 1, 1, 1,...     col
			#      784        4      7       [same    ]  7(?), ., 7, ., 7     col
			#      691        4      8 [sameEnd ]  8(?),xx, 7,xx, 7,xx, 8     col
			#      7942       5      8             [same    ]  8(?), 8, 8     col
			#      7831       5      7          [sameEnd ]  7(?), 8, 8, 7     col
			#      1          2      1      [seqReb  ]  1(?), 1, 7, 7,...  Slide/
			#      11         5      1       [same    ]  1(?), ., 1, ., 1 Slide\\
			#      12         5     11      [seqReb  ] 11(?),11, 1, 1,... Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(7)   2(3)   4(2)   7(4)   8(3)   11(2)   12(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        3      7 [same    ]  7(?), ., 7, ., 7     col
			#      2        6     -7 [desc1   ] -7(?),xx,-6,xx,-5     col
			#      E3       4     -5       [same    ] -5(?),-5,-5 Slide\\
			#      E1       4      6    [sameEnd ]  6(?),-5,-5, 6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -5(4)   -2(2)   -1(2)   3(2)   5(2)   6(2)   7(3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

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
			#      1  4 10 12 28 45    | 3  6  2 16 17 |                        |2 2 1 0 1 |2 2 1 1
			#     17 25 28 37 43 44(1) | 8  3  9  6  1 | 16  21  18  25  15  -1 |0 1 2 1 2 |1 2 1 2
			#      1  3 12 14 16 43(1) | 2  9  2  2 27 |-16 -22 -16 -23 -27  -1 |2 3 0 0 1 |2 3 1
			#     17 21 25 26 27 36    | 4  4  1  1  9 | 16  18  13  12  11  -7 |0 1 4 1 0 |1 4 1
			#     10 21 22 30 35 42(1) |11  1  8  5  7 | -7   0  -3   4   8   6 |0 1 2 2 1 |1 2 2 1
			#      5  6 16 18 37 38    | 1 10  2 19  1 | -5 -15  -6 -12   2  -4 |2 2 0 2 0 |2 2 2

			#   zoid width  ... 44   27   42   19   32   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  820        1     10 [seqReb  ] 10(?), .,10, ., 1, ., 1,...     col
			#  830        2      6          [seqReb  ]  6(?), 6,21,21,...     col
			#  815        5     26     [desc1   ] 26(?),xx,xx,27,xx,xx,28     col
			#  8201       6     41           [desc1   ] 41(?),xx,42,xx,43     col


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
			#   dup number  1:2   10:2   12:2   16:2   17:2   21:2   25:2   28:2   37:2   43:2
			# <  1>   0      1 ,  4 , 10 , 12 , 28 , 45 
			#                1 ,  3 , 12 , 14 , 16 , 43 
			#          -->   1*,  2!, 14 , 16 ,  4 , 41 
			# < 10>  -2     10 , 12 , 28 , 45 
			#               10 , 21 , 22 , 30 
			#          -->  10*, 30 , 16 , 15 
			# < 12>  -1      4 , 10 , 12 , 28 , 45 
			#                1 ,  3 , 12 , 14 , 16 
			#          -->  NA , NA , 12*, NA , NA 
			# < 16>  -2     12 , 14 , 16 , 43 
			#                5 ,  6 , 16 , 18 
			#          -->  NA , NA , 16*, NA 
			# < 17>   0     17 , 25 , 28 , 37 , 43 , 44 
			#               17 , 21 , 25 , 26 , 27 , 36 
			#          -->  17*, NA , 22 , NA , NA , 28 
			# < 21>   0     17 , 21 , 25 , 26 , 27 , 36 
			#               10 , 21 , 22 , 30 , 35 , 42 
			#          -->   3 , 21*, NA , 34 , 43 , NA 
			# < 25>   1     17 , 25 , 28 , 37 , 43 
			#               21 , 25 , 26 , 27 , 36 
			#          -->  NA , 25*, NA , NA , 29 
			# < 28>  -2     10 , 12 , 28 , 45 
			#               17 , 25 , 28 , 37 
			#          -->  24 , NA , 28*, 29 
			# < 37>   1     17 , 25 , 28 , 37 , 43 
			#                6 , 16 , 18 , 37 , 38 
			#          -->  NA ,  7 ,  8 , 37*, NA 
			# < 43>   1     17 , 25 , 28 , 37 , 43 
			#                3 , 12 , 14 , 16 , 43 
			#          -->  NA , NA , NA , NA , 43*

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
			# 820        1     -1           [desc1   ] -1(?),xx, 0,xx, 1     col
			# 8201       1      0 [seqReb  ]  0(?), ., 0, ., 1, ., 1,...     col
			# 830        2      6          [seqReb  ]  6(?), 6, 1, 1,...     col
			# 8202       3      2           [same    ]  2(?), ., 2, ., 2     col
			# 800        3      0     [sameEnd ]  0(?),xx, 2,xx, 2,xx, 0     col
			# 8203       5      4           [desc1   ]  4(?),xx, 5,xx, 6     col
			# 815        5      6     [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			# 812        5      6           [symm    ]  6(?), 7, 5, 7, 6     col
			# 8204       6      1           [desc1   ]  1(?),xx, 2,xx, 3     col
			# 8151       6      7     [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			# 1          1      6           [symm    ]  6(?), 6, 2, 6, 6  Slide/
			# 11         5      3           [desc1   ]  3(?),xx, 2,xx, 1 Slide\\

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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                               descript tgt.dir
			#      815       1      5     [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			#      820       3      8 [seqReb  ]  8(?), ., 8, ., 2, ., 2,...     col
			#      1         3      9                 [desc1   ]  9(?),10,11 Slide\\
			#      11        4      3                 [desc1   ]  3(?), 2, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(5)   3(2)   4(2)   6(2)   8(2)   9(3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
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
			# -------------------------------------------------------------------------------------
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -16(2)   -7(2)   -1(2)   16(2)   18(2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

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





