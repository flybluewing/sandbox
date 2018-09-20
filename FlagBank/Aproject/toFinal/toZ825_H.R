# toZ825_H.R 최종접근
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

# done
fCut.basic <- function( gEnv ,allIdxF ,rpt=FALSE ){

	#=====================================================================================
	#	fCutCnt.basic() 에서 정책적으로 자르기.
	zMtx <- gEnv$zhF					# rptObj<-anaQuoTbl( zMtx )
	if( TRUE ){	# 소스코드 접을 수 있으려고..
		stdMI <- fCutU.getMtxInfo( zMtx )
		# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

		# raw special
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						if( any(aZoid==stdMI$lastZoid) ) return(FALSE)
						if( 1<sum(aZoid%in%stdMI$lastZoid) ) return(FALSE)
						if( aZoid[6]==(aZoid[2]+aZoid[4]) ) return(FALSE)	# 9+29=38
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		# -------------------------------------------------------------------------------
		# 	basic dimPlane에서 cccObj 이외 이벤트 발생은 없다고 가정.
		# -------------------------------------------------------------------------------

		# auxCntMtx
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj( aZoid )$size
						if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 0,2,2
						if( all(quoSize[1:3+2]==c(1,3,1)) ) return(FALSE)	# next rebind of 2,2,0
						if( all(quoSize[1:3+0]==c(1,2,2)) ) return(FALSE)	# next rebind of 0,2,2 reverse
						if( all(quoSize[1:3+1]==c(3,1,0)) ) return(FALSE)	# next rebind of 2,2,1
						if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,1 reverse
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		allIdxF <- allIdxF[flag]

		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						if( aZoid[1]%in%c( 9    ) ) cnt<-cnt+1
						if( aZoid[2]%in%c( 9    ) ) cnt<-cnt+1
						if( aZoid[3]%in%c(24,20 ) ) cnt<-cnt+1
						if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
						if( aZoid[5]%in%c(      ) ) cnt<-cnt+1
						if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
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
						# <24>
						if( fCutU.hasPtn(c( 2,NA,24,32,29,36),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
						# <29>
						if( fCutU.hasPtn(c(      24,29),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c( 2, 5,NA,29),aZoid) ) cnt<-cnt+1
						# <38>
						if( fCutU.hasPtn(c(      35,38),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(32,33,NA,38),aZoid) ) cnt<-cnt+1
						# <40>
						if( fCutU.hasPtn(c(   23,NA,NA,40),aZoid) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(20,NA,19,NA,40),aZoid) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						# u0.zoidMtx_ana( stdMI$rawTail%%10 )
						cnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 3,9,2     ),c(  9    )) ) cnt<-cnt+1 # 1
						if( fCutU.remFilt(aZoid[2],c( 0,9       ),c(  9    )) ) cnt<-cnt+1 # 2
						if( fCutU.remFilt(aZoid[3],c( 4,0,5,9,6 ),c( 24,20 )) ) cnt<-cnt+1 # 3
						if( fCutU.remFilt(aZoid[4],c(           ),c(       )) ) cnt<-cnt+1 # 4
						if( fCutU.remFilt(aZoid[5],c( 9,0       ),c(       )) ) cnt<-cnt+1 # 5
						if( fCutU.remFilt(aZoid[6],c( 7         ),c(       )) ) cnt<-cnt+1 # 6
						return( cnt<3 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aCStep <- aZoid[2:6]-aZoid[1:5]
						if( aCStep[1]%in%c( 9 ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
						if( aCStep[3]%in%c( 3 ) ) cnt<-cnt+1
						if( aCStep[4]%in%c( 4 ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 5 ) ) cnt<-cnt+1

						if( 1<sum(aCStep[1:3+0]==c( 5,  5,  4 )) ) cnt<-cnt+1	# 2
						if( 1<sum(aCStep[1:3+1]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
						if( 1<sum(aCStep[1:3+2]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
						if( 1<sum(aCStep[1:3+2]==c( 6,  6,  2 )) ) cnt<-cnt+1	# 4

						if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1
						if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
						if( all(aCStep[1:2+2]==c(15, 5)) ) cnt<-cnt+1
						if( all(aCStep[1:2+0]==c( 2,15)) ) cnt<-cnt+1

						# --
						if( all(aCStep[2:3]== aCStep[4]*c(3,1) ) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#     16 25 33 38 40 45    | 9  8  5  2  5 |                        |0 1 1 2 2 |1 1 2 2
					#     10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1
					#      1 12 13 24 29 44    |11  1 11  5 15 | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
					#      9 18 20 24 27 36(1) | 9  2  4  3  9 |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
					#     12 18 24 26 39 40(2) | 6  6  2 13  1 |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
					#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- 0
						aFStep <- aZoid - stdMI$lastZoid
						if( aFStep[1]%in%c(          ) ) cnt<-cnt+1
						if( aFStep[2]%in%c(  2       ) ) cnt<-cnt+1
						if( aFStep[3]%in%c(          ) ) cnt<-cnt+1
						if( aFStep[4]%in%c(  4, 0, 8 ) ) cnt<-cnt+1
						if( aFStep[5]%in%c(  2       ) ) cnt<-cnt+1
						if( aFStep[6]%in%c(          ) ) cnt<-cnt+1

						if( 1<sum(aFStep[1:2+0]==c( -6,   2     )) ) cnt<-cnt+1 # -5
						if( 1<sum(aFStep[1:3+0]==c(  8,   6,   7)) ) cnt<-cnt+1 # -9
						if( 1<sum(aFStep[1:3+1]==c( -5,  -9,   0)) ) cnt<-cnt+1 #  0
						if( 1<sum(aFStep[1:3+3]==c( -5,  -9,   0)) ) cnt<-cnt+1 #  3
						if( 1<sum(aFStep[1:3+3]==c( -6,  -6,   2)) ) cnt<-cnt+1 # -5
						if( 1<sum(aFStep[1:3+3]==c(  4,   2,  12)) ) cnt<-cnt+1 # -2
						if( all(aFStep[1:2+4]==c(-5,-1)) ) cnt<-cnt+1

						# -- 
						if( aFStep[2]==sum(aFStep[c(1,4,5,6)]) ) cnt<-cnt+1

						return( cnt<2 )
					})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]


		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("reb","spanM")]) ) return( FALSE )	# reb는 3연속상태
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

		# nextZW dimPlane에서 cccObj,auxZW/auxQuo 이외 이벤트 발생은 없다고 가정.

    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41 ) ) cnt<-cnt+1
					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,11      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,NA,25),aZoid) ) cnt<-cnt+1
					# < 9>					# <21>					# <23>
					# <24>
					if( fCutU.hasPtn(c(24,37,45),aZoid) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <35>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(13,14,24,39),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0   ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5   ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1   ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,3 ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c( 41 )) ) cnt<-cnt+1 # 6
					return( cnt<3 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1,15       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(10,12, 1,12 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1, 3       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3          ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1          ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 1,  4    )) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+1]==c(13,  1,  7)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1,  1,  4)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+2]==c( 6,  3, 11)) ) cnt<-cnt+1	#  8

					if( fCutU.hasPtn(c( 7,14),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(15,14)) ) cnt<-cnt+1

					# --
					if( aCStep[5]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(4,3)]) ) cnt<-cnt+1
					if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3)]) ) cnt<-cnt+1

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
					if( aFStep[1]%in%c( -4      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c(  3,  11,  12)) ) cnt<-cnt+1 #-12
					if( 1<sum(aFStep[1:3+2]==c(  3,  11,  12)) ) cnt<-cnt+1 #-12
					if( 1<sum(aFStep[1:3+3]==c(-30, -26, -24)) ) cnt<-cnt+1 # -2

					# --
					if( all(aFStep[3:5]== aFStep[1]*c(4,4,2) ) ) cnt<-cnt+1
					if( all(aFStep[3:5]== aFStep[6]*c(6,6,3) ) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(1,5,6)]) ) cnt<-cnt+1

					return( cnt<2 )
				})	;kIdx<-anaFltCnt(flag,rpt)
		allIdxF <- allIdxF[flag]


		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c("quoAll","nbor")]) ) return( FALSE )
						
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
						if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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
						if( 0 < sum(score[c( "reb" )]) ) return( FALSE )	# reb 4연속은 불가..
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
						if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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
						if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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
						if( 0 < sum(score[c( "quoAll" )]) ) return( FALSE )
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

		# 1.3.c fStep wild
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( 1<sum(aFStep[1:2+0]==c( -2,  -1     )) ) cnt<-cnt+1 # 13
					if( 1<sum(aFStep[1:3+0]==c( 12,   2,  -3)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+2]==c( -5,  -2,  -4)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c(  0,   2     )) ) cnt<-cnt+1 # -3

					# --
					if( all(aFStep[c(2,6)]== aFStep[4]*c(6,-3) ) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,4,5,6)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1

					return( cnt<1 )
				})	;kIdx<-anaFltCnt( flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						# if( 0 < sum(score[c( , )]) ) return( FALSE )
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
						if( 0 < sum(score[c( "reb" )]) ) return( FALSE )
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
						if( 0 < sum(score[c( "spanM" )]) ) return( FALSE )
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

		# 1.3.c fStep wild
    	flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid

					if( 1<sum(aFStep[1:3+0]==c( -5,  11,   8)) ) cnt<-cnt+1 #-11
					if( 1<sum(aFStep[1:3+1]==c(  8,   8,  13)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+2]==c( -5,  11,   8)) ) cnt<-cnt+1 #-11
					if( 1<sum(aFStep[1:3+3]==c( 11,   6,   6)) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:3+3]==c(  8,  11,   6)) ) cnt<-cnt+1 # -6

					# --
					if( all(aFStep[c(2,5)]==aFStep[c(4,6)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,2)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,4)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,2)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,4)]) ) cnt<-cnt+1

					return( cnt<1 )
				})	;kIdx<-anaFltCnt( flag,rpt)
		allIdxF <- allIdxF[flag]

		cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
		flag <- apply( cccObj$scoreMtx ,1 ,function( score ){
						score.r <- score[names(score)!="reb"]
						if( 2<sum(score.r>0) ) return( FALSE )	# std
						if( 0 < sum(score[c( "reb","spanM" )]) ) return( FALSE )
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
					if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(1,3,1)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+0]==c(1,2,2)) ) return(FALSE)	# next rebind of 0,2,2 reverse
					if( all(quoSize[1:3+1]==c(3,1,0)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+2]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,1 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 9    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 9    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(24,20 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(      ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(      ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 9>
					if( fCutU.hasPtn(c( 9,30,38,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <12>
					if( fCutU.hasPtn(c(12,NA,24      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,23,NA,23,34),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(15,18,28,28,NA,44),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <24>
					if( fCutU.hasPtn(c( 2,NA,24,32,29,36),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <29>
					if( fCutU.hasPtn(c(      24,29),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 5,NA,29),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(      35,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(32,33,NA,38),aZoid) ) cnt<-cnt+1
					# <40>
					if( fCutU.hasPtn(c(   23,NA,NA,40),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(20,NA,19,NA,40),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,9,2     ),c(  9    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,9       ),c(  9    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 4,0,5,9,6 ),c( 24,20 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(           ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 9,0       ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 7         ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 9 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 4 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 5 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 5,  5,  4 )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+1]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:3+2]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:3+2]==c( 6,  6,  2 )) ) cnt<-cnt+1	# 4

					if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(15, 5)) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 2,15)) ) cnt<-cnt+1

					# --
					if( all(aCStep[2:3]== aCStep[4]*c(3,1) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     16 25 33 38 40 45    | 9  8  5  2  5 |                        |0 1 1 2 2 |1 1 2 2
				#     10 21 22 30 35 42    |11  1  8  5  7 | -6  -4 -11  -8  -5  -3 |0 1 2 2 1 |1 2 2 1
				#      1 12 13 24 29 44    |11  1 11  5 15 | -9  -9  -9  -6  -6   2 |1 2 2 0 1 |1 2 2 1
				#      9 18 20 24 27 36(1) | 9  2  4  3  9 |  8   6   7   0  -2  -8 |1 1 3 1 0 |1 1 3 1
				#     12 18 24 26 39 40(2) | 6  6  2 13  1 |  3   0   4   2  12   4 |0 2 2 1 1 |2 2 1 1
				#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(          ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  2       ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(          ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  4, 0, 8 ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  2       ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(          ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c( -6,   2     )) ) cnt<-cnt+1 # -5
					if( 1<sum(aFStep[1:3+0]==c(  8,   6,   7)) ) cnt<-cnt+1 # -9
					if( 1<sum(aFStep[1:3+1]==c( -5,  -9,   0)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+3]==c( -5,  -9,   0)) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:3+3]==c( -6,  -6,   2)) ) cnt<-cnt+1 # -5
					if( 1<sum(aFStep[1:3+3]==c(  4,   2,  12)) ) cnt<-cnt+1 # -2
					if( all(aFStep[1:2+4]==c(-5,-1)) ) cnt<-cnt+1

					# -- 
					if( aFStep[2]==sum(aFStep[c(1,4,5,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					# [1]   11  7 20 26
					# [2]   15 21  3 11 10  8
					# [3]   26 28 25 29 34 24 11
					# [4]   13 30 44
					# [5]   38 39 31 35 24 43
					# [6]   21 30 32 40
					if( 1<sum(aZoid==c(11,15,26,13,38,21) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 7,21,28,30,39,30) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(20, 3,25,44,31,32) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(26,11,29,NA,35,40) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(NA,10,34,NA,24,NA) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(NA, 8,24,NA,43,NA) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 17, 8,35,NA,NA,31 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(       ) ) score<-score+1
					if( aZoid[2]%in%c(       ) ) score<-score+1
					if( aZoid[3]%in%c( 27,26 ) ) score<-score+1
					if( aZoid[4]%in%c(       ) ) score<-score+1
					if( aZoid[5]%in%c( 37    ) ) score<-score+1
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
					if( aZoid[1]%in%c(  5  ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 11  ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 27  ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(     ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 38,43  ) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c(23,27),aZoid) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[ idx ]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[ 1 ]]$fndMtx )
					if( all(aZoid[1:2+0]==c( 6,14)) ) cnt<-cnt+1
					if( all(aZoid[1:2+0]==c( 7,14)) ) cnt<-cnt+1
					if( all(aZoid[1:2+0]==c( 9,11)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[ 4 ]]$fndMtx )
					if( all(aZoid[1:2+3]==c(34,38)) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[ 5 ]]$fndMtx )
					if( all(aZoid[1:2+4]==c(35,43)) ) cnt<-cnt+1

					# [  1]  3 10    24 35    24 27    35 41    40 41
					# [  2]  8 21    16 21    28 30    27 29    17 38
					# [  3]  6 14    14 18    17 31    20 26    33 39
					# [  4]  5 14             29 34    34 39    34 38
					# [  5]  8 11             15 26    34 40    35 43
					# [  6]  7 11                      40 45    30 43
					# [  7]  1  8                      24 26    26 30
					# [  8] 10 11                      13 36    39 43
					# [  9]  6 14                               14 21
					# [ 10]                                     35 43
					# [ 11]                                     17 36
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c( 5     ),c(  5    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 1     ),c( 11    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 7     ),c( 27    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 0     ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 9,5,7 ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 8,3   ),c( 38,43 )) )	remCnt <- remCnt+1
						# grp 1
						# grp 2
						if( aZoid[2]==11 && fCutU.remFilt(aZoid[1],c( 9),c( 9)) ) remCnt <- remCnt+1 
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	[1]  7  13   8   9   3   4   7   1   8 
					#	[2] 11   5   4 
					#	[3]  3   2  14   5  11 
					#	[4]  6   2   6   5   6   5   2  23 
					#	[5]  1  21   6   4   8  13   4   4   7   8  19
					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c( 7,11, 3, 6, 1 ),na.rm=T)
					matCnt <- sum(aCStep==c(13, 5, 2, 2,21 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 8, 4,14, 6, 6),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 9,NA, 5, 5, 4),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 3,NA,11, 6, 8),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 4,NA,NA, 5,13),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 7,NA,NA, 2, 4),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 1,NA,NA,23, 4),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )

					cnt <- 0
						if( aCStep[1]%in%c(      ) ) cnt<-cnt+1
						if( aCStep[2]%in%c( 4    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
						if( aCStep[4]%in%c( 2, 5 ) ) cnt<-cnt+1
						if( aCStep[5]%in%c(      ) ) cnt<-cnt+1

						if( aCStep[1]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1
						if( sum(aCStep[c(2,3)])==sum(aCStep[c(1,4,5)]) ) cnt<-cnt+1

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
						if( aCStep[1]%in%c(      ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(      ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c(      ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c( 2, 3 ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(      ) ) tCnt<-tCnt+1

						if( all(aCStep[c(1,3)]==c(4,3)) ) cnt<-cnt+1

						if( aCStep[1]==sum(aCStep[3:5]) ) cnt<-cnt+1
						if( aCStep[2]==sum(aCStep[3:5]) ) cnt<-cnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

						# [1]*   5  1 20 10  1  5  5  2  4  7  4  6  3  1  3 11  1  3  6 20  2 13  6  4  5  9 19 10  1  5
						# [2]    6 11 20
						# [3]*   5 12  4 15 11  1  9 10  9  1 19 22  1  8  2 14  8  5  2  6  2 20  1  3  5  2  1  4  1  1  9  4 11  3
						# [4]    8  2  7  2 11  3 16  1  8
						# [5]*   9 26 11  3 24  1  8  7  8  3  9  2  3  1 17  6  1  1  7  7  6  1 16 17 10 20 11  5  2  8 13  1  9  4  4  1  8  3  5  2  1 29  7  2 10 19  1 12
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
					# [  1]  2  2    15 11     8  7     5  3
					# [  2] 20  1    21  3     8  5    14  1
					# [  3]  2 21     4 13    30  1     7 12
					# [  4]  3  8    19  1     7  6     4  1
					# [  5]  4  9    12  1     6  1     4 13
					# [  6] 20  2    13  3     2  3     2  4
					# [  7] 15  7              7  8     1 21
					if( aCStep[1]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 8,30, 9 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 7       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 4    ) ) cnt<-cnt+1

					# anaMtx_ColVal( cvSeqNextLst[[1]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[2]]$fndMtx )
					# anaMtx_ColVal( cvSeqNextLst[[3]]$fndMtx )
					# if( all(aCStep[1:2+2]==c( , )) ) cnt<-cnt+1
					# anaMtx_ColVal( cvSeqNextLst[[4]]$fndMtx )
					if( all(aCStep[1:2+3]==c( 4, 1)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 5, 4)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 6,12)) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 4,13)) ) cnt<-cnt+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 37 ) ) return( FALSE )
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
					if( aZoid[3]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,11      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,NA,25),aZoid) ) cnt<-cnt+1
					# < 9>					# <21>					# <23>
					# <24>
					if( fCutU.hasPtn(c(24,37,45),aZoid) ) cnt<-cnt+1
					# <33>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <35>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(33,36),aZoid) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c(13,14,24,39),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0   ),c(    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5   ),c(    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1   ),c(    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 8,3 ),c(    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c( 41 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1,15       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(10,12, 1,12 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1, 3       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 3          ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1          ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 1,  4    )) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+1]==c(13,  1,  7)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1,  1,  4)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+2]==c( 6,  3, 11)) ) cnt<-cnt+1	#  8

					if( fCutU.hasPtn(c( 7,14),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(15,14)) ) cnt<-cnt+1

					# --
					if( aCStep[5]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(4,3)]) ) cnt<-cnt+1
					if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     24 25 33 34 38 39    | 1  8  1  4  1 |                        |0 0 2 4 0 |2 4
				#     20 30 33 35 36 44(1) |10  3  2  1  8 | -4   5   0   1  -2   5 |0 0 1 4 1 |1 4 1
				#      1  2  3  9 12 23    | 1  1  6  3 11 |-19 -28 -30 -26 -24 -21 |4 1 1 0 0 |4 1 1
				#      8  9 18 21 28 40(1) | 1  9  3  7 12 |  7   7  15  12  16  17 |2 1 2 0 1 |2 1 2 1
				#      6 21 35 36 37 41(1) |15 14  1  1  4 | -2  12  17  15   9   1 |1 0 1 3 1 |1 1 3 1
				#      3 10 23 24 31 39    | 7 13  1  7  8 | -3 -11 -12 -12  -6  -2 |1 1 2 2 0 |1 1 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -4      ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+1]==c(  3,  11,  12)) ) cnt<-cnt+1 #-12
					if( 1<sum(aFStep[1:3+2]==c(  3,  11,  12)) ) cnt<-cnt+1 #-12
					if( 1<sum(aFStep[1:3+3]==c(-30, -26, -24)) ) cnt<-cnt+1 # -2

					# --
					if( all(aFStep[3:5]== aFStep[1]*c(4,4,2) ) ) cnt<-cnt+1
					if( all(aFStep[3:5]== aFStep[6]*c(6,6,3) ) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(1,5,6)]) ) cnt<-cnt+1

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

	return( list( flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+0]==c(0,4,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(4,2,0)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+2]==c(2,0,0)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  4, 9 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 10, 9 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 20    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 33,12 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 37    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 2>
					if( fCutU.hasPtn(c( 2,NA,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2,15,14,27,37   ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 3>
					if( fCutU.hasPtn(c( 3,10            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,NA,30,37,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c( 3,10            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,10            ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,NA,NA,33   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,NA,NA,NA,37),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,15,33      ),aZoid) ) cnt<-cnt+1
					# <11>
					# <13>
					if( fCutU.hasPtn(c( 3,NA,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      13,30,37,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <39>
					if( fCutU.hasPtn(c( 2,NA,NA,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   19,13,26,NA,39),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 4,0   ),c(  4, 9 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,9,3 ),c( 10, 9 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7     ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 0     ),c( 20    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3,2   ),c( 33,12 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 7,0   ),c( 37    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 7, 6       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2, 3       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 8,13       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4, 1, 8, 3 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 4,  2,  2)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+0]==c( 8,  1,  8)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 3, 13,  8)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c(13,  8,  4)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 7, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 4, 2)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 3),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(16, 4),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 8, 4)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7, 1),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(16, 4),aCStep) ) cnt<-cnt+1

					# --
					if( aCStep[1]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      2  5  8 11 33 39    | 3  3  3 22  6 |                        |3 1 0 2 0 |3 1 2
				#      3 10 13 22 31 32    | 7  3  9  9  1 |  1   5   5  11  -2  -7 |1 2 1 2 0 |1 2 1 2
				#     10 14 16 18 27 28(1) | 4  2  2  9  1 |  7   4   3  -4  -4  -4 |0 4 2 0 0 |4 2
				#      1  9 12 23 39 43    | 8  3 11 16  4 | -9  -5  -4   5  12  15 |2 1 1 1 1 |2 1 1 1 1
				#      2 10 11 19 35 39(1) | 8  1  8 16  4 |  1   1  -1  -4  -4  -4 |1 3 0 2 0 |1 3 2
				#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   2   7  -1  -1 |1 2 1 2 0 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  1, -9  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -1      ) ) cnt<-cnt+1
					if( aFStep[3]%in%c( -4      ) ) cnt<-cnt+1
					if( aFStep[4]%in%c( -4,  3  ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -4      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c( -1      ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(  1,   0,   2 )) ) cnt<-cnt+1 #  1
					if( 1<sum(aFStep[1:3+1]==c(  4,   4,   4 )) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+3]==c( -9,  -5,  -4 )) ) cnt<-cnt+1 #  7
					if( 1<sum(aFStep[1:3+3]==c(  0,   2,   7 )) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c(  1,   0,   2 )) ) cnt<-cnt+1 # -1

					# --
					if( aFStep[2]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,5,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 36 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+1]==c(0,1,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,2,1
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
					if( aZoid[3]%in%c( 24    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 38    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 40,36 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,14         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,NA,16),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,13,NA,NA,41,36),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 7>
					if( fCutU.hasPtn(c( 7,40,42      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,NA,NA,29   ),aZoid) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,NA,NA,36),aZoid) ) cnt<-cnt+1
					# <14>
					if( fCutU.hasPtn(c( 3,NA,14         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      14,16      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,14,NA,41,36),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <16>
					if( fCutU.hasPtn(c( 3,NA,NA,16),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      14,16      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,NA,16,41,36),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(30,37),aZoid) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(10,NA,NA,36),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c( 4,NA,NA,24,38),aZoid) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c(            41,43),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,27,36,33,41   ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c( 4,34,25,24,35,44),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3       ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5,2     ),c( 24    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6,9,7,8 ),c( 38    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0,6     ),c( 40,36 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(         ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 4,2,6 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1     ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1     ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 8,  6    )) ) cnt<-cnt+1	# 20
					if( 1<sum(aCStep[1:3+1]==c( 2,  1,  8)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c( 2, 20,  2)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 4,  2, 20)) ) cnt<-cnt+1	#  6

					if( fCutU.hasPtn(c(20, 2),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 8, 6)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4,17),aCStep) ) cnt<-cnt+1

					# --
					if( all(aCStep[4:5]== aCStep[2]*c(4,3) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7 13 30 39 41 45    | 6 17  9  2  4 |                        |1 1 0 2 2 |1 1 2 2
				#      3  7 14 16 31 40(1) | 4  7  2 15  9 | -4  -6 -16 -23 -10  -5 |2 2 0 1 1 |2 2 1 1
				#     10 20 33 36 41 44    |10 13  3  5  3 |  7  13  19  20  10   4 |0 1 1 2 2 |1 1 2 2
				#      1  6 11 28 34 42    | 5  5 17  6  8 | -9 -14 -22  -8  -7  -2 |2 1 1 1 1 |2 1 1 1 1
				#      3 10 14 16 36 38    | 7  4  2 20  2 |  2   4   3 -12   2  -4 |1 3 0 2 0 |1 3 2
				#      7 27 29 30 38 44(1) |20  2  1  8  6 |  4  17  15  14   2   6 |1 0 2 2 1 |1 2 2 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  2, -7  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( 17,  15,  14)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+2]==c( -2,  -4,  -3)) ) cnt<-cnt+1 # 14
					if( 1<sum(aFStep[1:3+3]==c( 14,   2,   6)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c( -7, -13     )) ) cnt<-cnt+1 #  6

					# --
					if( aFStep[2]==sum(aFStep[c( 3, 5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c( 1, 5)]) ) cnt<-cnt+1
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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  9,19 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 25,38 ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 35    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <25>
					if( fCutU.hasPtn(c(      19,25   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 5,NA,25,29),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c(      30,31      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      30,NA,33   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      30,NA,NA,39),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(21,19,30         ),aZoid) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c( 6,18,34),aZoid) ) cnt<-cnt+1
					# <35>
					if( fCutU.hasPtn(c(13, 9,20,NA,35),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 5   ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 7,9 ),c(  9,19 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 5,8 ),c( 25,38 )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 5,3 ),c( 35    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 7   ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 5,6 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 1   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(10   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 8,  7,  9)) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 7,  9,  1)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+1]==c(10,  4,  2)) ) cnt<-cnt+1	# 7
					if( 1<sum(aCStep[1:2+3]==c(10,  4    )) ) cnt<-cnt+1	# 9

					if( fCutU.hasPtn(c(2,2),aCStep) ) cnt<-cnt+1

					# --
					if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#      7 27 30 33 35 37    |20  3  3  2  2 |                        |1 0 1 4 0 |1 1 4
					#     14 23 30 32 34 38(1) | 9  7  2  2  4 |  7  -4   0  -1  -1   1 |0 1 1 4 0 |1 1 4
					#      1 11 15 17 25 39    |10  4  2  8 14 |-13 -12 -15 -15  -9   1 |1 3 1 1 0 |1 3 1 1
					#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  5  -1   3   8   9  -4 |1 2 1 2 0 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-15, -15,  -9)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+3]==c( -8,  -9,   4)) ) cnt<-cnt+1 #  9

					# --
					if( aFStep[1]==sum(aFStep[c(5,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( (aZoid[6]-aZoid[1]) %in% c( 38 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 1       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(         ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(39,13    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(41,44,14 ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 5>					# <13>
					# <14>
					if( fCutU.hasPtn(c( 9,14,16),aZoid) ) cnt<-cnt+1
					# <21>
					# <27>
					if( fCutU.hasPtn(c(27,31),aZoid) ) cnt<-cnt+1
					# <30>
					if( fCutU.hasPtn(c( 9,NA,NA,30   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   11,25,30,44),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c( 1,10,27,40,39,41),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
					# <44>
					if( fCutU.hasPtn(c(37,33,42,44),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 1,7 ),c(  1       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(          )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 6   ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 2,9 ),c( 39,13    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1,4 ),c( 41,44,14 )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 1, 5,11 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 1, 9    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 4       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 8       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3, 2    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c(11,  8,  3)) ) cnt<-cnt+1	# 5
					if( 1<sum(aCStep[1:3+1]==c( 3,  7, 20)) ) cnt<-cnt+1	# 8 
					if( 1<sum(aCStep[1:3+2]==c(11,  8,  3)) ) cnt<-cnt+1	# 3

					if( fCutU.hasPtn(c( 5, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 1,12)) ) cnt<-cnt+1

					# --
					if( aCStep[3]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1
					if( aCStep[2]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#      5 10 13 27 37 41    | 5  3 14 10  4 |                        |1 2 1 1 1 |1 2 1 1 1
					#      5 21 27 34 44 45(2) |16  6  7 10  1 |  0  11  14   7   7   4 |1 0 2 1 2 |1 2 1 2
					#     13 14 26 28 30 36    | 1 12  2  2  6 |  8  -7  -1  -6 -14  -9 |0 2 2 2 0 |2 2 2
					#      7 27 29 30 38 44(1) |20  2  1  8  6 | -6  13   3   2   8   8 |1 0 2 2 1 |1 2 2 1
					#      5  6 11 14 21 41    | 1  5  3  7 20 | -2 -21 -18 -16 -17  -3 |2 2 1 0 1 |2 2 1 1
					#      3  8 19 27 30 41(1) | 5 11  8  3 11 | -2   2   8  13   9   0 |2 1 1 1 1 |2 1 1 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c( -2, -6,  2,-14 ) ) cnt<-cnt+1
					if( aFStep[2]%in%c( -9             ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(                ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(                ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  8             ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(                ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -2,   2,   8)) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+0]==c(-18, -16, -17)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:3+1]==c(-16, -17,  -3)) ) cnt<-cnt+1 #  8
					if( 1<sum(aFStep[1:3+2]==c( -2, -21, -18)) ) cnt<-cnt+1 # 13

					# --
					if( aFStep[6]==sum(aFStep[c( 1, 2)]) ) cnt<-cnt+1
					if( sum(aFStep[c( 2, 5)])==sum(aFStep[c( 1, 4)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+2]==c(0,4,2)) ) return(FALSE)	# next rebind of 1,2,1
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 23,16    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 15, 9,17 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 23       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <16>
					if( fCutU.hasPtn(c(16,NA,NA,24),aZoid) ) cnt<-cnt+1
					# <17>
					if( fCutU.hasPtn(c(17,NA,18),aZoid) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c(12,NA,18      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   15,18      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      18,19   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      18,NA,25),aZoid) ) cnt<-cnt+1
					# <19>
					if( fCutU.hasPtn(c(      18,19   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12,NA,NA,19   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   15,NA,19,NA),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(         19,25),aZoid) ) cnt<-cnt+1
					# <21>					# <23>
					# <24>
					if( fCutU.hasPtn(c(16,NA,NA,24),aZoid) ) cnt<-cnt+1
					# <39>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,6     ),c( 23,16    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5,9,7   ),c( 15, 9,17 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,0     ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 9,1,4,7 ),c(          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4,3     ),c( 23       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 5       ),c(          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 11, 5 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(  1    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 10, 5 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  4    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(  6, 7 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 5,  2,  1)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c(14, 10,  6)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c(10,  6,  5)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:2+3]==c( 5,  2    )) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c( 5, 2),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(14,10)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(10, 6)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(14, 9),aCStep) ) cnt<-cnt+1

					# --
					if( all(aCStep[4:5]== aCStep[1]*c(1,3) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      2 17 19 20 34 45    |15  2  1 14 11 |                        |1 2 1 1 1 |1 2 1 1 1
				#      7 12 19 21 29 32(1) | 5  7  2  8  3 |  5  -5   0   1  -5 -13 |1 2 2 1 0 |1 2 2 1
				#     10 17 18 19 23 27(1) | 7  1  1  4  4 |  3   5  -1  -2  -6  -5 |0 4 2 0 0 |4 2
				#      5 16 21 23 24 30(1) |11  5  2  1  6 | -5  -1   3   4   1   3 |1 1 3 1 0 |1 1 3 1
				#      4  9 23 33 39 44(1) | 5 14 10  6  5 | -1  -7   2  10  15  14 |2 0 1 2 1 |2 1 2 1
				#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7   7  -5 -14 -15  -5 |0 4 1 1 0 |4 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  1      ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -7,   5,  14)) ) cnt<-cnt+1 #  7
					if( 1<sum(aFStep[1:3+1]==c( -7,   5,  14)) ) cnt<-cnt+1 #  7
					if( 1<sum(aFStep[1:3+2]==c( -1,  -7,   2)) ) cnt<-cnt+1 # -5
					if( 1<sum(aFStep[1:3+1]==c( 14,  15,   5)) ) cnt<-cnt+1 #-14
					if( 1<sum(aFStep[1:3+3]==c( 14,  15,   5)) ) cnt<-cnt+1 #-15
					if( 1<sum(aFStep[1:3+3]==c(  4,   1,   3)) ) cnt<-cnt+1 # -5

					# --
					if( all(aFStep[c(2,4)]== aFStep[1]*c( 1,-2) ) ) cnt<-cnt+1
					if( all(aFStep[c(5,6)]== aFStep[3]*c( 3, 1) ) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+1]==c(2,0,0)) ) return(FALSE)	# next rebind of 2,0,0 reverse
					if( all(quoSize[1:3+2]==c(0,0,2)) ) return(FALSE)	# next rebind of 0,0,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  2, 1, 6 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 10       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(  7       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 11       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 40       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3,NA,11),aZoid) ) cnt<-cnt+1
					# < 4>
					# < 8>
					if( fCutU.hasPtn(c( 2, 8),aZoid) ) cnt<-cnt+1
					# <10>
					# <16>
					if( fCutU.hasPtn(c( 6,NA,NA,16),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    6,NA,16),aZoid) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c( 2,NA,NA,NA,34   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            34,36),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 2, 1, 6, 9 ),c(  2, 1, 6 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3, 0, 9    ),c( 10       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 7, 6       ),c(  7       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 1, 6, 0    ),c( 11       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 0, 7       ),c( 40       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 0          ),c(          )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2, 7, 4, 9 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 3          ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 6, 4       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(30, 5, 7    ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 4          ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2,  4,  5)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 4,  5, 25)) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+1]==c( 2,  4,  5)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 2,  4,  5)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 7, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 4,30 )) ) cnt<-cnt+1

					# --
					if( all(aCStep[c(2,5)]== aCStep[1]*c(2,2) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      4 18 26 33 34 38    |14  8  7  1  4 |                        |1 1 1 3 0 |1 1 1 3
				#      3  6 10 30 34 37(1) | 3  4 20  4  3 | -1 -12 -16  -3   0  -1 |2 1 0 3 0 |2 1 3
				#      4  8  9 16 17 19    | 4  1  7  1  2 |  1   2  -1 -14 -17 -18 |3 3 0 0 0 |3 3
				#      3 10 13 22 31 32    | 7  3  9  9  1 | -1   2   4   6  14  13 |1 2 1 2 0 |1 2 1 2
				#      1  3  8 12 42 43(1) | 2  5  4 30  1 | -2  -7  -5 -10  11  11 |3 1 0 0 2 |3 1 2
				#      5  7 11 16 41 45    | 2  4  5 25  4 |  4   4   3   4  -1   2 |2 2 0 0 2 |2 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -5, -10,  11)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+0]==c( -7,  -5, -10)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:3+1]==c(  1,  14,  17)) ) cnt<-cnt+1 #  3
					if( 1<sum(aFStep[1:3+2]==c( -7,  -5, -10)) ) cnt<-cnt+1 #  4
					if( 1<sum(aFStep[1:2+4]==c( -2,  -7     )) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:2+4]==c( -2,  -7     )) ) cnt<-cnt+1 #  2

					# --
					if( all(aFStep[1:2] == aFStep[4]*c(1,1)) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(1,5)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(2,5)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(4,5)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(3,5)]) ) cnt<-cnt+1
					if( aFStep[1]==sum(aFStep[c(3,5,6)]) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,5,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(3,5,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+0]==c(2,0,0)) ) return(FALSE)	# next rebind of 0,2,3
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,3,1 reverse
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 10,24 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 20    ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 6>
					if( fCutU.hasPtn(c( 6,NA,17         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6,14,NA,24,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,NA,34,43,43),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c(10,13         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,NA,33   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   13,30,NA,40),aZoid) ) cnt<-cnt+1
					# <17>
					if( fCutU.hasPtn(c( 6,NA,17         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   14,17,24,NA,45),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(      16,21   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 2, 7,NA,21,39),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <29>
					# <32>
					if( fCutU.hasPtn(c( 7, 4,21,32),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(31,36),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7   ),c(       )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 0,4 ),c( 10,24 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 0   ),c( 20    )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 3,2 ),c(       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 4, 13,  3)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 1, 10,  3)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 5,  4, 13)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 6,  5,  4)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 11, 3 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  2,13 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  6, 3 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(  7, 4 ),aCStep) ) cnt<-cnt+1

					# --
					if( all(aCStep[1]==aCStep[4:5]) ) cnt<-cnt+1
					if( aCStep[3]==sum(aCStep[c(1,4,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      3 10 13 22 31 32    | 7  3  9  9  1 |                        |1 2 1 2 0 |1 2 1 2
				#      9 33 36 40 42 43    |24  3  4  2  1 |  6  23  23  18  11  11 |1 0 0 2 3 |1 2 3
				#      6 10 17 18 21 29    | 4  7  1  3  8 | -3 -23 -19 -22 -21 -14 |1 3 2 0 0 |1 3 2
				#      6 12 17 21 34 37(3) | 6  5  4 13  3 |  0   2   0   3  13   8 |1 2 1 2 0 |1 2 1 2
				#      1 10 13 26 32 36    | 9  3 13  6  4 | -5  -2  -4   5  -2  -1 |1 2 1 2 0 |1 2 1 2
				#     14 15 25 28 29 30    | 1 10  3  1  1 | 13   5  12   2  -3  -6 |0 2 3 1 0 |2 3 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -4, -3  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:2+0]==c( -2,  -1     )) ) cnt<-cnt+1 # 13
					if( 1<sum(aFStep[1:3+0]==c( 12,   2,  -3)) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+2]==c( -5,  -2,  -4)) ) cnt<-cnt+1 #  2
					if( 1<sum(aFStep[1:2+4]==c(  0,   2     )) ) cnt<-cnt+1 # -3

					# --
					if( all(aFStep[c(2,6)]== aFStep[4]*c(6,-3) ) ) cnt<-cnt+1
					if( aFStep[2]==sum(aFStep[c(3,4,5,6)]) ) cnt<-cnt+1
					if( aFStep[3]==sum(aFStep[c(1,4,5)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+0]==c(1,3,1)) ) return(FALSE)	# next rebind of 3,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  7,10 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 24    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
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
					# <24>
					# <30>
					if( fCutU.hasPtn(c(11,16,30),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 7,4,0,3   ),c(  7,10 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 5         ),c(       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(           ),c(       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 3,1,5     ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4,1,3,0,9 ),c( 24    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(           ),c(       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 2, 1    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 5       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c( 2, 5, 1 ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 6       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 2, 15,  5)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c( 2, 15,  5)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 2, 15,  5)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 2, 15,  5)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 5, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 6),aCStep) ) cnt<-cnt+1

					# --
					if( all(aCStep[2:3]== aCStep[4]*c(3,1) ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      5 16 21 23 24 30    |11  5  2  1  6 |                        |1 1 3 1 0 |1 1 3 1
				#      4  5  6 12 25 37(1) | 1  1  6 13 12 | -1 -11 -15 -11   1   7 |3 1 1 1 0 |3 1 1 1
				#     17 20 30 31 33 45    | 3 10  1  2 12 | 13  15  24  19   8   8 |0 1 1 3 1 |1 1 3 1
				#      1  2 15 19 24 36    | 1 13  4  5 12 |-16 -18 -15 -12  -9  -9 |2 2 1 1 0 |2 2 1 1
				#      1  3  8 12 42 43(1) | 2  5  4 30  1 |  0   1  -7  -7  18   7 |3 1 0 0 2 |3 1 2
				#      7  9 24 29 34 38    | 2 15  5  5  4 |  6   6  16  17  -8  -5 |2 0 2 2 0 |2 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(   6, -9 ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+2]==c(  0,  -1,   7)) ) cnt<-cnt+1 # 16
					if( 1<sum(aFStep[1:3+3]==c( 12,   9,   9)) ) cnt<-cnt+1 # -8

					# --
					if( sum(aFStep[c(1,2)])==sum(aFStep[c(4,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+0]==c(0,2,0)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(4,0,2)) ) return(FALSE)	# next rebind of 2,1,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  3,17 ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 28    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 12    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 44    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 3>
					if( fCutU.hasPtn(c( 3, 8         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,NA,10,10,14),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 9>
					if( fCutU.hasPtn(c( 9,NA,12   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,NA,NA,25),aZoid) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt<-cnt+1
					# <14>
					if( fCutU.hasPtn(c(10,14),aZoid) ) cnt<-cnt+1
					# <25>
					if( fCutU.hasPtn(c( 9,NA,NA,25),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      12,25),aZoid) ) cnt<-cnt+1
					# <27>
					if( fCutU.hasPtn(c(   22,27         ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      27,NA,NA,42),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(13,NA,27,41,33   ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(         37,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3, 5, 5,NA,38),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <43>

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 3,7,5   ),c(  3,17 )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(         ),c(  8    )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 8       ),c( 28    )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(         ),c(       )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4,9,2,1 ),c( 12    )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 4       ),c( 44    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 5, 3, 4, 1 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2, 4, 5    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 2, 1       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(            ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 2, 1       ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 7,  4,  2)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c( 2, 20,  2)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+0]==c( 8,  3,  5)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c(6,2),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(8,4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(5,2),aCStep) ) cnt<-cnt+1

					# --
					if( all(aCStep[c(1,5)]== aCStep[2]*c(2,6)) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#     11 18 26 27 40 41    | 7  8  1 13  1 |                        |0 2 2 0 2 |2 2 2
				#      9 10 14 25 27 31(1) | 1  4 11  2  4 | -2  -8 -12  -2 -13 -10 |1 2 2 1 0 |1 2 2 1
				#     21 24 27 29 43 44(1) | 3  3  2 14  1 | 12  14  13   4  16  13 |0 0 4 0 2 |4 2
				#     17 23 27 35 38 43(2) | 6  4  8  3  5 | -4  -1   0   6  -5  -1 |0 1 2 2 1 |1 2 2 1
				#      3 10 14 16 36 38(1) | 7  4  2 20  2 |-14 -13 -13 -19  -2  -5 |1 3 0 2 0 |1 3 2
				#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  0  -1  -2  -3 -11   5 |2 2 1 0 1 |2 2 1 1
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  -4 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(     ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(     ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c(-13, -19,  -2)) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+0]==c(-14, -13, -13)) ) cnt<-cnt+1 # -1
					if( 1<sum(aFStep[1:3+1]==c( -3, -11,   5)) ) cnt<-cnt+1 # -2
					if( 1<sum(aFStep[1:3+3]==c(  3,  11,  15)) ) cnt<-cnt+1 #  5

					# --
					if( aFStep[1]==sum(aFStep[c(3,4,6)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,2 reverse
					if( all(quoSize[1:3+0]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,0,2
					if( all(quoSize[1:3+1]==c(0,3,2)) ) return(FALSE)	# next rebind of 0,2,1
					if( all(quoSize[1:3+2]==c(3,2,0)) ) return(FALSE)	# next rebind of 2,1,2
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(  6,37    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 21,17,27 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[4]%in%c( 36,30,41 ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 42       ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 41,44    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# <15>
					if( fCutU.hasPtn(c(15,25,43,43,NA,41),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <21>
					if( fCutU.hasPtn(c(21,NA,NA,42   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(21,36,28,NA,45),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <27>
					# <29>
					if( fCutU.hasPtn(c(      29,NA,38   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,NA,29,36,NA,40),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <36>
					if( fCutU.hasPtn(c(21,NA,36      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(      36,NA,41),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   17,36,43   ),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(      29,NA,38   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(15,NA,NA,36,38,40),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
					# <41>
					if( fCutU.hasPtn(c( 9,16,26,24,41),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6,5,7     ),c(  6,37    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 1,7       ),c( 21,17,27 )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 9,7,1     ),c(          )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 6,0,2,1,5 ),c( 36,30,41 )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 2,6       ),c( 42       )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 1,4,6     ),c( 41,44    )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(10, 6 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 2    ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(10, 3 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 3    ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 4,  1    )) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+0]==c(10,  1,  9)) ) cnt<-cnt+1	# 10
					if( 1<sum(aCStep[1:3+0]==c( 1,  9,  2)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c(20,  5, 10)) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 7,10 ),aCStep) ) cnt<-cnt+1

					# -- 
					if( all(aCStep[1:2]== aCStep[5]*c(3,5) ) ) cnt<-cnt+1
					if( aCStep[2]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
				#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
				#      7 27 29 30 38 44    |20  2  1  8  6 |                        |1 0 2 2 1 |1 2 2 1
				#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  4 -15   0   3   0  -2 |0 2 1 2 1 |2 1 2 1
				#      6 21 35 36 37 41    |15 14  1  1  4 | -5   9   6   3  -1  -1 |1 0 1 3 1 |1 1 3 1
				#     15 17 19 21 27 45(1) | 2  2  2  6 18 |  9  -4 -16 -15 -10   4 |0 3 2 0 1 |3 2 1
				#      1 21 26 36 40 41(1) |20  5 10  4  1 |-14   4   7  15  13  -4 |1 0 2 1 2 |1 2 1 2
				#     15 21 31 32 41 43(2) | 6 10  1  9  2 | 14   0   5  -4   1   2 |0 1 1 2 2 |1 1 2 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(    ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  8 ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  6 ) ) cnt<-cnt+1
					if( aFStep[5]%in%c( -5 ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(    ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -14,   0,  -5 )) ) cnt<-cnt+1 # 14
					if( 1<sum(aFStep[1:3+0]==c(   9,   6,   3 )) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+0]==c(   3,  -1,  -1 )) ) cnt<-cnt+1 #  0
					if( 1<sum(aFStep[1:3+2]==c(  -9,   4,  16 )) ) cnt<-cnt+1 #  5
					if( 1<sum(aFStep[1:3+1]==c(  -4,   1,   2 )) ) cnt<-cnt+1 # -4
					if( 1<sum(aFStep[1:3+3]==c(  15,  10,  -4 )) ) cnt<-cnt+1 #  1

					# --
					if( all(aFStep[c(1,4)]== aFStep[6]*c(7,-2) ) ) cnt<-cnt+1
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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

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
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+1]==c(1,2,0)) ) return(FALSE)	# next rebind of 2,1,2
					if( all(quoSize[1:3+2]==c(2,1,2)) ) return(FALSE)	# next rebind of 1,2,0
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1


	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c( 26,10    ) ) cnt<-cnt+1
					if( aZoid[2]%in%c( 13       ) ) cnt<-cnt+1
					if( aZoid[3]%in%c( 26       ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 34,26,24 ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 38       ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < 1>
					if( fCutU.hasPtn(c( 1,35,36,NA,NA,44),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# < 3>
					if( fCutU.hasPtn(c( 3, 7,10,28,42),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
					# <10>
					if( fCutU.hasPtn(c(   10,NA,24      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(   10,NA,NA,34   ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,10,23,NA,NA,32),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
					# <13>
					if( fCutU.hasPtn(c(13,36,44),aZoid) ) cnt<-cnt+1
					# <16>
					if( fCutU.hasPtn(c( 5,NA,16,30,18,20),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
					# <18>
					if( fCutU.hasPtn(c( 5,NA,18      ),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(    4,18,NA,30),aZoid) ) cnt<-cnt+1
					# <26>
					if( fCutU.hasPtn(c(26,38,42),aZoid) ) cnt<-cnt+1
					# <34>
					if( fCutU.hasPtn(c(   10,NA,NA,34),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(         24,34),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,NA,23,NA,34),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(            34,38),aZoid) ) cnt<-cnt+1
					# <38>
					if( fCutU.hasPtn(c(            34,38),aZoid) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 5,13,10,34,NA,38),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c( 6,0 ),c( 26,10    )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c( 3,0 ),c( 13       )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c( 6   ),c( 26       )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c( 4,5 ),c(          )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c( 4,6 ),c( 34,26,24 )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c( 8   ),c( 38       )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c( 8    ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(      ) ) cnt<-cnt+1
					if( aCStep[5]%in%c( 1, 4 ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:2+0]==c( 9,  1    )) ) cnt<-cnt+1	#  4
					if( 1<sum(aCStep[1:3+0]==c(10,  4,  1)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c(20,  5, 10)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+2]==c( 3,  3,  3)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+2]==c( 7,  9,  1)) ) cnt<-cnt+1	#  1

					if( fCutU.hasPtn(c( 8,9 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(10,5 ),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 7,2 ),aCStep) ) cnt<-cnt+1

					# --
					if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
					#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
					#      1  7 16 18 34 38    | 6  9  2 16  4 |                        |2 2 0 2 0 |2 2 2
					#      2  5  8 11 33 39    | 3  3  3 22  6 |  1  -2  -8  -7  -1   1 |3 1 0 2 0 |3 1 2
					#      3 13 16 24 26 29    |10  3  8  2  3 |  1   8   8  13  -7 -10 |1 2 3 0 0 |1 2 3
					#      3 10 13 26 34 38(3) | 7  3 13  8  4 |  0  -3  -3   2   8   9 |1 2 1 2 0 |1 2 1 2
					#      1 21 26 36 40 41(1) |20  5 10  4  1 | -2  11  13  10   6   3 |1 0 2 1 2 |1 2 1 2
					#      6 10 18 25 34 35    | 4  8  7  9  1 |  5 -11  -8 -11  -6  -6 |1 2 1 2 0 |1 2 1 2
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(  ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(  ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+0]==c( -5,  11,   8)) ) cnt<-cnt+1 #-11
					if( 1<sum(aFStep[1:3+1]==c(  8,   8,  13)) ) cnt<-cnt+1 # -8
					if( 1<sum(aFStep[1:3+2]==c( -5,  11,   8)) ) cnt<-cnt+1 #-11
					if( 1<sum(aFStep[1:3+3]==c( 11,   6,   6)) ) cnt<-cnt+1 # -6
					if( 1<sum(aFStep[1:3+3]==c(  8,  11,   6)) ) cnt<-cnt+1 # -6

					# --
					if( all(aFStep[c(2,5)]==aFStep[c(4,6)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,2)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,4)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,2)]) ) cnt<-cnt+1
					if( aFStep[6]==sum(aFStep[c(1,4)]) ) cnt<-cnt+1

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

	return( list(flgCnt=flgCnt ,cntMtx=cntMtx ,auxCntMtx=auxCntMtx 
					,cccMtx=cccObj$scoreMtx ,cStepValMtx=cccObj$cStepValMtx 
				) 
			)

} # fCutCnt.nextColVal_6()


#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================


cutCC <- function( ccObj ,allIdxF ){
	#  cntMtx	auxCntMtx	cccMtx

	surFlag <- rep( TRUE ,length(allIdxF) )

	flag <- apply( ccObj$auxCntMtx, 1, function( cntVal ){
					# auxZW auxQuo
					if( all(cntVal>0) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,F)
	surFlag[ !flag ] <- FALSE

	# flag <- apply( ccObj$cccMtx, 1, function( cntVal ){
	# 				# reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3
	# 			})	;kIdx<-anaFlagFnd(!flag,F)
	# surFlag[ !flag ] <- FALSE

	cccMtx <- ccObj$cccMtx
	cccMtx <- cccMtx[,-which(colnames(cccMtx)=="reb")]
	ccc <- apply( cccMtx ,1 ,function(cccVal){ sum(cccVal>0) })
	cntMtx <- cbind( ccc, ccObj$cntMtx )
	cName <- c( "ccc", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName
	flag <- apply( cntMtx, 1, function( cntVal ){
					# ccc   raw rawFV rem cStep fStep
					if( 1 < sum(cntVal>=thld) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,F)
	surFlag[ !flag ] <- FALSE

	return( surFlag )

} # cutCC()


fCut.rawFV3 <- function(  gEnv ,allIdxF ,rpt=FALSE ){

	surRawFV <- rep( TRUE ,length(allIdxF) )
	# anaMtx.freqVal( stdMI$rawTail )

    surRawFV <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# [fCutCnt.basic] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextZW] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextQuo10] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextBin] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextRebNum] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextCStepBin] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextFStepBin] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_1] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_2] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_3] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_4] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_5] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					# [fCutCnt.nextColVal_6] ---------------------------------------
					# < >
					if( fCutU.hasPtn(c( , ),aZoid,thld=3,fixIdx=1 ) ) return( FALSE )

					return( TRUE )
				})	;kIdx<-anaFltCnt(surRawFV,rpt)

	return( allIdxF[surRawFV] )

}	# fCut.rawFV3( )


finalFilt.common <- function( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol ) {

	flagCnt <- 0



	return( flagCnt )

} # finalFilt.common( )

finalFilt.gold <- function( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol ) {
	
	flagCnt <- 0

	if( TRUE ){	# reb ---------------------------------------------------------------------------------
		# 발생 수 범위.
		rebHpn.sum <- sum(cccMtx[,"reb"]>0)
		if( (rebHpn.sum<3)||(7<rebHpn.sum)  ) return( 10 )
		if( (5<rebHpn.sum)  ) flagCnt <- flagCnt+1
		# reb 연속발생은 2개 이하.
		rebHpnPhs.nm <- c( "basic" ,"nextRebNum" ,"nextColVal_4" ,"nextColVal_5" ,"nextColVal_6" )
		if( 2 < sum(cccMtx[rebHpnPhs.nm,"reb"]>0)  )  return( 10 )
		# reb 연속발생 4번이 얼마나 나올까?
		rebHpnPhs <- cccMtx[ c( "nextColVal_4" ,"nextColVal_6" ) ,"reb" ]
		if( all(rebHpnPhs>0) ){	 return( 10 )
		} else if( any(rebHpnPhs>0) ){	flagCnt <- flagCnt + 1	}

		# 최근 reb 발생 패턴과 똑같을 수는 없겠지.(실제로는 포함관계..)
		rebHpnPhs.nm <- c( "nextZW" ,"nextColVal_2" ,"nextColVal_4" ,"nextColVal_6" )	# toZ820
		if( all(cccMtx[rebHpnPhs.nm,"reb"]>0) )	 return( 10 )
		rebHpnPhs.nm <- c( "nextRebNum" ,"nextFStepBin" 
							,"nextColVal_1" ,"nextColVal_3" ,"nextColVal_4" ,"nextColVal_5" ,"nextColVal_6" )	# toZ816
		if( all(cccMtx[rebHpnPhs.nm,"reb"]>0) )	 return( 10 )
		rebHpnPhs.nm <- c( "basic" ,"nextZW" ,"nextBin" ,"nextColVal_1" ,"nextColVal_5" )	# toZ814
		if( all(cccMtx[rebHpnPhs.nm,"reb"]>0) )	 return( 10 )
		rebHpnPhs.nm <- c( "nextZW" ,"nextQuo10" ,"nextFStepBin" )	# toZ809
		if( all(cccMtx[rebHpnPhs.nm,"reb"]>0) )	 return( 10 )
	}
	if( TRUE ){	# nbor ---------------------------------------------------------------------------------
		fltKey <- "nbor"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( (2<hpn.sum)  ) return( 10 )
		hpnPhs.nm <- c("nextZW")	# 바로 이전 H에서의 발생 회피.
		if( any(cccMtx[hpnPhs.nm,fltKey]>0)  ) return( 10 )
		# 최근 reb 발생 패턴과 똑같을 수는 없겠지.(실제로는 포함관계..)
		hpnPhs.nm <- c( "nextColVal_2" ,"nextColVal_3" )	# toZ814
		hpnPhs.cnt <- sum(cccMtx[hpnPhs.nm,fltKey]>0)
		if( (length(hpnPhs.nm)==hpnPhs.cnt) && all(cccMtx[hpnPhs.nm,fltKey]>0) )	 return( 10 )
	}
	if( TRUE ){	# spanM ---------------------------------------------------------------------------------
		fltKey <- "spanM"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 3<hpn.sum)  ) return( 10 )
		hpnPhs.nm <- c("nextQuo10","nextColVal_5","nextColVal_6")	# 바로 이전 H에서의 발생 회피.
		if( any(cccMtx[hpnPhs.nm,fltKey]>0)  ) return( 10 )

	}
	if( TRUE ){	# quoAll ---------------------------------------------------------------------------------
		fltKey <- "quoAll"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 3<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# quoPtn ---------------------------------------------------------------------------------
		fltKey <- "quoPtn"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# zw ---------------------------------------------------------------------------------
		fltKey <- "zw"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# remH0 ---------------------------------------------------------------------------------
		fltKey <- "remH0"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# remH1 ---------------------------------------------------------------------------------
		fltKey <- "remH1"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# cStep2 ---------------------------------------------------------------------------------
		fltKey <- "cStep2"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}
	if( TRUE ){	# cStep3 ---------------------------------------------------------------------------------
		fltKey <- "cStep3"
		hpn.sum <- sum(cccMtx[,fltKey]>0)
		if( ( 5<hpn.sum)  ) return( 10 )
	}

	return( flagCnt )

} # finalFilt.gold( )



fCut.finalApproach <- function( gEnv ,allIdxF ,rpt=FALSE ){

	return( allIdxF )

} # fCut.finalApproach()









