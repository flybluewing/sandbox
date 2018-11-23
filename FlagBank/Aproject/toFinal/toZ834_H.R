# toZ834_H.R 최종접근
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

# undone
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
					if( (aZoid[6]-aZoid[1]) %in% c( 30, 36, 37 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,0,2)) ) return(FALSE)	# next rebind of 0,2,0
					if( all(quoSize[1:3+1]==c(3,0,2)) ) return(FALSE)	# next rebind of 2,0,2
					if( all(quoSize[1:3+2]==c(0,2,0)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+0]==c(0,2,0)) ) return(FALSE)	# next rebind of 0,3,1 reverse
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
			if( aZoid[1]%in%c( 11, 2,17,18 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 22          ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 22,38       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(             ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 31,42       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39,41       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 4>			# < 5>			# <13>
			# <16>
			if( fCutU.hasPtn(c(       16,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,14,16,NA,25    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(    18,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  8,18,23    ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(       19,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 16,12,19,21    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c( 28,NA,31,43 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c( 17,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    29,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c( 24,33,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,2,7,8   ),c( 11, 2,17,18 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(           ),c( 22          )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(           ),c( 22,38       )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,8       ),c(             )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 1,2,7,9,0 ),c( 31,42       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9,1,6     ),c( 39,41       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1, 3   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  6      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  1      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  8      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 12, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (4)   3 (3)   6 (3)   7 (2)   8 (3)   12 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(2,6)==aCStep[c(2,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 2,4,5 )]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  7 13 29 31 39    | 3  6 16  2  8 |                        |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 |  1   1 -15 -17  -6  -7 |2 2 0 2 0 |2 2 2
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 | -2   4   0   1  -6   1 |1 3 0 2 0 |1 3 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 | 10   4   3   7   9   4 |0 3 1 0 2 |3 1 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   4  11  13   1  -1 |0 2 0 2 2 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  5     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4,  1 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( 19     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -2 (2)   -1 (2)   0 (2)   1 (5)   4 (4)   6 (2) 
			cnt.w2 <- 0
			if( aFStep[3]==sum(aFStep[c( 4,1,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c( 1,6,2,3 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(1,4)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aFStep[c(3,5)])==sum(aFStep[c(6,4)]) )	cnt.w2<-cnt.w2+1	# 12
			if( sum(aFStep[c(4,5)])==sum(aFStep[c( 1,2,3 )]) )	cnt.w2<-cnt.w2+1	# 14
			if( sum(aFStep[c(4,5)])==sum(aFStep[c( 6,2,3 )]) )	cnt.w2<-cnt.w2+1	# 14

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
					# [1]*   3  3 27 16 18
					# [2]   26  8 15 10 27
					# [3]   33 26 25 29
					# [4]   38
					# [5]   39 38 39 29 39 40
					# [6]   35 41 40 36 45
					if( 1<sum(aZoid==c(  3,26,33,38,39,35 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c(  3, 8,26,NA,38,41 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 27,15,25,NA,39,40 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 16,10,29,NA,29,39 ) ,na.rm=T) ) cnt<-cnt+1
					if( 1<sum(aZoid==c( 18,27,NA,NA,39,45 ) ,na.rm=T) ) cnt<-cnt+1

					# if( fCutU.hasPtn(c( 32,20 ),aZoid) ) cnt<-cnt+1

					score <- 0
					if( aZoid[1]%in%c(  3,27 ) ) score<-score+1
					if( aZoid[2]%in%c(       ) ) score<-score+1
					if( aZoid[3]%in%c( 38    ) ) score<-score+1
					if( aZoid[4]%in%c(       ) ) score<-score+1
					if( aZoid[5]%in%c( 40,29,38 ) ) score<-score+1
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
					if( aZoid[1]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(  9,26 ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(  8    ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
					if( aZoid[5]%in%c( 42    ) ) cnt<-cnt+1
					if( aZoid[6]%in%c( 34    ) ) cnt<-cnt+1

					# for(idx in 1:5){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aZoid[1:2+3]==c(38,39)) ) cnt<-cnt+1
					if( all(aZoid[1:2+4]==c(40,42)) ) cnt<-cnt+1

					# [  1]  7  9     4  9    40 42    34 42    31 41
					# [  2]  3  9     7 10    35 36    20 27    26 36
					# [  3] 14 26    33 38    19 26    26 27    39 41
					# [  4] 18 23    31 38    10 16    35 40    23 34
					# [  5]  9 15    24 25    14 26    36 43    34 39
					# [  6]  9 18     7  8    36 41    37 39    37 39
					# [  7]  3  4             29 39    15 23    40 42
					# [  8] 25 39                      36 37    36 39
					# [  9] 15 17                      25 29    36 45
					# [ 10]                            40 41    39 45
					# [ 11]                            22 42    40 42
					remCnt <- 0
						if( fCutU.remFilt(aZoid[1],c(         ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[2],c( 9,6     ),c(  9,26 )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[3],c( 8       ),c(  8    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[4],c( 2,6,1,5 ),c(       )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[5],c( 2       ),c( 42    )) )	remCnt <- remCnt+1
						if( fCutU.remFilt(aZoid[6],c( 4       ),c( 34    )) )	remCnt <- remCnt+1
						# grp 1
						#	if( aZoid[2]==17 && fCutU.remFilt(aZoid[1],c( 2),c( )) ) remCnt <- remCnt+1 
						# grp 2
						#	if( aZoid[2]==11 && fCutU.remFilt(aZoid[1],c( 9),c( 9)) ) remCnt <- remCnt+1 
					if(remCnt>1) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flgCnt[fltCnt> 2] <- flgCnt[fltCnt> 2] + 2
	flgCnt[fltCnt==2] <- flgCnt[fltCnt==2] + 1

	# -- conditional custom
	# <remove>	cStep 
	#		idx<-1	;cvSeqNextLst[[idx]]$fndMtx[,2] - cvSeqNextLst[[idx]]$fndMtx[,1]
    fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	[1]  2   6  12   5   6   9   1  14   2 
					#	[2]  5   3   5   7   1   1 
					#	[3]  2   1   7   6  12   5  10 
					#	[4]  8   7   1   5   7   2   8   1   4   1  20   3  11   3 
					#	[5] 10  10   2  11   5   2   2   3   9   6   2 
					aCStep <- aZoid[2:6]-aZoid[1:5]

					score  <- sum(aCStep==c(  2, 5, 2, 8,10 ),na.rm=T)
					matCnt <- sum(aCStep==c(  6, 3, 1, 7,10 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 12, 5, 7, 1, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  5, 7, 6, 5,11 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  6, 1,12, 7, 5 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  9, 1, 5, 2, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  1,NA,10, 8, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( 14,NA,NA, 1, 3 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c(  2,NA,NA, 4, 9 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA, 1, 6 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )
					matCnt <- sum(aCStep==c( NA,NA,NA,20, 2 ),na.rm=T)
					score <- score + ifelse( matCnt>1 ,matCnt-1 ,0 )

					cnt <- 0
						if( fCutU.hasPtn(c(  5, 2 ),aCStep) ) aCStep<-aCStep+1
						if( 1<sum(aCStep[1:2+1]==c( 12, 7 )) )	cnt<-cnt+1
						if( fCutU.hasPtn(c(  5, 7 ),aCStep) ) aCStep<-aCStep+1
						if( fCutU.hasPtn(c( 10, 8, 2 ),aCStep) ) aCStep<-aCStep+1

						if( aCStep[1]%in%c(  7, 5    ) ) cnt<-cnt+1
						if( aCStep[2]%in%c(  7, 3    ) ) cnt<-cnt+1
						if( aCStep[3]%in%c(  3, 4    ) ) cnt<-cnt+1
						if( aCStep[4]%in%c(  9, 1    ) ) cnt<-cnt+1
						if( aCStep[5]%in%c( 10, 2, 9   ) ) cnt<-cnt+1

						if( 1<sum( aCStep[ 1 ]*c(1,4)==aCStep[c(3,4)] ) )	cnt<-cnt+1
						if( 1<sum( aCStep[c(1,2)]*c(1,2)==aCStep[c(3,5)] ) )	cnt<-cnt+1
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
						if( aCStep[1]%in%c( 5, 8, 6 ) ) tCnt<-tCnt+1
						if( aCStep[2]%in%c(    ) ) tCnt<-tCnt+1
						if( aCStep[3]%in%c( 3   ) ) tCnt<-tCnt+1
						if( aCStep[4]%in%c( 1  ) ) tCnt<-tCnt+1
						if( aCStep[5]%in%c(    ) ) tCnt<-tCnt+1

						if( 1<sum( aCStep[c(4,5)]*c(3,1)==aCStep[c(3,2)] ) )	cnt<-cnt+1
					cnt <- cnt + ifelse( tCnt>1 ,tCnt-1 ,0 )

					# if( fCutU.hasPtn(c( , ),aCStep) )	cnt<-cnt+1
					# if( 1<sum(aCStep[1:2+ ]==c( , )) )	cnt<-cnt+1
					if( fCutU.hasPtn(c( 1,1 ),aCStep) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  5, 1, 5 ),aCStep) )	cnt<-cnt+1
					if( fCutU.hasPtn(c(  3,20, 3 ),aCStep) )	cnt<-cnt+1

					# [1]*  26  4 15  3 10 10 |  6  2 16  3  1  3  8  5  7  4 10  2  2  8  1 13  5  6  4  6  1  5  8  4  2  7  4
					# [2]    1 14  9 20  7  1 |
					# [3]    9  6  5  3 12  1 |  5  1
					# [4]*   3 18  1  4  1  1 |  1  3
					# [5]*   1  3  5  8  1  1 | 21  7  4  2 11  8  2  7  7 11  7  7  5 13 12  6  3 16  9 13 14 12 11 22 13  4  1 10 11 10  5...
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
					# [  1]  1 12     8  1     6  6    13  3
					# [  2]  3  1     5 12     4  1     4  5
					# [  3]  1  4     3  6     6 17    11  1
					# [  4] 11  5    12  2    20  4    13  8
					# [  5]  1  2     8 16     3  3     2  2
					# [  6]  6  7     6  3     7  1     9  4
					# [  7]           6  9     5  1     2  1
					# [  8]          15  2     4  6    13  7
					# [  9]                    1 11     1 12
					# [ 10]                   13  9     9  1

					if( aCStep[1]%in%c( 11       ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 20, 4, 5 ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(  6       ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

					# for(idx in 1:4){ cat(sprintf("+%d----------------\n",idx-1)) ;anaMtx_ColVal(cvSeqNextLst[[idx]]$fndMtx) }
					if( all(aCStep[1:2+2]==c(  5, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(  8,15 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 14, 9 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  2, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  2, 1 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  2, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  2, 3 )) ) cnt<-cnt+1

					if( all(aCStep[1:2+3]==c(  8, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 13, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 14, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 19, 8 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  4, 2 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 13, 3 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c(  8, 6 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 13, 8 )) ) cnt<-cnt+1

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(0,2,0)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,2,0
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
			if( aZoid[1]%in%c( 11      ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 18      ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 36      ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# <11>
			if( fCutU.hasPtn(c( 11,20,NA,NA,NA,36 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,NA,40    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c( 15,16,30,31,42,33 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <18>
			if( fCutU.hasPtn(c(  8,18,41,NA,43 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <24>
			if( fCutU.hasPtn(c(  8,13,24 ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c( 25,32 ),aZoid) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c( 21,NA,21,33,34 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(    17,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,36,39 ),aZoid) ) cnt<-cnt+1
			# <42>
			if( fCutU.hasPtn(c( 13,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       31,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1       ),c( 11 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 8       ),c( 18 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(         ),c( 36 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,3     ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9       ),c( 39 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(           ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 10        ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  9, 1     ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 10        ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2, 1, 3  ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  8, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  2, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 8, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3,7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 7,3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 9,1 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (3)   4 (2)   5 (4)   7 (2)   8 (3)   9 (3) 

			cnt.w2 <- 0
 			if( 1<sum( aCStep[c(1,4)]*c(2,6)==aCStep[c(2,2)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[2]==sum(aCStep[c(3,4,5)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[3]==sum(aCStep[c(1,4,5)]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 2,4,5 )]) )	cnt.w2<-cnt.w2+1	# 15

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4 11 20 23 32 39    | 7  9  3  9  7 |                        |1 1 2 2 0 |1 1 2 2
			#     11 24 32 33 35 40(2) |13  8  1  2  5 |  7  13  12  10   3   1 |0 1 1 3 1 |1 1 3 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  0 -12  -3   0   3   2 |0 2 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  0   4 -11 -14 -14  -3 |0 4 1 1 0 |4 1 1
			#     13 16 24 25 33 36(2) | 3  8  1  8  3 |  2   0   6   6   9  -3 |0 2 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   2   6  14   8   6 |0 2 0 2 2 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(            ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  6,-14     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  6,-11,  2 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(            ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  7         ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  6         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -14 (2)   -3 (3)   0 (4)   2 (3)   3 (2)   6 (4) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[2]*c(3,7,4,3)==aFStep[c(3,4,5,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c( 3,5   )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c( 6,5   )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c( 2,3,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c( 2,3   )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c( 3,6   )]) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,3,0
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 3,0,2
					if( all(quoSize[1:3+2]==c(0,2,0)) ) return(FALSE)	# next rebind of 0,2,0
					if( all(quoSize[1:3+2]==c(1,3,0)) ) return(FALSE)	# next rebind of 0,2,0
					# if( all(quoSize[1:3+2]==c(1,1,1)) ) return(FALSE)	# next rebind of 0,2,0
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
			if( aZoid[3]%in%c( 16,12,13 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 20       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 38,39,40 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,10             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,NA,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,NA,19,12,28    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 5>
			if( fCutU.hasPtn(c(  5,NA,20,NA,41,32 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c(  3,10          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,NA,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 10,19,12,28    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,19       ),aZoid) ) cnt<-cnt+1
			# <11>
			if( fCutU.hasPtn(c( 11,NA,23,27 ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(       16,20       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,NA,NA,40 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,14,16,NA,25    ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(  4,NA,NA,19       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,19       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          19,NA,39 ),aZoid) ) cnt<-cnt+1
			# <31>
			if( fCutU.hasPtn(c(    17,NA,31    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,17,31,45 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <33>
			if( fCutU.hasPtn(c(  2, 7, 3,27,33 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  2,NA,11,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			# <39>
			if( fCutU.hasPtn(c(          19,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,NA,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    10,NA,NA,NA,39 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       21,NA,27,39 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 3         ),c(  3       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,2,8     ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6,2,3     ),c( 16,12,13 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 9,0,1,6,5 ),c( 20       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(           ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 8,9,0     ),c( 38,39,40 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(     ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1  ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  4  ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(     ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 10, 2 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 2,19 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 6, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  3,22 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 11, 8 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (2)   3 (2)   4 (3)   6 (3)   7 (3)   8 (4)   10 (2)
			cnt.w2 <- 0
			if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5  )]) )	cnt.w2<-cnt.w2+1	# 15
			if( sum(aCStep[c(2,4)])==sum(aCStep[c(1,3,5)]) )	cnt.w2<-cnt.w2+1	# 18

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 10 11 19 35 39    | 8  1  8 16  4 |                        |1 3 0 2 0 |1 3 2
			#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   2   7  -1  -1 |1 2 1 2 0 |1 2 1 2
			#      8 15 21 31 33 38(1) | 7  6 10  2  5 |  5   5   8   5  -1   0 |1 1 1 3 0 |1 1 1 3
			#      5 11 12 29 33 44(1) | 6  1 17  4 11 | -3  -4  -9  -2   0   6 |1 2 1 1 1 |1 2 1 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 |  0  -5   4 -11   4  -6 |2 2 0 2 0 |2 2 2
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 | -2   4   0   1  -6   1 |1 3 0 2 0 |1 3 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  4, -2 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -6     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -1, 1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -2 (2)   -1 (3)   0 (5)   1 (3)   4 (3)   5 (3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,4)]*c(-2, 1)==aFStep[c(2,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c( 2, 5 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c( 1,4,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c( 2,4,6,5 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 1,2 )])==sum(aFStep[c( 4,6 )]) )	cnt.w2<-cnt.w2+1	# 2

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,1,0 reverse
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
			if( aZoid[2]%in%c( 36    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 27    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 25, 8 ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 39    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2, 7       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,29,40 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  2, 7    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,29,40 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,16 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 21,25,NA,30 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 27,39,45 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 15,23,29,39 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <39>
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,8,6   ),c( 36    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 7,5     ),c( 27    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 7       ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,5,8   ),c( 25, 8 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 9       ),c( 39    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  3,12, 5 ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  7       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 12       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  3, 6, 4 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  2, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 12, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c(  4, 7 )) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c(  6, 2 )) )	cnt.w1<-cnt.w1+1	# -
			if( 1<sum(aCStep[1:2+3]==c(  4, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  5, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  7, 6 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 12, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (4)   4 (4)   5 (4)   7 (3)   8 (2)   12 (2) 

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c( 1,5 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 4,5 )]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aCStep[c( 1,2 )])==sum(aCStep[c( 3,4,5 )]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 23 27 29 33 41    | 7  4  2  4  8 |                        |0 1 3 1 1 |1 3 1 1
			#     11 12 14 15 18 39    | 1  2  1  3 21 | -5 -11 -13 -14 -15  -2 |0 5 0 1 0 |5 1
			#      1  2  7  9 10 38    | 1  5  2  1 28 |-10 -10  -7  -6  -8  -1 |4 1 0 1 0 |4 1 1
			#      4  8 27 34 39 40    | 4 19  7  5  1 |  3   6  20  25  29   2 |2 0 1 2 1 |2 1 2 1
			#     15 17 25 37 42 43    | 2  8 12  5  1 | 11   9  -2   3   3   3 |0 2 1 1 2 |2 1 1 2
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 |-13 -10  -6 -12 -13  -7 |2 1 2 1 0 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(   ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(   ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(   ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(   ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(   ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -13,-13 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -10, -5 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  -3, -3 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -13 (3)   -10 (3)   -7 (2)   -6 (2)   -2 (2)   3 (4) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,3)]*c(1,2)==aFStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c( 3,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c( 3,6 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 4,6 )])==sum(aFStep[c( 1,3 )]) )	cnt.w2<-cnt.w2+1	# -19
			if( sum(aFStep[c( 4,6 )])==sum(aFStep[c( 5,3 )]) )	cnt.w2<-cnt.w2+1	# -19

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
					if( (aZoid[6]-aZoid[1]) %in% c( 34 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 1,2,1
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
			if( aZoid[1]%in%c(  0       ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 12       ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 12,15,13 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(          ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(          ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,12             ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			# < 4>
			# <10>
			if( fCutU.hasPtn(c( 10,14 ),aZoid) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  1,12          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,NA,27,26    ),aZoid) ) cnt<-cnt+1
			# <13>
			if( fCutU.hasPtn(c(  1,NA,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 12,13          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,27,26    ),aZoid) ) cnt<-cnt+1
			# <14>
			if( fCutU.hasPtn(c( 10,14 ),aZoid) ) cnt<-cnt+1
			# <18>
			# <22>
			if( fCutU.hasPtn(c(  5,10,22 ),aZoid) ) cnt<-cnt+1
			# <32>
			if( fCutU.hasPtn(c(  4,11,13,32 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,4     ),c(  0       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,4     ),c( 12       )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 2,5,3,4 ),c( 12,15,13 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1       ),c(          )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 4,0     ),c(          )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(         ),c(          )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  1,12    ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  9, 6    ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4,13,15 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 11, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c(  5, 6 )) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   4 (5)   5 (2)   6 (2)   8 (4)   11 (4)   14 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(1,3)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 2,5 )])==sum(aCStep[c( 3,4 )]) )	cnt.w2<-cnt.w2+1	# 16
			if( sum(aCStep[c( 2,5 )])==sum(aCStep[c( 1,4 )]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1 12 13 21 32 45    |11  1  8 11 13 |                        |1 2 1 1 1 |1 2 1 1 1
			#      5 10 16 17 31 32(1) | 5  6  1 14  1 |  4  -2   3  -4  -1 -13 |1 3 0 2 0 |1 3 2
			#      4 18 26 33 34 38    |14  8  7  1  4 | -1   8  10  16   3   6 |1 1 1 3 0 |1 1 1 3
			#      4 10 14 15 18 22(2) | 6  4  1  3  4 |  0  -8 -12 -18 -16 -16 |1 4 1 0 0 |1 4 1
			#      2 10 14 22 32 36(3) | 8  4  8 10  4 | -2   0   0   7  14  14 |1 2 1 2 0 |1 2 1 2
			#      1 12 13 24 29 44    |11  1 11  5 15 | -1   2  -1   2  -3   8 |1 2 2 0 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  0, -3  ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  8      ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -2      ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -2, -1  ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(  1      ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  6      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -16 (2)   -2 (2)   -1 (4)   0 (3)   2 (2)   3 (2)   8 (2)   14 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(1,1)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[2]*c(1,4)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c( 2,5 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[c( 2,5 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 2,4 )])==sum(aFStep[c( 1,5,6 )]) )	cnt.w2<-cnt.w2+1	# 4
			if( sum(aFStep[c( 2,4 )])==sum(aFStep[c( 3,5,6 )]) )	cnt.w2<-cnt.w2+1	# 4

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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(1,1,2)) ) return(FALSE)	# next rebind of 1,2,1
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,1,0 reverse
					if( all(quoSize[1:3+0]==c(0,2,1)) ) return(FALSE)	# next rebind of 1,2,0 reverse
					if( all(quoSize[1:3+2]==c(1,2,0)) ) return(FALSE)	# next rebind of 0,2,1
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
			if( aZoid[2]%in%c(  7    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  6    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 35    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 2>
			if( fCutU.hasPtn(c(  2, 7       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  2,NA,29,40 ),aZoid) ) cnt<-cnt+1
			# < 4>
			# < 7>
			if( fCutU.hasPtn(c(  2, 7       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,29,40 ),aZoid) ) cnt<-cnt+1
			# < 8>
			if( fCutU.hasPtn(c(  8,NA,NA,33,34 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,12 ),aZoid) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 15,19 ),aZoid) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c( 21,25,NA,30 ),aZoid) ) cnt<-cnt+1
			# <36>
			# <37>
			if( fCutU.hasPtn(c(  5, 9,NA,37,38 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <40>
			if( fCutU.hasPtn(c( 12,12,NA,38,35,40 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 4       ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,8     ),c(  7 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,6     ),c(  6 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 4,5,7   ),c( 35 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5       ),c(    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  2, 1   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  5      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  2      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  5, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 4, 7 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 6, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 2, 3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  3, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 12, 7 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (6)   2 (3)   3 (3)   4 (4)   5 (4)   6 (2)   7 (2)   12 (2) 
			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c( 1,5 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 4,5 )]) )	cnt.w2<-cnt.w2+1	# 11
			if( sum(aCStep[c( 1,2 )])==sum(aCStep[c( 3,4,5 )]) )	cnt.w2<-cnt.w2+1	# 17

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  2  7  9 10 38    | 1  5  2  1 28 |                        |4 1 0 1 0 |4 1 1
			#      4  8 27 34 39 40    | 4 19  7  5  1 |  3   6  20  25  29   2 |2 0 1 2 1 |2 1 2 1
			#     15 17 25 37 42 43    | 2  8 12  5  1 | 11   9  -2   3   3   3 |0 2 1 1 2 |2 1 1 2
			#      4 10 14 15 18 22(1) | 6  4  1  3  4 |-11  -7 -11 -22 -24 -21 |1 4 1 0 0 |1 4 1
			#      8 10 13 36 37 40(1) | 2  3 23  1  3 |  4   0  -1  21  19  18 |1 2 0 2 1 |1 2 2 1
			#      2  7 19 25 29 36(1) | 5 12  6  4  7 | -6  -3   6 -11  -8  -4 |2 1 2 1 0 |2 1 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 3, 3 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (3)   3 (4)   6 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[c(2,6)]*c(2,2)==aFStep[c(1,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[ 2 ]*c( 2,-2)==aFStep[c(1,3)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c( 3,5,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c( 2,5   )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c( 2,3,4 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 2,4 )])==sum(aFStep[c( 1,5 )]) )	cnt.w2<-cnt.w2+1	# -14

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
					if( all(quoSize[1:3+1]==c(0,0,1)) ) return(FALSE)	# next rebind of 2,2,0 reverse
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
			if( aZoid[1]%in%c(  1    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c( 11    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 16    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 43,41 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			# <12>
			if( fCutU.hasPtn(c( 11,12,31 ),aZoid) ) cnt<-cnt+1
			# <14>
			# <15>
			if( fCutU.hasPtn(c(       15,NA,NA,44 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  4,10,15,16       ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <34>
			# <38>
			if( fCutU.hasPtn(c( 38,39 ),aZoid) ) cnt<-cnt+1
			# <43>
			if( fCutU.hasPtn(c(    11,NA,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,NA,NA,43 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,NA,NA,15,16,43 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,6    ),c(  1    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 2,4,5  ),c( 11    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 6,1,9  ),c( 16    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,1,4  ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 6      ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 3,5,1  ),c( 43,41 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  4, 9    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1, 4, 6 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(          ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (3)   3 (4)   4 (3)   5 (2)   6 (2)   9 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(1,2)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c( 1,2,3,4 )]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     15 17 19 34 38 41    | 2  2 15  4  3 |                        |0 3 0 2 1 |3 2 1
			#     10 14 15 32 36 42(1) | 4  1 17  4  6 | -5  -3  -4  -2  -2   1 |0 3 0 2 1 |3 2 1
			#      1 13 14 33 34 43(1) |12  1 19  1  9 | -9  -1  -1   1  -2   1 |1 2 0 2 1 |1 2 2 1
			#      1  4 37 38 40 45(1) | 3 33  1  2  5 |  0  -9  23   5   6   2 |2 0 0 2 2 |2 2 2
			#      7 12 15 24 25 43    | 5  3  9  1 18 |  6   8 -22 -14 -15  -2 |1 2 2 0 1 |1 2 2 1
			#      1  2  3  9 12 23(1) | 1  1  6  3 11 | -6 -10 -12 -15 -13 -20 |4 1 1 0 0 |4 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[3]%in%c( -15,  2 ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -16     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -12     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  -2,  1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+4]==c( -13,-20 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  0,  1 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -15 (2)   -9 (2)   -2 (4)   -1 (2)   1 (3)   6 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,2)]*c(2,2)==aFStep[c(3,6)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 4,5   )])==sum(aFStep[c( 1,2,3 )]) )	cnt.w2<-cnt.w2+1	# -28
			if( sum(aFStep[c( 2,4   )])==sum(aFStep[c( 3,5   )]) )	cnt.w2<-cnt.w2+1	# -25
			if( sum(aFStep[c( 4,6   )])==sum(aFStep[c( 2,3,5 )]) )	cnt.w2<-cnt.w2+1	# -35
			if( sum(aFStep[c( 1,3,6 )])==sum(aFStep[c( 2,4,5 )]) )	cnt.w2<-cnt.w2+1	# -38

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+1]==c(0,1,2)) ) return(FALSE)	# next rebind of 0,2,2 reverse
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+0]==c(2,0,2)) ) return(FALSE)	# next rebind of 2,2,1
					if( all(quoSize[1:3+1]==c(0,2,2)) ) return(FALSE)	# next rebind of 2,1,0
					if( all(quoSize[1:3+2]==c(2,2,0)) ) return(FALSE)	# next rebind of 1,0,1
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
			if( aZoid[1]%in%c(  1, 9 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  9, 4 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 11    ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 10    ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1, 6, 5, 5,32 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,36,45,43 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c(  2,NA,12 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     8,12 ),aZoid) ) cnt<-cnt+1
			# <13>			# <24>			# <28>
			# <29>
			if( fCutU.hasPtn(c( 29,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 29,35    ),aZoid) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c( 29,NA,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    35,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,36,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 1,9   ),c(  1, 9 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9,4,5 ),c(  9, 4 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,6,9 ),c( 11    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 0,5   ),c( 10    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3     ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 4,5   ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 1   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 13, 5   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  1      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 5,4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 11, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 4,5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 5,5 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  2,14 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (2)   3 (3)   4 (4)   5 (4)   6 (2)   12 (2)   17 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[  4  ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  2 15 19 24 36    | 1 13  4  5 12 |                        |2 2 1 1 0 |2 2 1 1
			#     11 12 29 33 38 42    | 1 17  4  5  4 | 10  10  14  14  14   6 |0 2 1 2 1 |2 1 2 1
			#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  4   0  12  16   9  -5 |2 0 2 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  12    ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  -2, -2 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( -14,-14 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (2)   -4 (2)   -3 (2)   -2 (2)   1 (2)   2 (2)   10 (2)   14 (3) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 1 ]*c(3,4)==aFStep[c(3,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(5,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,3)]) )	cnt.w2<-cnt.w2+1

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
					if( all(quoSize[1:3+0]==c(0,2,1)) ) return(FALSE)	# next rebind of 2,0,2
					if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 0,2,2
					if( all(quoSize[1:3+2]==c(3,0,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+0]==c(0,4,0)) ) return(FALSE)	# next rebind of 0,2,2 reverse
					if( all(quoSize[1:3+1]==c(2,0,2)) ) return(FALSE)	# next rebind of 2,2,1
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
			if( aZoid[2]%in%c( 15    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 24,12 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 40    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(       ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 3>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,25 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  3,14,15       ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,NA,24,30 ),aZoid) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  9,36,45,43 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,23 ),aZoid) ) cnt<-cnt+1
			# <16>			# <18>
			# <24>
			if( fCutU.hasPtn(c(  2,NA,24,32,29,36 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <25>
			if( fCutU.hasPtn(c(  3,NA,NA,NA,25 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    14,15,NA,25 ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c(             34,38 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 13,11,32,NA,34    ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(  4,33,NA,NA,38 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(          ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 0,5,7    ),c( 15    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 4,2,9,3  ),c( 24,12 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5        ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,6,4    ),c( 40    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 7        ),c(       )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 2   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3      ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  4, 5   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  3      ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  1, 1 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  6, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (7)   2 (3)   4 (2)   5 (3)   6 (4)   15 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[  4  ]*c(3,1)==aCStep[c(2,3)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aCStep[c(1,3)]*c(2,3)==aCStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      1  7 16 18 34 38    | 6  9  2 16  4 |                        |2 2 0 2 0 |2 2 2
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3  -7   6  -9  -5 |3 0 2 1 0 |3 2 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
			#      3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
			#     12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   9  12  13  14  -3 |0 2 2 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c( 11     ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( 14     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( 15, 13 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -1, 12 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			# -------------------------------------------------------------------------------------
			#     FV :    -9 (2)   -7 (2)   -5 (3)   -4 (2)   -3 (2)   -2 (2)   9 (2)   11 (2)   12 (2)   13 (2) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(1,4)]*c(1,-3)==aFStep[c(5,2)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c( 1,4,5,6 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c( 1,4 )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c( 5,4 )]) )	cnt.w2<-cnt.w2+1

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+2]==c(1,0,0)) ) return(FALSE)	# next rebind of 2,1,0
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
			if( aZoid[1]%in%c(  1 ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  6 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(  8 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c( 25 ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c(  7 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,NA,16 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     6,NA,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,27    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  7,28,36,16,29 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 8>
			# <16>
			if( fCutU.hasPtn(c(  6,NA,NA,16 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(     6,NA,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(        7,16,NA,39 ),aZoid) ) cnt<-cnt+1
			# <19>			# <23>
			# <25>
			if( fCutU.hasPtn(c( 10,NA,25 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c(  6,NA,NA,27    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          27,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       27,NA,35 ),aZoid) ) cnt<-cnt+1
			# <35>
			if( fCutU.hasPtn(c(  6,NA,NA,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       27,NA,35 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          27,35 ),aZoid) ) cnt<-cnt+1

			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(  1,0     ),c(  1 )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c(  6       ),c(  6 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c(  5,8     ),c(  8 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(  5,6     ),c( 25 )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(  7       ),c(    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c(  3,5,6,7 ),c(  7 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  1,15   ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  1,22   ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  7, 4   ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 1,8 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 1,4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 1, 3 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (5)   2 (3)   3 (5)   5 (2)   6 (2)   8 (3)   15 (2) 

			cnt.w2 <- 0
			if( aCStep[2]==sum(aCStep[c( 1,3,5 )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c( 1,4 )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[c( 3,4 )]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      8 23 25 27 35 44    |15  2  2  8  9 |                        |1 0 3 1 1 |1 3 1 1
			#      6  8 13 16 30 43(1) | 2  5  3 14 13 | -2 -15 -12 -11  -5  -1 |2 2 0 1 1 |2 2 1 1
			#      6  7 10 16 38 41(2) | 1  3  6 22  3 |  0  -1  -3   0   8  -2 |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c( -2     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  0     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -2, -3 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -11, -7 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  -5, -1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  -2, -2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(   0, 10 ),aFStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -11 (2)   -10 (2)   -6 (3)   -5 (2)   -3 (2)   -2 (5)   -1 (3)   0 (3) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 3 ]*c(-1,-1)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(1,3)]*c(-1,-1)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1

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
					if( (aZoid[6]-aZoid[1]) %in% c( 33 ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(2,0,1)) ) return(FALSE)	# next rebind of 1,1,2
					if( all(quoSize[1:3+2]==c(1,1,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+0]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,2,0	# *
					if( all(quoSize[1:3+1]==c(1,2,1)) ) return(FALSE)	# next rebind of 2,0,1
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 0,1,1
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
			if( aZoid[2]%in%c( 19    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 33    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 15    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,NA,NA,37    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6,NA,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			# < 7>
			if( fCutU.hasPtn(c(  5, 7, 9,NA,35,45 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,NA,16       ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,18 ),aZoid) ) cnt<-cnt+1
			# <16>
			if( fCutU.hasPtn(c(  7,NA,16       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       16,NA,39 ),aZoid) ) cnt<-cnt+1
			# <19>
			if( fCutU.hasPtn(c(  9,19          ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    19,24       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    19,NA,29    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 19,NA,27 ),aZoid) ) cnt<-cnt+1
			# <27>
			if( fCutU.hasPtn(c( 19,NA,27 ),aZoid) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c(  6,NA,NA,NA,NA,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(             37,41 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 5         ),c(    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 7,3,9,4,5 ),c( 19 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,6       ),c(    )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 5,8       ),c(    )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 3         ),c( 33 )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5         ),c( 15 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 5, 4    ) ) cnt<-cnt+1
			if( aCStep[2]%in%c( 5       ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 4, 3, 6 ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c(  7, 1 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+2]==c( 11, 6 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  4, 4 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+3]==c( 1, 8 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c(  6, 2 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c(  7, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   2 (3)   3 (3)   4 (5)   6 (3)   7 (4)   8 (2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(3,5)]*c(2,1)==aCStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[1]==sum(aCStep[c( 3,4,5 )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 1,3 )])==sum(aCStep[c( 2,4,5 )]) )	cnt.w2<-cnt.w2+1	# 14

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      6 13 29 37 39 41    | 7 16  8  2  2 |                        |1 1 1 2 1 |1 1 1 2 1
			#      6  7 10 16 38 41(2) | 1  3  6 22  3 |  0  -6 -19 -21  -1   0 |2 2 0 1 1 |2 2 1 1
			#     10 17 18 19 23 27(1) | 7  1  1  4  4 |  4  10   8   3 -15 -14 |0 4 2 0 0 |4 2
			#      1  7 19 26 27 35(2) | 6 12  7  1  8 | -9 -10   1   7   4   8 |2 1 2 1 0 |2 1 2 1
			#      3  7 14 16 31 40(1) | 4  7  2 15  9 |  2   0  -5 -10   4   5 |2 2 0 1 1 |2 2 1 1
			#      8 19 25 28 32 36    |11  6  3  4  4 |  5  12  11  12   1  -4 |1 1 2 2 0 |1 1 2 2

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[5]%in%c(   1    ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(  10    ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c(  0, -4 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -10 (2)   0 (3)   1 (2)   4 (3)   5 (2)   8 (2)   12 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-3,-3)==aFStep[c(2,4)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(3,1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[c(1,6  )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 4,5 )])==sum(aFStep[c( 1,6,2 )]) )	cnt.w2<-cnt.w2+1	# 13

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
					quoSize <- fCutU.getQuoObj( aZoid )$size
					if( all(quoSize[1:3+0]==c(1,1,1)) ) return(FALSE)	# next rebind of 2,1,1
					if( all(quoSize[1:3+1]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,1,1
					if( all(quoSize[1:3+2]==c(1,0,1)) ) return(FALSE)	# next rebind of 1,1,1
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
			if( aZoid[1]%in%c( 10    ) ) cnt<-cnt+1
			if( aZoid[2]%in%c(  8    ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 11,12 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 23,18 ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 1>
			if( fCutU.hasPtn(c(  1,NA,NA,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1,NA,NA,NA,NA,42 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  1, 7, 3,NA,44    ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 6>
			if( fCutU.hasPtn(c(  6,NA,14       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  6, 9,NA,NA,16 ),aZoid) ) cnt<-cnt+1
			# <10>
			if( fCutU.hasPtn(c( 10,17,NA,NA,27 ),aZoid) ) cnt<-cnt+1
			# <11>
			# <13>
			if( fCutU.hasPtn(c(  8,NA,13,38,45 ),aZoid,thld=3,fixIdx=3) ) cnt<-cnt+1
			# <15>
			if( fCutU.hasPtn(c( 12,15       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    15,16    ),aZoid) ) cnt<-cnt+1
			# <21>
			# <23>
			if( fCutU.hasPtn(c(  1,NA,NA,23       ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(          23,41 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(    21,22,23    ),aZoid) ) cnt<-cnt+1
			# <34>
			if( fCutU.hasPtn(c( 19,19,15,19,34,38 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <41>
			if( fCutU.hasPtn(c( 15,20,23,15,31,41 ),aZoid,thld=3,fixIdx=6) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c( 0,7     ),c( 10    )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 4       ),c(  8    )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 1,2     ),c( 11,12 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c( 1,3     ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c(         ),c(       )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 2,3,8   ),c( 23,18 )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c(  6, 5       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(  3, 7, 5, 4 ) ) cnt<-cnt+1
			if( aCStep[3]%in%c(  3          ) ) cnt<-cnt+1
			if( aCStep[4]%in%c( 13, 1       ) ) cnt<-cnt+1
			if( aCStep[5]%in%c(  4, 1       ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 5, 3 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 5, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 3,13 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 3,13 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 5 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( 4,11 ),aCStep) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    2 (2)   3 (5)   4 (4)   5 (3)   7 (2)   10 (4)   11 (2)   13 (2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[ 3 ]*c(2,3)==aCStep[c( 2,4 )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[4]==sum(aCStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aCStep[5]==sum(aCStep[ 1:4 ]) )	cnt.w2<-cnt.w2+1

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  6 13 16 29 30    | 4  7  3 13  1 |                        |2 2 1 1 0 |2 2 1 1
			#      1 11 21 23 34 44    |10 10  2 11 10 | -1   5   8   7   5  14 |1 1 2 1 1 |1 1 2 1 1
			#      5 10 13 27 37 41    | 5  3 14 10  4 |  4  -1  -8   4   3  -3 |1 2 1 1 1 |1 2 1 1 1
			#     10 15 18 21 34 41(2) | 5  3  3 13  7 |  5   5   5  -6  -3   0 |0 3 1 1 1 |3 1 1 1
			#      1  9 12 23 39 43    | 8  3 11 16  4 | -9  -6  -6   2   5   2 |2 1 1 1 1 |2 1 1 1 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  5   2   3  -6 -16  -3 |1 3 1 0 1 |1 3 1 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(  4,  2 ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[4]%in%c(  4     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -6,  5 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c(        ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( 5, 2 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+0]==c( -16, -3 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 5 ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    -6 (4)   -3 (3)   -1 (2)   2 (3)   3 (2)   4 (2)   5 (7) 

			cnt.w2 <- 0
			if( 1<sum( aFStep[ 2 ]*c(-3,-8)==aFStep[c(4,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[2:3]*c(-3,-1)==aFStep[c(4,6)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[c(2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[2]==sum(aFStep[c(1,6)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(3,4)]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c(1,4)])==sum(aFStep[c(2,6  )]) )	cnt.w2<-cnt.w2+1	# -1
			if( sum(aFStep[c(4,6)])==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1	# -9

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
					if( all(quoSize[1:3+0]==c(1,2,1)) ) return(FALSE)	# next rebind of 1,2,2 reverse
					if( all(quoSize[1:3+1]==c(2,1,2)) ) return(FALSE)	# next rebind of 2,2,0
					if( all(quoSize[1:3+2]==c(2,1,0)) ) return(FALSE)	# next rebind of 2,0,1
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
			if( aZoid[2]%in%c(  9,27 ) ) cnt<-cnt+1
			if( aZoid[3]%in%c( 12,18 ) ) cnt<-cnt+1
			if( aZoid[4]%in%c(       ) ) cnt<-cnt+1
			if( aZoid[5]%in%c( 28    ) ) cnt<-cnt+1
			if( aZoid[6]%in%c( 45    ) ) cnt<-cnt+1
			cntMtx[idx,"raw"] <- cnt

			cnt <- 0
			cntMtx[idx,"raw.w1"] <- cnt
		}
		if( TRUE ){ # rawFV		# anaMtx.freqVal( stdMI$rawTail )
			cnt <- 0
			# < 7>
			if( fCutU.hasPtn(c(  7,NA,NA,28    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(  7,11,21,NA,34 ),aZoid,thld=3,fixIdx=1) ) cnt<-cnt+1
			# < 9>
			if( fCutU.hasPtn(c(  2, 9,10,31,32 ),aZoid,thld=3,fixIdx=2) ) cnt<-cnt+1
			# <12>
			if( fCutU.hasPtn(c( 12,NA,15,20 ),aZoid) ) cnt<-cnt+1
			# <29>
			if( fCutU.hasPtn(c( 23,29 ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c(       29,NA,38    ),aZoid) ) cnt<-cnt+1
			# <30>
			if( fCutU.hasPtn(c( 11, 1,23,30 ),aZoid,thld=3,fixIdx=4) ) cnt<-cnt+1
			# <38>
			if( fCutU.hasPtn(c(       29,NA,38    ),aZoid) ) cnt<-cnt+1
			if( fCutU.hasPtn(c( 15,NA,NA,36,38,40 ),aZoid,thld=3,fixIdx=5) ) cnt<-cnt+1
			# <44>
			if( fCutU.hasPtn(c( 18,20,44 ),aZoid) ) cnt<-cnt+1
			cntMtx[idx,"rawFV"] <- cnt
		}
		if( TRUE ){ # rem		# u0.zoidMtx_ana( stdMI$rawTail%%10 )
			cnt <- 0
			if( fCutU.remFilt(aZoid[1],c(         ),c(       )) ) cnt<-cnt+1 # 1
			if( fCutU.remFilt(aZoid[2],c( 9,7,0   ),c(  9,27 )) ) cnt<-cnt+1 # 2
			if( fCutU.remFilt(aZoid[3],c( 9,2     ),c( 12,18 )) ) cnt<-cnt+1 # 3
			if( fCutU.remFilt(aZoid[4],c(         ),c(       )) ) cnt<-cnt+1 # 4
			if( fCutU.remFilt(aZoid[5],c( 0,8,4,7 ),c( 28    )) ) cnt<-cnt+1 # 5
			if( fCutU.remFilt(aZoid[6],c( 5,9     ),c( 45    )) ) cnt<-cnt+1 # 6
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aCStep[1]%in%c( 12       ) ) cnt<-cnt+1
			if( aCStep[2]%in%c(          ) ) cnt<-cnt+1
			if( aCStep[3]%in%c( 10       ) ) cnt<-cnt+1
			if( aCStep[4]%in%c(  6, 5,11 ) ) cnt<-cnt+1
			if( aCStep[5]%in%c( 14, 8, 5 ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1CStep.cnt"]	# fv in cStep
			if( fCutU.hasPtn(c( 4,5 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+0]==c( 6, 4 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5,12 ),aCStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aCStep[1:2+1]==c( 11, 1 )) )	cnt.w1<-cnt.w1+1
			if( fCutU.hasPtn(c( 5, 7 ),aCStep) )	cnt.w1<-cnt.w1+1	# -
			# -------------------------------------------------------------------------------------
			#     FV :    1 (4)   4 (5)   5 (4)   6 (2)   7 (2)   11 (2)   12 (3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(1,4)]*c(1,3)==aCStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( 2,5 )])==sum(aCStep[c( 3,4 )]) )	cnt.w2<-cnt.w2+1	# 16
			if( sum(aCStep[c( 2,5 )])==sum(aCStep[c( 1,4 )]) )	cnt.w2<-cnt.w2+1	# 16

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 27 29 30 38 44    |20  2  1  8  6 |                        |1 0 2 2 1 |1 2 2 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  4 -15   0   3   0  -2 |0 2 1 2 1 |2 1 2 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 | -3  -3 -11 -12 -10  -2 |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1

		}
		if( TRUE ){ # fStep		#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
			cnt <- 0
			if( aFStep[1]%in%c(        ) ) cnt<-cnt+1
			if( aFStep[2]%in%c(  4, -1 ) ) cnt<-cnt+1
			if( aFStep[3]%in%c(  3     ) ) cnt<-cnt+1
			if( aFStep[4]%in%c( -5     ) ) cnt<-cnt+1
			if( aFStep[5]%in%c( -1, -3 ) ) cnt<-cnt+1
			if( aFStep[6]%in%c( -1     ) ) cnt<-cnt+1

			cnt.w1 <- cccObj$scoreMtx[idx,"w1FStep.cnt"]	# fv in fStep
			if( fCutU.hasPtn(c( -2, 1 ),aFStep) )	cnt.w1<-cnt.w1+1
			if( 1<sum(aFStep[1:2+3]==c( 2,-5 )) )	cnt.w1<-cnt.w1+1
			# -------------------------------------------------------------------------------------
			#     FV :    -4 (2)   -3 (2)   -2 (4)   -1 (2)   0 (2)   1 (5)   3 (3)   4 (2) 
			cnt.w2 <- 0
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(3,5)] ) )	cnt.w2<-cnt.w2+1
			if( 1<sum( aFStep[c(3,4)]*c(1,2)==aFStep[c(5,1)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[1]==sum(aFStep[ 3:5 ]) )	cnt.w2<-cnt.w2+1
			if( aFStep[3]==sum(aFStep[ 1:2 ]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,3)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(1,2,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[4]==sum(aFStep[c(3,5  )]) )	cnt.w2<-cnt.w2+1
			if( aFStep[5]==sum(aFStep[ 1:2 ]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,3,5)]) )	cnt.w2<-cnt.w2+1
			if( aFStep[6]==sum(aFStep[c(2,4  )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( 1,6 )])==sum(aFStep[c( 3,4 )]) )	cnt.w2<-cnt.w2+1	# -3
			if( sum(aFStep[c( 1,6 )])==sum(aFStep[c( 4,5 )]) )	cnt.w2<-cnt.w2+1	# -3

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid는 cccObj 안에..

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
	pastHpn <- rep( FALSE ,nrow(scoreMtx) )	;names(pastHpn)<-rownames(scoreMtx)

	if( TRUE ){	# 1.1.a reb - common
		# 1.1.a reb - gold/last 3~7
		rebHpn.sum <- sum(cccMtx[,"reb"]>0)
		if( (rebHpn.sum<3) || (7<rebHpn.sum) )	return( 10 )

		# 1.1.a reb - gold/last 이전 H와 연속은 2개 이내. (3개는 OL)
		lastHpn <- c("basic","nextRebNum","nextColVal_1","nextColVal_2","nextColVal_4")	# late
		if( 2<sum(cccMtx[lastHpn,"reb"]>0) )	return( 10 )
		lastHpn <- c("basic","nextRebNum","nextColVal_4","nextColVal_5","nextColVal_6")	# late
		if( 2<sum(cccMtx[lastHpn,"reb"]>0) )	return( 10 )

		# 1.1.a reb - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ809"]]	<- c("nextZW","nextQuo10","nextFStepBin")
		pastHpnLst[["toZ814"]]	<- c("basic","nextZW","nextBin","nextColVal_1","nextColVal_5")
		pastHpnLst[["toZ816"]]	<- c("nextRebNum","nextFStepBin"
									,"nextColVal_1","nextColVal_3","nextColVal_4","nextColVal_5","nextColVal_6")
		pastHpnLst[["toZ819"]]	<- c("nextBin","nextRebNum","nextCStepBin","nextFStepBin","nextColVal_4")
		pastHpnLst[["toZ820"]]	<- c("nextZW","nextColVal_2","nextColVal_4","nextColVal_6")
		pastHpnLst[["toZ821"]]	<- c("nextZW","nextColVal_2","nextColVal_3")
		pastHpnLst[["toZ822"]]	<- c("basic","nextQuo10","nextRebNum")
		pastHpnLst[["toZ823"]]	<- c("basic","nextRebNum","nextColVal_4","nextColVal_5","nextColVal_6")
		pastHpnLst[["toZ824"]]	<- c("basic","nextRebNum","nextColVal_1","nextColVal_2","nextColVal_4")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"reb"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"reb"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.b nbor - common
		# 1.1.b nbor - 0~2
		if( 2<sum(0<cccMtx[,"nbor"]) )	return( 10 )

		# 1.1.b nbor - common 과거패턴 재발제거.
		#	- gold
		pastHpn[]<-FALSE	;pastHpn[c("nextColVal_2","nextColVal_3")]<-TRUE					# toZ814
		if( all( pastHpn== (cccMtx[,"nbor"]>0) ) )	return( 10 )
		pastHpn[]<-FALSE	;pastHpn[c("nextZW")]<-TRUE											# toZ823
		if( all( pastHpn== (cccMtx[,"nbor"]>0) ) )	return( 10 )
		#	- late
		#		gold																			  toZ823
	}
	if( TRUE ){	# 1.1.c spanM - common
		# 1.1.b spanM - 0~3
		if( 3<sum(0<cccMtx[,"spanM"]) )	return( 10 )

		# 1.1.c spanM - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		#	- gold --------------------------------------------		
		pastHpnLst[["toZ809"]]	<- c("nextBin","nextCStepBin","nextColVal_1")
		pastHpnLst[["toZ814"]]	<- c("nextZW","nextColVal_2")
		pastHpnLst[["toZ816"]]	<- c("nextColVal_3")
		pastHpnLst[["toZ823"]]	<- c("nextQuo10","nextColVal_5","nextColVal_6")
		#	- late --------------------------------------------
		pastHpnLst[["toZ819"]]	<- c("nextBin","nextFStepBin","nextColVal_4")
		#		gold	 toZ820
		pastHpnLst[["toZ822"]]	<- c("nextZW")
		#		gold	 toZ823
		pastHpnLst[["toZ824"]]	<- c("nextZW","nextCStepBin","nextFStepBin")

		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"spanM"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"spanM"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.d quoAll - common
		# 1.1.d quoAll - 0~2
		if( 3<sum(0<cccMtx[,"quoAll"]) )	return( 10 )

		# 1.1.d quoAll - common 과거패턴 재발제거.
		#	- gold
		pastHpnLst <-list() 
		pastHpnLst[["toZ821"]]	<- c("basic","nextCStepBin")
		pastHpnLst[["toZ823"]]	<- c("nextZW","nextColVal_1")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"quoAll"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"quoAll"]>0) ){
				return( 10 )
			}
		}
	}
	if( TRUE ){	# 1.1.e quoPtn - common
		# 1.1.e quoPtn - 0~3
		if( 4<sum(0<cccMtx[,"quoPtn"]) )	return( 10 )

		# 1.1.e quoPtn - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ809"]]	<- c("nextFStepBin","nextColVal_2","nextColVal_5")
		pastHpnLst[["toZ819"]]	<- c("nextQuo10")
		pastHpnLst[["toZ820"]]	<- c("nextZW","nextBin")
		pastHpnLst[["toZ821"]]	<- c("nextRebNum","nextColVal_2","nextColVal_4")
		pastHpnLst[["toZ822"]]	<- c("nextZW","nextBin")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"quoPtn"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"quoPtn"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.f zw - common
		# 1.1.f zw - 0~2
		if( 3<sum(0<cccMtx[,"zw"]) )	return( 10 )

		# 1.1.f zw - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ809"]]	<- c("nextColVal_3","nextColVal_4")
		pastHpnLst[["toZ819"]]	<- c("nextColVal_1")
		pastHpnLst[["toZ820"]]	<- c("nextFStepBin","nextColVal_2")
		pastHpnLst[["toZ821"]]	<- c("nextColVal_6")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"zw"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"zw"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.g remH0 - common
		# 1.1.g remH0 - 0~0
		if( 1<sum(0<cccMtx[,"remH0"]) )	return( 10 )

		# 1.1.g remH0 - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"remH0"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"remH0"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.h remH1 - common
		# 1.1.h remH1 - 0~1
		if( 2<sum(0<cccMtx[,"remH1"]) )	return( 10 )

		# 1.1.h remH1 - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ820"]]	<- c("nextColVal_2")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"remH1"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"remH1"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.i cStep2 - common
		# 1.1.i cStep2 - 0~3
		if( 4<sum(0<cccMtx[,"cStep2"]) )	return( 10 )

		# 1.1.i cStep2 - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ820"]]	<- c("basic","nextFStepBin")
		pastHpnLst[["toZ821"]]	<- c("basic")
		pastHpnLst[["toZ823"]]	<- c("nextCStepBin")
		pastHpnLst[["toZ824"]]	<- c("nextZW","nextColVal_3","nextColVal_5")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"cStep2"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"cStep2"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.j cStep3 - common
		# 1.1.j cStep3 - 0~1
		if( 2<sum(0<cccMtx[,"cStep3"]) )	return( 10 )

		# 1.1.j cStep3 - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ821"]]	<- c("basic")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(cccMtx[,"cStep3"]) != length(pastHpnLst[[nIdx]]) ) next

			if( all(cccMtx[ pastHpnLst[[nIdx]] ,"cStep3"]>0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.1.z etc

		hpnSum <- apply( cccMtx[,c("spanM","quoPtn")] ,1 ,sum )
		if( 1 > sum(hpnSum>0) )	return( 10 )
		if( 6 < sum(hpnSum>0) )	return( 10 )

	}


	if( TRUE ){	# 1.2.a c3n - common
		hpnCnt <- apply( cStepValMtx[,c("c31","c32","c33","c34")] ,1 ,sum )
		# 1.2.a c3n - 0~2
		if( 2<sum(hpnCnt>0) )	return( 10 )

		# 1.2.a c3n - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ821"]]	<- c("nextRebNum")
		pastHpnLst[["toZ822"]]	<- c("nextZW","nextColVal_1")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(hpnCnt>0) != length(pastHpnLst[[nIdx]]) ) next

			if( all(hpnCnt[pastHpnLst[[nIdx]]] > 0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.2.b c2n - common
		hpnCnt <- apply( cStepValMtx[,c("c21","c22","c23","c24","c25")] ,1 ,sum )
		# 1.2.b c2n - 0~4, 3~5
		if( 5<sum(hpnCnt>0) )	return( 10 )

		# 1.2.b c2n - common 2개 이상 발생한 차원은 다음 H에서 발생 없음.
		if( hpnCnt["nextColVal_2"]>0 )	return( 10 )	# toZ824

		# 1.2.b c2n - common 2개 이상 발생 dimPlace 수 : 0~2
		if( 2 < sum(hpnCnt>2) )	return( 10 )	# toZ824

		# 1.2.b c2n - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ809"]]	<- c("nextZW","nextColVal_1")
		pastHpnLst[["toZ814"]]	<- c("basic","nextZW","nextColVal_4","nextColVal_5")
		pastHpnLst[["toZ816"]]	<- c("nextFStepBin")
		pastHpnLst[["toZ819"]]	<- c("nextBin","nextCStepBin","nextColVal_1","nextColVal_2")
		pastHpnLst[["toZ821"]]	<- c("nextZW","nextRebNum","nextCStepBin","nextColVal_3","nextColVal_6")
		pastHpnLst[["toZ822"]]	<- c("basic","nextZW","nextBin")
		pastHpnLst[["toZ823"]]	<- c("nextBin","nextColVal_5","nextColVal_6")
		pastHpnLst[["toZ824"]]	<- c("nextRebNum","nextColVal_2","nextColVal_4")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(hpnCnt) != length(pastHpnLst[[nIdx]]) ) next

			if( all(hpnCnt[pastHpnLst[[nIdx]]] > 0) ){
				return( 10 )
			}
		}

	}
	if( TRUE ){	# 1.2.c max2/min2 - common
		hpnCnt <- apply( cStepValMtx[,c("max2","min2")] ,1 ,sum )
		# 1.2.c max2/min2 - 0~0
		if( 2<sum(hpnCnt>0) )	return( 10 )

		# 1.2.c max2/min2 - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ819"]]	<- c("nextColVal_1")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(hpnCnt) != length(pastHpnLst[[nIdx]]) ) next

			if( all(hpnCnt[pastHpnLst[[nIdx]]] > 0) ){
				return( 10 )
			}
		}

	}

	if( TRUE ){	# 1.3.a rawFV - common
		# 1.3.a rawFV - 0~2(sum)
		if( 3<sum(scoreMtx[,"rawFV"]) )	return( 10 )
		if( 2<sum(scoreMtx[,"rawFV"]>0) )	return( 10 )

		# 1.3.a rawFV - common 과거패턴 재발제거.
		pastHpnLst <-list() 
		pastHpnLst[["toZ809"]]	<- c("nextRebNum")
		pastHpnLst[["toZ816"]]	<- c("nextColVal_3")
		pastHpnLst[["toZ823"]]	<- c("nextZW")
		for( nIdx in attributes(pastHpnLst)$names ){
			if( sum(scoreMtx[,"rawFV"]>0) != length(pastHpnLst[[nIdx]]) ) next

			if( all(scoreMtx[ pastHpnLst[[nIdx]] ,"rawFV"]>0) ){
				return( 10 )
			}
		}
	}
	# if( TRUE ){	# 1.1.a xxx - common
	# 	# 1.1.d xxx - 0~2
	# 	if( <sum(cccMtx[,"xxx"]) )	return( 10 )

	# 	# 1.1.a xxx - common 과거패턴 재발제거.
	# 	pastHpnLst <-list() 
	# 	pastHpnLst[["toZ"]]	<- c("","")
	# 	for( nIdx in attributes(pastHpnLst)$names ){
	# 		if( sum(cccMtx[,"xxx"]) != length(pastHpnLst[[nIdx]]) ) next

	# 		if( all(cccMtx[ pastHpnLst[[nIdx]] ,"xxx"]>0) ){
	# 			return( 10 )
	# 		}
	# 	}
	# }

	return( flagCnt )

} # finalFilt.common( )



fCut.wildF_cStep <- function( aZoid ){

	rName <- c("basic" ,"nextZW" ,"nextQuo10" ,"nextBin" ,"nextRebNum" ,"nextCStepBin" ,"nextFStepBin" 
				,"nextColVal_1" ,"nextColVal_2" ,"nextColVal_3" ,"nextColVal_4" ,"nextColVal_5" ,"nextColVal_6" )
	cName <- c("*1","*2")
	wildFMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
	colnames(wildFMtx) <- cName	;rownames(wildFMtx) <- rName


	aCStep <- aZoid[2:6] - aZoid[1:5]	
	if(TRUE){	# basic
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 2, 15,  5)) ) cnt<-cnt+1	# 6
					if( 1<sum(aCStep[1:2+3]==c( 7,  6    )) ) cnt<-cnt+1	# 2
					if( 1<sum(aCStep[1:3+2]==c( 6, 10,  2)) ) cnt<-cnt+1	# 5

					if( fCutU.hasPtn(c( 8, 6),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 7, 6)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["basic","*1"] <- 1

		#	*2
		cnt <- 0
					if( all(aCStep[2:3]== aCStep[4]*c(3,5) ) ) cnt<-cnt+1
					if( sum(aCStep[c(3,4)])==sum(aCStep[c(1,5)]) ) cnt<-cnt+1
					if( aCStep[1]==sum(aCStep[c(4,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["basic","*2"] <- 1
	}
	if(TRUE){	# nextZW
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:2+0]==c( 5, 15    )) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 4,  5,  4)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 5,  2,  1)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 1,  5, 15)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c(  1,17),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 17, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 10, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:3+0]==c( 3,9,7 )) ) cnt<-cnt+1	#
		if( cnt>0 )	wildFMtx["nextZW","*1"] <- 1

		#	*2
		cnt <- 0
					if( all(aCStep[4:5]== aCStep[1]*c(1,3) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextZW","*2"] <- 1
	}
	if(TRUE){	# nextQuo10
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:2+0]==c( 9,  4    )) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+0]==c( 2,  7,  9)) ) cnt<-cnt+1	#  7
					if( 1<sum(aCStep[1:3+1]==c(27,  7,  6)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+2]==c(27,  7,  6)) ) cnt<-cnt+1	#  9
					if( 1<sum(aCStep[1:3+2]==c( 6,  2,  1)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 8, 9),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextQuo10","*1"] <- 1

		#	*2
		cnt <- 0
					if( aCStep[3]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
					if( aCStep[4]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextQuo10","*2"] <- 1
	}
	if(TRUE){	# nextBin
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 5,  3,  8)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 8, 18,  4)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c( 7,  3, 13)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 6, 14,  3)) ) cnt<-cnt+1	# 18
					if( 1<sum(aCStep[1:3+2]==c(14,  3,  5)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 7, 3),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 14, 3)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 4, 7),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(  6,14)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextBin","*1"] <- 1

		#	*2
		cnt <- 0
					if( aCStep[3]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextBin","*2"] <- 1
	}
	if(TRUE){	# nextRebNum
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 6,  5,  8)) ) cnt<-cnt+1	#  9

					if( fCutU.hasPtn(c( 1, 9),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 5),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c(1,2)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(12, 3),aCStep) ) cnt<-cnt+1 # 
					if( fCutU.hasPtn(c( 7, 7),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextRebNum","*1"] <- 1

		#	*2
		cnt <- 0
					if( all(aCStep[4:5]== aCStep[2]*c(1,5) ) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextRebNum","*2"] <- 1
	}
	if(TRUE){	# nextCStepBin
		#	*1
		cnt <- 0
					if( aCStep[1]%in%c(15, 4, 2 ) ) cnt<-cnt+1
					if( aCStep[2]%in%c( 4, 3, 6 ) ) cnt<-cnt+1
					if( aCStep[3]%in%c( 7       ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+0]==c( 8,  3,  9)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 5,  3,  8)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+2]==c( 5,  3,  8)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+2]==c( 2,  7,  3)) ) cnt<-cnt+1	# 18
					if( 1<sum(aCStep[1:3+2]==c( 7,  3,  2)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 3, 7),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(3,5)) ) cnt<-cnt+1 #
		if( cnt>0 )	wildFMtx["nextCStepBin","*1"] <- 1

		#	*2
		cnt <- 0
					if( aCStep[5]==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextCStepBin","*2"] <- 1
	}
	if(TRUE){	# nextFStepBin
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 6,  3, 15)) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+0]==c(12,  5,  1)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c(11,  1, 15)) ) cnt<-cnt+1	#  3
					if( 1<sum(aCStep[1:3+1]==c( 6,  9,  8)) ) cnt<-cnt+1	# 15
					if( 1<sum(aCStep[1:3+2]==c( 7, 10,  6)) ) cnt<-cnt+1	# 11

					if( fCutU.hasPtn(c( 3, 8),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8, 6),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c(12, 5)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 3,15),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+2]==c( 1,15)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9,12),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 6, 1),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextFStepBin","*1"] <- 1

		#	*2
		cnt <- 0
					if( all(aCStep[c(2,4)]== aCStep[3]*c(2,5) ) ) cnt<-cnt+1
					if( aCStep[5]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
					if( sum(aCStep[c(3,5)])==sum(aCStep[c(1,2)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextFStepBin","*2"] <- 1
	}
	if(TRUE){	# nextColVal_1
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 3, 10,  6)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+1]==c( 3,  3, 10)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c( 3,  3, 10)) ) cnt<-cnt+1	#  2

					if( fCutU.hasPtn(c( 2,13 ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c(13, 3)) ) cnt<-cnt+1
		#	*2
		cnt <- 0
					if( all(aCStep[3:4]== aCStep[1]*c(1,1) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_1","*2"] <- 1
	}
	if(TRUE){	# nextColVal_2
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:2+0]==c( 2,  5    )) ) cnt<-cnt+1	#  8
					if( 1<sum(aCStep[1:3+1]==c( 1,  7,  8)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+2]==c( 1, 17,  4)) ) cnt<-cnt+1	#  2
					if( 1<sum(aCStep[1:3+2]==c(13,  1,  7)) ) cnt<-cnt+1	#  5

					if( fCutU.hasPtn(c( 8, 4),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c(11, 1),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 2, 5)) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 1, 7),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 5, 2)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_2","*1"] <- 1

		#	*2
		cnt <- 0
					if( sum(aCStep[c(2,4)])==sum(aCStep[c(3,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_2","*2"] <- 1
	}
	if(TRUE){	# nextColVal_3
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 3, 13,  7)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+1]==c( 3,  3, 13)) ) cnt<-cnt+1	#  1
					if( 1<sum(aCStep[1:3+2]==c( 9,  2, 10)) ) cnt<-cnt+1	#  8

					if( fCutU.hasPtn(c( 3, 2),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 9, 3),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_3","*1"] <- 1

		#	*2
		cnt <- 0
					if( all(aCStep[c(1,2)]==aCStep[c(3,4)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_3","*2"] <- 1
	}
	if(TRUE){	# nextColVal_4
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:2+0]==c( 9,  1    )) ) cnt<-cnt+1	# 4
					if( 1<sum(aCStep[1:3+0]==c( 6,  4,  7)) ) cnt<-cnt+1	# 8
					if( 1<sum(aCStep[1:3+0]==c( 7,  9,  1)) ) cnt<-cnt+1	# 7
					if( 1<sum(aCStep[1:3+2]==c( 5, 12,  6)) ) cnt<-cnt+1	# 1

					if( fCutU.hasPtn(c( 4, 9),aCStep) ) cnt<-cnt+1
					if( fCutU.hasPtn(c( 8, 8),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_4","*1"] <- 1

		#	*2
		cnt <- 0
					if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
					if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_4","*2"] <- 1
	}
	if(TRUE){	# nextColVal_5
		#	*1
		cnt <- 0
					if( 1<sum(aCStep[1:3+0]==c( 3, 10,  3)) ) cnt<-cnt+1	#  5
					if( 1<sum(aCStep[1:3+0]==c( 8,  5,  8)) ) cnt<-cnt+1	# 12
					if( 1<sum(aCStep[1:3+1]==c( 3,  1,  4)) ) cnt<-cnt+1	#  6
					if( 1<sum(aCStep[1:3+1]==c( 6,  4,  7)) ) cnt<-cnt+1	#  4

					if( fCutU.hasPtn(c( 1, 5),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+0]==c( 5,13)) ) cnt<-cnt+1
					if( all(aCStep[1:2+1]==c( 4, 8)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_5","*1"] <- 1

		#	*2
		cnt <- 0
					if( aCStep[2]==sum(aCStep[c( 1, 5)]) ) cnt<-cnt+1
					if( sum(aCStep[c(1,2)])==sum(aCStep[c(3,4,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_5","*2"] <- 1
	}
	if(TRUE){	# nextColVal_6
		#	*1
		cnt <- 0
					if( 1<sum(aFStep[1:3+0]==c(  7,  -5, -10)) ) cnt<-cnt+1 #  -3
					if( 1<sum(aFStep[1:3+0]==c(  4,  -3,  12)) ) cnt<-cnt+1 #  -2
					if( 1<sum(aFStep[1:3+1]==c( -4,  -9, -22)) ) cnt<-cnt+1 #   7
					if( 1<sum(aFStep[1:3+2]==c(  1,   9,   6)) ) cnt<-cnt+1 #  -5
					if( 1<sum(aFStep[1:3+2]==c(  9,   6,   7)) ) cnt<-cnt+1 #  -5
					if( 1<sum(aFStep[1:3+3]==c(  2,   4,   9)) ) cnt<-cnt+1 #  -7
		if( cnt>0 )	wildFMtx["nextColVal_6","*1"] <- 1

		#	*2
		cnt <- 0
					if( aFStep[6]==sum(aFStep[c(2,4)]) ) cnt<-cnt+1
					if( aFStep[5]==sum(aFStep[c(1,6)]) ) cnt<-cnt+1
					if( aFStep[4]==sum(aFStep[c(1,2)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_6","*2"] <- 1
	}

	return( wildFMtx )

} # fCut.wildF_cStep()

fCut.wildF_fStep <- function( aZoid ,lastZoid ){

	rName <- c("basic" ,"nextZW" ,"nextQuo10" ,"nextBin" ,"nextRebNum" ,"nextCStepBin" ,"nextFStepBin" 
				,"nextColVal_1" ,"nextColVal_2" ,"nextColVal_3" ,"nextColVal_4" ,"nextColVal_5" ,"nextColVal_6" )
	cName <- c("*1","*2")
	wildFMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
	colnames(wildFMtx) <- cName	;rownames(wildFMtx) <- rName

	if(TRUE){	# xxx
		#	*1
		cnt <- 0
		if( cnt>0 )	wildFMtx["xxx","*1"] <- 1

		#	*2
		cnt <- 0
		if( cnt>0 )	wildFMtx["xxx","*2"] <- 1
	}

	return( wildFMtx )

} # fCut.wildF_fStep()

finalFilt.gold.old <- function( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol ) {
	
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












