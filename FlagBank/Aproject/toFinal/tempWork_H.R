
tempWork_H.R

# 공용
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

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#========================================================================


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








