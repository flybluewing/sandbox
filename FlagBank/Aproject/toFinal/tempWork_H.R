
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
		if( 1<sum(aCStep[1:3+0]==c( 5,  5,  4 )) ) cnt<-cnt+1	# 2
		if( 1<sum(aCStep[1:3+1]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
		if( 1<sum(aCStep[1:3+2]==c( 4,  3,  9 )) ) cnt<-cnt+1	# 5
		if( 1<sum(aCStep[1:3+2]==c( 6,  6,  2 )) ) cnt<-cnt+1	# 4

		if( fCutU.hasPtn(c( 8, 5 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(11, 1 ),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+2]==c(15, 5)) ) cnt<-cnt+1
		if( all(aCStep[1:2+0]==c( 2,15)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["basic","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[2:3]== aCStep[4]*c(3,1) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["basic","*2"] <- 1
	}
	if(TRUE){	# nextZW
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:2+0]==c( 1,  4    )) ) cnt<-cnt+1	#  7
		if( 1<sum(aCStep[1:3+1]==c(13,  1,  7)) ) cnt<-cnt+1	#  1
		if( 1<sum(aCStep[1:3+2]==c( 1,  1,  4)) ) cnt<-cnt+1	#  7
		if( 1<sum(aCStep[1:3+2]==c( 6,  3, 11)) ) cnt<-cnt+1	#  8

		if( fCutU.hasPtn(c( 7,14),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 1, 8),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 1, 4),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+2]==c(15,14)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextZW","*1"] <- 1

		#	*2
		cnt <- 0
		if( aCStep[5]==sum(aCStep[c(1,3)]) ) cnt<-cnt+1
		if( aCStep[5]==sum(aCStep[c(4,3)]) ) cnt<-cnt+1
		if( sum(aCStep[c(1,4)])==sum(aCStep[c(2,3)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextZW","*2"] <- 1
	}
	if(TRUE){	# nextQuo10
		#	*1
		cnt <- 0
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
		if( cnt>0 )	wildFMtx["nextQuo10","*1"] <- 1

		#	*2
		cnt <- 0
		if( aCStep[1]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextQuo10","*2"] <- 1
	}
	if(TRUE){	# nextBin
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:2+0]==c( 8,  6    )) ) cnt<-cnt+1	# 20
		if( 1<sum(aCStep[1:3+1]==c( 2,  1,  8)) ) cnt<-cnt+1	#  2
		if( 1<sum(aCStep[1:3+1]==c( 2, 20,  2)) ) cnt<-cnt+1	#  8
		if( 1<sum(aCStep[1:3+2]==c( 4,  2, 20)) ) cnt<-cnt+1	#  6

		if( fCutU.hasPtn(c(20, 2),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+0]==c( 8, 6)) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 4,17),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextBin","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[4:5]== aCStep[2]*c(4,3) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextBin","*2"] <- 1
	}
	if(TRUE){	# nextRebNum
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 8,  7,  9)) ) cnt<-cnt+1	# 4
		if( 1<sum(aCStep[1:3+0]==c( 7,  9,  1)) ) cnt<-cnt+1	# 8
		if( 1<sum(aCStep[1:3+1]==c(10,  4,  2)) ) cnt<-cnt+1	# 7
		if( 1<sum(aCStep[1:2+3]==c(10,  4    )) ) cnt<-cnt+1	# 9

		if( fCutU.hasPtn(c(2,2),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextRebNum","*1"] <- 1

		#	*2
		cnt <- 0
		if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
		if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextRebNum","*2"] <- 1
	}
	if(TRUE){	# nextCStepBin
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c(11,  8,  3)) ) cnt<-cnt+1	# 5
		if( 1<sum(aCStep[1:3+1]==c( 3,  7, 20)) ) cnt<-cnt+1	# 8 
		if( 1<sum(aCStep[1:3+2]==c(11,  8,  3)) ) cnt<-cnt+1	# 3

		if( fCutU.hasPtn(c( 5, 3),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+1]==c( 1,12)) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextCStepBin","*1"] <- 1

		#	*2
		cnt <- 0
		if( aCStep[3]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1
		if( aCStep[2]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1
		if( aCStep[5]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextCStepBin","*2"] <- 1
	}
	if(TRUE){	# nextFStepBin
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 5,  2,  1)) ) cnt<-cnt+1	#  5
		if( 1<sum(aCStep[1:3+0]==c(14, 10,  6)) ) cnt<-cnt+1	#  2
		if( 1<sum(aCStep[1:3+1]==c(10,  6,  5)) ) cnt<-cnt+1	#  1
		if( 1<sum(aCStep[1:2+3]==c( 5,  2    )) ) cnt<-cnt+1	#  5

		if( fCutU.hasPtn(c( 5, 2),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+0]==c(14,10)) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 2, 1),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+1]==c(10, 6)) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(14, 9),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextFStepBin","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[4:5]== aCStep[1]*c(1,3) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextFStepBin","*2"] <- 1
	}
	if(TRUE){	# nextColVal_1
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 2,  4,  5)) ) cnt<-cnt+1	#  2
		if( 1<sum(aCStep[1:3+0]==c( 4,  5, 25)) ) cnt<-cnt+1	#  4
		if( 1<sum(aCStep[1:3+1]==c( 2,  4,  5)) ) cnt<-cnt+1	#  5
		if( 1<sum(aCStep[1:3+2]==c( 2,  4,  5)) ) cnt<-cnt+1	#  4

		if( fCutU.hasPtn(c( 7, 1),aCStep) ) cnt<-cnt+1
		if( all(aCStep[1:2+2]==c( 4,30 )) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_1","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[c(2,5)]== aCStep[1]*c(2,2) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_1","*2"] <- 1
	}
	if(TRUE){	# nextColVal_2
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 4, 13,  3)) ) cnt<-cnt+1	#  1
		if( 1<sum(aCStep[1:3+1]==c( 1, 10,  3)) ) cnt<-cnt+1	#  3
		if( 1<sum(aCStep[1:3+2]==c( 5,  4, 13)) ) cnt<-cnt+1	#  1
		if( 1<sum(aCStep[1:3+2]==c( 6,  5,  4)) ) cnt<-cnt+1	#  1

		if( fCutU.hasPtn(c( 11, 3 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(  2,13 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(  6, 3 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(  7, 4 ),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_2","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[1]==aCStep[4:5]) ) cnt<-cnt+1
		if( aCStep[3]==sum(aCStep[c(1,4,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_2","*2"] <- 1
	}
	if(TRUE){	# nextColVal_3
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 2, 15,  5)) ) cnt<-cnt+1	#  2
		if( 1<sum(aCStep[1:3+1]==c( 2, 15,  5)) ) cnt<-cnt+1	#  5
		if( 1<sum(aCStep[1:3+2]==c( 2, 15,  5)) ) cnt<-cnt+1	#  5
		if( 1<sum(aCStep[1:3+2]==c( 2, 15,  5)) ) cnt<-cnt+1	#  4

		if( fCutU.hasPtn(c( 5, 4),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 1, 6),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_3","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[2:3]== aCStep[4]*c(3,1) ) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_3","*2"] <- 1
	}
	if(TRUE){	# nextColVal_4
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:3+0]==c( 7,  4,  2)) ) cnt<-cnt+1	#  6
		if( 1<sum(aCStep[1:3+0]==c( 2, 20,  2)) ) cnt<-cnt+1	#  3
		if( 1<sum(aCStep[1:3+0]==c( 8,  3,  5)) ) cnt<-cnt+1	#  1

		if( fCutU.hasPtn(c(6,2),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(8,4),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(5,2),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_4","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[c(1,5)]== aCStep[2]*c(2,6)) ) cnt<-cnt+1
		if( aCStep[5]==sum(aCStep[c(1,4)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_4","*2"] <- 1
	}
	if(TRUE){	# nextColVal_5
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:2+0]==c( 4,  1    )) ) cnt<-cnt+1	#  6
		if( 1<sum(aCStep[1:3+0]==c(10,  1,  9)) ) cnt<-cnt+1	# 10
		if( 1<sum(aCStep[1:3+0]==c( 1,  9,  2)) ) cnt<-cnt+1	#  1
		if( 1<sum(aCStep[1:3+2]==c(20,  5, 10)) ) cnt<-cnt+1	#  2

		if( fCutU.hasPtn(c( 7,10 ),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_5","*1"] <- 1

		#	*2
		cnt <- 0
		if( all(aCStep[1:2]== aCStep[5]*c(3,5) ) ) cnt<-cnt+1
		if( aCStep[2]==sum(aCStep[c(3,4)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_5","*2"] <- 1
	}
	if(TRUE){	# nextColVal_6
		#	*1
		cnt <- 0
		if( 1<sum(aCStep[1:2+0]==c( 9,  1    )) ) cnt<-cnt+1	#  4
		if( 1<sum(aCStep[1:3+0]==c(10,  4,  1)) ) cnt<-cnt+1	#  8
		if( 1<sum(aCStep[1:3+2]==c(20,  5, 10)) ) cnt<-cnt+1	#  7
		if( 1<sum(aCStep[1:3+2]==c( 3,  3,  3)) ) cnt<-cnt+1	#  9
		if( 1<sum(aCStep[1:3+2]==c( 7,  9,  1)) ) cnt<-cnt+1	#  1

		if( fCutU.hasPtn(c( 8,9 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c(10,5 ),aCStep) ) cnt<-cnt+1
		if( fCutU.hasPtn(c( 7,2 ),aCStep) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_6","*1"] <- 1

		#	*2
		cnt <- 0
		if( aCStep[4]==sum(aCStep[c(2,5)]) ) cnt<-cnt+1
		if( aCStep[2]==sum(aCStep[c(3,5)]) ) cnt<-cnt+1
		if( cnt>0 )	wildFMtx["nextColVal_6","*2"] <- 1
	}
	
	return( wildFMtx )

} # fCut.wildF_cStep()




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



	# surFlag(scoreMtx) ------------------------------------------------------
	surFlag <- rep( TRUE ,length(allIdxF) )
	fName <- attributes(ccObjLst)$names
	cName <- c( "ccc", "auxZW", "auxQuo", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName
	scoreMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(thld) )
	rownames(scoreMtx) <- fName		;colnames(scoreMtx) <- names(thld)
	cName <- c( "reb",  "nbor", "spanM", "quoAll", "quoPtn", "zw",  "remH0", "remH1", "cStep2", "cStep3" )	
	cccMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )	
	rownames(cccMtx) <- fName		;colnames(cccMtx) <- cName
	cccMtx.rCol <- cName[ cName!="reb" ]
	cName <- c( "c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2" )	# cccObj$cStepValMtx 
	cStepValMtx = matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )
	rownames(cStepValMtx) <- fName		;colnames(cStepValMtx) <- cName
	cnt4Spy <- rep( 1 ,length(allIdxF) )
	cnt4Spy <- rep( TRUE ,length(allIdxF) )
	cName <- c("reb","spanM","quoPtn")
	spyMtx <- matrix( 0, ncol=length(cName), nrow=length(allIdxF) )	;colnames(spyMtx)<-cName

	for( aIdx in 1:length(allIdxF) ){
		scoreMtx[,] <- 0	;cccMtx[,] <- 0		;cStepValMtx[,] <- 0
		for( nIdx in fName ){
			cccVal <- ccObjLst[[nIdx]]$cccMtx[aIdx,]
			cccMtx[nIdx,] <- ccObjLst[[nIdx]]$cccMtx[aIdx,]

			scoreMtx[nIdx,"ccc"] <- sum( 0 < cccVal[cccMtx.rCol] )
			scoreMtx[nIdx,c("auxZW", "auxQuo")] <- ccObjLst[[nIdx]]$auxCntMtx[aIdx,c("auxZW", "auxQuo")]
			scoreMtx[nIdx,c("raw", "rawFV", "rem", "cStep", "fStep")] <-
				ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]

			cStepValMtx[nIdx,] <- ccObjLst[[nIdx]]$cStepValMtx[aIdx,]

		} # for(nIdx)

		# scoreMtx : ccc auxZW auxQuo raw rawFV rem cStep fStep
		# cccMtx : reb nbor spanM quoAll quoPtn zw remH0 remH1 cStep2 cStep3
		# cStepValMtx : c31 c32 c33 c34 c21 c22 c23 c24 c25 max2 min2

		cutCnt <- 0
		cnt4Spy[aIdx] <- sum(cccMtx[,c("reb","spanM","quoPtn")])
		spyMtx[aIdx,] <- apply( cccMtx[,c("reb","spanM","quoPtn")] ,2 ,function(p){sum(p>0)} )

		# 이벤트 발생
		eventFlag <- apply(scoreMtx ,1 ,function(score){ sum(score>=thld) })
		if( TRUE ){	# eventFlag
			if( any( 0<eventFlag[c("nextZW","nextBin","nextColVal_3","nextColVal_6")] ) ){
				# 2.1 one dimPlane - gold : event h방향 연속발생 없음.
				surFlag[aIdx] <- FALSE
				next
			}
			if( any( 2<eventFlag[c("nextZW","nextQuo10","nextFStepBin","nextColVal_3")] ) ){
				# 2.1 one dimPlane - late : event h방향 연속발생 1~2
				#	(그런데 allIdxFObj$allIdxF.fCutCnt 단계에서 2개 이상은 모두 잘렸으니... 의미없다.)
				surFlag[aIdx] <- FALSE
				next
			}
			hpnCnt <- apply( scoreMtx ,1 ,sum )
			if( any(0==hpnCnt[c("basic","nextBin","nextRebNum","nextColVal_1")]) ){
				# 2.1 one dimPlane - common cnt 0~4 (0, 4는 h,dp 모든 방향에서 연속발생 없음)
				surFlag[aIdx] <- FALSE
				next
			}
			if( any(4<=hpnCnt[c("nextColVal_2","nextFStepBin")]) ){
				# 2.1 one dimPlane - common cnt 0~4 (0, 4는 h,dp 모든 방향에서 연속발생 없음)
				surFlag[aIdx] <- FALSE
				next
			}
			if( any(hpnCnt>4) ){
				# 2.1 one dimPlane - cnt 0~4
				surFlag[aIdx] <- FALSE
				next
			}
			hpnCnt.sum <- sum(hpnCnt)
			if( (hpnCnt.sum<20) || (31<hpnCnt.sum) ){
				# 2.2 sum:through dimPlane - common
				surFlag[aIdx] <- FALSE
				next
			}
			eventCnt <- sum( eventFlag )
			if( (eventCnt<1) || (2<eventCnt) ){ 
				# 2.2 sum:through dimPlane - gold : event 1~2(OL:,4)
				surFlag[aIdx] <- FALSE
				next
			}
		}

		# common : filt for gold & late
		flagCnt.gold <- finalFilt.common( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol )
		if( 5<flagCnt.gold ){
			surFlag[aIdx] <- FALSE
			next
		}


	}

	table(surFlag)	;kIdx <- head(which(!surFlag))	


	cnt <- apply( spyMtx[,c("spanM","quoPtn")] ,1 ,sum )

	allIdxF <- allIdxF[ surFlag ]
