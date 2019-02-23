# toZ848_H.R 최종접근
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     14 26 32 36 39 42    |12  6  4  3  3 |                        |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
			#   dup number  16:2   26:2   30:3   33:3   42:3   45:3
			#   zoid width  ... 28   23   38   44   40   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  844        2      8       [symm    ]  8(?),16,18,16, 8     col
			#  845        5     41 [desc1   ] 41(?),xx,xx,40,xx,xx,39     col
			#  846        6     45       [same    ] 45(?), .,45, .,45     col
			#  842        6     42 [sameEnd ] 42(?),xx,45,xx,45,xx,42     col
			#  847        6     42      [seqReb  ] 42(?),42,45,45,...     col
			#  8421       6     42 [symm    ] 42(?),42,45,45,45,42,42     col

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
			# < 16>   0      1 , 16 , 29 , 33 , 40 , 45 
			#               12 , 16 , 26 , 28 , 30 , 42 
			#          -->  NA , 16*, 23 , 23 , 20 , 39 
			# < 26>   1     14 , 26 , 32 , 36 , 39 
			#               16 , 26 , 28 , 30 , 42 
			#          -->  18 , 26*, NA , NA , 45 
			# < 30>   2      5 , 18 , 30 , 41 
			#               26 , 28 , 30 , 42 
			#          -->  NA , NA , 30*, 43!
			# < 33>  -1      8 , 13 , 15 , 33 , 45 
			#                1 , 16 , 29 , 33 , 40 
			#          -->  NA , 19 , NA , 33*, 35 
			# < 42>   0     19 , 21 , 30 , 33 , 34 , 42 
			#               12 , 16 , 26 , 28 , 30 , 42 
			#          -->   5 , 11 , 22 , 23 , 26 , 42*
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
			# 846        2      8       [same    ]  8(?), ., 8, ., 8     col
			# 842        2      6 [sameEnd ]  6(?),xx, 8,xx, 8,xx, 6     col
			# 845        2      6 [same    ]  6(?), ., ., 6, ., ., 6     col
			# 844        2      8       [symm    ]  8(?), 6, 8, 6, 8     col
			# 8461       5      3       [same    ]  3(?), ., 3, ., 3     col
			# 8421       5      9 [sameEnd ]  9(?),xx, 3,xx, 3,xx, 9     col
			# 8441       5      3       [symm    ]  3(?), 0, 3, 0, 3     col
			# 8462       6      5       [same    ]  5(?), ., 5, ., 5     col
			# 8422       6      2 [sameEnd ]  2(?),xx, 5,xx, 5,xx, 2     col
			# 847        6      2      [seqReb  ]  2(?), 2, 5, 5,...     col
			# 8423       6      2 [symm    ]  2(?), 2, 5, 5, 5, 2, 2     col
			# 1          3      7             [desc1   ]  7(?), 6, 5 Slide\\
			# 11         6     -1             [desc1   ] -1(?), 0, 1 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      845        3      4 [same    ]  4(?), ., ., 4, ., ., 4     col
			#      847        4      2             [same    ]  2(?), 2, 2     col
			#      8451       4      7          [sameEnd ]  7(?), 2, 2, 7     col
			#      1          1      9             [desc1   ]  9(?),10,11  Slide/
			#      11         1     10       [desc1   ] 10(?),xx,11,xx,12  Slide/
			#      12         2      2             [same    ]  2(?), 2, 2  Slide/
			#      13         2      5          [sameEnd ]  5(?), 2, 2, 5  Slide/
			#      14         3      2             [same    ]  2(?), 2, 2  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(6)   3(3)   4(3)   5(2)   12(4)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     14 26 32 36 39 42    |12  6  4  3  3 |                        |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			#     12 16 26 28 30 42(1) | 4 10  2  2 12 |  7  -2  -4 -13 -13  -3 |0 2 2 1 1 |2 2 1 1
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
			#       tgt.col banVal                      descript tgt.dir
			#      1       6     -3 [seqReb  ] -3(?),-3, 0, 0,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    -13(3)   -5(2)   -3(2)   -2(2)   0(3)   3(2)   7(2)   8(2) 
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
					if( all(aCStep[1:2+3]==c(  1, 7 )) ) cnt<-cnt+1
					if( all(aCStep[1:2+3]==c( 11, 9 )) ) cnt<-cnt+1

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
			#     11 24 32 33 35 40    |13  8  1  2  5 |                        |0 1 1 3 1 |1 1 3 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  0 -12  -3   0   3   2 |0 2 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  0   4 -11 -14 -14  -3 |0 4 1 1 0 |4 1 1
			#     13 16 24 25 33 36(2) | 3  8  1  8  3 |  2   0   6   6   9  -3 |0 2 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   2   6  14   8   6 |0 2 0 2 2 |2 2 2
			#      6  8 18 35 42 43(2) | 2 10 17  7  1 | -6 -10 -12  -4   1   1 |2 1 0 1 2 |2 1 1 2
			#   dup number  11:3   12:2   16:2   18:3   24:3   33:3   35:2   39:2   42:3
			#   zoid width  ... 29   31   28   23   30   37 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  833        1     13           [desc1   ] 13(?),xx,12,xx,11     col
			#  8331       1     12 [seqReb  ] 12(?), .,12, .,11, .,11,...     col
			#  834        5     43                 [desc1   ] 43(?),42,41     col
			#  8341       6     44                 [desc1   ] 44(?),43,42     col
			#  1          2     39           [same    ] 39(?), .,39, .,39  Slide/
			#  11         4     42                 [same    ] 42(?),42,42  Slide/
			#  12         4     18                 [same    ] 18(?),18,18 Slide\\
			#  13         4     13              [sameEnd ] 13(?),18,18,13 Slide\\
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
			# < 11>   0     11 , 12 , 29 , 33 , 38 , 42 
			#               11 , 16 , 18 , 19 , 24 , 39 
			#          -->  11*, 20 , NA , NA , NA , 36 
			# < 12>  -1     12 , 29 , 33 , 38 , 42 
			#               12 , 18 , 30 , 39 , 41 
			#          -->  12*, NA , 27 , 40!, 40!
			# < 16>   0     11 , 16 , 18 , 19 , 24 , 39 
			#               13 , 16 , 24 , 25 , 33 , 36 
			#          -->  15 , 16*, 30 , 31 , 42 , 33 
			# < 18>   1     12 , 18 , 30 , 39 , 41 
			#                8 , 18 , 35 , 42 , 43 
			#          -->   4 , 18*, 40 , 45 , 45 
			# < 24>  -2     18 , 19 , 24 , 39 
			#               13 , 16 , 24 , 25 
			#          -->   8 , 13 , 24*, NA 
			# < 33>   1     11 , 12 , 29 , 33 , 38 
			#               16 , 24 , 25 , 33 , 36 
			#          -->  21 , NA , 21 , 33*, 34 
			# < 35>  -1     24 , 32 , 33 , 35 , 40 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->  NA , NA ,  3 , 35*, 44 
			# < 39>  -2     18 , 19 , 24 , 39 
			#               12 , 18 , 30 , 39 
			#          -->   6 , 17!, 36 , 39*
			# < 42>  -1     18 , 30 , 39 , 41 , 42 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->  NA , NA , NA , 29 , 42*
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
			# 833        1      3           [desc1   ]  3(?),xx, 2,xx, 1     col
			# 8331       1      2 [seqReb  ]  2(?), ., 2, ., 1, ., 1,...     col
			# 834        2      8                 [same    ]  8(?), 8, 8     col
			# 826        2      6              [sameEnd ]  6(?), 8, 8, 6     col
			# 8332       2     10    [desc(-2) ] 10(?),xx, 8,xx, 6,xx, 4     col
			# 8333       4      9           [same    ]  9(?), ., 9, ., 9     col
			# 733        4      3     [sameEnd ]  3(?),xx, 9,xx, 9,xx, 3     col
			# 781        4      9           [symm    ]  9(?), 5, 9, 5, 9     col
			# 8341       5      3                 [desc1   ]  3(?), 2, 1     col
			# 8342       6      4                 [desc1   ]  4(?), 3, 2     col
			# 1          2      9           [same    ]  9(?), ., 9, ., 9  Slide/
			# 11         2      7                 [desc1   ]  7(?), 8, 9  Slide/
			# 12         4      2                 [same    ]  2(?), 2, 2  Slide/
			# 13         4      8                 [same    ]  8(?), 8, 8 Slide\\
			# 14         4      3              [sameEnd ]  3(?), 8, 8, 3 Slide\\
			# 15         5     -1           [desc1   ] -1(?),xx, 0,xx, 1 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      833        1      7           [desc1   ]  7(?),xx, 6,xx, 5     col
			#      826        2      8     [same    ]  8(?), ., ., 8, ., ., 8     col
			#      8261       3      1     [same    ]  1(?), ., ., 1, ., ., 1     col
			#      834        3     25             [desc(-8) ] 25(?),17, 9, 1     col
			#      8331       3      9 [seqReb  ]  9(?), ., 9, ., 1, ., 1,...     col
			#      8341       5      1                 [same    ]  1(?), 1, 1     col
			#      8262       5      3              [sameEnd ]  3(?), 1, 1, 3     col
			#      1          1     11              [desc1   ] 11(?),10, 9, 8  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(4)   3(2)   4(2)   5(4)   8(3)   17(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 24 32 33 35 40    |13  8  1  2  5 |                        |0 1 1 3 1 |1 1 3 1
			#     11 12 29 33 38 42(2) | 1 17  4  5  4 |  0 -12  -3   0   3   2 |0 2 1 2 1 |2 1 2 1
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  0   4 -11 -14 -14  -3 |0 4 1 1 0 |4 1 1
			#     13 16 24 25 33 36(2) | 3  8  1  8  3 |  2   0   6   6   9  -3 |0 2 2 2 0 |2 2 2
			#     12 18 30 39 41 42    | 6 12  9  2  1 | -1   2   6  14   8   6 |0 2 0 2 2 |2 2 2
			#      6  8 18 35 42 43(2) | 2 10 17  7  1 | -6 -10 -12  -4   1   1 |2 1 0 1 2 |2 1 1 2

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
			#      1        1     -2    [desc1   ] -2(?),xx,-1,xx, 0     col
			#      2        3    -12 [seqReb  ] -12(?),-12, 6, 6,...     col
			#      E2       1    -10 [seqReb  ] -10(?),-10, 6, 6,...  Slide/
			#      E3       4    -12 [seqReb  ] -12(?),-12, 2, 2,... Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -14(2)   -12(2)   -3(3)   0(4)   1(2)   2(3)   6(4)
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  3   3   7  -9  -8  -8 |1 2 1 2 0 |1 2 1 2
			#     17 25 28 37 43 44    | 8  3  9  6  1 |  9  13   9  16  12   9 |0 1 2 1 2 |1 2 1 2
			#     14 15 25 28 29 30(2) | 1 10  3  1  1 | -3 -10  -3  -9 -14 -14 |0 2 3 1 0 |2 3 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -7  -6  -1   1   5   8 |2 0 2 2 0 |2 2 2
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -6   7   5   4   6   7 |1 1 1 1 2 |1 1 1 1 2
			#   dup number  9:2   12:2   25:2   28:2   29:3   30:2   43:2
			#   zoid width  ... 38   27   27   16   31   44 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#   tgt.col banVal                      descript tgt.dir
			#  1       2     29     [same    ] 29(?),29,29,29  Slide/
			#  2       2     44  [sameEnd ] 44(?),29,29,29,44  Slide/
			#  3       2     29 [seqReb  ] 29(?),29,29,29,...  Slide/
			#  4       3     32        [desc1   ] 32(?),33,34  Slide/
			#  5       5     42    [desc(-9) ] 42(?),33,24,15 Slide\\

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
			# <  9>   0      5 ,  9 , 12 , 30 , 39 , 43 
			#                7 ,  9 , 24 , 29 , 34 , 38 
			#          -->  NA ,  9*, 36 , 28!, 29 , 33 
			# < 12>  -1      9 , 12 , 30 , 39 , 43 
			#                8 , 12 , 19 , 21 , 31 
			#          -->   7!, 12*, NA , NA , 19 
			# < 25>   1     17 , 25 , 28 , 37 , 43 
			#               15 , 25 , 28 , 29 , 30 
			#          -->  13 , 25*, 28!, NA , NA 
			# < 28>   1     17 , 25 , 28 , 37 , 43 
			#               15 , 25 , 28 , 29 , 30 
			#          -->  13 , 25!, 28*, NA , NA 
			# < 29>  -1      9 , 24 , 29 , 34 , 38 
			#                1 , 16 , 29 , 33 , 40 
			#          -->  NA ,  8 , 29*, 32!, 42 
			# < 30>   2      5 ,  9 , 12 , 30 
			#               25 , 28 , 29 , 30 
			#          -->  NA , NA , NA , 30*
			# < 43>  -1      9 , 12 , 30 , 39 , 43 
			#               17 , 25 , 28 , 37 , 43 
			#          -->  25 , 38 , 26 , 35 , 43*
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
			# 824        1      7       [same    ]  7(?), ., 7, ., 7     col
			# 758        1      5 [sameEnd ]  5(?),xx, 7,xx, 7,xx, 5     col
			# 818        1      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 8181       5      9 [same    ]  9(?), ., ., 9, ., ., 9     col
			# 8241       5      5       [desc1   ]  5(?),xx, 4,xx, 3     col
			# 1          1      5       [desc1   ]  5(?),xx, 4,xx, 3  Slide/
			# 11         2      9          [same    ]  9(?), 9, 9, 9  Slide/
			# 12         2      4       [sameEnd ]  4(?), 9, 9, 9, 4  Slide/
			# 13         2      9      [seqReb  ]  9(?), 9, 9, 9,...  Slide/
			# 14         3      2             [desc1   ]  2(?), 3, 4  Slide/
			# 15         3      5             [desc1   ]  5(?), 6, 7 Slide\\
			# 16         4      9             [same    ]  9(?), 9, 9 Slide\\
			# 17         4      4          [sameEnd ]  4(?), 9, 9, 4 Slide\\
			# 18         5      2          [desc1   ]  2(?), 3, 4, 5 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      824        2     15 [seqReb  ] 15(?), .,15, ., 3, ., 3,...     col
			#      845        3      3                 [desc1   ]  3(?), 4, 5     col
			#      8241       4      4           [desc1   ]  4(?),xx, 5,xx, 6     col
			#      8451       5      6                 [desc1   ]  6(?), 5, 4     col
			#      1          2      3                 [desc1   ]  3(?), 4, 5  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(2)   3(3)   4(6)   5(3)   7(2)   9(2)   10(2)   15(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5  9 12 30 39 43    | 4  3 18  9  4 |                        |2 1 0 2 1 |2 1 2 1
			#      8 12 19 21 31 35(1) | 4  7  2 10  4 |  3   3   7  -9  -8  -8 |1 2 1 2 0 |1 2 1 2
			#     17 25 28 37 43 44    | 8  3  9  6  1 |  9  13   9  16  12   9 |0 1 2 1 2 |1 2 1 2
			#     14 15 25 28 29 30(2) | 1 10  3  1  1 | -3 -10  -3  -9 -14 -14 |0 2 3 1 0 |2 3 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -7  -6  -1   1   5   8 |2 0 2 2 0 |2 2 2
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -6   7   5   4   6   7 |1 1 1 1 2 |1 1 1 1 2

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
			#      1        1     -5       [desc1   ] -5(?),-6,-7     col
			#      2        5      7       [desc1   ]  7(?), 6, 5     col
			#      3        6      6       [desc1   ]  6(?), 7, 8     col
			#      4        6      7 [desc1   ]  7(?),xx, 8,xx, 9     col
			#      E2       1     15   [desc(-8) ] 15(?), 7,-1,-9  Slide/
			#      E4       3      3       [desc1   ]  3(?), 4, 5  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -14(2)   -9(2)   -8(2)   -6(2)   -3(2)   3(2)   5(2)   7(3)   9(3)
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     26 27 28 42 43 45    | 1  1 14  1  2 |                        |0 0 3 0 3 |3 3
			#      1  2  6 16 20 33    | 1  4 10  4 13 |-25 -25 -22 -26 -23 -12 |3 1 1 1 0 |3 1 1 1
			#      1 23 28 30 34 35(1) |22  5  2  4  1 |  0  21  22  14  14   2 |1 0 2 3 0 |1 2 3
			#     12 24 33 38 40 42    |12  9  5  2  2 | 11   1   5   8   6   7 |0 1 1 2 2 |1 1 2 2
			#      1  5 27 30 34 36    | 4 22  3  4  2 |-11 -19  -6  -8  -6  -6 |2 0 1 3 0 |2 1 3
			#     12 17 23 34 42 45(1) | 5  6 11  8  3 | 11  12  -4   4   8   9 |0 2 1 1 2 |2 1 1 2
			#   dup number  1:3   12:2   23:2   27:2   28:2   30:2   33:2   34:3   42:3   45:2
			#   zoid width  ... 19   32   34   30   35   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  503        1      1           [same    ]  1(?), ., 1, ., 1     col
			#  143        1     26     [sameEnd ] 26(?),xx, 1,xx, 1,xx,26     col
			#  441        1      1           [symm    ]  1(?),12, 1,12, 1     col
			#  5031       3     26           [desc1   ] 26(?),xx,27,xx,28     col
			#  5032       3     27 [seqReb  ] 27(?), .,27, .,28, .,28,...     col
			#  5033       4     30           [same    ] 30(?), .,30, .,30     col
			#  1431       4     42     [sameEnd ] 42(?),xx,30,xx,30,xx,42     col
			#  5034       5     34           [same    ] 34(?), .,34, .,34     col
			#  1432       5     43     [sameEnd ] 43(?),xx,34,xx,34,xx,43     col
			#  5035       6     37           [desc1   ] 37(?),xx,36,xx,35     col
			#  1          3     34                 [same    ] 34(?),34,34  Slide/
			#  11         3     42              [sameEnd ] 42(?),34,34,42  Slide/

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
			# <  1>   0      1 , 23 , 28 , 30 , 34 , 35 
			#                1 ,  5 , 27 , 30 , 34 , 36 
			#          -->   1*, NA , 26!, 30!, 34!, 37!
			# < 12>   0     12 , 24 , 33 , 38 , 40 , 42 
			#               12 , 17 , 23 , 34 , 42 , 45 
			#          -->  12*, NA , 13 , 30 , 44 , NA 
			# < 23>   1      1 , 23 , 28 , 30 , 34 
			#               17 , 23 , 34 , 42 , 45 
			#          -->  NA , 23*, 40 , NA , NA 
			# < 27>   1     26 , 27 , 28 , 42 , 43 
			#                5 , 27 , 30 , 34 , 36 
			#          -->  NA , 27*, 32 , NA , 29 
			# < 28>   0     26 , 27 , 28 , 42 , 43 , 45 
			#                1 , 23 , 28 , 30 , 34 , 35 
			#          -->  NA , 19 , 28*, NA , NA , NA 
			# < 30>   0      1 , 23 , 28 , 30 , 34 , 35 
			#                1 ,  5 , 27 , 30 , 34 , 36 
			#          -->   1!, NA , 26!, 30*, 34!, 37!
			# < 33>  -3     16 , 20 , 33 
			#               12 , 24 , 33 
			#          -->   8 , 28 , 33*
			# < 34>  -1      5 , 27 , 30 , 34 , 36 
			#               12 , 17 , 23 , 34 , 42 
			#          -->  19 ,  7 , 16 , 34*, NA 
			# < 42>  -1     24 , 33 , 38 , 40 , 42 
			#               12 , 17 , 23 , 34 , 42 
			#          -->  NA ,  1 ,  8 , 28 , 42*
			# < 45>   0     26 , 27 , 28 , 42 , 43 , 45 
			#               12 , 17 , 23 , 34 , 42 , 45 
			#          -->  NA ,  7 , 18 , 26 , 41!, 45*
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
			# 503        1      1           [same    ]  1(?), ., 1, ., 1     col
			# 143        1      6     [sameEnd ]  6(?),xx, 1,xx, 1,xx, 6     col
			# 799        1      3                 [desc1   ]  3(?), 2, 1     col
			# 441        1      1           [symm    ]  1(?), 2, 1, 2, 1     col
			# 5031       3      6           [desc1   ]  6(?),xx, 7,xx, 8     col
			# 5032       3      7 [seqReb  ]  7(?), ., 7, ., 8, ., 8,...     col
			# 4411       3      8           [symm    ]  8(?), 3, 7, 3, 8     col
			# 5033       4      0           [same    ]  0(?), ., 0, ., 0     col
			# 1431       4      2     [sameEnd ]  2(?),xx, 0,xx, 0,xx, 2     col
			# 5034       5      4           [same    ]  4(?), ., 4, ., 4     col
			# 1432       5      3     [sameEnd ]  3(?),xx, 4,xx, 4,xx, 3     col
			# 7991       6      4                 [desc1   ]  4(?), 5, 6     col
			# 5035       6      7           [desc1   ]  7(?),xx, 6,xx, 5     col
			# 5036       6      6 [seqReb  ]  6(?), ., 6, ., 5, ., 5,...     col
			# 1          1      7                 [same    ]  7(?), 7, 7  Slide/
			# 11         1      8              [sameEnd ]  8(?), 7, 7, 8  Slide/
			# 12         2      3          [seqReb  ]  3(?), 3, 0, 0,...  Slide/
			# 13         3      4                 [same    ]  4(?), 4, 4  Slide/
			# 14         3      2              [sameEnd ]  2(?), 4, 4, 2  Slide/
			# 15         5      1           [symm    ]  1(?), 4, 7, 4, 1 Slide\\
			cntMtx[idx,"rem"] <- cnt
		}
		if( TRUE ){ # cStep		#	
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      799        1      6             [desc1   ]  6(?), 5, 4     col
			#      503        3      4       [desc1   ]  4(?),xx, 3,xx, 2     col
			#      5031       4      4       [same    ]  4(?), ., 4, ., 4     col
			#      143        4      1 [sameEnd ]  1(?),xx, 4,xx, 4,xx, 1     col
			#      453        4      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      4531       5      2 [same    ]  2(?), ., ., 2, ., ., 2     col
			#      7991       5      4             [desc1   ]  4(?), 3, 2     col
			#      5032       5      3       [desc1   ]  3(?),xx, 2,xx, 1     col
			#      7992       5      3      [seqReb  ]  3(?), 3, 2, 2,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(5)   3(2)   4(5)   5(3)   22(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     26 27 28 42 43 45    | 1  1 14  1  2 |                        |0 0 3 0 3 |3 3
			#      1  2  6 16 20 33    | 1  4 10  4 13 |-25 -25 -22 -26 -23 -12 |3 1 1 1 0 |3 1 1 1
			#      1 23 28 30 34 35(1) |22  5  2  4  1 |  0  21  22  14  14   2 |1 0 2 3 0 |1 2 3
			#     12 24 33 38 40 42    |12  9  5  2  2 | 11   1   5   8   6   7 |0 1 1 2 2 |1 1 2 2
			#      1  5 27 30 34 36    | 4 22  3  4  2 |-11 -19  -6  -8  -6  -6 |2 0 1 3 0 |2 1 3
			#     12 17 23 34 42 45(1) | 5  6 11  8  3 | 11  12  -4   4   8   9 |0 2 1 1 2 |2 1 1 2

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
			#       tgt.col banVal                      descript tgt.dir
			#      1       1      0 [symm    ]  0(?),11,-11,11, 0     col
			# -------------------------------------------------------------------------------------
			#     FV :    -25(2)   -6(3)   8(2)   11(2)   14(2) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2  8 13 25 28 37(3) | 6  5 12  3  9 |  0   0  -2   3   3  -4 |2 1 2 1 0 |2 1 2 1
			#      5 13 17 23 28 36(2) | 8  4  6  5  8 |  3   5   4  -2   0  -1 |1 2 2 1 0 |1 2 2 1
			#      7  8 10 19 21 31    | 1  2  9  2 10 |  2  -5  -7  -4  -7  -5 |2 2 1 1 0 |2 2 1 1
			#     13 16 24 25 33 36    | 3  8  1  8  3 |  6   8  14   6  12   5 |0 2 2 2 0 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  0  -2  -5   1   7   7 |0 3 1 0 2 |3 1 2
			#   dup number  2:2   8:3   13:4   19:2   25:3   28:2   36:2
			#   zoid width  ... 39   35   31   24   23   30 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  832        1     13             [same    ] 13(?),13,13     col
			#  727        1      7          [sameEnd ]  7(?),13,13, 7     col
			#  7271       2      8 [same    ]  8(?), ., ., 8, ., ., 8     col
			#  8321       4     27             [desc1   ] 27(?),26,25     col
			#  826        6     36       [same    ] 36(?), .,36, .,36     col
			#  588        6     41 [sameEnd ] 41(?),xx,36,xx,36,xx,41     col
			#  1          3     15             [desc1   ] 15(?),14,13 Slide\\
			#  11         6     55        [desc(-15) ] 55(?),40,25,10 Slide\\

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
			# <  2>   0      2 ,  8 , 15 , 22 , 25 , 41 
			#                2 ,  8 , 13 , 25 , 28 , 37 
			#          -->   2*,  8!, 11 , 28 , 31 , 33 
			# <  8>   0      2 ,  8 , 13 , 25 , 28 , 37 
			#                7 ,  8 , 10 , 19 , 21 , 31 
			#          -->  NA ,  8*, NA , 13 , 14 , 25 
			# < 13>   0     13 , 16 , 24 , 25 , 33 , 36 
			#               13 , 14 , 19 , 26 , 40 , 43 
			#          -->  13*, NA , 14 , 27!, NA , NA 
			# < 19>  -1      8 , 10 , 19 , 21 , 31 
			#               13 , 14 , 19 , 26 , 40 
			#          -->  18 , 18 , 19*, 31 , NA 
			# < 25>   0      2 ,  8 , 13 , 25 , 28 , 37 
			#               13 , 16 , 24 , 25 , 33 , 36 
			#          -->  24 , 24 , NA , 25*, 38 , 35!
			# < 28>   0      2 ,  8 , 13 , 25 , 28 , 37 
			#                5 , 13 , 17 , 23 , 28 , 36 
			#          -->   8 , 18 , 21 , 21 , 28*, 35!
			# < 36>   0      5 , 13 , 17 , 23 , 28 , 36 
			#               13 , 16 , 24 , 25 , 33 , 36 
			#          -->  21 , 19 , 31 , 27 , NA , 36*
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
			# 832        1      3             [same    ]  3(?), 3, 3     col
			# 727        1      7          [sameEnd ]  7(?), 3, 3, 7     col
			# 7271       2      8 [same    ]  8(?), ., ., 8, ., ., 8     col
			# 8321       2      2         [desc( 2) ]  2(?), 4, 6, 8     col
			# 8322       4      7             [desc1   ]  7(?), 6, 5     col
			# 826        6      6       [same    ]  6(?), ., 6, ., 6     col
			# 588        6      1 [sameEnd ]  1(?),xx, 6,xx, 6,xx, 1     col
			# 7272       6      1 [same    ]  1(?), ., ., 1, ., ., 1     col
			# 1          1      4             [same    ]  4(?), 4, 4  Slide/
			# 11         1      9          [sameEnd ]  9(?), 4, 4, 9  Slide/
			# 12         2      4       [desc1   ]  4(?),xx, 5,xx, 6  Slide/
			# 13         2     13         [desc(-4) ] 13(?), 9, 5, 1  Slide/
			# 14         3      5             [desc1   ]  5(?), 4, 3 Slide\\
			# 15         5      3       [desc1   ]  3(?),xx, 4,xx, 5 Slide\\
			# 16         6      3       [symm    ]  3(?), 0, 5, 0, 3 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      644        1      8       [symm    ]  8(?), 1, 3, 1, 8     col
			#      727        4      1 [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			#      832        4     20         [desc(-6) ] 20(?),14, 8, 2     col
			#      8321       5      3             [same    ]  3(?), 3, 3     col
			#      7271       5     10          [sameEnd ] 10(?), 3, 3,10     col
			#      1          2      6             [desc1   ]  6(?), 7, 8  Slide/
			#      11         4      6             [desc1   ]  6(?), 7, 8 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(2)   3(5)   5(3)   6(3)   7(3)   8(4)   9(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2  8 13 25 28 37(3) | 6  5 12  3  9 |  0   0  -2   3   3  -4 |2 1 2 1 0 |2 1 2 1
			#      5 13 17 23 28 36(2) | 8  4  6  5  8 |  3   5   4  -2   0  -1 |1 2 2 1 0 |1 2 2 1
			#      7  8 10 19 21 31    | 1  2  9  2 10 |  2  -5  -7  -4  -7  -5 |2 2 1 1 0 |2 2 1 1
			#     13 16 24 25 33 36    | 3  8  1  8  3 |  6   8  14   6  12   5 |0 2 2 2 0 |2 2 2
			#     13 14 19 26 40 43(1) | 1  5  7 14  3 |  0  -2  -5   1   7   7 |0 3 1 0 2 |3 1 2

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
			#      E5       6      8       [desc1   ]  8(?), 7, 6 Slide\\
			#      E4       6      7 [desc1   ]  7(?),xx, 6,xx, 5 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -7(2)   -5(3)   -4(2)   -2(3)   0(4)   3(3)   5(2)   6(2)   7(2) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 17 23 24 29 44    | 1  6  1  5 15 |                        |0 2 3 0 1 |2 3 1
			#      6  7 12 28 38 40    | 1  5 16 10  2 |-10 -10 -11   4   9  -4 |2 1 1 1 1 |2 1 1 1 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 | -3   4   2 -13  -6  -4 |1 3 0 2 0 |1 3 2
			#      4  5  8 16 21 29    | 1  3  8  5  8 |  1  -6  -6   1 -11  -7 |3 1 2 0 0 |3 1 2
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7  11  10   3   3  10 |0 4 1 1 0 |4 1 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  1   1   5  15  18   6 |0 2 1 1 2 |2 1 1 2
			#   dup number  11:2   12:2   16:3   17:2   23:2   24:2   29:2
			#   zoid width  ... 28   34   33   25   28   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal               descript tgt.dir
			#  799        1     13 [desc1   ] 13(?),12,11     col
			#  7991       2     18 [desc1   ] 18(?),17,16     col
			#  1          1     16 [desc1   ] 16(?),17,18  Slide/

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
			# < 11>  -1     11 , 14 , 15 , 32 , 36 
			#               11 , 16 , 18 , 19 , 24 
			#          -->  11*, 18 , 21 , NA , 12 
			# < 12>  -2     12 , 28 , 38 , 40 
			#               12 , 17 , 23 , 34 
			#          -->  12*, NA , NA , 28 
			# < 16>  -2      8 , 16 , 21 , 29 
			#               11 , 16 , 18 , 19 
			#          -->  14 , 16*, NA , NA 
			# < 17>   0     16 , 17 , 23 , 24 , 29 , 44 
			#               12 , 17 , 23 , 34 , 42 , 45 
			#          -->   8 , 17*, 23!, 44 , NA , NA 
			# < 23>   0     16 , 17 , 23 , 24 , 29 , 44 
			#               12 , 17 , 23 , 34 , 42 , 45 
			#          -->   8 , 17!, 23*, 44 , NA , NA 
			# < 24>   1     16 , 17 , 23 , 24 , 29 
			#               16 , 18 , 19 , 24 , 39 
			#          -->  16!, 19!, 15 , 24*, NA 
			# < 29>   1     16 , 17 , 23 , 24 , 29 
			#                5 ,  8 , 16 , 21 , 29 
			#          -->  NA , NA ,  9 , 18 , 29*
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
			#      tgt.col banVal                      descript tgt.dir
			# 799        1      3        [desc1   ]  3(?), 2, 1     col
			# 7991       2      8     [desc1   ]  8(?), 7, 6, 5     col
			# 7992       3      3 [seqReb  ]  3(?), 3, 8, 8,...     col
			# 7993       6      5 [seqReb  ]  5(?), 5, 9, 9,...     col
			# 1          1      6        [desc1   ]  6(?), 7, 8  Slide/
			# 11         3      4        [same    ]  4(?), 4, 4  Slide/
			# 12         3      9     [sameEnd ]  9(?), 4, 4, 9  Slide/
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                               descript tgt.dir
			#      799        1      5                 [same    ]  5(?), 5, 5     col
			#      699        1      1              [sameEnd ]  1(?), 5, 5, 1     col
			#      6991       1      1     [same    ]  1(?), ., ., 1, ., ., 1     col
			#      781        2      1           [desc1   ]  1(?),xx, 2,xx, 3     col
			#      7811       3      1     [same    ]  1(?), ., 1, ., 1, ., 1     col
			#      7812       3      1 [seqReb  ]  1(?), ., 1, ., 1, ., 1,...     col
			#      6992       4      5     [same    ]  5(?), ., ., 5, ., ., 5     col
			#      7991       4      8          [seqReb  ]  8(?), 8, 5, 5,...     col
			#      1          3      7                 [desc1   ]  7(?), 6, 5 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(2)   3(3)   5(6)   6(2)   8(4)   15(2) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 17 23 24 29 44    | 1  6  1  5 15 |                        |0 2 3 0 1 |2 3 1
			#      6  7 12 28 38 40    | 1  5 16 10  2 |-10 -10 -11   4   9  -4 |2 1 1 1 1 |2 1 1 1 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 | -3   4   2 -13  -6  -4 |1 3 0 2 0 |1 3 2
			#      4  5  8 16 21 29    | 1  3  8  5  8 |  1  -6  -6   1 -11  -7 |3 1 2 0 0 |3 1 2
			#     11 16 18 19 24 39(1) | 5  2  1  5 15 |  7  11  10   3   3  10 |0 4 1 1 0 |4 1 1
			#     12 17 23 34 42 45    | 5  6 11  8  3 |  1   1   5  15  18   6 |0 2 1 1 2 |2 1 1 2

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
			#      1        1     -3 [symm    ] -3(?), 1, 7, 1,-3     col
			#      E5       1     -6 [symm    ] -6(?), 1,10, 1,-6  Slide/
			#      E4       6      2 [desc1   ]  2(?),xx, 3,xx, 4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -10(2)   -6(3)   -4(2)   1(4)   3(2)   4(2)   10(2) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  5  7 18 20 25    | 1  2 11  2  5 |                        |3 1 2 0 0 |3 1 2
			#      4  8 11 18 37 45(2) | 4  3  7 19  8 |  0   3   4   0  17  20 |2 2 0 1 1 |2 2 1 1
			#      8 24 28 35 38 40(1) |16  4  7  3  2 |  4  16  17  17   1  -5 |1 0 2 2 1 |1 2 2 1
			#      8 10 23 24 35 43(3) | 2 13  1 11  8 |  0 -14  -5 -11  -3   3 |1 1 2 1 1 |1 1 2 1 1
			#      3 11 13 21 33 37    | 8  2  8 12  4 | -5   1 -10  -3  -2  -6 |1 2 1 2 0 |1 2 1 2
			#      3 10 14 16 36 38(1) | 7  4  2 20  2 |  0  -1   1  -5   3   1 |1 3 0 2 0 |1 3 2
			#   dup number  3:2   4:2   8:3   10:2   11:2   18:2   24:2   35:2   37:2   38:2
			#   zoid width  ... 21   41   32   35   34   35 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                     descript tgt.dir
			#  701        1      3       [same    ]  3(?), 3, 3     col
			#  597        1      8    [sameEnd ]  8(?), 3, 3, 8     col
			#  7011       2      9       [desc1   ]  9(?),10,11     col
			#  595        2     24 [symm    ] 24(?),10,11,10,24     col
			#  7012       3     15       [desc1   ] 15(?),14,13     col
			#  7013       6     39       [desc1   ] 39(?),38,37     col
			#  1          4     35       [desc1   ] 35(?),36,37  Slide/
			#  11         4     17   [desc(-3) ] 17(?),14,11, 8 Slide\\
			#  12         5     19   [desc(-3) ] 19(?),16,13,10 Slide\\

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
			# <  3>   0      3 , 11 , 13 , 21 , 33 , 37 
			#                3 , 10 , 14 , 16 , 36 , 38 
			#          -->   3*,  9!, 15!, 11 , 39 , 39!
			# <  4>   0      4 ,  5 ,  7 , 18 , 20 , 25 
			#                4 ,  8 , 11 , 18 , 37 , 45 
			#          -->   4*, 11 , 15 , 18!, NA , NA 
			# <  8>   0      8 , 24 , 28 , 35 , 38 , 40 
			#                8 , 10 , 23 , 24 , 35 , 43 
			#          -->   8*, NA , 18 , 13 , 32 , NA 
			# < 10>   0      8 , 10 , 23 , 24 , 35 , 43 
			#                3 , 10 , 14 , 16 , 36 , 38 
			#          -->  NA , 10*, NA , NA , 37!, 33 
			# < 11>  -1      8 , 11 , 18 , 37 , 45 
			#                3 , 11 , 13 , 21 , 33 
			#          -->  NA , 11*, NA , NA , 21 
			# < 18>   0      4 ,  5 ,  7 , 18 , 20 , 25 
			#                4 ,  8 , 11 , 18 , 37 , 45 
			#          -->   4!, 11 , 15 , 18*, NA , NA 
			# < 24>   2      8 , 24 , 28 , 35 
			#               23 , 24 , 35 , 43 
			#          -->  NA , 24*, 42 , NA 
			# < 35>   1      8 , 24 , 28 , 35 , 38 
			#               10 , 23 , 24 , 35 , 43 
			#          -->  12 , 22!, 20 , 35*, NA 
			# < 37>   1      4 ,  8 , 11 , 18 , 37 
			#               11 , 13 , 21 , 33 , 37 
			#          -->  18 , 18 , 31 , NA , 37*
			# < 38>   1      8 , 24 , 28 , 35 , 38 
			#               10 , 14 , 16 , 36 , 38 
			#          -->  12 ,  4 ,  4 , 37!, 38*
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
			#      tgt.col banVal                      descript tgt.dir
			# 701        1      3        [same    ]  3(?), 3, 3     col
			# 597        1      8     [sameEnd ]  8(?), 3, 3, 8     col
			# 7011       2     -1        [desc1   ] -1(?), 0, 1     col
			# 595        2      4  [symm    ]  4(?), 0, 1, 0, 4     col
			# 7012       3      5        [desc1   ]  5(?), 4, 3     col
			# 7013       3      4 [seqReb  ]  4(?), 4, 3, 3,...     col
			# 7014       6      9        [desc1   ]  9(?), 8, 7     col
			# 1          2      2  [desc1   ]  2(?),xx, 1,xx, 0  Slide/
			# 11         3      6 [seqReb  ]  6(?), 6, 3, 3,...  Slide/
			# 12         4      5        [desc1   ]  5(?), 6, 7  Slide/
			# 13         5      9    [desc(-3) ]  9(?), 6, 3, 0 Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                           descript tgt.dir
			#      701       1      6             [desc1   ]  6(?), 7, 8     col
			#      597       1      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      698       3      9       [desc1   ]  9(?),xx, 8,xx, 7     col
			#      1         4      2          [same    ]  2(?), 2, 2, 2 Slide\\
			#      11        4      2      [seqReb  ]  2(?), 2, 2, 2,... Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(7)   3(2)   4(4)   7(3)   8(4)   11(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      4  5  7 18 20 25    | 1  2 11  2  5 |                        |3 1 2 0 0 |3 1 2
			#      4  8 11 18 37 45(2) | 4  3  7 19  8 |  0   3   4   0  17  20 |2 2 0 1 1 |2 2 1 1
			#      8 24 28 35 38 40(1) |16  4  7  3  2 |  4  16  17  17   1  -5 |1 0 2 2 1 |1 2 2 1
			#      8 10 23 24 35 43(3) | 2 13  1 11  8 |  0 -14  -5 -11  -3   3 |1 1 2 1 1 |1 1 2 1 1
			#      3 11 13 21 33 37    | 8  2  8 12  4 | -5   1 -10  -3  -2  -6 |1 2 1 2 0 |1 2 1 2
			#      3 10 14 16 36 38(1) | 7  4  2 20  2 |  0  -1   1  -5   3   1 |1 3 0 2 0 |1 3 2

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
			#         tgt.col banVal                      descript tgt.dir
			#      1         1      4  [symm    ]  4(?), 0,-5, 0, 4     col
			#      2         6     -7  [desc1   ] -7(?),xx,-6,xx,-5     col
			#      E3        2      1 [seqReb  ]  1(?), 1,-3,-3,...  Slide/
			#      E31       4      1        [same    ]  1(?), 1, 1 Slide\\
			#      E1        4      0     [sameEnd ]  0(?), 1, 1, 0 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -5(4)   -3(2)   0(4)   1(4)   3(3)   4(2)   17(3) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  4   0  12  16   9  -5 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
			#   dup number  6:2   9:2   12:3   13:2   28:2   29:2   38:2   42:2   43:2
			#   zoid width  ... 31   23   44   40   31   37 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  834        1      5             [desc1   ]  5(?), 6, 7     col
			#  8341       2      7             [desc1   ]  7(?), 8, 9     col
			#  8342       2      8      [seqReb  ]  8(?), 8, 9, 9,...     col
			#  817        6     44 [desc1   ] 44(?),xx,xx,43,xx,xx,42     col
			#  800        6     45       [symm    ] 45(?),43,38,43,45     col
			#  1          3     36             [desc1   ] 36(?),35,34  Slide/
			#  11         3      9             [desc1   ]  9(?), 8, 7 Slide\\

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
			# <  6>  -1      6 , 13 , 16 , 27 , 28 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->   6*, NA , 20 , 43 , NA 
			# <  9>   0      3 ,  9 , 12 , 13 , 25 , 43 
			#                7 ,  9 , 24 , 29 , 34 , 38 
			#          -->  NA ,  9*, 36 , 45 , 43 , 33 
			# < 12>  -1      4 , 10 , 12 , 28 , 45 
			#                3 ,  9 , 12 , 13 , 25 
			#          -->   2!,  8!, 12*, NA , NA 
			# < 13>   1      5 ,  6 , 13 , 16 , 27 
			#                9 , 12 , 13 , 25 , 43 
			#          -->  NA , NA , 13*, 34 , NA 
			# < 28>  -1      6 , 13 , 16 , 27 , 28 
			#                1 ,  4 , 10 , 12 , 28 
			#          -->  NA , NA ,  4 , NA , 28*
			# < 29>   1     11 , 12 , 29 , 33 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->   7 , NA , 29*, 35!, 38!
			# < 38>   1     11 , 12 , 29 , 33 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->   7 , 36 , 29!, 35!, 38*
			# < 42>  -1     12 , 29 , 33 , 38 , 42 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->  NA , NA ,  3 , 32 , 42*
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
			# 824        1      7 [seqReb  ]  7(?), ., 7, ., 1, ., 1,...     col
			# 8341       2      7                 [desc1   ]  7(?), 8, 9     col
			# 8342       2      8          [seqReb  ]  8(?), 8, 9, 9,...     col
			# 817        4      3     [same    ]  3(?), ., ., 3, ., ., 3     col
			# 8241       5      4 [seqReb  ]  4(?), ., 4, ., 8, ., 8,...     col
			# 8171       6      4     [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			# 8242       6     11    [desc(-3) ] 11(?),xx, 8,xx, 5,xx, 2     col
			# 800        6      5           [symm    ]  5(?), 3, 8, 3, 5     col
			# 1          2      7                 [desc1   ]  7(?), 8, 9  Slide/
			# 11         3      6              [desc1   ]  6(?), 5, 4, 3  Slide/
			# 12         3      9                 [desc1   ]  9(?), 8, 7 Slide\\
			# 13         4      7                 [desc1   ]  7(?), 8, 9 Slide\\
			# 14         5      6                 [desc1   ]  6(?), 5, 4 Slide\\
			# 15         6      4           [symm    ]  4(?), 2, 9, 2, 4 Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                     descript tgt.dir
			#      834       1      2       [same    ]  2(?), 2, 2     col
			#      817       1      6    [sameEnd ]  6(?), 2, 2, 6     col
			#      824       1      1 [desc1   ]  1(?),xx, 2,xx, 3     col
			#      1         5      9   [desc(-2) ]  9(?), 7, 5, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(3)   3(3)   4(3)   5(3)   6(2)   7(2)   17(3) 

			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      5  6 13 16 27 28    | 1  7  3 11  1 | -6  -6 -16 -17 -11 -14 |2 2 2 0 0 |2 2 2
			#      1  4 10 12 28 45(1) | 3  6  2 16 17 | -4  -2  -3  -4   1  17 |2 2 1 0 1 |2 2 1 1
			#      3  9 12 13 25 43(1) | 6  3  1 12 18 |  2   5   2   1  -3  -2 |2 2 1 0 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  4   0  12  16   9  -5 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2

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
			#      1        2     -2       [desc1   ] -2(?),-1, 0     col
			#      2        5      7       [desc1   ]  7(?), 8, 9     col
			#      E4       2     15 [desc1   ] 15(?),xx,16,xx,17  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -6(3)   -4(2)   -3(2)   -2(2)   -1(2)   1(2)   2(2)   5(2) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     13 19 28 37 38 43    | 6  9  9  1  5 |                        |0 2 1 2 1 |2 1 2 1
			#      5 10 13 27 37 41(2) | 5  3 14 10  4 | -8  -9 -15 -10  -1  -2 |1 2 1 1 1 |1 2 1 1 1
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -3   7   6  -3   0   0 |1 2 1 1 1 |1 2 1 1 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  4   1  12  10   1   4 |1 1 0 3 1 |1 1 3 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -1  -7 -19  -5  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  0   7  18  12  10   1 |1 1 0 1 3 |1 1 1 3
			#   dup number  5:3   13:2   18:2   19:2   37:3   38:2   41:3   43:2   45:2
			#   zoid width  ... 30   36   39   39   39   40 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  846        1      5             [same    ]  5(?), 5, 5     col
			#  782        1      6          [sameEnd ]  6(?), 5, 5, 6     col
			#  7821       2     17 [desc1   ] 17(?),xx,xx,18,xx,xx,19     col
			#  753        2     17       [symm    ] 17(?),18,11,18,17     col
			#  7822       5     38 [same    ] 38(?), ., .,38, ., .,38     col
			#  8461       6     46             [desc1   ] 46(?),45,44     col
			#  7531       6     41       [symm    ] 41(?),45,44,45,41     col
			#  1          2     31             [desc1   ] 31(?),30,29  Slide/
			#  11         4     42             [desc1   ] 42(?),43,44  Slide/

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
			# <  5>   0      5 , 11 , 12 , 29 , 33 , 44 
			#                5 , 18 , 30 , 41 , 43 , 45 
			#          -->   5*, 25 , NA , NA , NA , NA 
			# < 13>   2     13 , 19 , 28 , 37 
			#               13 , 27 , 37 , 41 
			#          -->  13*, 35 , NA , 45 
			# < 18>   0      6 , 18 , 31 , 34 , 38 , 45 
			#                5 , 18 , 30 , 41 , 43 , 45 
			#          -->   4!, 18*, 29!, NA , NA , 45!
			# < 19>   1     13 , 19 , 28 , 37 , 38 
			#               17 , 19 , 24 , 37 , 41 
			#          -->  NA , 19*, 20 , 37!, 44 
			# < 37>   0      5 , 10 , 13 , 27 , 37 , 41 
			#                2 , 17 , 19 , 24 , 37 , 41 
			#          -->  NA , 24 , 25 , 21 , 37*, 41!
			# < 38>   0     13 , 19 , 28 , 37 , 38 , 43 
			#                6 , 18 , 31 , 34 , 38 , 45 
			#          -->  NA , 17!, 34 , 31 , 38*, NA 
			# < 41>  -2     19 , 24 , 37 , 41 
			#                5 , 18 , 30 , 41 
			#          -->  NA , 12 , 23 , 41*
			# < 43>  -1     19 , 28 , 37 , 38 , 43 
			#                5 , 18 , 30 , 41 , 43 
			#          -->  NA ,  8 , 23 , NA , 43*
			# < 45>   0      6 , 18 , 31 , 34 , 38 , 45 
			#                5 , 18 , 30 , 41 , 43 , 45 
			#          -->   4!, 18!, 29!, NA , NA , 45*

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
			# 846        1      5             [same    ]  5(?), 5, 5     col
			# 782        1      6          [sameEnd ]  6(?), 5, 5, 6     col
			# 7821       2      7 [desc1   ]  7(?),xx,xx, 8,xx,xx, 9     col
			# 753        2      7       [symm    ]  7(?), 8, 1, 8, 7     col
			# 8461       5      3             [same    ]  3(?), 3, 3     col
			# 7822       5      8          [sameEnd ]  8(?), 3, 3, 8     col
			# 7823       5      8 [same    ]  8(?), ., ., 8, ., ., 8     col
			# 8462       6      6             [desc1   ]  6(?), 5, 4     col
			# 7531       6      1       [symm    ]  1(?), 5, 4, 5, 1     col
			# 1          3     -1         [desc( 2) ] -1(?), 1, 3, 5  Slide/
			# 11         4      2             [desc1   ]  2(?), 3, 4  Slide/
			# 12         4     -1             [desc1   ] -1(?), 0, 1 Slide\\
			# 13         5      2       [same    ]  2(?), ., 2, ., 2 Slide\\
			# 14         5      0             [desc1   ]  0(?), 1, 2 Slide\\

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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                      descript tgt.dir
			#      846       4      2 [seqReb  ]  2(?), 2, 4, 4,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(3)   3(2)   4(4)   5(3)   6(2)   9(2)   11(2)   12(2)   13(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     13 19 28 37 38 43    | 6  9  9  1  5 |                        |0 2 1 2 1 |2 1 2 1
			#      5 10 13 27 37 41(2) | 5  3 14 10  4 | -8  -9 -15 -10  -1  -2 |1 2 1 1 1 |1 2 1 1 1
			#      2 17 19 24 37 41(2) |15  2  5 13  4 | -3   7   6  -3   0   0 |1 2 1 1 1 |1 2 1 1 1
			#      6 18 31 34 38 45    |12 13  3  4  7 |  4   1  12  10   1   4 |1 1 0 3 1 |1 1 3 1
			#      5 11 12 29 33 44    | 6  1 17  4 11 | -1  -7 -19  -5  -5  -1 |1 2 1 1 1 |1 2 1 1 1
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  0   7  18  12  10   1 |1 1 0 1 3 |1 1 1 3

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
			#       tgt.col banVal                     descript tgt.dir
			#      1       1      1       [desc1   ]  1(?), 0,-1     col
			#      2       6     -2 [desc1   ] -2(?),xx,-1,xx, 0     col
			# -------------------------------------------------------------------------------------
			#     FV :    -5(2)   -3(2)   -1(3)   0(3)   1(3)   4(2)   7(2)   10(2)   12(2) 
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 21 26 31 36 43    | 5  5  5  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 | -9  16  12   8   4   1 |1 0 0 3 2 |1 3 2
			#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  0 -28 -28 -26  -9  -9 |2 2 0 2 0 |2 2 2
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
			#     10 14 16 18 27 28(2) | 4  2  2  9  1 |  9   7   0   0  -7 -10 |0 4 2 0 0 |4 2
			#      5 22 31 32 39 45    |17  9  1  7  6 | -5   8  15  14  12  17 |1 0 1 3 1 |1 1 3 1
			#   dup number  7:3   10:2   16:3   18:2   31:3   38:2   39:2
			#   zoid width  ... 27   37   28   37   18   40 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                      descript tgt.dir
			#  797        3     31 [seqReb  ] 31(?),31,16,16,...     col
			#  7971       4     32 [seqReb  ] 32(?),32,18,18,...     col

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
			# <  7>   1      7 ,  9 , 10 , 13 , 31 
			#                7 , 16 , 18 , 34 , 38 
			#          -->   7*, 23 , 26 , NA , 45 
			# < 10>  -2     10 , 13 , 31 , 35 
			#               10 , 14 , 16 , 18 
			#          -->  10*, 15!, NA , NA 
			# < 16>   0      1 ,  7 , 16 , 18 , 34 , 38 
			#               10 , 14 , 16 , 18 , 27 , 28 
			#          -->  NA , NA , 16*, 18!, 20 , 18 
			# < 18>   0      1 ,  7 , 16 , 18 , 34 , 38 
			#               10 , 14 , 16 , 18 , 27 , 28 
			#          -->  NA , NA , 16!, 18*, 20 , NA 
			# < 31>  -2     10 , 13 , 31 , 35 
			#                5 , 22 , 31 , 32 
			#          -->  NA , NA , 31*, NA 
			# < 38>   3      7 , 37 , 38 
			#               18 , 34 , 38 
			#          -->  29 , 31 , 38*
			# < 39>   1      7 , 37 , 38 , 39 , 40 
			#               22 , 31 , 32 , 39 , 45 
			#          -->  37 , 25 , 26 , 39*, NA 
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
			# 696        3      6 [same    ]  6(?), ., ., 6, ., ., 6     col
			# 797        3      1      [seqReb  ]  1(?), 1, 6, 6,...     col
			# 7971       4      2      [seqReb  ]  2(?), 2, 8, 8,...     col
			# 7972       6      5      [seqReb  ]  5(?), 5, 8, 8,...     col
			# 655        6      4    [symm    ]  4(?), 5, 8, 8, 5, 4     col
			# 1          4     10             [desc1   ] 10(?), 9, 8  Slide/
			# 11         5      5       [desc1   ]  5(?),xx, 6,xx, 7 Slide\\
			# 12         6     10             [desc1   ] 10(?), 9, 8 Slide\\
			# 13         6      7       [desc1   ]  7(?),xx, 8,xx, 9 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      696        1      7 [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			#      756        2      3       [desc1   ]  3(?),xx, 2,xx, 1     col
			#      671        2      1       [symm    ]  1(?), 9, 2, 9, 1     col
			#      7561       3      1       [desc1   ]  1(?),xx, 2,xx, 3     col
			#      7971       3      1      [seqReb  ]  1(?), 1, 2, 2,...     col
			#      11         5      2       [same    ]  2(?), ., 2, ., 2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   2(4)   4(4)   5(4)   6(2)   7(2)   9(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#     16 21 26 31 36 43    | 5  5  5  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      7 37 38 39 40 44    |30  1  1  1  4 | -9  16  12   8   4   1 |1 0 0 3 2 |1 3 2
			#      7  9 10 13 31 35(1) | 2  1  3 18  4 |  0 -28 -28 -26  -9  -9 |2 2 0 2 0 |2 2 2
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -6  -2   6   5   3   3 |2 2 0 2 0 |2 2 2
			#     10 14 16 18 27 28(2) | 4  2  2  9  1 |  9   7   0   0  -7 -10 |0 4 2 0 0 |4 2
			#      5 22 31 32 39 45    |17  9  1  7  6 | -5   8  15  14  12  17 |1 0 1 3 1 |1 1 3 1

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
			#        tgt.col banVal                       descript tgt.dir
			#      1        2      9         [desc1   ]  9(?), 8, 7     col
			#      2        6    -11 [desc1   ] -11(?),xx,-10,xx,-9     col
			#      E2       3      7         [desc1   ]  7(?), 8, 9 Slide\\
			#      E3       5      0   [same    ]  0(?), ., 0, ., 0 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -28(2)   -9(3)   0(3)   3(2)   8(2)   12(2)
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 15 20 25 33 43    | 8  5  5  8 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#     10 14 16 18 27 28    | 4  2  2  9  1 |  3  -1  -4  -7  -6 -15 |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 |  4   2   3  16   7  10 |1 2 0 3 0 |1 2 3
			#   dup number  11:3   14:3   16:2   25:2   28:2   33:4   38:3
			#   zoid width  ... 36   18   31   29   27   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                            descript tgt.dir
			#  836       2      6 [desc( 3) ]  6(?),xx, 9,xx,12,xx,15     col
			#  1         1     11              [same    ] 11(?),11,11  Slide/
			#  11        1     38           [sameEnd ] 38(?),11,11,38  Slide/
			#  12        2     14              [same    ] 14(?),14,14  Slide/
			#  13        2     40           [sameEnd ] 40(?),14,14,40  Slide/
			#  14        5     11        [same    ] 11(?), .,11, .,11 Slide\\
			#  15        6     12        [symm    ] 12(?),33,14,33,12 Slide\\

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
			# < 11>  -1      9 , 11 , 14 , 26 , 28 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   1 , 11*, 14!, 34 , 38 
			# < 14>  -1      9 , 11 , 14 , 26 , 28 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   1 , 11!, 14*, 34 , 38 
			# < 16>  -2     16 , 18 , 27 , 28 
			#               16 , 25 , 33 , 38 
			#          -->  16*, 32 , 39 , NA 
			# < 25>  -2     20 , 25 , 33 , 43 
			#               16 , 25 , 33 , 38 
			#          -->  12 , 25*, 33!, 33 
			# < 28>   0     10 , 14 , 16 , 18 , 27 , 28 
			#                1 ,  9 , 11 , 14 , 26 , 28 
			#          -->  NA ,  4 ,  6 , 10 , 25!, 28*
			# < 33>   2     16 , 25 , 33 , 38 
			#               14 , 30 , 33 , 38 
			#          -->  12 , NA , 33*, 38!
			# < 38>   2     16 , 25 , 33 , 38 
			#               14 , 30 , 33 , 38 
			#          -->  12 , 35 , 33!, 38*
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
			# 836        1      1       [same    ]  1(?), ., 1, ., 1     col
			# 694        1      7 [sameEnd ]  7(?),xx, 1,xx, 1,xx, 7     col
			# 819        1      5 [desc1   ]  5(?),xx,xx, 6,xx,xx, 7     col
			# 8191       2      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			# 8361       4      5       [desc1   ]  5(?),xx, 4,xx, 3     col
			# 841        4     -4         [desc( 4) ] -4(?), 0, 4, 8     col
			# 8411       6      8             [same    ]  8(?), 8, 8     col
			# 8192       6      5          [sameEnd ]  5(?), 8, 8, 5     col
			# 1          1      1             [same    ]  1(?), 1, 1  Slide/
			# 11         1      8          [sameEnd ]  8(?), 1, 1, 8  Slide/
			# 12         2      4             [same    ]  4(?), 4, 4  Slide/
			# 13         2      0          [sameEnd ]  0(?), 4, 4, 0  Slide/
			# 14         3      1             [same    ]  1(?), 1, 1 Slide\\
			# 15         5      1       [same    ]  1(?), ., 1, ., 1 Slide\\
			# 16         5     -1             [desc1   ] -1(?), 0, 1 Slide\\
			# 17         6      2             [desc1   ]  2(?), 3, 4 Slide\\
			# 18         6      2       [symm    ]  2(?), 3, 4, 3, 2 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      819        1     10 [desc1   ] 10(?),xx,xx, 9,xx,xx, 8     col
			#      841        2      4             [desc1   ]  4(?), 3, 2     col
			#      8191       3      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			#      836        3      2 [desc1   ]  2(?),xx, 3,xx, 4,xx, 5     col
			#      775        5      4       [symm    ]  4(?), 5, 2, 5, 4     col
			#      1          1      3             [same    ]  3(?), 3, 3  Slide/
			#      11         1      2          [sameEnd ]  2(?), 3, 3, 2  Slide/
			#      12         1      2       [desc1   ]  2(?),xx, 3,xx, 4  Slide/
			#      13         3      4             [desc1   ]  4(?), 3, 2  Slide/
			#      14         5      3             [same    ]  3(?), 3, 3 Slide\\
			#      15         5      8          [sameEnd ]  8(?), 3, 3, 8 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(5)   3(3)   4(3)   5(6)   8(4)   9(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 15 20 25 33 43    | 8  5  5  8 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#     10 14 16 18 27 28    | 4  2  2  9  1 |  3  -1  -4  -7  -6 -15 |0 4 2 0 0 |4 2
			#     11 12 29 33 38 42    | 1 17  4  5  4 |  1  -2  13  15  11  14 |0 2 1 2 1 |2 1 2 1
			#     16 25 33 38 40 45(2) | 9  8  5  2  5 |  5  13   4   5   2   3 |0 1 1 2 2 |1 1 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 |-15 -16 -22 -24 -14 -17 |2 2 2 0 0 |2 2 2
			#      5 11 14 30 33 38(2) | 6  3 16  3  5 |  4   2   3  16   7  10 |1 2 0 3 0 |1 2 3

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
			#     FV :    -15(2)   2(2)   3(3)   4(2)   5(2)   13(2) 
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
			#      2 19 25 26 27 43    |17  6  1  1 16 |                        |1 1 3 0 1 |1 1 3 1
			#      3 10 14 16 36 38    | 7  4  2 20  2 |  1  -9 -11 -10   9  -5 |1 3 0 2 0 |1 3 2
			#     10 14 16 18 27 28(3) | 4  2  2  9  1 |  7   4   2   2  -9 -10 |0 4 2 0 0 |4 2
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -8  -4  -4  13   6  14 |1 2 0 2 1 |1 2 2 1
			#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   1  -5   1  -4 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -2   0   0   0  -2  -2 |1 2 1 2 0 |1 2 1 2
			#   dup number  2:2   3:2   10:5   13:2   14:2   16:2   26:3   27:2   36:2   38:2
			#   zoid width  ... 41   35   18   40   35   35 and ?
			#        Quo10 pattern rebind table 
			#        none:83.3%(5/6)   match:16.7%(1/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  791        1      2     [same    ]  2(?), ., ., 2, ., ., 2     col
			#  804        2     10              [same    ] 10(?),10,10,10     col
			#  756        2     14           [sameEnd ] 14(?),10,10,10,14     col
			#  8041       2     10          [seqReb  ] 10(?),10,10,10,...     col
			#  8042       3     13                 [same    ] 13(?),13,13     col
			#  7911       3     12              [sameEnd ] 12(?),13,13,12     col
			#  8043       4     26                 [same    ] 26(?),26,26     col
			#  7912       4     31              [sameEnd ] 31(?),26,26,31     col
			#  795        5     34 [seqReb  ] 34(?), .,34, .,27, .,27,...     col
			#  7913       6     41     [desc1   ] 41(?),xx,xx,42,xx,xx,43     col
			#  1          3     18             [desc( 8) ] 18(?),26,34,42  Slide/

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
			# <  2>   0      2 , 19 , 25 , 26 , 27 , 43 
			#                2 , 10 , 12 , 31 , 33 , 42 
			#          -->   2*, NA , NA , 36 , 39 , 41!
			# <  3>   0      3 , 10 , 14 , 16 , 36 , 38 
			#                3 , 10 , 13 , 26 , 34 , 38 
			#          -->   3*, 10!, 12!, 36 , 32 , 38!
			# < 10>   0      3 , 10 , 13 , 26 , 34 , 38 
			#                1 , 10 , 13 , 26 , 32 , 36 
			#          -->  NA , 10*, 13!, 26!, 30 , 34 
			# < 13>   0      3 , 10 , 13 , 26 , 34 , 38 
			#                1 , 10 , 13 , 26 , 32 , 36 
			#          -->  NA , 10!, 13*, 26!, 30 , 34 
			# < 14>  -1     10 , 14 , 16 , 36 , 38 
			#               10 , 14 , 16 , 18 , 27 
			#          -->  10!, 14*, 16!, NA , 16 
			# < 16>  -1     10 , 14 , 16 , 36 , 38 
			#               10 , 14 , 16 , 18 , 27 
			#          -->  10!, 14!, 16*, NA , NA 
			# < 26>   0      3 , 10 , 13 , 26 , 34 , 38 
			#                1 , 10 , 13 , 26 , 32 , 36 
			#          -->  NA , 10!, 13!, 26*, 30 , 34 
			# < 27>   0      2 , 19 , 25 , 26 , 27 , 43 
			#               10 , 14 , 16 , 18 , 27 , 28 
			#          -->  18 ,  9 ,  7 , 10 , 27*, NA 
			# < 36>   1      3 , 10 , 14 , 16 , 36 
			#               10 , 13 , 26 , 32 , 36 
			#          -->  17 , 16 , NA , NA , 36*
			# < 38>   0      3 , 10 , 14 , 16 , 36 , 38 
			#                3 , 10 , 13 , 26 , 34 , 38 
			#          -->   3!, 10!, 12!, 36 , 32 , 38*
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
			# 791        1      2     [same    ]  2(?), ., ., 2, ., ., 2     col
			# 804        2      0              [same    ]  0(?), 0, 0, 0     col
			# 756        2      4           [sameEnd ]  4(?), 0, 0, 0, 4     col
			# 8041       2      0          [seqReb  ]  0(?), 0, 0, 0,...     col
			# 8042       3      3                 [same    ]  3(?), 3, 3     col
			# 7911       3      2              [sameEnd ]  2(?), 3, 3, 2     col
			# 8043       4      6                 [same    ]  6(?), 6, 6     col
			# 7912       4      1              [sameEnd ]  1(?), 6, 6, 1     col
			# 795        5      4 [seqReb  ]  4(?), ., 4, ., 7, ., 7,...     col
			# 7951       6      8           [same    ]  8(?), ., 8, ., 8     col
			# 603        6      3     [sameEnd ]  3(?),xx, 8,xx, 8,xx, 3     col
			# 7913       6      1     [desc1   ]  1(?),xx,xx, 2,xx,xx, 3     col
			# 1          2      8           [symm    ]  8(?), 3, 6, 3, 8  Slide/
			# 11         3      8             [desc(-2) ]  8(?), 6, 4, 2  Slide/
			# 12         5      9             [desc(-3) ]  9(?), 6, 3, 0 Slide\\
			# 13         6      4           [symm    ]  4(?), 2, 6, 2, 4 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      804        2      3             [same    ]  3(?), 3, 3     col
			#      791        2      2          [sameEnd ]  2(?), 3, 3, 2     col
			#      795        2      4       [desc1   ]  4(?),xx, 3,xx, 2     col
			#      8041       3     13             [same    ] 13(?),13,13     col
			#      7911       3     19          [sameEnd ] 19(?),13,13,19     col
			#      7951       4      7       [desc1   ]  7(?),xx, 8,xx, 9     col
			#      7912       4      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      8042       5      4             [same    ]  4(?), 4, 4     col
			#      7913       5      9          [sameEnd ]  9(?), 4, 4, 9     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(6)   3(2)   4(4)   6(2)   7(2)   8(2)   9(3)   13(2) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2 19 25 26 27 43    |17  6  1  1 16 |                        |1 1 3 0 1 |1 1 3 1
			#      3 10 14 16 36 38    | 7  4  2 20  2 |  1  -9 -11 -10   9  -5 |1 3 0 2 0 |1 3 2
			#     10 14 16 18 27 28(3) | 4  2  2  9  1 |  7   4   2   2  -9 -10 |0 4 2 0 0 |4 2
			#      2 10 12 31 33 42(1) | 8  2 19  2  9 | -8  -4  -4  13   6  14 |1 2 0 2 1 |1 2 2 1
			#      3 10 13 26 34 38(1) | 7  3 13  8  4 |  1   0   1  -5   1  -4 |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -2   0   0   0  -2  -2 |1 2 1 2 0 |1 2 1 2

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
			#      1         2      0       [same    ]  0(?), 0, 0     col
			#      2         2     -4    [sameEnd ] -4(?), 0, 0,-4     col
			#      3         3     -1       [desc1   ] -1(?), 0, 1     col
			#      4         3      0 [desc1   ]  0(?),xx, 1,xx, 2     col
			#      E2        1     -1       [desc1   ] -1(?), 0, 1  Slide/
			#      E4        3     -1       [desc1   ] -1(?), 0, 1  Slide/
			#      E21       3     -1       [desc1   ] -1(?), 0, 1 Slide\\
			#      E3        4      0       [same    ]  0(?), 0, 0 Slide\\
			#      E1        4     -8    [sameEnd ] -8(?), 0, 0,-8 Slide\\
			#      E41       5     -1       [desc1   ] -1(?), 0, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -9(2)   -5(2)   -4(3)   -2(3)   0(4)   1(4)   2(2)
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

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  7 19 25 29 36    | 5 12  6  4  7 |                        |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 13  13  12  -2  -8  -1 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#   dup number  7:2   8:2   13:2   19:2   29:2   30:2   33:2   42:2   43:2
			#   zoid width  ... 34   38   43   37   23   38 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  834        2      9     [desc1   ]  9(?),xx,xx, 8,xx,xx, 7     col
			#  821        2     12           [symm    ] 12(?), 8,21, 8,12     col
			#  8341       3     17     [desc1   ] 17(?),xx,xx,18,xx,xx,19     col
			#  844        5     32                 [desc1   ] 32(?),33,34     col
			#  843        5     34 [seqReb  ] 34(?), .,34, .,29, .,29,...     col
			#  1          1     31           [desc1   ] 31(?),xx,30,xx,29  Slide/
			#  11         6     33                 [same    ] 33(?),33,33 Slide\\
			#  12         6     18              [sameEnd ] 18(?),33,33,18 Slide\\

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
			# <  7>  -1      7 , 19 , 25 , 29 , 36 
			#                7 ,  8 , 13 , 15 , 33 
			#          -->   7*, NA , NA , NA , 30 
			# <  8>   0      6 ,  8 , 18 , 35 , 42 , 43 
			#                7 ,  8 , 13 , 15 , 33 , 45 
			#          -->  NA ,  8*, NA , NA , 24 , NA 
			# < 13>   0      1 , 12 , 13 , 24 , 29 , 44 
			#                7 ,  8 , 13 , 15 , 33 , 45 
			#          -->  NA ,  4 , 13*, NA , 37 , NA 
			# < 19>  -2     19 , 25 , 29 , 36 
			#               19 , 21 , 30 , 33 
			#          -->  19*, NA , 31!, 30 
			# < 29>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#                1 , 12 , 13 , 24 , 29 , 44 
			#          -->  NA , 17 ,  7 , 23!, 29*, NA 
			# < 30>  -2     14 , 26 , 30 , 43 
			#               19 , 21 , 30 , 33 
			#          -->  24 , 16 , 30*, NA 
			# < 33>   1     19 , 21 , 30 , 33 , 34 
			#                8 , 13 , 15 , 33 , 45 
			#          -->  NA ,  5 , NA , 33*, NA 
			# < 42>   1      6 ,  8 , 18 , 35 , 42 
			#               21 , 30 , 33 , 34 , 42 
			#          -->  36 , NA , NA , 33!, 42*
			# < 43>   0      5 ,  9 , 14 , 26 , 30 , 43 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   7!,  7!, 22 , NA , NA , 43*
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
			# 843        2      0           [desc1   ]  0(?),xx, 1,xx, 2     col
			# 834        2      9     [desc1   ]  9(?),xx,xx, 8,xx,xx, 7     col
			# 821        2      2           [symm    ]  2(?), 8, 1, 8, 2     col
			# 8341       3      7     [desc1   ]  7(?),xx,xx, 8,xx,xx, 9     col
			# 8342       4      5     [same    ]  5(?), ., ., 5, ., ., 5     col
			# 8431       4      2     [desc1   ]  2(?),xx, 3,xx, 4,xx, 5     col
			# 8211       4      4           [symm    ]  4(?), 5, 3, 5, 4     col
			# 844        5      2                 [desc1   ]  2(?), 3, 4     col
			# 8432       5      4 [seqReb  ]  4(?), ., 4, ., 9, ., 9,...     col
			# 8433       6      0    [desc( 2) ]  0(?),xx, 2,xx, 4,xx, 6     col
			# 1          2      3                 [same    ]  3(?), 3, 3  Slide/
			# 11         2      2              [sameEnd ]  2(?), 3, 3, 2  Slide/
			# 12         2      2           [desc1   ]  2(?),xx, 3,xx, 4  Slide/
			# 13         3      6              [desc1   ]  6(?), 5, 4, 3  Slide/
			# 14         4      4                 [desc1   ]  4(?), 3, 2  Slide/
			# 15         3      7                 [desc1   ]  7(?), 8, 9 Slide\\
			# 16         5     -1           [desc1   ] -1(?),xx, 0,xx, 1 Slide\\
			# 17         6      3                 [same    ]  3(?), 3, 3 Slide\\
			# 18         6      8              [sameEnd ]  8(?), 3, 3, 8 Slide\\
			# 19         6      4           [desc1   ]  4(?),xx, 3,xx, 2 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                      descript tgt.dir
			#      8441       1      1 [seqReb  ]  1(?), 1, 2, 2,...     col
			#      8442       3      1        [desc1   ]  1(?), 2, 3     col
			#      1          2      3        [desc1   ]  3(?), 2, 1  Slide/
			#      11         2      2 [seqReb  ]  2(?), 2, 1, 1,...  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(3)   4(3)   5(4)   7(2)   11(2)   12(3) 
			cnt.w2 <- 0
			if( 1<sum( aCStep[c(,)]*c(,)==aCStep[c( , )] ) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( aCStep[ ]==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 
			if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"cStep.w1"] <- cnt.w1	;cntMtx[idx,"cStep.w2"] <- cnt.w2
			cntMtx[idx,"cStep"] <- cnt + cnt.w1 + cnt.w2

			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      2  7 19 25 29 36    | 5 12  6  4  7 |                        |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 | 13  13  12  -2  -8  -1 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
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
			#         tgt.col banVal                      descript tgt.dir
			#      1         4     -2  [same    ] -2(?), .,-2, .,-2     col
			#      2         6      3 [seqReb  ]  3(?), 3,-1,-1,...     col
			#      E3        2    -32  [desc(15) ] -32(?),-17,-2,13  Slide/
			#      E5        4     -1        [same    ] -1(?),-1,-1  Slide/
			#      E51       6      0        [desc1   ]  0(?),-1,-2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -4(2)   -2(2)   -1(5)   1(3)   3(3)   5(2)   13(3) 
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
		aBin <- aZoid %%  2
		aRem <- aZoid %% 10

		#	a,b,b.. a?	fCutCnt.nextColVal_3 fCutCnt.nextColVal_4 fCutCnt.nextColVal_5 fCutCnt.nextColVal_6
		if( 2<=sum(aZoid[c(2,3,4,6)]==c( 5,12,30,38)) ){	surviveFlg[idx]<-FALSE	;next }
		#	a,b,b.. b?
		if( 2<=sum(aZoid[c(2,3,4,6)]==c( 8,21,18,45)) ){	surviveFlg[idx]<-FALSE	;next }

		#	a,a,b.. b?
		if( 2<=sum(aZoid[c(3,4,5,6)]==c( 18,41,40,42 )) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<=sum(aZoid[c(3,4,5,6)]==c( 18,41,39,42 )) ){	surviveFlg[idx]<-FALSE	;next }
			# fCutCnt.nextColVal_2
			# fCutCnt.nextColVal_6
			# fCutCnt.nextBin(40) fCutCnt.nextCStepBin(40) fCutCnt.nextColVal_1(39)
			# fCutCnt.nextColVal_6(42)

		# fCutCnt.basic()
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			if( aZoid[6] %in% c(45,42) ){	surviveFlg[idx]<-FALSE	;next }
			if( 3<=sum(aBin[c(1,2,4,6)]==c(1,0,1,1)) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aZoid[ 1 ]*c(6,9)==aZoid[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			#      7 22 24 31 34 36    |15  2  7  3  2 |                        |1 0 2 3 0 |1 2 3
			#      1  9 12 23 39 43    | 8  3 11 16  4 | -6 -13 -12  -8   5   7 |2 1 1 1 1 |2 1 1 1 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 |  1  -2   7   2 -10  -7 |2 1 2 1 0 |2 1 2 1
			#      5 22 31 32 39 45    |17  9  1  7  6 |  3  15  12   7  10   9 |1 0 1 3 1 |1 1 3 1
			#      2 10 14 22 32 36(2) | 8  4  8 10  4 | -3 -12 -17 -10  -7  -9 |1 2 1 2 0 |1 2 1 2
			#     14 15 25 28 29 30(1) | 1 10  3  1  1 | 12   5  11   6  -3  -6 |0 2 3 1 0 |2 3 1
			if( 1<sum( aZoid[c(1,2)]*c(2,2)==aZoid[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( all( c(15,28) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.

		# fCutCnt.nextQuo10
			#      1 13 20 22 25 28    |12  7  2  3  3 |                        |1 1 4 0 0 |1 1 4
			#      4  8 18 19 39 44    | 4 10  1 20  5 |  3  -5  -2  -3  14  16 |2 2 0 1 1 |2 2 1 1
			#      1  7 16 18 34 38(1) | 6  9  2 16  4 | -3  -1  -2  -1  -5  -6 |2 2 0 2 0 |2 2 2
			#      1 28 35 41 43 44(1) |27  7  6  2  1 |  0  21  19  23   9   6 |1 0 1 1 3 |1 1 1 3
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 | 11 -14 -14 -11  -4  -1 |0 2 1 2 1 |2 1 2 1
			#     13 16 24 25 33 36    | 3  8  1  8  3 |  1   2   3  -5  -6  -7 |0 2 2 2 0 |2 2 2
			if( all(aZoid[1:2]==c(14,18)) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(aZoid[c(4,1,6)]==c(13,16,24,25,33,36)[c(3,1,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(aRem[c(1,2)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			#     11 18 21 26 38 43    | 7  3  5 12  5 |                        |0 2 2 1 1 |2 2 1 1
			#      1  8 11 15 18 45(2) | 7  3  4  3 27 |-10 -10 -10 -11 -20   2 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2
			if( all( c(13,14) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( all( c(26,40) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( 1<sum(aZoid[1:2+1]==c(13,18)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			#     13 18 26 31 34 44    | 5  8  5  3 10 |                        |0 2 1 2 1 |2 1 2 1
			#      6  7 10 16 38 41    | 1  3  6 22  3 | -7 -11 -16 -15   4  -3 |2 2 0 1 1 |2 2 1 1
			#      9 10 13 24 33 38(2) | 1  3 11  9  5 |  3   3   3   8  -5  -3 |1 2 1 2 0 |1 2 1 2
			#     15 24 31 32 33 40(2) | 9  7  1  1  7 |  6  14  18   8   0   2 |0 1 1 3 1 |1 1 3 1
			#      1 11 21 23 34 44    |10 10  2 11 10 |-14 -13 -10  -9   1   4 |1 1 2 1 1 |1 1 2 1 1
			#      3 10 16 19 31 39    | 7  6  3 12  8 |  2  -1  -5  -4  -3  -5 |1 3 0 2 0 |1 3 2
			if( all(aBin==c(1, 0, 0, 1, 1, 1)) ){	surviveFlg[idx]<-FALSE	;next }		# 최근에  1  0  1  0  1  0 반복 존재.
			if( all( c(16,31) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( all( c(19,31 ) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.

		# fCutCnt.nextCStepBin
			#     Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl 
			#      7 18 19 27 29 42    |11  1  8  2 13 |                        |1 2 2 0 1 |1 2 2 1
			#      1  8 11 15 18 45(1) | 7  3  4  3 27 | -6 -10  -8 -12 -11   3 |2 3 0 0 1 |2 3 1
			#      2  5 15 18 19 23(2) | 3 10  3  1  4 |  1  -3   4   3   1 -22 |2 3 1 0 0 |2 3 1
			#      3  6 10 30 34 37    | 3  4 20  4  3 |  1   1  -5  12  15  14 |2 1 0 3 0 |2 1 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 |  2   9  10   1   0   5 |1 1 1 2 1 |1 1 1 2 1
			#     13 14 19 26 40 43    | 1  5  7 14  3 |  8  -1  -1  -5   6   1 |0 3 1 0 2 |3 1 2
			if( all( c(26,40) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( 2<=sum(aZoid[c(5,5)]==c(13,14,19,26,40,43)[c(2,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextFStepBin
			#      3  5 20 34 35 44    | 2 15 14  1  9 |                        |2 0 1 2 1 |2 1 2 1
			#      8 13 20 22 23 36(1) | 5  7  2  1 13 |  5   8   0 -12 -12  -8 |1 1 3 1 0 |1 1 3 1
			#     12 15 19 26 40 43    | 3  4  7 14  3 |  4   2  -1   4  17   7 |0 3 1 0 2 |3 1 2
			#     11 18 21 26 38 43(2) | 7  3  5 12  5 | -1   3   2   0  -2   0 |0 2 2 1 1 |2 2 1 1
			#      5 12 14 32 34 42    | 7  2 18  2  8 | -6  -6  -7   6  -4  -1 |1 2 0 2 1 |1 2 2 1
			#      1  8 17 34 39 45(1) | 7  9 17  5  6 | -4  -4   3   2   5   3 |2 1 0 2 1 |2 1 2 1
			if( 3<=sum(aBin[c(1,2,4)]==c(1,0,0)) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(aZoid[c(3,4)]==c( 1, 8,17,34,39,45)[c(3,5)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all( c(34,45) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.

		# fCutCnt.nextColVal_1
			#      2 10 14 22 32 36    | 8  4  8 10  4 |                        |1 2 1 2 0 |1 2 1 2
			#      1 10 13 26 32 36(3) | 9  3 13  6  4 | -1   0  -1   4   0   0 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  7   1   6  -5   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2
			#     14 26 32 36 39 42(1) |12  6  4  3  3 | 11  16  16  17   8   3 |0 1 1 3 1 |1 1 3 1
			if( 1<=sum(aZoid[c(5)]==c(14,26,32,36,39,42)[c(6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	unique
			if( all( c(39,42) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( all(aRem[c(2,3)]==aRem[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
			#      3  4  9 24 25 33    | 1  5 15  1  8 |                        |3 0 2 1 0 |3 2 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 | 11  11   7  -7  13  12 |0 4 0 1 1 |4 1 1
			#      3  9 12 13 25 43    | 6  3  1 12 18 |-11  -6  -4  -4 -13  -2 |2 2 1 0 1 |2 2 1 1
			#     12 18 24 26 39 40(1) | 6  6  2 13  1 |  9   9  12  13  14  -3 |0 2 2 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 | -5  -9   0   3  -5  -2 |2 0 2 2 0 |2 2 2
			#      6  8 18 35 42 43    | 2 10 17  7  1 | -1  -1  -6   6   8   5 |2 1 0 1 2 |2 1 1 2
			if( all(aZoid[1:2]==c( 5,7 )) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<=sum(aZoid[c(3,1)]==c( 6, 8,18,35,42,43)[c(3,3)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	

		# fCutCnt.nextColVal_3
			#      6  7 10 16 38 41    | 1  3  6 22  3 |                        |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#      6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  1   0  -5 -20  -9   2 |2 2 0 1 1 |2 2 1 1
			if( 2<=sum(aZoid[c(2,3)]==c( 7, 8,13,15,33,45)[c(2,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn	
			if( all(aRem[c(3,4)]==aRem[c(5,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			#      3  7  8 34 39 41    | 4  1 26  5  2 |                        |3 0 0 2 1 |3 2 1
			#      7 12 16 34 42 45(2) | 5  4 18  8  3 |  4   5   8   0   3   4 |1 2 0 1 2 |1 2 1 2
			#      2  9 15 23 34 40(1) | 7  6  8 11  6 | -5  -3  -1 -11  -8  -5 |2 1 1 1 1 |2 1 1 1 1
			#      1  9 12 28 36 41(1) | 8  3 16  8  5 | -1   0  -3   5   2   1 |2 1 1 1 1 |2 1 1 1 1
			#      5 16 21 23 24 30    |11  5  2  1  6 |  4   7   9  -5 -12 -11 |1 1 3 1 0 |1 1 3 1
			#     12 14 21 30 39 43(2) | 2  7  9  9  4 |  7  -2   0   7  15  13 |0 2 1 2 1 |2 1 2 1
			if( 2<=sum(aZoid[c(5,2)]==c(12,14,21,30,39,43)[c(4,2)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn
			if( all( c(14,30) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.
			if( all( c(21,43) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.

		# fCutCnt.nextColVal_5
			#      5 16 21 23 24 30    |11  5  2  1  6 |                        |1 1 3 1 0 |1 1 3 1
			#     17 23 27 35 38 43(1) | 6  4  8  3  5 | 12   7   6  12  14  13 |0 1 2 2 1 |1 2 2 1
			#      3 11 14 15 32 36    | 8  3  1 17  4 |-14 -12 -13 -20  -6  -7 |1 3 0 2 0 |1 3 2
			#     12 14 21 30 39 43(1) | 2  7  9  9  4 |  9   3   7  15   7   7 |0 2 1 2 1 |2 1 2 1
			#     10 11 12 18 24 42(1) | 1  1  6  6 18 | -2  -3  -9 -12 -15  -1 |0 4 1 0 1 |4 1 1
			#      5  6 16 18 37 38(1) | 1 10  2 19  1 | -5  -5   4   0  13  -4 |2 2 0 2 0 |2 2 2
			if( 2<=sum(aZoid[c(2,2,3,4)]==c( 5, 6,16,18,37,38)[c(4,3,1,4)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn
			if( all(aRem[c(2,4)]==aRem[c(3,6)]) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			#     10 21 22 30 35 42    |11  1  8  5  7 |                        |0 1 2 2 1 |1 2 2 1
			#      5  6 16 18 37 38    | 1 10  2 19  1 | -5 -15  -6 -12   2  -4 |2 2 0 2 0 |2 2 2
			#      1  9 11 14 26 28    | 8  2  3 12  2 | -4   3  -5  -4 -11 -10 |2 2 2 0 0 |2 2 2
			#      9 14 17 33 36 38(2) | 5  3 16  3  2 |  8   5   6  19  10  10 |1 2 0 3 0 |1 2 3
			#      1 16 29 33 40 45(1) |15 13  4  7  5 | -8   2  12   0   4   7 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			if( 2<=sum(aZoid[c(4,6)]==c( 5,18,30,41,43,45)[c(4,6)]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn
			if( all( c(18,41) %in% aZoid ) ){	surviveFlg[idx]<-FALSE	;next }		# 2 reb ptn.

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
		if( 1<sum(aZoid==c(  2,15,27,43,NA,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  7,15,25,29,42,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  4,23,36,25,36,39 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( 18,23,28,NA,24,23 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  5,NA,NA,NA,43,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c(  2,NA,NA,NA,38,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( NA,NA,NA,NA,32,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( NA,NA,NA,NA,44,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( NA,NA,NA,NA,31,36 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( NA,NA,NA,NA,27,44 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum(aZoid==c( NA,NA,NA,NA,36,45 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }


		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  4,10, 9,17, 1 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  7, 5,26,10, 4 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA, 3, 7, 9, 6 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA, 5,NA, 1, 3 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA, 7,NA, 8, 6 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA, 1,NA, 5, 5 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA, 1,NA, 3, 9 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( NA,NA,NA,NA,NA ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }


		cnt <- 0
			# if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			# if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=3 )
    	# [  1]              7  8 34                23 24 30
		if( all(aZoid[1:3+1]==c( 7, 8,34)) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[1:3+3]==c(23,24,30)) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	a,a,a.. a?
		if( all(aCStep[1:2]==c(3,8)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextColVal_1 ,fCutCnt.nextFStepBin
		if( all(aCStep[1:2]==c(7,6)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextColVal_1 ,fCutCnt.nextFStepBin

		#	a,a,b.. b?
		if( all(aCStep[c(2,5)]==c(6,3)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextQuo10 ,fCutCnt.nextColVal_1

		#	a,b,b.. a?
		if( all(aCStep[1:2]==c(6,4)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextColVal_2 ,fCutCnt.nextBin
		if( all(aCStep[1:2]==c(2,4)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextColVal_5 ,fCutCnt.nextBin

		#	1,2,3..4?
		if( all(aCStep[3]==c(4)) ){	surviveFlg[idx]<-FALSE	;next } 	# fCutCnt.nextRebNum


		# fCutCnt.basic()
			if( 1<sum( aCStep[ 5 ]*c(6,1)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23,17, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18,20, 4,11, 7 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 5,NA, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 4, 9 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 4,14, 4 ),aCStep,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 22,12,NA, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 5, 5, 4, 4 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
  			if( 1<sum( aCStep[c(1,2)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4,NA,14, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,16, 1, 4 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,12,16, 7 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 9,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 9, 6 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
			if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,17, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 3,25 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,25, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 7,21, 2 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
			if( 1<sum( aCStep[ 5 ]*c(4,2,1)==aCStep[c(1,2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aCStep[c(5,3)]*c(1,3)==aCStep[c(4,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 3,13, 7 ),aCStep,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 17,10, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 6,NA,21 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,11, 5,17 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15,12,NA,19,10 ),aCStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 19, 1,11,11 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
			if( fCutU.hasPtn(c(  9,17,3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,NA, 4, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,25, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 5, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 7,21, 2 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			if( fCutU.hasPtn(c(  8,NA,NA, 1,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 6,17 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,13,22, 5 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,16,16, 8, 4 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6,NA,14, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum(aCStep[1:2+0]==c( 3, 5 )) ){	surviveFlg[idx]<-FALSE	;next }	# cStep[1] 3은.. unique

		# fCutCnt.nextColVal_1
			if( fCutU.hasPtn(c(  9,24, 2, 1 ),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 17, 2, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5, 2, 3 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 6, 4 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 17, 6, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11, 6, 5 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( aCStep[2]%in%c(  6, 8      ) ){	surviveFlg[idx]<-FALSE	;next }	# unique 6 재발?

		# fCutCnt.nextColVal_2
			if( fCutU.hasPtn(c( 14,NA, 1, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 5,29, 9 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 29, 5,NA, 1 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 9, 3,14 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 1,25 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,19,13, 7 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 15, 9, 2 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			
		# fCutCnt.nextColVal_3
			if( 1<sum( aCStep[ 3 ]*c(9,6)==aCStep[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,26, 7 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 3,14 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1, 5,NA,NA,16 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 2,11 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14,22, 7, 1 ),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 24, 1, 8 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,10,NA,13 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
			if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2,13,12 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,12,10 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
			if( 1<sum( aCStep[c(1,3)]*c(1,5)==aCStep[c(5,2)] ) ){	surviveFlg[idx]<-FALSE	;next }

			if( fCutU.hasPtn(c(  1,14,NA,20 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 11,17, 1, 4 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  6, 8,28 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
			if( fCutU.hasPtn(c( 21,21, 6, 1, 2 ),aCStep,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 23, 8,19, 2 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 29,18, 3, 7 ),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  2, 3,20 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 3,20, 4 ),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 13,20,15 ),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

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

		# a,b,b.. b?
		# aFStep[2] %in% c( 2 )	# 20 : fCutCnt.nextColVal_6
		# aFStep[6] %in% c( 0 )	# 45 : fCutCnt.basic
		if( all(aZoid[c(2,6)]==c(20,45)) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(2,4)==aFStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 14,-4,-14,-2,-1, 0 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 21,21,20,17, 3 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 8,-10,-18 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(-4,-2, 2)==aFStep[c(1,4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[ 6 ]*c(-2,-1)==aFStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,-12,-26,-25,-21 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  -4,NA,NA,-10,-7 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 32, 5,15 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 12, 3,12, 3 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid
			#	unique	: 1,2,3... -5,-6,-7 처럼 간격이 동일한 패턴이 과연 반복될 수 있을까?
			if( fCutU.hasPtn(c(  3, 6, 7, -5,-6 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3, 6, 7, -5,-6 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3,-16,-20 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -9, 3,-2, 1,-24,-28 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -21,-6,-1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,-5,-10,-11 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3, -3,-5, 0,-13 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-1,15 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 19, 1, 5,-5 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(-2, 1)==aFStep[c(4,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-10, -5, -5 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5,-13,-11,-14, -5 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-5,-18,-3,-3 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,25,NA, 8, 5, 7 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 6 ]*c(-1,-1)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3,-3,-5, 0,-13 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4,-19, 2, 1 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10, 6,19, 3 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[ 4 ]*c(-2,-2)==aFStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( 1<sum( aFStep[c(1,3)]*c(1,1)==aFStep[c(2,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -14, -4,  7 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-4, 0, 8,12 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7, 3, 2,10, 8 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -7, 3, 2,10, 8 ),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[ 5 ]*c(2,2)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1,NA,NA,NA, 0, 0 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -15,-13,10, -5,-16 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 6, 7,-24, 0, 0 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 6, 7,-24, 0, 0 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 6, 7,-24, 0, 0 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1, 2, 6,-20 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  3,12,-9, 4,18 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11, 0, 1 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2, 7, 2,-20, 0 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c( 1,-1)==aFStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8,-6,16,20,23 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-12, 4,10, 3, -2 ),aFStep,thld=3,fixIdx=6) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 25, 5,12 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,11,NA,13,16 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid
			if( fCutU.hasPtn(c( -14, -9,-10 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -11, -8,23,-6 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2,-7, 2,-1,-6 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5,NA,-27,-6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7, 0,-20,NA,-7,10 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  1,-6,-4,NA,-8 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( fCutU.hasPtn(c( 23,-7,-22,-11 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5,-21,-21 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  9,25,26,-5 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  5,-3,11,15,10 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -1,11, 2,15 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3, 0,17,25,24 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  4, 9,10,-10 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -10, 5,-4, 2 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  7,-13, 5,26 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -4, 7,21 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( fCutU.hasPtn(c( -4,-12,-17,18 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -17,-16,-4,-14,13 ),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -6, 7,24, 2, 0 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 18, 7, 8 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c(2,4)==aFStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -5, 7,-16, -8 ),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -2,18,-12,-4 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 10,14,-6, 0 ),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 16, 2,-10,16, 2,-7 ),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 20, 3, 5 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(  8, 1,-6 ),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

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

		#	fCutCnt.nextZW

		#	fCutCnt.nextQuo10

		#	fCutCnt.nextBin

		#	fCutCnt.nextRebNum

		#	fCutCnt.nextCStepBin

		#	fCutCnt.nextFStepBin

		#	fCutCnt.nextColVal_1

		#	fCutCnt.nextColVal_2

		#	fCutCnt.nextColVal_3

		#	fCutCnt.nextColVal_4

		#	fCutCnt.nextColVal_5

		#	fCutCnt.nextColVal_6

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
			#      5 11 14 30 33 38    | 6  3 16  3  5 |                        |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			#     19 21 30 33 34 42(1) | 2  9  3  1  8 |  5  -5  -2  -3  -5   0 |0 1 1 3 1 |1 1 3 1
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |-12 -13 -17 -18  -1   3 |2 2 0 1 1 |2 2 1 1
			#      1 16 29 33 40 45(2) |15 13  4  7  5 | -6   8  16  18   7   0 |1 1 1 1 2 |1 1 1 1 2
			#      5 18 30 41 43 45(1) |13 12 11  2  2 |  4   2   1   8   3   0 |1 1 0 1 3 |1 1 1 3
			if( all(quoSize==c( 1 ,1 ,0 ,1 ,3)) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#      3  5 20 34 35 44    | 2 15 14  1  9 |                        |2 0 1 2 1 |2 1 2 1
			#      8 13 20 22 23 36(1) | 5  7  2  1 13 |  5   8   0 -12 -12  -8 |1 1 3 1 0 |1 1 3 1
			#     12 15 19 26 40 43    | 3  4  7 14  3 |  4   2  -1   4  17   7 |0 3 1 0 2 |3 1 2
			#     11 18 21 26 38 43(2) | 7  3  5 12  5 | -1   3   2   0  -2   0 |0 2 2 1 1 |2 2 1 1
			#      5 12 14 32 34 42    | 7  2 18  2  8 | -6  -6  -7   6  -4  -1 |1 2 0 2 1 |1 2 2 1
			#      1  8 17 34 39 45(1) | 7  9 17  5  6 | -4  -4   3   2   5   3 |2 1 0 2 1 |2 1 2 1
			if( all(quoSize[1:3+2]==c(0,2,1)) ){	surviveFlg[idx]<-FALSE	;next }	# next rebind of 0,2,1	# unique
			if( all(quoSize[1:3+2]==c(2,1,1)) ){	surviveFlg[idx]<-FALSE	;next }	# next rebind of 0,2,1	# unique

		# fCutCnt.nextColVal_3
			#      6  7 10 16 38 41    | 1  3  6 22  3 |                        |2 2 0 1 1 |2 2 1 1
			#      1  7 19 26 27 35(1) | 6 12  7  1  8 | -5   0   9  10 -11  -6 |2 1 2 1 0 |2 1 2 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 |  2  -3 -10  -2  -2  -2 |3 0 2 1 0 |3 2 1
			#      2  5 15 18 19 23    | 3 10  3  1  4 | -1   1   6  -6  -6 -10 |2 3 1 0 0 |2 3 1
			#      6  8 18 35 42 43(1) | 2 10 17  7  1 |  4   3   3  17  23  20 |2 1 0 1 2 |2 1 1 2
			#      7  8 13 15 33 45(1) | 1  5  2 18 12 |  1   0  -5 -20  -9   2 |2 2 0 1 1 |2 2 1 1
			if( all(quoSize==c( 2, 1, 2, 1, 0 )) ){	surviveFlg[idx]<-FALSE	;next }		# 2 2 0 1 1->2 1 2 1 0

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
	#	"nextRebNum( NA )","nextColVal_1( 35, 36 )","nextColVal_5( 31,34 )","nextFStepBin( NA )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c( 35,36,31,34) )]

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






