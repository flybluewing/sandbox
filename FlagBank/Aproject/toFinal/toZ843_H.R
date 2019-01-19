# toZ843_H.R 최종접근
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
			#      2 25 28 30 33 45    |23  3  2  3 12 |                        |1 0 2 2 1 |1 2 2 1
			#      9 14 17 33 36 38(1) | 5  3 16  3  2 |  7 -11 -11   3   3  -7 |1 2 0 3 0 |1 2 3
			#      3  9 11 12 13 19(1) | 6  2  1  1  6 | -6  -5  -6 -21 -23 -19 |2 4 0 0 0 |2 4
			#      2  4 11 28 29 43(1) | 2  7 17  1 14 | -1  -5   0  16  16  24 |2 1 2 0 1 |2 1 2 1
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  3   7   3   2   4  -5 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1

			#   zoid width  ... 43   29   16   41   33   28 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                           descript tgt.dir
			#  840       1      2 [same    ]  2(?), ., ., 2, ., ., 2     col
			#  1         1     15       [desc1   ] 15(?),xx,14,xx,13  Slide/
			#  11        4     40             [desc1   ] 40(?),39,38  Slide/

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
			#   dup number  2:2   9:2   11:3   14:3   28:2   30:2   33:3   36:2   38:2
			# <  2>   0      2 , 25 , 28 , 30 , 33 , 45 
			#                2 ,  4 , 11 , 28 , 29 , 43 
			#          -->   2*, NA , NA , 26 , 25 , 41 
			# <  9>   1      9 , 14 , 17 , 33 , 36 
			#                9 , 11 , 12 , 13 , 19 
			#          -->   9*, NA , NA , NA , NA 
			# < 11>  -1      4 , 11 , 28 , 29 , 43 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   6!, 11*, NA , 31!, 23 
			# < 14>  -2     14 , 30 , 33 , 38 
			#               14 , 26 , 32 , 36 
			#          -->  14*, 22 , 31!, 34 
			# < 28>   1      2 , 25 , 28 , 30 , 33 
			#                4 , 11 , 28 , 29 , 43 
			#          -->   6 , NA , 28*, NA , NA 
			# < 30>   0      2 , 25 , 28 , 30 , 33 , 45 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   8 , NA , NA , 30*, 33!, 31 
			# < 33>   1      9 , 14 , 17 , 33 , 36 
			#               11 , 14 , 30 , 33 , 38 
			#          -->  13 , 14!, NA , 33*, 40 
			# < 36>  -1     14 , 17 , 33 , 36 , 38 
			#               14 , 26 , 32 , 36 , 39 
			#          -->  14!, 35 , 31!, 36*, 40!
			# < 38>   0      9 , 14 , 17 , 33 , 36 , 38 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   1 ,  8 , 11 , 27 , 30 , 38*
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
			# 840        1      2     [same    ]  2(?), ., ., 2, ., ., 2     col
			# 842        1      3                 [desc1   ]  3(?), 4, 5     col
			# 8401       2      3     [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 841        5      3     [same    ]  3(?), ., 3, ., 3, ., 3     col
			# 8411       5      3 [seqReb  ]  3(?), ., 3, ., 3, ., 3,...     col
			# 839        5      3           [symm    ]  3(?), 9, 3, 9, 3     col
			# 8412       6      7           [desc1   ]  7(?),xx, 8,xx, 9     col
			# 1          1      5           [desc1   ]  5(?),xx, 4,xx, 3  Slide/
			# 11         3      6          [seqReb  ]  6(?), 6, 3, 3,...  Slide/
			# 12         4     10                 [desc1   ] 10(?), 9, 8  Slide/
			# 13         3      7                 [desc1   ]  7(?), 6, 5 Slide\\
			# 14         4      3                 [desc1   ]  3(?), 2, 1 Slide\\
			# 15         5      5           [desc1   ]  5(?),xx, 4,xx, 3 Slide\\
			# 16         5      6          [seqReb  ]  6(?), 6, 4, 4,... Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      841        1      6       [same    ]  6(?), ., 6, ., 6     col
			#      837        1     23 [sameEnd ] 23(?),xx, 6,xx, 6,xx,23     col
			#      8411       2      4       [desc1   ]  4(?),xx, 3,xx, 2     col
			#      842        4      3             [same    ]  3(?), 3, 3     col
			#      840        4      1          [sameEnd ]  1(?), 3, 3, 1     col
			#      8412       5      4       [desc1   ]  4(?),xx, 5,xx, 6     col
			#      1          2      5             [desc1   ]  5(?), 4, 3  Slide/
			#      11         3      6             [same    ]  6(?), 6, 6 Slide\\
			#      12         4      5          [desc1   ]  5(?), 4, 3, 2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(4)   3(8)   5(2)   6(4)   12(2)   16(2) 
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
			# -------------------------------------------------------------------------------------
			#     [1] tgt.col  banVal   descript tgt.dir 
			#      <0 rows> (or 0-length row.names)
			# -------------------------------------------------------------------------------------
			#     FV :    -11(2)   -6(2)   -5(3)   3(4)   4(2)   6(2)   7(2)   16(2) 
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
			#      2 11 19 25 28 32    | 9  8  6  3  4 |                        |1 2 2 1 0 |1 2 2 1
			#     23 27 28 38 42 43(1) | 4  1 10  4  1 | 21  16   9  13  14  11 |0 0 3 1 2 |3 1 2
			#      4  8  9 16 17 19    | 4  1  7  1  2 |-19 -19 -19 -22 -25 -24 |3 3 0 0 0 |3 3
			#      6 18 31 34 38 45    |12 13  3  4  7 |  2  10  22  18  21  26 |1 1 0 3 1 |1 1 3 1
			#      6 11 15 17 23 40(1) | 5  4  2  6 17 |  0  -7 -16 -17 -15  -5 |1 3 1 0 1 |1 3 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 |  1  -2   9  12  11  -2 |2 0 2 2 0 |2 2 2

			#   zoid width  ... 30   20   15   39   34   31 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                            descript tgt.dir
			#  824        1      8              [desc1   ]  8(?), 7, 6     col
			#  809        1      8 [desc(-2) ]  8(?),xx, 6,xx, 4,xx, 2     col
			#  8241       1      7       [seqReb  ]  7(?), 7, 6, 6,...     col
			#  8091       4     18        [desc1   ] 18(?),xx,17,xx,16     col

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
			#   dup number  6:2   9:2   11:2   17:2   19:2   23:2   28:2   34:2   38:3
			# <  6>   0      6 , 18 , 31 , 34 , 38 , 45 
			#                6 , 11 , 15 , 17 , 23 , 40 
			#          -->   6*, NA , NA , NA ,  8 , 35 
			# <  9>  -1      8 ,  9 , 16 , 17 , 19 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->   6!,  9*, 32 , 41 , NA 
			# < 11>   0      2 , 11 , 19 , 25 , 28 , 32 
			#                6 , 11 , 15 , 17 , 23 , 40 
			#          -->  10 , 11*, NA , NA , 18 , NA 
			# < 17>  -1      8 ,  9 , 16 , 17 , 19 
			#                6 , 11 , 15 , 17 , 23 
			#          -->   4 , 13 , 14!, 17*, 27 
			# < 19>   3      2 , 11 , 19 
			#               16 , 17 , 19 
			#          -->  NA , NA , 19*
			# < 23>   4     23 , 27 
			#               23 , 40 
			#          -->  23*, NA 
			# < 28>  -2     19 , 25 , 28 , 32 
			#               23 , 27 , 28 , 38 
			#          -->  27 , NA , 28*, 44 
			# < 34>   1      6 , 18 , 31 , 34 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->  12 , 30 , 27 , 34*, 38!
			# < 38>   1      6 , 18 , 31 , 34 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->  12 , 30 , 27 , 34!, 38*
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
			# 824        1      8                 [desc1   ]  8(?), 7, 6     col
			# 809        1      8    [desc(-2) ]  8(?),xx, 6,xx, 4,xx, 2     col
			# 8241       1      7          [seqReb  ]  7(?), 7, 6, 6,...     col
			# 8242       3      3                 [desc1   ]  3(?), 4, 5     col
			# 8091       3      5 [seqReb  ]  5(?), ., 5, ., 9, ., 9,...     col
			# 8092       4      8     [desc1   ]  8(?),xx, 7,xx, 6,xx, 5     col
			# 782        4      3     [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 7821       5      8     [same    ]  8(?), ., ., 8, ., ., 8     col
			# 8243       5      5                 [desc1   ]  5(?), 4, 3     col
			# 1          5      6           [desc1   ]  6(?),xx, 5,xx, 4 Slide\\
			# 11         6      6           [desc1   ]  6(?),xx, 7,xx, 8 Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                           descript tgt.dir
			#      809       1      6       [desc1   ]  6(?),xx, 5,xx, 4     col
			#      824       4      4             [desc1   ]  4(?), 5, 6     col
			#      782       4      5 [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			#      1         1      2       [same    ]  2(?), ., 2, ., 2  Slide/
			#      11        2      4          [desc1   ]  4(?), 5, 6, 7  Slide/
			#      12        4      6             [desc1   ]  6(?), 5, 4 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(3)   3(2)   4(7)   5(3)   6(2)   7(2) 
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal               descript tgt.dir
			#      1       1      2 [desc1   ]  2(?), 1, 0     col
			# -------------------------------------------------------------------------------------
			#     FV :    -19(3)   -2(2)   9(2)   11(2)   21(2) 
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
			#      2  5 15 18 19 23    | 3 10  3  1  4 |                        |2 3 1 0 0 |2 3 1
			#      6 16 37 38 41 45    |10 21  1  3  4 |  4  11  22  20  22  22 |1 1 0 2 2 |1 1 2 2
			#      6 12 19 24 34 41(2) | 6  7  5 10  7 |  0  -4 -18 -14  -7  -4 |1 2 1 1 1 |1 2 1 1 1
			#     14 15 16 17 38 45    | 1  1  1 21  7 |  8   3  -3  -7   4   4 |0 4 0 1 1 |4 1 1
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 |-12  -5  -2   5  -6  -9 |1 2 1 2 0 |1 2 1 2
			#     12 18 24 26 39 40    | 6  6  2 13  1 | 10   8  10   4   7   4 |0 2 2 1 1 |2 2 1 1
			
			#   zoid width  ... 21   39   35   31   34   28 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  783        3     17 [desc1   ] 17(?),xx,xx,16,xx,xx,15     col
			#  7831       4     16 [desc1   ] 16(?),xx,xx,17,xx,xx,18     col
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
			#   dup number  2:2   6:2   12:2   14:2   15:2   16:2   18:2   19:2   24:2   38:2   41:2   45:2
			# <  2>   0      2 ,  5 , 15 , 18 , 19 , 23 
			#                2 , 10 , 14 , 22 , 32 , 36 
			#          -->   2*, 15 , 13!, 26 , 45 , NA 
			# <  6>   0      6 , 16 , 37 , 38 , 41 , 45 
			#                6 , 12 , 19 , 24 , 34 , 41 
			#          -->   6*,  8 , NA , 10 , 27 , 37 
			# < 12>  -1     12 , 19 , 24 , 34 , 41 
			#               12 , 18 , 24 , 26 , 39 
			#          -->  12*, 17!, 24!, 18 , 37 
			# < 14>   2     14 , 15 , 16 , 17 
			#               14 , 22 , 32 , 36 
			#          -->  14*, 29 , NA , NA 
			# < 15>  -1      5 , 15 , 18 , 19 , 23 
			#               14 , 15 , 16 , 17 , 38 
			#          -->  NA , 15*, NA , NA , NA 
			# < 16>   1      6 , 16 , 37 , 38 , 41 
			#               15 , 16 , 17 , 38 , 45 
			#          -->  NA , 16*, NA , 38!, NA 
			# < 18>  -2     15 , 18 , 19 , 23 
			#               12 , 18 , 24 , 26 
			#          -->   9 , 18*, 29 , 29 
			# < 19>  -2     15 , 18 , 19 , 23 
			#                6 , 12 , 19 , 24 
			#          -->  NA ,  6 , 19*, 25!
			# < 24>  -1     12 , 19 , 24 , 34 , 41 
			#               12 , 18 , 24 , 26 , 39 
			#          -->  12!, 17!, 24*, NA , 37 
			# < 38>   1      6 , 16 , 37 , 38 , 41 
			#               15 , 16 , 17 , 38 , 45 
			#          -->  24 , 16!, NA , 38*, NA 
			# < 41>   1      6 , 16 , 37 , 38 , 41 
			#               12 , 19 , 24 , 34 , 41 
			#          -->  18 , 22 , 11 , 30 , 41*
			# < 45>   0      6 , 16 , 37 , 38 , 41 , 45 
			#               14 , 15 , 16 , 17 , 38 , 45 
			#          -->  22 , 14!, NA , NA , 35 , 45*
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
			# 823        1      2             [same    ]  2(?), 2, 2     col
			# 783        1      4          [sameEnd ]  4(?), 2, 2, 4     col
			# 7831       2      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			# 8231       3      4             [same    ]  4(?), 4, 4     col
			# 7832       3      6          [sameEnd ]  6(?), 4, 4, 6     col
			# 7833       3      7 [desc1   ]  7(?),xx,xx, 6,xx,xx, 5     col
			# 7834       4      6 [desc1   ]  6(?),xx,xx, 7,xx,xx, 8     col
			# 7835       5      7 [desc1   ]  7(?),xx,xx, 8,xx,xx, 9     col
			# 1          1      4       [same    ]  4(?), ., 4, ., 4  Slide/
			# 11         2      3       [desc1   ]  3(?),xx, 2,xx, 1  Slide/
			# 12         6      2       [same    ]  2(?), ., 2, ., 2 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                            descript tgt.dir
			#      798        2      1 [desc( 3) ]  1(?),xx, 4,xx, 7,xx,10     col
			#      7981       4     10        [same    ] 10(?), .,10, .,10     col
			#      713        4      1  [sameEnd ]  1(?),xx,10,xx,10,xx, 1     col
			#      1          1      9        [desc1   ]  9(?),xx, 8,xx, 7  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(6)   3(3)   4(4)   6(3)   7(3)   8(2)   10(4)   21(2) 
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                     descript tgt.dir
			#      1       2     -6 [desc1   ] -6(?),xx,-5,xx,-4     col
			#      2       4      3       [desc1   ]  3(?), 4, 5     col
			#      3       5     -5 [desc1   ] -5(?),xx,-6,xx,-7     col
			#      4       6     -4 [symm    ] -4(?), 4,-9, 4,-4     col
			# -------------------------------------------------------------------------------------
			#     FV :    -7(2)   -4(2)   4(5)   8(2)   10(2)   22(3) 
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
			#      7 20 22 25 38 40    |13  2  3 13  2 |                        |1 0 3 1 1 |1 3 1 1
			#     12 15 19 26 40 43(1) | 3  4  7 14  3 |  5  -5  -3   1   2   3 |0 3 1 0 2 |3 1 2
			#     10 11 15 25 35 41(1) | 1  4 10 10  6 | -2  -4  -4  -1  -5  -2 |0 3 1 1 1 |3 1 1 1
			#      3  4  9 24 25 33(1) | 1  5 15  1  8 | -7  -7  -6  -1 -10  -8 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2

			#   zoid width  ... 33   31   31   30   38   31 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                               descript tgt.dir
			#  824        1      8                 [desc1   ]  8(?), 7, 6     col
			#  710        4     23     [desc1   ] 23(?),xx,xx,24,xx,xx,25     col
			#  757        4     17 [seqReb  ] 17(?), .,17, .,25, .,25,...     col
			#  8241       5     35                 [desc1   ] 35(?),34,33     col
			#  1          3     29          [seqReb  ] 29(?),29,33,33,...  Slide/
			#  11         5     12           [desc1   ] 12(?),xx,11,xx,10 Slide\\

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
			#   dup number  7:3   9:2   11:2   15:2   24:2   25:3   33:2   38:2   40:2
			# <  7>  -1      7 , 11 , 17 , 33 , 44 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->   7*, NA , 31 , 25 , 24 
			# <  9>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->  NA ,  9*, 24!, 33 , 35!
			# < 11>   1     10 , 11 , 15 , 25 , 35 
			#                7 , 11 , 17 , 33 , 44 
			#          -->   4 , 11*, 19 , 41 , NA 
			# < 15>   1     12 , 15 , 19 , 26 , 40 
			#               11 , 15 , 25 , 35 , 41 
			#          -->  10!, 15*, 31 , 44 , 42!
			# < 24>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->  10 ,  9!, 24*, 33 , 35!
			# < 25>   1     10 , 11 , 15 , 25 , 35 
			#                4 ,  9 , 24 , 25 , 33 
			#          -->  NA ,  7 , NA , 25*, 31 
			# < 33>  -1      4 ,  9 , 24 , 25 , 33 
			#                6 ,  7 , 11 , 17 , 33 
			#          -->   8 ,  5 , NA ,  9 , 33*
			# < 38>   1      7 , 20 , 22 , 25 , 38 
			#                9 , 24 , 29 , 34 , 38 
			#          -->  11 , 28 , 36 , NA , 38*
			# < 40>  -1     20 , 22 , 25 , 38 , 40 
			#               12 , 15 , 19 , 26 , 40 
			#          -->   4 ,  8 , 13 , 14 , 40*
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
			# 824        1      8                 [desc1   ]  8(?), 7, 6     col
			# 710        4      3     [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 757        4      7 [seqReb  ]  7(?), ., 7, ., 5, ., 5,...     col
			# 8241       5      5                 [desc1   ]  5(?), 4, 3     col
			# 1          3      9          [seqReb  ]  9(?), 9, 3, 3,...  Slide/
			# 11         4      4                 [same    ]  4(?), 4, 4  Slide/
			# 12         5      2           [desc1   ]  2(?),xx, 1,xx, 0 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      757        1      1       [same    ]  1(?), ., 1, ., 1     col
			#      505        1     13 [sameEnd ] 13(?),xx, 1,xx, 1,xx,13     col
			#      824        1      3             [desc1   ]  3(?), 2, 1     col
			#      8241       1      2      [seqReb  ]  2(?), 2, 1, 1,...     col
			#      7571       2      4       [same    ]  4(?), ., 4, ., 4     col
			#      5051       2      2 [sameEnd ]  2(?),xx, 4,xx, 4,xx, 2     col
			#      8242       3      4             [desc1   ]  4(?), 5, 6     col
			#      1          1      6       [same    ]  6(?), ., 6, ., 6  Slide/
			#      11         4      6             [desc1   ]  6(?), 5, 4 Slide\\
			#      12         5      4             [desc1   ]  4(?), 5, 6 Slide\\
			#      13         5      1       [symm    ]  1(?), 5, 6, 5, 1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(3)   3(3)   4(4)   5(3)   6(2)   10(2)   13(2)   15(2) 
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                    descript tgt.dir
			#      1         2      1      [desc1   ]  1(?), 2, 3     col
			#      E2        1      2      [same    ]  2(?), 2, 2  Slide/
			#      E4        1     -1   [sameEnd ] -1(?), 2, 2,-1  Slide/
			#      E21       3      1      [desc1   ]  1(?), 2, 3 Slide\\
			#      E3        4     23 [desc(-10) ] 23(?),13, 3,-7 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -7(3)   -6(2)   -5(2)   -4(2)   -2(2)   -1(2)   1(3)   2(3)   3(3) 
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
			#      7 15 20 25 33 43    | 8  5  5  8 10 |                        |1 1 2 1 1 |1 1 2 1 1
			#     11 17 21 26 36 45    | 6  4  5 10  9 |  4   2   1   1   3   2 |0 2 2 1 1 |2 2 1 1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
			#     14 26 32 36 39 42(1) |12  6  4  3  3 |  9  15  18   6   6   4 |0 1 1 3 1 |1 1 3 1
			
			#   zoid width  ... 36   34   35   41   33   28 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                     descript tgt.dir
			#  841        1      6 [desc1   ]  6(?),xx, 5,xx, 4     col
			#  8411       3     15 [desc1   ] 15(?),xx,14,xx,13     col
			#  8412       4     31 [desc1   ] 31(?),xx,30,xx,29     col
			#  8413       6     37 [desc1   ] 37(?),xx,38,xx,39     col
			#  1          4     40       [desc1   ] 40(?),39,38  Slide/

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
			#   dup number  4:2   5:2   7:2   11:2   14:2   26:2   31:2   33:2   36:2   39:2   43:2   45:2
			# <  4>   0      4 ,  7 , 13 , 29 , 31 , 39 
			#                4 ,  5 , 31 , 35 , 43 , 45 
			#          -->   4*, NA , NA , 41 , NA , NA 
			# <  5>  -1      5 , 31 , 35 , 43 , 45 
			#                5 , 11 , 14 , 30 , 33 
			#          -->   5*, NA , NA , 17 , 21 
			# <  7>   1      7 , 15 , 20 , 25 , 33 
			#                7 , 13 , 29 , 31 , 39 
			#          -->   7*, 11 , 38 , 37 , 45 
			# < 11>   1     11 , 17 , 21 , 26 , 36 
			#               11 , 14 , 30 , 33 , 38 
			#          -->  11*, NA , 39 , 40 , 40 
			# < 14>  -2     14 , 30 , 33 , 38 
			#               14 , 26 , 32 , 36 
			#          -->  14*, 22 , 31!, 34 
			# < 26>  -2     21 , 26 , 36 , 45 
			#               14 , 26 , 32 , 36 
			#          -->   7 , 26*, 28 , 27 
			# < 31>  -2     13 , 29 , 31 , 39 
			#                4 ,  5 , 31 , 35 
			#          -->  NA , NA , 31*, NA 
			# < 33>   0      7 , 15 , 20 , 25 , 33 , 43 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   3 ,  7 ,  8 , NA , 33*, NA 
			# < 36>  -1     17 , 21 , 26 , 36 , 45 
			#               14 , 26 , 32 , 36 , 39 
			#          -->  11 , 31 , NA , 36*, NA 
			# < 39>  -1      7 , 13 , 29 , 31 , 39 
			#               14 , 26 , 32 , 36 , 39 
			#          -->  21 , NA , 35 , NA , 39*
			# < 43>  -1     15 , 20 , 25 , 33 , 43 
			#                4 ,  5 , 31 , 35 , 43 
			#          -->  NA , NA , 37 , 37 , 43*
			# < 45>   0     11 , 17 , 21 , 26 , 36 , 45 
			#                4 ,  5 , 31 , 35 , 43 , 45 
			#          -->  NA , NA , 41 , 44 , NA , 45*
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
			# 842        1      3             [desc1   ]  3(?), 4, 5     col
			# 841        1      6       [desc1   ]  6(?),xx, 5,xx, 4     col
			# 828        1      4       [symm    ]  4(?), 4, 5, 4, 4     col
			# 829        2      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			# 8411       3      5       [desc1   ]  5(?),xx, 4,xx, 3     col
			# 8291       3      2 [desc1   ]  2(?),xx,xx, 1,xx,xx, 0     col
			# 8292       4      5 [same    ]  5(?), ., ., 5, ., ., 5     col
			# 8293       5      3 [same    ]  3(?), ., ., 3, ., ., 3     col
			# 8421       5      9      [seqReb  ]  9(?), 9, 3, 3,...     col
			# 8412       6      7       [desc1   ]  7(?),xx, 8,xx, 9     col
			# 1          4     10             [desc1   ] 10(?), 9, 8  Slide/
			# 11         3      7             [desc1   ]  7(?), 6, 5 Slide\\
			# 12         4      3             [desc1   ]  3(?), 2, 1 Slide\\
			# 13         5      4       [same    ]  4(?), ., 4, ., 4 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      841        3     16       [same    ] 16(?), .,16, .,16     col
			#      694        3      5 [sameEnd ]  5(?),xx,16,xx,16,xx, 5     col
			#      829        3      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			#      828        3     16       [symm    ] 16(?), 4,16, 4,16     col
			#      842        4      3             [same    ]  3(?), 3, 3     col
			#      8291       4      8          [sameEnd ]  8(?), 3, 3, 8     col
			#      8292       4      8 [same    ]  8(?), ., ., 8, ., ., 8     col
			#      8411       4      4       [desc1   ]  4(?),xx, 3,xx, 2     col
			#      1          2      5          [desc1   ]  5(?), 4, 3, 2  Slide/
			#      11         3      6             [same    ]  6(?), 6, 6 Slide\\
			#      12         4      5             [desc1   ]  5(?), 4, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    2(2)   3(5)   4(3)   5(4)   6(4)   8(4)   10(2)   16(2) 
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                      descript tgt.dir
			#      1        3     -8 [symm    ] -8(?),18,-17,18,-8     col
			#      2        4      3  [symm    ]  3(?), 6,-5, 6, 3     col
			#      3        6     -8  [desc1   ] -8(?),xx,-7,xx,-6     col
			#      E4       2     -4  [desc1   ] -4(?),xx,-5,xx,-6  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    -10(2)   -7(2)   -5(2)   1(3)   2(2)   3(2)   4(2)   6(5)   18(2) 
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
			#      3  5 14 20 42 44    | 2  9  6 22  2 |                        |2 1 1 0 2 |2 1 1 2
			#     12 14 15 24 27 32(1) | 2  1  9  3  5 |  9   9   1   4 -15 -12 |0 3 2 1 0 |3 2 1
			#      6 13 20 27 28 40(1) | 7  7  7  1 12 | -6  -1   5   3   1   8 |1 1 3 0 1 |1 1 3 1
			#      3  4  9 24 25 33    | 1  5 15  1  8 | -3  -9 -11  -3  -3  -7 |3 0 2 1 0 |3 2 1
			#      6  7 11 17 33 44(1) | 1  4  6 16 11 |  3   3   2  -7   8  11 |2 2 0 1 1 |2 2 1 1
			#      7  9 24 29 34 38(1) | 2 15  5  5  4 |  1   2  13  12   1  -6 |2 0 2 2 0 |2 2 2

			#   zoid width  ... 41   20   34   30   38   31 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  757        1      6       [same    ]  6(?), ., 6, ., 6     col
			#  581        1      3 [sameEnd ]  3(?),xx, 6,xx, 6,xx, 3     col
			#  710        1      3 [same    ]  3(?), ., ., 3, ., ., 3     col
			#  824        1      8             [desc1   ]  8(?), 7, 6     col
			#  7101       2      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			#  8241       5     35             [desc1   ] 35(?),34,33     col
			#  1          3     29      [seqReb  ] 29(?),29,33,33,...  Slide/

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
			#   dup number  3:2   6:2   7:2   9:2   14:2   20:2   24:3   27:2   33:2   44:2
			# <  3>   0      3 ,  5 , 14 , 20 , 42 , 44 
			#                3 ,  4 ,  9 , 24 , 25 , 33 
			#          -->   3*, NA ,  4 , 28 ,  8 , 22 
			# <  6>   0      6 , 13 , 20 , 27 , 28 , 40 
			#                6 ,  7 , 11 , 17 , 33 , 44 
			#          -->   6*, NA , NA ,  7 , 38 , NA 
			# <  7>  -1      7 , 11 , 17 , 33 , 44 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->   7*, NA , 31 , 25 , 24 
			# <  9>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->  NA ,  9*, 24!, 33 , 35!
			# < 14>  -1      5 , 14 , 20 , 42 , 44 
			#               12 , 14 , 15 , 24 , 27 
			#          -->  NA , 14*, NA , NA , NA 
			# < 20>  -1      5 , 14 , 20 , 42 , 44 
			#                6 , 13 , 20 , 27 , 28 
			#          -->   7!, 12!, 20*, NA , NA 
			# < 24>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  9 , 24 , 29 , 34 
			#          -->  10 ,  9!, 24*, 33 , 35!
			# < 27>  -1     14 , 15 , 24 , 27 , 32 
			#                6 , 13 , 20 , 27 , 28 
			#          -->  NA , 11 , 16 , 27*, NA 
			# < 33>  -1      4 ,  9 , 24 , 25 , 33 
			#                6 ,  7 , 11 , 17 , 33 
			#          -->   8 ,  5 , NA ,  9 , 33*
			# < 44>   0      3 ,  5 , 14 , 20 , 42 , 44 
			#                6 ,  7 , 11 , 17 , 33 , 44 
			#          -->   9 ,  9 ,  8 , 14 , 24 , 44*
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
			# 			     tgt.col banVal                           descript tgt.dir
			# 757        1      6       [same    ]  6(?), ., 6, ., 6     col
			# 581        1      3 [sameEnd ]  3(?),xx, 6,xx, 6,xx, 3     col
			# 710        1      3 [same    ]  3(?), ., ., 3, ., ., 3     col
			# 824        1      8             [desc1   ]  8(?), 7, 6     col
			# 7101       2      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			# 7571       3      2       [desc1   ]  2(?),xx, 1,xx, 0     col
			# 7572       4      7       [same    ]  7(?), ., 7, ., 7     col
			# 5811       4      0 [sameEnd ]  0(?),xx, 7,xx, 7,xx, 0     col
			# 8241       5      5             [desc1   ]  5(?), 4, 3     col
			# 7102       6      2 [desc1   ]  2(?),xx,xx, 3,xx,xx, 4     col
			# 1          3      9      [seqReb  ]  9(?), 9, 3, 3,...  Slide/
			# 11         4      4             [same    ]  4(?), 4, 4  Slide/
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
			#          tgt.col banVal                      descript tgt.dir
			#      824        1      3        [desc1   ]  3(?), 2, 1     col
			#      8241       1      2 [seqReb  ]  2(?), 2, 1, 1,...     col
			#      8242       3      4        [desc1   ]  4(?), 5, 6     col
			#      757        3      5  [desc1   ]  5(?),xx, 6,xx, 7     col
			#      7571       5     10  [desc1   ] 10(?),xx,11,xx,12     col
			#      1          4      6        [desc1   ]  6(?), 5, 4 Slide\\
			#      11         5      4        [desc1   ]  4(?), 5, 6 Slide\\
			#      12         5      5  [desc1   ]  5(?),xx, 6,xx, 7 Slide\\
			#      13         5      7  [symm    ]  7(?), 5, 6, 5, 7 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(4)   4(2)   5(4)   6(2)   7(3)   9(2)   15(2) 
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
			#         tgt.col banVal                     descript tgt.dir
			#      1         2      1       [desc1   ]  1(?), 2, 3     col
			#      E2        1      2       [same    ]  2(?), 2, 2  Slide/
			#      E4        1     -3    [sameEnd ] -3(?), 2, 2,-3  Slide/
			#      E3        1      3 [desc1   ]  3(?),xx, 2,xx, 1  Slide/
			#      E21       3      1       [desc1   ]  1(?), 2, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -7(2)   -6(2)   -3(3)   1(4)   2(2)   3(3)   8(2)   9(2) 
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
			#      4 20 26 28 35 40    |16  6  2  7  5 |                        |1 0 3 1 1 |1 3 1 1
			#      1  4 20 23 29 45(2) | 3 16  3  6 16 | -3 -16  -6  -5  -6   5 |2 0 3 0 1 |2 3 1
			#      7 37 38 39 40 44    |30  1  1  1  4 |  6  33  18  16  11  -1 |1 0 0 3 2 |1 3 2
			#      2  3 12 20 27 38(1) | 1  9  8  7 11 | -5 -34 -26 -19 -13  -6 |2 1 2 1 0 |2 1 2 1
			#     10 15 18 21 34 41    | 5  3  3 13  7 |  8  12   6   1   7   3 |0 3 1 1 1 |3 1 1 1
			#      5 11 14 30 33 38    | 6  3 16  3  5 | -5  -4  -4   9  -1  -3 |1 2 0 3 0 |1 2 3

			#   zoid width  ... 36   44   37   36   31   33 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                            descript tgt.dir
			#  744       1     13 [desc(-3) ] 13(?),xx,10,xx, 7,xx, 4     col
			#  841       5     32              [desc1   ] 32(?),33,34     col
			#  655       6     44        [symm    ] 44(?),38,41,38,44     col
			#  1         3     26          [desc( 4) ] 26(?),30,34,38  Slide/
			#  11        3     12              [desc1   ] 12(?),11,10 Slide\\
			#  12        4     13              [desc1   ] 13(?),14,15 Slide\\

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
			#   dup number  4:2   20:3   38:3   40:2
			# <  4>   1      4 , 20 , 26 , 28 , 35 
			#                4 , 20 , 23 , 29 , 45 
			#          -->   4*, 20!, 20 , 30!, NA 
			# < 20>   1      1 ,  4 , 20 , 23 , 29 
			#                3 , 12 , 20 , 27 , 38 
			#          -->   5 , NA , 20*, 31 , NA 
			# < 38>   0      2 ,  3 , 12 , 20 , 27 , 38 
			#                5 , 11 , 14 , 30 , 33 , 38 
			#          -->   8 , 19 , 16 , NA , NA , 38*
			# < 40>  -1     20 , 26 , 28 , 35 , 40 
			#                7 , 37 , 38 , 39 , 40 
			#          -->  NA , NA , NA , NA , 40*
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
			# 744        3      8       [same    ]  8(?), ., 8, ., 8     col
			# 557        3      6 [sameEnd ]  6(?),xx, 8,xx, 8,xx, 6     col
			# 841        4     -1             [desc1   ] -1(?), 0, 1     col
			# 655        4      9       [symm    ]  9(?), 0, 1, 0, 9     col
			# 8411       5      2             [desc1   ]  2(?), 3, 4     col
			# 6551       6      4       [symm    ]  4(?), 8, 1, 8, 4     col
			# 1          3     -4         [desc( 4) ] -4(?), 0, 4, 8  Slide/
			# 11         3      2             [desc1   ]  2(?), 1, 0 Slide\\
			# 12         4      3             [desc1   ]  3(?), 4, 5 Slide\\
			# 13         5      9       [desc1   ]  9(?),xx, 8,xx, 7 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      841        1      7             [desc1   ]  7(?), 6, 5     col
			#      8411       2      3             [same    ]  3(?), 3, 3     col
			#      661        2      9          [sameEnd ]  9(?), 3, 3, 9     col
			#      6611       4      7 [same    ]  7(?), ., ., 7, ., ., 7     col
			#      1          1      3             [same    ]  3(?), 3, 3  Slide/
			#      11         1      7          [sameEnd ]  7(?), 3, 3, 7  Slide/
			#      12         1      2       [desc1   ]  2(?),xx, 3,xx, 4  Slide/
			#      13         5      3             [same    ]  3(?), 3, 3 Slide\\
			#      14         5      9          [sameEnd ]  9(?), 3, 3, 9 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   3(6)   5(3)   6(3)   7(3)   16(4) 
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                     descript tgt.dir
			#      1        1      6 [symm    ]  6(?),-5, 8,-5, 6     col
			#      E3       5      6 [same    ]  6(?), ., 6, ., 6 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -6(3)   -5(3)   -4(2)   -3(2)   -1(2)   6(2) 
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
			#      2  8 15 22 25 41    | 6  7  7  3 16 |                        |2 1 2 0 1 |2 1 2 1
			#      2 22 27 33 36 37(2) |20  5  6  3  1 |  0  14  12  11  11  -4 |1 0 2 3 0 |1 2 3
			#     11 18 21 36 37 43(2) | 7  3 15  1  6 |  9  -4  -6   3   1   6 |0 2 1 2 1 |2 1 2 1
			#      3 10 23 24 31 39    | 7 13  1  7  8 | -8  -8   2 -12  -6  -4 |1 1 2 2 0 |1 1 2 2
			#      6 10 18 25 34 35(1) | 4  8  7  9  1 |  3   0  -5   1   3  -4 |1 2 1 2 0 |1 2 1 2
			#     16 25 33 38 40 45(1) | 9  8  5  2  5 | 10  15  15  13   6  10 |0 1 1 2 2 |1 1 2 2

			#   zoid width  ... 39   35   32   36   29   29 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                           descript tgt.dir
			#  784       1      4 [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			#  819       2     25      [seqReb  ] 25(?),25,10,10,...     col

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
			#   dup number  2:2   10:2   18:2   22:2   25:3   33:2   36:2   37:2
			# <  2>   0      2 ,  8 , 15 , 22 , 25 , 41 
			#                2 , 22 , 27 , 33 , 36 , 37 
			#          -->   2*, 36 , 39 , 44 , NA , 33 
			# < 10>   0      3 , 10 , 23 , 24 , 31 , 39 
			#                6 , 10 , 18 , 25 , 34 , 35 
			#          -->   9 , 10*, 13 , 26!, 37 , 31 
			# < 18>   1     11 , 18 , 21 , 36 , 37 
			#               10 , 18 , 25 , 34 , 35 
			#          -->   9!, 18*, 29 , 32 , 33 
			# < 22>  -2     15 , 22 , 25 , 41 
			#                2 , 22 , 27 , 33 
			#          -->  NA , 22*, 29 , 25 
			# < 25>  -2     18 , 25 , 34 , 35 
			#               16 , 25 , 33 , 38 
			#          -->  14 , 25*, 32!, 41 
			# < 33>  -1     22 , 27 , 33 , 36 , 37 
			#               16 , 25 , 33 , 38 , 40 
			#          -->  10 , 23 , 33*, 40 , 43 
			# < 36>  -1     22 , 27 , 33 , 36 , 37 
			#               11 , 18 , 21 , 36 , 37 
			#          -->  NA ,  9 ,  9 , 36*, 37!
			# < 37>  -1     22 , 27 , 33 , 36 , 37 
			#               11 , 18 , 21 , 36 , 37 
			#          -->  NA ,  9 ,  9 , 36!, 37*
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
			# 819        1      6                 [same    ]  6(?), 6, 6     col
			# 784        1      3              [sameEnd ]  3(?), 6, 6, 3     col
			# 7841       1      4     [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			# 8191       2      5          [seqReb  ]  5(?), 5, 0, 0,...     col
			# 807        2      0 [seqReb  ]  0(?), ., 0, ., 8, ., 8,...     col
			# 641        3      1           [symm    ]  1(?), 3, 8, 3, 1     col
			# 8071       4      4           [desc1   ]  4(?),xx, 5,xx, 6     col
			# 8192       6      5                 [same    ]  5(?), 5, 5     col
			# 7842       6      9              [sameEnd ]  9(?), 5, 5, 9     col
			# 8072       6      7    [desc(-2) ]  7(?),xx, 5,xx, 3,xx, 1     col
			# 1          1      9           [desc1   ]  9(?),xx, 8,xx, 7  Slide/
			# 11         3      4                 [desc1   ]  4(?), 5, 6 Slide\\
			# 12         5      8                 [same    ]  8(?), 8, 8 Slide\\
			# 13         5      0              [sameEnd ]  0(?), 8, 8, 0 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      784        1      8 [desc1   ]  8(?),xx,xx, 7,xx,xx, 6     col
			#      819        2      8             [same    ]  8(?), 8, 8     col
			#      7841       2     13          [sameEnd ] 13(?), 8, 8,13     col
			#      1          1      9             [desc1   ]  9(?), 8, 7  Slide/
			#      11         1      8       [desc1   ]  8(?),xx, 7,xx, 6  Slide/
			#      12         1      8      [seqReb  ]  8(?), 8, 7, 7,...  Slide/
			#      13         3      3             [desc1   ]  3(?), 2, 1  Slide/
			#      14         5      7       [same    ]  7(?), ., 7, ., 7 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   3(3)   5(3)   6(3)   7(6)   8(3)   9(2) 
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                      descript tgt.dir
			#      1       3     -4  [desc1   ] -4(?),xx,-5,xx,-6     col
			#      2       6     10 [seqReb  ] 10(?),10,-4,-4,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    -8(2)   -6(2)   -4(4)   0(2)   1(2)   3(3)   6(2)   10(2)   11(2)   15(2) 
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
			#      2  3 11 26 37 43    | 1  8 15 11  6 |                        |2 1 1 1 1 |2 1 1 1 1
			#     17 20 29 35 38 44    | 3  9  6  3  6 | 15  17  18   9   1   1 |0 1 2 2 1 |1 2 2 1
			#      4  9 13 18 21 34    | 5  4  5  3 13 |-13 -11 -16 -17 -17 -10 |2 2 1 1 0 |2 2 1 1
			#      4 17 30 32 33 34(2) |13 13  2  1  1 |  0   8  17  14  12   0 |1 1 0 4 0 |1 1 4
			#     11 13 15 17 25 34(2) | 2  2  2  8  9 |  7  -4 -15 -15  -8   0 |0 4 1 1 0 |4 1 1
			#      1  8  9 17 29 32(1) | 7  1  8 12  3 |-10  -5  -6   0   4  -2 |3 1 1 1 0 |3 1 1 1
			
			#   zoid width  ... 41   27   30   30   23   31 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                            descript tgt.dir
			#  374        3     17 [desc(-2) ] 17(?),xx,15,xx,13,xx,11     col
			#  388        4     17              [same    ] 17(?),17,17     col
			#  176        4     32           [sameEnd ] 32(?),17,17,32     col
			#  3741       4     16        [desc1   ] 16(?),xx,17,xx,18     col
			#  3742       6     34        [same    ] 34(?), .,34, .,34     col
			#  35         6     43  [sameEnd ] 43(?),xx,34,xx,34,xx,43     col
			#  3881       6     32       [seqReb  ] 32(?),32,34,34,...     col
			#  1          5      4        [symm    ]  4(?),17,15,17, 4 Slide\\

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
			#   dup number  4:2   9:2   11:2   13:2   17:4   29:2   32:2   34:3
			# <  4>   0      4 ,  9 , 13 , 18 , 21 , 34 
			#                4 , 17 , 30 , 32 , 33 , 34 
			#          -->   4*, 25 , NA , NA , 45 , 34!
			# <  9>   1      4 ,  9 , 13 , 18 , 21 
			#                8 ,  9 , 17 , 29 , 32 
			#          -->  NA ,  9*, 21 , 40 , 43 
			# < 11>  -2     11 , 26 , 37 , 43 
			#               11 , 13 , 15 , 17 
			#          -->  11*, NA , NA , NA 
			# < 13>  -1      9 , 13 , 18 , 21 , 34 
			#               11 , 13 , 15 , 17 , 25 
			#          -->  NA , 13*, NA , NA , 16 
			# < 17>   0     11 , 13 , 15 , 17 , 25 , 34 
			#                1 ,  8 ,  9 , 17 , 29 , 32 
			#          -->  NA ,  3 ,  3 , 17*, 33 , 30 
			# < 29>   2     17 , 20 , 29 , 35 
			#                9 , 17 , 29 , 32 
			#          -->   1 , 14 , 29*, NA 
			# < 32>   2      4 , 17 , 30 , 32 
			#                9 , 17 , 29 , 32 
			#          -->  14 , 17!, 28!, 32*
			# < 34>   0      4 , 17 , 30 , 32 , 33 , 34 
			#               11 , 13 , 15 , 17 , 25 , 34 
			#          -->  18 ,  9 , NA ,  2 , 17 , 34*
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
			# 388        1      1              [same    ]  1(?), 1, 1     col
			# 176        1      4           [sameEnd ]  4(?), 1, 1, 4     col
			# 1761       3     -1  [desc1   ] -1(?),xx,xx, 0,xx,xx, 1     col
			# 374        3      7 [desc(-2) ]  7(?),xx, 5,xx, 3,xx, 1     col
			# 3881       4      7              [same    ]  7(?), 7, 7     col
			# 1762       4      2           [sameEnd ]  2(?), 7, 7, 2     col
			# 3741       4      6        [desc1   ]  6(?),xx, 7,xx, 8     col
			# 3742       6      4        [same    ]  4(?), ., 4, ., 4     col
			# 35         6      3  [sameEnd ]  3(?),xx, 4,xx, 4,xx, 3     col
			# 1763       6      5  [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			# 3882       6      2       [seqReb  ]  2(?), 2, 4, 4,...     col
			# 1          1     11          [desc(-3) ] 11(?), 8, 5, 2  Slide/
			# 11         5      6        [desc1   ]  6(?),xx, 5,xx, 4 Slide\\
			# 12         5      4        [symm    ]  4(?), 7, 5, 7, 4 Slide\\
			# 13         6      0    [ptnReb   ]  0(?), 9, 7, 0, 9, 7 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                      descript tgt.dir
			#      3881       3      8 [seqReb  ]  8(?), 8, 2, 2,...     col
			#      11         1     13  [symm    ] 13(?), 1, 2, 1,13  Slide/
			#      12         2      8        [same    ]  8(?), 8, 8  Slide/
			#      13         2      1     [sameEnd ]  1(?), 8, 8, 1  Slide/
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   2(4)   3(4)   5(2)   6(3)   8(3)   9(2)   13(3) 
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                        descript tgt.dir
			#      1       2     -6          [desc1   ] -6(?),-5,-4     col
			#      2       3    -14 [desc1   ] -14(?),xx,-15,xx,-16     col
			#      3       6     -2   [seqReb  ] -2(?),-2, 0, 0,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    -17(2)   -15(2)   -10(2)   0(4)   1(2)   17(2) 
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
			#      9 26 35 37 40 42    |17  9  2  3  2 |                        |1 0 1 2 2 |1 1 2 2
			#      7 20 22 27 40 43(1) |13  2  5 13  3 | -2  -6 -13 -10   0   1 |1 0 3 0 2 |1 3 2
			#      8 14 18 30 31 44    | 6  4 12  1 13 |  1  -6  -4   3  -9   1 |1 2 0 2 1 |1 2 2 1
			#      5 27 31 34 35 43(1) |22  4  3  1  8 | -3  13  13   4   4  -1 |1 0 1 3 1 |1 1 3 1
			#      3  7 14 23 26 42    | 4  7  9  3 16 | -2 -20 -17 -11  -9  -1 |2 1 2 0 1 |2 1 2 1
			#      6 16 37 38 41 45    |10 21  1  3  4 |  3   9  23  15  15   3 |1 1 0 2 2 |1 1 2 2

			#   zoid width  ... 33   36   36   38   39   39 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                            descript tgt.dir
			#  508        2     28  [desc1   ] 28(?),xx,xx,27,xx,xx,26     col
			#  511        4     16 [desc( 7) ] 16(?),xx,23,xx,30,xx,37     col
			#  5081       6     44  [desc1   ] 44(?),xx,xx,43,xx,xx,42     col
			#  1          4     40              [desc1   ] 40(?),41,42  Slide/
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
			#   dup number  7:2   14:2   26:2   27:2   31:2   35:2   37:2   40:2   42:2   43:2
			# <  7>   1      7 , 20 , 22 , 27 , 40 
			#                7 , 14 , 23 , 26 , 42 
			#          -->   7*,  8 , 24!, 25!, 44 
			# < 14>   1      8 , 14 , 18 , 30 , 31 
			#                7 , 14 , 23 , 26 , 42 
			#          -->   6!, 14*, 28 , 22 , NA 
			# < 26>   3      9 , 26 , 35 
			#               23 , 26 , 42 
			#          -->  NA , 26*, NA 
			# < 27>  -2     22 , 27 , 40 , 43 
			#                5 , 27 , 31 , 34 
			#          -->  NA , 27*, NA , NA 
			# < 31>  -2     18 , 30 , 31 , 44 
			#                5 , 27 , 31 , 34 
			#          -->  NA , 24 , 31*, NA 
			# < 35>   2      9 , 26 , 35 , 37 
			#               31 , 34 , 35 , 43 
			#          -->  NA , NA , 35*, NA 
			# < 37>  -1     26 , 35 , 37 , 40 , 42 
			#                6 , 16 , 37 , 38 , 41 
			#          -->  NA , NA , 37*, NA , 40!
			# < 40>   0      9 , 26 , 35 , 37 , 40 , 42 
			#                7 , 20 , 22 , 27 , 40 , 43 
			#          -->   5 , 14 ,  9 , 17 , 40*, 44!
			# < 42>   0      9 , 26 , 35 , 37 , 40 , 42 
			#                3 ,  7 , 14 , 23 , 26 , 42 
			#          -->  NA , NA , NA ,  9 , 12 , 42*
			# < 43>   0      7 , 20 , 22 , 27 , 40 , 43 
			#                5 , 27 , 31 , 34 , 35 , 43 
			#          -->   3 , 34 , 40 , 41 , 30 , 43*
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
			# 734        2      5             [desc1   ]  5(?), 6, 7     col
			# 508        2      8 [desc1   ]  8(?),xx,xx, 7,xx,xx, 6     col
			# 7341       2      6      [seqReb  ]  6(?), 6, 7, 7,...     col
			# 7342       3     10         [desc(-3) ] 10(?), 7, 4, 1     col
			# 5081       6      4 [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			# 1          1      6      [seqReb  ]  6(?), 6, 4, 4,...  Slide/
			# 11         2      2       [desc1   ]  2(?),xx, 3,xx, 4  Slide/
			# 12         4      0             [desc1   ]  0(?), 1, 2  Slide/
			# 13         4      7             [same    ]  7(?), 7, 7 Slide\\
			# 14         4      5          [sameEnd ]  5(?), 7, 7, 5 Slide\\
			# 15         6      2       [desc1   ]  2(?),xx, 3,xx, 4 Slide\\
			# 16         6      4       [symm    ]  4(?), 1, 3, 1, 4 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      508        3      4 [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			#      734        4      3             [same    ]  3(?), 3, 3     col
			#      5081       4      1          [sameEnd ]  1(?), 3, 3, 1     col
			# -------------------------------------------------------------------------------------
			#     FV :    1(3)   2(3)   3(5)   4(4)   9(2)   13(3)
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
			# -------------------------------------------------------------------------------------
			#       tgt.col banVal                      descript tgt.dir
			#      1       5     -9  [same    ] -9(?), .,-9, .,-9     col
			#      2       6      3 [seqReb  ]  3(?), 3,-1,-1,...     col
			# -------------------------------------------------------------------------------------
			#     FV :    -9(2)   -6(2)   -2(2)   -1(2)   1(3)   3(3)   4(2)   13(2)   15(2) 
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
			#      4  5  6 12 25 37    | 1  1  6 13 12 |                        |3 1 1 1 0 |3 1 1 1
			#      3  4  9 24 25 33(2) | 1  5 15  1  8 | -1  -1   3  12   0  -4 |3 0 2 1 0 |3 2 1
			#     15 19 21 34 41 44    | 4  2 13  7  3 | 12  15  12  10  16  11 |0 2 1 1 2 |2 1 1 2
			#      7  9 12 14 23 28    | 2  3  2  9  5 | -8 -10  -9 -20 -18 -16 |2 2 2 0 0 |2 2 2
			#      6 12 19 24 34 41(1) | 6  7  5 10  7 | -1   3   7  10  11  13 |1 2 1 1 1 |1 2 1 1 1
			#      5 22 31 32 39 45    |17  9  1  7  6 | -1  10  12   8   5   4 |1 0 1 3 1 |1 1 3 1
			
			#   zoid width  ... 33   30   29   21   35   40 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#     tgt.col banVal                  descript tgt.dir
			#  797       1      4 [desc1   ]  4(?), 5, 6, 7     col
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
			#   dup number  4:2   5:2   6:2   9:2   12:3   19:2   24:2   25:2   34:2   41:2
			# <  4>   1      4 ,  5 ,  6 , 12 , 25 
			#                4 ,  9 , 24 , 25 , 33 
			#          -->   4*, 13 , 42 , 38 , 41 
			# <  5>  -1      5 ,  6 , 12 , 25 , 37 
			#                5 , 22 , 31 , 32 , 39 
			#          -->   5*, 38 , NA , 39 , 41 
			# <  6>  -2      6 , 12 , 25 , 37 
			#                6 , 12 , 19 , 24 
			#          -->   6*, 12!, 13 , 11 
			# <  9>  -1      4 ,  9 , 24 , 25 , 33 
			#                7 ,  9 , 12 , 14 , 23 
			#          -->  NA ,  9*, NA , NA , 13 
			# < 12>  -1      9 , 12 , 14 , 23 , 28 
			#                6 , 12 , 19 , 24 , 34 
			#          -->   3 , 12*, 24 , 25!, 40 
			# < 19>   1     15 , 19 , 21 , 34 , 41 
			#               12 , 19 , 24 , 34 , 41 
			#          -->   9 , 19*, 27 , 34!, 41!
			# < 24>   0      3 ,  4 ,  9 , 24 , 25 , 33 
			#                6 , 12 , 19 , 24 , 34 , 41 
			#          -->   9 , 20 , NA , 24*, 43 , NA 
			# < 25>   0      4 ,  5 ,  6 , 12 , 25 , 37 
			#                3 ,  4 ,  9 , 24 , 25 , 33 
			#          -->   2!,  3!, 12 , NA , 25*, 29 
			# < 34>   1     15 , 19 , 21 , 34 , 41 
			#               12 , 19 , 24 , 34 , 41 
			#          -->   9 , 19!, 27 , 34*, 41!
			# < 41>   1     15 , 19 , 21 , 34 , 41 
			#               12 , 19 , 24 , 34 , 41 
			#          -->   9 , 19!, 27 , 34!, 41*
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
			# 797        1      4           [desc1   ]  4(?), 5, 6, 7     col
			# 779        1      7  [desc1   ]  7(?),xx, 6,xx, 5,xx, 4     col
			# 7971       2      2              [same    ]  2(?), 2, 2     col
			# 747        2      9           [sameEnd ]  9(?), 2, 2, 9     col
			# 7471       3      2    [ptnReb   ]  2(?), 1, 9, 2, 1, 9     col
			# 7791       4      4        [same    ]  4(?), ., 4, ., 4     col
			# 678        4      2  [sameEnd ]  2(?),xx, 4,xx, 4,xx, 2     col
			# 7972       4      2       [seqReb  ]  2(?), 2, 4, 4,...     col
			# 7472       6      9  [desc1   ]  9(?),xx,xx, 8,xx,xx, 7     col
			# 7792       6     -2 [desc( 3) ] -2(?),xx, 1,xx, 4,xx, 7     col
			# 1          2      4        [same    ]  4(?), ., 4, ., 4  Slide/
			# 11         4      0              [desc1   ]  0(?), 1, 2 Slide\\
			# 12         5      2       [seqReb  ]  2(?), 2, 9, 9,... Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                           descript tgt.dir
			#      747       1      3 [desc1   ]  3(?),xx,xx, 2,xx,xx, 1     col
			#      797       5      5             [desc1   ]  5(?), 6, 7     col
			#      1         1      3       [symm    ]  3(?), 9, 5, 9, 3  Slide/
			#      11        3      7             [same    ]  7(?), 7, 7  Slide/
			#      12        5      6       [desc1   ]  6(?),xx, 5,xx, 4 Slide\\
			#      13        5      9         [desc(-2) ]  9(?), 7, 5, 3 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(5)   2(3)   3(2)   5(3)   6(3)   7(4)   9(2)   13(2) 
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                     descript tgt.dir
			#      1         1     -1       [same    ] -1(?),-1,-1     col
			#      2         1     -8    [sameEnd ] -8(?),-1,-1,-8     col
			#      3         4     10 [same    ] 10(?), .,10, .,10     col
			#      E4        2      9 [desc1   ]  9(?),xx,10,xx,11  Slide/
			#      E41       5      9       [desc1   ]  9(?), 8, 7 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -1(4)   3(2)   10(3)   11(2)   12(4) 
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
			#      9 33 36 40 42 43    |24  3  4  2  1 |                        |1 0 0 2 3 |1 2 3
			#      5 15 20 31 34 42(1) |10  5 11  3  8 | -4 -18 -16  -9  -8  -1 |1 1 1 2 1 |1 1 1 2 1
			#      6 10 17 18 21 29    | 4  7  1  3  8 |  1  -5  -3 -13 -13 -13 |1 3 2 0 0 |1 3 2
			#      2 10 14 22 32 36(1) | 8  4  8 10  4 | -4   0  -3   4  11   7 |1 2 1 2 0 |1 2 1 2
			#      8 11 19 21 36 45(1) | 3  8  2 15  9 |  6   1   5  -1   4   9 |1 2 1 1 1 |1 2 1 1 1
			#      7  9 24 29 34 38    | 2 15  5  5  4 | -1  -2   5   8  -2  -7 |2 0 2 2 0 |2 2 2

			#   zoid width  ... 34   37   23   34   37   31 and ?
			#        Quo10 pattern rebind table 
			#        none:100.0%(6/6) 
			#      tgt.col banVal                      descript tgt.dir
			#  824        1      6        [desc1   ]  6(?), 7, 8     col
			#  811        2     12  [desc1   ] 12(?),xx,11,xx,10     col
			#  8241       3     29    [desc(-5) ] 29(?),24,19,14     col
			#  1          3     29 [seqReb  ] 29(?),29,36,36,...  Slide/
			#  11         3     10        [desc1   ] 10(?), 9, 8 Slide\\

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
			#   dup number  9:2   10:2   21:2   29:2   34:2   36:3   42:2
			# <  9>   1      9 , 33 , 36 , 40 , 42 
			#                9 , 24 , 29 , 34 , 38 
			#          -->   9*, 15 , 22 , 28 , 34 
			# < 10>   0      6 , 10 , 17 , 18 , 21 , 29 
			#                2 , 10 , 14 , 22 , 32 , 36 
			#          -->  NA , 10*, 11 , 26 , 43 , 43 
			# < 21>  -1     10 , 17 , 18 , 21 , 29 
			#                8 , 11 , 19 , 21 , 36 
			#          -->   6 ,  5 , 20!, 21*, 43 
			# < 29>  -2     17 , 18 , 21 , 29 
			#                7 ,  9 , 24 , 29 
			#          -->  NA , NA , 27 , 29*
			# < 34>   0      5 , 15 , 20 , 31 , 34 , 42 
			#                7 ,  9 , 24 , 29 , 34 , 38 
			#          -->   9 ,  3 , 28 , 27 , 34*, NA 
			# < 36>  -1     10 , 14 , 22 , 32 , 36 
			#                8 , 11 , 19 , 21 , 36 
			#          -->   6 ,  8 , 16 , 10 , 36*
			# < 42>   1      9 , 33 , 36 , 40 , 42 
			#               15 , 20 , 31 , 34 , 42 
			#          -->  21 ,  7 , 26 , 28 , 42*
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
			#     tgt.col banVal                           descript tgt.dir
			# 824       1      6             [desc1   ]  6(?), 7, 8     col
			# 811       2      2       [desc1   ]  2(?),xx, 1,xx, 0     col
			# 771       3      7       [symm    ]  7(?), 4, 9, 4, 7     col
			# 798       5      2 [same    ]  2(?), ., ., 2, ., ., 2     col
			# 1         1      9             [same    ]  9(?), 9, 9  Slide/
			# 11        1      2          [sameEnd ]  2(?), 9, 9, 2  Slide/
			# 12        3      9      [seqReb  ]  9(?), 9, 6, 6,...  Slide/
			# 13        4      3             [desc1   ]  3(?), 4, 5  Slide/
			# 14        3     10             [desc1   ] 10(?), 9, 8 Slide\\
			# 15        5      9             [same    ]  9(?), 9, 9 Slide\\
			# 16        5      0          [sameEnd ]  0(?), 9, 9, 0 Slide\\
			# 17        6      2       [desc1   ]  2(?),xx, 1,xx, 0 Slide\\
			# 18        6      0       [symm    ]  0(?), 4, 1, 4, 0 Slide\\
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
			# -------------------------------------------------------------------------------------
			#          tgt.col banVal                           descript tgt.dir
			#      824        1      1             [desc1   ]  1(?), 2, 3     col
			#      811        1      2       [desc1   ]  2(?),xx, 3,xx, 4     col
			#      8111       2      9       [desc1   ]  9(?),xx, 8,xx, 7     col
			#      798        2      5 [desc1   ]  5(?),xx,xx, 4,xx,xx, 3     col
			#      8112       3      3       [desc1   ]  3(?),xx, 2,xx, 1     col
			#      8113       5     10       [desc1   ] 10(?),xx, 9,xx, 8     col
			#      771        5      8       [symm    ]  8(?), 4, 9, 4, 8     col
			#      1          4      5      [seqReb  ]  5(?), 5, 8, 8,... Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(2)   2(3)   3(4)   4(5)   5(3)   8(5)   10(2)   15(2) 
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                  descript tgt.dir
			#      1        3      5    [same    ]  5(?), 5, 5     col
			#      2        3     -3 [sameEnd ] -3(?), 5, 5,-3     col
			#      E5       6     -3    [desc1   ] -3(?),-2,-1 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -13(3)   -4(2)   -3(2)   -2(2)   -1(3)   1(2)   4(2)   5(2) 
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
			#     11 12 29 33 38 42    | 1 17  4  5  4 |                        |0 2 1 2 1 |2 1 2 1
			#      8  9 18 21 28 40    | 1  9  3  7 12 | -3  -3 -11 -12 -10  -2 |2 1 2 0 1 |2 1 2 1
			#      2  7 19 25 29 36    | 5 12  6  4  7 | -6  -2   1   4   1  -4 |2 1 2 1 0 |2 1 2 1
			#      5  9 14 26 30 43    | 4  5 12  4 13 |  3   2  -5   1   1   7 |2 1 1 1 1 |2 1 1 1 1
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   3  -1  -2  -1   1 |1 2 2 0 1 |1 2 2 1
			#      6  8 18 35 42 43    | 2 10 17  7  1 |  5  -4   5  11  13  -1 |2 1 0 1 2 |2 1 1 2
			
			#   zoid width  ... 31   32   34   38   43   37 and ?
			#        Quo10 pattern rebind table 
			#        none:66.7%(4/6)   match:33.3%(2/6) 
			#      tgt.col banVal                           descript tgt.dir
			#  821        1      0       [desc1   ]  0(?),xx, 1,xx, 2     col
			#  8211       4     23       [desc1   ] 23(?),xx,24,xx,25     col
			#  8212       5     29       [same    ] 29(?), .,29, .,29     col
			#  775        5     38 [sameEnd ] 38(?),xx,29,xx,29,xx,38     col
			#  834        6     42             [desc1   ] 42(?),43,44     col
			#  803        6     44 [desc1   ] 44(?),xx,xx,43,xx,xx,42     col
			#  792        6     36       [symm    ] 36(?),43,44,43,36     col
			#  1          2     12      [desc( 6) ] 12(?),18,24,30,36  Slide/

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
			#   dup number  8:2   9:2   12:2   18:2   29:3   42:2   43:2
			# <  8>   1      8 ,  9 , 18 , 21 , 28 
			#                8 , 18 , 35 , 42 , 43 
			#          -->   8*, 27 , NA , NA , NA 
			# <  9>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                5 ,  9 , 14 , 26 , 30 , 43 
			#          -->   2 ,  9*, 10 , 31 , 32 , NA 
			# < 12>   0     11 , 12 , 29 , 33 , 38 , 42 
			#                1 , 12 , 13 , 24 , 29 , 44 
			#          -->  NA , 12*, NA , 15 , 20 , NA 
			# < 18>   0      8 ,  9 , 18 , 21 , 28 , 40 
			#                6 ,  8 , 18 , 35 , 42 , 43 
			#          -->   4 ,  7!, 18*, NA , NA , NA 
			# < 29>   0      2 ,  7 , 19 , 25 , 29 , 36 
			#                1 , 12 , 13 , 24 , 29 , 44 
			#          -->  NA , 17 ,  7 , 23!, 29*, NA 
			# < 42>  -1     12 , 29 , 33 , 38 , 42 
			#                6 ,  8 , 18 , 35 , 42 
			#          -->  NA , NA ,  3 , 32 , 42*
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
			# 821        1      0           [desc1   ]  0(?),xx, 1,xx, 2     col
			# 8211       3      3 [seqReb  ]  3(?), ., 3, ., 9, ., 9,...     col
			# 834        4      6                 [desc1   ]  6(?), 5, 4     col
			# 8212       4      3           [desc1   ]  3(?),xx, 4,xx, 5     col
			# 8213       5      9           [same    ]  9(?), ., 9, ., 9     col
			# 775        5      8     [sameEnd ]  8(?),xx, 9,xx, 9,xx, 8     col
			# 8341       6      2                 [desc1   ]  2(?), 3, 4     col
			# 803        6      4     [desc1   ]  4(?),xx,xx, 3,xx,xx, 2     col
			# 792        6      6           [symm    ]  6(?), 3, 4, 3, 6     col
			# 1          2     12             [desc(-4) ] 12(?), 8, 4, 0  Slide/
			# 11         5      4           [desc1   ]  4(?),xx, 3,xx, 2 Slide\\
			# 12         6      2          [seqReb  ]  2(?), 2, 4, 4,... Slide\\
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
			# -------------------------------------------------------------------------------------
			#         tgt.col banVal                           descript tgt.dir
			#      821       4      6       [desc1   ]  6(?),xx, 5,xx, 4     col
			#      803       4      3 [desc1   ]  3(?),xx,xx, 4,xx,xx, 5     col
			#      1         1      9             [desc1   ]  9(?),10,11  Slide/
			#      11        3      9             [desc1   ]  9(?),10,11 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    1(4)   4(5)   5(4)   7(3)   11(2)   12(3)   17(2) 
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
			# -------------------------------------------------------------------------------------
			#        tgt.col banVal                      descript tgt.dir
			#      E2       3     -4        [same    ] -4(?),-4,-4 Slide\\
			#      E3       4      5 [seqReb  ]  5(?), 5, 3, 3,... Slide\\
			#      E4       6     -2  [same    ] -2(?), .,-2, .,-2 Slide\\
			# -------------------------------------------------------------------------------------
			#     FV :    -4(3)   -3(2)   -2(3)   -1(3)   1(5)   3(2)   5(2) 
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

		# fCutCnt.basic()
		# 		840  2  4 11 28 29 43
		# 		841  5 11 14 30 33 38
		if( any(aZoid[1:5]==stdMI$lastZoid[2:6]) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 8,18,17,32,33,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
			# 동일 증감이... basic은 없겠지만 다른 ph는 유효할 듯.
		if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
		#      2 21 28 38 42 45(2) |19  7 10  4  3 |  1  17  18  26  14   0 |1 0 2 1 2 |1 2 1 2
		#      3 10 16 19 31 39    | 7  6  3 12  8 |  1 -11 -12 -19 -11  -6 |1 3 0 2 0 |1 3 2
		if( 2<sum(aZoid==c( 4,NA, 4,NA,20,33),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( all(aZoid==c( 4,NA,NA,NA,NA,39),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 너무 이쁘장한 패턴.

		# fCutCnt.nextQuo10
			#      6 10 18 25 34 35    | 4  8  7  9  1 |  5   7  10  13  -8  -8 |1 2 1 2 0 |1 2 1 2
			#      3  9 11 12 13 19    | 6  2  1  1  6 | -3  -1  -7 -13 -21 -16 |2 4 0 0 0 |2 4
		if( 2<sum(aZoid==c(NA, 8, 4,NA,NA,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 1<sum( aZoid[ 1 ]*c(3,4)==aZoid[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid==c(NA,NA,NA,NA,13,19),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# rebind

		# fCutCnt.nextBin
			#     10 14 19 39 40 43    | 4  5 20  1  3 |  5   7  10  28   8   8 |0 3 0 1 2 |3 1 2
			#      3 10 13 22 31 32(1) | 7  3  9  9  1 | -7  -4  -6 -17  -9 -11 |1 2 1 2 0 |1 2 1 2
		if( 2<sum(aZoid==c(NA, 6, 7, 6,22,21),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 2<sum(aZoid[2:6]==c( 3,10,13,22,31,32)[1:5]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextRebNum
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -7 -10  -8   3  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      4  5 31 35 43 45(2) | 1 26  4  8  2 |  0  -2  18   6  12   6 |2 0 0 2 2 |2 2 2
			#      5 11 14 30 33 38(1) | 6  3 16  3  5 |  1   6 -17  -5 -10  -7 |1 2 0 3 0 |1 2 3
		if( 2<sum(aZoid==c( 6,16,NA,25,23,31),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 1<sum( aZoid[c(1,2)]*c(6,3)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( all(aZoid[c(1,3)]==c( 5,11,14,30,33,38)[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextCStepBin
			#      6 21 35 36 37 41    |15 14  1  1  4 |  2  14  24  12  -5  -4 |1 0 1 3 1 |1 1 3 1
			#     12 15 16 20 24 30    | 3  1  4  4  6 |  6  -6 -19 -16 -13 -11 |0 3 2 1 0 |3 2 1
		if( 2<sum(aZoid==c(18, 9,NA, 4,11,19),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 1<sum( aZoid[c(1,2)]*c(2,2)==aZoid[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
			#      5 13 17 23 28 36    | 8  4  6  5  8 | -1   6   7   7 -10  -5 |1 2 2 1 0 |1 2 2 1
			#      4  5  6 12 25 37(1) | 1  1  6 13 12 | -1  -8 -11 -11  -3   1 |3 1 1 1 0 |3 1 1 1
		if( 2<sum(aZoid==c( 3,NA,NA,NA,22,28),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( 1<sum( aZoid[c(1,2)]*c(3,5)==aZoid[c(4,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid[2:6]==c( 4, 5, 6,12,25,37)[1:5]) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextColVal_1
			#      4  7 13 29 31 39    | 3  6 16  2  8 | -4  -4  -6   8  -5  -6 |2 1 1 2 0 |2 1 1 2
			#      3 10 16 19 31 39(2) | 7  6  3 12  8 | -1   3   3 -10   0   0 |1 3 0 2 0 |1 3 2
		if( 2<sum(aZoid==c( 2,13,19, 9,31,39),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감.
		if( all(aZoid[c(5,6)]==c( 3,10,16,19,31,39)[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextColVal_2
			#  1  3 12 14 16 43(1) | 2  9  2  2 27 | -4  -7  -1  -7 -23   0 |2 3 0 0 1 |2 3 1
			#  4  7 13 29 31 39    | 3  6 16  2  8 |  3   4   1  15  15  -4 |2 1 1 2 0 |2 1 1 2
		if( 2<sum(aZoid==c( 7,11,14,NA,NA,35),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_3
			#      2  7 13 25 42 45(1) | 5  6 12 17  3 | -1  -6  -3   1  16  16 |2 1 1 0 2 |2 1 1 2
			#     12 17 23 34 42 45(2) | 5  6 11  8  3 | 10  10  10   9   0   0 |0 2 1 1 2 |2 1 1 2
			#      1 10 13 26 32 36    | 9  3 13  6  4 |-11  -7 -10  -8 -10  -9 |1 2 1 2 0 |1 2 1 2
		if( 2<sum(aZoid==c(NA, 3, 3,16,22,27),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 
		if( all(aZoid[c(5,6)]==c( 1,10,13,26,32,36)[c(5,6)] ) ){	surviveFlg[idx]<-FALSE	;next }	#	rebind ptn

		# fCutCnt.nextColVal_4
			#      1 12 13 24 29 44    |11  1 11  5 15 | -4   5   2   8 -12  -1 |1 2 2 0 1 |1 2 2 1
			#      9 14 17 33 36 38    | 5  3 16  3  2 |  8   2   4   9   7  -6 |1 2 0 3 0 |1 2 3
		if( 2<sum(aZoid==c(17,16,21,42,43,32),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_5
			#      4  7 13 29 31 39(1) | 3  6 16  2  8 | -1  -4   1   0  -2  -5 |2 1 1 2 0 |2 1 1 2
			#      9 14 17 33 36 38    | 5  3 16  3  2 |  5   7   4   4   5  -1 |1 2 0 3 0 |1 2 3
		if( 2<sum(aZoid==c(14,21,21,37,41,37),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		# fCutCnt.nextColVal_6
			#      3 10 16 19 31 39(1) | 7  6  3 12  8 |-10  -6  -8  -6  -2   3 |1 3 0 2 0 |1 3 2
			#      3  9 11 12 13 19(2) | 6  2  1  1  6 |  0  -1  -5  -7 -18 -20 |2 4 0 0 0 |2 4
		if( 2<sum(aZoid==c( 3, 8, 6, 5,NA,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

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
		if( 2<sum(aZoid==c(  3, 3, 5,13,NA,38 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 13, 6,21,13,NA,38 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 14,23, 3,NA,NA,28 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 15,13,16,NA,NA,24 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c(  5, 2,NA,NA,NA,42 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 16, 7,NA,NA,NA,43 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }
		if( 2<sum(aZoid==c( 12, 4,NA,NA,NA,37 ) ,na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }

		# colValSeqNext( ,pColSize=2 )
		score  <- sum(aCStep==c(  3, 2, 7, 3, 3 ),na.rm=T)
		if(2<score){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  9, 2, 6,25, 8 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 14, 5,NA,10, 7 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  4, 3,NA,13, 1 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  9,  3,NA, 5, 5 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  5,11,NA, 5, 9 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  4,14,NA, 8, 3 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c( 18, 9,NA, 1, 7 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }
		matCnt <- sum(aCStep==c(  8, 6,NA,NA, 4 ),na.rm=T)
		if(2<matCnt){	surviveFlg[idx]<-FALSE	;next }

		cnt <- 0
			if( fCutU.hasPtn(c(  2, 3 ),aCStep) )   cnt<-cnt+1
			if( fCutU.hasPtn(c(  2, 8 ),aCStep) )   cnt<-cnt+1
			if( fCutU.hasPtn(c(  9, 1 ),aCStep) )   cnt<-cnt+1
			if( 1<sum( aCStep[ 1 ]*c(1,1)==aCStep[c(4,5)] ) )	cnt<-cnt+1
		if( 2 < cnt ){	surviveFlg[idx]<-FALSE	;next }

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
		if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 2, 3,12),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7, 3,16),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3,16, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(16, 3, 8),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 3,16),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 6, 4,NA, 5, 4),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextZW
		if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(11, 6, 4),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 1,11) ,c( 3,24) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextQuo10
		if( 1<sum( aCStep[c(4,5)]*c(1,1)==aCStep[c(3,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 6, 8,11),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextBin
		if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(3,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(13, 3,10),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 6, 8,11),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 4, 5,NA,NA, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c(20, 1) ,c( 2, 2) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextRebNum
		if( 1<sum( aCStep[ 4 ]*c(2,1)==aCStep[c(1,2)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 3,17),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(16, 4, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextCStepBin
		if( 1<sum( aCStep[c(1,3)]*c(2,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 1, 7, 4),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextFStepBin
		if( 1<sum( aCStep[c(1,3)]*c(1,2)==aCStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 1, 9,20, 2),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 1, 3,NA,31, 5),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(14, 4, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 8, 6, 8, 1),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_1
		if( 1<sum( aCStep[ 3 ]*c(2,4)==aCStep[c(2,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 9,18, 2, 1),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3, 3,11),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3, 3,NA,10),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3, 4,NA,NA, 7),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 4,16),aCStep) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_2
		if( 1<sum( aCStep[ 4 ]*c(3,4)==aCStep[c(2,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(10,23, 2,14),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 4, 3, 1),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 8, 3,10),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		fltRst <- sapply( getSideValues(aCStep, 9) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
		if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_3
		if( 1<sum( aCStep[ 2 ]*c(3,2)==aCStep[c(1,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(19, 9, 4),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 6,10,NA, 3),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 6,10,NA, 3),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5, 6,10,NA, 3),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 2,19, 8, 4),aCStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7,19,17),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 5, 5) ,c(13, 6) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_4
		if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 1, 4, 8),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c(11, 1) ,c( 2, 4) ,c(11, 5) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 4, 2) ,c( 3, 4) )
		if( 1<hasPtnCnt(aCStep,ptnLst) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_5
		if( 1<sum( aCStep[c(4,5)]*c(1,8)==aCStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 4, 1,26, 5),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3,26,NA, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7,NA,28, 4,15),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 5,NA,26, 2),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 9,24, 3,8),aCStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }

		# fCutCnt.nextColVal_6
		if( 1<sum( aCStep[ 2 ]*c(3,3)==aCStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( 1<sum( aCStep[c(1,3)]*c(1,1)==aCStep[c(5,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 9,NA, 1,NA, 9),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 1,NA, 1, 4),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 3,16,15),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c(13, 4, 3),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7, 6,NA,22,11),aCStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7, 6,NA,22,11),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 9,NA,23, 8),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 2, 8,NA, 7, 5),aCStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
		if( fCutU.hasPtn(c( 7,10,NA, 9),aCStep) ){	surviveFlg[idx]<-FALSE	;next }
		ptnLst <- list( c( 1, 6) ,c( 7, 9) )
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

		aFStep <- aZoid - stdMILst[["basic"]]$lastZoid
			if( 1<sum( aFStep[c(3,4)]*c(1,2)==aFStep[c(1,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-1,16,15),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextZW"]]$lastZoid
			if( 1<sum( aFStep[c(5,6)]*c(1,2)==aFStep[c(2,3)] ) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextQuo10"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextBin"]]$lastZoid
			if( fCutU.hasPtn(c(-2,-4,-3,-1,15),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextRebNum"]]$lastZoid
			if( fCutU.hasPtn(c(-3,-10,-6),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextCStepBin"]]$lastZoid
			if( fCutU.hasPtn(c( 2,NA,NA,NA, -5, -3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextFStepBin"]]$lastZoid
			if( 1<sum( aFStep[c(1,3)]*c(-1, 1)==aFStep[c(6,4)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5, -9,-15, -6, -3, 3),aFStep,thld=3,fixIdx=5) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(-1,NA,NA,NA, 4, 6),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감.

		aFStep <- aZoid - stdMILst[["nextColVal_1"]]$lastZoid
			if( 1<sum( aFStep[c(2,5)]*c(1,1)==aFStep[c(3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-1, 6, 7,NA, 0, 0),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(-2, 3, 3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3, 3,-17),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3, 3,NA, 3),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 3,12,-9, 4,18),aFStep,thld=3,fixIdx=4) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c( 2,10,12,NA, 5, 6),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_2"]]$lastZoid
			if( 1<sum( aFStep[c(2,4)]*c(-1, 1)==aFStep[c(6,5)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -8,NA,-1,NA,NA, 0),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(10,11, 3,NA,NA,-8),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_3"]]$lastZoid

		aFStep <- aZoid - stdMILst[["nextColVal_4"]]$lastZoid
			if( 1<sum( aFStep[ 2 ]*c( 4, 2,-3)==aFStep[c(1,3,6)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c(11, 2, 0),aFStep) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(NA,-1, 6,10,NA,-11),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감. 

		aFStep <- aZoid - stdMILst[["nextColVal_5"]]$lastZoid
			if( 1<sum( aFStep[c(4,5)]*c(1,1)==aFStep[c(3,1)] ) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( -3,-1, 5,-2),aFStep,thld=3,fixIdx=3) ){	surviveFlg[idx]<-FALSE	;next }

		aFStep <- aZoid - stdMILst[["nextColVal_6"]]$lastZoid
			if( sum(aFStep[c(3,6)])==sum(aFStep[c(4,5)]) ){	surviveFlg[idx]<-FALSE	;next }	# -25
			if( fCutU.hasPtn(c(-1,-8,-1, 2),aFStep,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			if( fCutU.hasPtn(c( 5,-1, 0,-11,-3),aFStep,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			fltRst <- sapply( getSideValues(aFStep,-8) ,function(vPair){ return( vPair[1]==vPair[2] ) } )
			if( (0<length(fltRst)) && any(fltRst) ){	surviveFlg[idx]<-FALSE	;next }
			if( 2<sum(aFStep==c(10, 4,-2,-8,NA,NA),na.rm=T) ){	surviveFlg[idx]<-FALSE	;next }	# 동일 증감.

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
			if( fCutU.hasPtn(c( 2,NA,NA,26,25,41 ),aZoid,thld=3,fixIdx=1) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <11>
			if( fCutU.hasPtn(c(  6,11,NA,31,23 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			if( fCutU.hasPtn(c( 13,14,43,33,40 ),aZoid,thld=3,fixIdx=2) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			# <30>
			if( fCutU.hasPtn(c(  8,NA,NA,30,33,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 13,14,NA,33,40 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <38>
			if( fCutU.hasPtn(c(  1, 8,11,27,30,38 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextZW
			# < 1>
			if( fCutU.hasPtn(c(  1, 6, 5, 5,32 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 2>
			if( fCutU.hasPtn(c(  2,27,37 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 4>
			if( fCutU.hasPtn(c(  4,13,13,32 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,25,14,18,31 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  2,10,20,NA,17 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 13,14,19,32,40 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <24>			# <28>
			# <38>
			if( fCutU.hasPtn(c( 23,26,38 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 35,NA,NA,42,45 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c(  3,38,NA,NA,NA,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextQuo10
			# < 3>
			# < 8>
			if( fCutU.hasPtn(c(  2, 8,15 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			# <10>
			if( fCutU.hasPtn(c( 10,14,23,37,28 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			# <19>
			if( fCutU.hasPtn(c(  2,10,13, 8, 9,19 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c( 26,31,NA,45 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>			# <43>

		#	fCutCnt.nextBin
			# < 3>
			if( fCutU.hasPtn(c(  3, 4, 8,14,28 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c( 10,12,25,23,24 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			# <15>
			if( fCutU.hasPtn(c( 13,15,26 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  5, 6,18,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 15,19,NA,NA,32 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <35>

		#	fCutCnt.nextRebNum
			# < 4>
			# < 5>
			if( fCutU.hasPtn(c( 5,NA,NA,17,21 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,11,38,37,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c( 11,NA,39,40,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c( 11,16,21,NA,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			# <33>
			if( fCutU.hasPtn(c(  3, 7, 8,NA,33 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 23,24,31,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>			# <45>

		#	fCutCnt.nextCStepBin
			# <12>			# <24>
			# <32>
			if( fCutU.hasPtn(c( 12, 7,19,NA,32 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 13,NA,36,37 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			if( fCutU.hasPtn(c( 13,36,36,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextFStepBin
			# < 4>
			if( fCutU.hasPtn(c(  4,NA,NA,NA,17,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 5>
			if( fCutU.hasPtn(c(  5,NA, 7,27 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 6>
			if( fCutU.hasPtn(c(  6,17,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			# <13>
			if( fCutU.hasPtn(c( 13,20,29,24,31 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>			# <16>
			# <17>
			if( fCutU.hasPtn(c( 12,17,NA,NA,30 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <28>
			if( fCutU.hasPtn(c( 12,12,22,28,35 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <37>
			# <41>
			if( fCutU.hasPtn(c(  1, 6,15,NA,41 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }

		#	fCutCnt.nextColVal_1
			# < 2>
			if( fCutU.hasPtn(c(  2,10,17,25,29,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  5,10,19,12,30,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  6,11,19,NA,33 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c(  7, 4,13,32,30,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 12,NA,19,41,42 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  2,13,19, 9,31,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <32>
			if( fCutU.hasPtn(c( 10,12,30,32,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  6, 9,12,10,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  2,13,19, 9,31,39 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }  

		#	fCutCnt.nextColVal_2
			# < 5>
			if( fCutU.hasPtn(c(  5,11,12,16,NA,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  7,18,NA,43 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <10>
			if( fCutU.hasPtn(c(  2,10,NA,21 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c(  3, 4,13,37,23,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <14>
			# <21>
			if( fCutU.hasPtn(c(  2,10, 7,21 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c(  4, 7,NA,31 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  9,16,NA,NA,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <43>

		#	fCutCnt.nextColVal_3
			# < 1>
			if( fCutU.hasPtn(c(  1,14,15,24,30,30 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			if( fCutU.hasPtn(c(  1, 4,11,41,NA,39 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			if( fCutU.hasPtn(c( 13,27,22,27 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <26>
			if( fCutU.hasPtn(c(  4, 2,26,35 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <34>
			if( fCutU.hasPtn(c( 18,23,18,34,42 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <42>
			if( fCutU.hasPtn(c( 22,27,33,NA,42,45 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>
			if( fCutU.hasPtn(c( 22,27,33,43,42,45 ),aZoid,thld=3,fixIdx=6 ) ){	surviveFlg[idx]<-FALSE	;next }		

		#	fCutCnt.nextColVal_4
			# < 9>
			if( fCutU.hasPtn(c(  9,NA,NA,26,30,33 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <11>
			# <17>
			if( fCutU.hasPtn(c( 17,45,NA,40 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 25,33,36,36 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c( 25,33,36 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <44>
			if( fCutU.hasPtn(c( 13,22,44 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <45>

		#	fCutCnt.nextColVal_5
			# < 5>
			if( fCutU.hasPtn(c(  5,13,12,28,27,45 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 7>
			if( fCutU.hasPtn(c(  6, 7,NA,33,33,42 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# < 9>
			if( fCutU.hasPtn(c(  9,16,NA,27,29 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <12>
			if( fCutU.hasPtn(c(  5,NA,12,28,27,45 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			# <25>
			if( fCutU.hasPtn(c( 24,NA,NA,25,37,36 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <29>
			if( fCutU.hasPtn(c(  3, 3,14,29,NA,34 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c(  7,16, 5,33 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>
			if( fCutU.hasPtn(c(  2, 4, 9,33,36 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <39>
			if( fCutU.hasPtn(c(  9,17,NA,32,39 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }		

		#	fCutCnt.nextColVal_6
			# < 3>
			# <10>
			if( fCutU.hasPtn(c( 10,14,13,28,43  ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <13>
			# <16>
			if( fCutU.hasPtn(c(  7,16,NA,37,45 ),aZoid,thld=3,fixIdx=2 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <19>
			if( fCutU.hasPtn(c( 14,10,19 ),aZoid,thld=3,fixIdx=3 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <21>
			if( fCutU.hasPtn(c( 21,36,30,36 ),aZoid,thld=3,fixIdx=1 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <25>
			if( fCutU.hasPtn(c( 20,22,NA,25,32,37 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <31>
			if( fCutU.hasPtn(c( 12,17,17,31,45 ),aZoid,thld=3,fixIdx=4 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <33>
			if( fCutU.hasPtn(c( 18,17,27,19,33,34 ),aZoid,thld=3,fixIdx=5 ) ){	surviveFlg[idx]<-FALSE	;next }
			# <36>

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
		# dbgLst[[1+length(dbgLst)]] <- paste( quoSize,collapse="," )

		# fCutCnt.nextColVal_1	: 1 3 0 2 0 --> 1 2 1 2 0
		if( all(quoSize==c( 1, 2, 1, 2, 0)) ){	surviveFlg[idx]<-FALSE	;next }
	}
	#	table( surviveFlg )	;dbgIdx <- head(which(!surviveFlg))	;dbgZoid <- gEnv$allZoidMtx[allIdxF[dbgIdx],,drop=F]

	cat(sprintf("  survive in rmvQuo10 %d from %d \n",length(allIdxF),initSize))
	allIdxF <- allIdxF[surviveFlg]
	return( allIdxF )

}	# rmvQuo10()

# UNdone		fCut.basic() 사용
rmvZW <- function( gEnv ,allIdxF ){

	initSize <- length(allIdxF)
	# fCutCnt.nextQuo10

	# Gold : 발생하더라도 이전에 발생한 영역에서 재발되지는 않겠지...
	#	"nextRebNum( NA )","nextColVal_1( 37 )","nextColVal_5( 40 )","nextFStepBin( NA )"
	zw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){aZoid[6]-aZoid[1]})
	allIdxF <- allIdxF[!( zw%in%c(37,40) )]

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






