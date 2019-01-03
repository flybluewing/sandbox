cntMtx.colName <- c( "raw","rawFV","rem","cStep","fStep"
						,"raw.w1","cStep.w1","cStep.w2","fStep.w1","fStep.w2"
					)

	# stdMI <- fCutU.getMtxInfo( zMtx )
	# 	mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
    # cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
	# 				# u0.zoidInc_ana(stdMI$rawTail[n,],stdMI$rawTail[m,])
	# 				# if( aZoid[c( , , )]==c(  ,  ,  ) ) cnt<-cnt+1
	# 				#	if( 1<sum(aZoid[1:3+0]==c( , , )) ) cnt<-cnt+1
	# 			})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)



# undone
fCutCnt.next181006 <- function( gEnv ,allIdxF ,rpt=FALSE ){

	zMtx <- fCutU.getXXXX( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
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
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1	# -
			if( fCutU.hasPtn(c( , ),aFStep) )	cnt.w1<-cnt.w1+1

			cnt.w2 <- 0
			if( 1<sum( aFStep[c(,)]*c(,)==aFStep[c(,)] ) )	cnt.w2<-cnt.w2+1
			if( aFStep[ ]==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1
			if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) )	cnt.w2<-cnt.w2+1	# 

			cntMtx[idx,"fStep.w1"] <- cnt.w1	;cntMtx[idx,"fStep.w2"] <- cnt.w2
			cntMtx[idx,"fStep"] <- cnt + cnt.w1 + cnt.w2
		}

	} # for( idx )	# kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)

	return( list( cccObj=cccObj	,auxCntMtx=auxCntMtx ,cntMtx=cntMtx	) )	# lastZoid´Â cccObj ¾È¿¡..

} # fCutCnt.next181006()



cntThld <- c(2,2,3,2,2)	;names(cntThld) <- c("raw","rawFV","rem","cStep","fStep")

# undone
fCutCnt.nextXXX <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getXXXX( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	cccObj <- fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )
	flgCnt <- flgCnt + cccObj$flgCnt

	# -- conditional
	auxCntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=2 )	;colnames(auxCntMtx)=c("auxZW","auxQuo")
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# if( (aZoid[6]-aZoid[1]) %in% c( , ) ) return( FALSE )
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxZW"] <- !flag
    flgCnt[!flag] <- flgCnt[!flag] + 1	
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					quoSize <- fCutU.getQuoObj( aZoid )$size
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
					# if( all(quoSize[1:3+ ]==c(,,)) ) return(FALSE)	# next rebind of ,,
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
	auxCntMtx[,"auxQuo"] <- !flag
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
					if( aZoid[6]%in%c(    ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rawFV"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# anaMtx.freqVal( stdMI$rawTail )
					cnt <- 0
					# < >
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rawFV"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# u0.zoidMtx_ana( stdMI$rawTail%%10 )
					cnt <- 0
					if( fCutU.remFilt(aZoid[1],c(     ),c(     )) ) cnt<-cnt+1 # 1
					if( fCutU.remFilt(aZoid[2],c(     ),c(     )) ) cnt<-cnt+1 # 2
					if( fCutU.remFilt(aZoid[3],c(     ),c(     )) ) cnt<-cnt+1 # 3
					if( fCutU.remFilt(aZoid[4],c(     ),c(     )) ) cnt<-cnt+1 # 4
					if( fCutU.remFilt(aZoid[5],c(     ),c(     )) ) cnt<-cnt+1 # 5
					if( fCutU.remFilt(aZoid[6],c(     ),c(     )) ) cnt<-cnt+1 # 6
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	u0.zoidCMtx_ana.rpt( stdMI$rawTail )
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 
					if( 1<sum(aCStep[1:3+ ]==c(  , ,  )) ) cnt<-cnt+1	# 

					if( fCutU.hasPtn(c( , ),aCStep) ) cnt<-cnt+1
					if( all(aCStep[1:2+ ]==c( , )) ) cnt<-cnt+1

					#
					if( all(aCStep[]==aCStep[]) ) cnt<-cnt+1
					if( aCStep[ ]==sum(aCStep[c( , )]) ) cnt<-cnt+1
					if( sum(aCStep[c( , )])==sum(aCStep[c( , )]) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					#	u0.zoidFMtx_ana.rpt( stdMI$rawTail )
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 
					if( 1<sum(aFStep[1:3+ ]==c( , , )) ) cnt<-cnt+1 # 

					#
					if( all(aFStep[]==aFStep[]) ) cnt<-cnt+1
					if( aFStep[ ]==sum(aFStep[c( , )]) ) cnt<-cnt+1
					if( sum(aFStep[c( , )])==sum(aFStep[c( , )]) ) cnt<-cnt+1

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
					,lastZoid=stdMI$lastZoid
				) 
			)

} # fCutCnt.nextXXX()

