

# 
fCutCnt.nextXXX <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	# 	if( fCutU.hasPtn(c(,),aXxxx) ) cnt<-cnt+1
	# 	if( all(aXxxx[c(,)]==c(,)) ) cnt<-cnt+1
    fltCnt.raw <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[ ]%in%c(     ) ) cnt<-cnt+1

					if( fCutU.hasPtn(c(xx,  ),aZoid) ) cnt<-cnt+1	# <xx>
					cnt <- cnt + sum( aZoid[ : ]==c(  ,xx,  ) )

					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt.raw,rpt)
    fltCnt.rem <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[ ]%in%c(     ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt.raw,rpt)
    fltCnt.cStep <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[ ]%in%c(  ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt.raw,rpt)

	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( fltCnt.raw[idx]>2 )		return( 2 )
					if( fltCnt.rem[idx]>2 )		return( 2 )
					if( fltCnt.cStep[idx]>2 )	return( 2 )

					if( fltCnt.raw[idx]==2 )	return( 1 )
					if( fltCnt.rem[idx]==2 )	return( 1 )
					if( fltCnt.cStep[idx]==2 )	return( 1 )

					if( fltCnt.raw[idx]>0 && (fltCnt.rem[idx]>0 || fltCnt.cStep[idx]>0 ) )		return( 1 )
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( flgCnt )
} # fCutCnt.nextXXX( )

