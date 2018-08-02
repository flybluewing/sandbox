cntThld <- c(2,3,2,2)	;names(cntThld) <- c("raw","rem","cStep","fStep")


# done
fCutCnt.nextXXX <- function( gEnv ,allIdxF ,rpt=FALSE ){

	flgCnt <- rep( 0 ,length(allIdxF) )
	# zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	if( 0==nrow(zMtx) ) return( rep(0,length(allIdxF)) )

	stdMI <- fCutU.getMtxInfo( zMtx )
	# mtxLen  lastZoid    rem quo10   cStep   fStep   rawTail cStepTail   quoTail quoRebPtn
	# rptObj<-anaMtx(stdMI$rawTail,stdZoid);u0.zoidMtx_ana.rpt( stdMI$rawTail )

	flgCnt <- flgCnt + fCutU.commonCutCnt( gEnv ,allIdxF ,zMtx )

	# -- conditional
	cntMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cntThld) )	;colnames(cntMtx)=names(cntThld)

    cntMtx[,"raw"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( aZoid[1]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[2]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[3]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[4]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[5]%in%c(   ) ) cnt<-cnt+1
					if( aZoid[6]%in%c(   ) ) cnt<-cnt+1
					# u0.zoidInc_ana(stdMI$rawTail[n,],stdMI$rawTail[m,])
					if( fCutU.hasPtn(c(  ,  ),aZoid) ) cnt<-cnt+1	# < >
					if( 1<sum(aZoid[ : ]==c( , , )) ) cnt<-cnt+1
					
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"raw"],rpt)
    cntMtx[,"rem"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					# rptObj<-anaMtx( stdMI$rawTail ) ;u0.zoidMtx_ana.rpt( stdMI$rawTail%%10 )
					cnt <- 0
					aRem <- aZoid%%10
					if( aRem[1]%in%c(   ) ) cnt<-cnt+1
					if( aRem[2]%in%c(   ) ) cnt<-cnt+1
					if( aRem[3]%in%c(   ) ) cnt<-cnt+1
					if( aRem[4]%in%c(   ) ) cnt<-cnt+1
					if( aRem[5]%in%c(   ) ) cnt<-cnt+1
					if( aRem[6]%in%c(   ) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"rem"],rpt)
    cntMtx[,"cStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aCStep <- aZoid[2:6]-aZoid[1:5]
					if( aCStep[1]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[2]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[3]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[4]%in%c(   ) ) cnt<-cnt+1
					if( aCStep[5]%in%c(   ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"cStep"],rpt)
    cntMtx[,"fStep"] <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					aFStep <- aZoid - stdMI$lastZoid
					if( aFStep[1]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[2]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[3]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[4]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[5]%in%c(         ) ) cnt<-cnt+1
					if( aFStep[6]%in%c(         ) ) cnt<-cnt+1

					return( cnt )
				})	;kIdx<-anaFltCnt(cntMtx[,"fStep"],rpt)
	score <- sapply( 1:length(flgCnt) ,function( idx ){
					if( any(cntMtx[idx,] >cntThld ) )	return( 2 )
					if( any(cntMtx[idx,]==cntThld ) )	return( sum(cntMtx[idx,]==2) )
					if( 0<cntMtx[idx,"raw"]){
						cnt <- sum(cntMtx[idx,c("rem","cStep","fStep")])
						if( cnt>1 )	return( 1 )
					}
					return( 0 )
				})
    flgCnt <- flgCnt + score

	return( flgCnt )
} # fCutCnt.nextXXX()

