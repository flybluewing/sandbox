# 20180109_C_H.R 교차모델
#	pEVL : encValLst, encoded value list
#	pSBC : search base code name
#	pNZC : next zoid code name
#	pCFLst : getCFLst.base(tEnv) # 비교함수를 얻기 위해.
#		pEVL<- encValLst ;pSBC<-"A0010_o3"	;pNZC<-"A0020_o3"	;pCFLst<-cfObjLst
evlScan <- function( pEVL ,pSBC ,pNZC ,pCFLst ){

	cfNames <- sapply(pCFLst,function(p){p$idStr})
	sbc.cfObj <- pCFLst[[which(cfNames==pSBC)]]
	sbcLst <- pEVL[[pSBC]]
	nzcLst <- pEVL[[pNZC]]
	
	sbc.last <- sbcLst[[length(sbcLst)]]

	nzcLst.fnd <- list()	# found nzcLst.
	idx.fnd <- list()
	for( idx in 1:(length(sbcLst)-1) ){
		dCnt <- sbc.cfObj$diffCnt( sbcLst[[idx]] ,sbc.last )
		if( 0==dCnt ){
			idx.fnd[[1+length(idx.fnd)]] <- idx
			nzcLst.fnd[[1+length(nzcLst.fnd)]] <- nzcLst[[(idx+1)]]
		}
	}

	rObj <- list( sbcName=pSBC ,nzcName=pNZC ,nzcLst.fnd=nzcLst.fnd )
	if( 0 < length(idx.fnd) ){
		rObj$idx.fnd <- do.call( c ,idx.fnd )
		rObj$lastZC <- nzcLst.fnd[[length(nzcLst.fnd)]]
	}

	return( rObj )
} # evlScan()

evalScan.pair <- function( pEVL ,pName ,pCFLst ,pThld=0 ){

	cfName <- sapply(pCFLst,function(p){p$idStr})
	cfObj <- pCFLst[[which(cfName==pName)]]
	sbcLst <- encValLst[[pName]]

	pairLst <- list()
	for( aIdx in 1:(length(sbcLst)-2) ){
		for( bIdx in (aIdx+1):length(sbcLst) ){
			dCnt <- cfObj$diffCnt( sbcLst[[aIdx]] ,sbcLst[[bIdx]] )
			if( pThld>=dCnt ){
				pairLst[[1+length(pairLst)]] <- c( aIdx ,bIdx ,dCnt )
			}
		}
	}

	pairMtx <- do.call( rbind ,pairLst )
	colnames(pairMtx) <- c("aIdx","bIdx","thld")
	return( pairMtx )

} # evalScan.pair()


# ====================================================================================

cf_A0010 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0010_o%s",pBase) )
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						(pZoidMtx[p,2:6]-pZoidMtx[p,1:5])%%pBase } 
					)
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0010()

cf_A0020 <- function( pEnv ,pBase=3 ){
	
	cfObj <- list( idStr=sprintf("A0020_o%s",pBase) )
	cfObj$lastZoid <- pEnv$zhF[nrow(pEnv$zhF),]
	cfObj$enc <- function( pZoidMtx ,pLogFunc=NULL ){
		tStmp <- Sys.time()

		rLst <- lapply( seq_len(nrow(pZoidMtx)) ,function(p){ 
						abs(cfObj$lastZoid-pZoidMtx[p,])%%pBase } 
					)
		if( !is.null(pLogFunc) ){
			tDiff <- Sys.time() - tStmp
			pLogFunc( sprintf("ID:%s cost:%.1f%s",cfObj$idStr,tDiff,units(tDiff)) )
		}
		
		return( rLst )
	} # cfObj$enc()
	cfObj$diffCnt <- function( p1 ,p2 ){
		return( sum(p1!=p2) )
	}
	
	return( cfObj )

} # cf_A0020()


