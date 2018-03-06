# 20180109_C_H.R 교차모델

tStmp <- Sys.time()
testSpan <- 300:nrow(gEnv$zhF)
rstLst <- list()
for( tIdx in testSpan ){

	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	banObj <- getCFltObj( tEnv )

	allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
	codeLst <- banObj$getCodeLst( allZoidMtx )

	# -<hntSameRow>------------
	bRstObj <- ban.hntSameRow(banObj ,allZoidMtx ,pCodeLst=codeLst)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstLst[[bRstObj$idStr]][[1+length(rstLst[[bRstObj$idStr]])]] <- bRstObj

	# -<hntCrossDim>------------
	bRstObj <- ban.hntCrossDim(banObj ,allZoidMtx ,pCodeLst=codeLst ,pDepth=2)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstLst[[bRstObj$idStr]][[1+length(rstLst[[bRstObj$idStr]])]] <- bRstObj

	# -<multiDim>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.multiDim(banObj ,allZoidMtx ,pCodeLst=codeLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstLst[[bRstObj$idStr]][[1+length(rstLst[[bRstObj$idStr]])]] <- bRstObj
	
}
tDiff <- Sys.time() - tStmp


for( idIdx in attributes(rstLst)$names ){
	filtedCnt <- sapply( rstLst[[idIdx]] ,function(p){ length(p$filtedIdx) } )
	cat(sprintf("%s : %5.1f%%(%d/%d) \n",idIdx,100*sum(filtedCnt)/length(filtedCnt),sum(filtedCnt),length(filtedCnt)))

	fltNameLst <- lapply( rstLst[[idIdx]] ,function(p){p$filtLst[[1]]})
	# table(do.call( c ,fltNameLst ))	# 어떤 필터에 주로 걸렸는지..?
	# flag <- sapply( rstLst[["hntSameRow"]] ,function(p){ (p$filtLst %in% c("A0080","A0090") ) } )
}


