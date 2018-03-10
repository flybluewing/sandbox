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

	# -<throughH>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstLst[[bRstObj$idStr]][[1+length(rstLst[[bRstObj$idStr]])]] <- bRstObj

	# -<throughH2>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst )
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


# ===============================================================================================
tIdx <- 791
tEnv <- gEnv
tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
banObj <- getCFltObj( tEnv )

allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
codeLst <- banObj$getCodeLst( allZoidMtx )

#	pName="A0010_o3"	;pBanObj <- banObj
ban.testRepeat <- function( pName ,pBanObj ){
	
	encValLst <- pBanObj$encValLst[[pName]]
	
	sDepth <- 5	# 다음 5개 까지 검색.
	matFlag <- rep( TRUE ,length(encValLst[[1]]) )
	matLog	<- rep( 0 ,sDepth )
	for( eIdx in 1:(length(encValLst)-sDepth-1)){
		matFlag[] <- TRUE
		for( mIdx <- 1:sDepth ){
			mrIdx <- eIdx+mIdx
			
		}
	} # for(eIdx)
	
} # ban.testRepeat()


