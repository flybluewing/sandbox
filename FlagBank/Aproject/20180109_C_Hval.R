# 20180109_C_H.R 교차모델

tStmp <- Sys.time()
testSpan <- 300:nrow(gEnv$zhF)
rstLst <- list()
for( tIdx in testSpan ){

	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	allZoidMtx <- gEnv$zhF[tIdx,,drop=F]

	# ==================================================================
	banObj <- getCFltObj( tEnv )
	codeLst <- banObj$getCodeLst( allZoidMtx )

	# -<hntSameRow>------------
	bRstObj <- ban.hntSameRow(banObj ,allZoidMtx ,pCodeLst=codeLst)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<hntCrossDim>------------
	bRstObj <- ban.hntCrossDim(banObj ,allZoidMtx ,pCodeLst=codeLst ,pDepth=2)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<multiDim>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.multiDim(banObj ,allZoidMtx ,pCodeLst=codeLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH2>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# ==================================================================
	banCmbObj <- getCFltCmbObj( tEnv )
	codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )

	# -<hntSameRow>------------
	bRstObj <- ban.hntSameRow(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<hntCrossDim>------------
	bRstObj <- ban.hntCrossDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pDepth=2)
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<multiDim>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.multiDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH2>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	if( is.null(rstLst[[bRstObj$idStr]]) ){
		rstLst[[bRstObj$idStr]] <- list()
	}
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

}
tDiff <- Sys.time() - tStmp

filtedIdxLst <- list()
for( idIdx in attributes(rstLst)$names ){
	filtedCnt <- sapply( rstLst[[idIdx]] ,function(p){ length(p$filtedIdx) } )
	cat(sprintf("%s : %5.1f%%(%d/%d) \n",idIdx,100*sum(filtedCnt)/length(filtedCnt),sum(filtedCnt),length(filtedCnt)))
	filtedIdxLst[[idIdx]] <- which(filtedCnt>0)

	fltNameLst <- lapply( rstLst[[idIdx]] ,function(p){p$filtLst[[1]]})
	# table(do.call( c ,fltNameLst ))	# 어떤 필터에 주로 걸렸는지..?
	# flag <- sapply( rstLst[["hntSameRow"]] ,function(p){ (p$filtLst %in% c("A0080","A0090") ) } )
}
filtedIdx <- do.call( c ,filtedIdxLst )
filtedIdx <- unique(filtedIdx)

# ===============================================================================================




