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
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<hntCrossDim>------------
	bRstObj <- ban.hntCrossDim(banObj ,allZoidMtx ,pCodeLst=codeLst ,pDepth=2)
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<multiDim>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.multiDim(banObj ,allZoidMtx ,pCodeLst=codeLst )
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH2>------------
	#	pBanObj<-banObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
	rstIdStr <- sprintf("%s_base",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# ==================================================================
	banCmbObj <- getCFltCmbObj( tEnv )
	codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )

	# -<hntSameRow>------------
	bRstObj <- ban.hntSameRow(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst)
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<hntCrossDim>------------
	bRstObj <- ban.hntCrossDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pDepth=2)
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<multiDim>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.multiDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2 ;pLevel="mid"
	bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

	# -<throughH2>------------
	#	pbanCmbObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDimThld=2 ;pDepth=2
	bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
	rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
	rstLst[[rstIdStr]][[1+length(rstLst[[rstIdStr]])]] <- bRstObj

}
tDiff <- Sys.time() - tStmp

filtedIdxLst <- list()
for( idIdx in attributes(rstLst)$names ){
	# if( idIdx %in% c("multiDim_comb","throughH_base") ){
	# 	next
	# }
	filtedCnt <- sapply( rstLst[[idIdx]] ,function(p){ length(p$filtedIdx) } )
	cat(sprintf("%s : %5.1f%%(%d/%d) \n",idIdx,100*sum(filtedCnt)/length(filtedCnt),sum(filtedCnt),length(filtedCnt)))
	filtedIdxLst[[idIdx]] <- which(filtedCnt>0)

	fltNameLst <- lapply( rstLst[[idIdx]] ,function(p){p$filtLst[[1]]})
	# table(do.call( c ,fltNameLst ))	# 어떤 필터에 주로 걸렸는지..?
	# flag <- sapply( rstLst[["hntSameRow"]] ,function(p){ (p$filtLst %in% c("A0080","A0090") ) } )
}
filtedIdx <- do.call( c ,filtedIdxLst )
filtedIdx <- sort(unique(filtedIdx))

classFCnt <- sapply( fRstLst ,length )

# ===============================================================================================
# 실제 필터링 측정 (10만/hr)

fltCnt <- rep( 0 ,nrow(gEnv$allZoidMtx) )
for( remIdx in 1:length(remLst) ){
	fltCnt[remLst[[remIdx]]] <- fltCnt[remLst[[remIdx]]] + 1
}


chosenZ.idx <- which( fltCnt==3 )
# chosenZ.idx <- sort(sample(chosenZ.idx,100))
chosenZMtx <- gEnv$allZoidMtx[chosenZ.idx,]

rstLst <- list()

tStmp <- Sys.time()
banObj <- getCFltObj( gEnv )
codeLst <- banObj$getCodeLst( chosenZMtx )
tDiff <- Sys.time() - tStmp
cat(sprintf("banObj %.1f%s\n",tDiff,units(tDiff)))

bRstObj <- ban.hntSameRow(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.hntCrossDim(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.multiDim(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH2(banObj ,chosenZMtx ,pCodeLst=codeLst)
rstIdStr <- sprintf("%s_base",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

banCmbObj <- getCFltCmbObj( gEnv )
codeCmbLst <- banCmbObj$getCodeLst( chosenZMtx )
tDiff <- Sys.time() - tStmp
cat(sprintf("banCmbObj %.1f%s\n",tDiff,units(tDiff)))

bRstObj <- ban.hntSameRow(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.hntCrossDim(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.multiDim(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

bRstObj <- ban.throughH2(banCmbObj ,chosenZMtx ,pCodeLst=codeCmbLst)
rstIdStr <- sprintf("%s_comb",bRstObj$idStr)
rstLst[[rstIdStr]] <- bRstObj$filtedIdx
tDiff <- Sys.time() - tStmp
cat(sprintf("%s %.1f%s\n",rstIdStr,tDiff,units(tDiff)))

filtedIdx <- sort(unique(do.call( c ,rstLst )))

# ==================================================================




