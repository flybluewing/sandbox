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
	#	pBanObj<-banCmbObj ;pZoidMtx<-allZoidMtx ;pCodeLst<-codeCmbLst ;pInitZIdx=NULL ;pDebug=T
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

val.getColSeq <- function(){

	tIdx <- 747
	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	allZoidMtx <- gEnv$zhF[tIdx,,drop=F]

	depth <- 3
	seqLst <- list()
	for( tIdx in 500:nrow(gEnv$zhF) ){
		valMtx <- gEnv$zhF[1:tIdx,]
		seqLst[[1+length(seqLst)]] <- getColSeq( valMtx ,pDepth=depth )
	}

	cnt <- sapply( seqLst ,function(p){ sum(p$flag) })

} # val.getColSeq()

val.getBanPtn <- function( ){

	tIdx <- 747
	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	allZoidMtx <- gEnv$zhF[tIdx,,drop=F]

	# valMtx : 테스트 목적에 따라 만들기
	banObj <- getCFltObj( tEnv )
	codeLst <- banObj$getCodeLst( allZoidMtx )
	valMtx <- do.call( rbind ,banObj$encValLst[[1]] )

	valMtx <- gEnv$zhF %% 10
	rownames(valMtx) <- 1:nrow(valMtx)

	rLst <- list()
	#for( vIdx in 600:nrow(valMtx) ){
	for( vIdx in 100:nrow(valMtx) ){		

		# pValMtx <- valMtx ;pMaxDepth=5 ;pDebug=F
		banObj <- getBanPtn( valMtx[1:(vIdx-1),] )

		# banRst <- banObj$chkMatchAll( valMtx[vIdx,,drop=F] ,pDebug=T )
		banRst <- banObj$chkMatchAny( valMtx[vIdx,,drop=F] ,pDebug=T )

		rObj <- list( ptnCnt=length(banObj$ptnLst) ,banRst=banRst$rstLst[[1]] )
		rObj$chkCnt		<- banRst$chkCntLst[[1]]
		rObj$matCnt		<- banRst$matCntLst[[1]]
		rObj$ptnSlide	<- banRst$ptnSlideLst[[1]]
		rObj$banObj <- banObj
		rLst[[1+length(rLst)]] <- rObj
	}

	ptnCnt <- sapply( rLst ,function(p){p$ptnCnt} )
	banCnt <- sapply( rLst ,function(p){length(p$banRst)} )
	tbl <- table(banCnt>0)
	# 테스트 구간 내에서 발생 수.
	cat(sprintf(" %d/%d (%.1f%%) \n",tbl["TRUE"],length(rLst),tbl["TRUE"]*100/length(rLst) ))

	# banCnt.idx <- which(banCnt>2)
	# lapply( rLst[banCnt.idx] ,function(p){p$chkCnt} )
	# lapply( rLst[banCnt.idx] ,function(p){p$matCnt} )
	# lapply( rLst[banCnt.idx] ,function(p){p$ptnSlide} )

	cName <- c("rIdx","fail","depth","pSlide","chkCnt","matCnt")
	rstMtx <- matrix( 0 ,nrow=0 ,ncol=length(cName) )
	# for( rIdx in banCnt.idx ){
	for( rIdx in 1:length(banCnt) ){
		rObj <- rLst[[rIdx]]
		mtx <- matrix( 0 ,nrow=rObj$ptnCnt ,ncol=length(cName) )
		colnames(mtx)<-cName

		mtx[,"rIdx"]	<- rIdx
		mtx[,"depth"]	<- sapply(rObj$banObj$ptnLst ,function(p){p$depth})
		mtx[rObj$banRst,"fail"] 	<- 1
		mtx[,"pSlide"]	<- sapply(rObj$banObj$ptnLst,function(p){p$ptnSlide})
		mtx[,"chkCnt"]	<- sapply(rObj$banObj$ptnLst,function(p){p$chkCnt})
		mtx[,"matCnt"]	<- sapply(rObj$banObj$ptnLst,function(p){p$matCnt})
		rstMtx <- rbind( rstMtx ,mtx )
	}

	anaMtx <- rstMtx
	tbl <- table(anaMtx[,"fail"])	# 전체 ptn에 대해 걸린 비율.
	cat(sprintf("rstMtx all %d/%d (%.1f%%) \n",tbl["1"],nrow(anaMtx),tbl["1"]*100/nrow(anaMtx) ))

	for( chkCnt in 1:4 ){
		anaMtx <- rstMtx[rstMtx[,"chkCnt"]>chkCnt,]
		failCnt <- sum(anaMtx[,"fail"]==1)
		cat(sprintf("chkCnt>%d all %d/%d (%.1f%%) \n"
				,chkCnt ,failCnt,nrow(anaMtx),failCnt*100/nrow(anaMtx) ))

		failCnt <- sum(0<tapply( anaMtx[,"fail"] ,anaMtx[,"rIdx"] ,sum ))
		cat(sprintf("    failCnt %d/%d (%.1f%%) \n"
				,failCnt,length(rLst),failCnt*100/length(rLst) ))
	}

} # val.getBanPtn()

tStmp <- Sys.time()
rstLst <- list()
testSpan <- 200:nrow(gEnv$zhF)
for( tIdx in testSpan ){

	tEnv <- gEnv
	tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
	tEnv$allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
	allIdx <- 1

    colValLst <- apply( gEnv$zhF ,2 ,function(p){
                        val <- sort(unique(p))
                        tbl <- table(p)
                        mtx <- matrix( 0 ,ncol=length(val) ,nrow=2 )
                        mtx[1,] <- val
                        mtx[2,] <- tbl[as.character(val)]
                        rownames(mtx) <- c("val","freq")
                        return(mtx)
                    })

	rstFlag <- character(0)

    rstObj <- cutEadge.colValCut( tEnv ,allIdx ,colValLst )
	if( !rstObj$flag ){	rstFlag[1+length(rstFlag)] <- rstObj$idStr }

    rstObj <- cutEadge.dup3Col( tEnv ,allIdx ,colValLst ,pThld=5 )  # pThld^6 에 비해 효과는 좋음.
	if( !rstObj$flag ){	rstFlag[[1+length(rstFlag)]] <- rstObj$idStr }

	cutEadgeLst <- getCutEadgeLst()
	for( idx in seq_len(length(cutEadgeLst)) ){
		rstObj <- cutEadgeLst[[idx]]( tEnv ,allIdx )
		if( !rstObj$flag ){	rstFlag[[1+length(rstFlag)]] <- rstObj$idStr }
	}

	rstLst[[1+length(rstLst)]] <- rstFlag

} # tIdx
tDiff <- Sys.time() - tStmp	# 43min

fltFlag <- sapply( rstLst ,length )	# table(fltFlag)
fltDensity <- do.call( c ,rstLst )	# table(fltDensity)
fltDensity.tbl <- sort( table(fltDensity) ,decreasing=T )

getCutEadgeLst <- function( ){

    rLst <- list()

    rLst[[1+length(rLst)]] <- cutEadge.getCFltObj
    rLst[[1+length(rLst)]] <- cutEadge.remLstHard
    rLst[[1+length(rLst)]] <- cutEadge.getColSeq
    rLst[[1+length(rLst)]] <- cutEadge.getBanPtn
    rLst[[1+length(rLst)]] <- cutEadge.getBanPtnColVal
    rLst[[1+length(rLst)]] <- cutEadge.getBanSym
    rLst[[1+length(rLst)]] <- cutEadge.getBanGrad
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeq
    rLst[[1+length(rLst)]] <- cutEadge.getBanRebBin
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeqBin
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymBin
    rLst[[1+length(rLst)]] <- cutEadge.getBanRebDiff
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeqDiff
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymDiff # 14th

    rLst[[1+length(rLst)]] <- cutEadge.banSeqRebCStep
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymCStep
    # rLst[[1+length(rLst)]] <- cutEadge.getBanStepRebCStep
    rLst[[1+length(rLst)]] <- cutEadge.getBanGradCStep

    rLst[[1+length(rLst)]] <- cutEadge.getBanSeqRebWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanStepRebWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanGradWidth

    rLst[[1+length(rLst)]] <- cutEadge.banReb3
    rLst[[1+length(rLst)]] <- cutEadge.banSeq3Twice

    return( rLst )

}



