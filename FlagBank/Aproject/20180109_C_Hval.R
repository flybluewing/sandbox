# 20180109_C_H.R 교차모델

# =====================================================================================
# 실제 Zoid History들의 필터링 테스트.
tStmp <- Sys.time()
testSpan <- 700:nrow(gEnv$zhF)
filtFuncLst.hard <- getFiltLst.hard()
fHardRstLst <- list() # 각 hIdx에서 걸린 필터들의 ID
for( hIdx in testSpan ){

	tEnv <- list( allZoidMtx = gEnv$zhF[(hIdx-1):hIdx,]
					,zhF = gEnv$zhF[1:(hIdx-1),]
					,logFile = "./log/test.log"
					,doLog = TRUE
				)
	tEnv$log <- function( pMsg ){ if(tEnv$doLog) k.FLog(pMsg ,pFile=tEnv$logFile) }
	tEnv$logStr <- function( pMsg ){ if(tEnv$doLog) k.FLogStr( pMsg ,pFile=tEnv$logFile) }

	remFlag <- rep( F ,length(filtFuncLst.hard) )
	for( fIdx in 1:length(filtFuncLst.hard) ){
		rstObj <- filtFuncLst.hard[[fIdx]]( tEnv )	# 소요시간 rstObj$tCost
		remFlag[fIdx] <- !rstObj$flag[2]
	} # fIdx

	fHardRstLst[[as.character(hIdx)]] <- remFlag

	if( 0==(hIdx%%100) ){
		k.FLogStr(sprintf("current test : %d",hIdx))
	}

} # hIdx
tDiff <- Sys.time() - tStmp

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

fltFlag <- sapply( rstLst ,length )	;names(fltFlag) <- testSpan	# table(fltFlag)
fltDensity <- do.call( c ,rstLst )	# table(fltDensity)
fltDensity.tbl <- sort( table(fltDensity) ,decreasing=T )

# fltFlag 기본
# 	0   1   2   3   4   5   6   7   8   9  10  11 
# 	6  54 126 130 124  66  45  22  17   8   3   3 
exceptFlt <- c("cutEadge.getBanPtnColVal","cutEadge.banSeqRebCStep","cutEadge.getCFltObj","cutEadge.banSeqRebCStep")
fltFlag.exc <- sapply( rstLst ,function( fn ){
						length( setdiff(fn,exceptFlt) )
					})
names(fltFlag.exc) <- testSpan	# table(fltFlag)


hardFlag <- sapply( rstLst ,function(fn){ any(fn=="cutEadge.remLstHard") })
names(hardFlag) <- testSpan	# table(fltFlag)

hardFlag.name <- names(hardFlag)[which(hardFlag)]
hardFlag.name <- hardFlag.name[ hardFlag.name %in% names(stdFiltedCnt) ]

stdFiltedCnt[hardFlag.name]
