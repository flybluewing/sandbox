# Z825
source("./toFinal/toZ825_H.R")

saveId <- "Z824"	;rpt=TRUE
load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))
allZoidGrpName <-"allZoid.idx0"	# 
allIdx <- allIdxLst[[allZoidGrpName]]	#   
allIdxF <- 1000:1010		;stdZoid <- NULL

# simMode start ----------------------------------------------------
	aZoid <- stdZoid <- c( ,,,,,,,, ) # ZH 825 채워넣을 것.
	allIdxF <- allIdx <- stdIdx <- which(apply(gEnv$allZoidMtx,1,function(zoid){all(zoid==stdZoid)}))
# simMode end   ----------------------------------------------------
finalCut <- function( gEnv ,allIdx ,allZoidGrpName ){
    # cutEadge.getBanPtnColVal() 에서 1~2개 발생 탈락값들에 대한 검토 권장.
	allIdxFObj <- list()
	# 참고 자료 --------------------------------------------------------------------	
	fCutU.rptColValSeqNext( gEnv ,allIdxF ,sprintf("toZ%d",nrow(gEnv$zhF)+1) )
	# aQuoTblLst <- fCutU.getQuoTblLst( gEnv$allZoidMtx[allIdxF,] )
	# aQuoTblStr <- sapply( aQuoTblLst ,function(quoTbl){quoTbl$valStr})	;table(aQuoTblStr)

    allIdxF <- allIdx
	stdMI <- fCutU.getMtxInfo( gEnv$zhF )	#	rptObj<-anaMtx( stdMI$rawTail )	# u0.zoidMtx_ana( stdMI$rawTail )


	tStmp <- Sys.time()
	# 기본제거 --------------------------------------------------------------------	
	allIdxF <- fCut.default( gEnv ,allIdxF )

	# colValSeqNext ------------------------------------------------------
	flgCnt <- fCutCnt.colValSeqNext( gEnv ,allIdxF ) 
	flgCnt <- flgCnt + fCutCnt.colValSeqNext.cStep( gEnv ,allIdxF )
	flgCnt <- flgCnt + fCutCnt.default( gEnv ,allIdxF )	# 효율이 의문시된다.
	flag <- flgCnt<2	;table(flag)
    allIdxF <- allIdxF[flag]
	allIdxF.bak <- allIdxF
	allIdxFObj$allIdxF.colValSeqNext <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF <- fCut.rawFV3(  gEnv ,allIdxF  )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))

	allIdxF <- fCut.basic( gEnv ,allIdxF )
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	allIdxFObj$allIdxF.fCut <- allIdxF
	tDiff <- Sys.time() - tStmp	

	# ------------------------------------------------------------------
	# fCutCnt.**
	#		각 파트에서 2 이상씩은 잘라낸 후,
	#		전체 파트에서 하나도 안 걸린 것들은 제외시키자.
	# ccObj <- fCutCnt.default( gEnv ,allIdxF )
	ccObj <- fCutCnt.basic( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextZW( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextQuo10( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextRebNum( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextCStepBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextFStepBin( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_1( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_2( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_4( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_5( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	ccObj <- fCutCnt.nextColVal_6( gEnv ,allIdxF )
	allIdxF <- allIdxF[ cutCC( ccObj ,allIdxF ) ]
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	save( allIdxF ,file="Obj_allIdxF.save" )

	tDiff <- Sys.time() - tStmp	
	allIdxFObj$allIdxF.fCutCnt <- allIdxF

	ccObjLst <- list()
	ccObjLst[["basic"		]] <- fCutCnt.basic( 		gEnv ,allIdxF )
	ccObjLst[["nextZW"		]] <- fCutCnt.nextZW( 		gEnv ,allIdxF )
	ccObjLst[["nextQuo10"	]] <- fCutCnt.nextQuo10( 	gEnv ,allIdxF )
	ccObjLst[["nextBin"		]] <- fCutCnt.nextBin( 		gEnv ,allIdxF )
	ccObjLst[["nextRebNum"	]] <- fCutCnt.nextRebNum( 	gEnv ,allIdxF )
	ccObjLst[["nextCStepBin"]] <- fCutCnt.nextCStepBin( gEnv ,allIdxF )
	ccObjLst[["nextFStepBin"]] <- fCutCnt.nextFStepBin( gEnv ,allIdxF )
	ccObjLst[["nextColVal_1"]] <- fCutCnt.nextColVal_1( gEnv ,allIdxF )
	ccObjLst[["nextColVal_2"]] <- fCutCnt.nextColVal_2( gEnv ,allIdxF )
	ccObjLst[["nextColVal_3"]] <- fCutCnt.nextColVal_3( gEnv ,allIdxF )
	ccObjLst[["nextColVal_4"]] <- fCutCnt.nextColVal_4( gEnv ,allIdxF )
	ccObjLst[["nextColVal_5"]] <- fCutCnt.nextColVal_5( gEnv ,allIdxF )
	ccObjLst[["nextColVal_6"]] <- fCutCnt.nextColVal_6( gEnv ,allIdxF )


	# surFlag(scoreMtx) ------------------------------------------------------
	surFlag <- rep( TRUE ,length(allIdxF) )
	fName <- attributes(ccObjLst)$names

	cName <- c( "ccc", "auxZW", "auxQuo", "raw", "rawFV", "rem", "cStep", "fStep" )
	thld <- c( 2, 2, 2, 2, 2, 3, 2, 2 )	;names(thld) <- cName

	scoreMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(thld) )
	rownames(scoreMtx) <- fName		;colnames(scoreMtx) <- names(thld)

	cName <- c( "reb",  "nbor", "spanM", "quoAll", "quoPtn", "zw",  "remH0", "remH1", "cStep2", "cStep3" )	
	cccMtx <- matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )	
	rownames(cccMtx) <- fName		;colnames(cccMtx) <- cName
	cccMtx.rCol <- cName[ cName!="reb" ]

	cName <- c( "c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2" )	# cccObj$cStepValMtx 
	cStepValMtx = matrix( 0 ,nrow=length(ccObjLst) ,ncol=length(cName) )
	rownames(cStepValMtx) <- fName		;colnames(cStepValMtx) <- cName

	for( aIdx in 1:length(allIdxF) ){
		scoreMtx[,] <- 0	;cccMtx[,] <- 0		;cStepValMtx[,] <- 0
		for( nIdx in fName ){
			cccVal <- ccObjLst[[nIdx]]$cccMtx[aIdx,]
			cccMtx[nIdx,] <- ccObjLst[[nIdx]]$cccMtx[aIdx,]

			scoreMtx[nIdx,"ccc"] <- sum( 0 < cccVal[cccMtx.rCol] )
			scoreMtx[nIdx,c("auxZW", "auxQuo")] <- ccObjLst[[nIdx]]$auxCntMtx[aIdx,c("auxZW", "auxQuo")]
			scoreMtx[nIdx,c("raw", "rawFV", "rem", "cStep", "fStep")] <-
				ccObjLst[[nIdx]]$cntMtx[aIdx,c("raw", "rawFV", "rem", "cStep", "fStep")]

			cStepValMtx[nIdx,] <- ccObjLst[[nIdx]]$cStepValMtx[aIdx,]

		} # for(nIdx)

		cutCnt <- 0

		eventFlag <- apply(scoreMtx ,1 ,function(score){ sum(score>=thld) })
		if( any( 0<eventFlag[c("nextZW","nextBin","nextColVal_3","nextColVal_6")] ) ){ # gold cut기준을 표준으로 적용.
			surFlag[aIdx] <- FALSE
			next
		}
		eventCnt <- sum( eventFlag )
		if( (1>eventCnt) || (eventCnt>2) ){ # gold cut기준을 표준으로 적용.
			surFlag[aIdx] <- FALSE
			next
		}

		# filt for gold
		flagCnt.gold <- finalFilt.gold( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol )
		if( 5<flagCnt.gold ){
			surFlag[aIdx] <- FALSE
			next
		}
		# filt for late
		flagCnt.late <- finalFilt.late( scoreMtx ,cccMtx ,cStepValMtx ,thld ,cccMtx.rCol )
		if( 5<flagCnt.late ){
			surFlag[aIdx] <- FALSE
			next
		}

		surFlag[aIdx] <- ( 1 >= (cutCnt+flagCnt.gold+flagCnt.late) )

	}  # aIdx

	table(surFlag)	;kIdx <- head(which(!surFlag))
	allIdxF <- allIdxF[ surFlag ]
	allIdxFObj$allIdxF.surFlag <- allIdxF
	cat(sprintf("allIdxF %d\n",length(allIdxF)))
	# save( allIdxF ,file="Obj_allIdxF.save" )


	tDiff <- Sys.time() - tStmp
	allIdxFObj$timeCost <- tDiff
	allIdxFObj$allIdxF.final <- allIdxF
	save( allIdxFObj ,file=sprintf("Obj_allIdxFObj_%s.save",allZoidGrpName) )


	fCutU.logAllZoidMtx( gEnv$allZoidMtx[ allIdxFObj$allIdxF.final ,,drop=F] 
					,logId=sprintf("final_nextOf%s_%s",saveId,allZoidGrpName) 
				)


    return( rObj )

} # finalCut()

finalCut.test <- function(){
	# 1 2 3 / 1 4 1	<- lastZoid quo
	quoLst <- apply( gEnv$zhF ,1 ,fCutU.getQuoObj )
	quoFlag <- sapply( quoLst ,function( quoObj ){
					return( quoObj$sameTbl(c(1,4,1)) )
				})
	qIdx <- which(quoFlag)
	qIdx <- setdiff( qIdx ,length(quoLst) )
	preTbl <- sapply( quoLst[qIdx-1] ,function(quoObj){ paste(quoObj$tbl,collapse=" " )})
	postTbl <- sapply( quoLst[qIdx+1] ,function(quoObj){ paste(quoObj$tbl,collapse=" " )})
	cbind( preTbl ,postTbl )
} # finalCut.test()

