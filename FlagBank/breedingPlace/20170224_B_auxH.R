# pInvM.ana<- anaLst[[9]] ;pRptFile="./report/invMAna" ;pRptMsg="none"	;pRptAppend=T
report.invMAna <- function( pInvM.ana ,pRptFile="./report/invMAna" ,pRptMsg="none" ,pRptAppend=T ){

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		valuableRowNum <- 0

		FLogStr("=====================================================================\n")
		FLogStr(sprintf("invM (matchLst.idx:%3d)",pInvM.ana$matchLst.idx),pAppend=pRptAppend )
		FLogStr(sprintf("    message : %s",pRptMsg) )
		if( !is.null(pInvM.ana$errMsg) ){
			FLogStr(sprintf("  Error message : %s",pInvM.ana$errMsg))
		}

		#	clue들의 가능성 검색.
		FLogStr(sprintf("Search by clue(%d candidates)",length(pInvM.ana$fnlCand)))
		flag <- scanGoodRow.cvrRateMtx( pInvM.ana$cvrMtx.clue ,pInvM.ana$cvrMtx.hu )
		if( any(flag) ){
			valuableRowNum <- valuableRowNum+sum(flag)
			rowFmtStr <- sprintf("%%%dd",ceiling(log10(nrow(pInvM.ana$cvrMtx.clue))))
			rName <- sprintf(rowFmtStr,1:nrow(pInvM.ana$cvrMtx.clue))
			FLogStr("  ---<RateMtx>---")
			FLogStr(k.matrixToStr(pInvM.ana$cvrMtx.clue[flag,,drop=F]
					,pFmt="%4.0f",pNA.str="  NA",pRowName=rName[flag]))
			FLogStr("  ---<cvrMtx>---")
			FLogStr(k.matrixToStr(pInvM.ana$cvrMtx[flag,,drop=F]
					,pFmt="%4.0f",pNA.str="  NA",pRowName=rName[flag]))
		} else {
			FLogStr("       found none.")
		}

		if( !is.null(pInvM.ana$cmbMtx) && !is.null(pInvM.ana$cmbCvrLst) ){
			#	combination 에 따른 가능성 있는 조합 검색.
			cvr <- ana.invM.getCvrMtx( pInvM.ana$huCompPair ,pInvM.ana$cmbCvrLst )
			mtxName <- attributes(cvr$cvrRateMtxLst.clue)$name
			FLogStr(sprintf("Search by clue combinations(%d matrixes)",length(mtxName)))
			FLogStr(sprintf("    final candidate size : %d",length(pInvM.ana$fnlCand)))
			FLogStr(sprintf("          candidate clue : %s",paste(pInvM.ana$fnlCand,collapse=",")))
			for( idx in 1:length(cvr$cvrRateMtxLst.clue) ){
				# idx <- 1
				FLogStr(sprintf("* CMB matrix: %s",mtxName[idx]))
				flag <- scanGoodRow.cvrRateMtx( cvr$cvrRateMtxLst.clue[[idx]] ,cvr$cvrRateMtxLst.hnt[[idx]] )
				if( any(flag) ){
					valuableRowNum <- valuableRowNum+sum(flag)
					mtx <- cvr$cvrRateMtxLst.clue[[idx]]
					# 실제 Row 이름은 cvr$typNameLst[[idx]] 참고.
					rowFmtStr <- sprintf("%%%dd",ceiling(log10(nrow(mtx))))
					rName = sprintf(rowFmtStr,1:nrow(mtx))
					FLogStr("  ---<RateMtx>---")
					FLogStr(k.matrixToStr(mtx[flag,,drop=F]
							,pFmt="%4.0f",pNA.str="  NA",pRowName=rName[flag]))
					FLogStr("  ---<cvrMtx>---")
					FLogStr(k.matrixToStr(cvr$cvrMtxLst[[idx]][flag,,drop=F]
							,pFmt="%4.0f",pNA.str="  NA",pRowName=rName[flag]))
				}
			}
		} # if

		if( 0 < valuableRowNum ){
			hntNumber <- sapply(pInvM.ana$huCompPair,length)
			FLogStr(sprintf("Haunt number : %s",paste(hntNumber,collapse=",")))
		}
		return( valuableRowNum )
	}

# rpt.opgRawPtn() : Search clue start from pred overlap
#		- pNoPredCol : remove clue columns when predict has same column & same value.
#	pSaveFile=	;
#	pRptFile="./report/report.ogpRawPtn" ;pRptMsg="none" ;pRptAppend=T	;pPredCpNum=100 ;pClueCvrRate=0.2 ;pGetResult=T	;pNoPredCol=F
rpt.opgRawPtn <- function( pSaveFile ,pRptFile="./report/report.ogpRawPtn" ,pRptMsg="none" ,pGetResult=T ,pPredCpNum=100 ,pClueCvrRate=0.2 ,pNoPredCol=F ,pRptAppend=T ){

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr("=====================================================================",pAppend=pRptAppend)
		FLogStr(sprintf("ptnObgGrp loading start from %s",pSaveFile),pTime=T)
		loadedObjName <- load( pSaveFile ) # ptnObjGrp 

		FLogStr(sprintf("          message : %s ",pRptMsg) )
		cpNum <- sapply(ptnObjGrp$rawPtn$matchLst ,function(p){cpV<-unique(as.vector(p$compPair));return(length(cpV))})
		FLogStr("CpNum of rawPtn")
		FLog( summary(cpNum) ,pTime=F )
		FLog( head(cpNum[order(cpNum,decreasing=T)],n=20) )

		# << pred overlap >>
		predFilm <- matrix(T ,nrow=ptnObjGrp$winDef$height ,ncol=ptnObjGrp$winDef$width )
		predFilm[ptnObjGrp$winDef$height,] <- F
		overlap <- findOverlap( ptnObjGrp$rawPtn ,pExcFilm=predFilm ,pDebug=T )
		cpNum <- sapply(overlap$matchLst ,function(p){cpV<-unique(as.vector(p$compPair));return(length(cpV))})
		FLogStr("CpNum of overlap from pred")
		FLog( summary(cpNum) ,pTime=F )
		FLog( head(cpNum[order(cpNum,decreasing=T)],n=20) )

		chosen <- which( cpNum>pPredCpNum )
		chosen <- chosen[order(cpNum[chosen],decreasing=T)]
		chosen.cpV <- lapply(overlap$matchLst[chosen],function(p){unique(as.vector(p$compPair))})
		FLogStr(sprintf("   %d pred pattern are chosen",length(chosen)))

		matMtx <- matrix( 0 ,nrow=ptnObjGrp$winDef$height ,ncol=ptnObjGrp$winDef$width )
		matMtx.pred <- matrix( F ,nrow=ptnObjGrp$winDef$height ,ncol=ptnObjGrp$winDef$width )

		# << clue search >>
		invLst <- list()
		for( choiceIdx in seq_along(chosen) ){ # choiceIdx <- 3

			ptnLst <- scanMatchLst( ptnObjGrp$rawPtn$matchLst[ chosen.cpV[[choiceIdx]] ]
										,pExcFilm = is.na(overlap$matchLst[[chosen[choiceIdx]]]$matMtx) )
			ptnLst[[1]]$excFilm[ptnObjGrp$winDef$height,] <- T # only for searching clue
			accObj <- getMatMtxAccum( pMatchLst=ptnObjGrp$rawPtn$matchLst ,pSearchArea=chosen.cpV[[choiceIdx]]
										,pExcFilm=ptnLst[[1]]$excFilm )
					# accObj for reducing clue search area
					# accum <- as.vector(accObj$accumMtx)	;accum <- accum[accum>1]	;hist(accum)

			# << clue overlap >>
			thld <- as.vector(accObj$accumMtx) # threshold
			thld <- thld[thld>0]
			thld <- quantile(thld[thld>0])["75%"]
			excFilm <- accObj$accumMtx < thld

			ol <- findOverlap.within( ptnObjGrp$rawPtn ,pIndices=chosen.cpV[[choiceIdx]] ,pExcFilm=excFilm ,pDebug=T )
			ol.cpNum <- sapply(ol$matchLst,function(p){cpV<-unique(as.vector(p$compPair));return(length(cpV))})
			ol.chosen <- which(ol.cpNum>round(cpNum[chosen[choiceIdx]]*pClueCvrRate)) # actually, hope more than 0.8
				# QQE: add report for ol.cpNum and cover rage of clue

			ol.chosen <- ol.chosen[order(ol.cpNum[ol.chosen],decreasing=T)]
			ol.chosen.cpV <- lapply(ol$matchLst[ol.chosen],function(p){ unique(as.vector(p$compPair)) })

			if( 0==length(ol.chosen) )
				next
			FLogStr(sprintf("overlap$matchLst[[%5d]] scan area : %5d, cpNum : %5d  ( %d of %d )"
						,chosen[choiceIdx],sum(!ptnLst[[1]]$excFilm),cpNum[chosen[choiceIdx]] ,choiceIdx ,length(chosen) ))
			FLogStr(sprintf("overlap$matchLst[[%5d]] clue overlap size : %d    chosen : %d",chosen[choiceIdx],length(ol$matchLst),length(ol.chosen) ))
			FLogStr(sprintf("                                     max cover : %d of %d",max(ol.cpNum),cpNum[chosen[choiceIdx]] ))

			# << clue assessment >>
			hitRateLst <- list()
			predHntRateLst <- list() # pred haunt rate in zoid history.
			hauntNum <- list()
			for( ocpIdx in seq_along(ol.chosen) ){ #  ocpIdx <- 1 # ocpIdx:idx for ol.chosen ptn
				matMtx[,] <- ol$matchLst[[ol.chosen[ocpIdx]]]$matMtx # clue
				matMtx.pred[,] <- !is.na(overlap$matchLst[[chosen[choiceIdx]]]$matMtx) # Position Flag for predict
				matMtx[matMtx.pred] <- overlap$matchLst[[chosen[choiceIdx]]]$matMtx[matMtx.pred]  # clue + pred
				# pWinDef=ptnObjGrp$winDef ;pMatMtx=matMtx ;pPredFlagMtx=NULL ;pScanArea=NULL
				assObj <- assessClue( pWinDef=ptnObjGrp$winDef ,pMatMtx=matMtx )
				
				hitRateLst[[(1+length(hitRateLst))]] <- assObj$hitRate
				hauntNum[[(1+length(hauntNum))]] <- length(assObj$clueFindLst)
				predHntRateLst[[(1+length(predHntRateLst))]] <- assObj$predHauntRate
			} # for(ocpIdx)

			if( 0<length(hitRateLst) ){ # same to length(ol.chosen). 'if' is required? really?
				hitRate <- do.call(c,hitRateLst)
				#idx4Max <- order(hitRate,decreasing=T)[1]
				idx4Max <- which.max(hitRate)
				FLogStr(sprintf("                                     max hit rate by clue : %d%%(from %d haunts)"
					,hitRateLst[[idx4Max]],hauntNum[[idx4Max]] ))

				if( pGetResult ){
					invObj <- list( chosen=chosen[choiceIdx]
										,ol=ol ,ol.cpNum=ol.cpNum ,ol.chosen=ol.chosen
										,hitRate=hitRate ,hauntNum=do.call(c,hauntNum) 
										,predHntRate=do.call(c,predHntRateLst) 
										,threshold = thld
									)
					invLst[[(1+length(invLst))]] <- invObj
				}
			}

		} # for(choiceIdx)

		FLogStr(" report end",pTime=T)
		
		if( pGetResult ){
			rObj <- list( overlap=overlap ,cpNum=cpNum ,chosen=chosen ,chosen.cpV=chosen.cpV )
			rObj$invLst <- invLst
			rObj$saveFile <- pSaveFile
			rObj$reportFile <- pRptFile

			return( rObj )
		} else {
			return( NULL )
		}

	} # rpt.opgRawPtn( )

getM.rpt.opgRawPtn <- function( pRptSaveFile ,pHitRate=NULL ){

			myObj <- load( pRptSaveFile ) # rptObj
			if( is.null(pHitRate) )
				pHitRate <- 50

			ptnLst <- list()
			for( invIdx in seq_along(rptObj$invLst) ){ # invIdx <- 1
				ptn <- list( predPtnIdx=rptObj$invLst[[invIdx]]$chosen )
				ptn$predMatMtx <- rptObj$overlap$matchLst[[ptn$predPtnIdx]]$matMtx
				ptn$predIdxMtx <- rptObj$overlap$matchLst[[ptn$predPtnIdx]]$idxMtx

				clueLst <- list()
				for( idx in seq_along(rptObj$invLst[[invIdx]]$ol.chosen) ){
					# clueIdx <- rptObj$invLst[[invIdx]]$ol.chosen[1]
					clueIdx <- rptObj$invLst[[invIdx]]$ol.chosen[idx]
					clueObj <- list( cluePtnIdx=clueIdx )
					clueObj$matMtx	<- rptObj$invLst[[invIdx]]$ol$matchLst[[clueIdx]]$matMtx
					clueObj$idxMtx	<- rptObj$invLst[[invIdx]]$ol$matchLst[[clueIdx]]$idxMtx
					clueObj$hitRate	<- rptObj$invLst[[invIdx]]$hitRate[idx]
					clueObj$hauntNum <-rptObj$invLst[[invIdx]]$hauntNum[idx]
					if( pHitRate <= clueObj$hitRate )
						clueLst[[(1+length(clueLst))]] <- clueObj
				} # for( clueIdx )

				if( 0<length(clueLst) ) {
					ptn$clueLst <- clueLst
					ptnLst[[ (1+length(ptnLst)) ]] <- ptn
				}
			} # for( invIdx )

			rObj <- list( rptSaveFile=pRptSaveFile )
			rObj$ptnLst <- ptnLst

			return( rObj )
		} # getM.rpt.opgRawPtn()








# scanRawPattern() 함수 결과물에 대한 레포팅.
#		pRP=winRstObj$winRstLst[[1]];	pRptFile=NULL
rpt.scanRawPattern <- function( pRP ,pRptFile="./report/scanRawPattern" ){

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr(sprintf("Report : scanRawPattern Obj (initPos:%d)",pRP$initPos),pAppend=F,pTime=T)

		rObj <- list( initPos=pRP$initPos )
		rObj$matchLst.num <- length(pRP$matchLst)
				
		cName <- c("hIdx","idxNum","compPairNum")
		sizeMtx <- matrix( 0 ,nrow=rObj$matchLst.num ,ncol=length(cName) )
		colnames(sizeMtx) <- cName
		if( 0 < rObj$matchLst.num ){
			for( idx in 1:rObj$matchLst.num ){
				sizeMtx[idx,"hIdx"]			<- pRP$matchLst[[idx]]$hIdx
				sizeMtx[idx,"idxNum"]		<- nrow(pRP$matchLst[[idx]]$idxMtx)
				sizeMtx[idx,"compPairNum"]	<- length(pRP$matchLst[[idx]]$compPair)
			}
		}
		rObj$sizeMtx <- sizeMtx
		rObj$compPair.tbl <- table(sizeMtx[,"compPairNum"])
		
		return( rObj )
	}

glance.scanRawPattern <- function( pRP ){
		cat(sprintf("idStr:%s  initPos:%d  matchLst size:%d",pRP$idStr,pRP$initPos,length(pRP$matchLst)))
		cat("\n")
		
		if( 0<length(pRP$matchLst) ){
			att.n <- attributes(pRP$matchLst[[1]])$names
			cat(sprintf("  matchLst[[n]] : %s",paste(att.n,collapse=",  ")))
			cat("\n")
		}
		
	}

glance.rawPtn <- function( pRawPtn ,pMatchLstIdx=1 ){
		cat("RawPtn glance ------------------------------------------\n")
		cat(sprintf("** %s matchLst:%d \n",pRawPtn$idStr.winDef,length(pRawPtn$matchLst)))
		size.idx 	<- sapply(pRawPtn$matchLst,function(p){return(nrow(p$idxMtx))})
		catStr <- capture.output(table(size.idx))
		cat(catStr,sep="\n")

		size.h		<- sapply(pRawPtn$matchLst,function(p){return(length(p$compPair))})
		catStr <- capture.output(table(size.h))
		cat(catStr,sep="\n")
		
		par( mfrow=c(2,2) )
		hist(size.idx);		hist(size.h);		plot( size.idx ,size.h )
		
		cat("\n")
		for( idx in 1:length(pMatchLstIdx) ){
			mat <- pRawPtn$matchLst[[ pMatchLstIdx[idx] ]]
			cat(sprintf("matchLst[[%d]]  size:%d\n%s\n",pMatchLstIdx[idx],length(mat$compPair),k.matrixToStr(mat$matMtx)))
		}
		cat("--------------------------------------------------------\n")
	}

glance.overlap <- function( pOl ,pMatchLstIdx=1 ){
		cat("Overlap glance -----------------------------------------\n")
		cat(sprintf("** winDef:%s  matchLst:%d \n",pOl$idStr.winDef,length(pOl$matchLst)))
		size.idx 	<- sapply(pOl$matchLst,function(p){return(nrow(p$idxMtx))})
		catStr <- capture.output(table(size.idx))
		cat(catStr,sep="\n")

		size.h		<- sapply(pOl$matchLst,function(p){s<-unique(as.vector(p$compPair));return(length(s))})
		catStr <- capture.output(table(size.h))
		cat(catStr,sep="\n")
		
		par( mfrow=c(2,2) )
		hist(size.idx);		hist(size.h);		plot( size.idx ,size.h )
		
		cat("\n")
		for( idx in 1:length(pMatchLstIdx) ){
			mat <- pOl$matchLst[[ pMatchLstIdx[idx] ]]
			compPair.v <- sort(unique(as.vector(mat$compPair)))
			cat(sprintf("matchLst[[%d]]  size:%d\n%s\n",pMatchLstIdx[idx],length(compPair.v),k.matrixToStr(mat$matMtx)))
		}
		
		cat("--------------------------------------------------------\n")
	}

tSample.winDef06 <- function( pFieldMtx=as.matrix(FB$zh) ){

		# -------------------------------------------------------
		# buildField( )
		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( F ,nrow=nrow(pFieldMtx) ,ncol=0 )
		
		fObj <- getFieldSet.dummy.ptn( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		rObj <- list( rawFieldMtx = vFieldMtx )
		rObj$vFieldMtx <- vFieldMtx
		rObj$vFieldMtx[!filmMtx] <- NA
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		# -------------------------------------------------------
		
		fbObj <- rObj
		win.height <- 3

		wIdx <- 1
		winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
								,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
							)
		return( winDef )
	} # tSample.winDef06()


save.winRstObj <- function( winRstObj ,chunk.size=5 ){

        
        winObj.size <- length(winRstObj$winRstLst)

        for( idx in 0:(winObj.size %/% chunk.size) ){
            curIdx <- 1:chunk.size + (idx*chunk.size)
            curIdx <- curIdx[curIdx<=winObj.size]
            if( 0==length(curIdx) ) 
                break
            fName <- sprintf("Obj_winObj.%04d.%04d.save",curIdx[1],curIdx[length(curIdx)])
            k.FLogStr(sprintf(" saving %s",fName))
            
            winRstUnit <- list( winRstLst=winRstObj$winRstLst[curIdx] )
            winRstUnit$winDefLst <- winRstObj$winDefLst[curIdx]
            winRstUnit$idx <- curIdx
            save( winRstUnit, file=fName )
        }

        return("finish save.")
    }
