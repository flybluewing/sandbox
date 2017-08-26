# rpt.opgRawPtn() 함수 실행 및 결과물에 대한 Review 예제.
source("20170224_B_H.R")
source("20170224_B_auxH.R")
source("20170224_B_Run_H.R")


sfInit( parallel=T ,cpus=1 )
sfExport("k.FLogStr")			;sfExport("k.FLogOpt")		;sfExport("k.FLog")
sfExport("scanRawPattern")		;sfExport("scanRawPattern.filtF")
sfExport("filtF.lastRowMiss")	;sfExport("findOverlap")	;sfExport("findOverlap.within")


ptnObjDir.save	<- "./save/rawPtnObjGrp"
ptnObjDir.rpt	<- "./report/rptObj"
ptnObjFile <- dir( ptnObjDir.save ,pattern="\\.save")

colRV <- NULL	# Column Remove Value, values that should be removed for too much haunt
if( 0<length(ptnObjFile) ){
	myObj <- load( paste(ptnObjDir.save,ptnObjFile[1],sep="/") ) # ptnObjGrp
	field <- ptnObjGrp$winDef$field
	colRV <- rep(NA,ncol(ptnObjGrp$winDef$field))

	for( cIdx in 1:ncol(field) ){ # cIdx <- 31
		tblR <- table( field[,cIdx] )
		tblR <- tblR / sum(tblR)
		if( 0.66 < max(tblR) ){	# 2/3 이상 과반하는 값은 제외하자.
			colRV[cIdx] <- as.integer(names(which.max(tblR)))
		} # if
	} # for
	ptnObjGrp <- NULL   ;field <- NULL
}


# =======================================================================================================
# ptnObjFile <- ptnObjFile[1]	# 모든 거 돌리면 너무 오래걸릴 수 있으니깐.
sfExport("ptnObjDir.save")		;sfExport("ptnObjDir.rpt")
sfExport("rpt.opgRawPtn")		;sfExport("scanMatchLst")	;sfExport("getMatMtxAccum")	;sfExport("assessClue")
rptObjFileLst <- sfLapply( as.list(ptnObjFile) ,function(fName){
						rptObj <- rpt.opgRawPtn( paste(ptnObjDir.save,fName,sep="/") ,pRptFile=paste(ptnObjDir.rpt,fName,sep="/") 
													,pRptAppend=F ,pPredCpNum=50 ,pNoPredCol=T
													,pColRV=colRV
												)
						saveFile <- paste(ptnObjDir.rpt,gsub("\\.save$","rpt.save",fName),sep="/")
						save( rptObj ,file=saveFile )
						k.FLogStr(sprintf(" finish report. (%s)",fName))
					})	# sfLapply

# =======================================================================================================
rptObjFile <- dir( ptnObjDir.rpt ,pattern="[[:digit:]]rpt\\.save$")
# rptObjFile <- rptObjFile[1]
for( fIdx in rptObjFile ){
	rptMObj <- getM.rpt.opgRawPtn( paste(ptnObjDir.rpt,fIdx,sep="/") )
	saveFile <- paste(ptnObjDir.rpt,gsub("rpt\\.save$","rptM.save",fIdx),sep="/")
	save( rptMObj ,file=saveFile )
	k.FLogStr(sprintf("  saved in %s",saveFile))
}

# =======================================================================================================
curWorkDir <- getwd()
setwd("..")

FB722 <- getFlagBank("./zoidHistory/ZH722.csv")
setwd(curWorkDir)

fb722Obj <- buildField( pFieldMtx=as.matrix(FB722$zh) )
	# dim( fb722Obj$vFieldMtx )

winDef4Vld <- getWindow( pField=fb722Obj$vFieldMtx	,pFilm=fb722Obj$filmMtx
						,pHeight=7 ,pInitPos=1 ,pIdStr="validateWD"
					)

# =======================================================================================================
rptSummary.dir <- "./report/rptObjSummary"
rptMObjFile <- dir( ptnObjDir.rpt ,pattern="[[:digit:]]rptM.save" )
# rptMObjFile <- rptMObjFile[1]
assRptLst <- list()
for( fIdx in rptMObjFile ){
	rptFile <- gsub(".save","",fIdx)
	selMtx <- rptMObjSummary( pSaveFile=paste(ptnObjDir.rpt,fIdx,sep="/") 
									# ,pRptFile=paste(rptSummary.dir,rptFile,sep="/") 
									,pWinDef=winDef4Vld ,pRptAppend=T
							)
	if( !is.null(assRptLst) ){
		assRptLst[[fIdx]] <- selMtx
	}
}
save( assRptLst ,file="Obj_assRptLst.save" )
mtx <- do.call( rbind, selMtxLst )
hitDiff <- mtx[,"pred.hauntRate"] - mtx[,"base.hitRate"]

# pSaveFile=paste(ptnObjDir.rpt,fIdx,sep="/")	;pRptFile=paste(rptSummary.dir,rptFile,sep="/")
#	pWinDef=winDef4Vld	;pRptMsg="none" ;pRptAppend=T
# QQE:working
rptMObjSummary <- function( pSaveFile ,pRptFile="./report/rptMObjSummary" ,pWinDef ,pRptMsg="none" ,pRptAppend=T ){

		log.txt <- sprintf("%s.txt",pRptFile)
		log.png <- sprintf("%s.png",pRptFile)
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				if( !is.null(pRptFile) )
					k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr("=====================================================================",pAppend=pRptAppend,pTime=T)
		FLogStr(sprintf("rptMObj loading start from %s",pSaveFile),pTime=T)
		loadedObjName <- load( pSaveFile ) # rptMObj

		assLst <- list()
		matMtx	<- matrix(0,nrow=pWinDef$height,ncol=pWinDef$width)
		maskMtx	<- matrix(F,nrow=pWinDef$height,ncol=pWinDef$width)
		for( pIdx in seq_along(rptMObj$ptnLst) ){
			# pIdx <- 1   # pred Idx
			for( cIdx in seq_along(rptMObj$ptnLst[[pIdx]]$clueLst) ){
				# cIdx <- 1 # clue Idx
				matMtx[,]	<- rptMObj$ptnLst[[pIdx]]$predMatMtx
				maskMtx[,]	<- !is.na(rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$matMtx)
				matMtx[maskMtx] <- rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$matMtx[maskMtx]
				ass <- assessClue( pWinDef=pWinDef ,pMatMtx=matMtx )
				# save 파일에 대한 기록정보도 필요할 듯
				ass$predIdx <- rptMObj$ptnLst[[pIdx]]$predPtnIdx
				ass$clueIdx <- rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$cluePtnIdx
				ass$matMtx <- matMtx
				ass$hauntIdx <- sapply( ass$clueFindLst ,function(p){p$hIdx} )
				ass$matchFlag <- sapply( ass$clueFindLst ,function(p){all(p$matchFlag)} )
				ass$base.hauntNum <- rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$hauntNum
				ass$base.hitRate <- rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$hitRate

				# revAss : Reverse Assessment Object. 
				#	use for searching pred haunt
				clueFlagMtx <- !is.na(matMtx)
				clueFlagMtx[nrow(matMtx),] <- F
				revAss <- assessClue( pWinDef=pWinDef ,pMatMtx=matMtx ,pPredFlagMtx= )
				ass$hauntIdx.pred <- sapply( revAss$clueFindLst ,function(p){p$hIdx} )

				assLst[[(1+length(assLst))]] <- ass
			} # for(cIdx)
		} # for(pIdx)

		hauntNum<- rep(0,length(assLst)) # haunt number in validation area
		hitNum	<- rep(0,length(assLst)) # hit number within validation area
		rateDiff<- rep(0,length(assLst)) # hit rate change
		for( aIdx in seq_along(assLst) ){ # aIdx <- 1
			hauntNum[aIdx]	<- length(assLst[[aIdx]]$hauntIdx) - assLst[[aIdx]]$base.hauntNum
			hitNum[aIdx]	<- sum(assLst[[aIdx]]$matchFlag[-(1:assLst[[aIdx]]$base.hauntNum)])
			rateDiff[aIdx]	<- assLst[[aIdx]]$base.hitRate - assLst[[aIdx]]$hitRate
		}

		base.hitRate	<- sapply(assLst,function(p){p$base.hitRate})
		base.hauntNum	<- sapply(assLst,function(p){p$base.hauntNum})
		idxNum			<- sapply(assLst,function(p){sum(!is.na(p$matMtx))})
		pred.hauntRate	<- sapply(assLst,function(p){p$predHauntRate})

		if( 0==length(base.hitRate) ){
			FLogStr("  No result in assLst ")
			return( NULL )
		}


		for( cIdx in which(base.hitRate>75) ){ # cIdx <- 4
			FLogStr(sprintf("%dth assLst hit %d%% ",cIdx,assLst[[cIdx]]$base.hitRate ))
			FLog( getIdxMtx( assLst[[cIdx]]$matMtx ) )
		}

		png( log.png ,height=479*2 ,width=479 )
		par( mfrow=c(3,2) ,mar=c(5.1, 5.1, 4.1, 2.1) )
		# pred의 자연적인 발생 확률보다 base.hitRate가 월등히 높아야 쓸모가 있는데..
		#	Validation 대상에 대한 hitRate 상승/하강 여부도 색으로써 표현해주자.
		plot( jitter(base.hitRate),jitter(pred.hauntRate) ,xlim=c(45,105) ,ylim=c(-5,105) ,cex.lab=1.5 ,cex.axis=1.5 )
		lines( c(45,100) ,c(45,100) )

		plot(base.hitRate,base.hauntNum ,xlim=c(40,100) ,cex.lab=1.5 ,cex.axis=1.5 )	# 1) haunt number와 hit rate 의 plot
		plot(base.hitRate,idxNum ,xlim=c(40,100) ,cex.lab=1.5 ,cex.axis=1.5 )			# 2) idx 수와 haunt number의 plot
		hist(base.hitRate ,xlim=c(40,100) ,cex.lab=1.5 ,cex.axis=1.5 )
		plot(base.hauntNum,idxNum ,cex.lab=1.5 ,cex.axis=1.5 )

		plot( base.hitRate ,(hitNum*100)/hauntNum ,xlim=c(40,100) ,cex.lab=1.5 ,cex.axis=1.5 )
		dev.off()
		
		rObj <- list( assLst=assLst ,perfMtx=cbind( base.hitRate ,pred.hauntRate ,idxNum ) )
		rObj$rptSaveFile <- rptMObj$rptSaveFile
		rObj$rptMObjFile <- pSaveFile
		return( rObj )

	} # rptMSummary

