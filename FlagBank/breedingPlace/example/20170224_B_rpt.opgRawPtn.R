# rpt.opgRawPtn() 함수 실행 및 결과물에 대한 Review 예제.
source("20170224_B_H.R")
source("20170224_B_auxH.R")
source("20170224_B_Run_H.R")


# =======================================================================================================
ptnObjDir.save	<- "./save/ptnObjGrp"
ptnObjDir.rpt	<- "./report/rptObj"
ptnObjFile <- dir( ptnObjDir.save ,pattern="\\.save")

# ptnObjFile <- ptnObjFile[1]	# 모든 거 돌리면 너무 오래걸릴 수 있으니깐.
rptObjFileLst <- list()
for( fIdx in ptnObjFile ){
	rptObj <- rpt.opgRawPtn( paste(ptnObjDir.save,fIdx,sep="/") ,pRptFile=paste(ptnObjDir.rpt,fIdx,sep="/") ,pRptAppend=F ,pGetResult=T  )
	saveFile <- paste(ptnObjDir.rpt,gsub("\\.save$","rpt.save",fIdx),sep="/")
	save( rptObj ,file=saveFile )
	rptObjFileLst[[(length(saveFile)+1)]] <- saveFile
}


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
						,pHeight=3 ,pInitPos=1 ,pIdStr="validateWD"
					)
					
rptSummary.dir <- "./report/rptObjSummary"
rptMObjFile <- dir( ptnObjDir.rpt ,pattern="[[:digit:]]rptM.save" )
# rptMObjFile <- rptMObjFile[1]
for( fIdx in rptMObjFile ){
	rptMObjSummary( pSaveFile=paste(fIdx,ptnObjDir.rpt,sep="/") ,pRptFile=paste(rptSummary.dir,"rpt",sep="/") ,pWindDef=winDef4Vld ,pRptAppend=F )
}

# pSaveFile=	;pRptFile=	;pWinDef=winDef4Vld	;pRptMsg="none" ;pRptAppend=T
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

		png( log.png )
		par( mfrow=c(2,3) )
		# pred의 자연적인 발생 확률보다 base.hitRate가 월등히 높아야 쓸모가 있는데..
		#	Validation 대상에 대한 hitRate 상승/하강 여부도 색으로써 표현해주자.
		plot( jitter(base.hitRate),jitter(pred.hauntRate) ,xlim=c(45,105) ,ylim=c(45,105) )
		lines( c(45,100) ,c(45,100) )

		plot(base.hitRate,base.hauntNum)	# 1) haunt number와 hit rate 의 plot
		plot(base.hitRate,idxNum)			# 2) idx 수와 haunt number의 plot
		hist(base.hitRate)
		plot(base.hauntNum,idxNum)

		plot( base.hitRate ,(hitNum*100)/hauntNum )
		dev.off()

	} # rptMSummary

