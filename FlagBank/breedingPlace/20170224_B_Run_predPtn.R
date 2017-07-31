# pred 패턴들에 대해서만 Overlap 수행함으로써 compPair 증가시도. haunt 향상 시도
# myObj <- load("Obj_ptnObjGrp017.save")

rpt.opgRawPtn( "Obj_ptnObjGrp017.save" )

saveDir <- "E:/zproject/DataStore/ptnObjGrp/save"
saveFiles <- dir(saveDir,pattern="\\.save$")
reportDir <- "./report/ptnObjGrp"
k.FLogStr(sprintf("Start save files (%d files)",length(saveFiles)),pConsole=T)
for( fName in saveFiles ){
	rpt.opgRawPtn( paste(saveDir,fName,sep="/") ,paste(reportDir,gsub("\\.save$","",fName),sep="/") )
}
k.FLogStr(sprintf("Finish save files"),pConsole=T)

# ------------------------------------------------------------------

sfInit( parallel=T ,cpus=2 )
sfExport("k.FLogStr");			sfExport("k.FLogOpt")
sfExport("scanRawPattern");		sfExport("scanRawPattern.filtF")
sfExport("filtF.lastRowMiss");	sfExport("findOverlap")


saveDir <- "./temp"
saveFiles <- dir(saveDir,pattern="\\.save$")
saveFileLst <- as.list(saveFiles)

sfExport("saveDir")
sfExport("rpt.opgRawPtn")	;sfExport("scanMatchLst")	;sfExport("getMatMtxAccum")
sfExport("findOverlap.within")	;sfExport("assessClue")
k.FLogStr(sprintf("Start save files (%d files)",length(saveFiles)),pConsole=T)
rptFileLst <- sfLapply( saveFileLst ,function( fName ){
					rptFile <- gsub(".save$",".rpt.save",fName)
					rpt.opgRawPtn( paste(saveDir,fName,sep="/") ,paste("./temp/test",rptFile,sep="/") ,pGetResult=T )
					return(rptFile)
				})
k.FLogStr(sprintf("Finish save files"),pConsole=T)


# ------------------------------------------------------------------
#	investigate rptMObj
#		rptMObj : rpt.opgRawPtn() -> getM.rpt.opgRawPtn()
#                 rptObj             rptMObj
# ------------------------------------------------------------------
#	k.FLogStr("start",pConsole=T)
#	rptObj <- rpt.opgRawPtn( "./save/ptnObjGrp/Obj_ptnObjGrp007.save" ,"./temp/test/testRpt" ,pGetResult=T  )
#	save( rptObj ,file="Obj_rptObj.save" )
#	rptMObj <- getM.rpt.opgRawPtn( "Obj_rptObj.save" )
#	k.FLogStr("end",pConsole=T)


curWorkDir <- getwd()
setwd("..")

FB722 <- getFlagBank("./zoidHistory/ZH722.csv")
setwd(curWorkDir)

fb722Obj <- buildField( pFieldMtx=as.matrix(FB722$zh) )
	# dim( fb722Obj$vFieldMtx )

winDef4Vld <- getWindow( pField=fb722Obj$vFieldMtx	,pFilm=fb722Obj$filmMtx
						,pHeight=3 ,pInitPos=1 ,pIdStr="validateWD"
					)

myObj <- load("./report/ptnObjGrpM/Obj_ptnObjGrp007.rptM.save") # rptMObj

assLst <- list()
matMtx	<- matrix(0,nrow=winDef4Vld$height,ncol=winDef4Vld$width)
maskMtx	<- matrix(F,nrow=winDef4Vld$height,ncol=winDef4Vld$width)
for( pIdx in seq_along(rptMObj$ptnLst) ){
	# pIdx <- 1   # pred Idx
	for( cIdx in seq_along(rptMObj$ptnLst[[pIdx]]$clueLst) ){
		# cIdx <- 1 # clue Idx
		matMtx[,]	<- rptMObj$ptnLst[[pIdx]]$predMatMtx
		maskMtx[,]	<- !is.na(rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$matMtx)
		matMtx[maskMtx] <- rptMObj$ptnLst[[pIdx]]$clueLst[[cIdx]]$matMtx[maskMtx]
		ass <- assessClue( pWinDef=winDef4Vld ,pMatMtx=matMtx )
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

# pred의 자연적인 발생 확률보다 base.hitRate가 월등히 높아야 쓸모가 있는데..
#	Validation 대상에 대한 hitRate 상승/하강 여부도 색으로써 표현해주자.
plot( jitter(base.hitRate),jitter(pred.hauntRate) ,xlim=c(45,105) ,ylim=c(45,105) )
lines( c(45,100) ,c(45,100) )

plot(base.hitRate,base.hauntNum)	# 1) haunt number와 hit rate 의 plot
plot(base.hitRate,idxNum)			# 2) idx 수와 haunt number의 plot
hist(base.hitRate)
plot(base.hauntNum,idxNum)

plot( base.hitRate ,(hitNum*100)/hauntNum )


groupNum <- 2
km.out <- kmeans( base.hitRate ,groupNum ,nstart=3 )
# plot( jitter(base.hitRate) ,jitter((hitNum*100)/hauntNum) ,col=km.out$cluster )

groupIdx <- tapply( base.hitRate ,km.out$cluster ,mean ) %>% which.max

assLst.chosen <- assLst[ km.out$cluster==groupIdx ]


do.call( c ,lapply(assLst.chosen,function(p){p$predIdxMtx[,2]})  )

mtx4Pr <- cbind(base.hitRate,base.hauntNum,idxNum)
km.out <- kmeans( scale(mtx4Pr) ,2 ,nstart=nrow(mtx4Pr)%/%10 )
rownames(mtx4Pr) <- km.out$cluster

pr.out <- prcomp( mtx4Pr ,scale=T )
biplot(pr.out ,scale=0 )


# 3) 1,2 각각 및 1&2 에 대한 unsupervised group 시도
# 선별된 clue에 대한 valildation에서의 검토 : haunt수(5개 이상), hit rate



























