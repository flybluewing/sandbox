# rpt.opgRawPtn() 함수 실행 및 결과물에 대한 Review 예제.
source("20170224_B_H.R")
source("20170224_B_auxH.R")
source("20170224_B_Run_H.R")


curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd( curWd )

fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )

k.FLogStr("Log Init",pAppend=F)

# wIdxLst <- as.list( c(7,34,42) )	# seq(1,nrow(FB$zh)-7,10)
wIdxLst <- as.list( c(seq(1,nrow(FB$zh)-7,10),seq(2,nrow(FB$zh)-7,10),seq(4,nrow(FB$zh)-7,10),seq(7,nrow(FB$zh)-7,10)) )
tLogLst <- list( start=Sys.time() )
for( initIdx in seq_along(wIdxLst) ){ # initIdx <- 1
	wIdx <- wIdxLst[[initIdx]]
	saveFileName <- sprintf("Obj_rawPtnObjGrp%03d.save",wIdx)
	winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
							,pHeight=7 ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
						)
	rawPtn <- scanRawPattern( winDef ,pFiltF=filtF.lastRowMiss ,pBuffSize=1 ,pDebug=T )
	ptnObjGrp <- list( winDef=winDef ,rawPtn=rawPtn )
	save( ptnObjGrp ,file=saveFileName )
	tLogLst[[ as.character(wIdx) ]] <- Sys.time()
}

tLogLst[[(length(tLogLst)+1)]] <- Sys.time()



# =======================================================================================================
ptnObjDir.save	<- "./save/ptnObjGrp"
ptnObjDir.rpt	<- "./report/rptObj"
ptnObjFile <- dir( ptnObjDir.save ,pattern="\\.save")

# ptnObjFile <- ptnObjFile[1]	# 모든 거 돌리면 너무 오래걸릴 수 있으니깐.
rptObjFileLst <- list()
for( fIdx in ptnObjFile ){
	rptObj <- rpt.opgRawPtn( paste(ptnObjDir.save,fIdx,sep="/") ,pRptFile=paste(ptnObjDir.rpt,fIdx,sep="/") 
								,pRptAppend=F ,pNoPredCol=F ,pGetResult=T  
							)
	saveFile <- paste(ptnObjDir.rpt,gsub("\\.save$","rpt.save",fIdx),sep="/")
	save( rptObj ,file=saveFile )
	rptObjFileLst[[(length(saveFile)+1)]] <- saveFile
	k.FLogStr(sprintf(" finish fIdx report.",fIdx))
}


