library(snowfall)
sfInit( parallel=T ,cpus=4 )
sfExport("k.FLogStr");		sfExport("k.FLogOpt")
sfExport("scanRawPattern");		sfExport("scanRawPattern.filtF")

# source("./breedingPlace/20170224_B_H.R")
#
FB <- getFlagBank()

# buildField() 테스트용 버전.
buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( F ,nrow=nrow(pFieldMtx) ,ncol=0 )
		
		# getFieldSet.left( ) -------------------------------------------------
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=4 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		rObj <- list( rawFieldMtx = vFieldMtx )
		rObj$vFieldMtx <- vFieldMtx
		rObj$vFieldMtx[!filmMtx] <- NA
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}


fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )
#fbObj <- buildField( pFieldMtx=as.matrix(FB$zh[1:100,]) )

win.height <- 3
set.seed(42) ##

wIdx <- 34
winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
						,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
					)

k.FLog(dim(winDef$film),pConsole=T,pAppend=F)
# --------------------------------

# pWin=winDef;	pFiltF=NULL;	pBuffSize=1;	pDebug=T
rawPtn <- scanRawPattern( winDef ,pFiltF=scanRawPattern.filtF ,pBuffSize=1 ,pDebug=T )



# ================================================================
# 다중코어 코드.
winDefLst <- list( )
for( wIdx in 1:(nrow(fbObj$vFieldMtx)-win.height+1) ){
	winDefLst[[(1+length(winDefLst))]] <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
												,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
											)
}

k.FLogStr(sprintf("Parallel test start ========================"),pAppend=F,pConsole=T)
tStamp <- Sys.time()
winRstLst <- sfLapply(winDefLst,	function( winDef ){
										winRst <- scanRawPattern( winDef ,pFiltF=scanRawPattern.filtF ,pBuffSize=1 ,pDebug=T )
									return( winRst )
								}
					)

k.FLogStr(sprintf("Parallel test finish (cost:%.1f) ",Sys.time()-tStamp),pConsole=T)

winRstObj <- list( winRstLst=winRstLst )
winRstObj$winDefLst <- winDefLst

#save( winRstObj ,file="Obj_winRstObj.save" ) # 전체로 저장하기엔 너무 거대하다.
save.winRstObj( winRstObj ,chunk.size=5 )

rptLst <- lapply( winRstObj$winRstLst, function(p){rpt.scanRawPattern(pRP=p,pRptFile=NULL)} )




# ================================================================
# 테스트 코드
k.FLogStr("Test Start",pConsole=T,pAppend=F)
filtF.test <- function( pMatPtn ,pIdxMtx ){ 1==nrow(pIdxMtx) }

tStamp <- Sys.time()
winDef <- tSample.winDef06()
k.FLogStr(sprintf("    finish tSample.winDef06() (cost:%.1f)",Sys.time()-tStamp),pConsole=T)
#rawPtn <- scanRawPattern( winDef ,pFiltF=scanRawPattern.filtF ,pBuffSize=1 ,pDebug=T )
rawPtn <- scanRawPattern( winDef ,pFiltF=filtF.test ,pBuffSize=1 ,pDebug=T )
k.FLogStr(sprintf("    finish scanRawPattern() (cost:%.1f)",Sys.time()-tStamp),pConsole=T)
overLap <- findOverlap( pScanRst=rawPtn ,pBuffSize=200 ,pDebug=T )
k.FLogStr(sprintf("    finish findOverlap() (cost:%.1f)",Sys.time()-tStamp),pConsole=T)

RstGrp <- list( winDef=winDef, rawPtn=rawPtn, overLap=overLap )
save( RstGrp ,file="Obj_RstGrp.save" )

# ================================================================
# findOverlap() 실행. 
#	- 5코어의 경우 18Gb메모리 소모
k.FLogStr(" Start findOverlapFromFile()",pAppend=F)
olUnit <- findOverlapFromFile( "Obj_winObj.0011.0015.save" ,pBuffSize=200 ,pMP=T ,pDebug=T )
k.FLogStr(" End findOverlapFromFile()")
save( olUnit ,file="Obj_winObj.0011.0015.overlap.save" )
k.FLogStr(" End sav overlap")

attributes(olUnit)
k.FLogStr(" End gc()")
