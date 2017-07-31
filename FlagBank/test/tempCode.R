# getFieldSet.diff( )
#	function( pFieldMtx=as.matrix(FB$zh) ,pDist=2 )
# -------------------------------------------------------------------------------------------

#pFieldMtx=as.matrix(FB$zh)
#pDist=2
#pAbs=T

FB <- getFlagBank()

# fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )
fbObj <- buildField( pFieldMtx=as.matrix(FB$zh[1:100,]) )

win.height <- 3
set.seed(42) ##

wIdx <- 50
winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
						,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
					)

k.FLog(dim(winDef$film),pConsole=T,pAppend=F)
# --------------------------------
gc()
fName <- "Pr_std.prof"
Rprof(filename=fName,append=F)
	k.FLogStr("Start",pConsole=T)
	stage <- slideWindow( winDef ,pDebug=T )
	k.FLogStr("    SlideWindow")
	stageLst <- list()
	stageLst[[1]] <- stage
	for( idx in 1:100 ){
		stage <- findOverlap( stage ,pDebug=T )
		stageLst[[length(stageLst)+1]] <- stage
		k.FLogStr(sprintf("    %04d %s",idx,stage$idStr.winDef))
		if( 0==length(stage$matchLst) )
			break;
	}
	k.FLogStr(sprintf("Finish"),pConsole=T)
Rprof(NULL)
summaryRprof(fName)

proObj <- list(stageLst=stageLst)
proObj$winDef <- winDef
save( proObj ,file="Obj_proObj.std.save" ,compress="bzip2")


# --------------------------------

gc()
fName <- "Pr_modified.prof"
matNum = 2
cpMtxScale=400

Rprof(filename=fName,append=F)
	k.FLogStr("Start",pConsole=T)
	stage <- slideWindow( winDef ,pMatNum=matNum ,pCpMtxScale=cpMtxScale ,pFiltRMF=filtRMF ,pDebug=T )
	k.FLogStr("    SlideWindow")
	stageLst <- list()
	stageLst[[1]] <- stage
	for( idx in 1:100 ){
		stage <- findOverlap( stage ,pMatNum=matNum ,pCpMtxScale=cpMtxScale ,pDebug=T )
		stageLst[[length(stageLst)+1]] <- stage
		k.FLogStr(sprintf("    %04d %s",idx,stage$idStr.winDef))
		if( 0==length(stage$matchLst) )
			break;
	}
	k.FLogStr(sprintf("Finish"),pConsole=T)
Rprof(NULL)
summaryRprof(fName)

proObj <- list(stageLst=stageLst)
proObj$winDef <- winDef
proObj$matNum <- matNum
proObj$cpMtxScale <- cpMtxScale
proObj$cpMtxScale <- cpMtxScale

save( proObj ,file="Obj_proObj0401A.mod.save" ,compress="bzip2")



# ---------------------------------------------------------------------------------------

buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )

		# getFieldSet.dummy( ) -------------------------------------------------
		fObj <- getFieldSet.dummy( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		
		# getFieldSet.left( ) -------------------------------------------------
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=4 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		rObj <- list( vFieldMtx = vFieldMtx )
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}





