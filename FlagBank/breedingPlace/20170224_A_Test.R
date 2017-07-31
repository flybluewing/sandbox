FB <- getFlagBank()
library(snowfall)
sfInit( parallel=T ,cpus=2 )

set.seed(42) ##
fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) )
# fbObj <- buildField( pFieldMtx=as.matrix(FB$zh[1:30,]) )

win.height <- 3
win.seed <- sample( win.height:nrow(fbObj$vFieldMtx) ,2 )

winDefLst <- list()
for( wIdx in win.seed ){
	winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
							,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
						)
	winDefLst[[length(winDefLst)+1]] <- winDef
}

winDef <- winDefLst[[1]]
stage <- slideWindow( winDef )
k.FLogStr(sprintf("    the first stage for %s",stage$idStr.winDef))
stageLst <- list()
stageLst[[1]] <- stage
for( idx in 1:100 ){
	stage <- findOverlap( stage )
	stageLst[[length(stageLst)+1]] <- stage
	k.FLogStr(sprintf("    %04d %s",idx,stage$idStr.winDef))
	if( 0==length(stage$matchLst) )
		break;
}




# -----------------------------------------------


sfExport("slideWindow");	sfExport("findOverlap");	sfExport("compareFilm");	sfExport("compareFilm.lf");
sfExport("k.FLogStr");		sfExport("k.FLogOpt")

k.FLogStr("Start sfLapply()")
winRstObj <- sfLapply(winDefLst,	function( winDef ){
									stage <- slideWindow( winDef )
									stageLst <- list()
									stageLst[[1]] <- stage
									for( idx in 1:100 ){
										stage <- findOverlap( stage )
										stageLst[[length(stageLst)+1]] <- stage
										k.FLogStr(sprintf("    %04d %s",idx,stage$idStr.winDef))
										if( 0==length(stage$matchLst) )
											break;
									}
									return( stageLst )
								}
					)
k.FLogStr("Finish sfLapply()")

# winRstObj$winDefLst <- winDefLst

winRst <- list( winDefLst=winDefLst )
winRst$stageLst <- winRstObj
winRst$fbObj <- fbObj

saveFileName <- "Obj_winRstTest.save"
save( winRst ,file=saveFileName ,compress="bzip2" )
k.FLogStr(sprintf("Finish fileSave : %s",saveFileName))

# ------------------------------------------------------------------------------------------------------
# ======================================================================================================
findOverlap <- function( pMatchInfo ){

						matchLst <- pMatchInfo$matchLst

						cumCount <- 0;
						idStrs <- character(0)
						cumObjLst <- list()
						for( oIdx in length(matchLst):2 ){
							for( iIdx in (oIdx-1):1 ){
								mObj <- compareFilm.lf( matchLst ,oIdx ,iIdx )
								if( 0==nrow(mObj$mfIdxMtx) )
									next

								saveIdx <- which(idStrs==mObj$idStr)
								if( 0==length(saveIdx) ){
									idStrs <- c(idStrs,mObj$idStr)
									cumObjLst[[length(cumObjLst)+1]] <- mObj
								} else {
									cumObjLst[[saveIdx]]$compPair <- rbind(cumObjLst[[saveIdx]]$compPair, mObj$compPair)
								}
								cumCount <- cumCount+1
							} # for(iIdx)
						}

						rObj <- list( depth=(pMatchInfo$depth+1) )
						rObj$cumCount=cumCount
						rObj$idStrs <- idStrs
						rObj$matchLst <- cumObjLst
						rObj$idStr.winDef <- pMatchInfo$idStr.winDef
						rObj$timeStamp <- Sys.time()
						return( rObj )
				}


buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )

		# getFieldSet.dummy( ) -------------------------------------------------
		fObj <- getFieldSet.dummy( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$filmMtx	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		
		# getFieldSet.left( ) -------------------------------------------------
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=4 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$filmMtx	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		rObj <- list( vFieldMtx = vFieldMtx )
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}



