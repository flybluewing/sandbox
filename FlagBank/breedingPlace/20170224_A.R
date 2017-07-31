FB <- getFlagBank()
library(snowfall)
sfInit( parallel=T ,cpus=4 )

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

# winDef <- winDefLst[[1]]

sfExport("slideWindow");	sfExport("findOverlap");	sfExport("compareFilm");	sfExport("compareFilm.lf");
sfExport("k.FLogStr");		sfExport("k.FLogOpt")

k.FLogStr("Start sfLapply()")
winRstObj <- sfLapply(winDefLst,	function( winDef ){
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
									return( stageLst )
								}
					)
k.FLogStr("Finish sfLapply()")

# winRstObj$winDefLst <- winDefLst

winRst <- list( winDefLst=winDefLst )
winRst$stageLst <- winRstObj
winRst$fbObj <- fbObj

saveFileName <- "Obj_winRst20170321.save"
save( winRst ,file=saveFileName ,compress="bzip2" )
k.FLogStr(sprintf("Finish fileSave : %s",saveFileName))




