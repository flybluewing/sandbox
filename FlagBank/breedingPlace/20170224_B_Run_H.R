#	pFileName <- do.call(c,saveFiles)
report.ptnObjGrp <- function( pFileName ,pRptFile="./report/report.ptnObjGrp" ){

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
							mRptMatchLst <- function( pMatchLst ,pMainTitle="Your Title" ){
									# pMatchLst <- ptnObjGrp$overlap$matchLst
									idxNum	<- sapply(pMatchLst,
														function(p){ return(nrow(p$idxMtx)) } )
									preNum	<- sapply(pMatchLst,
														function(p){ h<-nrow(p$matMtx) ;return( sum(!is.na(p$matMtx[h,])) ) }
													)
									compNum	<- sapply(pMatchLst,
														function(p){ cv<-unique(as.vector(p$compPair))  ; return(length(cv)) }
													)
									numMtx <- cbind( 1:length(idxNum) ,idxNum, preNum ,compNum )
									
									FLogStr(sprintf("** %s",pMainTitle))
									FLogStr(" - idxNum");	FLog(summary(idxNum))
									FLogStr(" - preNum");	FLog(summary(preNum))
									FLogStr(" - compNum");	FLog(summary(compNum))
									
									# par( mfrow=c(2,2) )
									plot( compNum	,idxNum ,main=pMainTitle )
									plot( compNum	,preNum ,main=pMainTitle )
									hist( compNum ,main=pMainTitle )
									return(numMtx)
								}

							FLogStr(sprintf("Report report.ptnObjGrp"),pAppend=F,pTime=T)
							for( fnIdx in 1:length(pFileName) ){ # fnIdx <- pFileName[1]
								FLogStr(sprintf("Loading %s (%dth)------------------------------------------- ",pFileName[fnIdx],fnIdx))
								png( sprintf("%s_%03d.png",pRptFile,fnIdx) ,width=768 ,height=1536 )
								myObj <- load(pFileName[fnIdx]) # ptnObjGrp
								FLogStr(sprintf("    rawPtn match:%d  overlap match:%d"
										,length(ptnObjGrp$rawPtn$matchLst) ,length(ptnObjGrp$overlap$matchLst) ))

								par( mfrow=c(3,2) )
								numMtx.raw	<- mRptMatchLst( ptnObjGrp$rawPtn$matchLst ,sprintf("Raw Ptn match(%dth)",fnIdx) )
								numMtx.ol	<- mRptMatchLst( ptnObjGrp$overlap$matchLst ,sprintf("Overlap Ptn match(%dth)",fnIdx) )
								compPair.real <- sapply( ptnObjGrp$overlap$matchLst ,function( p ){
														# p <- ptnObjGrp$overlap$matchLst[[1]]
														rpMatchIdx <- unique( as.vector(p$compPair) )
														ml <- ptnObjGrp$rawPtn$matchLst[rpMatchIdx]
														rpIdx <- do.call( c, lapply(ml,function(p){p$compPair}) )
														return( length(unique(rpIdx)) )
													})
								cbind( numMtx.ol ,compPair.real )
								dev.off()
							} # for
							return( T )
						}

