# 한글한글

#	pFlag <- bFlag
#	pHSpan <- 100:length(pFlag)
getFlagStatSample <- function( pFlag ,pHSpan ){

					cName <- c("output","mean","mean.diff","seqHaunt")
					mtx <- matrix( 0 ,nrow=length(pHSpan) ,ncol=length(cName) )
					colnames(mtx) <- cName

					for( idx in 1:length(pHSpan) ){ # idx <- 1
						cfIdx <- pHSpan[idx]
						mtx[idx,"output"]		<- pFlag[cfIdx]
						curFlag <- pFlag[(cfIdx-1):1] # reversed.

						mtx[idx,"mean"]			<- mean(curFlag)
						mtx[idx,"mean.diff"]	<- mtx[idx,"mean"] - mean(curFlag[1:20])

						cnt <- 0
						for( cntIdx in 2:length(curFlag) ){
							if( curFlag[cntIdx]==curFlag[1] ){
								cnt <- cnt+1
							} else {
								break
							}
						}
						mtx[idx,"seqHaunt"]	<- ifelse( curFlag[1] , cnt+1 , -cnt )
					} # for

					return( list( sampleMtx=mtx ) )
				}





