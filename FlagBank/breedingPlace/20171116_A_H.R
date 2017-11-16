# 20171116_A_H.R

#   pArea : matrix
getReboundLst <- function( pCode ,pArea ,pSearchFirst=F ){

	colSpan <- 1:length(pCode)
	reboundLst <- list()
	for( cIdx in 1:length(pCode) ){
		matchObj <- list( found=F ,fIdx=integer(0) ,codeVal<-pCode[cIdx] )
		for( rIdx in nrow(pArea):1 ){
			matchObj$found <- pCode[cIdx]%in%pArea[rIdx,]
			if( matchObj$found ){
				matchObj$fIdx <- c(matchObj$fIdx,rIdx)
				if( pSearchFirst )
					break
			}
		} # for(rIdx)
		reboundLst[[1+length(reboundLst)]] <- matchObj
	} # for(cIdx)

	return(reboundLst)

} # getReboundLst()
