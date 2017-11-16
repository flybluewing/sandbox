# 20171116_A_H.R

# 일치하는 갯수
getMatchLst <- function( pCode ,pArea ){

	matchLst <- list()
	for( rIdx in 1:nrow(pArea) ){
		# pCode 내에 같은 값이 있을 경우를 위해 for문 적용
		flag <- sapply( pCode ,function(p){p%in%pArea[rIdx,]} )
		if( any(flag) ){
			matchObj <- list(hIdx=rIdx,flag=which(flag))
			matchLst[[1+length(matchLst)]] <- matchObj
		}
	} # for(rIdx)

	return(matchLst)
} # getMatchLst()

# pCode의 각 요소가 얼마만에 재현된 것인지.
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
