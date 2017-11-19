# 20171116_A_H.R

getRebLen <- function( pCodeVal ,pBaseH ){
	rRebLen <- rep(NA,length(pCodeVal))
	names(rRebLen) <- sprintf("%s",pCodeVal)
	leftCode <- pCodeVal
	for( rIdx in nrow(pBaseH):1 ){
	# for( rIdx in nrow(pBaseH):685 ){	
		indices<-which(leftCode %in% pBaseH[rIdx,])
		if( 0==length(indices) )
			next
		rRebLen[leftCode[indices]] <- rIdx
		leftCode <- leftCode[-indices]
		if( !any(is.na(rRebLen)) )
			break
	}
	return( nrow(pBaseH)-rRebLen+1 )
} # getRebLen()

# 일치하는 갯수
getMatchLst <- function( pCode ,pArea ){

	matchLst <- list()
	for( rIdx in 1:nrow(pArea) ){
		# pCode 내에 같은 값이 있을 경우를 위해 for문 적용
		flag <- sapply( pCode ,function(p){p%in%pArea[rIdx,]} )
		if( any(flag) ){
			matchObj <- list(hIdx=rIdx,fIdx=which(flag))
			matchLst[[1+length(matchLst)]] <- matchObj
		}
	} # for(rIdx)

	return(matchLst)
} # getMatchLst()

# getMatchLst() 와 비슷하나, 코드의 위치까지 동일한 경우를 찾는다.
getMatchLst.fixed <- function( pCode ,pArea ){

	matchLst <- list()
	for( rIdx in 1:nrow(pArea) ){
		indices <- which(pArea[rIdx,]==pCode)
		if( 0<length(indices) ){
			matchLst[[1+length(matchLst)]] <-
				list( hIdx=rIdx ,fIdx=indices )
		}
	} # for(rIdx)

	return( matchLst )
} # getMatchLst.fixed()


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
