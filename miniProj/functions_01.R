 
# ---------------------------------------------------------------------
# p rawZh : zoid history
# p curIdx : 평가대상 바로이전 history
# p depth : 평가 깊이
# return : [hit number, element number, depth]
grOne <- function( rawZh, curIdx, depth ){

	rVal <- rep(0,3)
	depthSpace <- as.vector(as.matrix( rawZh[curIdx:(curIdx-depth+1),] ))
	depthSpace <- unique(depthSpace);
	rVal[2] <- length(depthSpace)
	rVal[3] <- depth

	foundE <- match( depthSpace, as.vector(as.matrix(rawZh[curIdx+1,])) )
	rVal[1] <- length(foundE[!is.na(foundE)])

	# print( as.vector(as.matrix(rawZh[curIdx+1,])) )
	# print( depthSpace )

	return( rVal )
}
