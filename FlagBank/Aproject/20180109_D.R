# 20180109_D.R 교차모델


localHisMtx <- gEnv$zhF[gEnv$zhF[,1]%in%4 ,]

pMtx <- localHisMtx[,2:6] %% 10
pMtx <- gEnv$zhF %% 10

colStr <- c("hIdx","size")
rstMtx <- matrix( 0 ,nrow=nrow(pMtx) ,ncol=length(colStr) )	;colnames(rstMtx)<-colStr
rstLst <- list()
for( hIdx in 2:nrow(pMtx) ){
	idx <- which( pMtx[hIdx,]==pMtx[(hIdx-1),] )
	idxLen <- length(idx)
	if( 1<idxLen ){
		cStep <- idx[2:idxLen] - idx[1:(idxLen-1)]
		if( 1<sum(cStep==1) ){
			rstMtx[hIdx,] <- c(hIdx,sum(cStep==1))
			rstObj <- list( hIdx=hIdx, zoidMtx=rbind(pMtx[hIdx-1,],pMtx[hIdx,]) )
			rstLst[[1+length(rstLst)]] <- rstObj
		}
	}
}
#	zhF 에 대해서는 3개 연속,
#	localHisMtx 에 대해서는 2개 연속을 제거하기로 한다.

table( rstMtx[,"size"] )

chkIdx <- which(rstMtx[,"hIdx"]>0)



chkPtnObj <- function(){
	# 이들은 각각 사이즈가 제한되어야 할 듯.(용도에 따라)
	rObj <- list()
	rObj$rebPtn <- function( pVal ){
		# 1,2,3,1,2,3
		
	} # rObj$rebPtn()
	rObj$symPtn <- function( pVal ){
		# 1,2,3,2,1  or 1,2,2,1
	} # rObj$symPtn
	rObj$gradPtn <- function( pVal ){
		# 1,2,3,4,5
	}
	return( rObj )
} # chkPtnObj()



