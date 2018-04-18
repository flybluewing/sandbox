# 20180109_D.R 교차모델

cutEadge.XXXX <- function( gEnv ,allIdx ){



    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- QQE # 
    return( rObj )

} # cutEadge.XXXX()


rstLst <- list()
for( hIdx in 100:nrow(gEnv$zhF) ){
	fndLst <- fnd2SeqReb( gEnv$zhF[1:(hIdx-1),] ,gEnv$zhF[hIdx,] ,pSrcNum=4 )
	rstLst[[1+length(rstLst)]] <- fndLst
}
#	5까지 23/703

rstLst <- list()
for( hIdx in 100:nrow(gEnv$zhF) ){
	fndLst <- fnd3SeqReb( gEnv$zhF[1:(hIdx-1),] ,gEnv$zhF[hIdx,] ,pSrcNum=4  )
	rstLst[[1+length(rstLst)]] <- fndLst
}
#	3연속 2번 발생 최대거리는 50,48,5,39번 4개뿐... 확실해야 하니 30가자.


kCnt <- sapply( rstLst ,length )

maxIdx <- sapply( rstLst ,function( p ){
				if( 2>length(p) ) return( 1000 )
				return( p[[2]]$idx )
			})

valLst <- lapply( rstLst[kCnt>0] ,function(p){

			})


flag <- apply( gEnv$allZoidMtx[allIdx,] ,1 ,function(aZoid){
				sum( aZoid %in% lastZoid )
			})



