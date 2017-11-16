
pZh <- as.matrix(FB$zh)
pCode <- pZh[20,]
pArea <- pZh[1:19,]
pSearchFirst <- F




pZh <- FB$zh
pStart <- 100
pDebug <- T

revMtx <- matrix(0,ncol=ncol(pZh) ,nrow=nrow(pZh) )
revMtx[1,] <- NA
for( rIdx in 2:nrow(pZh) ){
	curZoid <- pZh[rIdx,]
	for( cIdx in 1:ncol(pZh) ){
		#----------------------------------------------
		fIndices <- which( apply( pZh[1:(rIdx-1),] ,1 ,function(p){ curZoid[cIdx]%in%p }) )
		revMtx[rIdx,cIdx] <- if( 0==length(fIndices) ){ NA 
								} else { rIdx-fIndices[length(fIndices)] } 
		#----------------------------------------------
	} # for(cIdx)
	if( pDebug && 0==(rIdx%%50) )
		k.FLogStr(sprintf("rIdx:%d",rIdx),pConsole=T)
} # for(rIdx)

naIndices <- which(apply( revMtx ,1 ,function(p){any(is.na(p))} ))
hSpan <- (naIndices[length(naIndices)]+1):nrow(pZh)

for( hIdx in hSpan ){
	if(hIdx==hSpan[length(hSpan)])
		break
	
} # for(hIdx)


