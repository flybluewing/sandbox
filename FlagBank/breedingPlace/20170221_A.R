hMtx <- FB$zh

# 모든 DNA 가 나온 시점을 먼저 찾고.
dnaType <- 1:45
dnaSize <- ncol(hMtx)
dnaFlag <- rep( 0 ,length(dnaType) )
names(dnaFlag) <- dnaType

lastIdx <- 0
for( hIdx in 1:nrow(hMtx) ){
	# hIdx <- 1
	dnaFlag[as.character(hMtx[hIdx,])] <- dnaFlag[as.character(hMtx[hIdx,])]+1
	if( all(dnaFlag!=0) ){
		lastIdx <- hIdx
		k.FLogStr(sprintf("last Idx:%d",lastIdx),pConsole=T)
		break;
	}
}


#	fMtx : 각 원소는 몇 번만에 재발된 것인지?(hIdx시점 이후로)
#		dna 전체에 대해서 검색한다.
fMtx <- matrix( 0 ,ncol=ncol(hMtx) ,nrow=nrow(hMtx) )
for( hIdx in (lastIdx+1):nrow(hMtx) ){
	# hIdx <- lastIdx
	zoid <- rep( NA ,dnaSize )
	names(zoid) <- hMtx[hIdx,]
	for( tIdx in (hIdx-1):1 ){
		# tIdx <- hIdx-1
        rm <- hMtx[hIdx,is.na(zoid)]
		itst <- intersect(hMtx[tIdx,],rm)
        if( 0==length(itst) )
            next
		zoid[as.character(itst)] <- hIdx-tIdx
		if( !any(is.na(zoid)) )
			break
	}
	fMtx[hIdx,] <- zoid
}

farMost <- apply( fMtx ,1 ,max )
farMost.tbl <- table(farMost)


fMtx.sort <- t(apply(fMtx[28:nrow(fMtx),],1,sort))
srlst <- sameRow(fMtx.sort)
#	lapply(srlst,function(p){fMtx.sort[p,]})
isSeq <- sapply(srlst,function( p ,pWin ){
							p <- sort(p)
							d <- p[2:length(p)]-p[1:(length(p)-1)]
							return( sum(d%in%pWin) )
						}
					,pWin=c(1,2)
				)
sum(isSeq)
			
srlst <- sameRow(fMtx.sort[,1:(dnaSize-1)])
isSeq <- sapply(srlst,function( p ,pWin ){
							p <- sort(p)
							d <- p[2:length(p)]-p[1:(length(p)-1)]
							return( sum(d%in%pWin) )
						}
					,pWin=c(1,2)
				)
sum(isSeq)

srlst <- sameRow(fMtx.sort[,1:(dnaSize-2)])
isSeq <- sapply(srlst,function( p ,pWin ){
							p <- sort(p)
							d <- p[2:length(p)]-p[1:(length(p)-1)]
							return( sum(d%in%pWin) )
						}
					,pWin=c(1,2)
				)
sum(isSeq)
