
# 원본
compareFilm.lf <- function( pMatchLst ,pIdxA ,pIdxB ){

		mfMtx <- (pMatchLst[[pIdxA]]$mfMtx & pMatchLst[[pIdxB]]$mfMtx)
			# match flag mtx

		cName <- c("row","col")
		mfIdxMtx <- matrix( 0 ,nrow=sum(mfMtx) ,ncol=length(cName) )
		mtxInputIdx <- 1
		for( rIdx in 1:nrow(mfMtx) ){
			cSpan <- which( mfMtx[rIdx,] )
			if(0==length(cSpan))
				next
			for( cIdx in cSpan ){
				mfIdxMtx[mtxInputIdx,] <- c(rIdx,cIdx)
				mtxInputIdx <- mtxInputIdx+1
			}
		}
		colnames(mfIdxMtx) <- cName

		idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))

		return( rMObj )
	}

# 개조
compareFilm.lf <- function( pMatchLst ,pIdxA ,pIdxB ){

		mfMtx <- (pMatchLst[[pIdxA]]$mfMtx & pMatchLst[[pIdxB]]$mfMtx)
			# match flag mtx

		cName <- c("row","col")
		mfIdxMtx <- matrix( 0 ,nrow=sum(mfMtx) ,ncol=length(cName) )
		mtxInputIdx <- 1
		for( rIdx in 1:nrow(mfMtx) ){
			cSpan <- which( mfMtx[rIdx,] )
			if(0==length(cSpan))
				next
			for( cIdx in cSpan ){
				mfIdxMtx[mtxInputIdx,] <- c(rIdx,cIdx)
				mtxInputIdx <- mtxInputIdx+1
			}
		}
		colnames(mfIdxMtx) <- cName

		# idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		idxStr <- apply(mfIdxMtx,1,function(p){paste(p,collapse=".")})
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		# rMObj$compPair <- sort(c(pIdxA,pIdxB))
		rMObj$compPairLst <- list(sort(c(pIdxA,pIdxB)))

		return( rMObj )
	}
	
