# turning : compareFilm()

compareFilm <- function( pMatchLst ,pIdxA ,pIdxB ){

		mfMtx <- (!is.na(pMatchLst[[pIdxA]]$matMtx)) & (!is.na(pMatchLst[[pIdxB]]$matMtx)) 
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
		# idxStr <- "k"
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))
		
		return( rMObj )
	}


gc()
fName <- "Pr_compareFilmSF.prof"
Rprof(filename=fName,append=F)
replicate( n=3000 ,compareFilm( matchLst ,oIdx ,iIdx ) )
Rprof(NULL)
summaryRprof( fName )

