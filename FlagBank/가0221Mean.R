FB <- getFlagBank()
zhMtx <- as.matrix(FB$zh)

# 전체 분포
hv <- as.vector(zhMtx)
hv.table <- table(hv)
hv.tbl.v <- as.vector(hv.table)
#	plot(sort(hv.tbl.v))
#	hist(hv.tbl.v)

# =====================================================================================
#	Head Area
createSetOpt <- function( pSize=20 ,pSamArea=15 ){
		rObj <- list( size=pSize )
		rObj$samArea <- pSamArea
		rObj$dnaType <- 1:45
		rObj$dnaFreq <- rep( 0 ,length(rObj$dnaType) )
		names(rObj$dnaFreq) <- rObj$dnaType
		return( rObj )
	}

findMiss <- funtion( pZhMtx ,pDnaType ,pIdStr="findMiss" ){
		rObj <- list( idStr=pIdStr )
		return( rObj )
	}

anaSet <- function( pZhMtx ,pOpt ,pIdStr="anaSet" ){
		rObj <- list(idStr=pIdStr)
		return( rObj )
	}
	
# =====================================================================================
#	Excu Area

tSetOpt <- createSetOpt( pSize=20 ,pSamArea=15 )

