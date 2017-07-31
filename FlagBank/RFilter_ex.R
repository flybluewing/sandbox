# -----------------------------------------------
pZhMtx <- as.matrix(FB$zh)
pZoidMtx <- trZoid
rObj <- k.filt.RFhZoidOnly_a( pZhMtx=pZhMtx ,pZoidMtx=pZoidMtx ,pLog=T )

# -----------------------------------------------
rMtx <- NULL
fMtx <- matrix( 0 ,nrow=0 ,ncol=nrow(trZoid30k) )
for( idx in 300:nrow(FB$zh) ){
	# idx <- 300
	pZhMtx <- as.matrix( FB$zh[1:(idx-1),] )
	pZoidMtx <- as.matrix( FB$zh[idx,] )

	rObj <- k.filt.RFRelative_a( pZhMtx ,pZoidMtx )
	if( is.null(rMtx) ){
		rMtx <- rObj$filtCls
	} else {
		rMtx <- rbind( rMtx ,rObj$filtCls )
	}

     rObj <- k.filt.RFRelative_a(pZhMtx,trZoid30k)
	fMtx <- rbind( fMtx ,rObj$filtCls )
	

    k.FLogStr(sprintf("idx:%d",idx))
}
