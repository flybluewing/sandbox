bFCust.defaultHardFlag <- function( pName ){
    hardPh	<- c("basic")
    pFlag <- pName %in% hardPh

	flag <- c( p=pFlag )
	return( list( flag=flag ) )
}



FCust_stdCut.rawRow <- function( hName ,mName ,scoreMtxH ){

    rObj <- list( mName=mName )

    cfg <- scoreMtxCfg[[mName]]

    rObj$fCol <- cfg$fCol
    rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

    rObj$available <- TRUE

    rObj$cut <- function( aZoidMtx ,alreadyDead=NULL ,checkAll=F ){

        val.len <- nrow(aZoidMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( cutLst )

        # fCol --------------------------------------------
        for( fcName in names(rObj$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !checkAll && alreadyDead[aIdx] ) next


            }

        }

        # sm row --------------------------------------------
        for( aIdx in seq_len(val.len) ){
            if( !checkAll && alreadyDead[aIdx] ) next


        }

        return( cutLst )
    }

    return( rObj )

}
