
mayCw.getCntDf <- function( tLst ){   # tLst <- tLstB
    dfLst <- lapply( tLst$typLst ,function( typMtx ){
        if( 0==ncol(typMtx) ){
            data.frame( grpId=character(0) ,mName=character(0) ,M=character(0) ,I=character(0) )
        } else {
            as.data.frame( t(typMtx) )
        }
    })

    typDf <- NULL
    for( nIdx in names(dfLst) ){
        df <- dfLst[[nIdx]]

        if( 0==nrow(df) ) next

        # df$hIdx <- nIdx
        df <- cbind( hIdx=nIdx ,df )
        typDf <- rbind( typDf ,df )
    }
    cntDf <- ddply( typDf ,.(grpId,hIdx,mName,M,I) ,function( pDf ){
        data.frame( cnt=nrow(pDf) )
    })  ;head(cntDf) # 동일 타입 중복이 많아서..

    return( cntDf )
}
