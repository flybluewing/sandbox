


if( FALSE ){    # freq both A,B

    hpnDfA <- ddply( cntDfA ,.(mName,M,I) ,function( pDf ){ data.frame(cnt=sum(pDf$cnt)) })
    hpnDfB <- ddply( cntDfB ,.(mName,M,I) ,function( pDf ){ data.frame(cnt=sum(pDf$cnt)) })

    hpnDfAB <- merge( hpnDfA ,hpnDfB ,by=c("mName","M","I") ,suffixes=c(".A",".B") ,all=T)
    hpnDfAB[is.na(hpnDfAB)] <- 0
    hpnDfAB <- hpnDfAB[order( hpnDfAB$cnt.B ,decreasing=T ) ,]

    hpnDfAB <- subset( hpnDfAB ,cnt.A<=1 )

    hCntDfB <- ddply( cntDfB ,.(hIdx) ,function(pDf){ data.frame(tCnt=nrow(pDf)) })

    graphDf <- NULL
    mCol <- c("mName","M","I")
    for( cIdx in 1:nrow(cntDfB) ){
        for( hIdx in 1:nrow(hpnDfAB) ){
            mFlag <- cntDfB[cIdx,mCol]==hpnDfAB[hIdx,mCol]
            if( any(!mFlag) ) next

            newDf <- cbind( cntDfB[cIdx,c("grpId","hIdx","mName","M","I","lowHpn")] ,hpnDfAB[hIdx,c("cnt.A","cnt.B")] )
            graphDf <- rbind( graphDf ,newDf )
        }
    }
    # graphDf <- ddply( graphDf ,.(hIdx) ,function( pDf ){
    #     cnt <- c( sum(pDf$cnt.A==0) ,sum(pDf$cnt.A==1) )
    #     data.frame( hpnPast=c("N","Y") ,cnt=cnt )
    # })
    graphDf <- ddply( graphDf ,.(hIdx) ,function( pDf ){
        cnt <- c( sum(pDf$cnt.A==0) ,sum(pDf$cnt.A==1) )
        tCnt <- hCntDfB[hCntDfB$hIdx==pDf$hIdx[1] ,]$tCnt
        tCnt <- ifelse( tCnt>30 ,30 ,tCnt )
        cnt[3] <- tCnt - sum(cnt)
        data.frame( hpnPast=c("N","Y","left") ,cnt=cnt )
    })


    p <- ggplot( graphDf ,aes(x=hIdx ,y=cnt ,fill=hpnPast ) ) + geom_bar( stat="identity" )
    p + coord_flip()


    # [graphDf - tLstB$typLst]--------------------------------------------------
    tMtx <- NULL
    for( hIdx in names(tLstB$typLst) ){
        mtx <- t( tLstB$typLst[[hIdx]] )

        mCol <- c("mName","M","I")
        for( mIdx in seq_len(nrow(mtx)) ){
            tDat <- c( mtx[mIdx,] ,hIdx=hIdx ,lowHpn=TRUE ,cnt.A="N/A" )
            for( gfIdx in seq_len(nrow(graphDf)) ){
                if( graphDf[gfIdx,"hIdx"]!=hIdx )   next

                tDat["lowHpn"] <- graphDf[gfIdx,"lowHpn"]
                mFlag <- mtx[mIdx,c("mName","M","I")] == graphDf[gfIdx,c("mName","M","I")]
                if( all(mFlag) ) {
                    tDat["cnt.A"] <- graphDf[gfIdx,"cnt.A"]
                }
            }

            tMtx <- rbind( tMtx ,tDat )
        }
    }

    tMtx <- tMtx[,c("hIdx","mName" ,"M" ,"I" ,"info" ,"lowHpn" ,"cnt.A")]
    rownames(tMtx) <- NULL

    lowHpnMtx <- tMtx[tMtx[,"lowHpn"]=="TRUE" ,]    ;table(lowHpnMtx[,"hIdx"])
    lowHpnMtx[ ,c("hIdx","mName","M","I","info","cnt.A")]
}
