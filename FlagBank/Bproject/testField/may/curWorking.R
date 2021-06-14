tLst <- tLstB

cntDfA <- mayCw.getCntDf( tLstA )   ;cntDfA$ab <- "A"
cntDfB <- mayCw.getCntDf( tLstB )   ;cntDfB$ab <- "B"
cntDf <- rbind( cntDfA ,cntDfB )

hpnCntDf <- ddply( cntDf ,.(hIdx) ,function( pDf ){
    data.frame( cnt=nrow(pDf) )
})  ;head(hpnCntDf)
#   ggplot( hpnCntDf ,aes(x=cnt) ) + geom_histogram()

hIdx.low <- subset( hpnCntDf ,cnt<=9 )$hIdx
typDf.low <- subset(cntDf,hIdx %in% hIdx.low )

typDf880 <- typDf.low
typDf960 <- typDf.low

ddply( typDf960 ,.(hIdx) ,nrow )

if( FALSE ){
    grpDf <- ddply( cntDf ,.(hIdx) ,function( pDf ){
        data.frame( dd = nrow(pDf) )
    })

    ggplot( grpDf ,aes(x=dd) ) + geom_histogram( binwidth=5 )
}

