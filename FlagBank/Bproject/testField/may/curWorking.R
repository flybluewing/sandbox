
workDfA <- subset( cntDfA ,lowHpn )
workDfB <- subset( cntDfB ,lowHpn )

mCol <- c("mName","M","I")
hpnFlag <- rep( F ,nrow(workDfB) )
for( idxB in 1:nrow(workDfB) ){
    for( idxA in 1:nrow(workDfA) ){
        if( all(workDfB[idxB,mCol]==workDfA[idxA,mCol]) ){
            hpnFlag[idxB] <- TRUE
        }
    }
}

workDfB$lowHpn <- hpnFlag

graphDf <- ddply( workDfB ,.(hIdx) ,function( pDf ){
    hpnCnt <- c( sum(pDf$lowHpn) ,nrow(pDf)-sum(pDf$lowHpn) )
    data.frame( hpnYn=c("hpnY","hpnN") ,hpnCnt=hpnCnt )
})

ggplot( graphDf ,aes(x=hIdx,fill=hpnYn,y=hpnCnt) ) + geom_bar( stat="identity" )


