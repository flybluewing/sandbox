rst <- matrix( nrow=0, ncol=3 )

tHis <- as.matrix(tGlobal$rawZH)
for( rIdx in 99:(nrow(tHis)-1) ){
    tZoid <- tGlobal$rawZH[rIdx+1,]

    for( idx in rIdx:1 ){
        tZoid <- setdiff( tZoid ,tHis[idx,] )
        if( 0==length(tZoid) ){
            tD <- unique(as.vector( tHis[idx:rIdx,] ))
            # print(sprintf( "finished:%d length:%d" ,(rIdx-idx+1) ,length(tD) ))
            rst <- rbind( rst ,c((rIdx-idx+1),length(tD),0) )
            break
        } # if
    }
}

popu <- 10000
rZoid <- matrix( 0 ,nrow=popu ,ncol=tGlobal$dnaLength )
for( idx in 1:popu ){
    rZoid[idx,] <- sort(sample( tGlobal$dnaTypes ,tGlobal$dnaLength ))        
}

ov <- apply( rZoid, 1, function(p){length(setdiff(p,tDnas))} )
sum( ov==0 )    # 약 4~5% 걸러낼 수 있네...
