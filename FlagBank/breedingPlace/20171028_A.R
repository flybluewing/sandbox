# 한글한글


hDiffLst <- list()
pairLst <- list()
for( rIdx in 1:(nrow(zhF)-1)){
    for( rrIdx in (rIdx+1):nrow(zhF) ){
        diff <- abs(zhF[rIdx,]-zhF[rrIdx,])
        pairLst[[1+length(pairLst)]] <- c(rIdx,rrIdx,length(unique(diff)))
    }
}

kMtx <- do.call( rbind, pairLst )

flag <- kMtx[,3]==1


kMtx[flag,]