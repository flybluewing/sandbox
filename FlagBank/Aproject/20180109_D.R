# 20180109_D.R 교차모델


cutEadge.getBanRebDiff <- function( gEnv ,allIdx ,pDebug=F ){

    scanPtn <- function( pVal ,pFirstOnly=TRUE ){
        valLen <- length(pVal)

        banVal <- integer(0)
        for( dIdx in 1:5 ){
            ptnSpan <- valLen - (1:2)*dIdx + 1
            if( 1>ptnSpan[2] ){
                break
            }
            if( pVal[ptnSpan[1]]==pVal[ptnSpan[2]] ){
                banVal[1+length(banVal)] <- pVal[ptnSpan[1]]
                if( pFirstOnly ){
                    break
                }
            }
        }

        return(banVal)
    } # scanPtn()

    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    allCodeMtx <- t(apply( gEnv$allZoidMtx[allIdx,,drop=F] ,1 ,function(aZoid){lastZoid-aZoid} ))
    stdCodeMtx <- gEnv$zhF[1:(nrow(gEnv$zhF)-1),] - gEnv$zhF[2:nrow(gEnv$zhF),]

    flagLst.base <- lapply( seq_len(length(allIdx)) ,function(p){integer(0)})
    for( azColIdx in 1:6 ){
        banVal <- scanPtn( stdCodeMtx[,azColIdx] )
        for( idx in seq_len(length(allIdx)) ){
            if( any( allCodeMtx[idx,azColIdx]%in%banVal ) ){
                flagLst.base[[idx]][1+length(flagLst.base[[idx]])] <- azColIdx
            }
        }
    }
	flag.base <- sapply( flagLst.base ,function(p){1>length(p)} )

    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){sort(unique(p))})
    flagLst.cv <- vector( "list" ,length(allIdx) )
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tZhF <- gEnv$zhF[vIdx==gEnv$zhF[,azColIdx] ,,drop=F]
            if( 2>nrow(tZhF)){
                next
            }
            tStdMtx <- tZhF[1:(nrow(tZhF)-1),,drop=F] - tZhF[2:nrow(tZhF),,drop=F]
            tAllMtx <- t(apply(gEnv$allZoidMtx[allIdx,,drop=F],1,function(aZoid){tZhF[nrow(tZhF),]-aZoid}))

            banValLst <- lapply( 1:6 ,function(p){scanPtn(tStdMtx[,p])})
            banValLst[[azColIdx]] <- integer(0) # 작업중인 컬럼은 건너뛰어야..
            for( idx in seq_len(length(allIdx)) ){
                if( vIdx!=gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                    next
                }
                flag <- sapply(1:6,function(pCol){ tAllMtx[idx,pCol]%in%banValLst[[pCol]] })
                if( any(flag) ){
                    flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c(azColIdx,vIdx)
                }
            } # idx
        } # vIdx
    }
	flag.cv <- sapply( flagLst.cv ,function(p){ 2>length(p) } )

    rObj <- list( idStr="cutEadge.getBanRebDiff" )
    rObj$flag <- flag.base & flag.cv
	if( pDebug ){
		rObj$flagLst.base	<- flagLst.base
		rObj$flagLst.cv		<- flagLst.cv
	}
    return( rObj )

} # cutEadge.getBanRebDiff()






kCnt <- sapply( flagLst.cv ,length )



