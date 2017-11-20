# 20171116_A_H_cliper.R

# clp.dumNum <- cliper.dupNum( zh )
# clp.dumNum$report()
# baseH <- clp.dumNum$getBaseH( zh[5:15,] )   # 임의생성.
# zoidMtx <- zh[1:4,]                         # 임의생성.
# clp.dumNum$byBase( zoidMtx )
# clp.dumNum$byLate( zoidMtx ,baseH )

cliper.quotient <- function( pBaseZh ,pThld=6 ,pSanc=10 ,pBase=5 ){
    rObj <- list( idStr="quotient" ,thld=pThld ,sanc=pSanc )
    rObj$base <- pBase
    rObj$getBaseH <- function( pZoidH ){ pZoidH %/% rObj$base }
    rObj$baseH <- rObj$getBaseH( pBaseZh )
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        codeMtx <- rObj$getBaseH( pZoidMtx )
        baseH <- rObj$getBaseH( pZoidH ) # 상위 손실 발생영역이 제거됨 유의.
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        if( rObj$thld <= sum(pCode==baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        codeMtx <- rObj$getBaseH( pZoidMtx )
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        if( rObj$thld <= sum(pCode==rObj$baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byBaes()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){
        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst.fixed( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()
    return( rObj )
} # cliper.quotient()

cliper.remainder <- function( pBaseZh ,pThld=6 ,pSanc=10 ,pBase=5 ){
    rObj <- list( idStr="remainder" ,thld=pThld ,sanc=pSanc )
    rObj$base <- pBase
    rObj$getBaseH <- function( pZoidH ){ pZoidH %% rObj$base }
    rObj$baseH <- rObj$getBaseH( pBaseZh )
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        codeMtx <- rObj$getBaseH( pZoidMtx )
        baseH <- rObj$getBaseH( pZoidH ) # 상위 손실 발생영역이 제거됨 유의.
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        if( rObj$thld <= sum(pCode==baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        codeMtx <- rObj$getBaseH( pZoidMtx )
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        if( rObj$thld <= sum(pCode==rObj$baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byBaes()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){
        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst.fixed( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()
    return( rObj )
} # cliper.remainder()

cliper.backStep <- function( pBaseZh ,pThld=4 ,pSanc=10 ,pBackStep=1 ){
    rObj <- list( idStr="backStep" ,thld=pThld ,sanc=pSanc )
    rObj$backStep <- pBackStep
    rObj$getBaseH <- function( pZoidH ){
        stepDiff <- pZoidH[1:(nrow(pZoidH)-rObj$backStep),] 
            - pZoidH[(rObj$backStep+1):nrow(pZoidH)]
        return( abs( stepDiff ) )
    } # rObj$getBaseH()
    rObj$baseH <- rObj$getBaseH( pBaseZh )
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        lateZoid <- pZoidH[(nrow(pZoidH)-rObj$backStep+1),]
        codeMtx <- t(apply( pZoidMtx ,1 ,function(p){abs(p-lateZoid)}))
        baseH <- rObj$getBaseH( pZoidH ) # 상위 손실 발생영역이 제거됨 유의.
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        if( rObj$thld <= sum(pCode==baseH[rIdx,] ,na.rm=T) )
                            return( rIdx+rObj$backStep )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        #   - pZoidH : 가장 최신 H. 여기선 사용치 않음.
        lateZoid <- pZoidH[(nrow(pZoidH)-rObj$backStep+1),]
        codeMtx <- t(apply( pZoidMtx ,1 ,function(p){abs(p-lateZoid)}))
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        if( rObj$thld <= sum(pCode==rObj$baseH[rIdx,] ,na.rm=T) )
                            return( rIdx+rObj$backStep )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byBase()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){

        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst.fixed( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()
    return( rObj )
} # cliper.backStep()

cliper.stepWidth <- function( pBaseZh ,pThld=4 ,pSanc=10 ){
    rObj <- list( idStr="stepWidth" ,thld=pThld ,sanc=pSanc )
    rObj$getBaseH <- function( pZoidH ){ 
                            return( pZoidH[,2:ncol(pZoidH),drop=F]
                                    - pZoidH[,1:(ncol(pZoidH)-1),drop=F] 
                                )
                        }
    rObj$baseH <- rObj$getBaseH( pBaseZh )
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        codeMtx <- rObj$getBaseH( pZoidMtx ) # 기작은 동일하므로..
        baseH <- rObj$getBaseH( pZoidH ) # 상위 NA 발생영역이 제거됨 유의.
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        if( rObj$thld <= sum(pCode==baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })

        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH=NULL ,pDebugInfo=F ){
        #   - pZoidH : 가장 최신 H. 여기선 사용치 않음.
        codeMtx <- rObj$getBaseH( pZoidMtx ) # 기작은 동일하므로..
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        if( rObj$thld <= sum(pCode==rObj$baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })

        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byBase()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){

        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst.fixed( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()
    return( rObj )
} # cliper.stepWidth

cliper.rebLen <- function( pBaseZh ,pThld=4 ,pSanc=20 ,pCodeVal=NULL ){
    rObj <- list( idStr="rebLen" ,thld=pThld ,sanc=pSanc )
    rObj$codeVal <- if( is.null(pCodeVal) ){
                        rObj$codeVal <- sort(unique(as.vector(pBaseZh)))
                    } else { pCodeVal }
    rObj$getBaseH <- function( pZh ){
        rebMtx <- matrix( 0 ,nrow=nrow(pZh) ,ncol=ncol(pZh) )
        rebMtx[1,] <- NA
        for( hIdx in 2:nrow(pZh) ){
            ml <- getReboundLst( pZh[hIdx,] ,pZh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
            rebMtx[hIdx,] <- 
                sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
        } # for(hIdx)
        naIndices <- which(apply(rebMtx,1,function(p){any(is.na(p))}))
        rebMtx <- rebMtx[(max(naIndices)+1):nrow(rebMtx),]
        return( rebMtx )
    } # rObj$getBaseH()
    rObj$baseH <- rObj$getBaseH( pBaseZh )
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        #   - pZoidH : zoidH원본을 받는 것으로 변경.(대량 pZoidMtx에 최적화 정책.)
        #   - pDebugInfo가 F이면 clip여부 T/F만, T이면 디버깅 정보 반환.
        rebLen <- getRebLen( rObj$codeVal ,pZoidH )
        codeMtx <- t(apply(pZoidMtx,1,function(p){rebLen[p]}))

        baseH <- rObj$getBaseH( pZoidH ) # 상위 NA 발생영역이 제거됨 유의.
        idxDiff <- nrow(pZoidH) - nrow(baseH) # 고로, baseH와 pZoidH의 rIdx 보정..
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        if( rObj$thld <= sum(pCode==baseH[rIdx,] ,na.rm=T) )
                            return( rIdx+idxDiff )
                    } # for(rIdx)
                    return( NA )
                })

        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        #   - pZoidH : 가장 최신 H. codeMax 생성을 위해 필요.
        rebLen <- getRebLen( rObj$codeVal ,pZoidH )
        codeMtx <- t(apply(pZoidMtx,1,function(p){rebLen[p]}))
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        if( rObj$thld <= sum(pCode==rObj$baseH[rIdx,] ,na.rm=T) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })

        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byBase()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){

        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst.fixed( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()

    return( rObj )
} # cliper.rebLen()

cliper.dupNum <- function( pBaseZh ,pThld=4 ,pSanc=2 ){
    #   - pThld : 이 갯수 이상으로 중복된 것을 절단 대상으로 다룸.
    #   - pSanc : 청정구역 크기. byLate() 에서 사용됨.
    rObj <- list( idStr="dupNum" ,thld=pThld ,sanc=pSanc )
    rObj$getBaseH <- function( pZh ){ return(pZh) }
    rObj$baseH <- rObj$getBaseH(pBaseZh)
    
    byLate <- function( pZoidMtx ,pZoidH ,pDebugInfo=F ){
        #   - pDebugInfo가 F이면 clip여부 T/F만, T이면 디버깅 정보 반환.
        codeMtx <-pZoidMtx
        baseH <- rObj$getBaseH( pZoidH )
        scanSpan <- (nrow(baseH)-rObj$sanc+1 ):nrow(baseH)
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in scanSpan ){
                        flag <- sapply(pCode,function(p){p%in%baseH[rIdx,]})
                        if( rObj$thld<=sum(flag) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }
    } # byLate()
    byBase <- function( pZoidMtx ,pZoidH=NULL ,pDebugInfo=F ){
        #   - pDebugInfo가 F이면 clip여부만, T이면 디버깅 정보 반환.
        #   - pZoidH : 가장 최신 H. 여기선 사용치 않음.
        codeMtx <-pZoidMtx
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(rObj$baseH) ){
                        flag <- sapply(pCode,function(p){p%in%rObj$baseH[rIdx,]})
                        if( rObj$thld<=sum(flag) )
                            return( rIdx )
                    } # for(rIdx)
                    return( NA )
                })
        
        if( pDebugInfo ) {    return( rst )
        } else {    return( !is.na(rst) ) }

    } # byBase()
    rObj$byLate <- byLate
    rObj$byBase <- byBase
    rObj$report <- function( pFile="./report/cliperReport.txt" ){
        k.FLogStr(sprintf("cliper id : %s",rObj$idStr) ,pFile=pFile)
        matchLst <- list()
        for( hIdx in 2:nrow(rObj$baseH) ){
            ml <- getMatchLst( rObj$baseH[hIdx,] ,rObj$baseH[1:(hIdx-1),,drop=F] )
            for( idx in seq_len(length(ml)) ){
                matLen <- length(ml[[idx]]$fIdx)
                if(rObj$thld<=matLen){
                    matchLst[[1+length(matchLst)]] <- 
                        c( hIdx ,ml[[idx]]$hIdx ,matLen)
                }
            } #for(idx)
        }

        mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
        k.FLog( sort(mtx[,1]-mtx[,2])[1:10] ,pFile=pFile )

        dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398
        k.FLogStr(sprintf("dupIndices : %d of %d",length(dupIndices),nrow(rObj$baseH))
                     ,pFile=pFile )

        testFlag <- rep(0,nrow(zh))
        testFlag[dupIndices] <- 1
        seqObj <- k.seq(testFlag)
        zeroFlag <- seqObj$seqCntMtx[,"val"]==0
        k.FLog( table(seqObj$seqCntMtx[!zeroFlag,"cnt"]) ,pFile=pFile )

    } # rObj$report()


    return( rObj )
} # cliper.dupNum()

