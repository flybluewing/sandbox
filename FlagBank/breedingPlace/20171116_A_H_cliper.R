# 20171116_A_H_cliper.R

# clp.dumNum <- cliper.dupNum( zh )
# clp.dumNum$report()
# baseH <- clp.dumNum$getBaseH( zh[5:15,] )   # 임의생성.
# zoidMtx <- zh[1:4,]                         # 임의생성.
# clp.dumNum$byBase( zoidMtx ,baseH )
# clp.dumNum$byLate( zoidMtx ,baseH )


cliper.dupNum <- function( pBaseZh ,pThld=4 ,pSanc=2 ){
    #   - pThld : 이 갯수 이상으로 중복된 것을 절단 대상으로 다룸.
    #   - pSanc : 청정구역 크기. byLate() 에서 사용됨.
    rObj <- list( idStr="dupNum" ,thld=pThld ,sanc=pSanc )
    rObj$getBaseH <- function( pZh ){ return(pZh) }
    rObj$baseH <- rObj$getBaseH(pBaseZh)
    
    byLate <- function( pZoidMtx ,pBaseH ,pDebugInfo=F ){ 
        return(rep(F,nrow(pZoidMtx))) 
    } # byLate()
    byBase <- function( pZoidMtx ,pBaseH ,pDebugInfo=F ){
        #   - pDebugInfo가 F이면 clip여부만, T이면 디버깅 정보 반환.
        codeMtx <-pZoidMtx
        rst <- apply( codeMtx ,1 ,function(pCode){
                    for( rIdx in 1:nrow(pBaseH) ){
                        flag <- sapply(pCode,function(p){p%in%pBaseH[rIdx,]})
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

