# u0_H.R unit model zero

zMtx <- tail(gEnv$allZoidMtx)

pVal <- zMtx[,4]
pCordLst <- lapply( 1:6 ,function(idx){ c(idx,1) })
# 1,1,1 / 1,2,3 / 2,4,6
u0.srchStep_std <- function( pVal ,pCordLst=NULL ){

    idxFlagLst <- u0.getChkIdx_std( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
    # same
    for( fIdx in seq_len(idxFlagLst) ){
        srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 2>srcVal.len ) next

        matCnt <- 0 # 연속이 몇 번 발생중인지.
        for( idx in 2:srcVal.len ){
            if( srcVal[idx]!=srcVal[1] ) break

            matCnt <- matCnt+1
        }
        if( matCnt==0 ) next

        banObj <- list( banVal=srcVal[1] ,certSize=matCnt )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_same( banObj ,idxFlagLst[[fIdx]] )
        banLst[[1+length(banLst)]] <- banObj

        if( (matCnt+1)<srcVal.len ){   # 끝자락 값은 대칭 방지를 위해 추가...
            banObj <- list( banVal=srcVal[matCnt+2] ,certSize=matCnt )
            banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$descript <- u0.getDescript_sameEnd( banObj ,idxFlagLst[[fIdx]] )
            banLst[[1+length(banLst)]] <- banObj
        }
    } # same for()
    

    return( banLst )

} # u0.srchStep_std

# 1,2,3,2,1 / 1,2,2,1
u0.srchStep_symm <- function( pVal ){

} # u0.srchStep_std

# 1,2,1,2,1,2
u0.srchStep_ptnReb <- function( pVal ){

} # u0.srchStep_std

# 1,2,2,2,1
u0.srchStep_seq <- function( pVal ){

} # u0.srchStep_std


u0.getChkIdx_std <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    tempFlag <- c( T ,T ,T ,T ,T ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[1]] <- tempFlag

    tempFlag <- c( F ,T ,F ,T ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[2]] <- tempFlag

    tempFlag <- c( F ,F ,T ,F ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[3]] <- tempFlag

    return(idxFlagLst)
} # u0.getChkIdx4std()

u0.getChkIdx_ptn <- function( pMaxLen=6 ){
    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 4<=maxLen ){
        idxFlagLst[[1]] <- list( chk1=0:1 ,ban=2 ,chk2=3:4 )
    }

    return(idxFlagLst)
} # u0.getChkIdx_ptn()

u0.getChkIdx_symm <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 4<=maxLen ){
        idxFlagLst[[1]] <- list( chk1=0:1 ,chk2=3:2 ,ban=4 )
    }
    if( 4<=maxLen ){
        idxFlagLst[[1]] <- list( chk1=0:1 ,chk2=4:3 ,ban=5 )
    }

    return(idxFlagLst)

} # u0.getChkIdx_symm()

u0.getDescript_same <- function( banObj ,idxFlag ){
    return("QQE work u0.getDescript_same")
}
u0.getDescript_sameEnd <- function( banObj ,idxFlag ){
    return("QQE work u0.getDescript_sameEnd")
}
