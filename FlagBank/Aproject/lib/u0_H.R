# u0_H.R unit model zero


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

