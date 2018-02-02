# 20180109_B_H.R Flag thread. Bank model.

createEleStatLst <- function( pEleSet ,pEleMean ,pSalScale=NULL){

    if( is.null(pSalScale) ){
        pSalScale <- rep( 1 ,length(pEleMean) )
    }

    eleStatLst <- list()
    for( idx in 1:length(pEleSet) ){
        eleObj <- list( val=pEleSet[idx] ,salary=pEleMean[idx]*pSalScale[idx] )
        eleObj$energy <- eleObj$salary  # 기본급. 즉 최소 발생확률은 항상 유지되어야 한다.
        eleObj$bank <- 0
        eleObj$hSpend <- pSalScale[idx]
        #------------ 
        eleObj$lastH<- rep( FALSE ,5 )  # 최근 발생기록.
        eleObj$eMax <- 1.0              # 보유가능 최대한계(군집성 대응용)
        eleObj$eMin <- eleObj$salary    # 보유가능 최대한계(군집성 대응용)

        eleStatLst[[sprintf("ele%d",pEleSet[idx])]] <- eleObj
    }

    return( eleStatLst )

} # createEleStatLst()


bank.quiet <- function( pEleObj ) {     # 미발생 시 뱅킹처리.

    totE    <- pEleObj$energy + pEleObj$bank + pEleObj$salary
    rEleObj <- pEleObj
    rEleObj$lastH   <- c( pEleObj$lastH[2:length(pEleObj$lastH)] ,FALSE )
    
    rEleObj$energy <- totE  ;rEleObj$bank <- 0
    if( rEleObj$eMin > totE ){
        rEleObj$energy <- rEleObj$eMin
        rEleObj$bank <- totE - rEleObj$eMin
    }
    if( rEleObj$eMax < totE ) {
        rEleObj$energy <- rEleObj$eMax
        rEleObj$bank <- totE - rEleObj$eMax
    }

    return( rEleObj )
} # bank.quiet()

bank.haunt <- function( pEleObj ) {     # 발생 시 뱅킹처리.

    totE    <- pEleObj$energy + pEleObj$bank + pEleObj$salary
    totE    <- totE - pEleObj$hSpend
    rEleObj <- pEleObj
    rEleObj$lastH   <- c( pEleObj$lastH[2:length(pEleObj$lastH)] ,TRUE )

    rEleObj$energy <- totE  ;rEleObj$bank <- 0
    if( rEleObj$eMin > totE ){
        rEleObj$energy <- rEleObj$eMin
        rEleObj$bank <- totE - rEleObj$eMin
    }
    if( rEleObj$eMax < totE ) {
        rEleObj$energy <- rEleObj$eMax
        rEleObj$bank <- totE - rEleObj$eMax
    }

    return( rEleObj )
} # bank.haunt()

