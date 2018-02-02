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


bank.quiet3 <- function( pEleObj ) {     # 미발생 시 뱅킹처리.

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
} # bank.quiet3()

bank.haunt3 <- function( pEleObj ) {     # 발생 시 뱅킹처리.

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
} # bank.haunt3()


bank.quiet2 <- function( pEleObj ) {     # 미발생 시 뱅킹처리.

    totE    <- pEleObj$energy + pEleObj$bank + pEleObj$salary
    rEleObj <- pEleObj
    rEleObj$lastH   <- c( pEleObj$lastH[2:length(pEleObj$lastH)] ,FALSE )
    if( !any(rEleObj$lastH) ){
        rEleObj$eMax <- pEleObj$salary*2
        rEleObj$eMax <- ifelse( rEleObj$eMax>0.5 ,0.5 ,rEleObj$eMax )
        # rEleObj$eMax <- 1.0
    } else {
        rEleObj$eMax <- 1.0
    }

    rEleObj$eMin <- pEleObj$salary
    
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
} # bank.quiet2()

bank.haunt2 <- function( pEleObj ) {     # 발생 시 뱅킹처리.

    totE    <- pEleObj$energy + pEleObj$bank + pEleObj$salary
    totE    <- totE - 0.5 # 1.0
    rEleObj <- pEleObj
    rEleObj$lastH   <- c( pEleObj$lastH[2:length(pEleObj$lastH)] ,TRUE )
    rEleObj$eMax <- 1.0
    if( 2<=sum(rEleObj$lastH) ){
        rEleObj$eMin <- pEleObj$salary*2
        rEleObj$eMin <- ifelse( rEleObj$eMin>0.5 ,0.5 ,rEleObj$eMin )
    }

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
} # bank.haunt2()



bank.haunt <- function( pEleObj ) {     # 발생 시 뱅킹처리.

    rEleObj <- pEleObj
    
    energy.tot <- (rEleObj$energy+rEleObj$bank) - 1.0
    if( 1.00001 < energy.tot ){     # 빼고나서도 에너지가 남으면 저축.
        rEleObj$energy <- 1.0
        rEleObj$bank <- (energy.tot-rEleObj$energy) + rEleObj$salary
    } else if( 0 < energy.tot ){
        energy.tot <- energy.tot + rEleObj$salary
        if( 1.00001 < energy.tot ){
            rEleObj$energy <- 1.0
            rEleObj$bank <- (energy.tot-rEleObj$energy)
        } else {
            rEleObj$bank <- 0.0
            rEleObj$energy <- energy.tot
        }
    } else if( 0 > energy.tot ){    # 발생 에너지 부족하면 대출 추가.
        rEleObj$bank <- energy.tot
        rEleObj$energy <- rEleObj$salary
    }

    return( rEleObj )
} # bank.haunt()

bank.quiet <- function( pEleObj ) {     # 미발생 시 뱅킹처리.
    rEleObj <- pEleObj

    energy.tot <- (rEleObj$energy+rEleObj$bank)
    if( 1.00001 < energy.tot ){
        rEleObj$energy <- 1.0
        rEleObj$bank <- (energy.tot-rEleObj$energy) + rEleObj$salary
    } else if( 0 < energy.tot ) {
        energy.tot <- energy.tot + rEleObj$salary
        if( 1.00001 < energy.tot ){
            rEleObj$energy <- 1.0
            rEleObj$bank <- (energy.tot-rEleObj$energy)
        } else {
            rEleObj$bank <- 0.0
            rEleObj$energy <- energy.tot
        }
    } else if( 0 > energy.tot ) {
        rEleObj$bank <- energy.tot
        rEleObj$energy <- rEleObj$salary        
    }

    return( rEleObj )
} # bank.haunt()

