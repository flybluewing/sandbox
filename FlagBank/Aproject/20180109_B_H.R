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

# getFreqDist() 의 초기모델. 
#	평균내는 시점에 energy 값이 0.0 or 1.0으로 떨어진다.
getFreqDist.old <- function( pFlag ,pEleSet ,pDbg=F ){	# Frequency Distribute

	flag.len <- length(pFlag)
	eleMean <- sapply( pEleSet ,function(p){ sum(pFlag==p)/flag.len })
	eleStatLst <- createEleStatLst( pEleSet ,eleMean )

	for( hIdx in 1:flag.len ){
		hauntVal <- pFlag[hIdx]
		if( pDbg ){
			if( hIdx==flag.len ){				
				energyStr <- sprintf("%.3f" ,sapply( eleStatLst ,function(p){p$energy+p$bank}) )
				logStr <- sprintf("hIdx:%d [%d] %s" ,hIdx ,hauntVal ,paste(energyStr,collapse=" ") )
				k.FLogStr( logStr )

				meanStr <- sprintf("%.3f" ,sapply(eleStatLst ,function(p){p$salary}) )
				k.FLogStr(sprintf( "      mean : %s" ,paste(meanStr,collapse=" ") ))
			}
		} # if(pDbg)

		for( idx in 1:length(pEleSet) ){
			if( hauntVal==eleStatLst[[idx]]$val ){
				eleStatLst[[idx]] <- bank.haunt( eleStatLst[[idx]] )
			} else {
				eleStatLst[[idx]] <- bank.quiet( eleStatLst[[idx]] )
			}
		}
	} # for(hIdx)

	rFreqDist <- sapply( eleStatLst ,function(p){p$energy} )

	return( rFreqDist )
} # getFreqDist.old()
