# 20180109_B_H.R Flag thread. Bank model.

getBiCoder <- function( pZh ){
    
    rObj <- list()
    rObj$codeMtx <- matrix( c(0,0,1,1 ,0,1,0,1) ,nrow=4 ,ncol=2 )    
    rObj$enCode <- function( bCodeVal ){
        zipVal <- rep(0,length(bCodeVal)-1)
        for( idx in 2:length(bCodeVal) ){
            zipVal[idx-1] <- which(apply(rObj$codeMtx ,1 ,function(p){ all(p==bCodeVal[(idx-1):idx]) }))
        }
        return( zipVal )
    } # rObj$getCode()
    rObj$deCode <- function( zipVal ){
        bCodeVal <- rep( 0 ,length(zipVal)+1 )
        for( idx in 1:length(zipVal) ){
            bCodeVal[idx:(idx+1)] <- rObj$codeMtx[zipVal[idx],]
        }
        return( bCodeVal )
    } # rObj$deCode()

    rObj$stdCodeMtx <- t(apply( pZh%%2 ,1 ,rObj$enCode ))

    # --------------------------------------------------------------------------------------------
    rObj$getAllCode <- function( ){ # 모든 발생가능 ZipValue

        codeSeed <- seq_len(nrow(rObj$codeMtx))

        codeLst <- as.list( codeSeed )
        for( lvl in 2:5 ){
            extLst <- lapply( codeLst ,function(p){ 
                                lastVal <- p[length(p)]
                                nextVal <- which(rObj$codeMtx[lastVal,2]==rObj$codeMtx[,1])
                                return( lapply(nextVal,function(pp){c(p,pp)}) )
                            })
            lst <- extLst[[1]]
            for( lIdx in 2:length(extLst) ){
                lst <- append( lst ,extLst[[lIdx]] )
            }
            codeLst <- lst
        }

        return( do.call(rbind,codeLst) )
    } # rObj$getAllCode()

    return( rObj )
}


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


# ================================================================================================
# filt for getBiCoder()$allCodeMtx
# ================================================================================================
# pBiObj <- getBiCoder( zhF )       ;allCodeMtx <- pBiObj$getAllCode()

biObj_FA0010 <- function( pBiObj ,allCodeMtx ){
    # 21 / 791 (2.6%)
	filtId="biFA0010";	tStmp <- Sys.time()
	stdCodeMtx <- pBiObj$stdCodeMtx

    codeMtx <- allCodeMtx[,2:5]-allCodeMtx[,1:4]
    stepM <- apply( codeMtx ,1 ,function(p){max(table(p))})
	flag <- stepM<=4

	pEnv$logStr( sprintf("biF. ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # biObj_FA0010()

biObj_FA0020 <- function( pBiObj ,allCodeMtx ){
    # 21 / 791 (2.6%)
	filtId="biFA0020";	tStmp <- Sys.time()
	stdCodeMtx <- pBiObj$stdCodeMtx

    flag <- apply(allCodeMtx ,1 ,function(p){ max(table(p)) } )
	flag <- stepM<=5

	pEnv$logStr( sprintf("biF. ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # biObj_FA0020()

biObj_FA0030 <- function( pBiObj ,allCodeMtx ){
    # 8 / 791 (1%)
	filtId="biFA0030";	tStmp <- Sys.time()
	stdCodeMtx <- pBiObj$stdCodeMtx

    nStd <- nrow(stdCodeMtx)

    flag <- apply(allCodeMtx ,1 ,function(p){!all(p==stdCodeMtx[nStd,])} )

	pEnv$logStr( sprintf("biF. ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # biObj_FA0030()

