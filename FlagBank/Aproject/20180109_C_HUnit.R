# 20180109_C_HUnit.R 교차모델

getOrthoLineLst <- function( pMtx ){

    nCol <- ncol(pMtx)
    if( 3>nCol ){
        return(list())
    }
    nRow <- nrow(pMtx)
    if( 2>nRow ){
        return(list())
    }

    lineLst.left <- list()
    col.left <- 3:nCol
    for( cIdx in col.left ){ # from left side
        rowSpan <- if( (cIdx-1)<=nRow ) (nRow-(cIdx-2)):nRow else 1:nRow
        colSpan <- ( (cIdx-1)-(length(rowSpan)-1) ):(cIdx-1)
        val <- rep( NA ,length(rowSpan) )
        for( idx in 1:length(colSpan) ){
            val[idx] <- pMtx[rowSpan[idx],colSpan[idx]]
            lineLst.left[[sprintf("c%d leftSide",cIdx)]] <- val
        }
    }

    lineLst.right <- list()
    col.right <- 1:(nCol-2)
    for( cIdx in col.right ){ # from right side
        reqSize <- nCol - cIdx
        rowSpan <- if( reqSize<=nRow ) (nRow-(reqSize-1)):nRow else 1:nRow
        colSpan <- ( (cIdx+1)+(length(rowSpan)-1) ) : (cIdx+1)
        val <- rep( NA ,length(rowSpan) )
        for( idx in 1:length(colSpan) ){
            val[idx] <- pMtx[rowSpan[idx],colSpan[idx]]
            lineLst.right[[sprintf("c%d rightSide",cIdx)]] <- val
        }
    }

    lineObj <- list( lineLst.left=lineLst.left ,col.left=col.left ,lineLst.right=lineLst.right ,col.right=col.right )

    return( lineObj )

} # getOrthoLineLst()

#   pECol : 예외대상 컬럼. Exception column
getUnitAnalyzer <- function( pMtx ,pECol=NULL ,pWidth=TRUE ){
    # nrow(pMtx) 는 2 이상을 사용하자.
    getDInfo <- function( pMtx ,pECol ){
        dSize <- nrow(pMtx) ;dWidth <- ncol(pMtx)
        dLast <- pMtx[dSize,]
        dCStep <- dLast[2:dWidth] - dLast[1:(dWidth-1)]
        dFStep <- if( dSize>1 ) pMtx[dSize,]-pMtx[(dSize-1),] else NULL

        dInfo <- list( dLast=dLast ,dCStep=dCStep ,dFStep=dFStep ,dSize=dSize ,dWidth=dWidth )
        dInfo$eCol <- pECol
        return( dInfo )
    } # getDInfo()
    getColValBan <- function( pWorkMtx ,pDInfo ,pFunc ){ # function( val )
        colValBan <- list()
        for( cIdx in 1:ncol(pWorkMtx) ){
            banVal <- pFunc( pWorkMtx[,cIdx] )
            if( !is.null(pDInfo$eCol) && (cIdx%in%pDInfo$eCol) ){
                banVal <- NULL
            }
            colValBan[[cIdx]] <- if( is.null(banVal) ) integer(0) else banVal
        }

        return(colValBan)
    } # getColValBan()
    getColValBan.leftLine <- function( pLineObj ,pFunc ,pNCol=6 ){ # function( val )
        colValBan <- lapply( 1:pNCol ,function(idx){integer(0)} )
        for( cIdx in 1:pNCol ){
            fndIdx <- which( pLineObj$col.left ==cIdx )
            if( 0<length(fndIdx) ){
                banVal <- pFunc( pLineObj$lineLst.left[[fndIdx]] )
                colValBan[[cIdx]] <- if( is.null(banVal) ) integer(0) else banVal
            }
        }
        return(colValBan)
    } # getColValBan.leftLine()
    getColValBan.rightLine <- function( pLineObj ,pFunc ,pNCol=6 ){ # function( val )
        colValBan <- lapply( 1:pNCol ,function(idx){integer(0)} )
        for( cIdx in 1:pNCol ){
            fndIdx <- which( pLineObj$col.right==cIdx )
            if( 0<length(fndIdx) ){
                banVal <- pFunc( pLineObj$lineLst.right[[fndIdx]] )
                colValBan[[cIdx]] <- if( is.null(banVal) ) integer(0) else banVal
            }
        }
        return(colValBan)
    } # getColValBan.rightLine()
    getWidthBan <- function( pWidth ,pFunc ){ # function( val )
        banVal <- pFunc( pWidth )
        return(banVal)
    } # getColValBan()


    dInfo <- getDInfo( pMtx=pMtx ,pECol=pECol )
    cStepMtx <- pMtx[,2:6,drop=F] - pMtx[,1:5,drop=F]
    fStepMtx <- if( 1>=nrow(pMtx) ) {
                    matrix( 0 ,nrow=0 ,ncol=ncol(pMtx) )
                } else {
                    pMtx[2:dInfo$dSize,,drop=F] - pMtx[1:(dInfo$dSize-1),,drop=F]
                }

    # ==========================================================================
    #   RAW Data
    colValBanLst <- list()
    valPtnBanLst <- list()
    # --------------------------------------------------------------------------
    # Basic filtering
    workMtx <- pMtx
    colValBanLst[["lastReb" ]] <- getColValBan( workMtx ,dInfo ,uaUtil.lastReb  )
    colValBanLst[["decline1"]] <- getColValBan( workMtx ,dInfo ,uaUtil.decline1 )
    colValBanLst[["decline2"]] <- getColValBan( workMtx ,dInfo ,uaUtil.decline2 )
    colValBanLst[["rebAgain"]] <- getColValBan( workMtx ,dInfo ,uaUtil.rebAgain )
    colValBanLst[["rebPtn1" ]] <- getColValBan( workMtx ,dInfo ,uaUtil.rebPtn1  )
    colValBanLst[["rebPtn2" ]] <- getColValBan( workMtx ,dInfo ,uaUtil.rebPtn2  )
    colValBanLst[["rebPtn3" ]] <- getColValBan( workMtx ,dInfo ,uaUtil.rebPtn3  )
    colValBanLst[["rebPtn4" ]] <- getColValBan( workMtx ,dInfo ,uaUtil.rebPtn4  )
    colValBanLst[["symm1"   ]] <- getColValBan( workMtx ,dInfo ,uaUtil.symm1    )
    colValBanLst[["symm2"   ]] <- getColValBan( workMtx ,dInfo ,uaUtil.symm2    )
    colValBanLst[["symm3"   ]] <- getColValBan( workMtx ,dInfo ,uaUtil.symm3    )
    valPtnBanLst[["seqReb"  ]] <- uaUtil.seqReb( workMtx ,pECol=dInfo$eCol      )
    # --------------------------------------------------------------------------
    # two step
    if( 4<=dInfo$dSize ){
        rowSpan <- seq( from=(dInfo$dSize-1) ,to=1 ,by=-2 )
        workMtx <- pMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=pECol )
        colValBanLst[["lastReb.s2" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst[["decline1.s2"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst[["decline2.s2"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst[["rebAgain.s2"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst[["rebPtn1.s2" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst[["rebPtn2.s2" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst[["rebPtn3.s2" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst[["rebPtn4.s2" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst[["symm1.s2"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst[["symm2.s2"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst[["symm3.s2"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
        valPtnBanLst[["seqReb.s2"  ]] <- uaUtil.seqReb( workMtx ,pECol=dInfo.work$eCol      )
    }
    # --------------------------------------------------------------------------
    # three step
    if( 6<=dInfo$dSize ){
        rowSpan <- seq( from=(dInfo$dSize-2) ,to=1 ,by=-3 )
        workMtx <- pMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=pECol )
        colValBanLst[["lastReb.s3" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst[["decline1.s3"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst[["decline2.s3"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst[["rebAgain.s3"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst[["rebPtn1.s3" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst[["rebPtn2.s3" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst[["rebPtn3.s3" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst[["rebPtn4.s3" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst[["symm1.s3"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst[["symm2.s3"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst[["symm3.s3"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
        valPtnBanLst[["seqReb.s3"  ]] <- uaUtil.seqReb( workMtx ,pECol=dInfo.work$eCol      )
    }

    # ==========================================================================
    #   fStep Data
    colValBanLst.f <- list()
    valPtnBanLst.f <- list()
    # --------------------------------------------------------------------------
    # Basic filtering
    workMtx <- fStepMtx
    dInfo.work <- getDInfo( pMtx=workMtx ,pECol=pECol )
    colValBanLst.f[["lastReb_F" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.lastReb  )
    colValBanLst.f[["decline1_F"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.decline1 )
    colValBanLst.f[["decline2_F"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.decline2 )
    colValBanLst.f[["rebAgain_F"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebAgain )
    colValBanLst.f[["rebPtn1_F" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn1  )
    colValBanLst.f[["rebPtn2_F" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn2  )
    colValBanLst.f[["rebPtn3_F" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn3  )
    colValBanLst.f[["rebPtn4_F" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn4  )
    colValBanLst.f[["symm1_F"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm1    )
    colValBanLst.f[["symm2_F"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm2    )
    colValBanLst.f[["symm3_F"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm3    )
    valPtnBanLst.f[["seqReb_F"  ]] <- uaUtil.seqReb( workMtx ,pECol=pECol      )
    # --------------------------------------------------------------------------
    # two step
    if( 4<=nrow(fStepMtx) ){
        rowSpan <- seq( from=(nrow(fStepMtx)-1) ,to=1 ,by=-2 )
        workMtx <- fStepMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=pECol )
        colValBanLst.f[["lastReb.s2_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst.f[["decline1.s2_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst.f[["decline2.s2_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst.f[["rebAgain.s2_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst.f[["rebPtn1.s2_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst.f[["rebPtn2.s2_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst.f[["rebPtn3.s2_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst.f[["rebPtn4.s2_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst.f[["symm1.s2_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst.f[["symm2.s2_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst.f[["symm3.s2_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
        valPtnBanLst.f[["seqReb.s2_F"  ]] <- uaUtil.seqReb( workMtx ,pECol=pECol      )
    }
    # --------------------------------------------------------------------------
    # three step
    if( 6<=nrow(fStepMtx) ){
        rowSpan <- seq( from=(nrow(fStepMtx)-2) ,to=1 ,by=-3 )
        workMtx <- fStepMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=pECol )
        colValBanLst.f[["lastReb.s3_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst.f[["decline1.s3_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst.f[["decline2.s3_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst.f[["rebAgain.s3_F"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst.f[["rebPtn1.s3_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst.f[["rebPtn2.s3_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst.f[["rebPtn3.s3_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst.f[["rebPtn4.s3_F" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst.f[["symm1.s3_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst.f[["symm2.s3_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst.f[["symm3.s3_F"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
        valPtnBanLst.f[["seqReb.s3_F"  ]] <- uaUtil.seqReb( workMtx ,pECol=pECol      )
    }


    # ==========================================================================
    #   cStep Data
    colValBanLst.c <- list()
    # valPtnBanLst.f <- list() 일단 사용보류.
    # --------------------------------------------------------------------------
    # Basic filtering
    workMtx <- cStepMtx
    dInfo.work <- getDInfo( pMtx=workMtx ,pECol=NULL )
    colValBanLst.c[["lastReb_C" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.lastReb  )
    colValBanLst.c[["decline1_C"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.decline1 )
    colValBanLst.c[["decline2_C"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.decline2 )
    colValBanLst.c[["rebAgain_C"]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebAgain )
    colValBanLst.c[["rebPtn1_C" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn1  )
    colValBanLst.c[["rebPtn2_C" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn2  )
    colValBanLst.c[["rebPtn3_C" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn3  )
    colValBanLst.c[["rebPtn4_C" ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.rebPtn4  )
    colValBanLst.c[["symm1_C"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm1    )
    colValBanLst.c[["symm2_C"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm2    )
    colValBanLst.c[["symm3_C"   ]] <- getColValBan(  workMtx ,dInfo.work ,uaUtil.symm3    )
    # --------------------------------------------------------------------------
    # two step
	if( 4<=nrow(cStepMtx) ){
        rowSpan <- seq( from=(nrow(cStepMtx)-1) ,to=1 ,by=-2 )
        workMtx <- cStepMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=NULL )
        colValBanLst.c[["lastReb.s2_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst.c[["decline1.s2_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst.c[["decline2.s2_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst.c[["rebAgain.s2_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst.c[["rebPtn1.s2_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst.c[["rebPtn2.s2_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst.c[["rebPtn3.s2_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst.c[["rebPtn4.s2_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst.c[["symm1.s2_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst.c[["symm2.s2_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst.c[["symm3.s2_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
	}
    # --------------------------------------------------------------------------
    # three step
	if( 6<=nrow(cStepMtx) ){
        rowSpan <- seq( from=(nrow(cStepMtx)-2) ,to=1 ,by=-3 )
        workMtx <- cStepMtx[sort(rowSpan),,drop=F]
        dInfo.work <- getDInfo( pMtx=workMtx ,pECol=NULL )
        colValBanLst.c[["lastReb.s3_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.lastReb  )
        colValBanLst.c[["decline1.s3_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline1 )
        colValBanLst.c[["decline2.s3_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.decline2 )
        colValBanLst.c[["rebAgain.s3_C"]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebAgain )
        colValBanLst.c[["rebPtn1.s3_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn1  )
        colValBanLst.c[["rebPtn2.s3_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn2  )
        colValBanLst.c[["rebPtn3.s3_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn3  )
        colValBanLst.c[["rebPtn4.s3_C" ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.rebPtn4  )
        colValBanLst.c[["symm1.s3_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm1    )
        colValBanLst.c[["symm2.s3_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm2    )
        colValBanLst.c[["symm3.s3_C"   ]] <- getColValBan( workMtx ,dInfo.work ,uaUtil.symm3    )
	}

    # ==========================================================================
    #   width
    widthBanLst <- list()
    if( pWidth ){
        # --------------------------------------------------------------------------
        # Basic filtering
        zWidth <- pMtx[,6] - pMtx[,1]
        banVal <- getWidthBan( zWidth ,uaUtil.lastReb )
        if( !is.null(banVal) )  widthBanLst[["lastReb" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.decline1 )
        if( !is.null(banVal) )  widthBanLst[["decline1" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.decline2 )
        if( !is.null(banVal) )  widthBanLst[["decline2" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.rebAgain )
        if( !is.null(banVal) )  widthBanLst[["rebAgain" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.rebPtn1 )
        if( !is.null(banVal) )  widthBanLst[["rebPtn1" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.rebPtn2 )
        if( !is.null(banVal) )  widthBanLst[["rebPtn2" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.rebPtn3 )
        if( !is.null(banVal) )  widthBanLst[["rebPtn3" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.rebPtn4 )
        if( !is.null(banVal) )  widthBanLst[["rebPtn4" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.symm1 )
        if( !is.null(banVal) )  widthBanLst[["symm1" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.symm2 )
        if( !is.null(banVal) )  widthBanLst[["symm2" ]] <- banVal
        banVal <- getWidthBan( zWidth ,uaUtil.symm3 )
        if( !is.null(banVal) )  widthBanLst[["symm3" ]] <- banVal

        # --------------------------------------------------------------------------
        # two step
        if( 4<=length(zWidth) ){
            rowSpan <- seq( from=(length(zWidth)-1) ,to=1 ,by=-2 )
            workWidth <- zWidth[sort(rowSpan)]
            banVal <- getWidthBan( workWidth ,uaUtil.lastReb )
            if( !is.null(banVal) )  widthBanLst[["lastReb.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.decline1 )
            if( !is.null(banVal) )  widthBanLst[["decline1.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.decline2 )
            if( !is.null(banVal) )  widthBanLst[["decline2.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebAgain )
            if( !is.null(banVal) )  widthBanLst[["rebAgain.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn1 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn1.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn2 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn2.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn3 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn3.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn4 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn4.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm1 )
            if( !is.null(banVal) )  widthBanLst[["symm1.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm2 )
            if( !is.null(banVal) )  widthBanLst[["symm2.s2" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm3 )
            if( !is.null(banVal) )  widthBanLst[["symm3.s2" ]] <- banVal
        }

        # --------------------------------------------------------------------------
        # three step
        if( 6<=length(zWidth) ){
            rowSpan <- seq( from=(length(zWidth)-2) ,to=1 ,by=-3 )
            workWidth <- zWidth[sort(rowSpan)]
            banVal <- getWidthBan( workWidth ,uaUtil.lastReb )
            if( !is.null(banVal) )  widthBanLst[["lastReb.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.decline1 )
            if( !is.null(banVal) )  widthBanLst[["decline1.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.decline2 )
            if( !is.null(banVal) )  widthBanLst[["decline2.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebAgain )
            if( !is.null(banVal) )  widthBanLst[["rebAgain.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn1 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn1.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn2 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn2.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn3 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn3.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.rebPtn4 )
            if( !is.null(banVal) )  widthBanLst[["rebPtn4.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm1 )
            if( !is.null(banVal) )  widthBanLst[["symm1.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm2 )
            if( !is.null(banVal) )  widthBanLst[["symm2.s3" ]] <- banVal
            banVal <- getWidthBan( workWidth ,uaUtil.symm3 )
            if( !is.null(banVal) )  widthBanLst[["symm3.s3" ]] <- banVal
        }

    } # pWidth

    # ==========================================================================
    # ortho line (step 2 정도는 추가하는 게 좋으려나?)
    orthoBanLst <- list()
    lineObj <- getOrthoLineLst( pMtx )
    orthoBanLst[["lastReb.othL" ]] <- getColValBan.leftLine(  lineObj ,uaUtil.lastReb  )
    orthoBanLst[["decline1.othL"]] <- getColValBan.leftLine(  lineObj ,uaUtil.decline1 )
    orthoBanLst[["decline2.othL"]] <- getColValBan.leftLine(  lineObj ,uaUtil.decline2 )
    orthoBanLst[["rebAgain.othL"]] <- getColValBan.leftLine(  lineObj ,uaUtil.rebAgain )
    orthoBanLst[["rebPtn1.othL" ]] <- getColValBan.leftLine(  lineObj ,uaUtil.rebPtn1  )
    orthoBanLst[["rebPtn2.othL" ]] <- getColValBan.leftLine(  lineObj ,uaUtil.rebPtn2  )
    orthoBanLst[["rebPtn3.othL" ]] <- getColValBan.leftLine(  lineObj ,uaUtil.rebPtn3  )
    orthoBanLst[["rebPtn4.othL" ]] <- getColValBan.leftLine(  lineObj ,uaUtil.rebPtn4  )
    orthoBanLst[["symm1.othL"   ]] <- getColValBan.leftLine(  lineObj ,uaUtil.symm1    )
    orthoBanLst[["symm2.othL"   ]] <- getColValBan.leftLine(  lineObj ,uaUtil.symm2    )
    orthoBanLst[["symm3.othL"   ]] <- getColValBan.leftLine(  lineObj ,uaUtil.symm3    )
    orthoBanLst[["lastReb.othR" ]] <- getColValBan.rightLine( lineObj ,uaUtil.lastReb  )
    orthoBanLst[["decline1.othR"]] <- getColValBan.rightLine( lineObj ,uaUtil.decline1 )
    orthoBanLst[["decline2.othR"]] <- getColValBan.rightLine( lineObj ,uaUtil.decline2 )
    orthoBanLst[["rebAgain.othR"]] <- getColValBan.rightLine( lineObj ,uaUtil.rebAgain )
    orthoBanLst[["rebPtn1.othR" ]] <- getColValBan.rightLine( lineObj ,uaUtil.rebPtn1  )
    orthoBanLst[["rebPtn2.othR" ]] <- getColValBan.rightLine( lineObj ,uaUtil.rebPtn2  )
    orthoBanLst[["rebPtn3.othR" ]] <- getColValBan.rightLine( lineObj ,uaUtil.rebPtn3  )
    orthoBanLst[["rebPtn4.othR" ]] <- getColValBan.rightLine( lineObj ,uaUtil.rebPtn4  )
    orthoBanLst[["symm1.othR"   ]] <- getColValBan.rightLine( lineObj ,uaUtil.symm1    )
    orthoBanLst[["symm2.othR"   ]] <- getColValBan.rightLine( lineObj ,uaUtil.symm2    )
    orthoBanLst[["symm3.othR"   ]] <- getColValBan.rightLine( lineObj ,uaUtil.symm3    )


    uAnaObj <- list( colValBanLst=colValBanLst ,valPtnBanLst=valPtnBanLst )
    uAnaObj$colValBanLst.f <- colValBanLst.f
    uAnaObj$valPtnBanLst.f <- valPtnBanLst.f
    uAnaObj$colValBanLst.c <- colValBanLst.c
    uAnaObj$colValBanLst.c <- list()
    uAnaObj$widthBanLst <- widthBanLst
    uAnaObj$orthoBanLst <- orthoBanLst
    uAnaObj$dInfo <- dInfo

    return( uAnaObj )

} # getUnitAnalyzer()

rptUnitAnalyze <- function( uAnaObj ,pTitle="" ,pRptFile="./report/rptUnitAnalyze" ){

    log.txt <- sprintf("%s.txt",pRptFile)
    FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
            if( !is.null(pRptFile) )
                k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
        }
    FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
            if( !is.null(pRptFile) )
                k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
        }

    FLogStr(sprintf("Report : %s",pTitle),pAppend=F,pTime=T)
    FLogStr(sprintf("dInfo - row:%d col:%d",uAnaObj$dInfo$dSize,uAnaObj$dInfo$dWidth))
    FLogStr("lastZoid ,fStep ,cStep")
    FLog( uAnaObj$dInfo$dLast )
    FLog( uAnaObj$dInfo$dFStep )
    FLog( uAnaObj$dInfo$dCStep )

    FLogStr("#-[colValBanLst]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$colValBanLst)$names ,function( nIdx ){
                    fndSize <- sapply( uAnaObj$colValBanLst[[nIdx]] ,length )
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(length(uAnaObj$colValBanLst[[nIdx]])) ){
                            if( 0==length(uAnaObj$colValBanLst[[nIdx]][[idx]]) ){
                                next
                            }
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$colValBanLst[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })
    FLogStr("#-[valPtnBanLst]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$valPtnBanLst)$names ,function( nIdx ){
                    fndSize <- length(uAnaObj$valPtnBanLst[[nIdx]])
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(fndSize) ){
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$valPtnBanLst[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })
    FLogStr("#-[colValBanLst.f]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$colValBanLst.f)$names ,function( nIdx ){
                    fndSize <- sapply( uAnaObj$colValBanLst.f[[nIdx]] ,length )
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(length(uAnaObj$colValBanLst.f[[nIdx]])) ){
                            if( 0==length(uAnaObj$colValBanLst.f[[nIdx]][[idx]]) ){
                                next
                            }
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$colValBanLst.f[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })
    FLogStr("#-[valPtnBanLst.f]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$valPtnBanLst.f)$names ,function( nIdx ){
                    fndSize <- length(uAnaObj$valPtnBanLst.f[[nIdx]])
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(fndSize) ){
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$valPtnBanLst.f[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })

    FLogStr("#-[colValBanLst.c]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$colValBanLst.c)$names ,function( nIdx ){
                    fndSize <- sapply( uAnaObj$colValBanLst.c[[nIdx]] ,length )
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(length(uAnaObj$colValBanLst.c[[nIdx]])) ){
                            if( 0==length(uAnaObj$colValBanLst.c[[nIdx]][[idx]]) ){
                                next
                            }
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$colValBanLst.c[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })
    FLogStr("#-[widthBanLst]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$widthBanLst)$names ,function( nIdx ){
                    FLogStr(sprintf("<%s> %s",nIdx,paste(uAnaObj$widthBanLst[[nIdx]],collapse=", ")))
                })
    FLogStr("#-[orthoBanLst]-------------------------------------------------------------------------------------------------------")
    a <- sapply( attributes(uAnaObj$orthoBanLst)$names ,function( nIdx ){
                    fndSize <- sapply( uAnaObj$orthoBanLst[[nIdx]] ,length )
                    if( 0<sum(fndSize) ){
                        FLogStr(sprintf("<%s>",nIdx))
                        for( idx in seq_len(length(uAnaObj$orthoBanLst[[nIdx]])) ){
                            if( 0==length(uAnaObj$orthoBanLst[[nIdx]][[idx]]) ){
                                next
                            }
                            FLogStr(sprintf("   %d : %s",idx,paste(uAnaObj$orthoBanLst[[nIdx]][[idx]],collapse=", ")  ))
                        }
                    }
                })

} # rptUnitAnalyze()

getStdCutData_UnitAnaObj <- function( uAnaObj ){

    #======================================================================================
    #   Raw Value
    # colVal : colValBanLst ,orthoBanLst
    colVal <- lapply( 1:6 ,function(idx){ 
                    if( idx %in% uAnaObj$dInfo$eCol ){
                        data.frame( fName=character(0) ,banVal=integer(0) ) 
                    } else {
                        data.frame( fName="lastVal" ,banVal=uAnaObj$dInfo$dLast[idx] ) 
                    }
                })
    banLst <- uAnaObj$colValBanLst
    for( nIdx in attributes(banLst)$names ){
        for( cIdx in 1:length(banLst[[nIdx]]) ){
            for( vIdx in seq_len(length(banLst[[nIdx]][[cIdx]])) ){
                colVal[[cIdx]] <- rbind( colVal[[cIdx]] ,data.frame(fName=nIdx ,banVal=banLst[[nIdx]][[cIdx]][vIdx]) )
            }
        }
    }
    banLst <- uAnaObj$orthoBanLst
    for( nIdx in attributes(banLst)$names ){
        for( cIdx in 1:length(banLst[[nIdx]]) ){
            for( vIdx in seq_len(length(banLst[[nIdx]][[cIdx]])) ){
                colVal[[cIdx]] <- rbind( colVal[[cIdx]] ,data.frame(fName=nIdx ,banVal=banLst[[nIdx]][[cIdx]][vIdx]) )
            }
        }
    }

    # valPtn
    valPtn <- list()
    valPtn.name <- character(0)
    banLst <- uAnaObj$valPtnBanLst
    for( nIdx in attributes(banLst)$names ){
        for( ptnIdx in seq_len(length(banLst[[nIdx]])) ){
            valPtn.name[1+length(valPtn.name)] <- nIdx
            valPtn[[1+length(valPtn)]] <- banLst[[nIdx]][[ptnIdx]]
        }
    }


    #======================================================================================
    #   fStep
    # colVal : colValBanLst.f
    colVal.f <- lapply( 1:6 ,function(idx){ 
                    data.frame( fName=character(0) ,banVal=integer(0) ) 
                })
    banLst <- uAnaObj$colValBanLst.f
    for( nIdx in attributes(banLst)$names ){
        for( cIdx in 1:length(banLst[[nIdx]]) ){
            for( vIdx in seq_len(length(banLst[[nIdx]][[cIdx]])) ){
                colVal.f[[cIdx]] <- rbind( colVal.f[[cIdx]] ,data.frame(fName=nIdx ,banVal=banLst[[nIdx]][[cIdx]][vIdx]) )
            }
        }
    }

    # valPtn.f
    valPtn.f <- list()
    valPtn.f.name <- character(0)
    banLst <- uAnaObj$valPtnBanLst.f
    for( nIdx in attributes(banLst)$names ){
        for( ptnIdx in seq_len(length(banLst[[nIdx]])) ){
            valPtn.f.name[1+length(valPtn.f.name)] <- nIdx
            valPtn.f[[1+length(valPtn.f)]] <- banLst[[nIdx]][[ptnIdx]]
        }
    }

    #======================================================================================
    #   cStep
    # colVal : colValBanLst.c
    colVal.c <- lapply( 1:5 ,function(idx){ 
                    data.frame( fName=character(0) ,banVal=integer(0) )
                })
    banLst <- uAnaObj$colValBanLst.c
    for( nIdx in attributes(banLst)$names ){
        for( cIdx in 1:length(banLst[[nIdx]]) ){
            for( vIdx in seq_len(length(banLst[[nIdx]][[cIdx]])) ){
                colVal.c[[cIdx]] <- rbind( colVal.c[[cIdx]] ,data.frame(fName=nIdx ,banVal=banLst[[nIdx]][[cIdx]][vIdx]) )
            }
        }
    }

    # zWidth
    zWidth <- list()
    zWidth.name <- character(0)
    banLst <- uAnaObj$widthBanLst
    for( nIdx in attributes(banLst)$names ){
        for( ptnIdx in seq_len(length(banLst[[nIdx]])) ){
            zWidth.name[1+length(zWidth.name)] <- nIdx
            zWidth[[1+length(zWidth)]] <- banLst[[nIdx]][[ptnIdx]]
        }
    }

    cutDataObj <- list( colVal=colVal ,valPtn=valPtn )
    cutDataObj$colVal.f <- colVal.f
    cutDataObj$valPtn.f <- valPtn.f
    cutDataObj$colVal.c <- colVal.c
    cutDataObj$zWidth <- zWidth

    return( cutDataObj )

} # getStdCutData_UnitAnaObj( )

getStdCutDataGrp <- function( uAnaLstGrp ){

    iGetColDf <- function( valLst ,gName ){
        tDf <- data.frame( gName=character(0) ,fName=character(0) ,col=integer(0) ,banVal=integer(0) )
        for( idx in seq_len(length(valLst)) ){
            if( 0==nrow(valLst[[idx]]) ){
                next
            }
            df <- data.frame( gName=gName ,fName=valLst[[idx]]$fName ,col=idx ,banVal=valLst[[idx]]$banVal )
            tDf <- rbind( tDf ,df )
        }
        return( tDf )
    } # iGetColDf()
    iGetWidthDf <- function( valLst ,gName ){
        tDf <- data.frame( gName=character(0) ,banVal=integer(0) )
        for( idx in seq_len(length(valLst)) ){
            if( 0==length(valLst[[idx]]) ){
                next
            }
            df <- data.frame( gName=gName ,banVal=valLst[[idx]] )
            tDf <- rbind( tDf ,df )
        }
        return( tDf )
    } # iGetWidthDf()


    df.colVal   <- data.frame( gName=character(0) ,fName=character(0) ,col=integer() ,banVal=integer(0) )
    df.fStep    <- data.frame( gName=character(0) ,fName=character(0) ,col=integer() ,banVal=integer(0) )
    df.cStep    <- data.frame( gName=character(0) ,fName=character(0) ,col=integer() ,banVal=integer(0) )
    df.width    <- data.frame( gName=character(0) ,banVal=integer(0) )
    # valPtn은 어찌 표현할 방법이..

    uAnaLst <- uAnaLstGrp$uAnaLst.rawData
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.rebCnt
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.colVal1
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.colVal3
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.colVal4
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.colVal6
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.nextZW
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    uAnaLst <- uAnaLstGrp$uAnaLst.zw
    for( idx in seq_len(length(uAnaLst)) ){
        stdCut <- getStdCutData_UnitAnaObj( uAnaLst[[idx]]$uAnaObj )
        # colVal
        df.colVal   <- rbind( df.colVal,iGetColDf( stdCut$colVal,uAnaLst[[idx]]$idStr ) )
        df.fStep    <- rbind( df.fStep ,iGetColDf( stdCut$fStep ,uAnaLst[[idx]]$idStr ) )
        df.cStep    <- rbind( df.cStep ,iGetColDf( stdCut$cStep ,uAnaLst[[idx]]$idStr ) )
        df.width    <- rbind( df.width ,iGetWidthDf( stdCut$zWidth ,uAnaLst[[idx]]$idStr ) )
    }

    return( list(df.colVal=df.colVal ,df.fStep=df.fStep ,df.cStep=df.cStep ,df.width=df.width ) )

} # getStdCutDataGrp( )

stdFltCntByUA <- function( gEnv ,uAnaCutData ,aZoidMtx ,pDebugIdx=NULL ){    # aZoidMtx 는 allZoidMtx에서 이미 해당 조건을 만족시킨 그룹

    cStepMtx <- aZoidMtx[,2:6,drop=F] - aZoidMtx[,1:5,drop=F]
    fStepMtx <- t(apply( aZoidMtx ,1 ,function(aZoid){ aZoid-gEnv$zhF[nrow(gEnv$zhF),] }))

    fltCnt <- rep( 0 ,nrow(aZoidMtx) )

    dbg.colVal  <- 0    ;dbg.colVal.f <- 0     ;dbg.colVal.c <- 0
    dbg.valPtn  <- 0    ;dbg.valPtn.f <- 0
    dgb.azWidth <- 0


    # colVal ,colVal.f
    for( aIdx in 1:nrow(aZoidMtx) ){
        for( cIdx in 1:6 ){
            if( 0<nrow(uAnaCutData$colVal[[cIdx]]["banVal"]) ){
                cnt <- sum( uAnaCutData$colVal[[cIdx]]["banVal"]==aZoidMtx[aIdx,cIdx] )
                fltCnt[aIdx] <- fltCnt[aIdx] + cnt
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dbg.colVal <- dbg.colVal + cnt
                }
            }
            if( 0<nrow(uAnaCutData$colVal.f[[cIdx]]["banVal"]) ){
				cnt <- sum( uAnaCutData$colVal.f[[cIdx]]["banVal"]==fStepMtx[aIdx,cIdx] )
				fltCnt[aIdx] <- fltCnt[aIdx] + cnt
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dbg.colVal.f <- dbg.colVal.f + cnt
                }
            }
        }
    }
    # colVal.c
    for( aIdx in 1:nrow(aZoidMtx) ){
        for( cIdx in 1:5 ){
            if( 0<nrow(uAnaCutData$colVal.c[[cIdx]]["banVal"]) ){
                cnt <- sum( uAnaCutData$colVal.c[[cIdx]]["banVal"]==cStepMtx[aIdx,cIdx] )
                fltCnt[aIdx] <- fltCnt[aIdx] + cnt
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dbg.colval.c <- dbg.colval.c + cnt
                }
            }
        }
    }

    # valPtn ,valPtn.f
    for( aIdx in 1:nrow(aZoidMtx) ){
        # uAnaCutData$valPtn
        for( idx in seq_len(length(uAnaCutData$valPtn)) ) {
            matCnt <- chkHaveSeq( fStepMtx[aIdx,] ,uAnaCutData$valPtn[[idx]] )
            if( matCnt>0 ){
                fltCnt[aIdx] <- fltCnt[aIdx] + 1    # 사실 matCnt 갯수대로 추가해도 될 듯.
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dbg.valPtn <- dbg.valPtn + 1
                }
            }
        }
        for( idx in seq_len(length(uAnaCutData$valPtn.f)) ) {
            matCnt <- chkHaveSeq( fStepMtx[aIdx,] ,uAnaCutData$valPtn.f[[idx]] )
            if( matCnt>0 ){
                fltCnt[aIdx] <- fltCnt[aIdx] + 1    # 사실 matCnt 갯수대로 추가해도 될 듯.
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dbg.valPtn.f <- dbg.valPtn.f + 1
                }
            }
        }
    }

    # zWidth
    for( aIdx in 1:nrow(aZoidMtx) ){
        azWidth <- aZoidMtx[aIdx,6] - aZoidMtx[aIdx,1]
        for( idx in seq_len(length(uAnaCutData$zWidth)) ){
            if( azWidth %in% uAnaCutData$zWidth[[idx]] ){
                fltCnt[aIdx] <- fltCnt[aIdx] + 1
                if( !is.null(pDebugIdx) && (aIdx==pDebugIdx) ){
                    dgb.azWidth <- dgb.azWidth + 1
                }
            }
        }
    }

    rObj <- list( fltCnt=fltCnt )
	# rObj$dbg.* : pDebugIdx 가 NULL이라면 모두 0 값.
	rObj$dbg.colVal <- dbg.colVal   ;rObj$dbg.colVal.f <- dbg.colVal.f  ;rObj$dbg.colVal.c <- dbg.colVal.c
	rObj$dbg.valPtn <- dbg.valPtn   ;rObj$dbg.valPtn.f <- dbg.valPtn.f
	rObj$dgb.azWidth<- dgb.azWidth

    return( rObj )

} # stdFltCntByUA()

stdFltCntByUA.rObj.empty <- function( fltLen ){	# 텅 빈 rObj 얻기위해..

    rObj <- list( fltCnt=rep(0,fltLen) )
    if( !is.null(pDebugIdx) ){
        rObj$dbg.colVal <- 0   ;rObj$dbg.colVal.f <- 0  ;rObj$dbg.colVal.c <- 0
        rObj$dbg.valPtn <- 0   ;rObj$dgb.valPtn.f <- 0
        rObj$dgb.azWidth<- 0
    }
    return( rObj )

} # stdFltCntByUA.rObj.empty()


#   2,2,2(?) 연속인데 또 연속?
uaUtil.lastReb <- function( val ){

    val.len <- length(val)
    if( 2>val.len ){
        return( NULL )
    }
    if( val[val.len]==val[val.len-1] ){
        return( val[val.len] )
    }
    return(NULL)

} # uaUtil.decline1()

#   2,3,4,5(?) 순차증가/감소
uaUtil.decline1 <- function( val ){ # 1간격 증감

    val.len <- length(val)
    if( 2>val.len ){
        return( NULL )
    }

    fDiff <- val[val.len] - val[val.len-1]
    if( 1!=abs(fDiff) ){
        return( NULL )
    }

    banVal <- val[val.len] + fDiff[1]
    return(banVal)

} # uaUtil.decline1()
uaUtil.decline2 <- function( val ){ # 같은 간격으로 증/감 3개

    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }

    fDiff <- val[ val.len + 0:-1 ] - val[ val.len + -1:-2 ]
    if( fDiff[1]!=fDiff[2] ){
        return( NULL )
    }
    if( 0==fDiff[2] ){
        return( NULL )
    }

    banVal <- val[val.len] + fDiff[1]
    return(banVal)

} # uaUtil.decline2()

#   2,2,3,3(?) 연속의 재발생
uaUtil.rebAgain <- function( val ){
    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }
    if( val[val.len-1]!=val[val.len-2] ){
        return( NULL )
    }
    banVal <- val[val.len]
    return(banVal)
} # uaUtil.decline()

#   2,3,4,2,3,4(?) 동일 패턴 재발
#       비슷한 값들로 인해 rebPtn2,rebPtn3,rebPtn4 중첩발생 가능
uaUtil.rebPtn1 <- function( val ){
    val.len <- length(val)
    if( 3>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0]==val[val.len-2]) ){
        return(val[val.len-1])
    }
    return( NULL )
} # uaUtil.decline1()
uaUtil.rebPtn2 <- function( val ){
    val.len <- length(val)
    if( 5>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:1]==val[val.len-3:4]) ){
        return(val[val.len-2])
    }
    return( NULL )
} # uaUtil.decline2()
uaUtil.rebPtn3 <- function( val ){  
    val.len <- length(val)
    if( 7>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:2]==val[val.len-4:6]) ){
        return(val[val.len-3])
    }
    return( NULL )
} # uaUtil.decline3()
uaUtil.rebPtn4 <- function( val ){
    val.len <- length(val)
    if( 9>val.len ){
        return( NULL )
    }
    if( all(val[val.len-0:3]==val[val.len-5:8]) ){
        return(val[val.len-4])
    }
    return( NULL )
} # uaUtil.decline4()

#   1,2,3,2,1(?)
uaUtil.symm1 <- function( val ){
    val.len <- length(val)
    if( 3<=val.len ){ # 1,2,2,(1?)
        if( val[val.len]==val[val.len-1] ){
            return(val[val.len-2])
        }
    } else if( 4<=val.len ){ # 1,2,3,2,(1?)
        if( val[val.len]==val[val.len-2] ){
            return(val[val.len-3])
        }
    }
    return( NULL )
} # uaUtil.symm1()
uaUtil.symm2 <- function( val ){
    val.len <- length(val)
    if( 5<=val.len ){ # 1,2,3,3,2,(1?)
        if( all(val[val.len-0:1]==val[val.len-3:2]) ){
            return(val[val.len-4])
        }
    } else if( 6<=val.len ){ # 1,2,3,4,3,2(1?)
        if( all(val[val.len-0:1]==val[val.len-4:3]) ){
            return(val[val.len-5])
        }
    }
    return( NULL )
} # uaUtil.symm2()
uaUtil.symm3 <- function( val ){
    val.len <- length(val)
    if( 7<=val.len ){ # 1,2,3,4,4,3,2,(1?)
        if( all(val[val.len-0:2]==val[val.len-5:3]) ){
            return(val[val.len-6])
        }
    } else if( 8<=val.len ){ # 1,2,3,4,5,4,3,2,(1?)
        if( all(val[val.len-0:2]==val[val.len-6:4]) ){
            return(val[val.len-7])
        }
    }
    return( NULL )
} # uaUtil.symm3()

#   동일 패턴이 연속 발생.
uaUtil.seqReb <- function( pCodeMtx ,pECol=NULL ){
    #  A,e,C,e,e
    #  e,A,e,C,e
    #  e,e,A,e,C
    code.len <- nrow(pCodeMtx)
    col.len <- ncol(pCodeMtx)
    if( 1>=code.len ){
        return( list() )
    }

    # 차라리 날코딩이 가독성 좋을 듯....
    #             5,6 1,2
    #           4,5,6 1,2,3
    #         3,4,5,6 1,2,3,4
    #       2,3,4,5,6 1,2,3,4,5
    #     1,2,3,4,5,6 1,2,3,4,5,6
    #     1,2,3,4,5     2,3,4,5,6
    #     1,2,3,4         3,4,5,6
    #     1,2,3             4,5,6
    #     1,2                 5,6
    idxLst <- list()
    idxLst[[1+length(idxLst)]] <- c(        5,6)
    idxLst[[1+length(idxLst)]] <- c(      4,5,6)
    idxLst[[1+length(idxLst)]] <- c(    3,4,5,6)
    idxLst[[1+length(idxLst)]] <- c(  2,3,4,5,6)
    idxLst[[1+length(idxLst)]] <- c(1,2,3,4,5,6)
    idxLst[[1+length(idxLst)]] <- c(1,2,3,4,5  )
    idxLst[[1+length(idxLst)]] <- c(1,2,3,4    )
    idxLst[[1+length(idxLst)]] <- c(1,2,3      )
    idxLst[[1+length(idxLst)]] <- c(1,2        )

    fndLst <- list()
    for( cIdx in 0:(length(idxLst)-1) ){
        idxSpan.pre <- idxLst[[             1+cIdx]]
        idxSpan.next<- idxLst[[length(idxLst)-cIdx]]
        flag <- pCodeMtx[code.len  ,idxSpan.next]==pCodeMtx[code.len-1,idxSpan.pre ]
        if( !is.null(pECol) ){
            naFlag <- (idxSpan.pre %in% pECol) | (idxSpan.next %in% pECol)
            flag[naFlag] <- FALSE
        }
        if( 1<sum(flag) ){
            val <- pCodeMtx[code.len ,idxSpan.next]
            val[!flag] <- NA
            flagIdx <- which(flag)
            val <- val[ flagIdx[1]:flagIdx[length(flagIdx)] ]
            fndLst[[1+length(fndLst)]] <- val
        }
    }

    # 개선 가능성 : fndLst 내에서 서로 포함관계가 되는 대상은 제외시킬 수 있다.

    return( fndLst )
} # uaUtil.seqReb()

assInterUAnaGrp <- function( gEnv ,allIdxF ,uAnaLstGrp ,pDebugIdx=NULL ){
    # getUAnaLstGrp() 변경과 연동 필요.

    fltCntSum <- rep( 0 ,length(allIdxF) )
    dbg.fltCnt <- list()

    anaLst <- uAnaLstGrp$uAnaLst.rawData
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.rawData"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.rebCnt
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.rebCnt"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.colVal1
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.colVal1"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.colVal3
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.colVal3"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.colVal4
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.colVal4"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.colVal6
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.colVal6"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.nextZW
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.nextZW"]] <- dgbObj

    anaLst <- uAnaLstGrp$uAnaLst.zw
    fltCnt <- 0
    for( uaIdx in seq_len(length(anaLst)) ){
        fltCntObj <- anaLst[[uaIdx]]$getFltCnt( gEnv ,allIdxF ,pDebugIdx )
        fltCnt <- fltCnt + fltCntObj$fltCnt
    }
    fltCntSum <- fltCntSum + fltCnt
    dgbObj <- list( fltCnt=fltCnt )
    dbg.fltCnt[["uAnaLst.zw"]] <- dgbObj

    assRstObj <- list( fltCntSum=fltCntSum )
    if( !is.null(pDebugIdx) ){
        assRstObj$dbg.fltCnt <- dbg.fltCnt
    }

    return( assRstObj )

} # assInterUAnaGrp( )

getUAnaLstGrp <- function( gEnv ,allIdxF ,pDefaultCut=TRUE ,pReport=TRUE ){

	# ==============================================================================
	# rawData
    uAnaLst.rawData <- getUAnaLst.rawData( gEnv ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.rawData)) ){
            uAna <- uAnaLst.rawData[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

	# ==============================================================================
	# rebCnt
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
	rebCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){ sum(aZoid%in%lastZoid) })
	uAnaLst.rebCnt <- getUAnaLst.rebCnt( gEnv ,rebVal=sort(unique(rebCnt)) ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.rebCnt)) ){
            uAna <- uAnaLst.rebCnt[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

	# ==============================================================================
	# colVal
	colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,1]))
    uAnaLst.colVal1 <- getUAnaLst.colVal( gEnv ,col=1 ,colVal=colVal ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.colVal1)) ){
            uAna <- uAnaLst.colVal1[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

    # col 2와 col 5는 default cut 작업에서 제외시키자. 
    # 각각 col1과 col5에서의 값 범위가 너무 한정되기 때문.
    # 나중에 수동 검토에서 반영할 것
	#   colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,2]))
	#   uAnaLst.colVal2 <- getUAnaLst.colVal( gEnv ,col=2 ,colVal=colVal )

	colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,3]))
	uAnaLst.colVal3 <- getUAnaLst.colVal( gEnv ,col=3 ,colVal=colVal ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.colVal3)) ){
            uAna <- uAnaLst.colVal3[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

	colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,4]))
	uAnaLst.colVal4 <- getUAnaLst.colVal( gEnv ,col=4 ,colVal=colVal ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.colVal4)) ){
            uAna <- uAnaLst.colVal4[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

    # col 2와 col 5는 default cut 작업에서 제외시키자. 
    # 각각 col1과 col5에서의 값 범위가 너무 한정되기 때문.
    # 나중에 수동 검토에서 반영할 것
    #    colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,5]))
    #    uAnaLst.colVal5 <- getUAnaLst.colVal( gEnv ,col=5 ,colVal=colVal )

	colVal <- sort(unique(gEnv$allZoidMtx[allIdxF,6]))
	uAnaLst.colVal6 <- getUAnaLst.colVal( gEnv ,col=6 ,colVal=colVal ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.colVal6)) ){
            uAna <- uAnaLst.colVal6[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

	# ==============================================================================
	# nextZW : next zoid width
    uAnaLst.nextZW <- getUAnaLst.nextZW( gEnv ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.nextZW)) ){
            uAna <- uAnaLst.nextZW[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

	# ==============================================================================
	# zw : next zoid width
	zwVal <- sort(unique(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]))
	uAnaLst.zw <- getUAnaLst.zw( gEnv ,zwVal=zwVal ,pReport=pReport )
    if( pDefaultCut ){
        tStmp <- Sys.time()
        for( uaIdx in seq_len(length(uAnaLst.zw)) ){
            uAna <- uAnaLst.zw[[uaIdx]]
            allIdxF.cut <- uAna$defaultCut( gEnv ,allIdxF )
            allIdxF <- allIdxF.cut	# 디버깅 정보를 위해 allIdxF.cut을 별도로 거쳤다.
        }
        cat(sprintf("allIdxF %d\n",length(allIdxF)))
        tDiff <- Sys.time() - tStmp
    }

    uAnaLstGrp <- list( )
    uAnaLstGrp$uAnaLst.rawData <- uAnaLst.rawData
    uAnaLstGrp$uAnaLst.rebCnt <- uAnaLst.rebCnt
    uAnaLstGrp$uAnaLst.colVal1 <- uAnaLst.colVal1
    uAnaLstGrp$uAnaLst.colVal3 <- uAnaLst.colVal3
    uAnaLstGrp$uAnaLst.colVal4 <- uAnaLst.colVal4
    uAnaLstGrp$uAnaLst.colVal6 <- uAnaLst.colVal6
    uAnaLstGrp$uAnaLst.nextZW <- uAnaLst.nextZW
    uAnaLstGrp$uAnaLst.zw <- uAnaLst.zw
    if( pDefaultCut ){
        uAnaLstGrp$allIdxF <- allIdxF
    }

    return( uAnaLstGrp )

} # getUAnaLstGrp()

getUAnaLst.rawData <- function( gEnv ,pReport=TRUE ){
    uAnaLst <- list()
    uAnaLst[[1+length(uAnaLst)]] <- uAna.rawData( gEnv )
    if( pReport ){
        uAnaLst[[1]]$report()
    }
    return( uAnaLst )
} # getUAnaLst.rawData()

uAna.rawData <- function( gEnv ,cutThld=2 ){ # uAnaLst.rawData() 필요할때가 있을까?

    ruObj <- list( idStr=sprintf("rawData") )
    ruObj$cutThld <- cutThld
    ruObj$getWorkHMtx <- function( gEnv ){
        return( gEnv$zhF )
    } # ruObj$workHMtx()

    zoidMtx <- ruObj$getWorkHMtx( gEnv )
    ruObj$uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=NULL )
    ruObj$uAnaCutData <- getStdCutData_UnitAnaObj( ruObj$uAnaObj )

    ruObj$report <- function( ){
        rptFileName <- sprintf("./report/rptUnitAnalyze.%s" ,ruObj$idStr)
        title <- sprintf("%s (cutThld:%d)" ,ruObj$idStr ,ruObj$cutThld )
        rptUnitAnalyze( ruObj$uAnaObj ,pTitle=title ,pRptFile=rptFileName )
    } # ruObj$report()
    ruObj$getFltCnt <- function( gEnv ,allIdxF ,pDebugIdx=NULL ){ # filt에 걸린 갯수 측정.
        awZoidMtx <- gEnv$allZoidMtx[allIdxF ,,drop=F ]
        fltCutObj <- stdFltCntByUA( gEnv ,ruObj$uAnaCutData ,awZoidMtx ,pDebugIdx )
        return( fltCutObj )
    } # ruObj$getFltCnt()
    ruObj$defaultCut <- function( gEnv ,allIdxF ){
        fltCnt <- ruObj$getFltCnt( gEnv ,allIdxF )$fltCnt
        return( allIdxF[fltCnt<ruObj$cutThld] )
    } # ruObj$defaultCut()

    return( ruObj )

} # uAnaLst.rawData()


getUAnaLst.nextZW <- function( gEnv ,pReport=TRUE ){
    uAnaLst <- list()
    uAnaLst[[1+length(uAnaLst)]] <- uAna.nextZW( gEnv )
    if( pReport ){
        uAnaLst[[1]]$report()
    }
    return( uAnaLst )
} # getUAnaLst.nextZW()

uAna.nextZW <- function( gEnv ,cutThld=2 ){ # uAnaLst.nextZW() 필요할때가 있을까?

    ruObj <- list( idStr=sprintf("nextZW") )
    ruObj$cutThld <- cutThld
    ruObj$getWorkHMtx <- function( gEnv ){
        zWidth <- gEnv$zhF[,6]-gEnv$zhF[,1]
        hIdx <- which( zWidth[1:(length(zWidth)-1)]==zWidth[length(zWidth)] )
        return( gEnv$zhF[hIdx+1,,drop=F] )
    } # ruObj$workHMtx()

    zoidMtx <- ruObj$getWorkHMtx( gEnv )
    ruObj$uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=NULL )
    ruObj$uAnaCutData <- getStdCutData_UnitAnaObj( ruObj$uAnaObj )

    ruObj$report <- function( ){
        rptFileName <- sprintf("./report/rptUnitAnalyze.%s" ,ruObj$idStr)
        title <- sprintf("%s (cutThld:%d)" ,ruObj$idStr ,ruObj$cutThld )
        rptUnitAnalyze( ruObj$uAnaObj ,pTitle=title ,pRptFile=rptFileName )
    } # ruObj$report()
    ruObj$getFltCnt <- function( gEnv ,allIdxF ,pDebugIdx=NULL ){ # filt에 걸린 갯수 측정.

        awZoidMtx <- gEnv$allZoidMtx[allIdxF ,,drop=F ]
        fltCutObj <- stdFltCntByUA( gEnv ,ruObj$uAnaCutData ,awZoidMtx ,pDebugIdx )
        return( fltCutObj )
    } # ruObj$getFltCnt()
    ruObj$defaultCut <- function( gEnv ,allIdxF ){
        fltCnt <- ruObj$getFltCnt( gEnv ,allIdxF )$fltCnt
        return( allIdxF[fltCnt<ruObj$cutThld] )
    } # ruObj$defaultCut()

    return( ruObj )

} # uAnaLst.nextZW()


getUAnaLst.rebCnt <- function( gEnv ,rebVal=NULL ,pReport=TRUE ){

    rebVal.h <- sort(unique( sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[hIdx,]%in%gEnv$zhF[(hIdx-1),]) }) ))
    if( is.null(rebVal) ){
        rebVal <- rebVal.h
    } else {
        rebVal <- intersect( rebVal ,rebVal.h )
    }

    uAnaLst <- list()
    for( rebIdx in rebVal ){
        uAna <- getUAna.rebCnt( gEnv ,rebIdx )
        if( pReport ){
            uAna$report()
        }

        uAnaLst[[1+length(uAnaLst)]] <- uAna
    }

    return( uAnaLst )

} # getUAna.rebCnt()

getUAna.rebCnt <- function( gEnv ,rebIdx ,cutThld=2 ){

    ruObj <- list( idStr=sprintf("rebCnt%d",rebIdx) )
    ruObj$rebIdx <- rebIdx
    ruObj$cutThld <- cutThld
	ruObj$lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    ruObj$getWorkHMtx <- function( gEnv ){
        hSize <- nrow(gEnv$zhF)
        rebCnt <- rep( 0 ,hSize )
        rebCnt[2:hSize] <- sapply( 2:hSize ,function(hIdx){ sum(gEnv$zhF[hIdx,]%in%gEnv$zhF[(hIdx-1),]) })
        return( gEnv$zhF[rebCnt==ruObj$rebIdx,,drop=F] )
    } # ruObj$workHMtx()

    zoidMtx <- ruObj$getWorkHMtx( gEnv )
    ruObj$uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=NULL )
    ruObj$uAnaCutData <- getStdCutData_UnitAnaObj( ruObj$uAnaObj )

    ruObj$report <- function( ){
        rptFileName <- sprintf("./report/rptUnitAnalyze.%s" ,ruObj$idStr)
        title <- sprintf("%s (cutThld:%d)" ,ruObj$idStr ,ruObj$cutThld )
        rptUnitAnalyze( ruObj$uAnaObj ,pTitle=title ,pRptFile=rptFileName )
    } # ruObj$report()
    ruObj$getFltCnt <- function( gEnv ,allIdxF ,pDebugIdx=NULL ){ # filt에 걸린 갯수 측정.
        rebCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function(aZoid){
                        return( sum(aZoid %in% ruObj$lastZoid ) )
                    })
        awIdx <- which( rebCnt==ruObj$rebIdx )
        if( 0==length(awIdx) ){
			return( stdFltCntByUA.rObj.empty(length(allIdxF)) )
        }
		awIdx.debug <- NULL
		if( !is.null(pDebugIdx) ){
			rDebugIdx <- which( awIdx==pDebugIdx )
			if( 0<length(rDebugIdx) ){
				awIdx.debug <- rDebugIdx
			}
		}
        awZoidMtx <- gEnv$allZoidMtx[allIdxF[awIdx] ,,drop=F ]
		fltCntObj <- stdFltCntByUA( gEnv ,ruObj$uAnaCutData ,awZoidMtx ,pDebugIdx=awIdx.debug )

		fltCnt <- rep( 0 ,length(allIdxF) )
        fltCnt[awIdx] <- fltCntObj$fltCnt
		fltCntObj$fltCnt <- fltCnt	# 조건에 상관없이 모든 allIdxF 에 대응하는 fltCnt이어야 하므로.
        return( fltCntObj )
    } # ruObj$getFltCnt()
    ruObj$defaultCut <- function( gEnv ,allIdxF ){ # filt에 걸린 갯수 측정.
        fltCnt <- ruObj$getFltCnt( gEnv ,allIdxF )$fltCnt
        return( allIdxF[fltCnt<ruObj$cutThld] )
    } # ruObj$defaultCut()

    return( ruObj )

} # getUAna.rebCnt()


getUAnaLst.colVal <- function( gEnv ,col ,colVal=NULL ,pReport=TRUE ){

    if( is.null(colVal) ){
        colVal <- sort( unique(gEnv$zhF[,col]) )
    } else {
        colVal <- intersect( colVal ,sort(unique(gEnv$zhF[,col])) )
    }

    uAnaLst <- list()
    for( cvIdx in colVal ){
        uAna <- getUAna.colVal( gEnv ,col ,colVal=cvIdx )
        if( pReport ){
            uAna$report()
        }

        uAnaLst[[1+length(uAnaLst)]] <- uAna
    }

    return( uAnaLst )

} # getUAnaLst.colVal()

getUAna.colVal <- function( gEnv ,col ,colVal ,cutThld=2 ){

    ruObj <- list( idStr=sprintf("colVal_%d.%d",col,colVal) )
    ruObj$col <- col
    ruObj$colVal <- colVal
    ruObj$cutThld <- cutThld
    ruObj$getWorkHMtx <- function( gEnv ){
        hSize <- nrow(gEnv$zhF)
        return( gEnv$zhF[gEnv$zhF[,col]==ruObj$colVal,,drop=F] )
    } # ruObj$workHMtx()

    zoidMtx <- ruObj$getWorkHMtx( gEnv )
    ruObj$uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=ruObj$col )
    ruObj$uAnaCutData <- getStdCutData_UnitAnaObj( ruObj$uAnaObj )

    ruObj$report <- function( ){
        rptFileName <- sprintf("./report/rptUnitAnalyze.%s" ,ruObj$idStr)
        title <- sprintf("%s (cutThld:%d)" ,ruObj$idStr ,ruObj$cutThld )
        rptUnitAnalyze( ruObj$uAnaObj ,pTitle=title ,pRptFile=rptFileName )
    } # ruObj$report()
    ruObj$getFltCnt <- function( gEnv ,allIdxF ,pDebugIdx=NULL ){ # filt에 걸린 갯수 측정.

		awIdx <- which( gEnv$allZoidMtx[allIdxF,ruObj$col]==ruObj$colVal )
        if( 0==length(awIdx) ){
            return( stdFltCntByUA.rObj.empty(length(allIdxF)) )
        }
		awIdx.debug <- NULL
		if( !is.null(pDebugIdx) ){
			rDebugIdx <- which( awIdx==pDebugIdx )
			if( 0<length(rDebugIdx) ){
				awIdx.debug <- rDebugIdx
			}
		}

        awZoidMtx <- gEnv$allZoidMtx[allIdxF[awIdx] ,,drop=F ]
		fltCntObj <- stdFltCntByUA( gEnv ,ruObj$uAnaCutData ,awZoidMtx ,pDebugIdx=awIdx.debug )
		fltCnt <- rep( 0 ,length(allIdxF) )
        fltCnt[awIdx] <- fltCntObj$fltCnt
		fltCntObj$fltCnt <- fltCnt	# 조건에 상관없이 모든 allIdxF 에 대응하는 fltCnt이어야 하므로.
        return( fltCntObj )

    } # ruObj$getFltCnt()
    ruObj$defaultCut <- function( gEnv ,allIdxF ){ # filt에 걸린 갯수 측정.
        fltCnt <- ruObj$getFltCnt( gEnv ,allIdxF )$fltCnt
        return( allIdxF[fltCnt<ruObj$cutThld] )
    } # ruObj$defaultCut()

    return( ruObj )

} # getUAna.colVal()


getUAnaLst.zw <- function( gEnv ,zwVal=NULL ,pReport=TRUE ){

    if( is.null(zwVal) ){
        zwVal <- sort( unique(gEnv$zhF[,6]-gEnv$zhF[,1]) )
    } else {
        zwVal <- intersect( zwVal ,sort(unique(gEnv$zhF[,6]-gEnv$zhF[,1])) )
    }

    uAnaLst <- list()
    for( zwVIdx in zwVal ){
        uAna <- getUAna.zw( gEnv ,zwVal=zwVIdx )
        if( pReport ){
            uAna$report()
        }

        uAnaLst[[1+length(uAnaLst)]] <- uAna
    }

    return( uAnaLst )

} # getUAnaLst.zw()

getUAna.zw <- function( gEnv ,zwVal ,cutThld=2 ){

    ruObj <- list( idStr=sprintf("zw_%d",zwVal) )
    ruObj$zwVal <- zwVal
    ruObj$cutThld <- cutThld
    ruObj$getWorkHMtx <- function( gEnv ){
        zw <- (gEnv$zhF[,6]-gEnv$zhF[,1])
        return( gEnv$zhF[zw==ruObj$zwVal,,drop=F] )
    } # ruObj$workHMtx()

    zoidMtx <- ruObj$getWorkHMtx( gEnv )
    ruObj$uAnaObj <- getUnitAnalyzer( zoidMtx ,pECol=c(1,6) ,pWidth=FALSE )
        # zoid width 는 양 끝이 비슷한 값들의 연속인 경우가 많은 듯 하다.
    ruObj$uAnaCutData <- getStdCutData_UnitAnaObj( ruObj$uAnaObj )

    ruObj$report <- function( ){
        rptFileName <- sprintf("./report/rptUnitAnalyze.%s" ,ruObj$idStr)
        title <- sprintf("%s (cutThld:%d)" ,ruObj$idStr ,ruObj$cutThld )
        rptUnitAnalyze( ruObj$uAnaObj ,pTitle=title ,pRptFile=rptFileName )
    } # ruObj$report()
    ruObj$getFltCnt <- function( gEnv ,allIdxF ,pDebugIdx=NULL ){ # filt에 걸린 갯수 측정.

        awIdx <- which( ruObj$zwVal==(gEnv$allZoidMtx[allIdxF,6]-gEnv$allZoidMtx[allIdxF,1]) )
        if( 0==length(awIdx) ){
            return( stdFltCntByUA.rObj.empty(length(allIdxF)) )
        }
		awIdx.debug <- NULL
		if( !is.null(pDebugIdx) ){
			rDebugIdx <- which( awIdx==pDebugIdx )
			if( 0<length(rDebugIdx) ){
				awIdx.debug <- rDebugIdx
			}
		}

        awZoidMtx <- gEnv$allZoidMtx[allIdxF[awIdx] ,,drop=F ]
		fltCntObj <- stdFltCntByUA( gEnv ,ruObj$uAnaCutData ,awZoidMtx ,pDebugIdx=awIdx.debug )
		fltCnt <- rep( 0 ,length(allIdxF) )
        fltCnt[awIdx] <- fltCntObj$fltCnt
		fltCntObj$fltCnt <- fltCnt	# 조건에 상관없이 모든 allIdxF 에 대응하는 fltCnt이어야 하므로.
        return( fltCntObj )
    } # ruObj$getFltCnt()
    ruObj$defaultCut <- function( gEnv ,allIdxF ){ # filt에 걸린 갯수 측정.
        fltCnt <- ruObj$getFltCnt( gEnv ,allIdxF )$fltCnt
        return( allIdxF[fltCnt<ruObj$cutThld] )
    } # ruObj$defaultCut()

    return( ruObj )

} # getUAna.zw()


valTest <- function( gEnv ){

    tStmp <- Sys.time()
    testSpan <- 300:nrow(gEnv$zhF)
    fltSum <- rep( 0 ,nrow(gEnv$zhF) )
    dbgLst <- list()
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        tEnv$allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        uAnaLstGrp <- getUAnaLstGrp( tEnv ,1 ,pDefaultCut=FALSE ,pReport=F )
        assRstObj <- assInterUAnaGrp( tEnv ,1 ,uAnaLstGrp ,pDebugIdx=1 )
        fltSum[tIdx] <- assRstObj$fltCntSum
        dbgLst[[1+length(dbgLst)]] <- assRstObj$dbg.fltCnt
    }
    tDiff <- Sys.time() - tStmp

    fltSum.testSpan <- fltSum[testSpan]
    table( fltSum.testSpan )

    cNames <- attributes(dbgLst[[1]])$names
    fltSumMtx <- matrix( 0 ,nrow=length(dbgLst) ,ncol=length(cNames) ) ;colnames(fltSumMtx) <- cNames
    for( rIdx in seq_len(length(dbgLst)) ){
        for( cIdx in cNames ){
            fltSumMtx[rIdx,cIdx] <- dbgLst[[rIdx]][[cIdx]]$fltCnt
        }
    }
    apply( fltSumMtx ,2 ,table )

	saveObj <- list( fltSum=fltSum ,dbgLst=dbgLst ,allIdxF=allIdxF )
	save( saveObj ,file="Obj_saveObj.save" )

} # valTest()

