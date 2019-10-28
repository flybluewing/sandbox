#	scoreMtx 들에 대한 메타 scoreMtx 생성.
#		hIdx 별 scoreMtxN * phase 를 생성한다.

bFMM.getMetaScore.grp <- function( scoreMtx.grp ,hMtxLst ){

    datLen <- bFMM.getSize.scoreMtx.grp( scoreMtx.grp )["max"]

    mtxName.basic <- names(scoreMtx.grp$basic$basic)
    mtxName.mf <- names(scoreMtx.grp$mf)
    mtxName.bDup <- names(scoreMtx.grp$bDup)

    mtxGrp.basic <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
        # mtxGrp.basic[[mName]][[aIdx]]
    hIdxObj <- B.getHMtxLst_byHIdx( hMtxLst )
        # hIdxObj[[hName]][[mName]][["aIdx"]]

    metaScoreMtx.grp <- list()
    for( aIdx in 1:datLen ){
        metaScore <- list()
        for( hName in names(hMtxLst$sfcHLst) ){
            # <basic> ----------------------------------------
            basicLst <- list
            for( mName in mtxName.basic ){
                basicLst[[mName]] <- bFMM.getMetaScore.basic( mName 
                                                ,mtxGrp.basic[[mName]][[aIdx]] 
                                                ,hIdxObj[[hName]][[mName]]
                                            )
            }

            # <mf> -------------------------------------------
            mfLst <- list()
            for( mName in mtxName.mf ){
                mfLst[[mName]] <- bFMM.getMetaScore.mf( mName 
                                                ,scoreMtx.grp$mf[[mName]]$scoreMtx[aIdx,]
                                                ,hMtxLst$mfMtxLst[[hName]][[mName]]$scoreMtx
                                            )
            }

            # <bDup> -----------------------------------------
            bDupLst <- list()
            for( mName in mtxName.bDup ){
                bDupLst[[mName]] <- bFMM.getMetaScore.bDup( mName 
                                                ,scoreMtx.grp$bDup[[mName]]$scoreMtx[aIdx,]
                                                ,hMtxLst$bDupMtxLst[[hName]][[mName]]$scoreMtx
                                            )
            }

            metaScore[[hName]] <- list( basic=basicLst ,mfLst=mf ,bDup=bDupLst )
        }
        metaScoreMtx.grp[[as.character(aIdx)]] <- metaScore
    }

    return( metaScoreMtx.grp )

} # bFMM.getMetaScore.grp()


bFMM.getMetaScore.basic <- function( mName ,scoreMtx ,pastMtxLst ){
    #   pastMtxLst 는 가장 최근 것이 가장 처음 나오게 된다.
    # evtCnt ,hpnCnt

    phaseName <- colnames(scoreMtx)
    cName <- c("evtCnt","hpnCnt","rebEvtCnt","rebHpnCnt")
    rMtx <- matrix( 0 ,nrow=length(phaseName) ,ncol=length(cName) )
    rownames(rMtx) <- phaseName     ;colnames(rMtx) <- cName

    lastMtx <- pastMtxLst[[ length(pastMtxLst) ]]

    for( pName in phaseName ){
        rMtx[pName,"hpnCnt"] <- sum(scoreMtx[,pName]>0)
        
        evt <- bUtil.getEvtVal( scoreMtx[,pName] ,scoreEvtLst[[mName]] )    # FCust_H.R
        rMtx[pName,"evtCnt"] <- sum( !is.na(evt) )

        matFlag <- lastMtx[,pName] == scoreMtx[,pName]
        matFlag[lastMtx[,pName]==0] <- F
        rMtx[pName,"rebHpnCnt"] <- sum(matFlag)

        evt.last <- bUtil.getEvtVal( lastMtx[,pName] ,scoreEvtLst[[mName]] )    # FCust_H.R        
        rMtx[pName,"rebEvtCnt"] <- sum( evt.last==evt ,na.rm=T )
    }

    return( rMtx )

} # bFMM.getMetaScore.basic()

bFMM.getMetaScore.mf <- function( mName ,score ,pastScoreMtx ){

    lastScore <- pastScoreMtx[nrow(pastScoreMtx) ,]

    rVal <- c(evtCnt=0, hpnCnt=0, rebEvtCnt=0, rebHpnCnt=0 )
    rVal["hpnCnt"] <- sum( score>0 )

    evt <- bUtil.getEvtVal( score ,bScrEvtLst[[mName]] )
    rVal["evtCnt"] <- sum( !is.na(evt) )

    matFlag <- score==lastScore
    matFlag[ score==0 ] <- FALSE
    rVal["rebHpnCnt"] <- sum( matFlag )

    evt.last <- bUtil.getEvtVal( lastScore ,bScrEvtLst[[mName]] )
    rVal["rebEvtCnt"] <- sum( evt.last==evt ,na.rm=T )

    return( rVal )

} # bFMM.getMetaScore.mf()

bFMM.getMetaScore.bDup <- function(){
    lastScore <- pastScoreMtx[nrow(pastScoreMtx) ,]

    rVal <- c(evtCnt=0, hpnCnt=0, rebEvtCnt=0, rebHpnCnt=0 )

    rVal <- NULL    #   공사중...
    return( rVal )
}

bFMM.getSize.scoreMtx.grp <- function( scoreMtx.grp ){
    # scoreMtx를 선택적으로 처리하다보니 데이터 크기를 확인하기 복잡해져서..
    datLen <- c(basic=0,mf=0,bDup=0)

    for( pName in names(scoreMtx.grp$basic) ){
        mNames <- names(scoreMtx.grp$basic[[pName]])
        if( 0<length(mNames) ){
            datLen["basic"] <- nrow(scoreMtx.grp$basic[[pName]][[mNames[1]]]$scoreMtx)
        }
    }

    mNames.mf <- names(scoreMtx.grp$mf)
    if( 0<length(mNames.mf) ){
        datLen["mf"] <- nrow(scoreMtx.grp$mf[[ mNames.mf[1] ]]$scoreMtx)
    }

    mNames.bDup <- names(scoreMtx.grp$bDup)
    if( 0<length(mNames.bDup) ){
        datLen["bDup"] <- nrow(scoreMtx.grp$bDup[[ mNames.bDup[1] ]]$scoreMtx)
    }

    return( c(max=max(datLen) ,datLen) )
}



