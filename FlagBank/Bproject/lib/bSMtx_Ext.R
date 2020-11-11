
bSMtxExtFltLst <- list()

mName <- "score9"
if( FALSE ){ # "sScore09"

    bSMtxExtFltLst[[mName]] <- list()
        # rCnt rD2 rDn rLr rRl 
        # eCnt eD2 eDn eLr eRl 
        # cCnt cD2 cDn cLr cRl 
        # fCnt fD2 fDn fLr fRl

    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("xCntECnt" 
                                ,"xD2ECnt" ,"xDnECnt","xLrECnt","xRlECnt" 
                                ,"r2EvtCnt" ,"e2EvtCnt" ,"c2EvtCnt" ,"f2EvtCnt" 
        )

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["xCntECnt"]<- sum( score[c("rCnt","eCnt","cCnt","fCnt")]>=c(2,3,2,2) )
            rVal["xD2ECnt"] <- sum( score[c("rD2","eD2","cD2","fD2")]>=c(2,2,2,2) )
            rVal["xDnECnt"] <- sum( score[c("rDn","eDn","cDn","fDn")]>=c(2,2,2,2) )
            rVal["xLrECnt"] <- sum( score[c("rLr","eLr","cLr","fLr")]>=c(2,2,2,2) )
            rVal["xRlECnt"] <- sum( score[c("rRl","eRl","cRl","fRl")]>=c(2,2,2,2) )

            rVal["r2EvtCnt"]<- sum( score[c("rD2","rDn","rLr","rRl")] >= c(2,2,2,2) )
            rVal["e2EvtCnt"]<- sum( score[c("eD2","eDn","eLr","eRl")] >= c(2,2,2,2) )
            rVal["c2EvtCnt"]<- sum( score[c("cD2","cDn","cLr","cRl")] >= c(2,2,2,2) )
            rVal["f2EvtCnt"]<- sum( score[c("fD2","fDn","fLr","fRl")] >= c(2,2,2,2) )

            # rVal["xxx"]  <- sum(score[c("xxx","xxx","xxx")] ,na.rm=T)
            # rVal["xxx"]  <- sum(scrEvt[c("xxx","xxx")] ,na.rm=T)

            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }
            rownames(rMtx) <- rownames(scoreMtx)

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)

}


