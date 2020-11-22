
bSMtxExtFltLst <- list()

mName <- "sScore01"
if( TRUE ){
    bSMtxExtFltLst[[mName]] <- list()
        # rem0.num rem0.len.tot rem0.len.val 
        # rem1.num rem1.len.tot rem1.len.val 
        # c0.num c0.len.tot c0.len.val 
        # c1.num c1.len.tot c1.len.val 
        # f0.num f0.len.tot f0.len.val 
        # f1.num f1.len.tot f1.len.val
    fltCreater <- function( mName ) {    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("remN.num","remN.len.tot","remN.len.val" ,"cN.num","cN.len.tot","cN.len.val" ,"fN.num","fN.len.tot","fN.len.val")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            rVal["remN.num"]    <- sum(score[c("rem0.num","rem1.num")])
            rVal["remN.len.tot"]<- sum(score[c("rem0.len.tot","rem1.len.tot")])
            rVal["remN.len.val"]<- sum(score[c("rem0.len.val","rem1.len.val")])
            rVal["cN.num"]      <- sum(score[c("c0.num","c1.num")])
            rVal["cN.len.tot"]  <- sum(score[c("c0.len.tot","c1.len.tot")])
            rVal["cN.len.val"]  <- sum(score[c("c0.len.val","c1.len.val")])
            rVal["fN.num"]      <- sum(score[c("f0.num","f1.num")])
            rVal["fN.len.tot"]  <- sum(score[c("f0.len.tot","f1.len.tot")])
            rVal["fN.len.val"]  <- sum(score[c("f0.len.val","f1.len.val")])

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
    bSMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)    #   bSMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )

    fltCreater <- function( mName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("evt0.num","evt0.len.tot","evt0.len.val","evt1.num","evt1.len.tot","evt1.len.val")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,scoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["evt0.num"]    <- sum(scrEvt[c("rem0.num","c0.num","f0.num")] ,na.rm=T)
            rVal["evt0.len.tot"]<- sum(scrEvt[c("rem0.len.tot","c0.len.tot","f0.len.tot")] ,na.rm=T)
            rVal["evt0.len.val"]<- sum(scrEvt[c("rem0.len.val","c0.len.val","f0.len.val")] ,na.rm=T)
            rVal["evt1.num"]    <- sum(scrEvt[c("rem1.num","c1.num","f1.num")] ,na.rm=T)
            rVal["evt1.len.tot"]<- sum(scrEvt[c("rem1.len.tot","c1.len.tot","f1.len.tot")] ,na.rm=T)
            rVal["evt1.len.val"]<- sum(scrEvt[c("rem1.len.val","c1.len.val","f1.len.val")] ,na.rm=T)

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
    bSMtxExtFltLst[[mName]]$filter02 <- fltCreater(mName)    #   bSMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )


}













mName <- "sScore09"
if( TRUE ){ # "sScore09"

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
    bSMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)

}


