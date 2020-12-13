
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
        fltObj$mInfo$cName <- c("rem0.len.tot","rem1.len.tot","c0.len.tot","c1.len.tot","f0.len.tot","f1.len.tot")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            rVal["rem0.len.tot"]<- score[c("rem0.len.tot")]
            rVal["rem1.len.tot"]<- score[c("rem1.len.tot")]
            rVal["c0.len.tot"]<- score["c0.len.tot"]
            rVal["c1.len.tot"]<- score["c1.len.tot"]
            rVal["f0.len.tot"]<- score["f0.len.tot"]
            rVal["f1.len.tot"]<- score["f1.len.tot"]

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
    bSMtxExtFltLst[[mName]]$filter02 <- fltCreater(mName)    #   bSMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )

    fltCreater <- function( mName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("evt0.num","evt0.len.tot","evt0.len.val","evt1.num","evt1.len.tot","evt1.len.val")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

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
    bSMtxExtFltLst[[mName]]$filter03 <- fltCreater(mName)    #   bSMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )

} else if( FALSE ){
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

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

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

mName <- "sScore02"
if( TRUE ){
    bSMtxExtFltLst[[mName]] <- list()
        # rebC.r  rebC.c  rebC.f 
        # rebC2.r rebC2.c rebC2.f   
        # inc.r   inc.c   inc.f  
        # inc.r2  inc.c2  inc.f2

    fltCreater <- function( mName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("rebCN.r","rebCN.c","rebCN.f","incN.r","incN.c","incN.f"
                                    ,"matCntRebC12","matCntInc12","matCntInc123"
                                )

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            rVal["rebCN.r"] <- sum(score[c("rebC.r","rebC2.r")])
            rVal["rebCN.c"] <- sum(score[c("rebC.c","rebC2.c")])
            rVal["rebCN.f"] <- sum(score[c("rebC.f","rebC2.f")])
            rVal["incN.r"]  <- sum(score[c("inc.r","inc.r2")])
            rVal["incN.c"]  <- sum(score[c("inc.c","inc.c2")])
            rVal["incN.f"]  <- sum(score[c("inc.f","inc.f2")])

            if( all(score[c("rebC.r","rebC.c","rebC.f")]==score[c("rebC2.r","rebC2.c","rebC2.f")]) ){
                #   그냥 3개 컬럼 매치 여부로만 하면 빈도가 높을 것 같아, 매치되는 총 갯수로 한다.
                #   물론 모두 0 이라면 matCnt 값도 0
                rVal["matCntRebC12"]  <- sum(score[c("rebC.r","rebC.c","rebC.f")])
            }
            if( all(score[c("inc.r","inc.c","inc.f")]==score[c("inc.r2","inc.c2","inc.f2")]) ){
                #   그냥 3개 컬럼 매치 여부로만 하면 빈도가 높을 것 같아, 매치되는 총 갯수로 한다.
                rVal["matCntInc12"]  <- sum(score[c("inc.r","inc.c","inc.f")])
            }

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

    fltCreater <- function( mName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("evtRebLR","evtRebC","evtRebC2","evtInc","evtInc2")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["evtRebC"]     <- sum(scrEvt[c("rebC.r","rebC.c","rebC.f")] ,na.rm=T)
            rVal["evtRebC2"]    <- sum(scrEvt[c("rebC2.r","rebC2.c","rebC2.f")] ,na.rm=T)
            rVal["evtInc"]      <- sum(scrEvt[c("inc.r","inc.c","inc.f")] ,na.rm=T)
            rVal["evtInc2"]     <- sum(scrEvt[c("inc.r2","inc.c2","inc.f2")] ,na.rm=T)

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
    bSMtxExtFltLst[[mName]]$filter02 <- fltCreater(mName)

}

mName <- "sScore03"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # rebPtn.1    rebPtn.n 
        # snR3 
        # snMax.r     snFCnt.r
        # snMax.c     snFCnt.c
        # snMax.f     snFCnt.f

    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("rebPtnESum" ,"snR3E" ,"snMaxESum" ,"snFCntESum","snRESum","snCESum","snFESum")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["rebPtnESum"]  <- sum(scrEvt[c("rebPtn.1","rebPtn.n")] ,na.rm=T)
            rVal["snR3E"]       <- sum(scrEvt[c("snR3"    )] ,na.rm=T)
            rVal["snMaxESum"]   <- sum(scrEvt[c("snMax.r" ,"snMax.c"  ,"snMax.f" )] ,na.rm=T)
            rVal["snFCntESum"]  <- sum(scrEvt[c("snFCnt.r","snFCnt.c" ,"snFCnt.f")] ,na.rm=T)
            rVal["snRESum"]     <- sum(scrEvt[c("snMax.r" ,"snFCnt.r" )] ,na.rm=T)
            rVal["snCESum"]     <- sum(scrEvt[c("snMax.c" ,"snFCnt.c" )] ,na.rm=T)
            rVal["snFESum"]     <- sum(scrEvt[c("snMax.f" ,"snFCnt.f" )] ,na.rm=T)

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


mName <- "sScore04"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # pBanN.r pBanN.n pLCol pE3 pE4 pMH pfNum 
        # iBanN iLCol iE3 iE4 iMH ifNum 
        # FVa.m FVa.c 
        # m4
              
    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("xBan.x" ,"xLCol" ,"xEn","xfNum","xMH","eSum_FVaM4")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["xBan.x"]  <- sum(score[c("pBanN.r","pBanN.n","iBanN")] ,na.rm=T)
            rVal["xLCol"]   <- sum(score[c("pLCol","iLCol")] ,na.rm=T)
            rVal["xEn"]     <- sum(score[c("pE3","pE4","iE3","iE4")] ,na.rm=T)
            rVal["xfNum"]   <- sum(score[c("pfNum","ifNum")] ,na.rm=T)
            rVal["xMH"]     <- sum(score[c("pMH","iMH")] ,na.rm=T)
            rVal["eSum_FVaM4"]  <- sum(scrEvt[c("FVa.m","FVa.c","m4")] ,na.rm=T)

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


mName <- "sScore05"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # pBanN.r pBanN.n pLCol pE3 pE4 pMH pfNum 
        # iBanN iLCol iE3 iE4 iMH ifNum 
        # FVa.m FVa.c 
        # m4
              
    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("xBan.x" ,"xLCol" ,"xEn","xfNum","xMH","eSum_FVaM4")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["xBan.x"]  <- sum(score[c("pBanN.r","pBanN.n","iBanN")] ,na.rm=T)
            rVal["xLCol"]   <- sum(score[c("pLCol","iLCol")] ,na.rm=T)
            rVal["xEn"]     <- sum(score[c("pE3","pE4","iE3","iE4")] ,na.rm=T)
            rVal["xfNum"]   <- sum(score[c("pfNum","ifNum")] ,na.rm=T)
            rVal["xMH"]     <- sum(score[c("pMH","iMH")] ,na.rm=T)
            rVal["eSum_FVaM4"]  <- sum(scrEvt[c("FVa.m","FVa.c","m4")] ,na.rm=T)

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

mName <- "sScore06"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # pBanN.r pBanN.n pLCol pE3 pE4 pMH pfNum 
        # iBanN iLCol iE3 iE4 iMH ifNum 
        # FVa.m FVa.c 
        # m4
              
    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("xBan.x" ,"xLCol" ,"xEn","xfNum","xMH","eSum_FVaM4")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["xBan.x"]  <- sum(score[c("pBanN.r","pBanN.n","iBanN")] ,na.rm=T)
            rVal["xLCol"]   <- sum(score[c("pLCol","iLCol")] ,na.rm=T)
            rVal["xEn"]     <- sum(score[c("pE3","pE4","iE3","iE4")] ,na.rm=T)
            rVal["xfNum"]   <- sum(score[c("pfNum","ifNum")] ,na.rm=T)
            rVal["xMH"]     <- sum(score[c("pMH","iMH")] ,na.rm=T)
            rVal["eSum_FVaM4"]  <- sum(scrEvt[c("FVa.m","FVa.c","m4")] ,na.rm=T)

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

mName <- "sScore07"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # pBanN.r pBanN.n pLCol pE3 pE4 pMH pfNum 
        # iBanN iLCol iE3 iE4 iMH ifNum 
        # FVa.m FVa.c 
        # m4
              
    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("xBan.x" ,"xLCol" ,"xEn","xfNum","xMH","eSum_FVaM4")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["xBan.x"]  <- sum(score[c("pBanN.r","pBanN.n","iBanN")] ,na.rm=T)
            rVal["xLCol"]   <- sum(score[c("pLCol","iLCol")] ,na.rm=T)
            rVal["xEn"]     <- sum(score[c("pE3","pE4","iE3","iE4")] ,na.rm=T)
            rVal["xfNum"]   <- sum(score[c("pfNum","ifNum")] ,na.rm=T)
            rVal["xMH"]     <- sum(score[c("pMH","iMH")] ,na.rm=T)
            rVal["eSum_FVaM4"]  <- sum(scrEvt[c("FVa.m","FVa.c","m4")] ,na.rm=T)

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


mName <- "sScore08"
if( TRUE ){

    bSMtxExtFltLst[[mName]] <- list()
        # c31 c32 c33 c34 
        # c21 c22 c23 c24 c25 
        # max3 min3 
        # max2MatCnt min2MatCnt
        # cTbl fTbl

    fltCreater <- function( mName ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("c3x" ,"c2x" ,"c3x2xOvLAll","c3x2xOvLPar","c3x2xOvLNo","maxMin3","max3min2","max2min3","cfTbl")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["c3x"]     <- sum(score[c("c31","c32","c33","c34")] ,na.rm=T)
            rVal["c2x"]     <- sum(score[c("c21","c22","c23","c24","c25")] ,na.rm=T)

            c3c2Overlap_all_Name     <- c(   "c31c21"= (score["c31"]&&score["c21"])
                                            ,"c31c22"= (score["c31"]&&score["c22"])
                                            ,"c32c22"= (score["c32"]&&score["c22"])
                                            ,"c32c23"= (score["c32"]&&score["c23"])
                                            ,"c33c23"= (score["c33"]&&score["c23"])
                                            ,"c33c24"= (score["c33"]&&score["c24"])
                                            ,"c34c24"= (score["c34"]&&score["c24"])
                                            ,"c34c25"= (score["c34"]&&score["c25"])
            )
            c3c2Overlap_partial_Name <- c(   "c31c23"= (score["c31"]&&score["c23"])
                                            ,"c32c21"= (score["c32"]&&score["c21"])
                                            ,"c32c24"= (score["c32"]&&score["c24"])
                                            ,"c33c22"= (score["c33"]&&score["c22"])
                                            ,"c33c25"= (score["c33"]&&score["c25"])
                                            ,"c34c23"= (score["c34"]&&score["c23"])         
            )
            c3c2Overlap_None_Name    <- c(   "c31c24"= (score["c31"]&&score["c24"])
                                            ,"c31c25"= (score["c31"]&&score["c25"])
                                            ,"c32c25"= (score["c32"]&&score["c25"])
                                            ,"c33c21"= (score["c33"]&&score["c21"])
                                            ,"c34c21"= (score["c34"]&&score["c21"])
                                            ,"c34c22"= (score["c34"]&&score["c22"])
            )

            rVal["c3x2xOvLAll"] <- sum(c3c2Overlap_all_Name)
            rVal["c3x2xOvLPar"] <- sum(c3c2Overlap_partial_Name)
            rVal["c3x2xOvLNo" ] <- sum(c3c2Overlap_None_Name)

            rVal["maxMin3"] <- sum(score[c("max3","min3")] ,na.rm=T)
            rVal["max3min2"]<- (score["max3"]>0 && score["min2MatCnt"]>=2)
            rVal["max2min3"]<- (score["max2MatCnt"]>=2 && score["min3"]>0)
            rVal["cfTbl"]   <- sum(scrEvt[c("cTbl","fTbl")] ,na.rm=T)
            #   "maxMin3", "max3min2", "max2min3" <--- max,min table 길이가 5가 아닌 경우, 의미있다!

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
        fltObj$mInfo$cName <- c( "rD2" ,"rLr" ,"rRl" ,"eD2" ,"eLr" ,"eRl" ,"cD2" ,"cLr" ,"cRl" ,"fCnt" )
        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName
            scrEvt <- bFCust.getEvt( score ,bsScoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal[fltObj$mInfo$cName]<- score[fltObj$mInfo$cName]
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
    bSMtxExtFltLst[[mName]]$freqValReb <- fltCreater(mName)


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


