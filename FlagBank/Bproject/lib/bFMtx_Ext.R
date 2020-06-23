
bFMtxExtFltLst <- list()

mName <- "score1"
if( TRUE ){
    bFMtxExtFltLst[[mName]] <- list()
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
    bFMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)    #   bFMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )

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
    bFMtxExtFltLst[[mName]]$filter02 <- fltCreater(mName)
}

mName <- "score2"
if( TRUE ){
    bFMtxExtFltLst[[mName]] <- list()
        # rebV.r    rebL    rebR  
        # rebC.r  rebC.c  rebC.f 
        # rebC2.r rebC2.c rebC2.f   
        # inc.r   inc.c   inc.f  
        # inc.r2  inc.c2  inc.f2
        # inc.r3  inc.c3

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

            matFlag <- all( score[c("inc.r","inc.c")]==score[c("inc.r2","inc.c2")] ) && all( score[c("inc.r","inc.c")]==score[c("inc.r3","inc.c3")] )
            if( matFlag ){
                #   그냥 3개 컬럼 매치 여부로만 하면 빈도가 높을 것 같아, 매치되는 총 갯수로 한다.
                rVal["matCntInc12"]  <- sum(score[c("inc.r","inc.c")])
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
    bFMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)

    fltCreater <- function( mName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- c("evtRebLR","evtRebC","evtRebC2","evtInc","evtInc2","evtInc3")

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName

            scrEvt <- bFCust.getEvt( score ,scoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

            rVal["evtRebLR"]    <- sum(scrEvt[c("rebL","rebR")] ,na.rm=T)
            rVal["evtRebC"]     <- sum(scrEvt[c("rebC.r","rebC.c","rebC.f")] ,na.rm=T)
            rVal["evtRebC2"]    <- sum(scrEvt[c("rebC2.r","rebC2.c","rebC2.f")] ,na.rm=T)
            rVal["evtInc"]      <- sum(scrEvt[c("inc.r","inc.c","inc.f")] ,na.rm=T)
            rVal["evtInc2"]     <- sum(scrEvt[c("inc.r2","inc.c2","inc.f2")] ,na.rm=T)
            rVal["evtInc3"]     <- sum(scrEvt[c("inc.r3","inc.c3")] ,na.rm=T)

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
    bFMtxExtFltLst[[mName]]$filter02 <- fltCreater(mName)

}

mName <- "score3"
if( TRUE ){

    bFMtxExtFltLst[[mName]] <- list()
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

            scrEvt <- bFCust.getEvt( score ,scoreMtxCfg[[fltObj$mInfo$mName]]$fCol )["lev",]

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
    bFMtxExtFltLst[[mName]]$filter01 <- fltCreater(mName)


}


