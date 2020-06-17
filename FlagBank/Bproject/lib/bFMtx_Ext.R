
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


    colName=c("remN.num","remN.len.tot","remN.len.val" ,"cN.num","cN.len.tot","cN.len.val" ,"fN.num","fN.len.tot","fN.len.val")
    fltCreater <- function( mName ,cName ) {    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

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

            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter01 <- fltCreater(mName,colName)    #   bFMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )

    colName=c("evt0.num","evt0.len.tot","evt0.len.val","evt1.num","evt1.len.tot","evt1.len.val")
    fltCreater <- function( mName ,cName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

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

            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter02 <- fltCreater(mName,colName)
}

mName <- "score2"
if( TRUE ){
    bFMtxExtFltLst[[mName]] <- list()
        # rebC.r  rebC.c  rebC.f 
        # rebC2.r rebC2.c rebC2.f   
        # inc.r   inc.c   inc.f  
        # inc.r2  inc.c2  inc.f2

        # QQE : work
        # rebCN.r  rebCN.c  rebCN.f 
        # incN.r   incN.c   incN.f

        # evtRebC evtRebC2 evtInc evtInc2


    colName=c("ec1","ec2","ec3")
    fltCreater <- function( mName ,cName ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic[[mName]]$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName
            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter01 <- fltCreater(mName,colName)
}



