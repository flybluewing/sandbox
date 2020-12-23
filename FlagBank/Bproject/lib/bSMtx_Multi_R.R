if( FALSE ){    # document
    # bSMtxRMLst
    #   다수 scoreMtx 관점에서의 커팅시도. 즉 raw 값.
    #       참고코드 : bFMtxMulti.R (bFMtxMFltLst)
}


bSMtxRMLst <- list()

mfName <- "bsMR4567"
if( TRUE ){

    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore04","sScore05","sScore06","sScore07")
        fltObj$mInfo$cName <- c( "pBanN.r","pBanN.n"
                                ,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
                                ,"iBanN"
                                ,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"
                                ,"FVa.m","FVa.c"
                                ,"m4"
                                ,"xBan.x","xLCol","xEn","xfNum","xMH","eSum_FVaM4"  # extention
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,aIdx ){
            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            val <- c( "sScore04"=0 ,"sScore05"=0 ,"sScore06"=0 ,"sScore07"=0 )

            cName <- "pBanN.r"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pBanN.n"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pLCol"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pE3"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pE4"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pMH"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "pfNum"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "iBanN"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 2
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )


            cName <- "iLCol"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "iE3"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "iE4"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "iMH"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "ifNum"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "FVa.m"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "FVa.c"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 2
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )

            cName <- "m4"
            val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
            val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
            val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
            val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("sScore06","sScore07")]) + any( 0<val[c("sScore04","sScore05")] )


            cName <- "xBan.x"
            rowVal[cName]   <- sum( rowVal[c("pBanN.r","pBanN.n","iBanN")]  )
            if( 1 >= rowVal[cName] )  rowVal[cName] <- 0    # 원본 컬럼들과의 중복 방지.

            cName <- "xLCol"
            if( TRUE ){   # sScore04,6,7에 대해서만 체크. (sScore05는 너무 빈번함.)
                val <- c( "sScore04"=0,"sScore06"=0 ,"sScore07"=0 )
                accVal <- c("pLCol"=0,"iLCol"=0)

                cName <- "pLCol"
                val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "iLCol"
                val["sScore04"] <- mmMtxLst[["sScore04"]][aIdx,cName] >= 1
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                rowVal[cName]   <- sum( accVal )
                if( 1 >= rowVal[cName] )  rowVal[cName] <- 0
            }

            cName <- "xEn"
            rowVal[cName]   <- sum( rowVal[c("pE3","pE4","iE3","iE4")]  )
            if( 1 >= rowVal[cName] )  rowVal[cName] <- 0

            cName <- "xfNum"
            rowVal[cName]   <- sum( rowVal[c("pfNum","ifNum")]          )
            if( 1 >= rowVal[cName] )  rowVal[cName] <- 0

            cName <- "xMH"
            rowVal[cName]   <- sum( rowVal[c("pMH","iMH")]              )
            if( 1 >= rowVal[cName] )  rowVal[cName] <- 0

            cName <- "eSum_FVaM4"
            if( TRUE ){   # sScore06,7에 대해서만 체크. (sScore04,5는 너무 빈번함.)
                val <- c( "sScore06"=0 ,"sScore07"=0 )
                accVal <- c("FVa.m"=0,"FVa.c"=0,"m4"=0)

                cName <- "FVa.m"
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "FVa.c"
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "m4"
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                val["sScore07"] <- mmMtxLst[["sScore07"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                rowVal[cName]   <- sum( accVal )
                if( 1 >= rowVal[cName] )  rowVal[cName] <- 0
            }

            return( rowVal )

            # for( nIdx in names(mmMtxLst) ){   # 테스트 데이터 생성용.
            #     for( aIdx in 1:10 ){
            #         mmMtxLst[[nIdx]][aIdx,] <- sample(1:5,length(mmMtxLst[[nIdx]][aIdx,]),replace=T)
            #     }
            # }
            
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }

}


mfName <- "bsMR1234"
if( TRUE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore01","sScore02","sScore03","sScore04")
        fltObj$mInfo$cName <- c( "s1rem0.len.tot","s1rem1.len.tot"  ,"s1c0.len.tot" ,"s1c1.len.tot" ,"s1f0.len.tot","s1f1.len.tot"
                ,"s2rebC.r" ,"s2rebC.c" ,"s2rebC.f" ,"s2rebC2.r" ,"s2rebC2.c" ,"s2rebC2.f"
                ,"s2inc.r" ,"s2inc.c" ,"s2inc.f" ,"s2inc.r2" ,"s2inc.c2" ,"s2inc.f2"
                ,"s3rebPtn.1","s3snMax.r","s3snMax.c","s3snMax.f"
                ,"s4pBanN.r","s4pBanN.n","s4pLCol","s4pE3","s4pfNum","s4iBanN","s4iLCol","s4ifNum","s4FVa.m"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }
        fltObj$getScore <- function( mmMtxLst ,aIdx ){
            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            rowVal["s1rem0.len.tot"]<- mmMtxLst[["sScore01"]][aIdx,"rem0.len.tot"]
            rowVal["s1rem1.len.tot"]<- mmMtxLst[["sScore01"]][aIdx,"rem1.len.tot"]
            rowVal["s1c0.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"c0.len.tot"]
            rowVal["s1c1.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"c1.len.tot"]
            rowVal["s1f0.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"f0.len.tot"]
            rowVal["s1f1.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"f1.len.tot"]

            rowVal["s2rebC.r"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.r"]
            rowVal["s2rebC.c"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.c"]
            rowVal["s2rebC.f"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.f"]
            rowVal["s2rebC2.r"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.r"]
            rowVal["s2rebC2.c"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.c"]
            rowVal["s2rebC2.f"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.f"]
            rowVal["s2inc.r"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.r"]
            rowVal["s2inc.c"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.c"]
            rowVal["s2inc.f"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.f"]
            rowVal["s2inc.r2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.r2"]
            rowVal["s2inc.c2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.c2"]
            rowVal["s2inc.f2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.f2"]

            rowVal["s3rebPtn.1"]<- mmMtxLst[["sScore03"]][aIdx,"rebPtn.1"]
            rowVal["s3snMax.r"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.r"]
            rowVal["s3snMax.c"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.c"]
            rowVal["s3snMax.f"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.f"]

            rowVal["s4pBanN.r"] <- mmMtxLst[["sScore04"]][aIdx,"pBanN.r"]
            rowVal["s4pBanN.n"] <- mmMtxLst[["sScore04"]][aIdx,"pBanN.n"]
            rowVal["s4pLCol"]   <- mmMtxLst[["sScore04"]][aIdx,"pLCol"]
            rowVal["s4pE3"]     <- mmMtxLst[["sScore04"]][aIdx,"pE3"]
            rowVal["s4pfNum"]   <- mmMtxLst[["sScore04"]][aIdx,"pfNum"]
            rowVal["s4iBanN"]   <- mmMtxLst[["sScore04"]][aIdx,"iBanN"]
            rowVal["s4iLCol"]   <- mmMtxLst[["sScore04"]][aIdx,"iLCol"]
            rowVal["s4ifNum"]   <- mmMtxLst[["sScore04"]][aIdx,"ifNum"]
            rowVal["s4FVa.m"]   <- mmMtxLst[["sScore04"]][aIdx,"FVa.m"]

            return( rowVal )

        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }
        return(fltObj)
    }
}



mfName <- "bsMR569Lecf"
if( TRUE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c(   "sScore05","sScore06","sScore09"
                                ,"sScore0LAe13","sScore0LAe24","sScore0LVe24","sScore0LVe24"
                                ,"sScore0LAf13","sScore0LAf24","sScore0LVf24","sScore0LVf24"
                                ,"sScore0LAf13","sScore0LAf24","sScore0LVf24","sScore0LVf24"
        )        
        fltObj$mInfo$cName <- c( "pBanN.r","pBanN.n","pLCol","pE3","pfNum","iBanN","iLCol","ifNum","FVa.m"
                                ,"eCnt" ,"cCnt" ,"fCnt"
                                ,"Ae13Hpn1A" ,"Ae13Hpn1B" ,"Ae24Hpn1A" ,"Ae24Hpn1B" 
                                ,"Ve13Hpn1A" ,"Ve13Hpn1B" ,"Ve24Hpn1A" ,"Ve24Hpn1B" 
                                ,"Ac13Hpn1A" ,"Ac13Hpn1B" ,"Ac24Hpn1A" ,"Ac24Hpn1B" 
                                ,"Vc13Hpn1A" ,"Vc13Hpn1B" ,"Vc24Hpn1A" ,"Vc24Hpn1B" 
                                ,"Af13Hpn1A" ,"Af13Hpn1B" ,"Af24Hpn1A" ,"Af24Hpn1B" 
                                ,"Vf13Hpn1A" ,"Vf13Hpn1B" ,"Vf24Hpn1A" ,"Vf24Hpn1B" 
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }
        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){
            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            val <- c( "sScore05"=0 ,"sScore06"=0)

            cNames <- c( "pBanN.r","pBanN.n","pLCol","pE3","pfNum","iBanN","iLCol","ifNum","FVa.m" )
            for( cName in cNames ){
                val[] <- 0
                val["sScore05"] <- mmMtxLst[["sScore05"]][aIdx,cName] >= 1
                val["sScore06"] <- mmMtxLst[["sScore06"]][aIdx,cName] >= 1
                rowVal[cName] <- sum( val )
            }

            rowVal["eCnt"]  <- mmMtxLst[["sScore09"]][aIdx,"eCnt"]
            rowVal["cCnt"]  <- mmMtxLst[["sScore09"]][aIdx,"cCnt"]
            rowVal["fCnt"]  <- mmMtxLst[["sScore09"]][aIdx,"fCnt"]

            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Ae13Hpn1A"] <- sum( mmMtxLst[["sScore0LAe13"]][aIdx,colA]==1 )
            rowVal["Ae13Hpn1B"] <- sum( mmMtxLst[["sScore0LAe13"]][aIdx,colB]==1 )
            rowVal["Ae24Hpn1A"] <- sum( mmMtxLst[["sScore0LAe24"]][aIdx,colA]==1 )
            rowVal["Ae24Hpn1B"] <- sum( mmMtxLst[["sScore0LAe24"]][aIdx,colB]==1 )
            rowVal["Ve13Hpn1A"] <- sum( mmMtxLst[["sScore0LVe13"]][aIdx,colA]==1 )
            rowVal["Ve13Hpn1B"] <- sum( mmMtxLst[["sScore0LVe13"]][aIdx,colB]==1 )
            rowVal["Ve24Hpn1A"] <- sum( mmMtxLst[["sScore0LVe24"]][aIdx,colA]==1 )
            rowVal["Ve24Hpn1B"] <- sum( mmMtxLst[["sScore0LVe24"]][aIdx,colB]==1 )

            rowVal["Ac13Hpn1A"] <- sum( mmMtxLst[["sScore0LAc13"]][aIdx,colA]==1 )
            rowVal["Ac13Hpn1B"] <- sum( mmMtxLst[["sScore0LAc13"]][aIdx,colB]==1 )
            rowVal["Ac24Hpn1A"] <- sum( mmMtxLst[["sScore0LAc24"]][aIdx,colA]==1 )
            rowVal["Ac24Hpn1B"] <- sum( mmMtxLst[["sScore0LAc24"]][aIdx,colB]==1 )
            rowVal["Vc13Hpn1A"] <- sum( mmMtxLst[["sScore0LVc13"]][aIdx,colA]==1 )
            rowVal["Vc13Hpn1B"] <- sum( mmMtxLst[["sScore0LVc13"]][aIdx,colB]==1 )
            rowVal["Vc24Hpn1A"] <- sum( mmMtxLst[["sScore0LVc24"]][aIdx,colA]==1 )
            rowVal["Vc24Hpn1B"] <- sum( mmMtxLst[["sScore0LVc24"]][aIdx,colB]==1 )

            rowVal["Af13Hpn1A"] <- sum( mmMtxLst[["sScore0LAf13"]][aIdx,colA]==1 )
            rowVal["Af13Hpn1B"] <- sum( mmMtxLst[["sScore0LAf13"]][aIdx,colB]==1 )
            rowVal["Af24Hpn1A"] <- sum( mmMtxLst[["sScore0LAf24"]][aIdx,colA]==1 )
            rowVal["Af24Hpn1B"] <- sum( mmMtxLst[["sScore0LAf24"]][aIdx,colB]==1 )
            rowVal["Vf13Hpn1A"] <- sum( mmMtxLst[["sScore0LVf13"]][aIdx,colA]==1 )
            rowVal["Vf13Hpn1B"] <- sum( mmMtxLst[["sScore0LVf13"]][aIdx,colB]==1 )
            rowVal["Vf24Hpn1A"] <- sum( mmMtxLst[["sScore0LVf24"]][aIdx,colA]==1 )
            rowVal["Vf24Hpn1B"] <- sum( mmMtxLst[["sScore0LVf24"]][aIdx,colB]==1 )
            # rowVal["___"]  <- mmMtxLst[["sScore01"]][aIdx,"___"]

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}






mfName <- "bsMRLArn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAr13","sScore0LAr24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }


        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLVrn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVr13","sScore0LVr24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }


        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLAVrn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }


        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLAen"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAe13","sScore0LAe24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }


        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLVen"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVe13","sScore0LVe24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }


        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLAcn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){ 

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAc13","sScore0LAc24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLVcn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVc13","sScore0LVc24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLAfn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAf13","sScore0LAf24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}
mfName <- "bsMRLVfn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVf13","sScore0LVf24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}
mfName <- "bsMRLAVfn"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}


mfName <- "bsMRLAecf13"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAe13","sScore0LAc13","sScore0LAf13")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLAecf24"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAe24","sScore0LAc24","sScore0LAf24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLVecf13"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVe13","sScore0LVc13","sScore0LVf13")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}

mfName <- "bsMRLVecf24"
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LVe24","sScore0LVc24","sScore0LVf24")
        fltObj$mInfo$cName <- c( "hpn1","hpnE"
                                ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6"
                                ,"colEHpn1" ,"colEHpn2" ,"colEHpn3" ,"colEHpn4" ,"colEHpn5" ,"colEHpn6"
		)

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # cName <- c( "hpn1" ,"hpnE" ,"col1Hpn1" ,"col1Hpn2" ,"col1Hpn3" ,"col1Hpn4" ,"col1Hpn5" ,"col1Hpn6" )
            # ignoreCol <- cName[ rowVal[cName]==1 ]
            # rowVal[ ignoreCol ] <- 0

            for( mName in fltObj$fltMNames ){
                rVal <- mmMtxLst[[mName]][aIdx,]
                eVal <- mmEMtxLst[[mName]][aIdx,]
                rowVal["hpn1"] <- rowVal["hpn1"] + sum( rVal == 1 )
                rowVal["hpnE"] <- rowVal["hpnE"] + sum( !is.na(eVal) )

                rowVal["col1Hpn1"] <- rowVal["col1Hpn1"] + sum( rVal[c("colA1","colB1")]==1 )
                rowVal["col1Hpn2"] <- rowVal["col1Hpn2"] + sum( rVal[c("colA2","colB2")]==1 )
                rowVal["col1Hpn3"] <- rowVal["col1Hpn3"] + sum( rVal[c("colA3","colB3")]==1 )
                rowVal["col1Hpn4"] <- rowVal["col1Hpn4"] + sum( rVal[c("colA4","colB4")]==1 )
                rowVal["col1Hpn5"] <- rowVal["col1Hpn5"] + sum( rVal[c("colA5","colB5")]==1 )
                rowVal["col1Hpn6"] <- rowVal["col1Hpn6"] + sum( rVal[c("colA6","colB6")]==1 )

                rowVal["colEHpn1"] <- rowVal["colEHpn1"] + sum( !is.na(eVal[c("colA1","colB1")]) )
                rowVal["colEHpn2"] <- rowVal["colEHpn2"] + sum( !is.na(eVal[c("colA2","colB2")]) )
                rowVal["colEHpn3"] <- rowVal["colEHpn3"] + sum( !is.na(eVal[c("colA3","colB3")]) )
                rowVal["colEHpn4"] <- rowVal["colEHpn4"] + sum( !is.na(eVal[c("colA4","colB4")]) )
                rowVal["colEHpn5"] <- rowVal["colEHpn5"] + sum( !is.na(eVal[c("colA5","colB5")]) )
                rowVal["colEHpn6"] <- rowVal["colEHpn6"] + sum( !is.na(eVal[c("colA6","colB6")]) )

            }

            return( rowVal )
        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)

    }
}



# -------------------------------------------------------------------------------
# 일단 이후는 결과를 보류하고 추세에 따라 작업여부를 판단하자.
#     추가대상 목록
#         mfName <- "bsMR_FV1234" # cfg 생성할 것.
#         mfName <- "bsMR_FV567"  # cfg 생성할 것.
#         mfName <- "bsMR_FV89"   # cfg 생성할 것.
#         mfName <- "bsMRAVr_hpn1AB" # cfg 생성할 것.
#         mfName <- "bsMRAVe_hpn1AB" # cfg 생성할 것.
#         mfName <- "bsMRAVc_hpn1AB" # cfg 생성할 것.
#         mfName <- "bsMRAVf_hpn1AB" # cfg 생성할 것.

mfName <- "bsMR_FV1234" # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c( "sScore01","sScore02","sScore03","sScore04" )
        fltObj$mInfo$cName <- c( 
                 "s1rem0.len.tot","s1rem1.len.tot"  ,"s1c0.len.tot" ,"s1c1.len.tot" ,"s1f0.len.tot","s1f1.len.tot" ,"s1zwNum"
                ,"s2rebV.r" ,"s2rebC.r" ,"s2rebC.c" ,"s2rebC.f" ,"s2rebC2.r" ,"s2rebC2.c" ,"s2rebC2.f"
                ,"s2inc.r" ,"s2inc.c" ,"s2inc.f" ,"s2inc.r2" ,"s2inc.c2" ,"s2inc.f2" ,"s2inc.r3" ,"s2inc.c3"
                ,"s3rebPtn.1","s3snMax.r","s3snMax.c","s3snMax.f"
                ,"s4pBanN.r","s4pBanN.n","s4pLCol","s4pE3","s4pfNum","s4iBanN","s4iLCol","s4ifNum","s4FVa.m"
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            rowVal["s1rem0.len.tot"]<- mmMtxLst[["sScore01"]][aIdx,"rem0.len.tot"]
            rowVal["s1rem1.len.tot"]<- mmMtxLst[["sScore01"]][aIdx,"rem1.len.tot"]
            rowVal["s1c0.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"c0.len.tot"]
            rowVal["s1c1.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"c1.len.tot"]
            rowVal["s1f0.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"f0.len.tot"]
            rowVal["s1f1.len.tot"]  <- mmMtxLst[["sScore01"]][aIdx,"f1.len.tot"]
            rowVal["s1zwNum"]       <- mmMtxLst[["sScore01"]][aIdx,"zwNum"]

            rowVal["s2rebV.r"]  <- mmMtxLst[["sScore02"]][aIdx,"rebV.r"]
            rowVal["s2rebC.r"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.r"]
            rowVal["s2rebC.c"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.c"]
            rowVal["s2rebC.f"]  <- mmMtxLst[["sScore02"]][aIdx,"rebC.f"]
            rowVal["s2rebC2.r"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.r"]
            rowVal["s2rebC2.c"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.c"]
            rowVal["s2rebC2.f"] <- mmMtxLst[["sScore02"]][aIdx,"rebC2.f"]
            rowVal["s2inc.r"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.r"]
            rowVal["s2inc.c"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.c"]
            rowVal["s2inc.f"]   <- mmMtxLst[["sScore02"]][aIdx,"inc.f"]
            rowVal["s2inc.r2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.r2"]
            rowVal["s2inc.c2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.c2"]
            rowVal["s2inc.f2"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.f2"]
            rowVal["s2inc.r3"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.r3"]
            rowVal["s2inc.c3"]  <- mmMtxLst[["sScore02"]][aIdx,"inc.c3"]

            rowVal["s3rebPtn.1"]<- mmMtxLst[["sScore03"]][aIdx,"rebPtn.1"]
            rowVal["s3snMax.r"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.r"]
            rowVal["s3snMax.c"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.c"]
            rowVal["s3snMax.f"] <- mmMtxLst[["sScore03"]][aIdx,"snMax.f"]

            rowVal["s4pBanN.r"] <- mmMtxLst[["sScore04"]][aIdx,"pBanN.r"]
            rowVal["s4pBanN.n"] <- mmMtxLst[["sScore04"]][aIdx,"pBanN.n"]
            rowVal["s4pLCol"]   <- mmMtxLst[["sScore04"]][aIdx,"pLCol"]
            rowVal["s4pE3"]     <- mmMtxLst[["sScore04"]][aIdx,"pE3"]
            rowVal["s4pfNum"]   <- mmMtxLst[["sScore04"]][aIdx,"pfNum"]
            rowVal["s4iBanN"]   <- mmMtxLst[["sScore04"]][aIdx,"iBanN"]
            rowVal["s4iLCol"]   <- mmMtxLst[["sScore04"]][aIdx,"iLCol"]
            rowVal["s4ifNum"]   <- mmMtxLst[["sScore04"]][aIdx,"ifNum"]
            rowVal["s4FVa.m"]   <- mmMtxLst[["sScore04"]][aIdx,"FVa.m"]

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMR_FV567"  # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c( "sScore05","sScore06","sScore07" )
        fltObj$mInfo$cName <- c( 
                 "s5pBanN.r","s5pBanN.n","s5pLCol","s5pE3","s5pfNum","s5iBanN","s5iLCol","s5ifNum","s5FVa.m"
                ,"s6pBanN.r","s6pBanN.n","s6pLCol","s6pE3","s6pfNum","s6iBanN","s6iLCol","s6ifNum","s6FVa.m"
                ,"s7pBanN.r","s7pBanN.n","s7pLCol","s7pE3","s7pfNum","s7iBanN","s7iLCol","s7ifNum","s7FVa.m"
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            #  "pBanN.r" ,"pBanN.n" ,"pLCol" ,"pE3" ,"pfNum" ,"iBanN" ,"iLCol" ,"ifNum" ,"FVa.m"
            rowVal["s5pBanN.r"] <- mmMtxLst[["sScore05"]][aIdx,"pBanN.r"]
            rowVal["s5pBanN.n"] <- mmMtxLst[["sScore05"]][aIdx,"pBanN.n"]
            rowVal["s5pLCol"]   <- mmMtxLst[["sScore05"]][aIdx,"pLCol"]
            rowVal["s5pE3"]     <- mmMtxLst[["sScore05"]][aIdx,"pE3"]
            rowVal["s5pfNum"]   <- mmMtxLst[["sScore05"]][aIdx,"pfNum"]
            rowVal["s5iBanN"]   <- mmMtxLst[["sScore05"]][aIdx,"iBanN"]
            rowVal["s5iLCol"]   <- mmMtxLst[["sScore05"]][aIdx,"iLCol"]
            rowVal["s5ifNum"]   <- mmMtxLst[["sScore05"]][aIdx,"ifNum"]
            rowVal["s5FVa.m"]   <- mmMtxLst[["sScore05"]][aIdx,"FVa.m"]

            rowVal["s6pBanN.r"] <- mmMtxLst[["sScore06"]][aIdx,"pBanN.r"]
            rowVal["s6pBanN.n"] <- mmMtxLst[["sScore06"]][aIdx,"pBanN.n"]
            rowVal["s6pLCol"]   <- mmMtxLst[["sScore06"]][aIdx,"pLCol"]
            rowVal["s6pE3"]     <- mmMtxLst[["sScore06"]][aIdx,"pE3"]
            rowVal["s6pfNum"]   <- mmMtxLst[["sScore06"]][aIdx,"pfNum"]
            rowVal["s6iBanN"]   <- mmMtxLst[["sScore06"]][aIdx,"iBanN"]
            rowVal["s6iLCol"]   <- mmMtxLst[["sScore06"]][aIdx,"iLCol"]
            rowVal["s6ifNum"]   <- mmMtxLst[["sScore06"]][aIdx,"ifNum"]
            rowVal["s6FVa.m"]   <- mmMtxLst[["sScore06"]][aIdx,"FVa.m"]

            rowVal["s7pBanN.r"] <- mmMtxLst[["sScore07"]][aIdx,"pBanN.r"]
            rowVal["s7pBanN.n"] <- mmMtxLst[["sScore07"]][aIdx,"pBanN.n"]
            rowVal["s7pLCol"]   <- mmMtxLst[["sScore07"]][aIdx,"pLCol"]
            rowVal["s7pE3"]     <- mmMtxLst[["sScore07"]][aIdx,"pE3"]
            rowVal["s7pfNum"]   <- mmMtxLst[["sScore07"]][aIdx,"pfNum"]
            rowVal["s7iBanN"]   <- mmMtxLst[["sScore07"]][aIdx,"iBanN"]
            rowVal["s7iLCol"]   <- mmMtxLst[["sScore07"]][aIdx,"iLCol"]
            rowVal["s7ifNum"]   <- mmMtxLst[["sScore07"]][aIdx,"ifNum"]
            rowVal["s7FVa.m"]   <- mmMtxLst[["sScore07"]][aIdx,"FVa.m"]

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMR_FV89"   # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore08","sScore09")
        fltObj$mInfo$cName <- c( "s8_c21","s8_c22","s8_c23","s8_c24","s8_c25","s8_min3","s8_min2MatCnt" 
                                ,"s9_rCnt","s9_eCnt","s9_cCnt","s9_fCnt"
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            rowVal["s8_c21"]        <- mmMtxLst[["sScore08"]][aIdx,"c21"]
            rowVal["s8_c22"]        <- mmMtxLst[["sScore08"]][aIdx,"c22"]
            rowVal["s8_c23"]        <- mmMtxLst[["sScore08"]][aIdx,"c23"]
            rowVal["s8_c24"]        <- mmMtxLst[["sScore08"]][aIdx,"c24"]
            rowVal["s8_c25"]        <- mmMtxLst[["sScore08"]][aIdx,"c25"]
            rowVal["s8_min3"]       <- mmMtxLst[["sScore08"]][aIdx,"min3"]
            rowVal["s8_min2MatCnt"] <- mmMtxLst[["sScore08"]][aIdx,"min2MatCnt"]

            rowVal["s9_rCnt"]       <- mmMtxLst[["sScore09"]][aIdx,"rCnt"]
            rowVal["s9_eCnt"]       <- mmMtxLst[["sScore09"]][aIdx,"eCnt"]
            rowVal["s9_cCnt"]       <- mmMtxLst[["sScore09"]][aIdx,"cCnt"]
            rowVal["s9_fCnt"]       <- mmMtxLst[["sScore09"]][aIdx,"fCnt"]

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMRAVr_hpn1AB" # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAr13","sScore0LAr24","sScore0LVr24","sScore0LVr24")
        fltObj$mInfo$cName <- c( "Ar13Hpn1A" ,"Ar13Hpn1B" ,"Ar24Hpn1A" ,"Ar24Hpn1B" 
                                ,"Vr13Hpn1A" ,"Vr13Hpn1B" ,"Vr24Hpn1A" ,"Vr24Hpn1B" 
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # colA1 colA2 colA3 colA4 colA5 colA6 colB1 colB2 colB3 colB4 colB5 colB6
            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Ar13Hpn1A"] <- sum( mmMtxLst[["sScore0LAr13"]][aIdx,colA]==1 )
            rowVal["Ar13Hpn1B"] <- sum( mmMtxLst[["sScore0LAr13"]][aIdx,colB]==1 )
            rowVal["Ar24Hpn1A"] <- sum( mmMtxLst[["sScore0LAr24"]][aIdx,colA]==1 )
            rowVal["Ar24Hpn1B"] <- sum( mmMtxLst[["sScore0LAr24"]][aIdx,colB]==1 )
            rowVal["Vr13Hpn1A"] <- sum( mmMtxLst[["sScore0LVr13"]][aIdx,colA]==1 )
            rowVal["Vr13Hpn1B"] <- sum( mmMtxLst[["sScore0LVr13"]][aIdx,colB]==1 )
            rowVal["Vr24Hpn1A"] <- sum( mmMtxLst[["sScore0LVr24"]][aIdx,colA]==1 )
            rowVal["Vr24Hpn1B"] <- sum( mmMtxLst[["sScore0LVr24"]][aIdx,colB]==1 )

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMRAVe_hpn1AB" # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAe13","sScore0LAe24","sScore0LVe24","sScore0LVe24")
        fltObj$mInfo$cName <- c( "Ae13Hpn1A" ,"Ae13Hpn1B" ,"Ae24Hpn1A" ,"Ae24Hpn1B" 
                                ,"Ve13Hpn1A" ,"Ve13Hpn1B" ,"Ve24Hpn1A" ,"Ve24Hpn1B" 
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName


            # colA1 colA2 colA3 colA4 colA5 colA6 colB1 colB2 colB3 colB4 colB5 colB6
            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Ae13Hpn1A"] <- sum( mmMtxLst[["sScore0LAe13"]][aIdx,colA]==1 )
            rowVal["Ae13Hpn1B"] <- sum( mmMtxLst[["sScore0LAe13"]][aIdx,colB]==1 )
            rowVal["Ae24Hpn1A"] <- sum( mmMtxLst[["sScore0LAe24"]][aIdx,colA]==1 )
            rowVal["Ae24Hpn1B"] <- sum( mmMtxLst[["sScore0LAe24"]][aIdx,colB]==1 )
            rowVal["Ve13Hpn1A"] <- sum( mmMtxLst[["sScore0LVe13"]][aIdx,colA]==1 )
            rowVal["Ve13Hpn1B"] <- sum( mmMtxLst[["sScore0LVe13"]][aIdx,colB]==1 )
            rowVal["Ve24Hpn1A"] <- sum( mmMtxLst[["sScore0LVe24"]][aIdx,colA]==1 )
            rowVal["Ve24Hpn1B"] <- sum( mmMtxLst[["sScore0LVe24"]][aIdx,colB]==1 )

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMRAVc_hpn1AB" # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAc13","sScore0LAc24","sScore0LVc24","sScore0LVc24")
        fltObj$mInfo$cName <- c( "Ac13Hpn1A" ,"Ac13Hpn1B" ,"Ac24Hpn1A" ,"Ac24Hpn1B" 
                                ,"Vc13Hpn1A" ,"Vc13Hpn1B" ,"Vc24Hpn1A" ,"Vc24Hpn1B" 
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName


            # colA1 colA2 colA3 colA4 colA5 colA6 colB1 colB2 colB3 colB4 colB5 colB6
            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Ac13Hpn1A"] <- sum( mmMtxLst[["sScore0LAc13"]][aIdx,colA]==1 )
            rowVal["Ac13Hpn1B"] <- sum( mmMtxLst[["sScore0LAc13"]][aIdx,colB]==1 )
            rowVal["Ac24Hpn1A"] <- sum( mmMtxLst[["sScore0LAc24"]][aIdx,colA]==1 )
            rowVal["Ac24Hpn1B"] <- sum( mmMtxLst[["sScore0LAc24"]][aIdx,colB]==1 )
            rowVal["Vc13Hpn1A"] <- sum( mmMtxLst[["sScore0LVc13"]][aIdx,colA]==1 )
            rowVal["Vc13Hpn1B"] <- sum( mmMtxLst[["sScore0LVc13"]][aIdx,colB]==1 )
            rowVal["Vc24Hpn1A"] <- sum( mmMtxLst[["sScore0LVc24"]][aIdx,colA]==1 )
            rowVal["Vc24Hpn1B"] <- sum( mmMtxLst[["sScore0LVc24"]][aIdx,colB]==1 )

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

mfName <- "bsMRAVf_hpn1AB" # cfg 생성할 것.
if( FALSE ){
    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("sScore0LAf13","sScore0LAf24","sScore0LVf24","sScore0LVf24")
        fltObj$mInfo$cName <- c( "Af13Hpn1A" ,"Af13Hpn1B" ,"Af24Hpn1A" ,"Af24Hpn1B" 
                                ,"Vf13Hpn1A" ,"Vf13Hpn1B" ,"Vf24Hpn1A" ,"Vf24Hpn1B" 
        )

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        fltObj$getScore <- function( mmMtxLst ,mmEMtxLst ,aIdx ){

            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            # colA1 colA2 colA3 colA4 colA5 colA6 colB1 colB2 colB3 colB4 colB5 colB6
            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Af13Hpn1A"] <- sum( mmMtxLst[["sScore0LAf13"]][aIdx,colA]==1 )
            rowVal["Af13Hpn1B"] <- sum( mmMtxLst[["sScore0LAf13"]][aIdx,colB]==1 )
            rowVal["Af24Hpn1A"] <- sum( mmMtxLst[["sScore0LAf24"]][aIdx,colA]==1 )
            rowVal["Af24Hpn1B"] <- sum( mmMtxLst[["sScore0LAf24"]][aIdx,colB]==1 )
            rowVal["Vf13Hpn1A"] <- sum( mmMtxLst[["sScore0LVf13"]][aIdx,colA]==1 )
            rowVal["Vf13Hpn1B"] <- sum( mmMtxLst[["sScore0LVf13"]][aIdx,colB]==1 )
            rowVal["Vf24Hpn1A"] <- sum( mmMtxLst[["sScore0LVf24"]][aIdx,colA]==1 )
            rowVal["Vf24Hpn1B"] <- sum( mmMtxLst[["sScore0LVf24"]][aIdx,colB]==1 )

            return( rowVal )
        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,bsScoreMtxCfg[[mName]]$fCol )["lev",]
                }
                mmEMtxLst[[mName]] <- eMtx
            }

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName
            if( !fltObj$available ) return( rMtx )

            for( aIdx in seq_len(nrow(rMtx)) ){
                rMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,mmEMtxLst ,aIdx )
            }
            rownames(rMtx) <- rownames(mmMtxLst[[1]])

            return( rMtx )
        }

        return(fltObj)
    }
}

