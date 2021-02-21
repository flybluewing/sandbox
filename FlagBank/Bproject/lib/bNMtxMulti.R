#   multi-filter



bNMtxMFltLst <- list()

mfName <- "mfABCD"
if( TRUE ){
    
    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){
        # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("scoreA","scoreB","scoreC","scoreD")
        fltObj$mInfo$cName <- c( "aMHpn","aNHpn"
                                ,"paaAH1","paaAH2","paaAH3","paaAHn"
                                ,"pabbAH1","pabbAH2","pabbAH3","pabbAHn"
                                ,"pbbaA" ,"pbabA" ,"pabxbA"
                                ,"pNearSum" # ("pbbaA"+"pbabA"+"pabxbA") -3
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

            # B,C는 동일 발생이 자주 일어나는 듯.
            val <- c( "scoreA"=0 ,"scoreB"=0 ,"scoreC"=0 ,"scoreD"=0 )

            cName <- "aMHpn"    # 실행속도 땜시... 차라리 이렇게 풀어서 쓴다.
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 2
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 2
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 2
            rowVal[cName] <- sum( val )

            cName <- "aNHpn"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 0
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 0
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 0
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 0
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )


            cName <- "paaAH1"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "paaAH2"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "paaAH3"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 2
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 2
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 2
            rowVal[cName] <- sum( val )

            cName <- "paaAHn"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 1
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 1
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 1
            rowVal[cName] <- sum( val )



            cName <- "pabbAH1"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "pabbAH2"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "pabbAH3"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 2
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 2
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 2
            rowVal[cName] <- sum( val )

            cName <- "pabbAHn"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 1
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 1
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 1
            rowVal[cName] <- sum( val )


            cName <- "pbbaA"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 3 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 3 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 3 
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "pbabA"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 3 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 3 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 3 
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            cName <- "pabxbA"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 3 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 3 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 3 
            rowVal[cName] <- sum(val[c("scoreA","scoreD")]) + any( 0<val[c("scoreB","scoreC")] )

            
            rowVal["pNearSum"] <- sum( rowVal[c("pbbaA","pbabA","pabxbA")] )
            rowVal["pNearSum"] <- ifelse( rowVal["pNearSum"]>2 ,rowVal["pNearSum"]-2 ,0 )

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

mfName <- "mf4567"
if( TRUE ){

    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("score4","score5","score6","score7")
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

            val <- c( "score4"=0 ,"score5"=0 ,"score6"=0 ,"score7"=0 )

            cName <- "pBanN.r"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pBanN.n"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pLCol"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pE3"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pE4"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pMH"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "pfNum"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "iBanN"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 2
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )


            cName <- "iLCol"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "iE3"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "iE4"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "iMH"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "ifNum"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "FVa.m"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "FVa.c"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 2
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )

            cName <- "m4"
            val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
            val["score5"] <- mmMtxLst[["score5"]][aIdx,cName] >= 1
            val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
            val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
            rowVal[cName] <- sum(val[c("score6","score7")]) + any( 0<val[c("score4","score5")] )


            cName <- "xBan.x"
            rowVal[cName]   <- sum( rowVal[c("pBanN.r","pBanN.n","iBanN")]  )
            if( 1 >= rowVal[cName] )  rowVal[cName] <- 0    # 원본 컬럼들과의 중복 방지.

            cName <- "xLCol"
            if( TRUE ){   # score4,6,7에 대해서만 체크. (score5는 너무 빈번함.)
                val <- c( "score4"=0,"score6"=0 ,"score7"=0 )
                accVal <- c("pLCol"=0,"iLCol"=0)

                cName <- "pLCol"
                val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
                val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
                val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "iLCol"
                val["score4"] <- mmMtxLst[["score4"]][aIdx,cName] >= 1
                val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
                val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
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
            if( TRUE ){   # score6,7에 대해서만 체크. (score4,5는 너무 빈번함.)
                val <- c( "score6"=0 ,"score7"=0 )
                accVal <- c("FVa.m"=0,"FVa.c"=0,"m4"=0)

                cName <- "FVa.m"
                val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
                val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "FVa.c"
                val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
                val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
                accVal[cName] <- sum(val)

                cName <- "m4"
                val["score6"] <- mmMtxLst[["score6"]][aIdx,cName] >= 1
                val["score7"] <- mmMtxLst[["score7"]][aIdx,cName] >= 1
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

mfName <- "mfFreqVal1234"
if( TRUE ){

    #   score1 ,score2 의 raw 중복이 많아서 freqVal
    #   --> score2 컬럼들에 대해 모두 freqVal 처리를 해서 다른 컬럽들의 발생에 반응하도록 하자.
    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c( "score1","score2","score3","score4" )
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

            rowVal["s1rem0.len.tot"]<- mmMtxLst[["score1"]][aIdx,"rem0.len.tot"]
            rowVal["s1rem1.len.tot"]<- mmMtxLst[["score1"]][aIdx,"rem1.len.tot"]
            rowVal["s1c0.len.tot"]  <- mmMtxLst[["score1"]][aIdx,"c0.len.tot"]
            rowVal["s1c1.len.tot"]  <- mmMtxLst[["score1"]][aIdx,"c1.len.tot"]
            rowVal["s1f0.len.tot"]  <- mmMtxLst[["score1"]][aIdx,"f0.len.tot"]
            rowVal["s1f1.len.tot"]  <- mmMtxLst[["score1"]][aIdx,"f1.len.tot"]
            rowVal["s1zwNum"]       <- mmMtxLst[["score1"]][aIdx,"zwNum"]

            rowVal["s2rebV.r"]  <- mmMtxLst[["score2"]][aIdx,"rebV.r"]
            rowVal["s2rebC.r"]  <- mmMtxLst[["score2"]][aIdx,"rebC.r"]
            rowVal["s2rebC.c"]  <- mmMtxLst[["score2"]][aIdx,"rebC.c"]
            rowVal["s2rebC.f"]  <- mmMtxLst[["score2"]][aIdx,"rebC.f"]
            rowVal["s2rebC2.r"] <- mmMtxLst[["score2"]][aIdx,"rebC2.r"]
            rowVal["s2rebC2.c"] <- mmMtxLst[["score2"]][aIdx,"rebC2.c"]
            rowVal["s2rebC2.f"] <- mmMtxLst[["score2"]][aIdx,"rebC2.f"]
            rowVal["s2inc.r"]   <- mmMtxLst[["score2"]][aIdx,"inc.r"]
            rowVal["s2inc.c"]   <- mmMtxLst[["score2"]][aIdx,"inc.c"]
            rowVal["s2inc.f"]   <- mmMtxLst[["score2"]][aIdx,"inc.f"]
            rowVal["s2inc.r2"]  <- mmMtxLst[["score2"]][aIdx,"inc.r2"]
            rowVal["s2inc.c2"]  <- mmMtxLst[["score2"]][aIdx,"inc.c2"]
            rowVal["s2inc.f2"]  <- mmMtxLst[["score2"]][aIdx,"inc.f2"]
            rowVal["s2inc.r3"]  <- mmMtxLst[["score2"]][aIdx,"inc.r3"]
            rowVal["s2inc.c3"]  <- mmMtxLst[["score2"]][aIdx,"inc.c3"]

            rowVal["s3rebPtn.1"] <- mmMtxLst[["score3"]][aIdx,"rebPtn.1"]
            rowVal["s3snMax.r"] <- mmMtxLst[["score3"]][aIdx,"snMax.r"]
            rowVal["s3snMax.c"] <- mmMtxLst[["score3"]][aIdx,"snMax.c"]
            rowVal["s3snMax.f"] <- mmMtxLst[["score3"]][aIdx,"snMax.f"]

            rowVal["s4pBanN.r"] <- mmMtxLst[["score4"]][aIdx,"pBanN.r"]
            rowVal["s4pBanN.n"] <- mmMtxLst[["score4"]][aIdx,"pBanN.n"]
            rowVal["s4pLCol"]   <- mmMtxLst[["score4"]][aIdx,"pLCol"]
            rowVal["s4pE3"]     <- mmMtxLst[["score4"]][aIdx,"pE3"]
            rowVal["s4pfNum"]   <- mmMtxLst[["score4"]][aIdx,"pfNum"]
            rowVal["s4iBanN"]   <- mmMtxLst[["score4"]][aIdx,"iBanN"]
            rowVal["s4iLCol"]   <- mmMtxLst[["score4"]][aIdx,"iLCol"]
            rowVal["s4ifNum"]   <- mmMtxLst[["score4"]][aIdx,"ifNum"]
            rowVal["s4FVa.m"]   <- mmMtxLst[["score4"]][aIdx,"FVa.m"]

            return( rowVal )

        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,scoreMtxCfg[[mName]]$fCol )["lev",]
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


mfName <- "mfFreqVal89"
if( TRUE ){     # cfg 생성할 것.

    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("score8","score9")
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

            rowVal["s8_c21"]        <- mmMtxLst[["score8"]][aIdx,"c21"]
            rowVal["s8_c22"]        <- mmMtxLst[["score8"]][aIdx,"c22"]
            rowVal["s8_c23"]        <- mmMtxLst[["score8"]][aIdx,"c23"]
            rowVal["s8_c24"]        <- mmMtxLst[["score8"]][aIdx,"c24"]
            rowVal["s8_c25"]        <- mmMtxLst[["score8"]][aIdx,"c25"]
            rowVal["s8_min3"]       <- mmMtxLst[["score8"]][aIdx,"min3"]
            rowVal["s8_min2MatCnt"] <- mmMtxLst[["score8"]][aIdx,"min2MatCnt"]

            rowVal["s9_rCnt"]       <- mmMtxLst[["score9"]][aIdx,"rCnt"]
            rowVal["s9_eCnt"]       <- mmMtxLst[["score9"]][aIdx,"eCnt"]
            rowVal["s9_cCnt"]       <- mmMtxLst[["score9"]][aIdx,"cCnt"]
            rowVal["s9_fCnt"]       <- mmMtxLst[["score9"]][aIdx,"fCnt"]

            return( rowVal )

        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,scoreMtxCfg[[mName]]$fCol )["lev",]
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

mfName <- "mfFreqVal259"
if( TRUE ){     # cfg 생성할 것.

    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c("score2","score5","score9")
        fltObj$mInfo$cName <- c( "s2rebV.r" ,"s2rebC.r" ,"s2rebC.c" ,"s2rebC.f" ,"s2rebC2.r" ,"s2rebC2.c" ,"s2rebC2.f"
                                ,"s2inc.r" ,"s2inc.c" ,"s2inc.f" ,"s2inc.r2" ,"s2inc.c2" ,"s2inc.f2" ,"s2inc.r3" ,"s2inc.c3"
                                ,"s5pBanN.r","s5pBanN.n","s5pLCol","s5pE3","s5pfNum","s5iBanN","s5iLCol","s5ifNum","s5FVa.m"
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

            rowVal["s2rebV.r"]  <- mmMtxLst[["score2"]][aIdx,"rebV.r"]
            rowVal["s2rebC.r"]  <- mmMtxLst[["score2"]][aIdx,"rebC.r"]
            rowVal["s2rebC.c"]  <- mmMtxLst[["score2"]][aIdx,"rebC.c"]
            rowVal["s2rebC.f"]  <- mmMtxLst[["score2"]][aIdx,"rebC.f"]
            rowVal["s2rebC2.r"] <- mmMtxLst[["score2"]][aIdx,"rebC2.r"]
            rowVal["s2rebC2.c"] <- mmMtxLst[["score2"]][aIdx,"rebC2.c"]
            rowVal["s2rebC2.f"] <- mmMtxLst[["score2"]][aIdx,"rebC2.f"]
            rowVal["s2inc.r"]   <- mmMtxLst[["score2"]][aIdx,"inc.r"]
            rowVal["s2inc.c"]   <- mmMtxLst[["score2"]][aIdx,"inc.c"]
            rowVal["s2inc.f"]   <- mmMtxLst[["score2"]][aIdx,"inc.f"]
            rowVal["s2inc.r2"]  <- mmMtxLst[["score2"]][aIdx,"inc.r2"]
            rowVal["s2inc.c2"]  <- mmMtxLst[["score2"]][aIdx,"inc.c2"]
            rowVal["s2inc.f2"]  <- mmMtxLst[["score2"]][aIdx,"inc.f2"]
            rowVal["s2inc.r3"]  <- mmMtxLst[["score2"]][aIdx,"inc.r3"]
            rowVal["s2inc.c3"]  <- mmMtxLst[["score2"]][aIdx,"inc.c3"]

            rowVal["s5pBanN.r"] <- mmMtxLst[["score5"]][aIdx,"pBanN.r"]
            rowVal["s5pBanN.n"] <- mmMtxLst[["score5"]][aIdx,"pBanN.n"]
            rowVal["s5pLCol"]   <- mmMtxLst[["score5"]][aIdx,"pLCol"]
            rowVal["s5pE3"]     <- mmMtxLst[["score5"]][aIdx,"pE3"]
            rowVal["s5pfNum"]   <- mmMtxLst[["score5"]][aIdx,"pfNum"]
            rowVal["s5iBanN"]   <- mmMtxLst[["score5"]][aIdx,"iBanN"]
            rowVal["s5iLCol"]   <- mmMtxLst[["score5"]][aIdx,"iLCol"]
            rowVal["s5ifNum"]   <- mmMtxLst[["score5"]][aIdx,"ifNum"]
            rowVal["s5FVa.m"]   <- mmMtxLst[["score5"]][aIdx,"FVa.m"]

            rowVal["s9_rCnt"]       <- mmMtxLst[["score9"]][aIdx,"rCnt"]
            rowVal["s9_eCnt"]       <- mmMtxLst[["score9"]][aIdx,"eCnt"]
            rowVal["s9_cCnt"]       <- mmMtxLst[["score9"]][aIdx,"cCnt"]
            rowVal["s9_fCnt"]       <- mmMtxLst[["score9"]][aIdx,"fCnt"]

            return( rowVal )

        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,scoreMtxCfg[[mName]]$fCol )["lev",]
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

mfName <- "mf9ELecf"
if( TRUE ){

    bNMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){
        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName <- mfName
        fltObj$available <- TRUE    # bFCust.getFCustGrp() 에서 확인됨.

        fltObj$fltMNames <- c( "score9" ,"scoreE"
                            ,"scoreLAe13","scoreLAe24","scoreLVe24","scoreLVe24"
                            ,"scoreLAf13","scoreLAf24","scoreLVf24","scoreLVf24"
                            ,"scoreLAf13","scoreLAf24","scoreLVf24","scoreLVf24"
        )
        fltObj$mInfo$cName <- c( "rCnt" ,"eCnt" ,"cCnt" ,"fCnt" ,"cH0MLen" ,"cH1MLen" ,"cH2MLen"
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

            rowVal["rCnt"] <- mmMtxLst[["score9"]][aIdx,"rCnt"]
            rowVal["eCnt"] <- mmMtxLst[["score9"]][aIdx,"eCnt"]
            rowVal["cCnt"] <- mmMtxLst[["score9"]][aIdx,"cCnt"]
            rowVal["fCnt"] <- mmMtxLst[["score9"]][aIdx,"fCnt"]

            rowVal["cH0MLen"] <- mmMtxLst[["scoreE"]][aIdx,"cH0MLen"]
            rowVal["cH1MLen"] <- mmMtxLst[["scoreE"]][aIdx,"cH1MLen"]
            rowVal["cH2MLen"] <- mmMtxLst[["scoreE"]][aIdx,"cH2MLen"]


            # colA1 colA2 colA3 colA4 colA5 colA6 colB1 colB2 colB3 colB4 colB5 colB6
            colA <- c( "colA1" ,"colA2" ,"colA3" ,"colA4" ,"colA5" ,"colA6" )
            colB <- c( "colB1" ,"colB2" ,"colB3" ,"colB4" ,"colB5" ,"colB6" )

            rowVal["Ae13Hpn1A"] <- sum( mmMtxLst[["scoreLAe13"]][aIdx,colA]==1 )
            rowVal["Ae13Hpn1B"] <- sum( mmMtxLst[["scoreLAe13"]][aIdx,colB]==1 )
            rowVal["Ae24Hpn1A"] <- sum( mmMtxLst[["scoreLAe24"]][aIdx,colA]==1 )
            rowVal["Ae24Hpn1B"] <- sum( mmMtxLst[["scoreLAe24"]][aIdx,colB]==1 )
            rowVal["Ve13Hpn1A"] <- sum( mmMtxLst[["scoreLVe13"]][aIdx,colA]==1 )
            rowVal["Ve13Hpn1B"] <- sum( mmMtxLst[["scoreLVe13"]][aIdx,colB]==1 )
            rowVal["Ve24Hpn1A"] <- sum( mmMtxLst[["scoreLVe24"]][aIdx,colA]==1 )
            rowVal["Ve24Hpn1B"] <- sum( mmMtxLst[["scoreLVe24"]][aIdx,colB]==1 )

            rowVal["Ac13Hpn1A"] <- sum( mmMtxLst[["scoreLAc13"]][aIdx,colA]==1 )
            rowVal["Ac13Hpn1B"] <- sum( mmMtxLst[["scoreLAc13"]][aIdx,colB]==1 )
            rowVal["Ac24Hpn1A"] <- sum( mmMtxLst[["scoreLAc24"]][aIdx,colA]==1 )
            rowVal["Ac24Hpn1B"] <- sum( mmMtxLst[["scoreLAc24"]][aIdx,colB]==1 )
            rowVal["Vc13Hpn1A"] <- sum( mmMtxLst[["scoreLVc13"]][aIdx,colA]==1 )
            rowVal["Vc13Hpn1B"] <- sum( mmMtxLst[["scoreLVc13"]][aIdx,colB]==1 )
            rowVal["Vc24Hpn1A"] <- sum( mmMtxLst[["scoreLVc24"]][aIdx,colA]==1 )
            rowVal["Vc24Hpn1B"] <- sum( mmMtxLst[["scoreLVc24"]][aIdx,colB]==1 )

            rowVal["Af13Hpn1A"] <- sum( mmMtxLst[["scoreLAf13"]][aIdx,colA]==1 )
            rowVal["Af13Hpn1B"] <- sum( mmMtxLst[["scoreLAf13"]][aIdx,colB]==1 )
            rowVal["Af24Hpn1A"] <- sum( mmMtxLst[["scoreLAf24"]][aIdx,colA]==1 )
            rowVal["Af24Hpn1B"] <- sum( mmMtxLst[["scoreLAf24"]][aIdx,colB]==1 )
            rowVal["Vf13Hpn1A"] <- sum( mmMtxLst[["scoreLVf13"]][aIdx,colA]==1 )
            rowVal["Vf13Hpn1B"] <- sum( mmMtxLst[["scoreLVf13"]][aIdx,colB]==1 )
            rowVal["Vf24Hpn1A"] <- sum( mmMtxLst[["scoreLVf24"]][aIdx,colA]==1 )
            rowVal["Vf24Hpn1B"] <- sum( mmMtxLst[["scoreLVf24"]][aIdx,colB]==1 )

            return( rowVal )

        }
        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtxLst[fltObj$fltMNames] ,function(mtxObj){mtxObj$scoreMtx} )
            mmEMtxLst <- list()
            for( mName in fltObj$fltMNames ){
                eMtx <- mmMtxLst[[mName]]
                eMtx[,] <- NA
                for( rIdx in seq_len(nrow(eMtx)) ){
                    eMtx[rIdx,] <- bFCust.getEvt( mmMtxLst[[mName]][rIdx,] ,scoreMtxCfg[[mName]]$fCol )["lev",]
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

