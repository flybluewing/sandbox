#   multi-filter



bFMtxMFltLst <- list()

mfName <- "mfABCD"
if( TRUE ){
    
    bFMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){
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

    bFMtxMFltLst[[mfName]] <- function( tgt.scMtx=NULL ){

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






