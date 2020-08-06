#   multi-filter



bFMtxMFltLst <- list()

mfName <- "mfABCD"
if( TRUE ){

    bFMtxMFltLst[[mfName]] <- function( scoreMtxLst ,tgt.scMtx ){
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

        fltObj$thldMtx <- matrix( c(    1,2,2,2 ,NA,NA,NA,NA   # "aMHpn","aNHpn"
                                       ,2,3,3,3 ,2,3,3,3 ,1,2,2,2
                                    )
                            ,nrow=length(fltObj$mInfo$cName) ,ncol=length(fltObj$fltMNames)
                            dimmnames=list( fltObj$mInfo$cName, fltObj$fltMNames )
        )

        fltObj$getScore <- function( mmMtxLst ,aIdx ){
            rowVal <- rep( 0 ,length(fltObj$mInfo$cName) )
            names(rowVal) <- fltObj$mInfo$cName

            val <- c( "scoreA"=0 ,"scoreB"=0 ,"scoreC"=0 ,"scoreD"=0 )

            cName <- "aMHpn"    # 실행속도 땜시... 차라리 이렇게 풀어서 쓴다.
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 1
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 2
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 2
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 2
            rowVal[cName] <- sum( val )

            cName <- "aNHpn"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName]
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName]
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName]
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName]
            rowVal[cName] <- sum( val )


            cName <- "paaAH1"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum( val )

            cName <- "paaAH2"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum( val )

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
            rowVal[cName] <- sum( val )

            cName <- "pabbAH2"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] >= 2
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] >= 3
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] >= 3
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] >= 3
            rowVal[cName] <- sum( val )

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
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 0
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 0 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 0 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 0 
            rowVal[cName] <- sum( val )

            cName <- "pbabA"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 0
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 0 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 0 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 0 
            rowVal[cName] <- sum( val )

            cName <- "pabxbA"
            val["scoreA"] <- mmMtxLst[["scoreA"]][aIdx,cName] > 0
            val["scoreB"] <- mmMtxLst[["scoreB"]][aIdx,cName] > 0 
            val["scoreC"] <- mmMtxLst[["scoreC"]][aIdx,cName] > 0 
            val["scoreD"] <- mmMtxLst[["scoreD"]][aIdx,cName] > 0 
            rowVal[cName] <- sum( val )

            
            rowVal["pNearSum"] <- sum( rowVal[c("pbbaA","pbabA","pabxbA")] )
            rowVal["pNearSum"] <- ifelse( rowVal["pNearSum"]>2 ,rowVal["pNearSum"]-2 ,0 )

            return( rowVal )

            # for( nIdx in names(mmMtxLst) ){   # 테스트 데이터 생성용.
            #     for( aIdx in 1:10 ){
            #         mmMtxLst[[nIdx]][aIdx,] <- sample(1:5,length(mmMtxLst[[nIdx]][aIdx,]),replace=T)
            #     }
            # }
            
        }

        # tgt.scMtx가 fltObj$fltMNames 를 모두 포함하고 있는 지 체크.
        if( !is.null(tgt.scMtx) ){
            #   scoreMtxLst는 체크하지 않는다. 차라리 나중에 에러나는 게 인지하기 쉬우므로.            
            mFlag <- fltObj$fltMNames %in% tgt.scMtx
            fltObj$available <- ifelse( all(mFlag) ,fltObj$available ,FALSE )
        }

        if( fltObj$available ){
            mmMtxLst <- lapply(scoreMtxLst[fltObj$fltMNames],function(p){p$scoreMtx})
            hSize <- nrow(mmMtxLst[[1]])

            scrMtx <- matrix( 0 ,nrow=hSize ,ncol=length(fltObj$mInfo$cName) 
                        ,dimnames=list( rownames(mmMtxLst[[1]]) ,fltObj$mInfo$cName )
            )
            for( aIdx in seq_len(hSize) ){
                scrMtx[aIdx,] <- fltObj$getScore( mmMtxLst ,aIdx )
            }

        }

        fltObj$getScoreMtx <- function( scoreMtxLst ){
            # fltObj$available 는 이미 bFCust.getFCustGrp() 에서 cutter 구성 시 확인됨.

            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
            # scoreMtxLst <- scoreMtx.grp$basic[[pName]]

            mmMtxLst <- lapply( scoreMtx.grp$basic ,function( mtxObj ){
                rLst <- lapply( mtxObj[fltObj$fltMNames] ,function(p){ p$scoreMtx })
                return( rLst )
            })

            rMtx <- matrix( 0 ,nrow=nrow(mmMtxLst[[1]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            # for( rIdx in seq_len(nrow(rMtx)) ){
            #     lapply( scrMtxLst ,)
            #     # rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            # }
            # # rownames(rMtx) <- rownames(scoreMtx)

            return( rMtx )
        }

        return(fltObj)
    }

}




