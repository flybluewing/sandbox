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

mfName <- "bsMRLArn"
if( FALSE ){

    bSMtxRMLst[[mfName]] <- function( tgt.scMtx=NULL ){

        fltMNames = c("sScore0LAr13","sScore0LAr24")
        fltObj <- bS.get_bSMtxRM_bsMRLXxn( tgt.scMtx=tgt.scMtx ,fltMNames=fltMNames )

        return(fltObj)

    }

}

