# 20180109_C_HRad_val.R loose 필터 검증용.

loose.funcTest <- function( gEnv ,allIdxLst ){

    testSpan <- as.integer(names(allIdxLst$stdFiltedCnt))
    testSpan.n0 <- as.integer(allIdxLst$stdFiltedCnt.n0)
    testSpan.n1 <- as.integer(allIdxLst$stdFiltedCnt.n1)

    allIdx <- allIdxLst$allZoid.idx0
    allIdx <- allIdx[ gEnv$allZoidMtx[allIdx,1] %in% c(4) ]

    tStmp <- Sys.time()
    rst <- rep( FALSE ,length(testSpan) )   ;names(rst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]

        bRstObj <- loose.ban.colValSeqNext( tEnv$zhF ,allZoidMtx ,pLevel=2 )

        rst[as.character(tIdx)] <- 0<length(bRstObj$filtedIdx)
    }
    tDiff <- Sys.time() - tStmp

    table( rst )
    table( allIdxLst$stdFiltedCnt[rst] )

    table( allIdxLst$stdFiltedCnt )
    table( allIdxLst$stdFiltedCnt ) %/% 4


} # loose.funcTest()


loose.funcTest.getCFltObj <- function( gEnv ,allIdxLst ){

    testSpan <- as.integer(names(allIdxLst$stdFiltedCnt))
    testSpan.n0 <- as.integer(allIdxLst$stdFiltedCnt.n0)
    testSpan.n1 <- as.integer(allIdxLst$stdFiltedCnt.n1)

    allIdx <- allIdxLst$allZoid.idx0
    allIdx <- allIdx[ gEnv$allZoidMtx[allIdx,1] %in% c(4) ]

    tStmp <- Sys.time()
    rst <- rep( FALSE ,length(testSpan) )   ;names(rst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        banObj <- getCFltObj( tEnv )
        codeLst <- banObj$getCodeLst( allZoidMtx )

        bRstObj <- loose.ban.multiDim(banObj ,allZoidMtx ,pLevel=1 ,pCodeLst=codeLst)

        rst[as.character(tIdx)] <- 0<length(bRstObj$filtedIdx)
    }
    tDiff <- Sys.time() - tStmp

    table( rst )
    table( allIdxLst$stdFiltedCnt[rst] )

    table( allIdxLst$stdFiltedCnt )
    table( allIdxLst$stdFiltedCnt ) %/% 4

    # Function combination --------------------------------------------------------
    funcLst <- list()
    funcLst[[1+length(funcLst)]] <- loose.ban.hntSameRow
    funcLst[[1+length(funcLst)]] <- loose.ban.multiDim

    tStmp <- Sys.time()
    rstLst <- list()
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        banObj <- getCFltObj( tEnv )
        codeLst <- banObj$getCodeLst( allZoidMtx )

        rstFlag <- rep( F ,length(funcLst) )
        for( fIdx in seq_len(length(funcLst)) ){
            bRstObj <- funcLst[[fIdx]](banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
            rstFlag[fIdx] <- 0<length(bRstObj$filtedIdx)
        }
    }
    tDiff <- Sys.time() - tStmp

    rstMtx <- do.call( rbind ,rstLst )
    apply( rstMtx ,2 ,mean )
    flag <- apply( rstMtx ,1 ,any ) ;names(flag)<-attributes(rstLst)$names
    table( flag[allIdxLst$stdFiltedCnt.n0] )
    table( flag[allIdxLst$stdFiltedCnt.n1] )


    tStmp <- Sys.time()
    tEnv <- gEnv
    tEnv$zhF <- gEnv$zhF
    allZoidMtx <- gEnv$allZoidMtx[allIdx,]
    banObj <- getCFltObj( tEnv )
    codeLst <- banObj$getCodeLst( allZoidMtx )
    rstMtx <- matrix( F ,ncol=2 ,nrow=nrow(allZoidMtx) )
    for( fIdx in seq_len(length(funcLst)) ){
        bRstObj <- funcLst[[fIdx]](banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        rstMtx[bRstObj$filtedIdx,fIdx] <- TRUE
    }
    tDiff <- Sys.time() - tStmp

    flag <- apply(rstMtx,1,any)    
    table(flag)
    apply( rstMtx,2,mean )

} # loose.funcTest.getCFltObj()


loose.funcTest.anaColEndPtn <- function( gEnv ,allIdxLst ){

    testSpan <- as.integer(names(allIdxLst$stdFiltedCnt))
    testSpan.n0 <- as.integer(allIdxLst$stdFiltedCnt.n0)
    testSpan.n1 <- as.integer(allIdxLst$stdFiltedCnt.n1)

    allIdx <- allIdxLst$allZoid.idx0
    allIdx <- allIdx[ gEnv$allZoidMtx[allIdx,1] %in% c(4) ]

    tStmp <- Sys.time()
    rst <- rep( FALSE ,length(testSpan) )   ;names(rst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        colPtnLst <- anaColEndPtn( gEnv$zhF )

        bRstObj <- loose.ban.linePtn.reb( colPtnLst ,allZoidMtx ,pLevel=2 ,pDebug=T )
    	# colPtnLst[[1]]$val

        rst[as.character(tIdx)] <- 0<length(bRstObj$filtedIdx)
    }
    tDiff <- Sys.time() - tStmp

    table( rst )
    table( allIdxLst$stdFiltedCnt[rst] )

    table( allIdxLst$stdFiltedCnt )
    table( allIdxLst$stdFiltedCnt ) %/% 4



} # loose.funcTest.anaColEndPtn()


loose.funcTest.throughH <- function( gEnv ,allIdxLst ){

    testSpan <- as.integer(names(allIdxLst$stdFiltedCnt))
    testSpan.n0 <- as.integer(allIdxLst$stdFiltedCnt.n0)
    testSpan.n1 <- as.integer(allIdxLst$stdFiltedCnt.n1)

    allIdx <- allIdxLst$allZoid.idx0
    allIdx <- allIdx[ gEnv$allZoidMtx[allIdx,1] %in% c(4) ]

    tStmp <- Sys.time()
    rst <- rep( FALSE ,length(testSpan) )   ;names(rst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        banObj <- getCFltObj( tEnv )
        codeLst <- banObj$getCodeLst( allZoidMtx )

        # "hard" "mid" "easy"
        # bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="easy" )
        bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="easy" )

        rst[as.character(tIdx)] <- 0<length(bRstObj$filtedIdx)
    }
    tDiff <- Sys.time() - tStmp

    table( rst )
    table( allIdxLst$stdFiltedCnt[rst] )

    table( allIdxLst$stdFiltedCnt )
    table( allIdxLst$stdFiltedCnt ) %/% 4

    tStmp <- Sys.time()
    rst <- rep( FALSE ,length(testSpan) )   ;names(rst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        banCmbObj <- getCFltCmbObj( gEnv )
        codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )

        # bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pLevel="easy")
        bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pLevel="loose")

        rst[as.character(tIdx)] <- 0<length(bRstObj$filtedIdx)
    }
    tDiff <- Sys.time() - tStmp

    table( rst )
    table( allIdxLst$stdFiltedCnt[rst] )

    table( allIdxLst$stdFiltedCnt )
    table( allIdxLst$stdFiltedCnt ) %/% 4


} # loose.funcTest()
