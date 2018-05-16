# 20180109_C_HRad_val.R loose 필터 검증용.

loose.combTest <- function( gEnv ,allIdxLst ){

    testSpan <- as.integer(names(allIdxLst$stdFiltedCnt))
    testSpan.n0 <- as.integer(allIdxLst$stdFiltedCnt.n0)
    testSpan.n1 <- as.integer(allIdxLst$stdFiltedCnt.n1)

    allIdx <- allIdxLst$allZoid.idx0
    allIdx <- allIdx[ gEnv$allZoidMtx[allIdx,1] %in% c(4) ]

    tStmp <- Sys.time()
    rstLst <- vector("list",length(testSpan))   ;names(rstLst)<-as.character(testSpan)
    for( tIdx in testSpan ){
        tEnv <- gEnv
        tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
        allZoidMtx <- gEnv$zhF[tIdx,,drop=F]
        banObj <- getCFltObj( tEnv )
        banCmbObj <- getCFltCmbObj( gEnv )

        filted <- character(0)

        bRstObj <- loose.ban.colValSeqNext( tEnv$zhF ,allZoidMtx ,pLevel=2 )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.colValSeqNext"

        colPtnLst <- anaColEndPtn( gEnv$zhF )
        bRstObj <- loose.ban.linePtn.reb( colPtnLst ,allZoidMtx ,pLevel=2 ,pDebug=T )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.linePtn.reb"

        codeLst <- banObj$getCodeLst( allZoidMtx )
        bRstObj <- loose.ban.hntSameRow(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.hntSameRow(getCFltObj)"
        bRstObj <- loose.ban.hntCrossDim(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.hntCrossDim(getCFltObj)"
        #   bRstObj <- loose.ban.multiDim(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        #   if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.multiDim(getCFltObj)"
        bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="easy" )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "ban.throughH(getCFltObj)"
        bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="easy" )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "ban.throughH2(getCFltObj)"

        codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )
        #   bRstObj <- loose.ban.hntSameRow(banCmbObj ,allZoidMtx ,pLevel=1 ,pCodeLst=codeCmbLst)   # 100%나옴
        #   if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.hntSameRow(getCFltCmbObj)"
        bRstObj <- loose.ban.hntCrossDim(banCmbObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeCmbLst)
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.hntCrossDim(getCFltCmbObj)"
        #   bRstObj <- loose.ban.multiDim(banCmbObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeCmbLst)
        #   if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "loose.ban.multiDim(getCFltCmbObj)"
        bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pLevel="easy" )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "ban.throughH(getCFltCmbObj)"
        bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pLevel="easy" )
        if( 0<length(bRstObj$filtedIdx) ) filted[1+length(filted)] <- "ban.throughH2(getCFltCmbObj)"

        rstLst[[as.character(tIdx)]] <- filted
    }
    tDiff <- Sys.time() - tStmp

    tRstLst <- rstLst[ sort(unique(c(allIdxLst$stdFiltedCnt.n0,allIdxLst$stdFiltedCnt.n1))) ]
    # tRstLst <- rstLst
    rstNames <- sort(unique(do.call(c,tRstLst)))
    mtx <- matrix( 0 ,nrow=length(tRstLst) ,ncol=length(rstNames) )
    rownames(mtx) <- attributes(tRstLst)$names   ;colnames(mtx) <- rstNames
    for( idx in seq_len(length(tRstLst)) ){
        mtx[idx,tRstLst[[idx]]] <- mtx[idx,tRstLst[[idx]]] + 1
    }
    round( 100*apply(mtx,2,sum)/nrow(mtx) )
    table(apply(mtx,1,sum))

    combMtx <- combinations(length(rstNames),7)
    minVal <- rep( 0 ,nrow(combMtx) )
    for( rIdx in 1:nrow(combMtx) ){
        rSum <- apply( mtx[,combMtx[rIdx,]] ,1 ,sum )
        minVal[rIdx] <- sum( rSum==0 )
    }
    min(minVal)

    t(apply( combMtx[which(minVal==0),,drop=F] ,1 ,function(comb){rstNames[comb]}))



    save( rstLst ,file="Obj_tempSave.save")

    rstCnt <- sapply(rstLst,length) ;names(rstCnt) <- testSpan
    table( rstCnt )

    table( rstCnt[allIdxLst$stdFiltedCnt.n0] )
    table( rstCnt[allIdxLst$stdFiltedCnt.n1] )


} # loose.combTest()

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

        bRstObj <- loose.ban.multiDim(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)

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
