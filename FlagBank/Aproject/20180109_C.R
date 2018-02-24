# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )

allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

# ===============================================================================

getCFltObj <- function( pEnv ){

    cfObjLst <- NULL
    encSpan <- 100:nrow(pEnv$zhF)
    encValLst <- list()
    for( tIdx in encSpan ){	# lastZoid 기반 동작들 때문에 1부터 시작은 의미없다.
        tEnv <- pEnv
        tEnv$zhF <- pEnv$zhF[1:(tIdx-1),]
        tEnv$allZoidMtx <- pEnv$zhF[tIdx,,drop=F]
        
        cfObjLst <- getCFLst.base( tEnv )
        for( lIdx in seq_len(length(cfObjLst)) ){
            cfObj <- cfObjLst[[lIdx]]
            if( is.null(encValLst[[cfObj$idStr]]) ){
                encValLst[[cfObj$idStr]] <- list()
            }
            encValLst[[cfObj$idStr]][[1+length(encValLst[[cfObj$idStr]])]] <- cfObj$enc( tEnv$allZoidMtx )[[1]]
        }
    }
    
    cfNames <- sapply(cfObjLst,function(p){p$idStr})

    encValLst[[1]][[691]] <- encValLst[[1]][[692]]

    encVal.len <- length(encValLst[[1]])

    cfNameMtx <- apply( permutations( length(cfNames) ,2 )
                         ,1 ,function(p){cfNames[p]}
                    )
    cfNameMtx <- t(cfNameMtx)

    banLst <- list()
    for( cfIdx in 1:nrow(cfNameMtx) ){

        sbc.name <- cfNameMtx[cfIdx,1]
        nzc.name <- cfNameMtx[cfIdx,2]
        rstObj <- evlScan( encValLst ,sbc.name ,nzc.name ,cfObjLst )
        if( !is.null(rstObj$lastZC) ){
            banLst[[cfIdx]] <- rstObj$lastZC
        } else {
            banLst[[cfIdx]] <- integer(0)   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
        }
    } # cfIdx

    # Row가 동일 연속 재발하는 빈도 측정
    #   발생빈도를 봐가며 사용해야 할 듯.
    dupRowCnt <- rep( 0 ,length(cfNames) ) ;names(dupRowCnt) <- cfNames
    for( idx in 2:length(encSpan) ) {
        dupFlag <- sapply( seq_len(length(encValLst)) ,function(eIdx){
                        dCnt <- cfObjLst[[eIdx]]$diffCnt( encValLst[[eIdx]][[idx-1]] ,encValLst[[eIdx]][[idx]] )
                        return( !is.na(dCnt) && (0==dCnt) )
                    })
        dupRowCnt <- dupRowCnt + dupFlag
    }
    dupBanLst <- lapply( encValLst ,function(p){p[[length(p)]]})

    rObj <- list( cfNameMtx=cfNameMtx ,banLst=banLst ,cfObjLst=cfObjLst
                    ,dupRowCnt=dupRowCnt )

    return( rObj )

} # getCFltObj()








