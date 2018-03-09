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


banObj <- getCFltObj( gEnv )
allZoidMtx <- gEnv$zhF
codeLst <- banObj$getCodeLst( allZoidMtx )
# 개발 샘플 시점.

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

tStmp <- Sys.time()
filtedIdxObj <- banObj$getFiltedIdx( allZoidMtx )
filtedIdx <- unique( filtedIdxObj$filtedIdx.dupRow ,filtedIdxObj$filtedIdx.cf1 )
    # filtedIdxObj$filtedIdx.dupRow 에게 모두 파묻히는 거 같은데.. 뭔가 수상타?
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================


#   동일한 패턴이 여러 H동안 나타났을 때, 다음 H에도 동일하게 나타날 것인가?
#       단 한번씩 건너뛴 H에서의 패턴을 다룸.
#       pLevel : "hard","mid","easy"
#  	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pLevel="mid"   ;pDebug=F
ban.throughH2 <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pLevel="hard" ,pDebug=F ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    banFlagLst <- list( )
    for( depthIdx in 2:5 ){
        # 현재 pBanObj$encVal.len이 742라면 다음 차례는 743. 
        #   따라서 과거 패턴은 739, 741 스텝을 밟는다.
        stepSpan <- 2*(depthIdx:1)
        codeSearchSpan <- (pBanObj$encVal.len+1) - stepSpan

        encValLst <- lapply( pBanObj$encValLst ,function(p){p[codeSearchSpan]})

        thld <- sapply( pBanObj$cfObjLst ,function(cfObj){
                            cfObj$throughHisMtx[
                                depthIdx==cfObj$throughHisMtx[,"depth"]
                                ,pLevel]
                        })
        names(thld) <- pBanObj$cfNames

        for( cfName in pBanObj$cfNames ){

            if( cfName %in% c("A0080","A0090") ){
                # "A0080", "A0090"에서는 0000 상태가 많아 폭주발생위험있음.
                if( all(c(0,0,0,0)==encValLst[[cfName]][[1]]) ){
                    next
                }
            }

            dupFlag <- rep( TRUE ,length(encValLst[[cfName]][[1]]) )
            for( codeIdx in 2:length(encValLst[[cfName]]) ){
                dupFlag <- dupFlag & encValLst[[cfName]][[1]]==encValLst[[cfName]][[codeIdx]]
            }
            if( thld[cfName]<=sum(dupFlag) ){
                banFlagObj <- list(depth=depthIdx,cfName=cfName,thldSize=thld[cfName])
                banFlagObj$rawCode <- encValLst[[cfName]][[1]]
                banFlagObj$rawCode.Idx <- codeSearchSpan[1]
                banFlagObj$dupFlag <- dupFlag
                banFlagObj$rawCode.chk <- banFlagObj$rawCode[dupFlag]

                banFlagLst[[1+length(banFlagLst)]] <- banFlagObj
            }
        } # cfName
    } # depthIdx

    filtLst <- list()
    for( pIdx in 1:nrow(pZoidMtx) ){
        bfIdxLst <- list()  # idx for banFlagLst
        for( banIdx in seq_len(length(banFlagLst)) ){
            banFlagObj <- banFlagLst[[banIdx]]
            chkCode <- pCodeLst[[banFlagObj$cfName]][[pIdx]][banFlagObj$dupFlag]
            if( all(banFlagObj$rawCode.chk==chkCode) ){
                bfIdxLst[[1+length(bfIdxLst)]] <- banIdx
            }
        } # banIdx
        filtLst[[1+length(filtLst)]] <- bfIdxLst
    }
    filtedIdx <- which( sapply(filtLst,length) > 0 )

    rstObj <- list( idStr="throughH2" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
    rstObj$level        <- pLevel
    rstObj$banFlagLst     <- banFlagLst
    return( rstObj )

} # ban.throughH2()

