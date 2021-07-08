setwd("c:/zproject/ISLRgit/flagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")

lastH <- 960    ;hSpan <- 901:960       # 51min for 30
tgt.scMtx <- NULL

if( TRUE ){
    prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
    prll.initHeader <- function( ){
        k <- sfLapply(1:prllNum,function(prllId){
            curWd <- getwd();setwd("..");source("hCommon.R")
            setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
        })
    }
    prllNum <- 7     # ½Ç¼ö°¡ Àæ¾Æ¼­ ±×³É ¿À·ù ÄÚµå·Î ³öµÐ´Ù.
    sfInit( parallel=T, cpus=prllNum )  ;prll.initHeader( ) ;sfExport("prllLog") 

    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
    names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
    crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)

    logger <- prllLog
    sfExport("lastH")   ;sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
    sfExport("crScrH")  ;sfExport("crScrHTool")    
    sfExport("tgt.scMtx")	;sfExport("logger")
}

tStmp <- Sys.time()
resultLst <- sfLapply( hSpan ,function( curHIdx ){
    tStmp.sf <- Sys.time()
    idStr <- as.character(curHIdx)
    stdZoid <- gEnv$zhF[curHIdx,]
    stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )

    wLastH <- curHIdx-1
    wLastSpan <- 1:which(names(fRstLst)==wLastH)
    gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
    allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
    fRstLst.w <- fRstLst[wLastSpan]

    curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )
    return( list(hIdx=curHIdx ,stdIdx=stdIdx ,curHMtxLst=curHMtxLst ,tDiff=Sys.time()-tStmp.sf) )
})
tDiff <- Sys.time() - tStmp ;tDiff

hMtxLstBig.w <- resultLst
names(hMtxLstBig.w) <- sapply(resultLst,function(p){p$hIdx})

myObj <- load( "./save/Obj_hMtxLstBig.save" )

hMtxLstBig <- append( hMtxLstBig ,hMtxLstBig.w )
save( hMtxLstBig ,file="./save/Obj_hMtxLstBig.save")

