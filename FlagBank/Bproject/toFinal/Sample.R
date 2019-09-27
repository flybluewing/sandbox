#   BProject 디렉토리에서 실행한다고 전제.
source("header.r")
source("B_H.R")
source("./lib/FinalCut_H.R")


lastH <- 860

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
load(sprintf("./save/Obj_remLstZ%d.save",lastH) )


# stdCtrlCfgGrp
load("./save/HMtxLst/Obj_stdCtrlCfgGrp_840.save")

aZoidGrp.name <- c("allZoid.idx0" ,"allZoid.idx1" ,"allZoid.idx2")
scoreMtx.name <- c("score2","score3","score4","score5")

for( aZoidGrp in aZoidGrp.name ){   # aZoidGrp <- "allZoid.idx1"

    logger <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s.txt",lastH,aZoidGrp) )
    allIdxF <- allIdxLst[[aZoidGrp]]
    logger$fLogStr(sprintf("start %s(%dk)",aZoidGrp,length(allIdxF)%/%1000),pAppend=F,pTime=T)

    allIdxF <- FC.primaryCut.static( allIdxF ,gEnv )
    #   primaryCut.cust( )

    for( scoreMtx in scoreMtx.name ){

    }
}
