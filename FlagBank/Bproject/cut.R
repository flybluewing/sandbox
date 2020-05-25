source("header.r")
source("B_H.R")
lastH <- 912

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))

#-[Parallel init work]-------------------------------------------------------------
prllNum <- 3     # ½Ç¼ö°¡ Àæ¾Æ¼­ ±×³É ¿À·ù ÄÚµå·Î ³öµÐ´Ù.
prllLog <- k.getFlogObj( "./log/parallel_log.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        source("header.r")
        source("B_H.R")
    })
}

sfInit( parallel=T, cpus=prllNum )
sfExport("prllLog") ;sfExport("lastH")
sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
prll.initHeader( )
prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))


tStmp <- Sys.time()
stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )     ;stdMI.grp$anyWarn( )

