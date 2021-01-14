#     scoreMtxHObj
#     BUtil.makeScoreMtxHObj()

source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")

prllNum <- 7     # ½Ç¼ö°¡ Àæ¾Æ¼­ ±×³É ¿À·ù ÄÚµå·Î ³öµÐ´Ù.
prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd();setwd("..");source("hCommon.R")
        setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
    })
}
sfInit( parallel=T, cpus=prllNum )  ;prll.initHeader( ) ;sfExport("prllLog") 


tgt.scMtx <- NULL       #   13min per H
# tgt.scMtx <- c("score1","score2","sScore01","sScore02")   


#   crScrHTool$initData()
crScrHTool$addData( 860:880 ,tgt.scMtx )    # hSpan=860:880 ,prllNum=7   48min

crScrH <- crScrHTool$getData()