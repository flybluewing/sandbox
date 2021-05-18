#     scoreMtxHObj
#     BUtil.makeScoreMtxHObj()

source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")

prllNum <- 7     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
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


#   경고 :  1. hSpan 마지막 hIdx에 대한 allIdxLst가 존재해야 한다.
#           2. 모든 mName에 대한 configBasic들이 존재해야 한다.
#   crScrHTool$initData()       # bUtil.R 의 BUtil.makeCrScrHTool( ) 참고.
crScrHTool$addData( 730:955 ,tgt.scMtx ,prllLog )    # hSpan=860:880 ,prllNum=7   48 min
                                            # hSpan=730:940 ,prllNum=7  9.6 hours

crScrH <- crScrHTool$getData()
