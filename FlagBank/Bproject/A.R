source("A_H.R")
workH <- 860
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",workH-1) )
load(sprintf("./save/Obj_fRstLstZ%d.save",workH-1) )
load(sprintf("./save/Obj_gEnvZ%d.save",workH-1))

stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )
filter.grp <- getFilter.grp( stdMI.grp )

if(FALSE){
    stdZoid <- c( 8,22,35,38,39,41)    
    aZoidMtx <- matrix( stdZoid ,nrow=1 )
    # 문제가 있다. allIdx를 넘겼어야 한다.
    scoreMtx.grp <- getScoreMtx.grp( aZoidMtx )
}

#   create fMtxLst
#   loading fmFiltLst config 


for( allZoidGrpName in c("allZoid.idx0","allZoid.idx1","allZoid.idx2") ){

    allIdx <- allIdxLst[[allZoidGrpName]]		# allIdx <- c( allIdxLst[["allZoid.idx0"]] ,allIdxLst[["allZoid.idx1"]])
    if( FALSE ) allIdx <- sample(200000:800000,20,replace=F)  # 기능 점검 시 사용.

    # User Define Filtering.

    #   fmFiltLst : allZoidGrpName에 맞는 fMtx의 filter 생성.

    #   stdAssessAllZoid( gEnv ,allIdx ,workH ,fMtxLst ,fmFiltLst )

}



#   fMtx에 대해서만 처리.
stdAssessAllZoid <- function( gEnv ,allIdx ,fMtxLst ,fmFiltLst ,workH=NULL ,simul=F ){
    #   simul : T이면 allIdx들에 대한 필터링 결과만 반환. F이면 실제 삭제.
    #   fmFiltLst : fMtx들에 대한 




}   # stdAssessAllZoid()


