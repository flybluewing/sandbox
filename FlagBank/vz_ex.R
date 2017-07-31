source("vz_H.R")
source("vz_PF_H.R")

set.seed(42)


# =====================================================================================
#	vz.planetEnvF001() 테스트 샘플.
fZoid <- c(  3 ,4 ,7 ,9 ,10 )
pZh <- matrix( fZoid ,nrow=1 )

fObj <- vz.planetEnvF001( pPreC=c(1,5)	,pReq=c(3,7) ,pPreLogic="or" ,pReqLogic="or" )

condObj <- fObj$isFCond( pZh ,fObj )
sprintf("condObj: %s --> %s",condObj$fIdStr ,condObj$condF)

rZoid <- c(  1 ,5 ,8 ,9 ,10 )
# 주의(!) : fObj$reqCond() 에서는 condObj$condF 값을 체크하지 않는다.
reqObj <-fObj$reqCond( rZoid ,fObj ,condObj )
sprintf("reqObj: %s --> %s",reqObj$fIdStr ,reqObj$reqF)

# =====================================================================================
#	vz.definePlanet() 테스트 샘플.
planetZ <- vz.definePlanet()

k.FLogStr("Start vz.createHistory()",pConsole=T)
hisObj <- vz.createHistory( planetZ ,pHisLength=1000 )
k.FLogStr("Finish vz.createHistory()",pConsole=T)
if( is.null(hisObj$errStr) ){
	planetZ$zoid <- hisObj$zoid
	planetZ$zoid.flt <- hisObj$fltFitByHis
	planetZ$zoid.flt.actMtx <- hisObj$fltFitByHis.mtx
}

save( planetZ ,file="Obj_planetZ1000.save")

