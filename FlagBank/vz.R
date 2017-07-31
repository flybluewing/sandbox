source("vz_H.R")


planetZ <- vz.definePlanet()

hisObj <- vz.createHistory( planetZ ,pHisLength=7 )
if( is.null(hisObj$errStr) ){
	planetZ$zoid <- hisObj$zoid
	planetZ$zoid.flt <- hisObj$fltFitByHis
	planetZ$zoid.flt.actMtx <- hisObj$fltFitByHis.mtx
}


