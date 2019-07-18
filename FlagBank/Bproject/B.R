source("B_H.R")
lastH <- 859
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))


# Remove before flight
#   B_H.R   -  경고! 실제 동작에서는 Q_RBF 해제할 것.
hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst )
byFCol <- B.getHMtxLst_byFCol( hMtxLst )

save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
#   load(sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",lastH))

B.rptHMtxLst( hMtxLst )

if(FALSE){
    for( hIdx in (lastH-10:0) ){
        hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=hIdx )
        save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
    }
}
