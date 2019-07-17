source("B_H.R")
lastH <- 859
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))

hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst )
byFCol <- B.getHMtxLst_byFCol( hMtxLst$scoreMtxLst )

save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
#   load(sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",lastH))

if(FALSE){
    for( hIdx in (lastH-10:0) ){
        hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=hIdx )
        save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
    }
}
