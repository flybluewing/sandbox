source("B_H.R")
lastH <- 859
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))



if(FALSE){

    stdZoid <- c( 8,22,35,38,39,41)
    scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
    
}
