# 20180109_C.R ±³Â÷¸ðµ¨
#	Code : cf - cross filter
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

hFltCnt <- sapply( fRstLst ,length )
hFlt.cnt1 <- sapply( fRstLst[hFltCnt==1] ,function(p){p[1]} )
#   tblPer.h <- table(hFlt.cnt1)*100/length(hFlt.cnt1)
#   tblPer.h[c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")]

k.idx <- hFlt.cnt1 %in% c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")


# --[allZoidMtx]----------------------------------------------------
azFltLst <- vector("list",nrow(gEnv$allZoidMtx))
for( fnIdx in attributes(remLst)$names ){
    for( zIdx in remLst[[fnIdx]] ){
        azFltLst[[zIdx]][1+length(azFltLst[[zIdx]])] <- fnIdx
    }
}
azFltCnt <- sapply( azFltLst ,length )

azflt.cnt1 <- sapply( azFltLst[which(azFltCnt==1)] ,function(p){p[1]} )
#   tblPer.az <- table(azflt.cnt1)*100/length(azflt.cnt1)
#   tblPer.az[c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")]



