# 20171116_C_init_val.R
#	초기화에서 걸러지는 실제 Zoid History들의 목록을 확인하기 위함.
source("20171116_A_H.R")
source("20171116_A_H_cliper.R")
source("20171116_B_H.R")
source("20171116_C_H.R")

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()
FB.f <- getFlagBank("./zoidHistory/ZH_Final.csv")
setwd(curWd)
zh	<- as.matrix( FB$zh )
zhF	<- as.matrix( FB.f$zh )	;rownames(zhF) <- 1:nrow(zhF)
# allZoidMtx <- getAllZoid() # 38sec

#=[missHLst]===============================================================================
#	이 파일에서의 핵심 데이터.
#	사전 필터로 인해 유실되는 실제 Zoid History Index.
missHLst <- list()

# --------------------------------------------------------------
stdCode <- zhF[,6]-zhF[,1]
seqCntMtx <- k.seq(stdCode)$seqCntMtx
rebMtx <- seqCntMtx[seqCntMtx[,"cnt"]>1 ,]

sum(rebMtx[,"val"]<30)

table(rebMtx[,"val"])

