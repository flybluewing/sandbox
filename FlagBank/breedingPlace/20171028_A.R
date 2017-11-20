# 한글한글
library(compiler)
source("20171028_A_H.R")

myObj <- load("c:/zproject/rZoid/Obj_RZP.G1031.save")
RZP.w <- RZP.G
k.FLogStr(sprintf("RZP.w size : %d",RZP.size(RZP.w)),pConsole=T)

myObj <- load("c:/zproject/rZoid/Obj_RZP.G3.1M.save")
#	RZP.G
k.FLogStr(sprintf("RZP.G size : %d",RZP.size()),pConsole=T)

addZoid <- cmpfun( RZP.addZoid )

tStmp <- Sys.time()
procStat <- c(0,0)	;names(procStat)<-c("tot","add")
for( lIdx in seq_len(length(RZP.w$zoidLst)) ){
	for( zIdx in seq_len(length(RZP.w$zoidLst[[lIdx]])) ){
		procStat[1] <- procStat[1] + 1
		if( RZP.addZoid(RZP.w$zoidLst[[lIdx]][[zIdx]]$dna) ){
			procStat[2] <- procStat[2]+1
		}
		if( 0==(procStat[1]%%10000) ){
			tDiff <- Sys.time() - tStmp
			k.FLogStr(sprintf("%.1f %s : proc %d. added %d"
								,tDiff,units(tDiff),procStat[1],procStat[2]
							)
							,pConsole=T ,pTime=F 
						)
		}
	} # for(zIdx)
}
k.FLogStr(sprintf("Final %.1f %s : proc %d. added %d"
					,tDiff,units(tDiff),procStat[1],procStat[2]
				)
				,pConsole=T ,pTime=F 
			)

# intertF[idx] <- RZP.addZoid( sort(sample(1:45,6)) )