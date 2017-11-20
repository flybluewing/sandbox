# 한글한글
source("20171028_A_H.R")

# ------------------------------------------------------------
#	생성
RZP.G <- getRandomZoidPool()	# Random Zoid Pool - Global

batchSize <- 100000
insertF <- rep( F ,batchSize )
for( gIdx in 1:10 ){
	tStmp <- Sys.time()
	insertF[] <- F
	for( idx in 1:batchSize){
		intertF[idx] <- RZP.addZoid( sort(sample(1:45,6)) )
	}
	tDiff <- Sys.time() - tStmp
	k.FLogStr(sprintf("gIdx:%d  Cost:%.1f%s dup:%d total:%d"
				,gIdx 
				,tDiff ,units(tDiff) 
				,sum(!insertF)
				,RZP.size()
			) ,pConsole=T)

} # for(gIdx)

save( RZP.G ,file="Obj_RZP.G.save" )


# ------------------------------------------------------------
#	병합
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
tStr <- sprintf("%s",Sys.time()) # "2017-11-20 22:47:56"
saveFile <- gsub("^.*-(..)-(..).*","Obj_RZP\\1\\2.save",tStr)
RZP <- RZP.G
save( RZP ,file=saveFile )


# ------------------------------------------------------------
#	제 3 RZP로 병합(동작 확인필요)
myObj <- load("Obj_RZP.G3.1M.save")
RZP.base <- RZP.G

myObj <- load("Obj_RZP.G1.0M.save")
RZP.add <- RZP.G

RZP.G <- getRandomZoidPool()	# 나중에 로딩하여 사용될 수도 있다.

tStamp <- Sys.time()
procStat <- c(0,0)	;names(procStat)<-c("tot","add")
for( lIdx in seq_len(length(RZP.add$zoidLst)) ){
    for( idx in seq_len(length(RZP.add$zoidLst[[lIdx]])) ){
		procStat[1] <- procStat[1] + 1
        if( !RZP.haveZoid(RZP.add$zoidLst[[lIdx]][[idx]]$dna) ){
            if( RZP.addZoid(RZP.add$zoidLst[[lIdx]][[idx]]$dna) ){
                procStat[2] <- procStat[2] + 1
            }
        } # if()

		if( 0==(procStat[1]%%10000) ){
			tDiff <- Sys.time() - tStmp
			k.FLogStr(sprintf("%.1f %s : proc %d. added %d"
								,tDiff,units(tDiff),procStat[1],procStat[2]
							)
							,pConsole=T ,pTime=F 
						)
		}
    } # for(idx)
}
tDiff <- Sys.time() - tStamp
k.FLogStr(sprintf("%.1f %s",tDiff,units(tDiff))
    ,pConsole=T,pTime=F)

tStr <- sprintf("%s",Sys.time()) # "2017-11-20 22:47:56"
saveFile <- gsub("^.*-(..)-(..).*","Obj_RZP\\1\\2.save",tStr)
RZP <- RZP.G
save( RZP ,file=saveFile )
