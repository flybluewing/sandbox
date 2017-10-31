# 한글한글
source("20171028_A_H.R")

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


