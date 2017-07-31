source("03_H.R")
source("04_H.R")

library(snowfall)
sfInit( parallel=T ,cpus=2 )

myLogFile = "./temp/t04Flag.log"
myFLog("Create seqObj",myLogFile)
nnOpt <- NnOpt()
nnOpt$maxit <- 2000
nnOpt$size <- c(10,10)

myFLog("Create flagLst",myLogFile)
flagLst <- list()
t04Flag <- t04Flag.multiple( tGlobal$rawZH ,1 ,2 )
flagLst[[t04Flag$idStr]] <- t04Flag
t04Flag <- t04Flag.multiple( tGlobal$rawZH ,1 ,3 )
flagLst[[t04Flag$idStr]] <- t04Flag


myFLog("Start simulation ----------------",myLogFile)
sfLibrary(RSNNS)
sfExport("myFLog");	sfExport("myLogFile")
sfExport("nnOpt");sfExport("nnOpt");sfExport("myLogFile")
sfExport("t04_mlpSimul")
sfExport("t04_mlpSimul.t04Flag")
simulLst1 <- sfLapply( flagLst ,function( pFlag ){
						rObj <- list( flag=pFlag )
						simul <- t04_mlpSimul( pFlag ,pNnOpt=nnOpt 
										,pSimulNum=4 ,pSimulSetNum=2 ,pSampleSize=2 
										,pLogFile=myLogFile )
						rObj$simul <- simul
						return( rObj )
					}
				)

myFLog("Finish simul1",myLogFile)

myFLog("Start simulation ----------------",myLogFile)
simulLst2 <- lapply( flagLst ,function( pFlag ){
						rObj <- list( flag=pFlag )
						simul <- t04_mlpSimul( pFlag ,pNnOpt=nnOpt 
										,pSimulNum=4 ,pSimulSetNum=2 ,pSampleSize=2 
										,pLogFile=myLogFile )
						rObj$simul <- simul
						return( rObj )
					}
				)
myFLog("Finish simul2",myLogFile)

