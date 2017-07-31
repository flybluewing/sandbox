source("03_H.R")
source("04_H.R")

library(snowfall)
sfInit( parallel=T ,cpus=3 )


myLogFile = "./temp/04_tryFlag.log"
myFLog("Create seqObj",myLogFile)
nnOpt <- NnOpt()
nnOpt$maxit <- 10000
nnOpt$size <- c(20,20)

myFLog("Create flagLst",myLogFile)
flagLst <- list()
flagLst[["01_02"]] <- c(1,2)
flagLst[["01_03"]] <- c(1,3)
flagLst[["01_05"]] <- c(1,5)
flagLst[["02_02"]] <- c(2,2)
flagLst[["02_03"]] <- c(2,3)
flagLst[["02_05"]] <- c(2,5)
flagLst[["03_02"]] <- c(3,2)
flagLst[["03_03"]] <- c(3,3)
flagLst[["03_05"]] <- c(3,5)
flagLst[["04_02"]] <- c(4,2)
flagLst[["04_03"]] <- c(4,3)
flagLst[["04_05"]] <- c(4,5)
flagLst[["05_02"]] <- c(5,2)
flagLst[["05_03"]] <- c(5,3)
flagLst[["05_05"]] <- c(5,5)
flagLst[["06_02"]] <- c(6,2)
flagLst[["06_03"]] <- c(6,3)
flagLst[["06_05"]] <- c(6,5)



myFLog("Start simulation ----------------",myLogFile)
sfLibrary(RSNNS)
sfExport("t03.seq");sfExport("sameRow");
sfExport("myFLog");	sfExport("myLogFile")
sfExport("tGlobal");sfExport("nnOpt");sfExport("myLogFile")
sfExport("t04_mlpSimul")
sfExport("t04_mlpSimul.t04Flag")
sfExport("t04Flag.multiple")
simulLst <- sfLapply( flagLst ,function( pOpt ){
						rObj <- list( )
						rObj$flag <- t04Flag.multiple( tGlobal$rawZH ,pOpt[1] ,pOpt[2] )
						simul <- t04_mlpSimul( rObj$flag ,pNnOpt=nnOpt 
										,pSimulNum=100 ,pSimulSetNum=5 ,pSampleSize=10
										,pLogFile=myLogFile )
						rObj$simul <- simul
						return( rObj )
					}
				)
myFLog("Finish simulLst",myLogFile)


# save( simulLst ,file=file.save)
# myObj <- load( file.save )

rptObj <- list()

file.save <- "./temp/A/Obj_simulLst.save"
file.report <- "./temp/A/"
myFLog(sprintf("Report Start : %s",file.save))
myObj <- load( file.save )
rptLst <- list()
for( nIdx in attributes(simulLst)$names ){
	#p <- simulLst[[1]]
	rptLst[[nIdx]] <- t04_report( p$simul ,p$flag ,pReportFile=sprintf("%s%s",file.report,nIdx) )	
}
rptObj[[file.save]] <- rptLst			
myFLog("End Report.")

file.save <- "./temp/A2/Obj_simulLst_linF.save"
file.report <- "./temp/A2/"
myFLog(sprintf("Report Start : %s",file.save))
myObj <- load( file.save )
rptLst <- list()
for( nIdx in attributes(simulLst)$names ){
	#p <- simulLst[[1]]
	rptLst[[nIdx]] <- t04_report( p$simul ,p$flag ,pReportFile=sprintf("%s%s",file.report,nIdx) )	
}
rptObj[[file.save]] <- rptLst			
myFLog("End Report.")

file.save <- "./temp/A3/Obj_simulLst_722.save"
file.report <- "./temp/A3/"
myFLog(sprintf("Report Start : %s",file.save))
myObj <- load( file.save )
rptLst <- list()
for( nIdx in attributes(simulLst)$names ){
	#p <- simulLst[[1]]
	rptLst[[nIdx]] <- t04_report( p$simul ,p$flag ,pReportFile=sprintf("%s%s",file.report,nIdx) )	
}
rptObj[[file.save]] <- rptLst			
myFLog("End Report.")

file.save <- "./temp/A4/Obj_simulLst_722B.save"
file.report <- "./temp/A4/"
myFLog(sprintf("Report Start : %s",file.save))
myObj <- load( file.save )
rptLst <- list()
for( nIdx in attributes(simulLst)$names ){
	#p <- simulLst[[1]]
	rptLst[[nIdx]] <- t04_report( p$simul ,p$flag ,pReportFile=sprintf("%s%s",file.report,nIdx) )	
}
rptObj[[file.save]] <- rptLst			
myFLog("End Report.")

save( rptObj ,file="Obj_rptObj.save" )

