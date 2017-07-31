source("03_H.R")
source("04_H.R")

myLogFile = "./temp/t04Flag.log"
myFLog("Create seqObj",myLogFile)
t04Flag <- t04Flag.multiple( tGlobal$rawZH ,1 ,2 )
nnOpt <- NnOpt()
nnOpt$maxit <- 10
nnOpt$size <- c(3,3)

t04Flag.simul <- t04_mlpSimul( t04Flag ,pNnOpt=nnOpt ,pSimulNum=2 ,pSimulSetNum=2 ,pSampleSize=2 
					,pLogFile=myLogFile )
myFLog("Finish simul",myLogFile)

# save( t04Flag.simul ,file="t04Flag_simul.save" )

t04Flag.predr <- t04_predr( t04Flag ,pSimul=t04Flag.simul ,pNnOpt=nnOpt ,pLogFile=myLogFile )

pXVal <- t04Flag$rMtx[26,t04Flag$col.x]
pred <- t04Flag.predr$pred( pXVal )

