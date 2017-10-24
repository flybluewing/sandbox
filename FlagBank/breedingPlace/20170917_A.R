# 
library(plyr)
source("20170911_A_H.R")
source("20170911_A_auxH.R")

source("20170917_A_H.R")

# 한글 깨지지는 않겠지?

curWd <- getwd()
setwd("..")
FB <- getFlagBank()
setwd(curWd)

# k1 <- getSeqProbMapObj( pFlag=sample(1:3 ,500 ,replace=T ,prob=c(2,1,1) ) )
# stdSeqObj <- getStdSeqProbMapObj( pTestNum=100 ,pSeqLogMax=300 )
# kObj <- k.seqNum( pFlag=pFlag )

pSeqObj=stdSeqObj

means <- sapply(pSeqObj$probPLst ,function(p){p$mean} )


cName <- c("mean","seq","prob")
mtx <- matrix( 0 ,nrow=nrow(pSeqObj$probPMap) ,ncol=length(cName) )
colnames(mtx) <- cName

trainMtx <- mtx[0,]
for( idx in 1:ncol(pSeqObj$probPMap) ){ # idx <- 1
	mtx[ ,"mean"]	<- means[idx]
	mtx[ ,"seq" ]	<- 1:nrow(pSeqObj$probPMap)
	mtx[ ,"prob"]	<- pSeqObj$probPMap[,idx]
	
	trainMtx <- rbind( trainMtx ,mtx )
} # for(idx)



# require( RSNNS )
trainX <- trainMtx[,c("mean","seq")]
trainY <- trainMtx[,"prob"]

tStmp <- Sys.time()
fitMlp <- mlp( x=trainX ,y=trainY ,size=c(30,30) ,maxit=50000
				,initFunc="Randomize_Weights" ,initFuncParams=c(-0.3,0.3)
				,learnFunc="Std_Backpropagation" ,learnFuncParams=c(0.2,0)
				,updateFunc="Topological_Order"	,updateFuncParams=c(0)
				,hiddenActFunc="Act_Logistic"	,shufflePatterns=T
				,linOut=T
			)
tDiff <- Sys.time() - tStmp
logStr <- sprintf("cost : %.1f %s",tDiff,units(tDiff))
k.FLogStr( logStr ,pConsole=T )

predMlp <- predict( fitMlp ,trainX )

plot( 1:length(trainX) ,trainY ,type="l" )
points( 1:length(trainX) ,predMlp ,col="red" )

#	pFitMlp = fitMlp	;pProbMap=pSeqObj$probPMap	;pXLim=c(0,40) ;pYLim=c(0,1) ;pMeanStep=0.1
showProbMap <- function( pFitMlp=NULL ,pProbMap=NULL ,pXLim=c(0,40) ,pYLim=c(0,1) ,pMeanStep=0.1 ){

					if( is.null(pFitMlp) )
						pFitMlp <- rObj$fitMlp
					if( is.null(pProbMap) )
						pProbMap <- rObj$probMap

					probCol <- terrain.colors( ncol(pProbMap) )
					plot( NULL ,xlim=pXLim ,ylim=pYLim ,xlab="seq" ,ylab="prob" )
					for( cIdx in 1:ncol(pProbMap ) ){
						lines( 1:nrow(pProbMap) ,pProbMap[,cIdx] ,col=probCol[cIdx] )
					}

					meanStep <- seq( 0.01 ,1 ,pMeanStep )
					mtx <- matrix(0,nrow=nrow(pProbMap),ncol=2)
					mtx[,2] <- 1:nrow(mtx)	# seq number
					for( cIdx in 1:length(meanStep) ){
						mtx[,1] <- meanStep[cIdx]
						points( 1:nrow(pProbMap) ,predict(pFitMlp,mtx) ,col="blue" )
					}
				}

0.15

head( trainX[trainX[,"mean"]==0.3,] )

trainY[ trainX[,"mean"]==0.3 ]

head( trainMtx[trainX[,"mean"]==0.3,] )

plot( trainMtx[,"prob"] ,trainY )


# 81page