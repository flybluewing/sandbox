
#------------------------------------------------------
library(jsonlite)
library(snowfall)
library(plyr)
library(rgl)
library(neuralnet)
library(gtools)

TGlobal <- function(){

    rList <- list(
                dnaLength = 6
			   ,dnaTypes = 1:45
               ,Start = 600 # 적합률 평가시작위치.(따라서 depth 최대는 Start-1)
               ,rawZH = NULL    # RawZH에 대한 읽기전용 접근
			   
			   ,useMP=T		# 멀티프로세싱을 할 것인지?
			   ,useMP_cpu=2	# 멀티프로세싱을 한다면 CPU는 몇 개?
			   ,zoidPoolSize = 100
			   
            )

    return( rList );
}

tGlobal <- TGlobal();

#	 sfStop()
if( tGlobal$useMP ){
	sfInit(parallel=T,cpus=tGlobal$useMP_cpu)
} else {
		sfInit(parallel=F)
}


myLog <- function( msg ){ print( paste("LOG",msg,sep=":") ) }
myFLog <- function( msg ,fName="myFLog.Log" ,pAppend=T ,pLogTime=T ){ 
				if( pLogTime ){
					cat( sprintf("[%s] %s",Sys.time(),capture.output(msg)), file=fName, sep="\n", append=pAppend ) 
				} else {
					cat( sprintf("%s",capture.output(msg)), file=fName, sep="\n", append=pAppend ) 
				}
			}
			
			

source("Krondor.R")
source("functions_01.R")

#------------------------------------------------------
setwd("C:/zproject/ISLR/miniProj")

RawZH <- read.delim( "ZH694.csv", sep=",", stringsAsFactor=F, na.string="" )
#	RawZH <- RawZH[1:10,]
tGlobal$rawZH <- RawZH

myLog(sprintf( "RawZH is loaded.(size:%d, headers:'%s')"
            , nrow(RawZH)
            , paste(colnames(RawZH),collapse=" ") 
        ))

#------------------------------------------------------

source("GetOverView.R")
# source("BuildTable.R")
source("ZH_FilterTest.R")
# soruce("ZH_BaseProb.R")
# source("TempCode.R")

execMode <- "null"
testBand <- 1:5
myFLog( sprintf("execMode : %s",execMode) )
if( "makeData"==execMode ){	print( execMode )
	for( tIdx in testBand ){
		ZH_FilterTest( tIdx, pZoidPoolSize=tGlobal$zoidPoolSize )
	}
}else if( "analyzeData"==execMode ) {	print( execMode )
		# 로드 데이터
		# Filter 적중 확률 계산
		# 데이터 분석.
	}else {	print( execMode )
}


