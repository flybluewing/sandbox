# 20171029_A_H.R


source("20171029_A_H_createFun.R")
source("20171029_A_H_seqAnaFun.R")

#	- pTrainBase : 연속분석함수(seqAnaFun)에서 기초 데이터 분석용으로 사용할 구간.
getNewElementSet <- function( pCreateFunSet ,pTrainBase=100 ,pZh=NULL ){

	#	pZh 는 초기화를 위해 사용함.
	#		처음 생성 시 부터 rbind()로 시행하면 속도가 너무 느릴 수 있으니까.

	eleLst <- list()
	funIdLst 		<- list()
	funGIdLst		<- list()
	funInitNALst	<- list()
	funCodeValLst	<- list()
	for( eIdx in 1:length(pCreateFunSet) ){
		eleObj <- list()
		eleObj$mtx <- matrix( 0 ,nrow= ifelse( is.null(pZh) ,0 ,nrow(pZh) )
								,ncol= length(pCreateFunSet[[eIdx]]) 
							)
		eleLst[[(1+length(eleLst))]] <- eleObj
		funIdLst[[(1+length(funIdLst))]]			<- sapply(pCreateFunSet[[eIdx]],function(p){p$idStr})
		funGIdLst[[(1+length(funGIdLst))]]			<- sapply(pCreateFunSet[[eIdx]],function(p){p$fGIdStr})
		funInitNALst[[(1+length(funInitNALst))]]	<- sapply(pCreateFunSet[[eIdx]],function(p){p$initNA})
		funCodeValLst[[(1+length(funCodeValLst))]]	<- sapply(pCreateFunSet[[eIdx]],function(p){p$codeVal})
	}

	rObj <- list( eleLst=eleLst ,trainBase=pTrainBase )
	rObj$rowSpan <- NULL	# rowSpan : NULL은 아예 rowSpan 값을 사용치 않음을 의미.
							#			row를 선택치 않을때는 integer(0) 을 주면 된다.
							#			(rowSpan값은 생성 시 가 아닌 차후에 설정된다.)
	rObj$funIdLst		<- funIdLst
	rObj$funGIdLst		<- funGIdLst
	rObj$funInitNALst	<- funInitNALst
	rObj$funCodeValLst	<- funCodeValLst

	return(rObj)

} # getNewElementSet()


getCreateFunSet <- function( pZh ){

	funIdLst	<- list()
	funGIdLst	<- list()

	rLst <- list()

	gName <- "1st" # generation name
	rLst[[gName]]	<- get1stCreateFunSet( pZh )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "2nd"
	rLst[[gName]] <- get2ndCreateFunSet( pZh ,funIdLst ,funGIdLst )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "3rd"
	rLst[[gName]] <- get3rdCreateFunSet( pZh ,funIdLst ,funGIdLst )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "4th"
	rLst[[gName]] <- get4thCreateFunSet( pZh ,funIdLst ,funGIdLst )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "5th"
	rLst[[gName]] <- get5thCreateFunSet( pZh ,funIdLst ,funGIdLst )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "6th"
	rLst[[gName]] <- get6thCreateFunSet( pZh ,funIdLst ,funGIdLst )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "7th" # 제너레이션이 더 추가된다면...

	return( rLst )
}


