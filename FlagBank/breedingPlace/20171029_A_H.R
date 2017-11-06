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
	funCodeValNAidxLst <- list()
	for( eIdx in 1:length(pCreateFunSet) ){
		eleObj <- list()
		eleObj$mtx <- matrix( 0 ,nrow= ifelse( is.null(pZh) ,0 ,nrow(pZh) )
								,ncol= length(pCreateFunSet[[eIdx]]) 
							)
		eleLst[[(1+length(eleLst))]] <- eleObj
		funIdLst[[(1+length(funIdLst))]]			<- sapply(pCreateFunSet[[eIdx]],function(p){p$idStr})
		funGIdLst[[(1+length(funGIdLst))]]			<- sapply(pCreateFunSet[[eIdx]],function(p){p$fGIdStr})
		
		codeValNAidx<- sapply(pCreateFunSet[[eIdx]],function(p){	if( is.null(p$codeValNA.idx) || (0==length(p$codeValNA.idx)) ) {
																		return( NA )
																	} else {
																		return( p$codeValNA.idx )
																	}
																})

		funCodeValNAidxLst[[(1+length(funCodeValNAidxLst))]] <- if( 0==length(codeValNAidx) ) integer(0) else codeValNAidx

		funInitNA	<- sapply(pCreateFunSet[[eIdx]],function(p){p$initNA})
		funInitNALst[[(1+length(funInitNALst))]] <- if( 0==length(funInitNA) ) integer(0) else funInitNA
					# 타입을 맞추느라... sapply()는 값이 없으면 빈 list를 반환하거든.

		funCodeValLst[[(1+length(funCodeValLst))]]	<- lapply(pCreateFunSet[[eIdx]],function(p){p$codeVal})
	}

	rObj <- list( eleLst=eleLst ,trainBase=pTrainBase )
	rObj$rowSpan <- NULL	# rowSpan : NULL은 아예 rowSpan 값을 사용치 않음을 의미.
							#			row를 선택치 않을때는 integer(0) 을 주면 된다.
							#			(rowSpan값은 생성 시 가 아닌 차후에 설정된다.)
	rObj$funIdLst		<- funIdLst
	rObj$funGIdLst		<- funGIdLst
	rObj$funInitNALst	<- funInitNALst
	rObj$funCodeValLst	<- funCodeValLst
	rObj$funCodeValNAidxLst <- funCodeValNAidxLst

	rObj$createTime <- NULL	# 버전관리용 단서

	return(rObj)

} # getNewElementSet()


getCreateFunSet <- function( pZh ,pDev=F ){

	funIdLst	<- list()
	funGIdLst	<- list()

	rLst <- list()

	gName <- "1st" # generation name
	rLst[[gName]]	<- get1stCreateFunSet( pZh ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "2nd"
	rLst[[gName]] <- get2ndCreateFunSet( pZh ,funIdLst ,funGIdLst ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "3rd"
	rLst[[gName]] <- get3rdCreateFunSet( pZh ,funIdLst ,funGIdLst ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "4th"
	rLst[[gName]] <- get4thCreateFunSet( pZh ,funIdLst ,funGIdLst ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "5th"
	rLst[[gName]] <- get5thCreateFunSet( pZh ,funIdLst ,funGIdLst ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "6th"
	rLst[[gName]] <- get6thCreateFunSet( pZh ,funIdLst ,funGIdLst ,pDev )
	funIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$idStr})
	funGIdLst[[gName]]		<- sapply(rLst[[gName]],function(p){p$fGIdStr})

	gName <- "7th" # 제너레이션이 더 추가된다면...

	return( rLst )
} # getCreateFunSet()

# pEleSet=eleSet  ;pProbPredObj=NULL  ;pDebug=T
analyzeSeq <- function ( pEleSet ,pProbPredObj=NULL ,pDebug=F ){

	probPredObj <- pProbPredObj
	if( is.null(pProbPredObj) ){
		myObj <- load("Obj_stdSeqObj.save") # loading stdSeqObj
		pProbPredObj <- getSeqProbPredictor(stdSeqObj)
	}

	testSpan <- (pEleSet$trainBase+1):nrow(pEleSet$eleLst[[1]]$mtx)
	hLst <- list()
	for( tIdx in testSpan ){	# tIdx <- testSpan[1] # 테스트 history index
		if( pDebug )
			k.FLogStr(sprintf("analyzeSeq() current history :%4d",tIdx))

		seqAnaSet <- list( stdH=tIdx ,anaLst=list() )
		for( eIdx in seq_len(length(pEleSet$eleLst)) ){ # eIdx <- 1
			seqAnaLst <- list()
			curEle <- pEleSet$eleLst[[eIdx]]
			for( cIdx in seq_len(ncol(curEle$mtx)) ){
				# 해당 h 이전 과거 자료를 가지고 h 시점의 zoid ele 예측치를 구하려는 것이므로.
				anaObj <- seqAnaFun.default( curEle$mtx[1:(tIdx-1),cIdx] ,pProbPredObj 
												,pInitNA = pEleSet$funInitNALst[[eIdx]][cIdx]
												,pCodeVal= pEleSet$funCodeValLst[[eIdx]][[cIdx]]
											)
				seqAnaLst[[(1+length(seqAnaLst))]] <- anaObj
			} # for(cIdx)
			seqAnaSet$anaLst[[(1+length(seqAnaSet$anaLst))]] <- seqAnaLst
		} # eIdx
		hLst[[(1+length(hLst))]] <- seqAnaSet
	}

	hAnaSet <- list( hLst=hLst ,workTimes=list() )
	hAnaSet$workTimes[[1]] <- Sys.time()

	return( hAnaSet )

} # analyzeSeq()
