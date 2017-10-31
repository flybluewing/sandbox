# 20171029_A_H.R


source("20171029_A_H_createFun.R")
source("20171029_A_H_seqAnaFun.R")

getNewElementSet <- function( pCreateFunSet ,pZh=NULL ){
	
	#	pZh 는 초기화를 위해 사용함.
	#		처음 생성 시 부터 rbind()로 시행하면 속도가 너무 느릴 수 있으니까.
	
	eleLst <- list()
	funIdLst <- list()
	for( eIdx in 1:length(pCreateFunSet) ){
		eleObj <- list()
		eleObj$mtx <- matrix( 0 ,nrow= ifelse( is.null(pZh) ,0 ,nrow(pZh) )
								,ncol= length(pCreateFunSet[[eIdx]]) 
							)
		eleLst[[(1+length(eleLst))]] <- eleObj
		funIdLst[[(1+length(funIdLst))]] <- sapply(pCreateFunSet[[eIdx]],function(p){p$idStr})
	}
	
	rObj <- list( eleLst=eleLst ,funIdLst=funIdLst ,rowSpan=NULL )
		# rowSpan : NULL은 아예 rowSpan 값을 사용치 않음을 의미.
		#			row를 선택치 않을때는 integer(0) 을 주면 된다.
	return(rObj)

} # getNewElementSet()


getCreateFunSet <- function( pZh ){

	funIdLst <- list()

	rLst <- list()
	rLst[["1st"]] <- get1stCreateFunSet( pZh )
	funIdLst[["1st"]] <- sapply(rLst[["1st"]],function(p){p$idStr})

	rLst[["2nd"]] <- get2ndCreateFunSet( pZh ,funIdLst )
	funIdLst[["2nd"]] <- sapply(rLst[["2nd"]],function(p){p$idStr})
	
	rLst[["3rd"]] <- get3rdCreateFunSet( pZh ,funIdLst )
	funIdLst[["3rd"]] <- sapply(rLst[["3rd"]],function(p){p$idStr})
	
	rLst[["4th"]] <- get4thCreateFunSet( pZh ,funIdLst )
	funIdLst[["4th"]] <- sapply(rLst[["4th"]],function(p){p$idStr})
	
	rLst[["5th"]] <- get5thCreateFunSet( pZh ,funIdLst )
	funIdLst[["5th"]] <- sapply(rLst[["5th"]],function(p){p$idStr})

	rLst[["6th"]] <- get6thCreateFunSet( pZh ,funIdLst )
	funIdLst[["6th"]] <- sapply(rLst[["6th"]],function(p){p$idStr})
	
	return( rLst )
}


