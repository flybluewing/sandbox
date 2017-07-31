# ======================================================================================
# FlagBank 기본 값.
#	- 대부분 저장된 값을 불러다 사용
#	- 필터들의 선언을 위해 FilterSAF_H.R, FilterDPF_H.R를 미리 읽어들인 상태이어야 함.
# --------------------------------------------------------------------------------------
getFlagBank <- function( pZoidHistoryFile="./zoidHistory/ZH694.csv" ){

		loadZH <- function( pFileName ){
						rZH <- read.delim( pFileName ,sep="," ,stringsAsFactor=F ,na.string="" )
						return(rZH)
					}
		addHisLog <- function( pMsg ,pFB ){
						history <- NULL
						if( is.null(pFB$hisLog) ){
							history <- data.frame( time=Sys.time() ,msg=pMsg )
						} else {
							history <- rbind( pFB$HisLog ,data.frame( time=Sys.time() ,msg=pMsg ) )
						}
						
						return( history )
					}

		rObj <- list(
					dnaLength = 6
				   ,dnaType = 1:45
				   ,learnArea = 1:50 # ML학습범위 디폴트 값.
				)
		
		#	FB의 이력관리 용
		rObj$hisLog <- addHisLog( "최초생성" ,rObj )
		
		rObj$loadZH <- loadZH
		rObj$zh <- rObj$loadZH( pZoidHistoryFile )

		rObj$saf	# <- defineSAF( )
		rObj$dpf	# <- defineDPF( )
		
		rObj$flagMtx <- matrix( NA ,nrow=nrow(rObj$zh) ,ncol=0 )
		rObj$flagS	<- NULL	# SAF들로부터의 flag(data.frame) 사용보류
		rObj$flagD	<- NULL	# DPF들로부터의 flag(data.frmae) 사용보류
		
		k.FLogStr(sprintf("getFlagBank() : zoid history loaded(%s)",pZoidHistoryFile))
		
		
		rObj$flt	<- NULL	# filter Object
		
		# 글로벌하게 사용하기 위한 디폴트 값 목록.
		rObj$default.nnOpt <- getNnOpt( )

		return( rObj );
	}



defineSAF <- function( ){
		rLst <- list()

		saf <- list(def=getDefSAF_multiple(1, 2));	rLst[[saf$def$idStr]] <- saf	#
		saf <- list(def=getDefSAF_multiple(1, 3));	rLst[[saf$def$idStr]] <- saf	#
		saf <- list(def=getDefSAF_multiple(1, 4));	rLst[[saf$def$idStr]] <- saf	#

		colVal <- 1:6;	baseVal <- c(2,3,4,5,6,7,8,9,10,11,13,17,19,23,29,31,37,41,43)
		for( cvIdx in colVal ){
			for( bvIdx in baseVal ){
				saf <- list(def=getDefSAF_multiple(cvIdx, bvIdx))
				rLst[[saf$def$idStr]] <- saf	#
			}
		}
		
		pH <- 1:20;	pD <- 1:6
		for( hIdx in pH ){
			for( dIdx in pD ){
				saf <- list(def=getDefSAF_rebound(hIdx,dIdx))
				rLst[[saf$def$idStr]] <- saf
			}
		}
		
		for( idx in 1:length(rLst) ){
			class(rLst[[idx]]) <- class(rLst[[idx]]$def)
		}
		
		return( rLst )
	}

defineDPF <- function( ){
		rLst <- list()
		return( rLst )		
	}
