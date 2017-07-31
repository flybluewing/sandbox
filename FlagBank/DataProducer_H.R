# -----------------------------------------------------------
#	Data Producer
#		데이터 생산만을 담당
#		데이터에 대한 판단(선별이나 제거기준 설정)은 
#		Filter, RFiter클래스에서 구현할 것.
#		(같은 데이터에 대해서도 판단방법은 여러가지이므로)

G.DP <- list( classNm = "DataProducer" )


# -----------------------------------------------------------
#	DP.predr : DP.analyze()가 동작하기 위한 
#				기초정보 보관( 마지막 hz의 dna등.)
#	DB.analyze : 신규 zoid들(zoid pool)의 특성값 추출. 
DP.getFlag	<- function( pObj ,... ){	UseMethod("DP.getFlag")	}
DP.simul	<- function( pObj ,... ){	UseMethod("DP.simul")	}
DP.getPredr	<- function( pObj ,... ){	UseMethod("DP.getPredr")	}
DP.analyze	<- function( pObj ,... ){	UseMethod("DP.analyze")	}

# -----------------------------------------------------------
#	DP.defineDPrelative
DP.defineDPrelative <- function( pZh=FB$zh ){
		rObj <- list();	class(rObj) <- c(G.DP$classNm,"DPrelative")
		
		defObj <- list( )
		defObj$idStr <- sprintf("DPrelative")

		rObj$zh		<- pZh
		rObj$def	<- defObj
		rObj$create	<- k.relative
		
		return( rObj )
		
	} #DP.defineDPrelative

DP.getFlag.DPrelative <- function( pDP ,pZh=NULL ){
		if( is.null(pZh) )
			pZh <- pDP$zh
		rObj <- pDP$create( pZh )
		return( rObj )
	} # DP.getFlag.DPrelative

DP.getPredr.DPrelative <- function( pDP ){
		# Flag에서 모든 정보를 다 갖고있는데, 필요한가 싶다...
		rObj <- list( lastZoid=pDP$zh[nrow(pDP$zh),] )
		lastIdx <- nrow(pDP$zh)
		rObj$shPos <- which(pDP$zh[lastIdx,] %in% pDP$zh[lastIdx-1,])
		return( rObj )
	}

# -----------------------------------------------------------
#	DP.defineDPdiff

DP.defineDPdiff <- function( pZh=FB$zh ,pMax=7 ){

		rObj <- list();	class(rObj) <- c(G.DP$classNm,"DPdiff")
		
		defObj <- list( )
		defObj$pMax = pMax
		defObj$idStr <- sprintf("DPdiff_%d",pMax)

		rObj$zh		<- pZh
		rObj$def	<- defObj
		rObj$create	<- k.diff
		
		return( rObj )
	} # DP.defineDPdiff

DP.getFlag.DPdiff <- function( pDP ,pZh=NULL ){
		if( is.null(pZh) )
			pZh <- pDP$zh
		rObj <- pDP$create( pZh )
		return( rObj )
	} # DP.getFlag.DPdiff

DP.analyze.DPdiff <- function( pDP ,pDnaMtx ){
		# 이전 Zoid와 연산이 관계되지 않으므로, 그냥 create()함수 적용
		rObj <- pDP$create( pDnaMtx )
		return( rObj )
	} # DP.analyze.DPdiff
