myLst <- list()
myLst[["1"]] <- "One"
myLst[["2"]] <- NULL
myLst[["3"]] <- "Two"
myLst[["4"]] <- NULL
# ======================================================================================
# SAF Filter
#	- Def, Flag, Simul, Predr 4개의 객체를 가짐.
#	- Def에서 정의된 클래스는 부모 객체(SAF Filter)가 그대로 갖도록 할 것.
# --------------------------------------------------------------------------------------

# ======================================================================================
# SAF define Object
#	- flag를 생성하기 위한 정보, 즉 flag가 생성되기 이전 정보만을 관리
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
#	Filter : pH년도 내에서, 동일한 dna가 pD개 발생되면 Flag 1
#		- pD는 다수가 될 수 있다.(or조건)
#			pH는 어차피 최대값에 모두 포함될 것이므로 오로지 하나.
# --------------------------------------------------------------------------------------
getDefSAF_rebound <- function( pH ,pD ,pHighCut=0.85 ,pLowCut=0.15 ){

		rObj <- list( h=pH ,d=pD )
		class( rObj ) <- c( G.Filter$class.saf ,"safRebound" )
		rObj$idStr <- sprintf( "rebound_%s_%s",paste(pH,collapse="h"),paste(pD,collapse="d") )
		
		rObj$nnOpt <- FB$default.nnOpt
		rObj$nnOpt$linOut <- F

		rObj$highCut	<- pHighCut
		rObj$lowCut		<- pLowCut

		return( rObj )
	}


# --------------------------------------------------------------------------------------
#	Filter : n배수(pBase) 값이면 1, 아니면 0
#   - pBase가 다수인 경우, or 조건으로 적용한다.
#       즉 c(2,3)이면 2의 배수이거나 3의 배수인 경우 모두 Flag가 1이 됨.
# --------------------------------------------------------------------------------------
getDefSAF_multiple <- function( pCol ,pBase ,pHighCut=0.85 ,pLowCut=0.15 ){

		rObj <- list( col=pCol ,base=pBase )
		class( rObj ) <- c( G.Filter$class.saf ,"safMultiple" )
		rObj$idStr <- sprintf("multi%d_%s",pCol,paste(pBase,collapse="a"))
		
		rObj$nnOpt <- FB$default.nnOpt
		rObj$nnOpt$linOut <- F

		rObj$highCut	<- pHighCut
		rObj$lowCut		<- pLowCut

		return( rObj )
	}


