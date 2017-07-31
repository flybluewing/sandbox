# 의존소스 
#

vz.definePlanet <- function( pDna = 1:10 ,pDnaSize=5 ,pEnvFilt = NULL
			,pClassName = "vPlanetZ"
		)
	{
		rObj <- list( dna=pDna ,dnaSize=pDnaSize )
		class(rObj) <- pClassName
		rObj$envFilt <- pEnvFilt
		if( is.null(rObj$envFilt) ){
			rObj$envFilt <- vz.buildEnvFilt()
		}
		
		firstZoid <- sort(sample(pDna,pDnaSize))
		rObj$zoid <- matrix( firstZoid ,nrow=1 )

		return( rObj )
	}

#	pObj : vPlanetZ
vz.createHistory <- function( pObj ,pHisLength=5 )
	{
		assFObj <- list()
		assFObj$fOpt <- list( fltLst = pObj$envFilt )
		assFObj$fOpt$isFCondRstLst <- NULL # 적용되어야 할 필터 파악용.
		assFObj$fBody <- function( pZh ,fOpt ){
								return( lapply(fOpt$fltLst,function(p){return(p$isFCond(pZh,p))}) )
							}
		breedEndFObj <- list()
		breedEndFObj$fOpt <- list()
		breedEndFObj$fBody <- function( pZoidP ){
								surviveIdx <- pZoidP$ass$survive.idx[1]
								beObj <- list( newZoid=pZoidP$pool$childMtx[surviveIdx,] )
								beObj$flag <- ( 1 == pZoidP$ass$ass.score[surviveIdx] ) # 1~0
								return( beObj )
							}

		zoidP <- vz.defineSpecies( pPop=40 ,pDnaType=pObj$dna
									,pDnaSize=pObj$dnaSize ,pDnaReplace=F ,pUseDna.NA=F
									,pAssFObj=assFObj
									,pBreedEndFObj = breedEndFObj
									,pClassName="zoid"
								)

		fltFitByHis <- list() # 각 zoid 단계에서 적용되어야 하는 필터가 무엇이었는지 기록.
		errStr <- NULL # 에러 발생 시, 내용기록.
		for( idx in 1:(pHisLength-1) ){
			# idx <- 1
			# 현재 적용되어야 할 필터가 무엇인지 파악.
			zoidP$assF.Opt$isFCondRstLst <- zoidP$assF( pObj$zoid ,zoidP$assF.Opt )
			fltFitByHis[[length(fltFitByHis)+1]] <- zoidP$assF.Opt$isFCondRstLst
			
			# newZoid 생성
			zoidP$pool <- vz.createPool(zoidP)
			isFound <- F
			for( hIdx in 1:100 ){ # 진화를 하면서 조건에 맞는 정답 Zoid 검색.
				zoidP$ass <- vz.assessPool(zoidP)
				breedEnd <- zoidP$breedEndF( zoidP )
				if( breedEnd$flag ){
					pObj$zoid <- rbind( pObj$zoid ,breedEnd$newZoid )
					isFound <- T
					break
				}
				zoidP$pool$childMtx <- vz.nextGen( zoidP )
			} # for(hIdx)

			if( !isFound ){
				errStr <- sprintf("  Fail to get new Zoid. (final length:%d)",nrow(pObj$zoid))
				k.FLogStr(errStr,pConsole=T)
				# pObj$envFilt 내 필터들간의 충돌이 있을 수 있다. 
				#	예를 들어 한쪽에서는 2가 있어야하는데 다른쪽에서는 없어야 한다는 등.
				break
			}
		} # for( idx )

		cName <- attributes(fltFitByHis[[1]])$names
		fltFitByHis.mtx <- matrix( NA ,nrow=length(fltFitByHis) ,ncol=length(cName) )
		colnames(fltFitByHis.mtx) <- cName

		for( rIdx in 1:length(fltFitByHis) ){
			fltFitByHis.mtx[rIdx,] <- sapply( fltFitByHis[[rIdx]] ,function(p){p$condF})
		}
		
		rObj <- list( zoid=pObj$zoid )
		rObj$fltFitByHis <- fltFitByHis 
			# zoid 수 보다 하나 적다.(생성에 필요한 zoid들만 해당되므로 마지막 zoid는 해당 없음.)
		rObj$fltFitByHis.mtx <- fltFitByHis.mtx
		rObj$errStr <- errStr
		
		return(rObj)
	} # vz.createHistory


#	pPreLogic / pReqLogic : and or xor not
vz.planetEnvF001 <- function( pPreC=c(1,5)	,pReq=c(3,7)	,pPreLogic="or" ,pReqLogic="or" )
	{	# 이전 세대에서 pPreC 값 중 하나가 있으면 다음세대는 pRes 값을 가져야 함.
		idStr <- sprintf("F001_%s%s_%s%s"
					,paste(sprintf("%02d",pPreC),collapse="")
					,pPreLogic
					,paste(sprintf("%02d",pReq),collapse="")
					,pReqLogic
				)

		rObj <- list( idStr=idStr )
		rObj$preLogic <- pPreLogic;	rObj$reqLogic <- pReqLogic
		rObj$preC <- pPreC;			rObj$req <- pReq
		rObj$isFCond <- function( pZh ,pFObj ){ # 필터링 대상에 해당되는지 확인
							rrObj <- list( fIdStr = pFObj$idStr )
							if( 0 == nrow(pZh) ){
								rrObj$condF = NA
								return(rrObj)
							}
							rrObj$ints <- intersect(pZh[nrow(pZh),],pFObj$preC)
							if( "and"==pFObj$preLogic ){
								rrObj$condF <- length(rrObj$ints)==length(pFObj$preC)
							} else if( "or"==pFObj$preLogic ){
								rrObj$condF <- length(rrObj$ints)>0
							} else if( "xor"==pFObj$preLogic ){
								rrObj$condF <- length(rrObj$ints)==1
							} else if( "not"==pFObj$preLogic ){
								rrObj$condF <- length(rrObj$ints)==0
							} else {
								rrObj$condF = NA
							}							
							return( rrObj )
						}

		# 요구조건에 만족되는지
		#	pContObj : isFCond() 함수 리턴 값.
		rObj$reqCond <- function( pZoid ,pFObj ,pCondObj ){ 
							rrObj <- list( fIdStr = pFObj$idStr )
							rrObj$ints <- intersect(pZoid,pFObj$req)
							if( "and"==pFObj$reqLogic ){
								rrObj$reqF <- length(rrObj$ints)==length(pFObj$req)
							} else if( "or"==pFObj$reqLogic ){
								rrObj$reqF <- length(rrObj$ints)>0
							} else if( "xor"==pFObj$reqLogic ){
								rrObj$reqF <- length(rrObj$ints)==1
							} else if( "not"==pFObj$reqLogic ){
								rrObj$reqF <- length(rrObj$ints)==0
							} else {
								rrObj$reqF = NA
							}
							return( rrObj )
						}

		return( rObj )
	}

vz.buildEnvFilt <- function(){
		rObj <- list()
		rObj[[length(rObj)+1]] <- vz.planetEnvF001( pPreC=c(1,5) ,pReq=c(3,7) ,pPreLogic="or" ,pReqLogic="or" )
		rObj[[length(rObj)+1]] <- vz.planetEnvF001( pPreC=c(1,5) ,pReq=c(8,9) ,pPreLogic="and" ,pReqLogic="not" )
		rObj[[length(rObj)+1]] <- vz.planetEnvF001( pPreC=4 ,pReq=3 ,pPreLogic="or" ,pReqLogic="not"  )
		rObj[[length(rObj)+1]] <- vz.planetEnvF001( pPreC=c(1,3) ,pReq=c(2,3,5) ,pPreLogic="xor" ,pReqLogic="or"  )
		rObj[[length(rObj)+1]] <- vz.planetEnvF001( pPreC=c(1,3) ,pReq=c(6,5) ,pPreLogic="not" ,pReqLogic="and"  )
		names(rObj) <- sapply( rObj ,function(p){p$idStr} )
		return( rObj )
	}

# ==================================================================================================
# Generic Algorithem

vz.defineSpecies <- function( pPop=100 ,pDnaType ,pClassName="zoid" 
								,pDnaSize=10	,pUseDna.NA=T	,pDnaReplace=T	,pDnaSort=T
								,pAssFObj=NULL # 개체평가 함수 객체
								,pBreedEndFObj = NULL
								,pDupChild=T
							)
	{

		rObj <- list( pop=pPop ,dnaType=pDnaType
						,dnaSize=pDnaSize	,useDna.NA=pUseDna.NA	,dnaReplace=pDnaReplace	,dnaSort=pDnaSort
					)
		class(rObj) <- pClassName
		rObj$survive	<- 10 # 부모유전자 그대로 계승 비율 %
		rObj$children	<- 70 # 교접에 의한 신규 자식생성비율 %
		rObj$random		<- 20 # 랜덤하게 탄생한 자식 비율 %
		rObj$mutant		<-  5 # 돌연변이 발생 비율 %
		rObj$dupChild	<- pDupChild # 동일 자식 생성 허용여부
		
		# 자식 생성 (디폴트)함수들.
		#	필요 시 Species를 선언한 후 함수를 바꾸도록 한다.
		rObj$childRandom	<- vz.childRandom
		rObj$childMate		<- vz.childMate
		rObj$childMutate	<- vz.childMutate
		
		rObj$pool <- NULL
		rObj$ass  <- NULL
		
		# 번식종료할 상황인지(목표에 도달했는지) 확인함수.
		rObj$breedEndF <- NULL
		if( !is.null(pBreedEndFObj) ){
			rObj$breedEndF <- pBreedEndFObj$fBody
			rObj$breedEndF.Opt <- pBreedEndFObj$fOpt
		}
		
		# 개체평가 함수객체
		if( !is.null(pAssFObj) ){
			rObj$assF <- pAssFObj$fBody
			rObj$assF.Opt <- pAssFObj$fOpt
		}

		return(rObj)
	}



# ==========================================================
#	Generic Function
#		- createPool() 에는 assessPool()에서의 가치평가 위해 
#							필요한 정보들이 공급되어야 한다.
#		- rptIndividual() 개체에 대한 Report
vz.createPool	<- function( pObj ,... ){	UseMethod("vz.createPool") }
vz.assessPool	<- function( pObj ,... ){	UseMethod("vz.assessPool") }
vz.nextGen		<- function( pObj ,... ){	UseMethod("vz.nextGen") }
vz.rptIndividual	<- function( pObj ,... ){ UseMethod("vz.rptIndividual") }
vz.rptPool		<- function( pObj ,... ){	UseMethod("vz.rptPool") }

# ==========================================================
#		- pObj : Species define
vz.createPool.zoid <- function( pObj ,... ){
		rObj <- list( )
			
		# 여기서의 로직은 gE.nextGen.vzSpecies() 함수에서도 유지되어야 한다.
		rObj$childMtx <- pObj$childRandom( 
									 pDnaType=pObj$dnaType
									,pDnaSize=pObj$dnaSize
									,pChildSize=pObj$pop
									,pUseNA	= pObj$useDna.NA
									,pReplace = pObj$dnaReplace
									,pSort = pObj$dnaSort
									,pDupChild = pObj$dupChild
								)
		return( rObj )
	}


vz.assessPool.zoid <- function( pObj ){

		childMtx <- pObj$pool$childMtx
		actFltName <- sapply(pObj$assF.Opt$isFCondRstLst,function(p){
									if( is.na(p$condF) || !p$condF ){
										return( NA )
									} else {
										return( p$fIdStr )
									}
								})
		actFltName <- na.omit( actFltName )
		
		fltReqRstMtx <- matrix( 0 ,nrow=pObj$pop ,ncol=length(actFltName) )
		colnames(fltReqRstMtx) <- actFltName

		if( 0<length(actFltName) ){
			for( pIdx in 1:pObj$pop ){
				# pIdx <- 4
				flag	<-mapply(function(p){
										flt <- pObj$assF.Opt$fltLst[[p]]
										condRst <- pObj$assF.Opt$isFCondRstLs[[p]]
										return( flt$reqCond(childMtx[pIdx,],flt,condRst)$reqF )
									}
									,actFltName
								)
				fltReqRstMtx[pIdx,] <- flag
			} # for(pIdx)
		} # if
		
		rObj <- list( )
		rObj$ass.score	<- apply( fltReqRstMtx ,1 ,sum ) / length(actFltName)
		rObj$actFltName <- actFltName
		rObj$fltReqRstMtx <- fltReqRstMtx

		tot <- pObj$survive + pObj$children + pObj$random
			# round() 함수를 사용하므로 survive,children,random의 총합은 tot보다 커질 수 있다.
		rObj$survive.n	<- round(pObj$pop*pObj$survive/tot)
		rObj$random.n	<- round(pObj$pop*pObj$random/tot)
		rObj$child.n	<- round(pObj$pop*pObj$children/tot)
		rObj$mutant.n	<- round(pObj$pop*pObj$mutant/tot)

		if( rObj$survive.n>0 ){
			rObj$survive.idx	<- order( rObj$ass.score ,decreasing=T )[1:rObj$survive.n]
			rObj$survive.mean	<- mean( rObj$ass.score[rObj$survive.idx] )
		}

		return( rObj )

	} # vz.assessPool.zoid


vz.nextGen.zoid <- function( pObj ,pDebug=NULL ){

		if( !is.null(pDebug) ){
			k.FLogStr(sprintf("    [d] %s",pDebug),pConsole=T)
			k.FLogStr(sprintf("    [d] pObj$dnaReplace : %s",pObj$dnaReplace),pConsole=T)
			if( !pObj$dnaReplace ){ # dna 내에 중복 코드가 존재하는 지 확인필요.
				maxDup <- apply(pObj$pool$childMtx,1,function(p){max(table(p))})-1
				if( 0< sum(maxDup) )
					k.FLogStr(sprintf("    [d] dup found:%s",paste(maxDup,collapse=",")),pConsole=T)
			}
		} # if pDebug

		childMtx <- pObj$pool$childMtx
		ass <- pObj$ass

		# 일정 수는 우등생으로 채우고
		childMtx.survive <- matrix(0,nrow=0,ncol=pObj$dnaSize)
		if( ass$survive.n>0 ){
			childMtx.survive <- childMtx[ass$survive.idx,]

			# 우등생 중, 중복요소는 제거해주자.
			sr <- sameRow(childMtx.survive)
			if( 1<length(sr) ){
				rmd.idx <- do.call(c,lapply(sr,function(p){p[2:length(p)]}))
				childMtx.survive <- childMtx.survive[-rmd.idx,]
			}
		}

		# 일정 수는 랜덤
		#	vz.createPool.zoid( ) 함수 참고.
		childMtx.random <- matrix(0,nrow=0,ncol=pObj$dnaSize)
		if( ass$random.n>0 ){
			childMtx.random <- pObj$childRandom( 
										 pDnaType=pObj$dnaType
										,pDnaSize=pObj$dnaSize
										,pReplace = pObj$dnaReplace
										,pChildSize=ass$random.n
										,pUseNA	= pObj$useDna.NA
										,pSort = pObj$dnaSort
										,pDupChild = pObj$dupChild
									)
			if( !is.null(pDebug) ){
				if( !pObj$dnaReplace ){ # dna 내에 중복 코드가 존재하는 지 확인필요.
					maxDup <- apply(childMtx.random,1,function(p){max(table(p))})-1
					if( 0< sum(maxDup) )
						k.FLogStr(sprintf("    [d] dup found(random):%s",paste(maxDup,collapse=",")),pConsole=T)
				}
			} # if pDebug
		}

		# 나머지는 교배로 채움.
		#	survive와 random 에서의 중복제거 작업/옵션들로 인해 초기 크기보다 작을 수 있다.
		#	의도된 수량보다 적을 수가 있어서 nrow()함수로 다시 확인한다.
		#	(그런데 children,survive,random 을 모두 합친 후, 중복존재를 다시 확인해야 하기도 하다.)
		childMtx.child <- matrix(0,nrow=0,ncol=pObj$dnaSize)
		n.children <- pObj$pop - ( nrow(childMtx.survive) + nrow(childMtx.random) )
		if( n.children > 0 ){
			# vz.childMate <- function( pChildMtxA ,pChildMtxB 
			ord <- order( ass$ass.score )
			# ord <- ord[1:round(length(ord)*2/3)] # 혹시 우수객체만 교배후보로 한다면...
			ord.prob <- rep(1,length(ord))	# 교배 선택 확률.
			ord.prob[1:round(length(ord)*2/3)] <- 2
			ord.prob[1:round(length(ord)/3)] <- 3
			xx <- sample(ord ,n.children ,replace=T ,prob=ord.prob)
			xy <- sample(ord ,n.children ,replace=T ,prob=ord.prob)
			childMtx.child <- vz.childMate( childMtx[xx,] ,childMtx[xy,] ,pSort=pObj$dnaSort ,pReplace=pObj$dnaReplace ,pDebug=pDebug )
			if( !is.null(pDebug) ){
				if( !pObj$dnaReplace ){ # dna 내에 중복 코드가 존재하는 지 확인필요.
					maxDup <- apply(childMtx.child,1,function(p){max(table(p))})-1
					if( 0< sum(maxDup) )
						k.FLogStr(sprintf("    [d] dup found(child):%s",paste(maxDup,collapse=",")),pConsole=T)
				}
			} # if pDebug
		} # if( n.children > 0 )

		# 돌연변이 적용
		childMtx.r <- rbind( childMtx.survive ,childMtx.child )
		if( !is.null(pDebug) ){
			k.FLogStr(sprintf("    [d] %s",pDebug),pConsole=T)
			if( !pObj$dnaReplace ){ # dna 내에 중복 코드가 존재하는 지 확인필요.
				maxDup <- apply(childMtx.r,1,function(p){max(table(p))})-1
				if( 0< sum(maxDup) )
					k.FLogStr(sprintf("    [d] dup found(r):%s",paste(maxDup,collapse=",")),pConsole=T)
			}
		} # if pDebug
		
		mut.idx <- sample( 1:nrow(childMtx.r) ,pObj$ass$mutant.n )
		childMtx.r[mut.idx,] <- vz.childMutate( childMtx.r[mut.idx,,drop=F] ,pObj$dnaType  ) # ,pSort=pObj$dnaSort
		if( !is.null(pDebug) ){
			if( !pObj$dnaReplace ){ # dna 내에 중복 코드가 존재하는 지 확인필요.
				maxDup <- apply(childMtx.r,1,function(p){max(table(p))})-1
				if( 0< sum(maxDup) )
					k.FLogStr(sprintf("    [d] dup found(r mut):%s",paste(maxDup,collapse=",")),pConsole=T)
			}
		} # if pDebug

		childMtx.r <- rbind( childMtx.r ,childMtx.random )

		return( childMtx.r )

	} # vz.nextGen.zoid


# -----------------------------------------------------------
#	자식 생성 함수 : 랜덤에 의해 생성.
#		- pUseNA : DNA 코드가 없는 것 허용.
#				DNA 크기가 고정되지 않은 것을 표현하기 위해
#		- pExcept : 중복을 피해야 할 자식들.
#				또한 신규 자식들은 pExcept에서 가장 덜 사용된
#				DNA 코드를 우선해서 생성된다.
#		- pDupChild : 중복된 child 존재를 허용할 것인지.
#		- 추가고려 사항 : 발생가능 Random 경우의 수 없을 때 수단.
vz.childRandom <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T ,pDupChild=T ,pSort=T
						)
	{
		rObj <- NULL
		if( pDupChild ){
			rObj <- vz.childRandom.dup( pDnaType=pDnaType ,pDnaSize=pDnaSize ,pChildSize=pChildSize
							,pUseNA=pUseNA ,pExceptMtx=pExceptMtx ,pReplace=pReplace ,pSort=pSort
						)
		} else {
			rObj <- vz.childRandom.Ndup( pDnaType=pDnaType ,pDnaSize=pDnaSize ,pChildSize=pChildSize
							,pUseNA=pUseNA ,pExceptMtx=pExceptMtx ,pReplace=pReplace ,pSort=pSort
						)
		}
		return( rObj )
	}
#	중복 child 허용
vz.childRandom.dup <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T ,pSort=pSort
						)
	{
		code.all <- pDnaType
		if( pUseNA ){
			code.all <- c(NA,pDnaType)
		}

		code.prb <- rep( 1 ,length(code.all) )
		if( !is.null(pExceptMtx) ){
			tbl <- table(pExceptMtx)
			code.prb <- (max(tbl)+1) - tbl
			if( pUseNA ){
				code.prb <- c( 1 ,code.prb )
			}	
		}

		idx.fail <- NULL
		cMax <- pChildSize
		rMtx <- matrix( NA ,ncol=pDnaSize ,nrow=pChildSize )

		for( cIdx in 1:pChildSize ){
			dna <- sample( code.all ,pDnaSize ,replace=pReplace ,prob=code.prb )
			if( pSort ){
				rMtx[cIdx,] <- sort(dna,na.last=T)
			} else {
				rMtx[cIdx,] <- pSort
			}
		} # for cIdx

		return( rMtx )

	}

#	중복 child 허용하지 않는 경우.
vz.childRandom.Ndup <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T ,pSort=T
						)
	{
		dnaComp <- function( pDna1 ,pDna2 ){
				# NA 존재를 고려한 동일여부 비교.
				oneSideNA <- xor(is.na(pDna1),is.na(pDna2))
				if( any(oneSideNA) )
					return( FALSE )
				
				return( all( pDna1[!oneSideNA]==pDna2[!oneSideNA] ,na.rm=T ) )
			}

		code.all <- pDnaType
		if( pUseNA ){
			code.all <- c(NA,pDnaType)
		}

		code.prb <- rep( 1 ,length(code.all) )
		if( !is.null(pExceptMtx) ){
			tbl <- table(pExceptMtx)
			code.prb <- (max(tbl)+1) - tbl
			if( pUseNA ){
				code.prb <- c( 1 ,code.prb )
			}	
		}

		idx.fail <- NULL
		cMax <- pChildSize
		rMtx <- matrix( NA ,ncol=pDnaSize ,nrow=pChildSize )
		for( cIdx in 1:pChildSize ){

			for( idx in 1:cMax ){
				dna <- sample( code.all ,pDnaSize ,replace=pReplace ,prob=code.prb )
				if( pSort )
					dna <- sort( dna ,na.last=T )
				
				# 이미 기존에 발생된 DNA인지 확인
				chkSame <- T
				if( pUseNA ){
					chkSame <- apply( rMtx[1:cIdx,,drop=F] ,1 ,function(p){dnaComp(p,dna)} )
				} else {
					chkSame <- apply( rMtx[1:cIdx,,drop=F] ,1 ,function(p){all(p==dna)} )
				}
				
				# pExceptMtx 내에 있는 dna인지 확인
				if( !any(chkSame) && !is.null(pExceptMtx) ){
					if( pUseNA ){
						chkSame <- apply( pExceptMtx ,1 
									,function(p){dnaComp(p,dna)} )
					} else {
						chkSame <- apply( pExceptMtx ,1 
									,function(p){all(p==dna)} )
					}
				}

				if( !any(chkSame) ){
					rMtx[cIdx,] <- dna
					break
				} else {
					if( idx > 3*(cMax %/%4) )
						k.FLog(sprintf("[Warn] vz.childRandom : same data : %s",paste(dna,collapse=" ")))
				}
			} # for(idx)
			
			if( (idx==cMax)&&(cMax>1) ){
				idx.fail <- c( idx.fail ,cIdx )
				k.FLog(sprintf("[Warn] vz.childRandom : reaches max(%d)",cIdx)
					,pConsole=T)
			}

		} # for(cIdx)

		if( !is.null(idx.fail) ){
			rMtx <- rMtx[-idx.fail,,drop=F]
		}

		# 디버깅용.
		# rMtx[order(rMtx[,1]),]
		return( rMtx )
	}

# -----------------------------------------------------------
#	자식 생성 함수 : 교배에 의해 생성.
vz.childMate <- function( pChildMtxA ,pChildMtxB ,pSort=T ,pReplace=F ,pDebug=NULL )
	{
		mate <- function( pDnaA ,pDnaB ,pLength ,pSwichNum ,pDebug=F ){
				swichIdx <- sample( 1:pLength ,pSwichNum )
				rDna <- pDnaA
				rDna[swichIdx] <- pDnaB[swichIdx]
				return(rDna)
			} # mate
		mate.unique <- function( pDnaA ,pDnaB ,pLength ,pSwichNum ,pDebug=F ){

				rDna <- rep(NA,pLength)
				pool <- c(pDnaA,pDnaB)
				for( idx in 1:pLength ){
					if( 1==length(pool) ){
						rDna[idx] <- pool
					} else {
						val <- sample( pool, 1 )
						rDna[idx] <- val
						if( is.na(val) ){
							pool <- pool[!is.na(pool)]
						} else {
							pool <- pool[pool!=val]
						}
					}
				} # for
				return( rDna )
			}
		
		rMtx <- pChildMtxA # 어차피 교배결과가 담기므로 초기값 의미없음.
		lnth <- ncol(pChildMtxA)
		swichNum <- sample( 1:lnth ,nrow(pChildMtxA) ,replace=T )
		for( idx in 1:nrow(pChildMtxA) ){
			if(!is.null(pDebug)){
				k.FLogStr(sprintf("    [d] idx:%d dnaA:%s dnaB:%s",idx,paste(pChildMtxA[idx,],collapse=","),paste(pChildMtxB[idx,],collapse=",")),pConsole=T)
			}
			
			child <- NULL
			if( pReplace ){
				child <- mate( pChildMtxA[idx,] ,pChildMtxB[idx,] 
								,pLength=lnth ,pSwichNum=swichNum[idx]
							)
			} else {
				child <- mate.unique( pChildMtxA[idx,] ,pChildMtxB[idx,] 
								,pLength=lnth ,pSwichNum=swichNum[idx]
							)
			} # if pReplace
			
			if( pSort )
				child <- sort(child ,na.last=T)
			rMtx[idx,] <- child
		}

		return( rMtx )
	}

# -----------------------------------------------------------
#	자식 생성 함수 : 돌연변이
vz.childMutate <- function( pChildMtx ,pDnaType ,pSort=F ,pDebug=NULL )
	{
		mut.idx <- sample( 1:ncol(pChildMtx) ,nrow(pChildMtx) ,replace=T )
		if( !is.null(pDebug) ) {
			k.FLogStr(sprintf("   [d]mut.idx:%s",paste(mut.idx,collapse=",")),pConsole=T)
		}
		
		for( rIdx in 1:nrow(pChildMtx) ){
			surVal <- pChildMtx[rIdx,-mut.idx[rIdx]]
			mutVal <- setdiff( pDnaType ,surVal )
			if( !is.null(pDebug) ){
				k.FLogStr(sprintf("   [d] rIdx:%d surVal:%s",rIdx,paste(surVal,collapse=",")),pConsole=T)
				k.FLogStr(sprintf("   [d] rIdx:%d mutVal:%s",rIdx,paste(mutVal,collapse=",")),pConsole=T)
			}
			pChildMtx[rIdx,mut.idx[rIdx]] <- sample( mutVal ,1 )
			if( pSort )
				pChildMtx[rIdx,] <- sort(pChildMtx[rIdx,])
		}
		
		return( pChildMtx )
	}
