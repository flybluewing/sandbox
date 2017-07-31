

G.Filter <- list()
G.Filter$class.base	<- "Filter"
G.Filter$class.saf	<- c( G.Filter$class.base ,"filterSAF")
G.Filter$class.dpf	<- c( G.Filter$class.base ,"filterDPF")


# ======================================================================================
# Flag Object
#	- Simul을 위한 사전 정보를 준비/생성한다.
#		rMtx, col.x 등
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
#		pSeqHisCnt : 이전 몇 번째 까지의 연속출현을 가지고 예측할 것인지?
#			예를 들어 1이 1번, 0이 2번, 1이 3번, 0이 4번, 1이 5번 연속발생했다면
#			pSeqHisCnt가 5인 경우 1이 1번 발생한 시점부터,
#			pSeqHisCnt가 2인 경우 0이 4번 발생한 시점부터를 가지고 예측자료로 활용한다.
#
# --------------------------------------------------------------------------------------
getFlag.integer <- function( pFlag ,pIdStr ,pSeqHisCnt=6 ,pStart=50 ){
		# NULL 반환가능 함수.
		
		rObj <- list();	class(rObj) <- "flagFilter"
		
		rObj$flag <- pFlag

		# ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6"...
		rObj$col.x <- c("preVal" ,paste("seqNum",1:pSeqHisCnt,sep="") )

		if( pStart>=length(pFlag) ){
			k.FLog(sprintf("[Err:%s] not available flag. pStart:%d",pIdStr,pStart),pConsole=T)
			return( NULL )
		}
		lSpan <- pStart:length(pFlag)

		na.safe <- 1
		if( any(is.na(pFlag)) ){
			lastNA <- max(which(is.na(pFlag)))
			na.safe <- lastNA+1
		}

		cName <- c( "hIdx" ,"outVal" ,rObj$col.x )
		rMtx <- matrix( 0 ,nrow=length(lSpan) ,ncol=length(cName) )
		colnames(rMtx) <- cName

		tempLog <- NULL	# rMtx에서 NA가 발생한 이유 로깅.(차후 디버깅을 위해.)
		for( lIdx in lSpan ){
			# lIdx <- 25
			if( lIdx <= na.safe ){	# na.safe가 lIdx와 같은 경우라도, 이전 값이 NA이므로 무의미
				rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx] ,rep(NA,pSeqHisCnt) )
				tempLog <- c(tempLog,sprintf("lIdx:%d %d(na.safe skip)",lIdx,na.safe))
				next
			}
			sInfo <- k.seq( pFlag[na.safe:(lIdx-1)] )
			nR <- nrow(sInfo$seqCntMtx)
			if( nR > pSeqHisCnt ){
				mtx <- sInfo$seqCntMtx[(nR-(pSeqHisCnt-1)):nR,]
				seqNum <- mtx[,"cnt"]*ifelse(mtx[,"val"]==1,1,-1)
				rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx-1] ,seqNum )
			} else {
				rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx] ,rep(NA,pSeqHisCnt) )
				tempLog <- c(tempLog,sprintf("lIdx:%d %d(nR skip)",lIdx,nR))
				# k.FLogStr(sprintf("-lIdx:%d-----",lIdx))
				# k.FLog(sInfo$seqCntMtx)
			}
		}
		
		rObj$rMtx <- rMtx
		rownames(rObj$rMtx) <- as.character(rMtx[,"hIdx"])
		rObj$seqHisCnt <- pSeqHisCnt

		rObj$sameRow <- sameRow( rObj$rMtx ,rObj$col.x )

		return( rObj )
	}

getFlag.double <- function( pFlag ,pIdStr ,pSeqHisCnt=6 ,pStart=50 ){
		getFlag( as.integer(pFlag) ,pIdStr ,pSeqHisCnt ,pStart )
	}

getFlag.QQE <- function( pSaf ,pZH=FB$zh ){ # 템플릿 함수.

		defObj <- pSaf$def
		flag <- NULL
		# 여기서 조건을 구현한다.
		names(flag) <- as.character(1:nrow(pZH))
		
		rObj <- getFlag( flag ,defObj$idStr )
		if( is.null(rObj) )
			return( NULL )
		class( rObj ) <- c( class(defObj) ,class(rObj) )
		
		return( rObj )

	}
	
getFlag.safRebound <- function( pSaf ,pZh=FB$zh ){

		defObj <- pSaf$def

		tMtx <- matrix( F ,ncol=nrow(pZh) ,nrow=0 )
		rVec <- k.rebound( defObj$h ,pZh )
		tf	<- rVec%in%defObj$d	# rVec에서 NA 바로처리 없나..
		tf[is.na(rVec)] <- NA
		# k.FLog( rVec )
		# k.FLog( ifelse(tf,"T","F") )
		tMtx <- rbind( tMtx ,tf )

		flag <- apply( tMtx ,2 ,function(p){ ifelse(any(p),1,0) } )
		names(flag) <- as.character(1:nrow(pZh))
		# k.FLog(ifelse(flag,"T","F"))
		
		rObj <- getFlag( flag ,defObj$idStr )
		if( is.null(rObj) )
			return( NULL )
			
		class( rObj ) <- c( class(defObj) ,class(rObj) )
		rObj$rebound <- rVec
		
		return( rObj )
	}
	
getFlag.safMultiple <- function( pSaf ,pZH=FB$zh ){

		defObj <- pSaf$def
        
        flag <- NULL
        if( 1==length(defObj$base) ){
            flag <- pZH[,defObj$col] %% defObj$base
            flag <- ifelse( flag==0,1,0)
        }
        else {
            regMtx <- mapply(function(p){return(p%%defObj$base)},pZH[,defObj$col])
            flag <- apply( regMtx ,2 ,function(p){return(ifelse(any(p==0),1,0))} )
        }

		names(flag) <- as.character(1:nrow(pZH))

		rObj <- getFlag( flag ,defObj$idStr )
		if( is.null(rObj) )
			return( NULL )
			
		class( rObj ) <- c( class(defObj) ,class(rObj) )
		
		return( rObj )
		
	}



# ======================================================================================
# Simul Object
#		클래스 다형성을 지원할 필요가 있는지는 모르겠다.
# --------------------------------------------------------------------------------------
getMlpSimul <- function( pFlagF ,pNnOpt=NULL
						,pSimulRange=NULL ,pSimulNum=500 ,pSimulSetNum=5 ,pSampleSize=10 
						,pLogFile=k.FLogOpt$defaultLogFile
					)
	{

		FLogStr <- function( msg ,pConsole=F ,pTime=T ){
				k.FLogStr( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
		FLog <- function( msg ,pConsole=F ,pTime=T ){
				k.FLog( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
				
		dbgMsg <- sprintf("[Dbg]Obj:%s, pSimulNum:%d, pSampleSize:%d, pSimulSetNum:%d"
						,pFlagF$def$idStr,pSimulNum,pSampleSize,pSimulSetNum)
		FLogStr( dbgMsg ,pConsole=T ,pTime=F )

		if( is.null(pNnOpt) ){
			pNnOpt <- pFlagF$def$nnOpt
		}
		if( is.null(pFlagF$flag) )
			return( NULL )

		nR <- nrow( pFlagF$flag$rMtx )
		# -------------------------------------------------
		# 불용영역 제거 및 대응
		lastNA <- 0 # NA가 없다면 0.(lastNA다음부터 가용영역으로 판단.)
		if( any(is.na(pFlagF$flag$rMtx[,"seqNum1"])) ){
			lastNA <- max(which( is.na(pFlagF$flag$rMtx[,"seqNum1"]) ))
		}
		if( lastNA > 1 ){
			FLogStr(sprintf("<%s> rMtx has NA area.(last NA at %d)",pFlagF$def$idStr,lastNA),pConsole=T)
			if( lastNA==nR ){
				FLogStr(sprintf("<Err:%s> rMtx is not available. last NA at %d",pFlagF$def$idStr,lastNA),pConsole=T)
				return( NULL )
			}
		}
		availRange <- (lastNA+1):nR # rMtx에서 불용지역을 제외한 부분의 row 인덱스

		# -------------------------------------------------
		# SimulRange 정리 및 가용성 확인.
		if( is.null(pSimulRange) ){
			rangel <- length(availRange)
			pSimulRange <- availRange[1:as.integer(rangel-rangel/10)] # 1:0형태가 되어도 0은 인덱스에서 자동제외(좋군!)
		} else if( any(pSimulRange<=lastNA) ) {
			FLogStr(sprintf("<Err:%s> simulRange is not available. last NA at %d",pFlagF$def$idStr,lastNA),pConsole=T)
			return( NULL )
		}

		# simul이 끝난 후, 성능 테스트를 위한 영역.
		testRange <- setdiff(availRange,pSimulRange)			
		if( 0==length(testRange) ){
			FLogStr(sprintf("<Err:%s> No margin for test.(all available area is taken by simulRange )",pFlagF$def$idStr),pConsole=T)
			return( NULL )
		}


		rObj <- list( logFile=pLogFile )
		class(rObj) <- "flagSimul"
		rObj$nnOpt 		<- pNnOpt	# simul당시의 nnOpt 기록용.			
		rObj$simulRange <- pSimulRange;	rObj$availRange	<- availRange
		rObj$simulNum	<- pSimulNum;	rObj$simulSetNum <- pSimulSetNum
		rObj$sampleSize <- pSampleSize

		smLen <- length(pSimulRange)
		sRMtx <- pFlagF$flag$rMtx[pSimulRange,]

		cName <- c("hIdx","idx","tot","hit") # hIdx:Zoid History
		scoreMtx <- matrix( 0 ,nrow=smLen ,ncol=length(cName) )
		colnames(scoreMtx) <- cName
		scoreMtx[,"hIdx"]	<- sRMtx[,"hIdx"]
		scoreMtx[,"idx"]	<- pSimulRange

		# -----------------------------------------------------------------
		# 명중률 시뮬레이션을 위한 반복 테스트
		FLogStr(sprintf("Start %s simulation",pFlagF$def$idStr))
		rLst <- list()
		mlp.fit <- NULL			
		for( setIdx in 1:pSimulSetNum ){
			FLogStr(sprintf("%s SimulSet %d",pFlagF$def$idStr,setIdx))
			scoreMtx[,c("tot","hit")] <- 0
			for( tIdx in 1:pSimulNum ){

				testIdx <- sample( 1:smLen ,pSampleSize ,replace=F )
				mlp.fit <- mlp(	x=sRMtx[-testIdx,pFlagF$flag$col.x]
									,y=sRMtx[-testIdx,"outVal"]
									,size=pNnOpt$size	,maxit=pNnOpt$maxit
									,initFunc=	pNnOpt$initFunc
									,initFuncParams=	pNnOpt$initFuncParams
									,learnFunc=	pNnOpt$learnFunc
									,learnFuncParams=	pNnOpt$learnFuncParams
									,updateFunc=pNnOpt$updateFunc
									,updateFuncParams=	pNnOpt$updateFuncParams
									,hiddenActFunc=pNnOpt$hiddenActFunc
									,sufflePattern=pNnOpt$shufflePattern
									,linOut=pNnOpt$linOut
								)

				mlp.pred <- predict( mlp.fit ,sRMtx[testIdx,pFlagF$flag$col.x] )
				hit <- which( ifelse(mlp.pred>0.5,1,0) == sRMtx[testIdx,"outVal"] )
					# idx : testIdx의 인덱스임을 주의
				scoreMtx[testIdx,"tot"] <- scoreMtx[testIdx,"tot"]+1
				scoreMtx[testIdx[hit] ,"hit" ] <- scoreMtx[testIdx[hit] ,"hit" ] + 1

				if( (tIdx%%10)==0 ){ 
					FLogStr(sprintf("  simulate %dth of %d(%s)",tIdx,pSimulNum,pFlagF$def$idStr)) 
				}

			} # for

			FLogStr(sprintf("   captured sample:%d of %d",sum(scoreMtx[,"tot"]>0),nrow(scoreMtx)))
			
			rLst[[setIdx]] <- list()
			rLst[[setIdx]]$rate <- apply( scoreMtx ,1 ,function(p){ifelse(p["tot"]==0,NA,p["hit"]/p["tot"])} )
			names(rLst[[setIdx]]$rate) <- rownames(sRMtx) # 기존 rMtx에 있던 name전달.(pFlagF$flag$rMtx)
			
		} # for(setIdx)

		rateMtx		<- sapply( rLst ,function(p){p$rate} )
		rObj$rate.count	<- apply( rateMtx ,1 ,function(p){sum(!is.na(p))} )
		rObj$rate.var	<- apply( rateMtx ,1 ,var ,na.rm=T )	# 값이 2개 이상일때만 NA가 아님.

		rate.mean	<- apply( rateMtx ,1 ,mean ,na.rm=T )
		rate.mean[is.nan(rate.mean)] <- 0.5	# NaN은 예측치로서 사용불가이므로 0.5확률 적용.
		rObj$rate.mean	<- rate.mean
		# Warning!! rate.x들은 pSimulRange에 해당하는 값들임을 유의하자.

		# 테스트 영역 없는, 시뮬레이션 영역 전체를 대상으로 하는 Fit
		FLogStr("Last mlp fit for all simulation range")
		rObj$mlpFit.pred <- mlp(	x=sRMtx[,pFlagF$flag$col.x]	,y=sRMtx[,"outVal"]
								,size=pNnOpt$size	,maxit=pNnOpt$maxit
								,initFunc=	pNnOpt$initFunc
								,initFuncParams=	pNnOpt$initFuncParams
								,learnFunc=	pNnOpt$learnFunc
								,learnFuncParams=	pNnOpt$learnFuncParams
								,updateFunc=pNnOpt$updateFunc
								,updateFuncParams=	pNnOpt$updateFuncParams
								,hiddenActFunc=pNnOpt$hiddenActFunc
								,sufflePattern=pNnOpt$shufflePattern
								,linOut=pNnOpt$linOut
							)
		rObj$mlpFit.rate <- mlp(	x=sRMtx[,pFlagF$flag$col.x]	,y=rObj$rate.mean
								,size=pNnOpt$size	,maxit=pNnOpt$maxit
								,initFunc=	pNnOpt$initFunc
								,initFuncParams=	pNnOpt$initFuncParams
								,learnFunc=	pNnOpt$learnFunc
								,learnFuncParams=	pNnOpt$learnFuncParams
								,updateFunc=pNnOpt$updateFunc
								,updateFuncParams=	pNnOpt$updateFuncParams
								,hiddenActFunc=pNnOpt$hiddenActFunc
								,sufflePattern=pNnOpt$shufflePattern
								,linOut=pNnOpt$linOut
							)

		return( rObj )

	}


# ======================================================================================
# Predictor Object
#		클래스 다형성을 지원할 필요가 있는지는 모르겠다.
# --------------------------------------------------------------------------------------
getMlpPredr <- function( pFlagF ,pNnOpt=NULL ,pHighCut=NULL ,pLowCut=NULL 
						,pLogFile=k.FLogOpt$defaultLogFile
					)
	{
		FLogStr <- function( msg ,pConsole=F ,pTime=T ){
				k.FLogStr( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
		FLog <- function( msg ,pConsole=F ,pTime=T ){
				k.FLog( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
			
		if( is.null(pFlagF$simul) ){	# 어차피 simul이 NULL이면 flag도 NULL
			k.FLogStr(sprintf("[Fail:%s]simul is null",pFlagF$def$idStr),pConsole=T)
			return( NULL )
		}

		col.x <- pFlagF$flag$col.x
		if( is.null(pHighCut) )
			pHighCut <- pFlagF$def$highCut
		if( is.null(pLowCut) )
			pLowCut <- pFlagF$def$lowCut
		if( is.null(pNnOpt) )
			pNnOpt <- pFlagF$def$nnOpt
		rMtx <- pFlagF$flag$rMtx
		simulRange <- pFlagF$simul$simulRange
		availRange <- pFlagF$simul$availRange

		rObj <- list( logFile=pLogFile )
		rObj$highCut <- pHighCut;	rObj$lowCut <- pLowCut

		# 명중률 매핑
		# pSimul$rate.var 에 대한 레포트 산출 및 필요시 제거
		# 주의 : rate.x들은 simulRange에 해당하는 배열임.(rMtx가 아님.)
		cName <- c( "highlow" ,"rate" ,"var" ,"hIdx" ,"outVal" ,col.x )
		predMtx <- matrix( 0 ,ncol=length(cName) ,nrow=0 )	# rbind()연속이라, 컬럼명은 나중에..

		indices <- simulRange[which( pFlagF$simul$rate.mean > pHighCut )]
		if( 0<length(indices) ){
			mtx <- rMtx[indices,c("hIdx" ,"outVal",col.x)]
			if( 1==length(indices) ){
				mtx <- t(as.matrix(mtx))
			}
			mtx <- cbind( pFlagF$simul$rate.var[indices] ,mtx )
			mtx <- cbind( pFlagF$simul$rate.mean[indices] ,mtx )
			mtx <- cbind( rep("high",nrow(mtx)) ,mtx )
			predMtx <- rbind( predMtx ,mtx )
		}

		# QQE low 영역을 제외시킬 지 검토. (반드시 안 맞는다는 것도 아니니...)
		indices <- simulRange[which( pFlagF$simul$rate.mean < pLowCut )]
		if( 0<length(indices) ){
			mtx <- rMtx[indices,c("hIdx" ,"outVal",col.x)]
			if( 1==length(indices) ){
				mtx <- t(as.matrix(mtx))
			}
			mtx <- cbind( pFlagF$simul$rate.var[indices] ,mtx )
			mtx <- cbind( pFlagF$simul$rate.mean[indices] ,mtx )
			mtx <- cbind( rep("low",nrow(mtx)) ,mtx )
			predMtx <- rbind( predMtx ,mtx )
		}

		# QQE var 값이 크거나 NA인 것들도 제외시켜야 할 지?

		colnames(predMtx) <- cName	# rbind 연속이라 컬럼명을 매 나중에.
		rObj$predMtx <- predMtx

		# 실 사용을 위한 예측 mlpFit 생성
		#	명중률 예측은 simul$mlpFit.rate 을 사용한다.
		#		(어차피 명중률 학습치는 simulRange밖에 없으므로.)
		FLogStr("creating a mlp fit for hit rate prediction.")
		rObj$mlpFit.pred <- mlp( x=rMtx[availRange,col.x]	,y=rMtx[availRange,"outVal"]
							,size=pNnOpt$size				,maxit=pNnOpt$maxit
							,initFunc=pNnOpt$initFunc		,initFuncParams=pNnOpt$initFuncParams
							,learnFunc=pNnOpt$learnFunc		,learnFuncParams=pNnOpt$learnFuncParams
							,updateFunc=pNnOpt$updateFunc	,updateFuncParams=pNnOpt$updateFuncParams
							,hiddenActFunc=pNnOpt$hiddenActFunc
							,shufflePattern=pNnOpt$shufflePattern
							,linOut=pNnOpt$linOut
						)

		return( rObj )

	}



# ======================================================================================
# predict
#	pSimul : 시뮬레이션 영역으로 훈련된 mlpFit을 사용.(시뮬 결과확인 용.)
# --------------------------------------------------------------------------------------

k.predict.flagFilter <- function( pFlagF ,pXVal ,pSimul=F ){

		if( is.null(pFlagF$flag) || is.null(pFlagF$simul) || is.null(pFlagF$predr) ){
			return(NULL)
		}

		mlpFit.pred <- pFlagF$predr$mlpFit.pred
		if( pSimul )
			mlpFit.pred <- pFlagF$simul$mlpFit.pred
		mlpFit.rate <- pFlagF$simul$mlpFit.rate
		predMtx <- pFlagF$predr$predMtx


		rPredObj <- list()
		found <- apply( predMtx ,1 ,function(p){ return(all(p[pFlagF$flag$col.x]==pXVal)) }	)

		if( any(found) ){
			rVec <- NULL
			rMtx <- predMtx[found,]
			if( sum(found)==1 ){
				rVec <- rMtx
			} else {
				# 데이터 신뢰도를 var 기준으로..
				varNum <- as.numeric(rMtx[,"var"])  #문자 상태 데이터를 숫자로.
				rMtx <- rMtx[order(varNum,na.last=T),]
				rVec <- rMtx[1,]
			}
			
			rPredObj$m.hIdx	<- rVec["hIdx"]	# 어떤 history를 매핑했는지..
			rPredObj$m.outVal <- as.numeric( rVec["outVal"] )
			rPredObj$m.typ	<- rVec["highlow"]
			rPredObj$m.rate	<- as.numeric( rVec["rate"] )
			rPredObj$m.var	<- ifelse( rVec["var"]!="NA" ,as.numeric(rVec["var"]) ,NA )
			if( rPredObj$m.typ == "low" ){
				rPredObj$m.outVal	<- ifelse( rPredObj$m.outVal==0 ,1 ,0 )
				rPredObj$m.rate	<- 1-rPredObj$m.rate
			}
		}

		rPredObj$outVal <- ifelse( predict( mlpFit.pred ,pXVal )>0.5 ,1 ,0 )
		rPredObj$typ	<- "ANN"
		rPredObj$rate	<- predict( mlpFit.rate ,pXVal )

		return( rPredObj )
	}


# ======================================================================================
# Report
# --------------------------------------------------------------------------------------

k.report.flagFilter <- function( pFlagF ,pReportFile=NULL ,pLogFile=k.FLogOpt$defaultLogFile ,pSimulBase=T )
	{

		if( is.null(pReportFile) ){
			pReportFile <- sprintf("./report/%s",pFlagF$def$idStr)
		}
		report.txt <- sprintf("%s.txt",pReportFile)
		report.pdf <- sprintf("%s.pdf",pReportFile)

		# -[Functions]----------------------------------------------------------
		RepStr <- function( msg ,pConsole=F ,pTime=F ){
				k.FLogStr( msg ,pConsole=pConsole ,pFile=report.txt ,pTime=pTime )
			}
		Rep <- function( msg ,pConsole=F ,pTime=F ){
				k.FLog( msg ,pConsole=pConsole ,pFile=report.txt ,pTime=pTime )
			}
		FLogStr <- function( msg ,pConsole=F ,pTime=T ){
				k.FLogStr( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
		FLog <- function( msg ,pConsole=F ,pTime=T ){
				k.FLog( msg ,pConsole=pConsole ,pFile=pLogFile ,pTime=pTime )
			}
		# ----------------------------------------------------------------------
		if( is.null(pFlagF$predr) || is.null(pFlagF$simul) || is.null(pFlagF$flag) ){
			rStr <- sprintf("[Fail:%s] check flag,or simul, or predr",pFlagF$def$idStr)
			RepStr(rStr,pConsole=T);	FLogStr(rStr)
			return( NULL )
		}

		pdf(report.pdf) # 그래프 출력용 PDF

		rObj <- list()
		
		FLogStr(sprintf("[Report]%s",pFlagF$def$idStr),pConsole=T ,pTime=T)
		FLogStr(sprintf("    File:%s",pReportFile),pConsole=T)


		RepStr("[NN options]----------",pConsole=T)
		nnOpt <- pFlagF$def$nnOpt
		RepStr(k.str(nnOpt));	print(sprintf("size:%s  maxit:%d",paste(nnOpt$size,collapse=","),nnOpt$maxit))
		
		RepStr("[NN results]----------",pConsole=T)
		mlpFit <- pFlagF$predr$mlpFit.pred	# simul을 안할 수도 있으니, 실예측용 fit을 사용하자.
		if( pSimulBase ){
			mlpFit <- pFlagF$simul$mlpFit.pred
		}
		maxit <- length(mlpFit$IterativeFitError)
		rStr <- sprintf("    Iteration : %f(last), %f(min), %f(last/min)"
						,mlpFit$IterativeFitError[maxit],min(mlpFit$IterativeFitError)
						,ifelse( 0==min(mlpFit$IterativeFitError),0,mlpFit$IterativeFitError[maxit]/min(mlpFit$IterativeFitError) )
					)
		RepStr( rStr ,pConsole=T )

		lastQStart <- (maxit*3)/4
		lastErrDf <- data.frame( it=lastQStart:maxit ,err=mlpFit$IterativeFitError[lastQStart:maxit] )
		lm.fit <- lm( err~it ,data=lastErrDf )
		slope <- coef(lm.fit)["it"]
		rStr <- sprintf("    slope:%f (%f changed in last 4/4 area.)" ,slope ,slope*nrow(lastErrDf) )
		RepStr( rStr ,pConsole=T )

		par( mfrow=c(2,1) )
		plot( mlpFit$IterativeFitError		,main="Iterative Fit Error(ALL)")
		plot( lastQStart:maxit ,mlpFit$IterativeFitError[lastQStart:maxit]	,main="Iterative Fit Error(4/4Q)")


		par( mfrow=c(2,2) )
		rMtx <- pFlagF$flag$rMtx
		simulRange <- pFlagF$simul$simulRange
		# -[Assessment within simulRange]-------------------------------------------
        rStr <- "    Assessment within simulRange"
		RepStr( rStr ,pConsole=T )

		simulMtx <- rMtx[ simulRange,pFlagF$flag$col.x ]
		assLst <- apply( simulMtx ,1 ,function(p){ return(k.predict(pFlagF,p,pSimul=T)) })

		assDf <- do.call( rbind ,lapply(assLst,function(p){
											data.frame( outVal=p$outVal ,rate=p$rate 
														,m.hIdx=ifelse(is.null(p$m.hIdx),NA,p$m.hIdx) 
														,m.outVal=ifelse(is.null(p$m.outVal),NA,p$m.outVal)
														,m.typ=ifelse(is.null(p$m.typ),NA,p$m.typ)
														,m.rate=ifelse(is.null(p$m.rate),NA,p$m.rate)
														,m.var=ifelse(is.null(p$m.var),NA,p$m.var)
													)
										})
							)
		realOut <- rMtx[simulRange,"outVal"]
		predOut <- assDf$outVal
		assDf$isHit <- realOut==predOut
		rStr <- sprintf("        Hit : %d of %d",sum(assDf$isHit),length(assDf$isHit))
		RepStr( rStr ,pConsole=T )

		freq <- table(pFlagF$flag$flag[simulRange])	# NA 는 자동필터링 된다. 으미 좋은 거..
		if( 2 > length(freq) ){	# freq가 단일값만 가진 경우 대응.
			tFreq <- c(0,0);	names(tFreq) <- 0:1
			tFreq[names(freq)] <- freq
			freq <- tFreq
		}
		rStr <- sprintf("        Freq(0vs1) : %d vs %d(%f)"
						,freq["0"] ,freq["1"] ,ifelse(min(freq)==0,NA,max(freq)/min(freq)) 
					)
		RepStr( rStr ,pConsole=T )

		boxplot( assDf$rate[assDf$isHit] ,assDf$rate[!assDf$isHit] 
					,names=c("hit","miss")	,ylim=c(0,1)
					,ylab="Predicted Hit Rate by Ann" 
					,main="Within SimulRange"
					)

		hist( which(assDf$isHit) ,main="Hit position" ,xlab="Z history" )

		cName <- c("High rate hit","Low rate hit")
		rName <- c("Total","Hit count")
		m.hitMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
		colnames(m.hitMtx) <- cName;	rownames(m.hitMtx) <- rName
		m.highIdx <- which(assDf$m.typ == "high")
		m.lowIdx <- which( assDf$m.typ == "low" )
		m.hitMtx[1,] <- c( length(m.highIdx) ,length(m.lowIdx) )
		m.hitMtx[2,] <- c( sum(assDf$isHit[m.highIdx]) ,sum(assDf$isHit[m.lowIdx]))
		barplot( m.hitMtx ,beside=T ,legend=rName ,col=heat.colors(length(rName)) 
					,main="(Within SimulRange)"
				)
		hitMtx <- matrix( 0 ,nrow=2 ,ncol=2 );	colnames(hitMtx)=0:1;	rownames(hitMtx)=0:1
		tHitMtx <- table( predOut ,realOut )		#		   realOut
												#	predOut  0  1
												#		  0 14  8
												#		  1 29 14
		hitMtx <- k.fillMatrix( tHitMtx ,hitMtx )	# predOut,realOut이 1,0을 고루갖고있지 않은 경우대응.
		lName		<- sprintf("0: %d/%d hit(%.4f)" ,hitMtx["0","0"] ,sum(hitMtx[,"0"]) 
									,ifelse(sum(hitMtx[,"0"])==0,NA,hitMtx["0","0"]/sum(hitMtx[,"0"])) )
		lName[2]	<- sprintf("1: %d/%d hit(%.4f)" ,hitMtx["1","1"] ,sum(hitMtx[,"1"]) 
									,ifelse(sum(hitMtx[,"1"])==0,NA,hitMtx["1","1"]/sum(hitMtx[,"1"])) )
		barplot( hitMtx ,beside=T ,legend=lName ,main="Flag값 별 명중률")
		Rep( tHitMtx ,pConsole=T )



		# -[Assessment beyond simulRange]-------------------------------------------
		RepStr( "    Assessment beyond simulRange" ,pConsole=T )
		testRange <- setdiff( pFlagF$simul$availRange ,pFlagF$simul$simulRange )
		testMtx <- rMtx[testRange,pFlagF$flag$col.x]
		assLst <- apply( testMtx ,1 ,function(p){ return(k.predict(pFlagF,p)) })
		assDf <- do.call( rbind ,lapply(assLst,function(p){
										data.frame( outVal=p$outVal ,rate=p$rate 
													,m.hIdx=ifelse(is.null(p$m.hIdx),NA,p$m.hIdx) 
													,m.outVal=ifelse(is.null(p$m.outVal),NA,p$m.outVal)
													,m.typ=ifelse(is.null(p$m.typ),NA,p$m.typ)
													,m.rate=ifelse(is.null(p$m.rate),NA,p$m.rate)
													,m.var=ifelse(is.null(p$m.var),NA,p$m.var)
												)
									})
						)

		realOut <- rMtx[testRange,"outVal"]
		predOut <- assDf$outVal
		assDf$isHit <- realOut==predOut
		rObj$assDf.simulRange <- assDf

		RepStr(sprintf("        Hit : %d of %d",sum(assDf$isHit),length(assDf$isHit)) ,pConsole=T)
		freq <- table(pFlagF$flag$flag[testRange])
		if( 2 > length(freq) ){	# freq가 단일값만 가진 경우 대응.
			tFreq <- c(0,0);	names(tFreq) <- 0:1
			tFreq[names(freq)] <- freq
			freq <- tFreq
		}
		rStr <- sprintf("        Freq(0vs1) : %d vs %d(%f)"
						,freq["0"] ,freq["1"] ,ifelse(min(freq)==0,NA,max(freq)/min(freq)) 
					)
		RepStr( rStr ,pConsole=T )

		boxplot( assDf$rate[assDf$isHit] ,assDf$rate[!assDf$isHit] 
					,names=c("hit","miss")	,ylim=c(0,1)
					,ylab="Predicted Hit Rate by Ann" 
					,main="Beyond SimulRange"
					)

		hist( which(assDf$isHit) ,main="Hit position" ,xlab="distance from last simul" )

		cName <- c("High rate hit","Low rate hit(Inverted.)")
		rName <- c("Total","Hit count")
		m.hitMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
		colnames(m.hitMtx) <- cName;	rownames(m.hitMtx) <- rName
		m.highIdx <- which(assDf$m.typ == "high")
		m.lowIdx <- which( assDf$m.typ == "low" )
		m.hitMtx[1,] <- c( length(m.highIdx) ,length(m.lowIdx) )
		m.hitMtx[2,] <- c( sum(assDf$isHit[m.highIdx]) ,sum(!assDf$isHit[m.lowIdx]))
		barplot( m.hitMtx ,beside=T ,legend=rName ,col=heat.colors(length(rName)) 
					,main="(Beyond SimulRange)"
				)

		hitMtx <- matrix( 0 ,nrow=2 ,ncol=2 );	colnames(hitMtx)=0:1;	rownames(hitMtx)=0:1
		tHitMtx <- table( predOut ,realOut )		#		   realOut
												#	predOut  0  1
												#		  0 14  8
												#		  1 29 14
		hitMtx <- k.fillMatrix( tHitMtx ,hitMtx )	# predOut,realOut이 1,0을 고루갖고있지 않은 경우대응.
		lName		<- sprintf("0: %d/%d hit(%.4f)" ,hitMtx["0","0"] ,sum(hitMtx[,"0"]) 
									,ifelse(sum(hitMtx[,"0"])==0,NA,hitMtx["0","0"]/sum(hitMtx[,"0"])) )
		lName[2]	<- sprintf("1: %d/%d hit(%.4f)" ,hitMtx["1","1"] ,sum(hitMtx[,"1"]) 
									,ifelse(sum(hitMtx[,"1"])==0,NA,hitMtx["1","1"]/sum(hitMtx[,"1"])) )
		barplot( hitMtx ,beside=T ,legend=lName )
		Rep( tHitMtx ,pConsole=T )

		rObj$assDf <- assDf
		rObj$m.hitMtx <- m.hitMtx

		RepStr( "end of Report."  ,pTime=T)
		dev.off()

	}	# k.report.flagFilter







