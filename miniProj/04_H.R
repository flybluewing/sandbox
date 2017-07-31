T04.logFile <- "T04.log"


#----------------------------------------------------
#	Class : NN.mlpOption
NnOpt <- function( ){
			nnOpt <- list( );	class(nnOpt) <- "nnOpt.mlpOption"

			nnOpt$size <- c(20,20);	nnOpt$maxit <- 10000
			nnOpt$initFunc="Randomiz_Weights"			
			nnOpt$initFuncParams <- c(-1.5,1.5)
			nnOpt$learnFunc="Std_Backpropagation"
			nnOpt$learnFuncParams <- c(0.2,0)
			nnOpt$updateFunc="Topological_Order"
			nnOpt$updateFuncParams <- c(0)
			nnOpt$hiddenActFunc="Act_Logistic"
			nnOpt$shufflePattern=T
			nnOpt$linOut=T
			return( nnOpt )
		}


t04_mlpSimul <- function( pT04Flag ,... ) {
			UseMethod("t04_mlpSimul")
		}

t04_mlpSimul.t04Flag <- function( pT04Flag ,pNnOpt=NULL 
									,pSimulRange=NULL ,pSimulNum=1000 ,pSimulSetNum=5 ,pSampleSize=10
									,pLogFile=T04.logFile
								)
		{

			dbgMsg <- sprintf("[Dbg]Obj:%s, pSimulNum:%d, pSampleSize:%d, pSimulSetNum:%d"
							,pT04Flag$idStr,pSimulNum,pSampleSize,pSimulSetNum)
			myFLog(dbgMsg,pLogFile)
			
			if( is.null(pNnOpt) ){
				pNnOpt <- NnOpt()
			}
			if( is.null(pSimulRange) ){
				pSimulRange <- 1:( nrow(pT04Flag$rMtx)-nrow(pT04Flag$rMtx)/10 )
			}
			
			rObj <- list( logFile=pLogFile )
			class(rObj) <- "t04_mlpSimul_t04Flag"
			rObj$nnOpt 		<- pNnOpt			
			rObj$simulRange <- pSimulRange
			rObj$simulNum	<- pSimulNum;	rObj$simulSetNum <- pSimulSetNum
			rObj$samplesize <- pSampleSize
			
			nR <- length(pSimulRange)
			rMtx <- pT04Flag$rMtx[pSimulRange,]
			
			cName <- c("hIdx","idx","tot","hit") # hIdx:Zoid History
			scoreMtx <- matrix( 0 ,nrow=nR ,ncol=length(cName) )
			colnames(scoreMtx) <- cName
			scoreMtx[,"hIdx"]	<- rMtx[,"hIdx"]
			scoreMtx[,"idx"]	<- 1:nR
			
			# -----------------------------------------------------------------
			# 명중률 시뮬레이션을 위한 반복 테스트
			#	물론, t04_mlpFit.t04Flag() 반복실행에 따른 명중률 편차계산도 필요.
			myFLog("Start t04_mlpFit simulation",pLogFile) 
			rLst <- list()
			mlp.fit <- NULL			
			for( setIdx in 1:pSimulSetNum ){
				myFLog(sprintf("SimulSet %d",setIdx),pLogFile)
				scoreMtx[,] <- 0
				for( tIdx in 1:pSimulNum ){

					testIdx <- sample( 1:nR ,pSampleSize )		
					mlp.fit <- mlp(	x=rMtx[-testIdx,pT04Flag$col.x]
										,y=rMtx[-testIdx,"outVal"]
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

					mlp.pred <- predict( mlp.fit ,rMtx[testIdx,pT04Flag$col.x] )
					hit <- which( ifelse(mlp.pred>0.5,1,0) == rMtx[testIdx,"outVal"] )
					
					scoreMtx[testIdx,"tot"] <- scoreMtx[testIdx,"tot"]+1
					scoreMtx[testIdx[hit] ,"hit" ] <- scoreMtx[testIdx[hit] ,"hit" ] + 1

					if( (tIdx%%10)==0 ){ 
						myFLog(sprintf("  simulate %dth of %d(%s)",tIdx,pSimulNum,pT04Flag$idStr),pLogFile) 
					}

				} # for

				myFLog(sprintf("   captured sample:%d of %d",sum(scoreMtx[,"tot"]>0),nrow(scoreMtx)),pLogFile)
				
				rLst[[setIdx]] <- list()
				rLst[[setIdx]]$rate <- apply( scoreMtx ,1 ,function(p){ifelse(p["tot"]==0,NA,p["hit"]/p["tot"])} )
				names(rLst[[setIdx]]$rate) <- rownames(rMtx)
				
			} # for(setIdx)

			# 테스트 영역 없는, 시뮬레이션 영역 전체를 대상으로 하는 Fit
			myFLog("Last mlp fit for all simulation range",pLogFile)
			rObj$mlp.fit <- mlp(	x=rMtx[,pT04Flag$col.x]	,y=rMtx[,"outVal"]
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
			
			rateMtx		<- sapply( rLst ,function(p){p$rate} )
			rObj$rate.count	<- apply( rateMtx ,1 ,function(p){sum(!is.na(p))} )
			rObj$rate.var	<- apply( rateMtx ,1 ,var ,na.rm=T )	# 값이 2개 이상일때만 NA가 아님.

			rate.mean	<- apply( rateMtx ,1 ,mean ,na.rm=T )
			rate.mean[is.nan(rate.mean)] <- 0.5	# NaN은 예측치로서 사용불가이므로 0.5확률 적용.
			rObj$rate.mean	<- rate.mean

			return( rObj )
		} # t04_mlpSimul.t04Flag

#----------------------------------------------------
#	Predictor 생산
t04_predr <- function( pT04Flag, ... ){
			UseMethod("t04_predr")
		}
t04_predr.t04Flag <- function( pT04Flag ,pSimul ,pNnOpt=NULL ,pHighCut=0.85 ,pLowCut=0.15
							,pLogFile=T04.logFile 
						)
		{

			rObj <- list( logFile=pLogFile )
			class(rObj) <- "t04_predr_t04Flag"
			rObj$highCut <- pHighCut;	rObj$lowCut <- pLowCut
			rObj$col.x <- pT04Flag$col.x

			# 명중률 매핑
			# pSimul$rate.var 에 대한 레보트 산출 및 필요시 제거
			cName <- c( "highlow" ,"rate" ,"var" ,"hIdx" ,"outVal" ,pT04Flag$col.x )
			predMtx <- matrix( 0 ,ncol=length(cName) ,nrow=0 )

			indices <- which( pSimul$rate.mean > pHighCut )
			mtx <- pT04Flag$rMtx[indices,c("hIdx" ,"outVal",pT04Flag$col.x)]
			mtx <- cbind( pSimul$rate.var[indices] ,mtx )
			mtx <- cbind( pSimul$rate.mean[indices] ,mtx )
			mtx <- cbind( rep("high",nrow(mtx)) ,mtx )
			predMtx <- rbind( predMtx ,mtx )

			# QQE : low 영역을 제외시킬 지 검토. (반드시 안 맞는다는 것도 아니다.)
			indices <- which( pSimul$rate.mean < pLowCut )
			mtx <- pT04Flag$rMtx[indices,c("hIdx" ,"outVal",pT04Flag$col.x)]
			mtx <- cbind( pSimul$rate.var[indices] ,mtx )
			mtx <- cbind( pSimul$rate.mean[indices] ,mtx )
			mtx <- cbind( rep("low",nrow(mtx)) ,mtx )
			predMtx <- rbind( predMtx ,mtx )

			# QQE var 값이 크거나 NA인 것들도 제외시켜야 할 지?
			
			colnames(predMtx) <- cName
			rObj$predMtx <- predMtx

			# 예측용 NN생성
			rObj$mlpPred <- pSimul$mlp.fit

			# 결과값 예상용, 명중률 예상용.
			#	예측 성능에 대한 레포트 산출 필요.
			myFLog("creating a mlp fit for hit rate prediction.")
			rObj$mlpRate <- mlp(	x=pT04Flag$rMtx[pSimul$simulRange,pT04Flag$col.x]	
						,y=pSimul$rate.mean
						,size=pNnOpt$size				,maxit=pNnOpt$maxit
						,initFunc=pNnOpt$initFunc		,initFuncParams=pNnOpt$initFuncParams
						,learnFunc=pNnOpt$learnFunc		,learnFuncParams=pNnOpt$learnFuncParams
						,updateFunc=pNnOpt$updateFunc	,updateFuncParams=pNnOpt$updateFuncParams
						,hiddenActFunc=pNnOpt$hiddenActFunc
						,shufflePattern=pNnOpt$shufflePattern
						,linOut=F
					)


			rObj$pred <- function( pXVal ){
			
						rPredObj <- list()
						
						# rObj$predMtx 에 일치하는 목록 조사.
						found <- apply( rObj$predMtx ,1 ,function(p){
											return( all(p[rObj$col.x]==pXVal) )
										}
									)
									
						if( any(found) ){
							
							rVec <- NULL
							rMtx <- rObj$predMtx[found,]				
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
						
						rPredObj$hIdx	<- NA	# NN추측이므로 hIdx는 없다.
						rPredObj$outVal <- predict( rObj$mlpPred ,pXVal )
						rPredObj$typ	<- "ANN"
						rPredObj$rate	<- predict( rObj$mlpRate ,pXVal )
						rPredObj$var	<- NA
									
						return( rPredObj )
					}

			return( rObj )
		}



t04_report <- function( pObj ,... ) {
			UseMethod("t04_report")
		}

		
t04_report.t04_mlpSimul_t04Flag <- function( pObj ,pFlag ,pReportFile=NULL ,pLogFile=T04.logFile ){

			logStr <- function( pStr ){
							myFLog( pStr ,fileName.txt ,pLogTime=F )
						}
			graphFileName <- function( pFileNo ,pExt="pdf"){ 
								return(sprintf("%s%02d.%s",pReportFile,pFileNo,pExt)) 
							}

			rObj <- list()
							
			if( is.null(pReportFile) ){
				pReportFile <- "t04_report.t04_mlpSimul_t04Flag"
				logStr(sprintf("t04_report.t04_mlpSimul_t04Flag:%s",pReportFile))
			}
			
			graphFileNo <- 0
			fileName.txt <- sprintf("%s.txt",pReportFile)			
			myFLog( sprintf("REPORT:%s(%s)",class(pObj),pFlag$idStr) ,fileName.txt ,pAppend=F )
			
			logStr("[NN options]----------");	print("[NN options]")
			logStr( t04_str(pObj$nnOpt) );	print(sprintf("size:%s  maxit:%d",paste(pObj$nnOpt$size,collapse=","),pObj$nnOpt$maxit))

			logStr("[NN results]----------");	print("[NN result]")
			maxit <- length(pObj$mlp.fit$IterativeFitError)
			mlp.fit <- pObj$mlp.fit
			rStr <- sprintf("    Iteration : %f(last), %f(min), %f(last/min)"
							,mlp.fit$IterativeFitError[maxit],min(mlp.fit$IterativeFitError)
							,mlp.fit$IterativeFitError[maxit]/min(mlp.fit$IterativeFitError)
						)
			logStr(rStr);	print(rStr)
			
			lastQStart <- (maxit*3)/4
			lastErrDf <- data.frame( it=lastQStart:maxit ,err=mlp.fit$IterativeFitError[lastQStart:maxit] )
			lm.fit <- lm( err~it ,data=lastErrDf )
			slope <- coef(lm.fit)["it"]
			rStr <- sprintf("    slope:%f (%f changed in last 4/4 area.)" ,slope ,slope*nrow(lastErrDf) )
			logStr(rStr);	print(rStr)
			
			graphFileNo <- graphFileNo+1
			pdf(graphFileName(graphFileNo))	# 그래프 출력용 PDF
			par( mfrow=c(2,1) )
			plot( mlp.fit$IterativeFitError		,main="Iterative Fit Error(ALL)")
			plot( lastQStart:maxit ,mlp.fit$IterativeFitError[lastQStart:maxit]	,main="Iterative Fit Error(4/4Q)")


			predr <- t04_predr( pFlag ,pObj ,pNnOpt=pObj$nnOpt ,pLogFile=T04.logFile )

			par( mfrow=c(2,2) )
			# -[Assessment within simulRange]-------------------------------------------
            rStr <- "    Assessment within simulRange"
			logStr(rStr);	print(rStr)
			testMtx <- pFlag$rMtx[ pObj$simulRange,pFlag$col.x ]
			assLst <- apply( testMtx ,1 ,function(p){ return(predr$pred(p)) })
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
			realOut <- pFlag$rMtx[pObj$simulRange,"outVal"]
			predOut <- ifelse( assDf$outVal>0.5 ,1 ,0 )
			assDf$isHit <- realOut==predOut
			rStr <- sprintf("        Hit : %d of %d",sum(realOut==predOut),length(predOut))
			logStr(rStr);	print(rStr)
			
			freq <- table(pFlag$flag[pObj$simulRange]) # QQE 버그. flag 인덱스가 아님
			rStr <- sprintf("        Freq(0vs1) : %d vs %d(%f)"
							,freq["0"] ,freq["1"] ,ifelse(min(freq)==0,NA,max(freq)/min(freq)) 
						)
			logStr(rStr);	print(rStr)

			boxplot( assDf$rate[assDf$isHit] ,assDf$rate[!assDf$isHit] 
						,names=c("hit","miss")	,ylim=c(0,1)
						,ylab="Predicted Hit Rate by Ann" 
						,main="Within SimulRange"
						)

			hist( which(assDf$isHit) ,main="Hit position" ,xlab="Z history" )

			cName <- c("High rate hit","Low rate hit(Inverted.)")
			rName <- c("Total","Hit count")
			m.hitMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(cName) )
			colnames(m.hitMtx) <- cName;	rownames(m.hitMtx) <- rName
			m.highIdx <- which(assDf$m.typ == "high")
			m.lowIdx <- which( assDf$m.typ == "low" )
			m.hitMtx[1,] <- c( length(m.highIdx) ,length(m.lowIdx) )
			m.hitMtx[2,] <- c( sum(assDf$isHit[m.highIdx]) ,sum(!assDf$isHit[m.lowIdx]))
			barplot( m.hitMtx ,beside=T ,legend=rName ,col=heat.colors(length(rName)) 
						,main="(Within SimulRange)"
					)
			hitMtx <- table( predOut ,realOut )		#		   realOut
													#	predOut  0  1
													#		  0 14  8
													#		  1 29 14
			lName		<- sprintf("0: %d/%d hit(%.4f)" ,hitMtx["0","0"] ,sum(hitMtx[,"0"]) 
										,ifelse(sum(hitMtx[,"0"])==0,NA,hitMtx["0","0"]/sum(hitMtx[,"0"])) )
			lName[2]	<- sprintf("1: %d/%d hit(%.4f)" ,hitMtx["1","1"] ,sum(hitMtx[,"1"]) 
										,ifelse(sum(hitMtx[,"1"])==0,NA,hitMtx["1","1"]/sum(hitMtx[,"1"])) )
			barplot( hitMtx ,beside=T ,legend=lName )
			logStr(hitMtx);	print(hitMtx)


			# -[Assessment beyond simulRange]-------------------------------------------
            rStr <- "    Assessment beyond simulRange"
			logStr(rStr);	print(rStr)
			trainMtx <- pFlag$rMtx[-pObj$simulRange,pFlag$col.x]
			assLst <- apply( trainMtx ,1 ,function(p){ return(predr$pred(p)) })
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
			realOut <- pFlag$rMtx[-pObj$simulRange,"outVal"]
			predOut <- ifelse( assDf$outVal>0.5 ,1 ,0 )
			assDf$isHit <- realOut==predOut
			rObj$assDf.simulRange <- assDf
			
			rStr <- sprintf("        Hit : %d of %d",sum(realOut==predOut),length(predOut))
			logStr(rStr);	print(rStr)

			freq <- table(pFlag$flag[-pObj$simulRange]) # QQE 버그. flag 인덱스가 아님
			rStr <- sprintf("        Freq(0vs1) : %d vs %d(%f)"
							,freq["0"] ,freq["1"] ,ifelse(min(freq)==0,NA,max(freq)/min(freq)) 
						)
			logStr(rStr);	print(rStr)
			

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
			hitMtx <- table( predOut ,realOut )		#		   realOut
													#	predOut  0  1
													#		  0 14  8
													#		  1 29 14
			lName		<- sprintf("0: %d/%d hit(%.4f)" ,hitMtx["0","0"] ,sum(hitMtx[,"0"]) 
										,ifelse(sum(hitMtx[,"0"])==0,NA,hitMtx["0","0"]/sum(hitMtx[,"0"])) )
			lName[2]	<- sprintf("1: %d/%d hit(%.4f)" ,hitMtx["1","1"] ,sum(hitMtx[,"1"]) 
										,ifelse(sum(hitMtx[,"1"])==0,NA,hitMtx["1","1"]/sum(hitMtx[,"1"])) )
			barplot( hitMtx ,beside=T ,legend=lName )
			logStr(hitMtx);	print(hitMtx)

			rObj$assDf <- assDf
			rObj$m.hitMtx <- m.hitMtx

			myFLog( "end of Report." ,fileName.txt )
			dev.off()
			
			return( rObj )
		}

t04_str <- function( pObj ,... ){
			UseMethod("t04_str")
		}

t04_str.nnOpt.mlpOption <- function( pObj ){

			rStr				 <- sprintf("size:%s maxit:%d",paste(pObj$size,collapse=",") ,pObj$maxit)
			rStr[length(rStr)+1] <- sprintf("initFunc:%s initFuncParams:%s",pObj$initFunc,paste(pObj$initFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("learnFunc:%s learnFuncParams:%s",pObj$learnFunc,paste(pObj$learnFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("updateFunc:%s updateFuncParams:%s",pObj$updateFunc,paste(pObj$updateFuncParams,collapse=","))
			rStr[length(rStr)+1] <- sprintf("hiddenActFunc:%s",pObj$hiddenActFunc)
			rStr[length(rStr)+1] <- sprintf("shufflePattern:%s linOut:%s",pObj$shufflePattern,pObj$linOut)

			return( rStr )
		}

t04Flag <- function( pFlag ,pIdStr ,pStart=50 )
		{
			rLst <- list( )
			class(rLst) <- c("t04Flag")
			rLst$idStr <- pIdStr
			rLst$col.x <- c("preVal" ,"seqNum1" ,"seqNum2" ,"seqNum3" ,"seqNum4" ,"seqNum5" ,"seqNum6")

			rLst$flag <- pFlag

			lSpan <- pStart:length(pFlag)
			rLst$flag.start = pStart
			
			cName <- c( "hIdx" ,"outVal" ,rLst$col.x )
			rMtx <- matrix( 0 ,nrow=length(lSpan) ,ncol=length(cName) )
			colnames(rMtx) <- cName
			for( lIdx in lSpan ){
				# lIdx <- 25
				sInfo <- t03.seq( pFlag[1:(lIdx-1)] )
				nR <- nrow(sInfo$seqCntMtx)
				if( nR < 6 ){
					errMsg <- sprintf( "row number of seqCntMtx is less than 6.(not available data)")
					print(errMsg)
					myFLog(errMsg)
					return( NULL )
				}
				mtx <- sInfo$seqCntMtx[(nR-(6-1)):nR,]	# row가 충분치 않은 경우에 대한 검토필요
				seqNum <- mtx[,"cnt"]*ifelse(mtx[,"val"]==1,1,-1)
				rMtx[(lIdx-lSpan[1])+1,] <- c( lIdx ,pFlag[lIdx] ,pFlag[lIdx-1] ,seqNum )
			}
			rLst$rMtx <- rMtx
			rownames(rLst$rMtx) <- as.character(rMtx[,"hIdx"])
			
			rLst$sameRow <- sameRow( rLst$rMtx ,rLst$col.x )
			
			return( rLst )
		}		

#----------------------------------------------------
#	DNA의 지정된 컬럼(pCol)이 pNum의 배수이면 1
#		rMtx의 idx는 pStart 지점부터 시작된다.
#			처음부터 pStart 까지의 구간에서 
#			배수인 경우와 아닌 경우가 5번 이상 변경이 있어야 한다.
t04Flag.multiple <- function( pDnaMtx ,pColNum ,pNum ,pStart=50 ){

			flag <- ifelse( pDnaMtx[,pColNum]%%pNum==0 ,1 ,0 )
			idStr <- sprintf("t04Flag.multiple.%d.%d",pColNum,pNum)
			rLst <- t04Flag( flag ,idStr )
			class( rLst ) <- c( class(rLst) ,"t04Flag.multiple" )

			return( rLst )
		}
