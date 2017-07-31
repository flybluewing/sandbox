# ==========================================================
#	범용 유전 알고리즘 프레임
# ==========================================================

# -----------------------------------------------------------
#	자식 생성 함수 : 랜덤에 의해 생성.
#		- pUseNA : DNA 코드가 없는 것 허용.
#				DNA 크기가 고정되지 않은 것을 표현하기 위해
#		- pExcept : 중복을 피해야 할 자식들.
#				또한 신규 자식들은 pExcept에서 가장 덜 사용된
#				DNA 코드를 우선해서 생성된다.
#		- pDupChild : 중복된 child 존재를 허용할 것인지.
#		- 추가고려 사항 : 발생가능 Random 경우의 수 없을 때 수단.
gE.childRandom <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T ,pDupChild=T
						)
	{
		rObj <- NULL
		if( pDupChild ){
			rObj <- gE.childRandom.dup( pDnaType=pDnaType ,pDnaSize=pDnaSize ,pChildSize=pChildSize
							,pUseNA=pUseNA ,pExceptMtx=pExceptMtx ,pReplace=pReplace
						)
		} else {
			rObj <- gE.childRandom.Ndup( pDnaType=pDnaType ,pDnaSize=pDnaSize ,pChildSize=pChildSize
							,pUseNA=pUseNA ,pExceptMtx=pExceptMtx ,pReplace=pReplace
						)
		}
		return( rObj )
	}
#	중복 child 허용
gE.childRandom.dup <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T
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
			rMtx[cIdx,] <- dna
		} # for cIdx
		
		return( rMtx )

	}

#	중복 child 허용하지 않는 경우.
gE.childRandom.Ndup <- function( pDnaType ,pDnaSize ,pChildSize 
							,pUseNA=T ,pExceptMtx=NULL ,pReplace=T
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
						k.FLog(sprintf("[Warn] gE.childRandom : same data : %s",paste(dna,collapse=" ")))
				}
			} # for(idx)
			
			if( (idx==cMax)&&(cMax>1) ){
				idx.fail <- c( idx.fail ,cIdx )
				k.FLog(sprintf("[Warn] gE.childRandom : reaches max(%d)",cIdx)
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
gE.childMate <- function( pChildMtxA ,pChildMtxB )
	{
		mate <- function( pDnaA ,pDnaB ,pLength ,pSwichNum ){
				swichIdx <- sample( 1:pLength ,pSwichNum )
				rDna <- pDnaA
				rDna[swichIdx] <- pDnaB[swichIdx]
				return(rDna)
			}

			
		rMtx <- pChildMtxA # 어차피 교배결과가 담기므로 초기값 의미없음.
		lnth <- ncol(pChildMtxA)
		swichNum <- sample( 1:lnth ,nrow(pChildMtxA) ,replace=T )
		for( idx in 1:nrow(pChildMtxA) ){
			child <- mate( pChildMtxA[idx,] ,pChildMtxB[idx,] 
							,pLength=lnth ,pSwichNum=swichNum[idx]
						)
			rMtx[idx,] <- child
		}

		return( rMtx )
	}

# -----------------------------------------------------------
#	자식 생성 함수 : 돌연변이
gE.childMutate <- function( pChildMtx ,pDnaType )
	{
		mut.idx <- sample( 1:ncol(pChildMtx) ,nrow(pChildMtx) ,replace=T )
		mut.val <- sample( pDnaType ,nrow(pChildMtx) ,replace=T )

		for( rIdx in 1:nrow(pChildMtx) ){
			pChildMtx[rIdx,mut.idx[rIdx]] <- mut.val[rIdx]
		}
		
		return( pChildMtx )
	}

	
# -----------------------------------------------------------
#	신규 종(Species) 선언.
#		- pUseDna.NA : DNA 코드에 없음 선언 가능.
#		- pAsF : "assF.rank", "assF.mean"
gE.defineSpecies <- function( pPop=100 ,pDnaType ,pClassName="gE" 
								,pDnaSize=10	,pUseDna.NA=T	,pDnaReplace=T
								,pAsF="assF.rank"
								,pAsIndiF="assIndiF.mOpt"
								,pAsIndiF_Opt=NULL # 전체 합계는 100이어야 함.
								,pDupChild=T
							)
	{

		rObj <- list( pop=pPop ,dnaType=pDnaType
						,dnaSize=pDnaSize	,useDna.NA=pUseDna.NA	,dnaReplace=pDnaReplace
					)
		class(rObj) <- pClassName
		rObj$survive	<- 10 # 부모유전자 그대로 계승 비율 %
		rObj$children	<- 70 # 교접에 의한 신규 자식생성비율 %
		rObj$random		<- 20 # 랜덤하게 탄생한 자식 비율 %
		rObj$mutant		<-  5 # 돌연변이 발생 비율 %
		rObj$dupChild	<- pDupChild # 동일 자식 생성 허용여부
		
		# 자식 생성 (디폴트)함수들.
		#	필요 시 Species를 선언한 후 함수를 바꾸도록 한다.
		rObj$childRandom	<- gE.childRandom
		rObj$childMate		<- gE.childMate
		rObj$childMutate	<- gE.childMutate
		
		# 코드 값 평가 함수 선택
		#	실제 GE에서의 평가에서는 rObj$assF()이 사용된다.
		#	다만 차후 재활용을 위해 rObj$assF.rank(), rObj$assF.mean()등등도 외부 노출시켜 둠.
		rObj$assF.rank <- function( p ,... ){ 100*rank(p)/length(p) }
		rObj$assF.mean <- function( p ,pSize ,...){ p/pSize }
			# pSize : score를 제공한 수
				
		if( pAsF == "assF.rank" ){
			rObj$assF <- rObj$assF.rank
		} else if( pAsF == "assF.mean" ) {
			rObj$assF <- rObj$assF.mean
		} else {
			rObj$assF <- NULL
		}
		
		# 개체 적합도 평가 함수 선택
		#	rObj$assIndiF.stdOnly( ) : 정답 코드의 평가값 만으로 개체의 적합도 평가.
		rObj$assIndiF.stdOnly <- function( pOrd , ... ){
										std.loc <- length(pOrd[[1]])
										std.prop <- sapply(pOrd ,function(p){p[std.loc]})
										rObj <- list( finalScore = mean(std.prop) )
										rObj$std.prop <- std.prop
										return( rObj )
									}

		rObj$assIndiF_Opt <- pAsIndiF_Opt
		if( is.null(rObj$assIndiF_Opt) ){
			rObj$assIndiF_Opt <- c( 30 ,25 ,5 ,40 ) # 전체 합계가 100이어야 한다.
		}
		names(rObj$assIndiF_Opt) <- c( "score.stdAssMean" ,"score.nearDiff" ,"score.stepNum" ,"score.monopoly")
		rObj$assIndiF.mOpt <- function( pOrd ,pNearNum=4 ,pAsIndiF_Opt ,... ){
										# pAsIndiF_Opt 값의 합계는 100이어야 한다.
										std.loc <- length(pOrd[[1]])

										nearNum <- ifelse(pNearNum>std.loc,stdLoc,pNearNum)
										spcLst <- lapply(pOrd ,function(p){
															rObj <- list( )
															rObj$ass.std <- p[std.loc] # 0~100
															rObj$stepNum <- length(unique(p))

															d <- p[-std.loc]-p[std.loc]
															d <- sort(abs(d))[1:nearNum]
															rObj$nearDiff <- mean(d)
															return(rObj)
														})

										score.stdAss <- sapply(spcLst,function(p){p$ass.std})
										score.winStepTbl <- sort(table(score.stdAss %/% 5 ) ,decreasing=T )
										winStep.top <- ifelse( length(score.winStepTbl)>4 ,4 ,length(score.winStepTbl) )
										score.monopoly <- sum(score.winStepTbl[1:winStep.top])*100/sum(score.winStepTbl)
											# 0 ~ 100

										score.stdAssMean <- mean(score.stdAss)
										score.nearDiff <- mean(sapply(spcLst,function(p){p$nearDiff}))
										score.stepNum <- mean(sapply(spcLst,function(p){p$stepNum}))

										# 평가값 비율 배분.

										rName <- c("option","rawVal","finalVal")
										sMtx <- matrix( 0 ,nrow=length(rName) ,ncol=length(pAsIndiF_Opt)
																,dimnames=list(rName,names(pAsIndiF_Opt)) )
										sMtx["option",] <- pAsIndiF_Opt
										sMtx["rawVal","score.monopoly"] <- sum(score.winStepTbl[1:winStep.top])*100/sum(score.winStepTbl)
										sMtx["rawVal","score.stdAssMean"] <- mean(score.stdAss)
										sMtx["rawVal","score.nearDiff"] <- mean(sapply(spcLst,function(p){p$nearDiff}))
										sMtx["rawVal","score.stepNum"] <- mean(sapply(spcLst,function(p){p$stepNum}))

										sMtx["finalVal","score.monopoly"] <- sMtx["rawVal","score.monopoly"]*(pAsIndiF_Opt["score.monopoly"]/100)
										sMtx["finalVal","score.stdAssMean"] <- sMtx["rawVal","score.stdAssMean"]*(pAsIndiF_Opt["score.stdAssMean"]/100)
										sMtx["finalVal","score.nearDiff"] <- ifelse(sMtx["rawVal","score.nearDiff"]>pAsIndiF_Opt["score.nearDiff"]
														,pAsIndiF_Opt["score.nearDiff"] ,sMtx["rawVal","score.nearDiff"] )
										sMtx["finalVal","score.stepNum"] <- sMtx["rawVal","score.stepNum"]*(pAsIndiF_Opt["score.stepNum"]/100)

										rObj <- list( finalScore = sum(sMtx["finalVal",]) )
										rObj$sMtx <- sMtx
										return( rObj )
									}

		if( pAsIndiF=="assIndiF.stdOnly" ){
			rObj$assIndiF <- rObj$assIndiF.stdOnly
		} else if( pAsIndiF=="assIndiF.d702505" ){
			rObj$assIndiF <- rObj$assIndiF.d702505
		} else if( pAsIndiF=="assIndiF.mOpt" ){
			rObj$assIndiF <- rObj$assIndiF.mOpt
		} else {
			rObj$assIndiF <- NULL
		}
		
		return(rObj)
	}

# ==========================================================
#	Generic Function
#		- createPool() 에는 assessPool()에서의 가치평가 위해 
#							필요한 정보들이 공급되어야 한다.
#		- rptIndividual() 개체에 대한 Report
gE.createPool	<- function( pObj ,... ){	UseMethod("gE.createPool") }
gE.assessPool	<- function( pObj ,... ){	UseMethod("gE.assessPool") }
gE.nextGen		<- function( pObj ,... ){	UseMethod("gE.nextGen") }
gE.rptIndividual	<- function( pObj ,... ){ UseMethod("gE.rptIndividual") }
gE.rptPool		<- function( pObj ,... ){	UseMethod("gE.rptPool") }


# ==========================================================
#		- pObj : Species define
gE.createPool.miniS <- function( pObj ,pRstLst ){
		rObj <- list( rstLst=pRstLst )
		rstMtx <- rObj$rstLst[[1]]
		rObj$h <- unique( rstMtx[,"hist"] )
		rObj$dnaCode <- rstMtx[rstMtx[,"hist"]==rObj$h[1],"val"]
			# 코드의 맨 마지막에는 각 년도의 정답 코드가 담겨있다.
			# 따라서 rObj$dnaCode 맨 마지막 코드 값 자체는 신경쓰지 말자.
			# (자주 나오지 않는 코드가 정답인 경우를 처리하기 위해)
			
		# 여기서의 로직은 gE.nextGen.miniS() 함수에서도 유지되어야 한다.
		rObj$childMtx <- pObj$childRandom( 
									 pDnaType=pObj$dnaType
									,pDnaSize=pObj$dnaSize
									,pChildSize=pObj$pop
									,pUseNA	= pObj$useDna.NA
									,pReplace = pObj$dnaReplace
									,pDupChild = pObj$dupChild
								)
		return( rObj )
	}

gE.assessPool.miniS <- function( pObj ,pBaseScore = 10000 ){
		# pBaseScore : 평가값이 높을 수록 좋은 우수한 것이라는 정책을 일관성있게 유지하기 위해 사용.
		#			평가 기준, 즉 assessRstMtx() 함수를 어떻게 구성하느냐에 따라서
		#			값이 낮은 것일수록 좋은 경우가 있는데, 이렇게 되면 nextGen() 함수도 바뀌어야 한다.
		#			이런 경우, 다른 함수가 바뀔 필요가 없게하기 위해 pBaseScore/평가값을 최종 평가값으로하면
		#			평가값 우선순위를 일관성 있게 유지할 수 있다.
		#			(즉, assessRstMtx함수 반환값이 클수록 좋은 상태라면 pBaseScore는 불필요.)

		# pObj <- mSPool
		childMtx <- pObj$pool$childMtx
		rstLst <- pObj$pool$rstLst
		child.ass <- rep( 0 ,pObj$pop )

		accumMtx <- rstLst[[1]]	# score 총합저장
			# accumMtx와 childMtx는 직접적 관계가 없을 수도 있다. 평가값들의 총합이 반드시 개체적합도 평가는 아니므로.
		for( rIdx in 1:pObj$pop ){
			# rIdx <- 2
			dna <- childMtx[rIdx,]
			dna <- dna[!is.na(dna)]	# 모든 원소 Null은 없겠지?
			
			scoreMtx <- mapply(function(p){rstLst[[p]][,"score"]},dna)
			accumMtx[,"score"] <- apply(scoreMtx,1,sum)
			# 년도별 정답들의 order % 확인
			# 	ord의 각 원소의 맨 마지막 값은 정답이 갖는 확률이 된다.
			ord <- tapply( accumMtx[,"score"] ,list(accumMtx[,"hist"]) 
							,function(p,pSize){ pObj$assF(p,pSize) } 
							,pSize=length(dna)
						)

			# child.ass[rIdx] <- assessRstMtx( ord )
			child.ass[rIdx] <- pObj$assIndiF( ord ,pAsIndiF_Opt=pObj$assIndiF_Opt )$finalScore
		} # for

		rObj <- list( )
		rObj$ass.score	<- child.ass

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
	} # gE.assessPool.miniS


gE.nextGen.miniS <- function( pObj ){

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
		#	gE.createPool.miniS( ) 함수 참고.
		childMtx.random <- matrix(0,nrow=0,ncol=pObj$dnaSize)
		if( ass$random.n>0 ){
			childMtx.random <- pObj$childRandom( 
										 pDnaType=pObj$dnaType
										,pDnaSize=pObj$dnaSize
										,pReplace = pObj$dnaReplace
										,pChildSize=ass$random.n
										,pUseNA	= pObj$useDna.NA
										,pDupChild = pObj$dupChild
									)
		}
		
		# 나머지는 교배로 채움.
		#	survive와 random 에서의 중복제거 작업/옵션들로 인해 초기 크기보다 작을 수 있다.
		#	의도된 수량보다 적을 수가 있어서 nrow()함수로 다시 확인한다.
		#	(그런데 children,survive,random 을 모두 합친 후, 중복존재를 다시 확인해야 하기도 하다.)
		childMtx.child <- matrix(0,nrow=0,ncol=pObj$dnaSize)
		n.children <- pObj$pop - ( nrow(childMtx.survive) + nrow(childMtx.random) )
		if( n.children > 0 ){
			# gE.childMate <- function( pChildMtxA ,pChildMtxB 
			ord <- order( ass$ass.score )
			# ord <- ord[1:round(length(ord)*2/3)] # 혹시 우수객체만 교배후보로 한다면...
			ord.prob <- rep(1,length(ord))	# 교배 선택 확률.
			ord.prob[1:round(length(ord)*2/3)] <- 2
			ord.prob[1:round(length(ord)/3)] <- 3
			xx <- sample(ord ,n.children ,replace=T ,prob=ord.prob)
			xy <- sample(ord ,n.children ,replace=T ,prob=ord.prob)
			childMtx.child <- gE.childMate( childMtx[xx,] ,childMtx[xy,] )
			
			if( !pObj$dnaReplace ){
				for( rIdx in 1:nrow(childMtx.child) ){
					dupIdx <- sameVal.Jidx( childMtx.child[rIdx,] )
					if( 0<length(dupIdx) ){
						# 사실 코드에서 NA를 허용치 않는 경우에 대해서는.. 다른 방안이 필요하다.
						childMtx.child[rIdx,dupIdx] <- NA
					}
				} # for
			}
			
		} # if( n.children > 0 )

		# 돌연변이 적용(돌연변이로 인한 중복코드 발생은 그려려니 하자... 돌연변이잖음?)
		childMtx.r <- rbind( childMtx.survive ,childMtx.child )
		mut.idx <- sample( 1:nrow(childMtx.r) ,pObj$ass$mutant.n )
		childMtx.r[mut.idx,] <- gE.childMutate( childMtx.r[mut.idx,,drop=F] ,pObj$dnaType )

		childMtx.r <- rbind( childMtx.r ,childMtx.random )
		
		return( childMtx.r )

	} # gE.nextGen.miniS

gE.rptPool.miniS <- function( pObj ,pRptFile="./report/Pool" ,pAuxMsg="" ){

		# Report File
		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)

		pdf( log.pdf )
		par( mfrow=c(4,2) )
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
	
		assOrd <- function( pOrd ){
								std.loc <- length(pOrd[[1]])
								std.prop <- sapply(pOrd ,function(p){p[std.loc]})
								q <- quantile(std.prop)

								# 특이값 제거를 위해 q["25%"]와 q["75%"] 구간의 var 측정
								tgtFlag <- (q["25%"]<=std.prop)&(q["75%"]>=std.prop)
								rVal <- c(mean(std.prop[tgtFlag]),var(std.prop[tgtFlag]))
								names(rVal) <- c("mean","var")
								return( rVal )
							}

		# Start
		FLogStr("-[Pool Report]-------------------------------",pTime=T,pAppend=F)

		FLogStr(sprintf("  Log File:%s",log.txt),pConsole=T)
		FLogStr(sprintf("  AUX Msg:%s",pAuxMsg))

		# Species & Pool report
		FLogStr("<Species>")
		FLogStr(sprintf("  pop : %d  dnaSize : %d  useDna.NA : %s",pObj$pop,pObj$dnaSize,ifelse(pObj$useDna.NA,"T","F")))
		FLogStr(sprintf("  dnaType(selection) : %s ",paste(pObj$dnaType,collapse=" ")))
		FLogStr(sprintf("  Gen : survive %d,  children %d,  random %d,  mutant %d"
				,pObj$survive,pObj$children,pObj$random,pObj$mutant))
		FLog( pObj$assIndiF_Opt )
				
		FLogStr("<Pool>")
		FLogStr(sprintf("  Predict Target : %s (개체의 반응 대상)",paste(pObj$pool$dnaCode ,collapse=" ")))
		FLogStr(sprintf("  rstLst length : %d",length(pObj$pool$rstLst)))
		hRange <- range(pObj$pool$h)
		FLogStr(sprintf("  History : %d ~ %d",hRange[1],hRange[2]))
		chMtxDim <- dim(pObj$pool$childMtx)
		FLogStr(sprintf("  Child Matrix : %d X %d",chMtxDim[1],chMtxDim[2]))

		FLogStr("<Assessment>")
		hisIdx <- pObj$pool$rstLst[[1]][,"hist"] # tapply() 를 위해 미리 추출해놓음.
		stdVal <- tapply( pObj$pool$rstLst[[1]][,"val"] ,hisIdx ,function(p){ p[length(p)] } )

		cName <- c( "idx","mean(all)","var(all)","mean(core)","var(core)")
		mvMtx <- matrix( NA ,ncol=length(cName) ,nrow=length(pObj$ass$survive.idx) )
		colnames(mvMtx) <- cName
		mvMtx[,"idx"] <- pObj$ass$survive.idx
		assLst <- list()
		for( idx in 1:length(pObj$ass$survive.idx) ){
			# idx <- 1
			iIdx <- pObj$ass$survive.idx[idx] # individual idx
			indName <- sprintf("%d th individual(%d/%d)",iIdx,idx,length(pObj$ass$survive.idx))
			sDna <- sort( pObj$pool$childMtx[iIdx,] ) # NA는 자동제거됨.
			scoreMtx <- mapply( function(p){pObj$pool$rstLst[[p]][,"score"]} ,sDna )
			score <- apply( scoreMtx ,1 ,sum )
			ord.arr <- tapply( score ,hisIdx ,function(p,pSize){ pObj$assF(p,pSize) } ,pSize=length(sDna))
			assLst[[idx]] <- pObj$assIndiF( ord.arr ,pAsIndiF_Opt=pObj$assIndiF_Opt )

			ord.byHist <-sapply( ord.arr ,function(p){p[length(p)]} )
			mvMtx[idx,"mean(all)"]	<- mean(ord.byHist)
			mvMtx[idx,"var(all)"]	<- var(ord.byHist)

			# 특이값 제거를 위한 25~75% 구간 검토.
			ord.q <- quantile(ord.byHist)
			ord.nArea.flag <- (ord.q["25%"]<=ord.byHist)&(ord.q["75%"]>=ord.byHist)
			mvMtx[idx,"mean(core)"]	<- mean(ord.byHist[ord.nArea.flag])
			mvMtx[idx,"var(core)"]		<- var(ord.byHist[ord.nArea.flag])

			varNum <- sapply(ord.arr,function(p){length(unique(p))}) # 평가값들이 얼마나 다양한지...(몰빵여부 감지용.)
			FLogStr(sprintf("* %s : %s",indName,paste(sDna,collapse=" ")))
			FLogStr(sprintf("          Assess Score : %f",assLst[[idx]]$finalScore))
			FLogStr(sprintf("          Mean:%f Var:%f (All area)"
						,mvMtx[idx,"mean(all)"],mvMtx[idx,"var(all)"] 
					))
			FLogStr(sprintf("          Mean:%f Var:%f (25~75%% area)"
						,mvMtx[idx,"mean(core)"],mvMtx[idx,"var(core)"] 
					))
			FLogStr(sprintf("          score varity : %f (min:%d max:%d)",mean(varNum),min(varNum),max(varNum)))

			hist( ord.byHist ,ylim=c(0,length(ord.byHist)/2)	,main=indName )
			plot( ord.byHist ,ylim=c(0,100)						,main=indName )
		}

		par( mfrow=c(2,2) )
		plot( mvMtx[,"mean(all)"]	,ylim=c(0,100) ,main="Mean for all area" )
		plot( mvMtx[,"mean(core)"]	,ylim=c(0,100) ,main="Mean for core" )
		plot( mvMtx[,"var(all)"]	,main="Var for all area" )
		plot( mvMtx[,"var(core)"]	,main="Var for core" )

		if( !is.null(pObj$scoreTrend) ){
			plot( pObj$scoreTrend, main="Score Trend" )
		}
		plot( table(stdVal) ,main="Std Val frequency" )

		assMtx.fv <- sapply(assLst ,function(p){p$sMtx["finalVal",]} )
		barplot( assMtx.fv ,beside=F ,col=rainbow(nrow(assMtx.fv)) ,legend=rownames(assMtx.fv) 
				,ylim=c(0,120) ,main="composition of score " )

		# end
		dev.off()
		FLogStr( "-[Pool Report]-------------------------------",pTime=T )
	}

# 특정 개체에 대한 Report( pIndIdx : 개체 인덱스 )
#	- pHistTerm : 명중률 밀도 파악을 위해 나누는 구역의 크기.
gE.rptIndividual.miniS <- function( pObj ,pIndIdx ,pRptFile="./report/Individual" ,pHistTerm=10 ,pAuxMsg="" ) {

		# Report File
		log.txt <- sprintf("%s_%d.txt",pRptFile,pIndIdx)
		log.pdf <- sprintf("%s_%d.pdf",pRptFile,pIndIdx)		
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		assVar <- function( pOrd ){ # 분산도 평가
								std.loc <- length(pOrd[[1]])
								std.prop <- sapply(pOrd ,function(p){p[std.loc]})
								q <- quantile(std.prop)

								# 특이값 제거를 위해 q["25%"]와 q["75%"] 구간의 var 측정
								tgtFlag <- (q["25%"]<=std.prop)&(q["75%"]>=std.prop)
								return( var(std.prop[tgtFlag]) )
							}

		# Start
		FLogStr("-[Individual Report]-------------------------------",pTime=T,pAppend=F)
		pdf( log.pdf )
		par(mfrow=c(3,1))
		FLogStr(sprintf("  Log File:%s",log.txt),pConsole=T)
		FLogStr(sprintf("  AUX Msg:%s",pAuxMsg))

		# Species & Pool report
		FLogStr("<Species and Pool>")
		FLogStr(sprintf("  Pool Size:%d",pObj$pop))
		FLogStr(sprintf("  survive:%d children:%d random:%d mutant:%d (adjusted for pop)"
					,pObj$ass$survive.n ,pObj$ass$child.n ,pObj$ass$random.n ,pObj$ass$mutant.n
				))

		# Individual Report
		FLogStr("<Individual>")

		idn.dna <- pObj$pool$childMtx[pIndIdx,]
		idn.dna <- idn.dna[!is.na(idn.dna)]
		FLogStr(sprintf( "  DNA:%s" ,paste(idn.dna,collapse=", " ) ))

		# 먼저, Ord를 얻어내야 함.
		rstLst <- pObj$pool$rstLst
		cmmMtx <- rstLst[[1]] # score 총계 계산용.
			# cmmRst의 각 hist 맨 마지막 Row는 정답 코드임을 유의하자.
		cmmMtx[,c("score","order")] <- 0	# 초기화

		score.mtx <- mapply(function(p){rstLst[[p]][,"score"]},idn.dna)
		cmmMtx[,"score"]<- apply(score.mtx,1,sum)
		ord.arr <- tapply(cmmMtx[,"score"] ,list(cmmMtx[,"hist"])
						,function(p,pSize){ pObj$assF(p,pSize) } 
						,pSize=length(idn.dna)
					)
		ord.byHist <-sapply( ord.arr ,function(p){p[length(p)]} )
		assObj <- pObj$assIndiF( ord.arr ,pAsIndiF_Opt=pObj$assIndiF_Opt )
		FLogStr(sprintf("  final Score:%f",assObj$finalScore))
		FLog( assObj$sMtx )
		barplot( assObj$sMtx ,beside=T ,ylim=c(0,110) ,legend=rownames(assObj$sMtx) 
			,main="composition of the final score" )

		# 특이값 제거를 위한 25~75% 구간 검토.
		ord.q <- quantile(ord.byHist)
		ord.nArea.flag <- (ord.q["25%"]<=ord.byHist)&(ord.q["75%"]>=ord.byHist)
		ord.byHist.core <- ord.byHist[ord.nArea.flag]

		FLogStr(sprintf("  mean : %f  var : %f (All area)",mean(ord.byHist),var(ord.byHist)),pConsole=T)
		FLogStr(sprintf("  mean : %f  var : %f (25~75%%)",mean(ord.byHist.core),var(ord.byHist.core)))

		plot(ord.byHist ,main="score by history")
		hist(ord.byHist,ylim=c(0,400) ,main="score distribution")

		# 구간 별 출현 빈도 파악.(ordMtx,freqMtx)
		cName <- c("ord","term")
		ordMtx <- matrix( 0 ,ncol=length(cName) ,nrow=length(ord.byHist) )
		colnames(ordMtx) <- cName
		rownames(ordMtx) <- names(ord.byHist)

		ordMtx[,"ord"] <- ord.byHist

		termIdx <- ord.byHist%/%pHistTerm
			#	맨 마지막 구간은 100% 항목이므로 100% 값만 세느라 빈도 수가 비정상적으로 작음. 이전 구간(90%구간 등)과 합쳐버리자.
		termIdx.end <- 100 %/% pHistTerm
		ordMtx[,"term"] <- ifelse(termIdx==termIdx.end,termIdx.end-1,termIdx)

		#	density : 구간에 해당하는 명중 수 %. (최종 성적에 대한 신뢰도로 활용가능)
		cName <- c("from","to","cnt","cntOrd","cumsum","coverage","density")
		rName <- 0:(100%/%pHistTerm) # 단, 맨 마지막 지점인 100% 항목은 90%에 포함된다.
		freqMtx <- matrix( 0 ,ncol=length(cName) ,nrow=length(rName) )
		colnames(freqMtx) <- cName;	rownames(freqMtx) <- rName
		freqMtx[,"from"] <- rName*pHistTerm;	freqMtx[,"to"] <- freqMtx[,"from"]+pHistTerm

		tbl <- table(ordMtx[,"term"])
		freqMtx[names(tbl),"cnt"] <- tbl
		freqMtx[,"density"] <- round(freqMtx[,"cnt"]*100 / sum(freqMtx[,"cnt"]))
		biggerIdx <- order(freqMtx[,"cnt"],decreasing=T)
		freqMtx[biggerIdx,"cntOrd"] <- 1:nrow(freqMtx)

		freqMtx[biggerIdx,"cumsum"] <- cumsum(freqMtx[biggerIdx,"cnt"])
		freqMtx[biggerIdx,"coverage"] <- round(freqMtx[biggerIdx,"cumsum"]*100 / sum(tbl))

		FLogStr(sprintf("<Frequent Matrix(mean data over history)> (by %d%% step)",pHistTerm))
		FLog( freqMtx )

		plot(freqMtx[biggerIdx,"coverage"] ,main="Coverage by Frequent(accum)" )

		# 임의 구간 선택 시, 히스토리 별 명중?과 후보 갯수가 어떻게 되는 지 파악
		threhold <- 75	# 75% 까지의 점유가능 구간들에 대해 파악.
		selAreaIdx <- biggerIdx[freqMtx[biggerIdx,"coverage"] <= threhold]

		hitLst <- list()
		colIdx <-  heat.colors(length(unique(cmmMtx[,"hist"])))
		names(colIdx) <- unique(cmmMtx[,"hist"])
		plot( rep(0,length(pObj$pool$dnaCode)) ,ylim=c(0,100) ,pch="." ,main="scores by code" )
		check.idx <- sort(sample( unique(cmmMtx[,"hist"]) ,10 ))
		for( hIdx in unique(cmmMtx[,"hist"]) ){
			# hIdx <- unique(cmmMtx[,"hist"])[1]
			hMtx <- cmmMtx[cmmMtx[,"hist"]==hIdx,]
			hMtx[,"order"] <- pObj$assF( hMtx[,"score"], pSize=length(idn.dna) )
			stdVal <- hMtx[nrow(hMtx),"val"]		# 정답
			haveStd <- stdVal %in% hMtx[-nrow(hMtx),"val"] # 후보군에 정답이 있는 상황인지?
			if( hIdx %in% check.idx){
				lines( x=1:length(pObj$pool$dnaCode) ,y=hMtx[,"order"] ,col=colIdx[as.character(hIdx)] )
			}

			cName <- c("selArea","isHit","coverage","candNum","candPer","isHit_cum","candNum_cum","candPer_cum")
			hitMtx <- matrix( 0 ,nrow=length(selAreaIdx) ,ncol=length(cName) )
			colnames(hitMtx) <- cName
			candNumTot <- nrow(hMtx)-1; isHit_cum <- 0; candNum_cum <-0
			candLst <- list()
			for( idx in 1:length(selAreaIdx) ){
				# idx <- 1
				fromTo <- c( freqMtx[selAreaIdx[idx],"from"] ,freqMtx[selAreaIdx[idx],"to"] )
				areaFlag <- fromTo[1]<=hMtx[,"order"] & fromTo[2]>=hMtx[,"order"]

				hitMtx[idx,"selArea"] <- selAreaIdx[idx]
				hitMtx[idx,"isHit"] <- ifelse( stdVal %in% hMtx[areaFlag,"val"] ,1,0 )
				hitMtx[idx,"coverage"] <- freqMtx[selAreaIdx[idx],"coverage"]
					# 이것은 정답의 %만으로 판단할 뿐, 실제 후보군에 정답이 존재했는지는 haveStd로 판단해야 함
				hitMtx[idx,"candNum"] <- sum(areaFlag)
				hitMtx[idx,"candPer"] <- round(hitMtx[idx,"candNum"]*100/candNumTot)

				isHit_cum		<- ifelse(hitMtx[idx,"isHit"]==1,1,isHit_cum)
				candNum_cum		<- candNum_cum + hitMtx[idx,"candNum"]
				
				hitMtx[idx,"isHit_cum"] <- isHit_cum
				hitMtx[idx,"candNum_cum"] <- candNum_cum
				hitMtx[idx,"candPer_cum"] <- round(hitMtx[idx,"candNum_cum"]*100/candNumTot)	
			} # for idx

			hitObj <- list( hist=hIdx)
			hitObj$hMtx <- hMtx
			hitObj$hitMtx <- hitMtx
			hitObj$stdVal <- stdVal
			hitObj$haveStd <- haveStd
			
			hitLst[[as.character(hIdx)]] <- hitObj
		} # for hIdx

		FLogStr("<Hit Rate>")
		par( mfrow=c(2,2) )
		for( idx in 1:length(selAreaIdx) ){
			# idx <- 1
			# hitLst[[1]]$hitMtx[idx,]
			wMtx <- sapply(hitLst, function(p){p$hitMtx[idx,]} )
			stdMtx <- sapply(hitLst,function(p){c(p$stdVal,p$haveStd)})
			stdMtx <- rbind( stdMtx ,wMtx["isHit_cum",] )
			rownames(stdMtx) <- c("stdVal","haveStd","isHit_cum")

			isHit <- wMtx["isHit_cum",]
			isHit.real <- isHit
			isHit.real[stdMtx["haveStd",]==0] <- 0

			# 그냥 차라리 빈도수 만큼으로 때려맞추는 것 보다 나은 지 비교자료.
			stdFreq <- table(stdMtx["stdVal",])
			stdFreq <- sort(stdFreq,decreasing=T)
			stdFreq.cum <- cumsum(stdFreq)
			stdFreq.cvg <- round(stdFreq.cum*100 / length(isHit))

			# 정답 코드와 명중률.

			stdVal.tbl <- table(stdMtx["stdVal",])
			rName <- c("stdVal","tot","freqRate","hitRate")
			stdVal.hitMtx <- matrix( 0 ,ncol=length(stdVal.tbl) ,nrow=length(rName) )
			rownames(stdVal.hitMtx) <- rName
			colnames(stdVal.hitMtx) <- names(stdVal.tbl)
			stdVal.hitMtx["stdVal",] <- as.integer(names(stdVal.tbl))
			stdVal.hitMtx["tot",] <- stdVal.tbl
			if( 0<max(stdVal.hitMtx["tot",]) ){
				# stdVal.hitMtx["freqRate",] <- round(stdVal.hitMtx["tot",]*100/max(stdVal.hitMtx["tot",]))
				stdVal.hitMtx["freqRate",] <- round(stdVal.hitMtx["tot",]*100/sum(stdVal.hitMtx["tot",]))
			}
			std.hitRate <- tapply( stdMtx["isHit_cum",] ,stdMtx["stdVal",] ,function(p){round(sum(p)*100/length(p))} )
			stdVal.hitMtx["hitRate",names(std.hitRate)] <- std.hitRate

			plot( x=jitter(stdMtx["stdVal",]) ,y=jitter(stdMtx["isHit_cum",]) 
					,main=sprintf("%d%% coverage",wMtx["coverage",1]) 
					,xlab="stdVal" ,ylab="isHit(cum)"
				)
			plot( x=stdVal.hitMtx["freqRate",] ,y=stdVal.hitMtx["hitRate",]
					,main=sprintf("%d%% coverage",wMtx["coverage",1]) 
					,ylim=c(0,100)
					,xlab="freqRate" ,ylab="Hit rate" ,col="red" ,pch="*"
				)
			lines( x=stdVal.hitMtx["freqRate",] ,y=rep(round(mean(isHit)*100),ncol(stdVal.hitMtx)) ,col="blue" )
			lines( x=stdVal.hitMtx["freqRate",] ,y=rep(wMtx["coverage",1],ncol(stdVal.hitMtx)) ,col="green" )
			legend( "topright" ,c("blue:hit(mean)","green:coverage") )

			hist( wMtx["candNum",] ,ylim=c(0,ncol(wMtx)) ,main="cand num distribution" )
			plot( wMtx["candNum",] ,main="cand num by his" )

			FLogStr(sprintf("  ** Window Area %d (%d%% coverage)",idx,wMtx["coverage",1]))
			FLogStr(sprintf("     hit rate : %d%% (real:%d%%, %d out of cand)" 
				,round(mean(isHit)*100) ,round(mean(isHit.real)*100) ,sum(isHit)-sum(isHit.real) ))
			FLogStr(sprintf("     candidate size mean:%f max:%d min:%d"
				,mean(wMtx["candNum_cum",]),max(wMtx["candNum_cum",]),min(wMtx["candNum_cum",])))

			candNum <- round(mean(wMtx["candNum_cum",]))
			if( candNum >= 1 ){
				FLogStr(sprintf("     should be better than %d%% (expected by frequency)", stdFreq.cvg[candNum]))
			}

		} # for idx

		varNum <- sapply( hitLst ,function(p){length(unique(p$hMtx[,"order"]))} )
		cdNum <- nrow(hitLst[[1]]$hMtx)
		plot( varNum ,main="variaty number of score" ,ylim=c(0,cdNum) )
		hist( varNum ,main="variaty number of score" )

		# End
		dev.off()
		FLogStr("----------------------------------------------------",pTime=T)
		
	} # gE.rptIndividual.miniS
