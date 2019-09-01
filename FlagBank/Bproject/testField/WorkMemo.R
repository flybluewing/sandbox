
		# cutterObj$cutLst[["F_rowPairMat"]] <- function( scoreMtx ){
		F_rowPairMat <- function( scoreMtx ){
			rCutId <- character(0)

			rowPair <- c("rebC.r","rebC2.r")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rr %d",matCnt) )
			}

			rowPair <- c("rebC.c","rebC2.c")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rc %d",matCnt) )
			}

			rowPair <- c("rebC.f","rebC2.f")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("rf %d",matCnt) )
			}

			rowPair <- c("inc.r","inc.r2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir12 %d",matCnt) )
			}

			rowPair <- c("inc.r","inc.r3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir13 %d",matCnt) )
			}

			rowPair <- c("inc.r2","inc.r3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ir23 %d",matCnt) )
			}

			rowPair <- c("inc.c","inc.c2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic12 %d",matCnt) )
			}

			rowPair <- c("inc.c","inc.c3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic13 %d",matCnt) )
			}

			rowPair <- c("inc.c2","inc.c3")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("ic23 %d",matCnt) )
			}

			rowPair <- c("inc.f","inc.f2")	# scoreMtx[rowPair,]
			matFlag <- scoreMtx[rowPair[1],]==scoreMtx[rowPair[2],]
			validFlag <- 0<(scoreMtx[rowPair[1],]+scoreMtx[rowPair[2],])
			matCnt <- sum( matFlag[validFlag] )
			if( !bUtil.in(matCnt,eadge=c(min=1,max=1)) ){
				rCutId <- c( rCutId, sprintf("if12 %d",matCnt) )
			}

			return( rCutId )
		}


