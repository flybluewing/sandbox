bSMtxLst <- list()


if( TRUE ){ # "sScore01"

	bS.sScore1.cName <- c(	"rem0.num" ,"rem0.len.tot" ,"rem0.len.val"	# 이전에 값이 동일했던 컬럼쌍이, 다음에도 동일 쌍으로 나타나는 경우.
                            ,"rem1.num" ,"rem1.len.tot" ,"rem1.len.val"
                            ,"c0.num" ,"c0.len.tot" ,"c0.len.val" ,"c1.num" ,"c1.len.tot" ,"c1.len.val"
                            ,"f0.num" ,"f0.len.tot" ,"f0.len.val" ,"f1.num" ,"f1.len.tot" ,"f1.len.val"
	)

    bSMtx.sScore1 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){
        # stdMI<-phVP$stdMILst[[pvName]]
        # wMI <- phVP$getCodeH( stdMI )

        findSeg <- function( aCode ){

            segLst <- list()

            tbl <- table(aCode)
            if( all(tbl<2) ){
                return( segLst )
            } else {
                tbl <- tbl[tbl>=2]
            }

            for( val in as.integer(names(tbl)) ){
                segObj <- list( val=val ,idx=which(aCode==val) )
                segLst[[1+length(segLst)]] <- segObj
            }
            names(segLst) <- paste("val",names(tbl),sep="")

            return( segLst )
        }
        checkSeg <- function( segLst ,aCode ){
            rVal <- c( num=0 ,lenTot=0 ,lenVal=0 )
            for( lIdx in seq_len(length(segLst)) ){
                idx <- segLst[[lIdx]]$idx
                if( !all(aCode[idx[1]]==aCode[idx]) ) next

                rVal["num"]     <- rVal["num"] + 1
                rVal["lenTot"]  <- rVal["lenTot"] + length(idx)
                rVal["lenVal"]  <- rVal["lenVal"] + ifelse( segLst[[lIdx]]$val==aCode[idx[1]] ,length(idx) ,0 )
            }
            return( rVal )
        }

		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore1.cName) )	;colnames(scrMtx) <- bS.sScore1.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        segGrp <- list()
        segGrp[["rem0"]]<- findSeg( wMI$lastRaw%%10 )
        segGrp[["c0"]]  <- findSeg( wMI$cStepTail[stdMILen,] )
        segGrp[["f0"]]  <- NULL
        segGrp[["rem1"]]<-NULL      ;segGrp[["c1"]]<-NULL   ;segGrp[["f1"]]<-NULL
        if( 0<stdMI$mtxLen ){
            segGrp[["rem1"]]<- findSeg( wMI$rawTail[stdMILen-1,]%%10 )
            segGrp[["c1"]]  <- findSeg( wMI$cStepTail[stdMILen-1,] )

            segGrp[["f0"]]  <- findSeg( wMI$fStepTail[stdMILen,] )
        }
        if( 1<stdMI$mtxLen ){
            segGrp[["f1"]]  <- findSeg( wMI$fStepTail[stdMILen-1,] )
        }

        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aRem <- aObj$aZoidMtx[aIdx,]%%10
            aCStep <- aObj$cStepMtx[aIdx,]
            aFStep <- aObj$fStepMtx[aIdx,]

            fndInfo <- checkSeg( segGrp[["rem0"]] ,aRem )
            scrMtx[wIdx,"rem0.num"]     <- fndInfo["num"]
            scrMtx[wIdx,"rem0.len.tot"] <- fndInfo["lenTot"]
            scrMtx[wIdx,"rem0.len.val"] <- fndInfo["lenVal"]

            fndInfo <- checkSeg( segGrp[["c0"]] ,aCStep )
            scrMtx[wIdx,"c0.num"]     <- fndInfo["num"]
            scrMtx[wIdx,"c0.len.tot"] <- fndInfo["lenTot"]
            scrMtx[wIdx,"c0.len.val"] <- fndInfo["lenVal"]

            if( !is.null(segGrp[["f0"]]) ){
                fndInfo <- checkSeg( segGrp[["f0"]] ,aFStep )
                scrMtx[wIdx,"f0.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"f0.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"f0.len.val"] <- fndInfo["lenVal"]
            }

            if( !is.null(segGrp[["rem1"]]) ){
                fndInfo <- checkSeg( segGrp[["rem1"]] ,aRem )
                scrMtx[wIdx,"rem1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"rem1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"rem1.len.val"] <- fndInfo["lenVal"]
            }
            if( !is.null(segGrp[["c1"]]) ){
                fndInfo <- checkSeg( segGrp[["c1"]] ,aCStep )
                scrMtx[wIdx,"c1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"c1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"c1.len.val"] <- fndInfo["lenVal"]
            }
            if( !is.null(segGrp[["f1"]]) ){
                fndInfo <- checkSeg( segGrp[["f1"]] ,aFStep )
                scrMtx[wIdx,"f1.num"]     <- fndInfo["num"]
                scrMtx[wIdx,"f1.len.tot"] <- fndInfo["lenTot"]
                scrMtx[wIdx,"f1.len.val"] <- fndInfo["lenVal"]
            }

            if( any(0<scrMtx[wIdx,]) ){
                aQuo <- fCutU.getQuoObj( aZoidMtx[aIdx,] ,valSet=T )
                if( all(aQuo$size==stdMI$quoTail[stdMILen,]) ){
                    if( 0<scrMtx[wIdx,"rem0.len.tot"] ){
                        cName <- c("rem0.num" ,"rem0.len.tot" ,"rem0.len.val")
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"c0.len.tot"] ){
                        cName <- c( "c0.num" ,"c0.len.tot" ,"c0.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"f0.len.tot"] ){
                        cName <- c( "f0.num" ,"f0.len.tot" ,"f0.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                }
                if( 1<stdMILen && all(aQuo$size==stdMI$quoTail[stdMILen-1,]) ){
                    if( 0<scrMtx[wIdx,"rem1.len.tot"] ){
                        cName <- c( "rem1.num" ,"rem1.len.tot" ,"rem1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"c1.len.tot"] ){
                        cName <- c( "c1.num" ,"c1.len.tot" ,"c1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                    if( 0<scrMtx[wIdx,"f1.len.tot"] ){
                        cName <- c( "f1.num" ,"f1.len.tot" ,"f1.len.val" )
                        scrMtx[wIdx,cName] <- 1 + scrMtx[wIdx,cName]
                    }
                }
            }

        }

        # 의미 중복이 발생한 컬럼 데이터 제거
        if( TRUE ){
            for( cIdx in c("rem0.num","rem1.num","c0.num","c1.num","f0.num","f1.num") ){
                scrMtx[(1>=scrMtx[,cIdx]) ,cIdx] <- 0
            }
        }

        return( scrMtx )
    }

    bSMtxLst[["sScore01"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore1.cName) )	;colnames(scoreMtx) <- bS.sScore1.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore1( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )

            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}

if( TRUE ){ # "sScore02"

	bS.sScore2.cName <- c(	 "rebC.r","rebC.c","rebC.f","rebC2.r","rebC2.c","rebC2.f"   # 컬럼 값 유지
                            ,"inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2"         # 증가량 유지.
	)

    bSMtx.sScore2 <- function( stdMI ,workArea ,wMI ,aObj ,aZoidMtx ){

		scrMtx <- matrix( 0, nrow=length(workArea), ncol=length(bS.sScore2.cName) )	;colnames(scrMtx) <- bS.sScore2.cName
        rownames(scrMtx) <- workArea
        stdMILen <- nrow(wMI$rawTail)
        if( 0==stdMILen ){
            return( scrMtx )
        }

        inc.raw <- NULL     ;inc.cStep <-NULL   ;inc.fStep <-NULL
        inc.raw2<-NULL      ;inc.cStep2<-NULL   ;inc.fStep2<-NULL
        if( 2<stdMILen ){
            inc.raw     <- wMI$rawTail[stdMILen,] + (wMI$rawTail[stdMILen,]-wMI$rawTail[stdMILen-1,])
            inc.cStep   <- wMI$cStep[stdMILen,]   + (wMI$cStep[stdMILen,] - wMI$cStep[stdMILen-1,])
        }
        if( 3<stdMILen){
            inc.fStep   <- wMI$fStep[stdMILen,] + (wMI$fStep[stdMILen,] - wMI$fStep[stdMILen-1,])
        }

        if( 4<stdMILen){
            inc.raw2    <- wMI$rawTail[stdMILen-1,] + (wMI$rawTail[stdMILen-1,] - wMI$rawTail[stdMILen-3,])
            inc.cStep2  <- wMI$cStep[stdMILen-1,] + (wMI$cStep[stdMILen-1,] - wMI$cStep[stdMILen-3,])
        }
        if( 5<stdMILen){
            inc.fStep2  <- wMI$fStep[stdMILen-1,] + (wMI$fStep[stdMILen-1,] - wMI$fStep[stdMILen-3,])
        }


        for( wIdx in seq_len(length(workArea)) ){
            aIdx <- workArea[wIdx]
            aCode <- aObj$aZoidMtx[aIdx,]       ;aRem <- aCode%%10
            aCStep <- aObj$cStepMtx[aIdx,]      ;aFStep <- aObj$fStepMtx[aIdx,]

            scrMtx[wIdx,"rebC.r"] <- sum(aCode==wMI$rawTail[stdMILen,])
            scrMtx[wIdx,"rebC.c"] <- sum(aCStep==wMI$cStepTail[stdMILen,])
            if( 1<stdMILen ){
                scrMtx[wIdx,"rebC.f"] <- sum(aFStep==wMI$fStepTail[stdMILen,])

                scrMtx[wIdx,"rebC2.r"] <- sum(aCode==wMI$rawTail[stdMILen-1,])
                scrMtx[wIdx,"rebC2.c"] <- sum(aCStep==wMI$cStepTail[stdMILen-1,])
            }
            if( 2<stdMILen ){
                scrMtx[wIdx,"rebC2.f"] <- sum(aFStep==wMI$fStepTail[stdMILen-1,])

            }

            scrMtx[wIdx,"inc.r"] <- sum(inc.raw==aCode)
            scrMtx[wIdx,"inc.c"] <- sum(inc.cStep==aCStep)
            if( !is.null(inc.fStep) ){
                scrMtx[wIdx,"inc.f"] <- sum(inc.fStep==aFStep)
            }
            if( !is.null(inc.raw2) ){
                scrMtx[wIdx,"inc.r2"] <- sum(inc.raw2==aCode)
                scrMtx[wIdx,"inc.c2"] <- sum(inc.cStep2==aCStep)
            }
            if( !is.null(inc.fStep2) ){
                scrMtx[wIdx,"inc.f2"] <- sum(inc.fStep2==aFStep)
            }

            if( any(scrMtx[wIdx,]>=2) ){
                aQuo <- fCutU.getQuoObj( aZoidMtx[aIdx,] ,valSet=T )
                if( all(aQuo$size==stdMI$quoTail[stdMILen,]) ){
                    evtFlag <- scrMtx[wIdx,]>=2
                    scrMtx[wIdx,evtFlag] <- 1 + scrMtx[wIdx,evtFlag]
                }
            }

        }

        return( scrMtx )
    }

    bSMtxLst[["sScore02"]] <- function( phVP ,aZoidMtx ,makeInfoStr=F ){
        # phVP <- phVP.grp$phVPLst[[1]]
		aLen <- nrow(aZoidMtx)
        aObj <- phVP$getCodeW( aZoidMtx )

		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(bS.sScore2.cName) )	;colnames(scoreMtx) <- bS.sScore2.cName
        rownames(scoreMtx) <- aObj$miIdStr
		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pvSubHLen" ,"pvSubName" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"pvSubName"] <- aObj$miIdStr
            for( pvName in names(phVP$stdMILst) ){
                infoMtx[infoMtx[,"pvSubName"]==pvName ,"pvSubHLen"] <- phVP$stdMILst[[pvName]]$mtxLen
            }
		}
		if( 0==length(aObj$miIdStr) ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

        for( pvName in aObj$miNames ){  # pvName <- aObj$miNames[2]
            workArea <- which(aObj$miIdStr==pvName)
            stdMI<-phVP$stdMILst[[pvName]]
            wMI <- phVP$getCodeH( stdMI )
            scrMtx <- bSMtx.sScore2( stdMI=stdMI ,workArea ,wMI=wMI ,aObj ,aZoidMtx )
            scoreMtx[workArea,] <- scrMtx
        }

        return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
    }

}


#============================================================================================================
#   bS 그룹을 개발하는 동안의 임시 코드.
if( FALSE ){
    source("./lib/bSMtx.R")             ;source("./lib/bSMtx_VP.R")         ;source("./lib/bSMtx_Util.R")
    load("Obj_allIdxF0_chkStdMIPair_928.save")    ;aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]    # 4k
    phVP.grp <- bS.getPhVPGrp( gEnv ,aZoidMtx )

    phVP <- phVP.grp$phVPLst[[1]]
    aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    #  bSMtxLst[["sScore01"]]( phVP ,aZoidMtx )
}