# 20180109_C_HRad.R 교차모델

# ban.throughH() 
#   banObj <- getCFltObj( tEnv )
#           hard: 8%    mid:20%     easy:35%
# ban.throughH2() 
#   banObj <- getCFltObj( tEnv )
#           hard: 8%    mid: 15%     easy:35%

# ban.throughH()
#   banCmbObj <- getCFltCmbObj( gEnv )
#           hard: 2%    mid:20%(0) 4%(1)    easy: 45%(0) 27%(1)
# ban.throughH2()
#   banCmbObj <- getCFltCmbObj( gEnv )
#           hard: 1%    mid: 1%    easy: 1%     loose : 57%
# keyword - c("depth","hard","mid","easy")

#   pZhF<-gEnv$zhF  ;pZoidMtx<-gEnv$allZoidMtx[allIdxF,]    ;pLevel=1   ;pDebug=T
loose.ban.colValSeqNext <- function( pZhF ,pZoidMtx ,pLevel=1 ,pDebug=F ){

    thldLevel <- c( 2,1 ) # 0.0% ,0.1% ,12%
    thld <- thldLevel[pLevel]

    banNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )

    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
                        flag <- rep( F ,5 )
                        for( cIdx in 1:5 ){
                            if( 0==nrow(banNextLst[[cIdx]]$fndMtx) ){
                                next
                            }
                            flag[cIdx] <- any( banNextLst[[cIdx]]$fndMtx[1,]==pZoidMtx[pIdx,0:1+cIdx] )
                            # all이 아니고 일부만이라도 매칭되는 것 조차 일반적이지는 않음.. 사실.
                        }
                        return( which(flag) )
                    })
    filtedIdx <- which(sapply(filtLst,length)>=thld)

    rstObj <- list( idStr="loose.ban.colValSeqNext" )
    rstObj$filtLst  <- filtLst
    rstObj$filtedIdx<- filtedIdx
    if( pDebug ){
        rstObj$banNextLst <- banNextLst
    }

    return( rstObj )

} # loose.ban.colValSeqNext( )

#	pColPtnLst <- anaColEndPtn( gEnv$zhF )  ;pZoidMtx <- gEnv$allZoidMtx[allIdx,]   ;pLevel=1   ;pDebug=F
loose.ban.linePtn.reb <- function( pColPtnLst ,pZoidMtx ,pLevel=1 ,pDebug=F ){

    thldLevel <- c(2 ,10) # 19% ,44%(의미 없을 듯.)
    thld <- thldLevel[pLevel]
    banValLst <- lapply( pColPtnLst ,function( p ){
                        return( if(thld>length(p$val)) p$val else p$val[1:thld] )
                    })

    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
                        flag <- rep( F ,6 )
                        for( colIdx in 1:6 ){
                            flag <- pZoidMtx[pIdx,colIdx] %in% banValLst[[colIdx]]
                        }
                        return( which(flag) )
                    })
    filtedIdx <- ( 1:nrow(pZoidMtx) )[0<sapply(filtLst ,length)]

    rstObj <- list( idStr="loose.ban.linePtn.reb" )
    rstObj$filtLst  <- filtLst
    rstObj$filtedIdx<- filtedIdx
    if( pDebug ){
        rstObj$banValLst <- banValLst
    }

    return( rstObj )

} # loose.ban.linePtn.reb( )


loose.ban.hntSameRow <- function( pBanObj ,pZoidMtx ,pCodeLst ,pLevel=1 ,pInitZIdx=NULL ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }
    thldLevel <- c(1 ,2) # 6~9% ,35~50%
    thld <- thldLevel[pLevel]
    codeSize <- sapply(codeLst ,function(p){length(p[[1]])})
    thldSize <- ifelse( codeSize>4 ,thld ,thld-1 )
    thldSize[thldSize<0] <- 0   # codeSize 이름은 유지됨.

    codeLst <- pCodeLst
    banLst.dup <- lapply( pBanObj$encValLst ,function(p){p[[length(p)]]})

    # --------------------------------------------------
    #	filtLst.dupRow ,filtedIdx.dupRow
    excBan=c("A0070_o3")
    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
            # 어느 banLst.dupRow에서 걸렸는지의 flag
            flag <- sapply(pBanObj$cfNames ,function(pName){
                            if( pName %in% excBan ){
                                return( FALSE )
                            }
                            cfObj <- pBanObj$cfObjLst[[pName]]
                            dCnt <- cfObj$diffCnt( codeLst[[pName]][[pIdx]] ,banLst.dup[[pName]] )
                            return( dCnt<=thldSize[pName] )
                        })
            return( pBanObj$cfNames[flag] )
        })
    filtedIdx <- pInitZIdx[0<sapply(filtLst ,length)]

    rstObj <- list( idStr="hntSameRow" )
    rstObj$filtLst  <- filtLst
    rstObj$filtedIdx<- filtedIdx
    return( rstObj )

} # loose.ban.hntSameRow()

#   pBanObj <- banObj   ;pZoidMtx<-allZoidMtx   ;pCodeLst<-codeLst ;pLevel=1    ;pInitZIdx=NULL ;pDepth=2
loose.ban.hntCrossDim <- function( pBanObj ,pZoidMtx ,pCodeLst ,pLevel=1 ,pInitZIdx=NULL ,pDepth=2 ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    codeLst <- pCodeLst

    cfNameMtx <- apply( permutations( length(pBanObj$cfNames) ,2 )
                         ,1 ,function(p){pBanObj$cfNames[p]}
                    )
    cfNameMtx <- t(cfNameMtx)   ;colnames(cfNameMtx) <- c("sbc.name","nzc.name")

    banLst <- list()
    evlScanLst <- list()
    for( cfIdx in 1:nrow(cfNameMtx) ){

        sbc.name <- cfNameMtx[cfIdx,1]
        nzc.name <- cfNameMtx[cfIdx,2]
        rstObj <- evlScan( pBanObj$encValLst ,sbc.name ,nzc.name ,pBanObj$cfObjLst )
        if( !is.null(rstObj$lastZC) ){
            cmDepth <- ifelse( pDepth>length(rstObj$nzcLst.fnd) ,length(rstObj$nzcLst.fnd) ,pDepth ) # current max depth
            useSpan <- (length(rstObj$nzcLst.fnd)-cmDepth+1):length(rstObj$nzcLst.fnd)
            banLst[[cfIdx]] <- rstObj$nzcLst.fnd[useSpan]
        } else {
            # banLst[[cfIdx]] <- list(noData=integer(0))   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
            banLst[[cfIdx]] <- list()   # NULL 이면 맨 끝 인덱스가 달라질 수 있어서..
        }
        evlScanLst[[1+length(evlScanLst)]] <- rstObj
    } # cfIdx


    # --------------------------------------------------
    #	filtLst ,filtedIdx
    thldLevel <- c(1 ,2) # 15% ,65% 
    thld <- thldLevel[pLevel]
    codeSize <- sapply(codeLst ,function(p){length(p[[1]])})
    thldSize <- ifelse( codeSize>4 ,thld ,thld-1 )
    thldSize[thldSize<0] <- 0   # codeSize 이름은 유지됨.

    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
            # 어느 banLst에서 걸렸는지의 flag
            flag <- sapply( 1:nrow(cfNameMtx) ,function(pRIdx){
                            if( 0==length(banLst[[pRIdx]]) ){
                                return( FALSE )	# 판단 기준자체가 없으니 ban에 걸리지 않은 걸로 처리.
                            }
                            # banCode <- banLst[[pRIdx]]
                            banFltName <- cfNameMtx[pRIdx,2]
                            zCode <- codeLst[[banFltName]][[pIdx]]
                            dCnt <- sapply( banLst[[pRIdx]] ,function(banCode){ 
                                            pBanObj$cfObjLst[[banFltName]]$diffCnt( banCode ,zCode )
                                        })
                            return( all(dCnt<=thldSize[banFltName]) )
                        })
            return( which(flag) )
        })
    filtedIdx <- pInitZIdx[0<sapply(filtLst ,length)]

    rstObj <- list( idStr="hntCrossDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
    rstObj$depth        <- pDepth
    rstObj$cfNameMtx    <- cfNameMtx
    rstObj$banLst       <- banLst
    rstObj$evlScanLst   <- evlScanLst
    return( rstObj )

} # loose.ban.hntCrossDim()

#   pBanObj <- banObj   ;pZoidMtx<-allZoidMtx   ;pCodeLst<-codeLst ;pLevel=1    ;pInitZIdx=NULL ;pDepth=2
loose.ban.multiDim <- function( pBanObj ,pZoidMtx ,pCodeLst ,pLevel=1 ,pInitZIdx=NULL ,pDebug=F ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

	# --------------------------------------------------
	#	예외 리스트 정의
	excFiltCmbLst <- list(	cmb0103=which(pBanObj$cfNames %in% c("A0010_o3","A0030_o3"))
							,cmb0506=which(pBanObj$cfNames %in% c("A0050_o5","A0060_o7"))
							,cmbC12_23=which(pBanObj$cfNames %in% c("C0010w02","C0020w03"))
							,cmbC14_34=which(pBanObj$cfNames %in% c("C0010w04","C0030w04"))
						)
	isExcFiltCmb <- function( pFiltCmb ){
        for( idx in 1:length(excFiltCmbLst) ){
            if( length(pFiltCmb)==length(excFiltCmbLst[[idx]]) ){
                if( all(pFiltCmb==excFiltCmbLst[[idx]]) ){
                    return(TRUE)
                }
            }
        }
        return(FALSE)
	} # isExcFiltCmb( )

    # --------------------------------------------------
    #	filtLst ,filtedIdx
    thldLevel <- c(0,1) # 7% ,97%
    thld <- thldLevel[pLevel]
    codeSize <- sapply(codeLst ,function(p){length(p[[1]])})
    thldSize <- ifelse( codeSize>4 ,thld ,thld-1 )
    thldSize[thldSize<0] <- 0   # codeSize 이름은 유지됨.

    fndLst <- list()
    for( zIdx in 1:nrow(pZoidMtx) ){
		curCodeLst <- lapply( pCodeLst ,function(p){p[[zIdx]]})

		cfNameFndLst <- list()
		for( snIdx in pBanObj$cfNames ){ # search name index
			fndIdxLst <- list()
			for( eIdx in pBanObj$encVal.len:1 ){
				dCnt <- pBanObj$cfObjLst[[snIdx]]$diffCnt( 
								curCodeLst[[snIdx]] ,pBanObj$encValLst[[snIdx]][[eIdx]]
							)
				if( dCnt<=thldSize[snIdx] ){
					fndIdxLst[[1+length(fndIdxLst)]] <- eIdx
				}
			}
			matIdxLst	<- lapply(fndIdxLst ,function(pFndIdx){
							hCodeLst <- lapply(pBanObj$encValLst,function(valLst){valLst[[pFndIdx]]})
							dCnt <- sapply(pBanObj$cfNames,function(mName){
											pBanObj$cfObjLst[[mName]]$diffCnt( curCodeLst[[mName]] ,hCodeLst[[mName]] )
										})
							return( which(dCnt<=thldSize) )
						})
			matCnt <- sapply(matIdxLst,length)
			fndIdxLst <- fndIdxLst[matCnt>1]
			matIdxLst <- matIdxLst[matCnt>1]

			if( 0<length(matIdxLst) ){
				excFlag	<- sapply(matIdxLst,isExcFiltCmb)
				fndIdxLst <- fndIdxLst[!excFlag]
				matIdxLst <- matIdxLst[!excFlag]
			}
			cfNameFndLst[[snIdx]] <- list( fndIdxLst=fndIdxLst ,matIdxLst=matIdxLst )
		} # snIdx
		fndLst[[1+length(fndLst)]] <- cfNameFndLst

    } # for(zIdx)

	nameLst <- list()
	filtLst <- list()
	for( fIdx in 1:length(fndLst) ){
		cfNameFndLst <- fndLst[[fIdx]]
		maxFnd <- 0
		for( snIdx in pBanObj$cfNames ){
			fndObj <- cfNameFndLst[[snIdx]]
			if( 0<length(fndObj$matIdxLst) ){
				if( pDebug ){
					nameCmb <- sapply( fndObj$matIdxLst ,function(p){paste(pBanObj$cfNames[p],collapse=" ")})
					nameLst[[1+length(nameLst)]] <- nameCmb
				}
				curMax <- max(sapply(fndObj$matIdxLst,length))
				maxFnd <- ifelse( curMax>maxFnd ,curMax ,maxFnd )
			}
		}
		filtLst[[1+length(filtLst)]] <- maxFnd
	}

    thldLevel <- c(1,2) #  % , %
    thld <- thldLevel[pLevel]
	flag <- sapply( filtLst ,function(p){p>thld})  # thldLevel 적용 및 측정
	filtedIdx <- pInitZIdx[flag]

    rstObj <- list( idStr="multiDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------
	if( pDebug ){
		rstObj$nameLst <- nameLst
	}
	
	return( rstObj )
	
} # loose.ban.multiDim()


#=====================================================================================================
#   remPuriN functions()
#=====================================================================================================
#       끝까지 살아남는 게 더 수상해... 라는 컨셉

remPuri2.ban.throughH.loose <- function( gEnv ,allIdxF ){

    surviveLst <- list()
    banObj <- getCFltObj( gEnv )
    banCmbObj <- getCFltCmbObj( gEnv )

    allZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
    codeLst <- banObj$getCodeLst( allZoidMtx )
    bRstObj <- ban.throughH( banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="easy" )
    allIdxT <- allIdxF[-bRstObj$filtedIdx]
    if( 0<length(allIdxT) ){    # 전멸이 많다.
        allZoidMtx <- gEnv$allZoidMtx[allIdxT,,drop=F]
        codeLst <- banObj$getCodeLst( allZoidMtx )

        bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="loose" )
        surviveLst[["1.ban.throughH2(getCFltObj)"]] <- allIdxT[-bRstObj$filtedIdx]

        bRstObj <- loose.ban.hntCrossDim( banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst )
        surviveLst[["1.loose.ban.hntCrossDim(getCFltObj)"]] <- allIdxT[-bRstObj$filtedIdx]

        bRstObj <- loose.ban.hntSameRow(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        surviveLst[["1.loose.ban.hntSameRow(getCFltObj)"]] <- allIdxT[-bRstObj$filtedIdx]

        bRstObj <- loose.ban.multiDim(banObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        surviveLst[["1.loose.ban.multiDim(getCFltObj)"]] <- allIdxT[-bRstObj$filtedIdx]
    }


    allZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
    codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )
    bRstObj <- ban.throughH( banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pLevel="loose" )
    allIdxT <- allIdxF[-bRstObj$filtedIdx]
    if( 0<length(allIdxT) ){    # 전멸이 많다.
        allZoidMtx <- gEnv$allZoidMtx[allIdxT,,drop=F]
        codeLst <- banCmbObj$getCodeLst( allZoidMtx )

        bRstObj <- loose.ban.multiDim(banCmbObj ,allZoidMtx ,pLevel=2 ,pCodeLst=codeLst)
        surviveLst[["2.loose.ban.multiDim(getCFltObj)"]] <- allIdxT[-bRstObj$filtedIdx]
    }

    #     ban.throughH(getCFltObj)          ban.throughH2(getCFltObj)
    #     ban.throughH(getCFltObj)          loose.ban.hntCrossDim(getCFltObj)
    #     ban.throughH(getCFltObj)          loose.ban.hntSameRow(getCFltObj)
    #     ban.throughH(getCFltObj)          loose.ban.multiDim(getCFltObj)
    #     ban.throughH(getCFltCmbObj)       ban.throughH2(getCFltObj)


} # remPuri.ban.throughH.loose()

remPuri2.ban.throughH2.loose <- function(){

    ban.throughH2

    #     ban.throughH2(getCFltCmbObj)      ban.throughH2(getCFltObj)        
    #     ban.throughH2(getCFltObj)         loose.ban.colValSeqNext          
    #     ban.throughH2(getCFltObj)         loose.ban.hntCrossDim(getCFltObj)
    #     ban.throughH2(getCFltObj)         loose.ban.hntSameRow(getCFltObj) 
    #     ban.throughH2(getCFltObj)         loose.ban.linePtn.reb            
    #     ban.throughH2(getCFltObj)         loose.ban.multiDim(getCFltObj)

} # remPuri.ban.throughH2.loose()

remPuri2.loose.ban.hntCrossDim <- function(){

    loose.ban.hntCrossDim
    #     loose.ban.hntCrossDim(getCFltObj)   loose.ban.multiDim(getCFltObj)

} # remPuri2.loose.ban.hntCrossDim()

