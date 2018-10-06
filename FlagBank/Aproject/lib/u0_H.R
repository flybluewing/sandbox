# u0_H.R unit model zero

u0.zoidInc_ana <- function( zoid1 ,zoid2 ){
    # zoid1=c(6,18,31,34,38,45);zoid2=c(14,15,16,17,38,45)
    # u0.zoidInc_ana( zoid1 ,zoid2 )
    zoid1.str <- paste( sprintf("%2d",zoid1) ,collapse=" " )
    zoid2.str <- paste( sprintf("%2d",zoid2) ,collapse=" " )
    zoid.inc <- paste( sprintf("%2d",zoid2+(zoid2-zoid1)) ,collapse=" " )
    cat(sprintf("   zoid1 %s \n", zoid1.str))
    cat(sprintf("   zoid2 %s \n", zoid2.str))
    cat(sprintf("   next  %s \n", zoid.inc ))
    return("")
} # u0.zoidInc_ana()


u0.zoidMtx_ana.rpt <- function( pMtx ){
    # u0.zoidMtx_ana.rpt( pMtx )
    dfStr <- capture.output( u0.zoidMtx_ana(pMtx) )
    cat(sprintf("%s\n",dfStr))
    # QQE working
} # u0.zoidMtx_ana.rpt()

u0.zoidMtx_ana <- function( pMtx ){

    banLst <- list()
    pMtxLen <- nrow( pMtx )

    # ana by col
    for( cIdx in 1:6 ){
        lst <- u0.srchStep_std( pMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( pMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( pMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( pMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

    }

    # ana left slide /
    for( cIdx in 1:4 ){
        val <- integer(0)
        for( dIdx in 1:5 ){
            dRIdx <- pMtxLen - (dIdx-1)
            dCIdx <- cIdx+dIdx            
            # cat(sprintf("%d %d\n",dRIdx,dCIdx))
            if( (dCIdx>6)||(dRIdx<1) ) break

            val <- c( val ,pMtx[dRIdx,dCIdx] )
        }

        lst <- u0.srchStep_std( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )
    }

    # ana right slide \
    for( cIdx in 3:6 ){
        val <- integer(0)
        for( dIdx in 1:5 ){
            dRIdx <- pMtxLen - (dIdx-1)
            dCIdx <- cIdx-dIdx            
            # cat(sprintf("%d %d\n",dRIdx,dCIdx))
            if( (dCIdx<1)||(dRIdx<1) ) break

            val <- c( val ,pMtx[dRIdx,dCIdx] )
        }

        lst <- u0.srchStep_std( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )
    }

    banDF <- do.call( rbind ,lapply(banLst,function(ban){
                    data.frame( tgt.col=ban$tgt.col ,banVal=ban$banVal 
                                    ,descript=ban$descript
                                    ,tgt.dir=ban$tgt.dir
                                )
                }))
    if( 0==length(banLst) ){
        banDF <- data.frame( tgt.col=integer(0) ,banVal=integer(0)
                                ,descript=character(0)
                                ,tgt.dir=character(0)
                            )
    }

    return( banDF )

} # u0.zoidMtx_ana( )

u0.zoidCMtx_ana.rpt <- function( pMtx ){
    # u0.zoidCMtx_ana.rpt( pMtx )
    cat("cStep ptn info \n")

    cat("    Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl \n")
    dummy <- sapply( seq_len(nrow(pMtx)) ,function( zIdx ){
                    valStr <- paste(sprintf("%2d",pMtx[zIdx,]) ,collapse=" " )
                    rebCnt <- 0
                    if( zIdx>1 ){
                        rebCnt <- sum(pMtx[zIdx,] %in% pMtx[(zIdx-1),])
                    }

                    cStepStr <- paste(sprintf("%2d",pMtx[zIdx,2:6]-pMtx[zIdx,1:5]) 
                                    ,collapse=" " )
                    fStepStr <- if( zIdx>1 ){
                                    paste(sprintf("%3d" ,pMtx[zIdx,]-pMtx[(zIdx-1),])
                                        ,collapse=" " )
                                } else { "                       " }

                    quoObj <- fCutU.getQuoObj( pMtx[zIdx,]  )
                    quoTblStr <- paste( quoObj$tbl ,collapse=" " )
                    quoSizeStr <- paste( quoObj$size ,collapse=" " )

                    cat(sprintf("    %s%s |%s |%s |%s |%s\n"
                            ,valStr
                            ,ifelse(rebCnt>0,sprintf("(%d)",rebCnt),"   ")
                            ,cStepStr ,fStepStr ,quoSizeStr ,quoTblStr
                        ))
                })
    cat("-------------------------------------------------------------------------------------\n")

    dfStr <- capture.output( u0.zoidCMtx_ana(pMtx) )
    cat(sprintf("    %s\n",dfStr))
    cat("-------------------------------------------------------------------------------------\n")

    pMtxLen <- nrow( pMtx )
    if( 0 == pMtxLen ){
        return( "" )
    }
    cMtx <- t( apply( pMtx ,1 ,function(code){ code[2:6]-code[1:5] }) )
    tbl <- table(cMtx)
    tbl <- tbl[tbl>1]
    fvStr <- paste( sprintf("%s(%d)",names(tbl),tbl) ,collapse="   ")
    cat(sprintf("    FV :    %s \n",fvStr))

} # u0.zoidCMtx_ana.rpt()

u0.zoidCMtx_ana <- function( pMtx ){

    banLst <- list()
    pMtxLen <- nrow( pMtx )
    cMtx <- t( apply( pMtx ,1 ,function(code){ code[2:6]-code[1:5] }) )

    # ana by col
    for( cIdx in 1:5 ){
        lst <- u0.srchStep_std( cMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( cMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( cMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( cMtx[pMtxLen:1,cIdx] )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "col"
        }
        banLst <- c( banLst ,lst )
    }

    # ana left slide /
    for( cIdx in 1:3 ){
        val <- integer(0)
        for( dIdx in 1:5 ){
            dRIdx <- pMtxLen - (dIdx-1)
            dCIdx <- cIdx+dIdx            
            # cat(sprintf("%d %d\n",dRIdx,dCIdx))
            if( (dCIdx>5)||(dRIdx<1) ) break

            val <- c( val ,cMtx[dRIdx,dCIdx] )
        }

        lst <- u0.srchStep_std( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide/"
        }
        banLst <- c( banLst ,lst )
    }

    # ana right slide \
    for( cIdx in 3:5 ){
        val <- integer(0)
        for( dIdx in 1:5 ){
            dRIdx <- pMtxLen - (dIdx-1)
            dCIdx <- cIdx-dIdx            
            # cat(sprintf("%d %d\n",dRIdx,dCIdx))
            if( (dCIdx<1)||(dRIdx<1) ) break

            val <- c( val ,cMtx[dRIdx,dCIdx] )
        }

        lst <- u0.srchStep_std( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_seqReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_symm( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )

        lst <- u0.srchStep_ptnReb( val )
        for( lIdx in seq_len(length(lst)) ){
            lst[[lIdx]]$tgt.col <- cIdx
            lst[[lIdx]]$tgt.dir <- "Slide\\"
        }
        banLst <- c( banLst ,lst )
    }

    banDF <- do.call( rbind ,lapply(banLst,function(ban){
                    data.frame( tgt.col=ban$tgt.col ,banVal=ban$banVal 
                                    ,descript=ban$descript
                                    ,tgt.dir=ban$tgt.dir
                                )
                }))
    if( 0==length(banLst) ){
        banDF <- data.frame( tgt.col=integer(0) ,banVal=integer(0)
                                ,descript=character(0)
                                ,tgt.dir=character(0)
                            )
    } else {    # 필요없는 데이터는 지우자.
        banDF <- banDF[ banDF$banVal>0 ,]
    }

    return( banDF )
}

u0.zoidFMtx_ana.rpt <- function( pMtx ){
    # u0.zoidFMtx_ana.rpt( pMtx )
    cat("fStep ptn info \n")

    cat("    Raw value(reb)       |cStep          |fStep                   |QuoSize   |QuoTbl \n")
    dummy <- sapply( seq_len(nrow(pMtx)) ,function( zIdx ){
                    valStr <- paste(sprintf("%2d",pMtx[zIdx,]) ,collapse=" " )
                    rebCnt <- 0
                    if( zIdx>1 ){
                        rebCnt <- sum(pMtx[zIdx,] %in% pMtx[(zIdx-1),])
                    }

                    cStepStr <- paste(sprintf("%2d",pMtx[zIdx,2:6]-pMtx[zIdx,1:5]) 
                                    ,collapse=" " )
                    fStepStr <- if( zIdx>1 ){
                                    paste(sprintf("%3d" ,pMtx[zIdx,]-pMtx[(zIdx-1),])
                                        ,collapse=" " )
                                } else { "                       " }

                    quoObj <- fCutU.getQuoObj( pMtx[zIdx,]  )
                    quoTblStr <- paste( quoObj$tbl ,collapse=" " )
                    quoSizeStr <- paste( quoObj$size ,collapse=" " )

                    cat(sprintf("    %s%s |%s |%s |%s |%s\n"
                            ,valStr
                            ,ifelse(rebCnt>0,sprintf("(%d)",rebCnt),"   ")
                            ,cStepStr ,fStepStr ,quoSizeStr ,quoTblStr
                        ))
                })
    cat("-------------------------------------------------------------------------------------\n")

    dfStr <- capture.output( u0.zoidFMtx_ana(pMtx) )
    cat(sprintf("    %s\n",dfStr))
    cat("-------------------------------------------------------------------------------------\n")

    pMtxLen <- nrow( pMtx )-1
    if( 0 == pMtxLen ){
        return( "" )
    }
    fMtx <- do.call( rbind ,lapply( 1:pMtxLen ,function(rIdx){ pMtx[rIdx+1,]-pMtx[rIdx,] }) )
    tbl <- table(fMtx)
    tbl <- tbl[tbl>1]
    fvStr <- paste( sprintf("%s(%d)",names(tbl),tbl) ,collapse="   ")
    cat(sprintf("    FV :    %s \n",fvStr))

} # u0.zoidFMtx_ana.rpt()

u0.zoidFMtx_ana <- function( pMtx ){

    banLst <- list()
    pMtxLen <- nrow( pMtx )-1
    if( pMtxLen>0 ){
        fMtx <- do.call( rbind ,lapply( 1:pMtxLen ,function(rIdx){ pMtx[rIdx+1,]-pMtx[rIdx,] }) )

        # ana by col
        for( cIdx in 1:6 ){
            lst <- u0.srchStep_std( fMtx[pMtxLen:1,cIdx] )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "col"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_seqReb( fMtx[pMtxLen:1,cIdx] )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "col"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_symm( fMtx[pMtxLen:1,cIdx] )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "col"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_ptnReb( fMtx[pMtxLen:1,cIdx] )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "col"
            }
            banLst <- c( banLst ,lst )

        }

        # ana left slide /
        for( cIdx in 1:4 ){
            val <- integer(0)
            for( dIdx in 1:5 ){
                dRIdx <- pMtxLen - (dIdx-1)
                dCIdx <- cIdx+dIdx            
                # cat(sprintf("%d %d\n",dRIdx,dCIdx))
                if( (dCIdx>6)||(dRIdx<1) ) break

                val <- c( val ,fMtx[dRIdx,dCIdx] )
            }

            lst <- u0.srchStep_std( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide/"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_seqReb( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide/"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_symm( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide/"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_ptnReb( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide/"
            }
            banLst <- c( banLst ,lst )
        }

        # ana right slide \
        for( cIdx in 3:6 ){
            val <- integer(0)
            for( dIdx in 1:5 ){
                dRIdx <- pMtxLen - (dIdx-1)
                dCIdx <- cIdx-dIdx            
                # cat(sprintf("%d %d\n",dRIdx,dCIdx))
                if( (dCIdx<1)||(dRIdx<1) ) break

                val <- c( val ,fMtx[dRIdx,dCIdx] )
            }

            lst <- u0.srchStep_std( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide\\"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_seqReb( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide\\"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_symm( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide\\"
            }
            banLst <- c( banLst ,lst )

            lst <- u0.srchStep_ptnReb( val )
            for( lIdx in seq_len(length(lst)) ){
                lst[[lIdx]]$tgt.col <- cIdx
                lst[[lIdx]]$tgt.dir <- "Slide\\"
            }
            banLst <- c( banLst ,lst )
        }

    }

    banDF <- do.call( rbind ,lapply(banLst,function(ban){
                    data.frame( tgt.col=ban$tgt.col ,banVal=ban$banVal 
                                    ,descript=ban$descript
                                    ,tgt.dir=ban$tgt.dir
                                )
                }))
    if( 0==length(banLst) ){
        banDF <- data.frame( tgt.col=integer(0) ,banVal=integer(0)
                                ,descript=character(0)
                                ,tgt.dir=character(0)
                            )
    }

    return( banDF )

} # u0.zoidFMtx_ana( )


# 1,1,1 / 1,2,3 / 2,4,6
u0.srchStep_std <- function( pVal ,pCordLst=NULL ){

    idxFlagLst <- u0.getChkIdx_std( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
    # same
    for( fIdx in seq_len(length(idxFlagLst)) ){
        srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 2>srcVal.len ) next

        matCnt <- 0 # 연속이 몇 번 발생중인지.
        for( idx in 2:srcVal.len ){
            if( srcVal[idx]!=srcVal[1] ) break

            matCnt <- matCnt+1
        }
        if( matCnt==0 ) next

        banObj <- list( banVal=srcVal[1] ,certSize=matCnt )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_same( banObj ,pVal ,idxFlagLst[[fIdx]] )
        banLst[[1+length(banLst)]] <- banObj

        if( (matCnt+1)<srcVal.len ){   # 끝자락 값은 대칭 방지를 위해 추가...
            banObj <- list( banVal=srcVal[matCnt+2] ,certSize=matCnt )
            banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1+1)]
            banObj$descript <- u0.getDescript_sameEnd( banObj ,pVal ,idxFlagLst[[fIdx]] )
            banLst[[1+length(banLst)]] <- banObj
        }
    } # same for()
    
    # desc1
    for( fIdx in seq_len(length(idxFlagLst)) ){
		srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 2>srcVal.len ) next

		srcDiff <- srcVal[2:srcVal.len] - srcVal[1:(srcVal.len-1)]
		if( 1!=abs(srcDiff[1]) ) next
        matCnt <- 0 # 연속이 몇 번 발생중인지.
		for( idx in 1:length(srcDiff) ){
			if(srcDiff[idx]!=srcDiff[1]) break

			matCnt <- matCnt + 1
		}

        banObj <- list( banVal=(srcVal[1]-srcDiff[1]) ,certSize=matCnt )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_desc1( banObj ,pVal ,idxFlagLst[[fIdx]] )
		banLst[[1+length(banLst)]] <- banObj
    } # desc1 for()

    # descN
    for( fIdx in seq_len(length(idxFlagLst)) ){
		srcVal <- pVal[ idxFlagLst[[fIdx]] ]
        srcVal.len <- length(srcVal)
        if( 3>srcVal.len ) next	# desc N 이면 최소한 3연속어야 의미있음.

		srcDiff <- srcVal[2:srcVal.len] - srcVal[1:(srcVal.len-1)]
		if( 2>abs(srcDiff[1]) ) next	# desc에서의 N 값도 제한을 두는 게 좋을지...
        matCnt <- 0 # 연속이 몇 번 발생중인지.
		for( idx in 1:length(srcDiff) ){
			if(srcDiff[idx]!=srcDiff[1]) break

			matCnt <- matCnt + 1
		}
		if( matCnt<2 ) next

        banObj <- list( banVal=(srcVal[1]-srcDiff[1]) ,certSize=matCnt ,srcDiff=srcDiff[1] )
        banObj$cordLst=pCordLst[ idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$cordStr=cordStr[  idxFlagLst[[fIdx]] ][1:(matCnt+1)]
        banObj$descript <- u0.getDescript_descN( banObj ,pVal ,idxFlagLst[[fIdx]] )
		banLst[[1+length(banLst)]] <- banObj
    } # descN for()
	
    return( banLst )

} # u0.srchStep_std

# 1,2,3,2,1 / 1,2,2,1
u0.srchStep_symm <- function( pVal ,pCordLst=NULL ){

    idxFlagLst <- u0.getChkIdx_symm( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
	for( fIdx in seq_len(length(idxFlagLst)) ){
		chkIdx <- idxFlagLst[[fIdx]]
		if( !all(pVal[chkIdx$chk1]==pVal[chkIdx$chk2]) ) next
		if( all(pVal[chkIdx$coverArea[1]]==pVal[chkIdx$coverArea]) ) next # 한가지 값으로만 도배되어있다면..
		
		coverArea <- c( chkIdx$coverArea ,chkIdx$ban )

        banObj <- list( banVal=pVal[chkIdx$ban] ,coverArea=coverArea )
        banObj$cordLst=pCordLst[ coverArea ]
        banObj$cordStr=cordStr[  coverArea ]
        banObj$descript <- u0.getDescript_symm( banObj ,pVal )
		banLst[[1+length(banLst)]] <- banObj
		
	} # descN for()

	return( banLst )
} # u0.srchStep_std

# 7(?),1,2,7,1,2,...
u0.srchStep_ptnReb <- function( pVal ,pCordLst=NULL ){

    idxFlagLst <- u0.getChkIdx_ptn( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()
	for( fIdx in seq_len(length(idxFlagLst)) ){
		chkIdx <- idxFlagLst[[fIdx]]
		if( !all(pVal[chkIdx$chk1]==pVal[chkIdx$chk2]) ) next
		if( all(pVal[chkIdx$coverArea[1]]==pVal[chkIdx$coverArea]) ) next # 한가지 값으로만 도배되어있다면..

        banObj <- list( banVal=pVal[chkIdx$ban] ,coverArea=chkIdx$coverArea )
        banObj$cordLst=pCordLst[ chkIdx$coverArea ]
        banObj$cordStr=cordStr[  chkIdx$coverArea ]
        banObj$descript <- u0.getDescript_ptnReb( banObj ,pVal )
		banLst[[1+length(banLst)]] <- banObj
		
	} # descN for()

	return( banLst )

} # u0.srchStep_std

# 1(?),1,2,2,...
u0.srchStep_seqReb <- function( pVal ,pCordLst=NULL ){

    # idxFlagLst <- u0.getChkIdx_std( length(pVal) )
    if( is.null(pCordLst) ){
        pCordLst <- lapply( 1:length(pVal) ,function(idx){idx} )
    }
    cordStr <- sapply( pCordLst ,function(cord){ paste(cord,collapse=",") })

    banLst <- list()

    # 1(?) ,1 ,2 ,2 ...
    srcVal <- pVal  ;srcVal.len <- length(srcVal)
    if( (3<=srcVal.len) && (srcVal[2]==srcVal[3]) ){
        banObj <- list( banVal=srcVal[1] )
        banObj$cordLst=pCordLst[ 1:3 ]
        banObj$cordStr=cordStr[  1:3 ]
        banObj$descript <- sprintf("[seqReb  ] %2d(?),%2d,%2d,%2d,..."
                                    ,srcVal[1],srcVal[1],srcVal[2],srcVal[2]
                                )
		banLst[[1+length(banLst)]] <- banObj
    }

    # 1(?) ,x ,1 ,x ,2 ,x ,2 ...
    pVal.len <- length(pVal)
    if( (6<=pVal.len) && (pVal[4]==pVal[6]) ){
        banObj <- list( banVal=pVal[2] )
        banObj$cordLst=pCordLst[ c(2,4,6) ]
        banObj$cordStr=cordStr[  c(2,4,6) ]
        banObj$descript <- sprintf("[seqReb  ] %2d(?), .,%2d, .,%2d, .,%2d,..."
                                    ,pVal[2],pVal[2],pVal[4],pVal[6]
                                )
		banLst[[1+length(banLst)]] <- banObj
    }

	return( banLst )

} # u0.srchStep_seqReb



u0.getChkIdx_std <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    tempFlag <- c( T ,T ,T ,T ,T ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[1]] <- tempFlag

    tempFlag <- c( F ,T ,F ,T ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[2]] <- tempFlag

    tempFlag <- c( F ,F ,T ,F ,F ,T )[1:maxLen]
    if( 1<sum(tempFlag) ) idxFlagLst[[3]] <- tempFlag

    return(idxFlagLst)
} # u0.getChkIdx4std()
u0.getChkIdx_symm <- function( pMaxLen=6 ){

    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 4<=maxLen ){ # 1,2,1,3
        idxFlagLst[[1]] <- list( chk1=1 ,chk2=3 ,ban=4 ,coverArea=1:3 )
    }
    if( 5<=maxLen ){ # 1,2,2,1,3
        idxFlagLst[[2]] <- list( chk1=1:2 ,chk2=4:3 ,ban=5 ,coverArea=1:4 )
    }
    if( 6<=maxLen ){ # 1,2,4,2,1,3
        idxFlagLst[[3]] <- list( chk1=1:2 ,chk2=5:4 ,ban=6 ,coverArea=1:5 )
    }

    return(idxFlagLst)

} # u0.getChkIdx_symm()
u0.getChkIdx_ptn <- function( pMaxLen=6 ){
    idxFlagLst <- list()
    maxLen <- 6
    maxLen <- ifelse( maxLen>pMaxLen ,pMaxLen ,maxLen )

    # 로직보다는 차라리, 알아보기 쉽게 직관적인 날코딩을 하자.
    if( 5<=maxLen ){
        idxFlagLst[[1]] <- list( chk1=1:2 ,ban=3 ,chk2=4:5 ,coverArea=1:5 )
    }

    return(idxFlagLst)
} # u0.getChkIdx_ptn()

# rawVal<-pVal	;idxFlag<-idxFlagLst[[fIdx]]
u0.getDescript_same <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- " ."
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[same    ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_sameEnd <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+2)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[sameEnd ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_desc1 <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[desc1   ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_descN <- function( banObj ,rawVal ,idxFlag ){
	valStr <- sprintf("%2d",rawVal)	;valStr[!idxFlag] <- "xx"
	endIdx <- max(which(idxFlag)[1:(banObj$certSize+1)])
	valStr <- valStr[1:endIdx]	;valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[desc(%2d) ] %2d(?),%s",banObj$srcDiff,banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_symm <- function( banObj ,rawVal ){
	valStr <- sprintf("%2d",rawVal[banObj$coverArea])
	valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[symm    ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
u0.getDescript_ptnReb <- function( banObj ,rawVal ){
	valStr <- sprintf("%2d",rawVal[banObj$coverArea])
	valStr <- paste(valStr,collapse=",")
	descStr <- sprintf("[ptnReb   ] %2d(?),%s",banObj$banVal,valStr)
    return(descStr)
}
