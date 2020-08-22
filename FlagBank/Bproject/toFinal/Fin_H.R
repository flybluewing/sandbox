# B.rptStdMI.grp( stdMI.grp )
# B.rptScoreMtx.grp( scoreMtx.grp )



Fin.customCut <- function( lastH ,gEnv ,allIdxF ,stdMI.grp ,anaOnly=F ){

    if( FALSE ){   # comment
        #   allIdxF <- c( stdIdx ,sort(stdIdx + sample(-100:100,9 )*3000) )
        #       anaOnly=T 인 경우, allIdxF[1] 은 stdIdx로 고정하자.

        # anaOnly=T ;aIdx <- 1
        # k <- sample( 1:length(allIdxF) ,20 )    ;k <- sort(k)
        # allIdxF <- allIdxF[k]
        # aZoidMtx <- gEnv$allZoidMtx[allIdxF,]
    }

    cutInfoLst <- lapply( seq_len(length(allIdxF)) ,function(p){return(list())} )

    customCutHeader <- sprintf("./toFinal/Fin_z%d.R",lastH)
    cat(sprintf(" loading... %s\n",customCutHeader))
    source(customCutHeader)

    surFlag <- rep( T ,length(allIdxF) )
    aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
    rownames(aZoidMtx) <- sprintf("a%07d",allIdxF)
    lastZoid <- gEnv$zhF[lastH-1,]

    sCutRst <- Fin.staticCut( aZoidMtx ,lastZoid ,surFlag ,anaOnly=anaOnly )
    surFlag <- surFlag & sCutRst$surFlag
    if( anaOnly ){
        for( idx in 1:length(allIdxF) ){
            cutInfoLst[[idx]] <- append( cutInfoLst[[idx]] ,sCutRst$cutInfoLst[[idx]])
        }
    }

    for( pName in names(Fin.custCutLst) ){
        # B.rptStdMI.grp( stdMI.grp )
        cCutRst <- Fin.custCutLst[[pName]]( aZoidMtx ,surFlag ,stdMI=stdMI.grp$basic[[pName]]$stdMI ,anaOnly=anaOnly )
        surFlag <- surFlag & cCutRst$surFlag
        if( anaOnly ){
            for( idx in 1:length(allIdxF) ){
                cutInfoLst[[idx]] <- append( cutInfoLst[[idx]] ,cCutRst$cutInfoLst[[idx]])
            }
        }
    }

    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )    # cutRst$cutInfoLst[[1]]
}

Fin.staticCut <- function( aZoidMtx ,lastZoid ,surFlag=NULL ,anaOnly=F ){
    #   aCStepMtx <- aZoidMtx[,2:6]-aZoidMtx[,1:5]
    #   aZoidMtx를 Fin.getStdZoidGrp()$zoidMtx으로 넣어서 검증 요.

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){
        surFlag <- rep( T ,aLen )
    }

    cutInfoLst <- vector( "list" ,aLen )    ;names(cutInfoLst)<-rownames(aZoidMtx)

    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]
        aRem <- aZoid%%10   ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - lastZoid

        if( TRUE ){ # aZoid
            cutId <- "aZoid[1]>15"
            if( aZoid[1]>15 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aZoid[6]<25"
            if( aZoid[6]<25 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "zw<20"
            if( 20 > (aZoid[6]-aZoid[1]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem tbl>3"
            tbl <- table(aRem)
            if( any(tbl>3) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }

            cutId <- "aRem symm"
            if( all(aRem[1:3]==aRem[6:4]) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }

        if( TRUE ){ # aCStep
            cutId <- "aCStep tbl>=4"
            tbl <- table(aCStep)
            if( any(tbl>=4) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }

        if( TRUE ){ # aFStep
            cutId <- "aFStep tbl>=4"
            tbl <- table(aFStep)
            if( any(tbl>=4) ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    cutInfo=c(typ="Fin" ,hName="N/A" ,mName="staticCut" ,pName="N/A" ,info=cutId )
                    cutInfoLst[[aIdx]] <- append( cutInfoLst[[aIdx]] ,list(cutInfo) )
                }
            }
        }

    }

    cutRst <- list(surFlag=surFlag)
    if( anaOnly ){
        cutRst$cutInfoLst = cutInfoLst
    }
    return( cutRst )
}

Fin.getStdZoidGrp <- function( gEnv ){
    stdIdx <- c( 786 ,791 ,797
                ,801 ,808 ,818
                ,823 ,826 ,840
                ,848 ,854 ,860
                ,862 ,864 ,867 ,870
                ,892 ,894 ,899
    )
    stdIdx <-stdIdx[ stdIdx<=nrow(gEnv$zhF) ]
    rName <- sprintf( "z%04d",stdIdx )

    zoidMtx <- gEnv$zhF[stdIdx,]                ;rownames(zoidMtx) <- rName
    cStepMtx <- zoidMtx[,2:6] - zoidMtx[,1:5]   ;rownames(cStepMtx)<- rName
    fStepMtx <- zoidMtx ;fStepMtx[,]<-0         ;rownames(fStepMtx)<- rName
    for( idx in seq_len(length(stdIdx)) ){
        hIdx <- stdIdx[idx]
        fStepMtx[idx,] <- zoidMtx[idx,]-gEnv$zhF[hIdx-1,]
    }

    sZoidGrp <- list( zoid=zoidMtx ,cStep=cStepMtx ,fStep=fStepMtx)
    return()
}
