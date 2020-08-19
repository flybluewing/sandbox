
Fin.customCut <- function( lastH ,gEnv ,allIdxF ,anaOnly=F ){
    #   allIdxF <- c( stdIdx ,sort(stdIdx + sample(-100:100,9 )*3000) )
    #       anaOnly 인 경우, allIdxF[1] 은 stdIdx로 고정하자.
    cutInfoLst <- vector( "list" ,length(allIdxF) )

    customCutHeader <- sprintf("./toFinal/Fin_z%d.R",lastH)
    cat(sprintf(" loading... %s\n",customCutHeader))
    source(customCutHeader)

    aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
    lastZoid <- gEnv$zhF[lastH-1,]



    surFlag <- rep( T ,length(allIdxF) )
    cutRst <- list( surFlag=surFlag )
    if( anaOnly ){
        cutRst$cutInfoLst <- cutInfoLst
    }

    return( cutRst )
}

Fin.staticCut <- function( aZoidMtx ,lastZoid ,surFlag=NULL ,anaOnly=F ){

    # aCStepMtx <- aZoidMtx[,2:6,drop=F]-aZoidMtx[,1:5,drop=F]
    # aFStepMtx <- t( apply(aZoidMtx,1,function(pZoid){ pZoid-lastZoid }) )

    aLen <- nrow(aZoidMtx)
    if( is.null(surFlag) ){
        surFlag <- rep( T ,aLen )
    }

    cutInfoLst <- vector( "list" ,aLen )

    for( aIdx in seq_len(aLen) ){
        if( !anaOnly && !surFlag[aIdx] ) next

        aZoid <- aZoidMtx[aIdx,]    ;aCStep <- aZoid[2:6]-aZoid[1:5]    ;aFStep <- aZoid - lastZoid

        if( TRUE ){ # aZoid
            cutId <- "aZoid[1]>15"
            if( aZoid[1]>15 ){
                surFlag[aIdx] <- F
                if( !anaOnly ){ next
                } else {    # cutInfoLst[[aIdx]] <- 
                }
            }
        }

        if( TRUE ){ # aCStep

        }

        if( TRUE ){ # aFStep

        }

    }

}

