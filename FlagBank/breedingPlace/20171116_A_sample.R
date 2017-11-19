# 20171116_A_sample.R

curWd <- getwd()	;setwd("..")
FB <- getFlagBank()	;setwd(curWd)

source("20171116_A_H.R")
source("20171116_A_H_cliper.R")

zh <- as.matrix(FB$zh)

# -----------------------------------------------------------------------
# remainder
# -----------------------------------------------------------------------
#   <?ƒ?„±>
remainMtx <- zh %% 5
#   <ë¶„ì„>
matchLst <- list()
for( hIdx in 2:nrow(remainMtx) ){
    ml <- getMatchLst.fixed( remainMtx[hIdx,] ,remainMtx[1:(hIdx-1),,drop=F] )
    for( idx in seq_len(length(ml)) ){
        matLen <- length(ml[[idx]]$fIdx)
        if( 5<matLen ){
            matchLst[[1+length(matchLst)]] <- c( hIdx ,ml[[idx]]$hIdx ,matLen )
        }
    } # for(idx)
} # for(hIdx)
mtx <- do.call(rbind,matchLst)
sort( mtx[,1]-mtx[,2] )[1:10]
    #  2  12  24  44  48  57 105 112 129 129...

dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) )) # 54
testFlag <- rep(0,nrow(distMtx))
testFlag[dupIndices] <- 1
seqObj <- k.seq(testFlag)
zeroFlag <- seqObj$seqCntMtx[,"val"]==0
table(seqObj$seqCntMtx[!zeroFlag,"cnt"])
    # 1  2 
    #48  3 

# -----------------------------------------------------------------------
# ?´? „ h????˜ ê°„ê²©
# -----------------------------------------------------------------------
#   <?ƒ?„±>
hDistMtx <- abs(zh[1:(nrow(zh)-1),]-zh[2:nrow(zh),])
#   <ë¶„ì„>
matchLst <- list()
for( hIdx in 2:nrow(hDistMtx) ){
    ml <- getMatchLst.fixed( hDistMtx[hIdx,] ,hDistMtx[1:(hIdx-1),,drop=F] )
    for( idx in seq_len(length(ml)) ){
        matLen <- length(ml[[idx]]$fIdx)
        if( 3<matLen ){
            matchLst[[1+length(matchLst)]] <- c( hIdx ,ml[[idx]]$hIdx ,matLen )
        }
    } # for(idx)
} # for(hIdx)
mtx <- do.call(rbind,matchLst)
sort( mtx[,1]-mtx[,2] )[1:10]
    # 13ê¹Œì???Š” 4ê°? ?´?ƒ ë§¤ì¹˜ê°? ?—†?‹¤ê³? ë´ë„ ?  ?“¯.
    #  1  4  4 13 25 37 37 38 39 44...

dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) )) # 94
testFlag <- rep(0,nrow(distMtx))
testFlag[dupIndices] <- 1
seqObj <- k.seq(testFlag)
zeroFlag <- seqObj$seqCntMtx[,"val"]==0
table(seqObj$seqCntMtx[!zeroFlag,"cnt"])
    # 3 ?´ ìµœë??.
    # 1  2  3 
    #79  6  1
 
# -----------------------------------------------------------------------
# ì½”ë“œê°? ê°„ê²©
# -----------------------------------------------------------------------
distMtx <- zh[,2:6]-zh[,1:5]
matchLst <- list()
for( hIdx in 2:nrow(distMtx) ){
    ml <- getMatchLst.fixed( distMtx[hIdx,] ,distMtx[1:(hIdx-1),,drop=F] )
    for( idx in seq_len(length(ml)) ){
        matLen <- length(ml[[idx]]$fIdx)
        if( 3<matLen ){
            matchLst[[1+length(matchLst)]] <- c( hIdx ,ml[[idx]]$hIdx ,matLen )
        }
    } # for(idx)
} # for(hIdx)
mtx <- do.call(rbind,matchLst)
sort( mtx[,1]-mtx[,2] )[1:10]
    #  4 19 25 30 50 54 61 62 74 86

dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) )) # 56
testFlag <- rep(0,nrow(distMtx))
testFlag[dupIndices] <- 1
seqObj <- k.seq(testFlag)
zeroFlag <- seqObj$seqCntMtx[,"val"]==0
table(seqObj$seqCntMtx[!zeroFlag,"cnt"])
    # 3 ?—°?†?´ ìµœë??..
    #    1  2  3 
    #   45  4  1 

# -----------------------------------------------------------------------
# cliper.rebLen()
# -----------------------------------------------------------------------
rebMtx <- matrix( 0 ,nrow=nrow(zh) ,ncol=ncol(zh) )
rebMtx[1,] <- NA    # në²ˆì§¸?— ?‚˜?™”?Œ?„ ?˜ë¯?. ì¦? ë°”ë¡œ ?´? „?— ?‚˜?™”?œ¼ë©? 1
for( hIdx in 2:nrow(zh) ){
    ml <- getReboundLst( zh[hIdx,] ,zh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
    rebMtx[hIdx,] <- sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
} # for(hIdx)
naIndices <- which(apply(rebMtx,1,function(p){any(is.na(p))}))
    # 27ë²ˆì§¸ê¹Œì?? NA ë°œìƒ.
rebMtx <- rebMtx[(max(naIndices)+1):nrow(rebMtx),]

matchLst <- list()
for( hIdx in 2:nrow(rebMtx) ){
    ml <- getMatchLst.fixed( rebMtx[hIdx,] ,rebMtx[1:(hIdx-1),,drop=F] )
    for( idx in seq_len(length(ml)) ){
        matchLen <- length(ml[[idx]]$fIdx)
        if( matchLen>3 ){
            matchLst[[1+length(matchLst)]] <-
                c( hIdx ,ml[[idx]]$hIdx ,matchLen)
        }
    }
} # for(hIdx)
mtx <- do.call(rbind,matchLst)   # 82
sort(mtx[,1]-mtx[,2])[1:10]
    #  2  2  5  7 26 26 27 27 35 37

dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) )) # 122
testFlag <- rep( 0 ,nrow(rebMtx) )
testFlag[dupIndices] <- 1
seqObj <- k.seq(testFlag)
zeroFlag <- seqObj$seqCntMtx[,"val"]==0
table(seqObj$seqCntMtx[!zeroFlag,"cnt"])
    #    1  2  3 
    #   87 13  3 

# -----------------------------------------------------------------------
#   cliper.dumNum()
# -----------------------------------------------------------------------
matchLst <- list()
for( hIdx in 2:nrow(zh) ){
    ml <- getMatchLst( zh[hIdx,] ,zh[1:(hIdx-1),,drop=F] )
    for( idx in seq_len(length(ml)) ){
        matLen <- length(ml[[idx]]$fIdx)
        if(3<matLen){
            matchLst[[1+length(matchLst)]] <- 
                c( hIdx ,ml[[idx]]$hIdx ,matLen)
        }
    } #for(idx)
}

mtx <- do.call( rbind ,matchLst )    # curIdx, matchIdx, matchNum
sort(mtx[,1]-mtx[,2])[1:10]
    # 1  2  2  2  3  3  8  8  8 11

dupIndices <- sort(unique( c(mtx[,1],mtx[,2]) ))    # 398

testFlag <- rep(0,nrow(zh))
testFlag[dupIndices] <- 1
seqObj <- k.seq(testFlag)
zeroFlag <- seqObj$seqCntMtx[,"val"]==0
table(seqObj$seqCntMtx[!zeroFlag,"cnt"])
    #    1  2  3  4  5  6  7 11 12 
    #    85 41 22 17 11  2  1  1  1 







