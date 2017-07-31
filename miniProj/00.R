# source("./lab/00/00.R")

set.seed(1)
source("00_H.R")
G <- list()
G$hSize <- 200
G$sSize <- 100 # sampleSize
G$code <- 0:1

colnames(probMtx) <- as.character(G$code)
# ---------------------------------------------------------------
# [Sampling]
# ---------------------------------------------------------------
rst <- list()
hFlag.seq.df <- NULL
for( pIdx in 1:nrow(probMtx) ) {

    myFLog(sprintf("pIdx:%d",pIdx),"00.R.log")
    hFlag <- sample( G$code, G$hSize, replace=T, prob=probMtx[pIdx,] )
    hSpan <- (G$hSize/2):G$hSize


    fLst <- t00.filters()
    fLst.names <- sapply( fLst, function(pObj){return(pObj$name)} )

    fRst <- list( std=hFlag[hSpan] );   names(fRst$std) <- as.character(hSpan)
    fRst$probMtx <- probMtx[pIdx,]  # 코드값 분포 비율.
    names(fRst$probMtx) <- as.character(G$code)
    fRst$fSamLst <- list( )         # 각 filter별 sample추출 결과 Object
    for( fIdx in fLst ){
        fRst$fSamLst[[fIdx$name]] <- matrix( ncol=G$sSize, nrow=length(hSpan) )
        rownames( fRst$fSamLst[[fIdx$name]] ) <- as.character(hSpan)
    }

    # ------------------------------------------------------------
    hFlag.seq <- rep(0,length(hFlag))
    t.idx <- 1:length(hFlag)
    for( idx in 1:(length(hFlag)-1) ){
        t.idx <- t.idx[0<t.idx-idx]
        t.idx <- t.idx[ hFlag[t.idx]==hFlag[t.idx-idx] ]
        hFlag.seq[t.idx] <- 1+hFlag.seq[t.idx]
        if( 0 == length(t.idx) ){
            break
        }
    }

    hFlag.df <- data.frame( idx=1:length(hFlag), val=hFlag, pIdx=pIdx )
    hFlag.df$valProb <- sapply( hFlag, function(pV){probMtx[pIdx,as.character(pV)]} )
    hFlag.df$pVal <- c( NA, hFlag[1:(length(hFlag)-1)] )
    hFlag.df$pVal.seq <- c( NA, hFlag.seq[1:(length(hFlag)-1)] )
    for( fIdx in fLst ){
        hFlag.df[[fIdx$name]] <- 0
    }
    # ------------------------------------------------------------

    for( hIdx in hSpan ){ # 차후 병렬처리로 전환.
        hObj <- t00.get_hSeq(hFlag[1:(hIdx-1)])
        for( fIdx in fLst ){
            fRst$fSamLst[[fIdx$name]][as.character(hIdx),] <- fIdx$sample( hObj, G$sSize )
            hFlag.df[hIdx,fIdx$name] <- 
                sum( hFlag.df[hIdx,"val"] == fRst$fSamLst[[fIdx$name]][as.character(hIdx),] ) / G$sSize
        }
    }

    rst[[as.character(pIdx)]] <- fRst

    if( is.null(hFlag.seq.df) ){
        hFlag.seq.df <- hFlag.df[hSpan,]
    } else {
        hFlag.seq.df <- rbind(hFlag.seq.df,hFlag.df[hSpan,])
    }

} # for( pIdx )

# ---------------------------------------------------------------
# [Assessment]
# ---------------------------------------------------------------
rst.df <- NULL
probs <- attributes(rst)$names
for( pIdx in probs ){
    # pIdx <- "7"
    std <- rst[[pIdx]]$std
    prob <- rst[[pIdx]]$probMtx[1]
    fNames <- attributes(rst[[pIdx]]$fSamLst)$names
    for( fIdx in fNames ){
        # fIdx <- "t00.sam00_Freq"
        h <- rownames(rst[[pIdx]]$fSamLst[[fIdx]])
        m <- rep(0,length(h)); names(m) <- h
        for( hIdx in h ){
            m[hIdx] <- mean( std[hIdx] == rst[[pIdx]]$fSamLst[[fIdx]][hIdx,] )
        }

        f.df <- data.frame( fName=fIdx, his=as.integer(h), hitRate=m, p=prob )
        if( is.null(rst.df) ){
            rst.df <- f.df
        } else {
            rst.df <- rbind( rst.df, f.df )
        }
    } # for(fIdx)
} # for(pIdx)


# ---------------------------------------------------------------
# [Review]
# ---------------------------------------------------------------

# boxplot( hitRate~p+fName, data=rst.df, main="hitRate~p+fName" )
    # * 필터링 정확도 측정
    # * 필터링 오류위치 측정.

# fName <- "t00.sam00_Seq02";   f <- rst.df$fName==fName
# boxplot( hitRate~p, data=rst.df, subset=f, main=fName )



# my.th <- ddply( rst.df, .(fName), function(p){quantile(p$hitRate,0.25)} )

plot( hitRate~p, data=rst.df, ylim=c(0,1) )

fLst <- sort(unique(rst.df$fName))
rst.th <- tapply( rst.df$hitRate, rst.df$fName, quantile, prob=0.25 )
p.span <- seq(5,100,5)
cStr <- c("red","blue","green","beige","brown","navyblue","purple")

for( fIdx in 1:length(fLst) ){

    t.th <- quantile(rst.df[rst.df$fName==fLst[fIdx],]$hitRate, prob=0.25 )
    rst.df.f <- rst.df[(rst.df$fName==fLst[fIdx])&(rst.df$hitRate>t.th),]

    t.lm <- lm( hitRate~poly(p,5), data=rst.df.f )
    t.pred <- predict( t.lm, data.frame(p=p.span), interval="confidence" )
    lines( p.span, t.pred[,"fit"], col=cStr[fIdx] )
    # lines( p.span, t.pred[,"upr"], col=cStr[fIdx] )
    # lines( p.span, t.pred[,"lwr"], col=cStr[fIdx] )
}


