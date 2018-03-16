# 20180109_C.R 교차모델
#	Code : cf - cross filter
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

hFltCnt <- sapply( fRstLst ,length )
hFlt.cnt1 <- sapply( fRstLst[hFltCnt==1] ,function(p){p[1]} )
#   tblPer.h <- table(hFlt.cnt1)*100/length(hFlt.cnt1)
#   tblPer.h[c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")]

k.idx <- hFlt.cnt1 %in% c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")


# --[allZoidMtx]----------------------------------------------------
azFltLst <- vector("list",nrow(gEnv$allZoidMtx))
for( fnIdx in attributes(remLst)$names ){
    for( zIdx in remLst[[fnIdx]] ){
        azFltLst[[zIdx]][1+length(azFltLst[[zIdx]])] <- fnIdx
    }
}
azFltCnt <- sapply( azFltLst ,length )

azflt.cnt1 <- sapply( azFltLst[which(azFltCnt==1)] ,function(p){p[1]} )
#   tblPer.az <- table(azflt.cnt1)*100/length(azflt.cnt1)
#   tblPer.az[c("A0100.A","A0110.A","AP000.E","C1000.A","AR000.B","D0000.A")]

azFlt.cnt0 <- which(azFltCnt==0)
allZoidMtx <- gEnv$allZoidMtx[azFlt.cnt0,]

# 23, 38  보유 제외
filtFlag <- apply( allZoidMtx ,1 ,function(p){ any(c(23,38) %in% p) })
allZoidMtx <- allZoidMtx[!filtFlag,] #  448655 

# 다음 연발 제외 :  c(21,22) ,c(31,32) ,c(39,40) ,c(40,41)
filtFlag <- apply( allZoidMtx ,1 ,function(p){ all(c(21,22) %in% p) })
allZoidMtx <- allZoidMtx[!filtFlag,] #  439335 
filtFlag <- apply( allZoidMtx ,1 ,function(p){ all(c(31,32) %in% p) })
allZoidMtx <- allZoidMtx[!filtFlag,] #  439335
filtFlag <- apply( allZoidMtx ,1 ,function(p){ all(c(39,40) %in% p) })
allZoidMtx <- allZoidMtx[!filtFlag,] #  429950
filtFlag <- apply( allZoidMtx ,1 ,function(p){ all(c(40,41) %in% p) })
allZoidMtx <- allZoidMtx[!filtFlag,] #  427212

# 1 연속이 1개인 것은 제외
cnt <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){ sum(p==1) })
allZoidMtx <- allZoidMtx[cnt!=1,]   #  246002

#   %% 10 값에 대해서..
flag <- apply( allZoidMtx%%10 ,1 ,function(p){ (sum(p==1)==2) && (sum(p==6)==2)  })
allZoidMtx <- allZoidMtx[!flag,]    # 243988
flag <- apply( allZoidMtx%%10 ,1 ,function(p){ (sum(p==2)==2) && (sum(p==2)==2)  })
allZoidMtx <- allZoidMtx[!flag,]    # 225865
flag <- apply( allZoidMtx%%10 ,1 ,function(p){ (sum(p==3)==2) })
allZoidMtx <- allZoidMtx[!flag,]    # 26337

# 1 연속이 2개인 것도 제외
cnt <- apply( allZoidMtx[,3:6]-allZoidMtx[,1:4] ,1 ,function(p){ sum(p==2) })
allZoidMtx <- allZoidMtx[cnt==0,]   # 27500


# zoid[4] 는 10 보다 크다.
flag <- apply( allZoidMtx ,1 ,function(p){ p[4]<=10 })
allZoidMtx <- allZoidMtx[!flag,]    # 25771

# zoid[3]은 26 아님. (40->32->26)
flag <- apply( allZoidMtx ,1 ,function(p){ p[3]==26 })
allZoidMtx <- allZoidMtx[!flag,]    # 

# zoid[3] %% 10 은 7이 아님.(25,35,..,26,36,..,x7)
flag <- apply( allZoidMtx ,1 ,function(p){ p[3]==7 })
allZoidMtx <- allZoidMtx[!flag,]    # 

# zoid[5] %% 10 은 0이 아님.(30->34->40->39->)
flag <- apply( allZoidMtx ,1 ,function(p){ p[5]==0 })
allZoidMtx <- allZoidMtx[!flag,]    # 

# zoid[c(3,5)] %% 10 은 9 아님.(2,7,19,25,29,36)
flag <- apply( allZoidMtx ,1 ,function(p){ all(p[c(3,5)]==9) })
allZoidMtx <- allZoidMtx[!flag,]    # 

# 같은 자리의 2개 값이 바로 다음에도 연속 발생할 수는 없겠지..
#   1 ,21 ,26 ,36 ,40 ,41
#   5 ,22 ,31 ,32 ,39 ,45
flag <- apply( allZoidMtx ,1 ,function(p){
                if( all(c(1,5)%in%p) ){  return( TRUE )
                } else if( all(c(21,22)%in%p) ) {  return( TRUE )
                } else if( all(c(26,31)%in%p) ) {  return( TRUE )
                } else if( all(c(36,32)%in%p) ) {  return( TRUE )
                } else if( all(c(40,39)%in%p) ) {  return( TRUE )
                } else if( all(c(41,45)%in%p) ) {  return( TRUE )
                } 
                return(FALSE)
            })
allZoidMtx <- allZoidMtx[!flag,]    # 

# 1 포함
flag <- apply( allZoidMtx ,1 ,function(p){ p[1]==1 })
allZoidMtx <- allZoidMtx[flag,]    # 28879



# =====================================================================




testSpan <- 300:nrow(gEnv$zhF)
scanDepth <- 100
seqDepth <- 2

bhTLst <- list()
for( tIdx in testSpan ){
    byTObj <- list( tIdx=tIdx )

    scanSpan <- 1:scanDepth
    byScanLst=list()
    for( scanIdx in scanSpan ){
        hSpan <- tIdx - 1:seqDepth - scanIdx + 1
        scanObj <- list( scanIdx=scanIdx ,hSpan=hSpan )
        byColLst <- list()
        for( cIdx in 1:6 ){
            if( all(gEnv$zhF[hSpan,cIdx] %in% gEnv$zhF[tIdx,]) ){
                if( seqDepth==length(unique(gEnv$zhF[hSpan,cIdx])) ){ # 같은 값인 경우 방지.
                    colObj <- list( cIdx=cIdx ,comCode=gEnv$zhF[hSpan,] )
                    byColLst[[1+length(byColLst)]] <- colObj
                }
            }
        }
        scanObj$byColLst <- byColLst

        byScanLst[[1+length(byScanLst)]] <- scanObj
    } # scanIdx

    byTObj$byScanLst <- byScanLst

    bhTLst[[1+length(bhTLst)]] <- byTObj
}

cnt.t <- sapply( bhTLst ,function( byTObj ){
                # byTObj <- bhTLst[[1]]
                cnt <- sapply( byTObj$byScanLst ,function(scanObj){length(scanObj$byColLst)})
                indices <- which(cnt>0)
                return( ifelse(0==length(indices),scanDepth,indices[1]) )
            })
# tbl <- table(cnt.t)
# sapply(1:15,function(p){sum(tbl[1:p])})





#-[B0000.A]------------------------------------------------------
#	
#   pEnv <- gEnv    ;pDepth=3   ;pSpanSize=15
filt_B0000.A <- function( pEnv ,pDepth=3 ,pSpanSize=15 ){

	filtId="B0000.A";	tStmp <- Sys.time()
	allZoidMtx <- pEnv$allZoidMtx;	zhF <- pEnv$zhF

    curHIdx <- nrow(zhF)
	flag <- rep( 0 ,nrow(allZoidMtx) )
    flag.leftIdx <- 1:nrow(allZoidMtx)
    for( sIdx in 1:pSpanSize ){
        hSpan <- (curHIdx-(1:pDepth)+1)-(sIdx-1)
        for( fIdx in flag.leftIdx ){
            for( cIdx in 1:6 ){
                if( all(zhF[hSpan,cIdx]%in%allZoidMtx[fIdx,]) ){
                    if( pDepth==length(unique(zhF[hSpan,cIdx])) ){
                        flag[fIdx] <- sIdx
                    }
                }
            }
        } # fIdx

        flag.leftIdx <- which(flag==0)
    }
	flag <- flag==0

	pEnv$logStr( sprintf("ID:%s rem:%d",filtId,sum(!flag)) )
	return( list(filtId=filtId ,flag=flag ,filtCnt=sum(!flag), tCost=(Sys.time()-tStmp)) 
		)

} # filt_B0000.A()

