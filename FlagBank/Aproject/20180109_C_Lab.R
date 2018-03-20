# 20180109_C.R 교차모델
#	Code : cf - cross filter
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "Z797"
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

azFlt.cnt1 <- which(azFltCnt==1)

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
allZoidMtx <- allZoidMtx[flag,]    # 18361

# 각 col 값에서의 3연속 col값이 같은 것 제외.
#   zoid[1]에 대해 zoid[1:3]의 제거대상.
for( colIdx in 1:6 ){
    lastPtnObj <- last3Ptn.ana( gEnv$zhF ,colIdx ,pThld=10 )
    colSpan <- lastPtnObj$colSpan

    flag <- rep( FALSE ,nrow(allZoidMtx) )
    for( aIdx in 1:nrow(allZoidMtx) ){
        anaObj <- lastPtnObj$getAnaObj( allZoidMtx[aIdx,colIdx] )
        if( is.null(anaObj) ){
            next
        }

        for( stdIdx in 1:nrow(anaObj$stdCodeMtx) ){
            if( all(anaObj$stdCodeMtx[stdIdx,]==allZoidMtx[aIdx,colSpan] ) ){
                flag[aIdx] <- TRUE
                break
            }
        }
    } # aIdx

    allZoidMtx <- allZoidMtx[!flag,]
    cat(sprintf("last3Ptn.ana col %d left %d \n",colIdx,nrow(allZoidMtx)))

} # colIdx


banObj <- getCFltObj( gEnv )
codeLst <- banObj$getCodeLst( allZoidMtx )
bRstObj <- ban.hntSameRow(banObj ,allZoidMtx ,pCodeLst=codeLst)
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}

bRstObj <- ban.hntCrossDim(banObj ,allZoidMtx ,pCodeLst=codeLst ,pDepth=2)
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}


bRstObj <- ban.multiDim(banObj ,allZoidMtx ,pCodeLst=codeLst )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}

bRstObj <- ban.throughH(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}

bRstObj <- ban.throughH2(banObj ,allZoidMtx ,pCodeLst=codeLst ,pLevel="hard" )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}

banCmbObj <- getCFltCmbObj( gEnv )
codeCmbLst <- banCmbObj$getCodeLst( allZoidMtx )
bRstObj <- ban.hntSameRow(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst)
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}
bRstObj <- ban.hntCrossDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst ,pDepth=2)
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}
bRstObj <- ban.multiDim(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}
bRstObj <- ban.throughH(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}
bRstObj <- ban.throughH2(banCmbObj ,allZoidMtx ,pCodeLst=codeCmbLst )
if( 0<length(bRstObj$filtedIdx) ){
    allZoidMtx <- allZoidMtx[-bRstObj$filtedIdx,]
}

# 1 포함 시, 최소 폭...
#   마지막은 폭이 40이었고, 26이하는 6%(7개)
# std.width <- apply( gEnv$zhF[gEnv$zhF[,1]==1,] ,1 ,function(p){p[6]-p[1]} )
all.width <- apply( allZoidMtx ,1 ,function(p){p[6]-p[1]})
flag <- all.width<=26
allZoidMtx <- allZoidMtx[!flag,]    # 3075

#     tail(gEnv$zhF[,2:6]-gEnv$zhF[,1:5])
#                 E2 E3 E4 E5 E6
#             792  5 12  6  4  7
#             793  5  6 14  3  5
#             794  1 11  1 11  8
#             795  7  3 13  8  4
#             796 20  5 10  4  1
#             797 17  9  1  7  6
#   796,797의 간격 스텝 재발은 없다고 가정하자.
flag <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){
                if( p[2]%in%c( 5, 9) ){ return(2)
                } else if( p[3]%in%c(10, 1) ){ return(3)
                } else if( p[4]%in%c( 4, 7) ){ return(4)
                } else if( p[5]%in%c( 1, 6) ){ return(5)
                }
                return(0)
            })
allZoidMtx <- allZoidMtx[flag==0,]

flag <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){
                if( 1<sum(p==c(1,11,1,11,8)) ){ return(794)
                } else if( 1<sum(p==c(7,3,13,8,4)) ){ return(795)
                }
                return(0)
            })
allZoidMtx <- allZoidMtx[flag==0,]

flag <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){
                mCnt <-      sum(p==c( 5 , 6 ,14 , 3 , 5))
                mCnt <- mCnt+sum(p==c( 1 ,11 , 1 ,11 , 8))
                mCnt <- mCnt+sum(p==c( 7 , 3 ,13 , 8 , 4))
                return(mCnt)
            })
allZoidMtx <- allZoidMtx[flag<3,]

# 최근 5개 H 내에서 재발 수가 4개 이상 제외.
banPool <- as.vector(gEnv$zhF[nrow(gEnv$zhF):(nrow(gEnv$zhF)-4),])
banPool <- sort(unique(banPool))
flag <- apply( allZoidMtx ,1 ,function(p){ sum(p%in%banPool) })
allZoidMtx <- allZoidMtx[flag<4,]


stdCodeMtx <- gEnv$zhF[gEnv$zhF[,1]==1,]
stdCodeMtx <- stdCodeMtx[,2:6]-stdCodeMtx[,1:5]
#     > tail(stdCodeMtx)
#                 E2 E3 E4 E5 E6
#             745  1  1  6  3 11
#             750  1 13  4  5 12
#             762  2  9  9  5 15
#             765  2  5  4 30  1
#             770  8  3 11 16  4
#             796 20  5 10  4  1
flag <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){
                if( p[2]%in%c( 3 ) ){ return(2) # 5,3,5,3..?
                } else if( p[3]%in%c(9) ){ return(3)   # 11,10,9..?
                # } else if( p[4]%in%c(4) ){ return(4) # 추측되는 게 없다.
                } else if( p[5]%in%c(4) ){ return(5)   # 1,4,1,4..?
                }
                return(0)
            })
allZoidMtx <- allZoidMtx[flag==0,]

flag <- apply( allZoidMtx[,2:6]-allZoidMtx[,1:5] ,1 ,function(p){
                if( 1<sum(p==c(2 ,5 ,4 ,30 , 1)) ){ return(762)
                } else if( 1<sum(p==c(2 , 9 , 9 , 5 ,15)) ){ return(765)
                }
                return(0)
            })
allZoidMtx <- allZoidMtx[flag==0,]


stdCodeMtx <- gEnv$zhF[gEnv$zhF[,1]==1,]
banPool <- as.vector(stdCodeMtx[nrow(stdCodeMtx):(nrow(stdCodeMtx)-4),])
banPool <- sort(unique(banPool))
flag <- apply( allZoidMtx ,1 ,function(p){ sum(p%in%banPool) })
allZoidMtx <- allZoidMtx[flag<5,]

# zh[,1]==1 에서, 최근 12가 너무 많이 나왔다.
flag <- apply( allZoidMtx ,1 ,function(p){ 12%in%p } )
allZoidMtx <- allZoidMtx[!flag,]

# colSpan 패턴반복
workCol <- 4
colSpan <- (workCol-1):(workCol+1)
codeSpan <- sort(unique(allZoidMtx[,workCol]))
for( codeIdx in codeSpan ){
    fndIdx <- which(stdCodeMtx[,workCol]==codeIdx)
    if( 0==length(fndIdx) ){
        next
    }
    stdCode <- stdCodeMtx[fndIdx[length(fndIdx)],colSpan]
    flag <- apply( allZoidMtx[,colSpan] ,1 ,function(p){all(p==stdCode)})
}


# 각 col 값에서의 3연속 col값이 같은 것 제외.
#   zoid[1]에 대해 zoid[1:3]의 제거대상.
for( colIdx in 4:6 ){
    lastPtnObj <- last3Ptn.ana( gEnv$zhF[gEnv$zhF[,1]==1,] ,colIdx ,pThld=10 )
    colSpan <- lastPtnObj$colSpan

    flag <- rep( FALSE ,nrow(allZoidMtx) )
    for( aIdx in 1:nrow(allZoidMtx) ){
        anaObj <- lastPtnObj$getAnaObj( allZoidMtx[aIdx,colIdx] )
        if( is.null(anaObj) ){
            next
        }

        for( stdIdx in 1:nrow(anaObj$stdCodeMtx) ){
            if( all(anaObj$stdCodeMtx[stdIdx,]==allZoidMtx[aIdx,colSpan] ) ){
                flag[aIdx] <- TRUE
                break
            }
        }
    } # aIdx

    allZoidMtx <- allZoidMtx[!flag,]
    cat(sprintf("last3Ptn.ana col %d left %d \n",colIdx,nrow(allZoidMtx)))

} # colIdx


# 1개 col 단위의 연속. 벼라별 가지가지 발악을 다 한다...
#   sort(unique(allZoidMtx[,6]))
depth <- 1
zh.len <- nrow(gEnv$zhF)
scanSpan <- 1:750
seqCode <- rep( NA ,6 )
for( colIdx in 2:6 ){
    lastSeqCode <- gEnv$zhF[zh.len:(zh.len-depth+1),colIdx]
    for( scanIdx in scanSpan ){
        mSpan <- (zh.len-scanIdx)-((1:depth)-1)
        if( all(lastSeqCode==gEnv$zhF[mSpan,colIdx]) ){
            seqCode[colIdx] <- gEnv$zhF[mSpan[1]+1,colIdx]
            break
        }
    }
}

# seqCode <- NA  3 16 31 21 41
flag <- apply( allZoidMtx[,2:6] ,1 ,function(p){ sum(p==c(3,16,31,21,41)) })
allZoidMtx <- allZoidMtx[flag<2,]

# 검토결과, 2:5 정도의 범위는 연속을 불허해도 될 거 같다..
flag <- apply( allZoidMtx[,2:5] ,1 ,function(p){ sum(p==c(3,16,31,21)) })
allZoidMtx <- allZoidMtx[flag==0,]



flag <- apply( allZoidMtx[,c(2,3)] ,1 ,function(p){
                if( all(p==c(7,17)) ){ return( TRUE )
                }
                return( FALSE )
            })
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,5)] ,1 ,function(p){
                if( all(p==c(7,31)) ){ return( TRUE )
                }
                return( FALSE )
            })
allZoidMtx <- allZoidMtx[!flag,]


flag <- apply( allZoidMtx[,c(2,6)] ,1 ,function(p){
                if( all(p==c(8,31)) ){ return( TRUE )
                }
                return( FALSE )
            })
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(5,6)] ,1 ,function(p){
                if( all(p==c(27,32)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,6)] ,1 ,function(p){
                if( all(p==c(7,33)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(5,6)] ,1 ,function(p){
                if( all(p==c(25,33)) ){ return( TRUE )
                } else if( all(p==c(27,33)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(4,6)] ,1 ,function(p){
                if( all(p==c(26,33)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,6)] ,1 ,function(p){
                if( all(p==c(2,35)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(5,6)] ,1 ,function(p){
                if( all(p==c(34,37)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,6)] ,1 ,function(p){
                if( all(p==c(8,40)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,6)] ,1 ,function(p){
                if( all(p==c(7,44)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(2,3)] ,1 ,function(p){
                if( all(p==c(2,8)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(3,6)] ,1 ,function(p){
                if( all(p==c(8,44)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(3,4)] ,1 ,function(p){
                if( all(p==c(13,16)) ){ return( TRUE )
                } else if( all(p==c(13,16)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(4,2)] ,1 ,function(p){
                if( all(p==c(25,8)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(4,6)] ,1 ,function(p){
                if( all(p==c(28,40)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c(5,6)] ,1 ,function(p){
                if( all(p==c(17,28)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 4)] ,1 ,function(p){
                if( all(p==c(24,19)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 4)] ,1 ,function(p){
                if( all(p==c(25,24)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 4)] ,1 ,function(p){
                if( all(p==c(28,22)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 4)] ,1 ,function(p){
                if( all(p==c(28,20)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 4)] ,1 ,function(p){
                if( all(p==c(29,26)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 2)] ,1 ,function(p){
                if( all(p==c(30, 6)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 3)] ,1 ,function(p){
                if( all(p==c(30,17)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)
allZoidMtx <- allZoidMtx[!flag,]

flag <- apply( allZoidMtx[,c( 5, 6)] ,1 ,function(p){
                if( all(p==c(32,36)) ){ return( TRUE )
                }
                return( FALSE )
            })  ;table(flag)


# apply(allZoidMtx, 2 ,function(p){table(p)})

lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
cnt <- apply( allZoidMtx ,1 ,function(p){ sum(p%in%lastZoid) })


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

