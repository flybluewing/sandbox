
getRandomZoid <- function( pZoidH ,pMaxRepeat=100 ){
        
        newZoid <- NULL

        loopCum <- 0
        while( pMaxRepeat > loopCum ) {
            newZoid <- sort(sample(G01$dnaType,G01$dnaLength))
            hasIt <- apply( pZoidH ,1 ,function(pZoid){all(pZoid==newZoid)} )
            if( !any(hasIt) ){
                return( newZoid )
            }
            loopCum <- loopCum+1
        } # repeat
        print(sprintf("loopCum:%d(fail)",loopCum))
        return(NULL)
    }





zpAssess    <- function( pZoidPool ,... ){ UseMethod("zpAssess"  ,pZoidPool) }
zpBreed     <- function( pZoidPool ,pGenNum ,... ){ UseMethod("zpBreed"   ,pZoidPool) }
zpHarvest   <- function( pZoidPool ,... ){ UseMethod("zpHarvest" ,pZoidPool) }

zpAssess.ZoidPoolCls    <- function( pZoidPool ,... ){

        rZP <- pZoidPool
        
        # 평가
        efNames <- attributes(rZP$efLst)$names
        for( rIdx in 1:nrow(rZP$poolMtx) ){
            st  <- sapply( rZP$efLst ,function(pEff){ pEff$state(rZP$poolMtx[rIdx,]) } )
            ass <- sapply( rZP$efLst ,function(pEff){ pEff$assess(rZP$poolMtx[rIdx,]) } )
            rZP$effAtvMtx[rIdx,efNames]     <- st[efNames]
            rZP$effAssessMtx[rIdx,efNames]  <- ass[efNames]
        }
        rZP$score <- apply( rZP$effAssessMtx ,1 ,sum )
        zOrder <- order( rZP$score ,decreasing=T )

        # zoid 재정렬
        rZP$score   <- rZP$score[zOrder]
        rZP$poolMtx <- rZP$poolMtx[zOrder,]
        rZP$effAtvMtx       <- rZP$effAtvMtx[zOrder,]
        rZP$effAssessMtx    <- rZP$effAssessMtx[zOrder,]
        
        return( rZP )
    }

zpBreed.ZoidPoolCls     <- function( pZP ,pGenNum=10 ,... ){

        assZP <- pZP
        assZP$scoreHLst <- list()

        poolSize <- nrow( assZP$poolMtx )
        newPoolMtx <- matrix( 0 ,nrow=poolSize ,ncol=G01$dnaLength )
        for( gIdx in 1:pGenNum ){
            # 교배 ------------------------------------------
            xxIdx <- sample( 1:poolSize ,assZP$babyNum ,replace=T ,prob=assZP$mateSuperity )
            xyIdx <- sample( 1:poolSize ,assZP$babyNum ,replace=T ,prob=assZP$mateSuperity )
            for( rIdx in 1:assZP$babyNum ){
                newPoolMtx[rIdx,] <- assZP$mate( assZP$poolMtx[xxIdx[rIdx],] ,assZP$poolMtx[xyIdx[rIdx],] )
            }

            # 돌연변이 --------------------------------------
            mutateIdx <- sample( 1:assZP$babyNum ,assZP$mutateNum )
            for( rIdx in mutateIdx ){
                newPoolMtx[rIdx,] <- assZP$mutate( newPoolMtx[rIdx,] )
            }

            # 중복 존재 확인 --------------------------------
            dupIdx <- NULL
            for( rIdx in 1:(assZP$babyNum-1) ){
                for( rRIdx in (rIdx+1):assZP$babyNum ){
                    if( all(newPoolMtx[rIdx,] == newPoolMtx[rRIdx,]) ){
                        dupIdx <- c(dupIdx,rRIdx)
                    }
                } # for( rRIdx)
            }
            dupIdx <- unique(dupIdx)    # 동일 zoid가 3개 이상인 경우를 위해.

            # 랜덤 zoid 추가 --------------------------------
            rZoidIdx <- c( dupIdx ,(assZP$babyNum+1):poolSize )  # 중복발생 부분과 랜덤zoid 할당영역.
            for( rIdx in rZoidIdx ){
                newPoolMtx[rIdx,] <- getRandomZoid( newPoolMtx )
            }

            # 평가 ------------------------------------------
            assZP$poolMtx <- newPoolMtx
            assZP <- zpAssess( assZP )

            # 기록 ------------------------------------------
            breedLength <- as.character( gIdx )
            assZP$scoreHLst[[breedLength]] <- assZP$score

        } # for(gIdx)

        return( assZP )
    }

zpHarvest.ZoidPoolCls   <- function( pZoidPool ,... ){
        # QQE
    }

ZoidPoolCls <- function( pEfLst ,pPoolSize=100 ,pBabyRatio=0.8 ,pMutateRatio=0.05 ){

        rObj <- list(); class(rObj) <- "ZoidPoolCls"
        rObj$efLst <- pEfLst
        efNames     <- attributes( efLst )$names
        rObj$mateSuperity   <- pPoolSize - (1:pPoolSize-1)  # 교배확률 가중치
        rObj$mateSuperity   <- rObj$mateSuperity^2.5        # 가중치 차이를 좀 크게 주자.
        rObj$babyNum        <- as.integer( pPoolSize*pBabyRatio )
        rObj$mutateNum      <- as.integer( pPoolSize*pMutateRatio )

        poolMtx <- matrix( ncol=G01$dnaLength ,nrow=0 )
        for( idx in 1:pPoolSize ){
            poolMtx <- rbind( getRandomZoid(poolMtx) ,poolMtx )
        }
        rObj$poolMtx    <- poolMtx
        rObj$score      <- rep( 5,pPoolSize );
        rObj$effAtvMtx     <- matrix( NA ,ncol=length(efLst) ,nrow=pPoolSize )
        colnames(rObj$effAtvMtx) <- efNames
        rObj$effAssessMtx  <- matrix( NA ,ncol=length(efLst) ,nrow=pPoolSize )
        colnames(rObj$effAssessMtx) <- efNames

        rObj$scoreHLst  <- list()   # 번식에 따른 Zoid들의 스코어 기록.( zpBreed()함수에 의해 증가.)

        rObj$mate   <- function( pZoid1 ,pZoid2 ){
                dna <- c(pZoid1,pZoid2)
                baby <- unique(sample(dna,G01$dnaLength))
                while( G01$dnaLength>length(baby) ){
                    dna <- setdiff( dna ,baby )
                    baby <- c( baby ,sample(dna,1) )
                }
                return(sort(baby))
            }
        rObj$mutate <- function( pZoid ){
                newDna <- setdiff( G01$dnaType ,pZoid )
                pZoid <- sample(pZoid,length(pZoid)-1)
                return( sort(c( pZoid ,sample(newDna,1) )) )
            }

        return(rObj)
    }

getEfLst <- function( pZoidH ){
        rObj <- list()
        ef01 <- Ef01Cls( pZoidH );      rObj[[ef01$sid]] <- ef01
        ef02 <- Ef02Cls( pZoidH );      rObj[[ef02$sid]] <- ef02
        ef03 <- Ef03Cls( pZoidH );      rObj[[ef03$sid]] <- ef03
        ef04 <- Ef04Cls( ef01 ,ef02 );  rObj[[ef04$sid]] <- ef04
        ef05 <- Ef05Cls( pZoidH );      rObj[[ef05$sid]] <- ef05
        ef06 <- Ef06Cls( pZoidH );      rObj[[ef06$sid]] <- ef06
        ef07 <- Ef07Cls( pZoidH );      rObj[[ef07$sid]] <- ef07
        ef08 <- Ef08Cls( pZoidH );      rObj[[ef08$sid]] <- ef08
        ef09 <- Ef09Cls( pZoidH );      rObj[[ef09$sid]] <- ef09
        ef10 <- Ef10Cls( ef08 ,ef09 );  rObj[[ef10$sid]] <- ef10
        return( rObj )
    }


Ef10Cls <- function( pEf08 ,pEf09 ){

        if( !(pEf08$sid=="Ef08Cls" && pEf09$sid=="Ef09Cls") ){
            print(sprintf("Error[Ef10Cls] creater param error : %s %s" ,pEf08$sid ,pEf09$sid))
            return( NULL )
        }
        
        rObj <- list( sid="Ef10Cls" )
        rObj$activated <- pEf08$activated && pEf09$activated
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated ,any(pZoid==3) ,NA ) )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state(pZoid)
                return( ifelse( rObj$activated ,ifelse(st,10,0) ,5 ) )
            }
        return( rObj )
    }

Ef09Cls <- function( pZH ){
        rObj <- list( sid="Ef09Cls" )

        rIdx <- nrow(pZH)
        rObj$activated <- (2==sum(pZH[rIdx,]%%2==0)) && (2==sum(pZH[(rIdx-1),]%%2==0))
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated ,(4==sum(pZoid%%2==0)) ,NA ) )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state(pZoid)
                return( ifelse( rObj$activated ,ifelse(st,10,0) ,5 ) )
            }
        return( rObj )
    }

Ef08Cls <- function( pZH ){
        rObj <- list( sid="Ef08Cls" )

        rIdx <- nrow(pZH)
        rObj$activated <- any(pZH[rIdx,]==17) || any(pZH[(rIdx-1),]==17)
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated ,any(pZoid==9) ,NA ) )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state(pZoid)
                return( ifelse( rObj$activated ,ifelse(st,10,0) ,5 ) )
            }
        return( rObj )
    }

Ef07Cls <- function( pZH ){
        rObj <- list( sid="Ef07Cls" )
        
        rIdx <- nrow(pZH)
        rObj$activated <- any(pZH[rIdx,]==7) && any(pZH[(rIdx-1),]==7)
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated ,any(pZoid==10) ,NA ) )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state(pZoid)
                return( ifelse( rObj$activated ,ifelse(st,10,0) ,5 ) )
            }
        return( rObj )
    }

Ef06Cls <- function( pZH ){
        rObj <- list( sid="Ef06Cls" )
        rObj$activated <- 2 == length(intersect(pZH[nrow(pZH),],c(3,7)))
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated ,any(pZoid==20) ,NA ) )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state(pZoid)
                return( ifelse( rObj$activated ,ifelse(st,10,0) ,5 ) )
            }
        return( rObj )
    }

Ef05Cls <- function( pZH ){
        rObj <- list( sid="Ef05Cls" )
        rObj$activated <- ( 3 == sum(0==(pZH[nrow(pZH),]%%2)) )
        rObj$state <- function( pZoid ){
                return( ifelse( rObj$activated
                            ,( 2 == sum(0==(pZoid%%2)) )
                            ,NA )
                    )
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state( pZoid )
                return(ifelse( rObj$activated ,ifelse(st,10,0) ,5 ))
            }
        return( rObj )
    }

Ef04Cls <- function( pEf01 ,pEf02 ){

        if( !(pEf01$sid=="Ef01Cls" && pEf02$sid=="Ef02Cls") ){
            print(sprintf("Error[Ef04Cls] creater param error : %s %s" ,pEf01$sid ,pEf02$sid))
            return( NULL )
        }
        rObj <- list( sid="Ef04Cls" )
        rObj$activated <- (pEf01$activated && pEf02$activated)
        rObj$state <- function( pZoid ){
                return(ifelse( rObj$activated ,any(pZoid==7) ,NA ))
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state( pZoid )
                return(ifelse( rObj$activated ,ifelse(st,10,0) ,5 ))
            }
        return( rObj )
    }

Ef03Cls <- function( pZH ){
        rObj <- list( sid="Ef03Cls" )
        rObj$activated <- T # 항상 True
        rObj$lastH <- pZH[nrow(pZH),]
        rObj$state <- function( pZoid ){
                return( 2==length(intersect(rObj$lastH,pZoid)) )
            }
        rObj$assess <- function( pZoid ){
                return( ifelse( rObj$state(pZoid) ,10 ,0 )
                    )
            }
        return( rObj )
    }

Ef02Cls <- function( pZH ){
        rObj <- list( sid="Ef02Cls" )

        lastH <- pZH[nrow(pZH),]
        rObj$activated <- any(lastH==10) || any(lastH==12)
        rObj$state <- function( pZoid ){
                return(ifelse( rObj$activated ,any(pZoid==15) ,NA ))
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state( pZoid )
                return(ifelse( rObj$activated, ifelse(st,10,0), 5 ))
            }
        return( rObj )
    }

Ef01Cls <- function( pZH ){
        rObj <- list( sid="Ef01Cls" )
        rObj$activated <- any(pZH[nrow(pZH)]==3)
        rObj$state <- function( pZoid ){
                return(ifelse( rObj$activated ,any(pZoid==5) ,NA ))
            }
        rObj$assess <- function( pZoid ){
                st <- rObj$state( pZoid )
                return(ifelse( rObj$activated ,ifelse(st,10,0 ) ,5 ))
            }
        return( rObj )
    }
