source("01_H.R")

set.seed(1)

G01 <- TGlobal()
G01$dnaType <- 1:20
G01$rawZH <- matrix( nrow=0 ,ncol=G01$dnaLength )
for( idx in nrow(G01$rawZH):5 ){
    newZoid <- getRandomZoid( G01$rawZH )
    G01$rawZH <- rbind( newZoid ,G01$rawZH )
}

efLst <- getEfLst( G01$rawZH )
pZoidPool <- ZoidPoolCls( efLst )

