



cnt <- apply( gEnv$zhF ,1 ,function(zoid){
	cStep <- zoid[2:6] - zoid[1:5]
	max(table(cStep))
})


lCStep <- c( 1, 2, 3, 1, 2 )

aZoidMtx <- rbind( aZoid ,aZoid-1 )
aZoidMtx <- rbind( aZoidMtx ,aZoid+1 )
aZoidMtx <- rbind( aZoidMtx ,aZoid+2 )

