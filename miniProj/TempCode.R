
tmpdf <- score.df
tmpdf$hitJ <- jitter(tmpdf$hit)	# pair( tmpdf )

plot( tmpdf$depth, jitter(tmpdf$hit) )
pred <- lm( tmpdf$hit ~ tmpdf$depth )
abline( pred )
pred.hit <- predict( pred, data.frame(depth=tmpdf$depth), interval="confidence" )
points( )

