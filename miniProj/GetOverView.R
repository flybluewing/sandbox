
score.df <- data.frame( hit=integer(0), element=integer(0), depth=integer(0) )
for( depth in 1:4){
	for( idx in nrow(RawZH):(tGlobal$sampleStart) ){
		# myLog( sprintf("%2d th for",idx) )
		score <- grOne( RawZH, idx-1, depth );
		score.df <- rbind( score.df, score )
	}
}
colnames(score.df) <- c("hit","element","depth")

score.df$fhit <- factor( score.df$hit, 0:max(score.df$hit) )
score.df$fdepth <- factor( score.df$depth, 0:max(score.df$depth) )


