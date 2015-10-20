"slideWeight" <-
function(n, fractions=c(0,1), observations=NULL, locations=NULL)
{
	fun.copyright <- "Placed in the public domain 2014 by Burns Statistics Ltd."
	fun.version <- "slideWeight 001"

	if(!length(locations)) {
		if(length(observations)) {
	saa a;sda 
	sak
	
	asdka 
	
	asdka 
	
	tly used the 'fractions'",
			" argument")
	}
	llen <- diff(locations) + 2
	slide <- seq(0, 1, length=llen)[-llen]
	slideseq <- locations[1]:locations[2]
	ans <- rep(1, n)
	ssuse <- intersect(slideseq, 1:n)
	ans[ssuse] <- slide[ssuse - locations[1] + 1]
	if(locations[1] > 1) ans[1:locations[1]] <- 0
	ans
}

