# Define functions
FIT.test  = function(df, one.sided=FALSE) {
  # Perform the Fitness Increment Test (Feder et al. 2014)
  # Args:
  #   df: binned data frame with three columns
  #     df$year : the midpoint year of a bin
  #     df$value : the number of tokens in a bin of variant 1
  #     df$count : the total number of tokens in a bin
  # 
  # Returns:
  #   FIT.p: the p-value returned by the FIT test
  #   FIT.stat: the test statistic of the FIT test
  #   Y.bar: the average of the scaled fitness increments
  #   n: number of bins
  #   mu: average number of tokens per bin
  #   sigma: standard deviation of number of tokens per bin
  #   W.stat: test statistic of Shapiro-Wilk test for normality
  #   W.p: p-value for Shapiro-Wilk test
  #   
  # Get number of bins from data
  q = nrow(df)
  # Create vector to hold fitness increments
  Y = rep(0,(q-1))
  # Get variant frequencies from df
  v = df$value/df$count
  # Get years from df
  t = df$year
  # Rescale increments according to definition
  for (i in c(2:q)) { # R indexes from 1 rather than 0
    Y[i-1] = (v[i] - v[i - 1])/sqrt(2*v[i-1]*(1 - v[i-1])*(t[i] - t[i-1]))
  }
  # Mean fitness increment
  Y.bar = mean(Y)
  # Get t statistic from rescaled fitness increments
  FIT.stat = as.numeric(t.test(Y)$statistic)
  # Calculate the p-value for the test statistic: 
  FIT.p = t.test(Y)$p.value
  if (one.sided) {FIT.p = t.test(Y, alternative="greater")$p.value}
  # Get mean number of tokens per bin
  mu = floor(mean(df$count))
  # Get standard deviation of tokens across bins
  sigma = floor(sd(df$count))
  # Shapiro-Wilk test : 
  #   Note that H_0 assumes that rescaled increments are normally distributed
  W.stat = shapiro.test(Y)$statistic
  W.p = shapiro.test(Y)$p.value
  # return values in vector
  values = list(FIT.p=FIT.p, FIT.stat=FIT.stat, Y.bar=Y.bar, q=q, mu=mu, sigma=sigma,
                W.stat=W.stat, W.p=W.p)
  return(values)
}

bin.by.quantile = function(df,bins=NA) {
  # Split data into varaible-width time bins
  #
  # Args:
  #   df : data frame with two columns 
  #     df$year : the year a given token occurs
  #     df$value : the binary value of the variants
  #     q : number of variable-width bins (quantiles) to split data
  #
  # Returns:
  #   data frame with data binned into variable-width bins of
  #   roughly equal size, where the year is the midpiont of the
  #   bin.
  #
  # Get quantiles
  df.quant = c()
  if (length(bins) == 1) {
    q = bins
    df.quant = as.vector(quantile(df$year, probs=seq(0,1,1/q))) }
  else {
    df.quant = bins }
  # Group data by quantiles
  sum.df = df %>% group_by(cut(year,df.quant, include.lowest=TRUE)) %>%
    summarize(value=sum(value), count=n())
  # Rename columns
  colnames(sum.df) = c('year','value','count')
  # Replace year ranges with midpoints
  years = as.integer((head(df.quant,-1) + tail(df.quant,-1))/2)
  sum.df$year = years
  return(sum.df)
}

extract.number = function(string) {
	if (length(string)){
		return(as.numeric(strsplit(string, ' = ')[[1]][2]))
	} else {
		return(NA)
	}
}

adjust.absorption = function(df) {
  # Fix counts to avoid absorption events
  #
  # Args:
  #   df : a data frame with 
  #
  # Returns:
  #   data frame with absorption events removed, all 0 values in df$value
  #   are move up to 1 and all values where df$value == df$count are moved
  #   down by one.
  adjusted.df = df %>% mutate(value = ifelse(value == count, count - 1, ifelse(value == 0, 1, value)))
  return(adjusted.df)
}

logistic = function(x0, t0, tN, s) {
	xt = c()
	
	for (i in c(1:(tN - t0))) {
		xt[i] = (x0/(1-x0))*exp(s*i)/(1 + (x0/(1-x0))*exp(s*i))
	}
	return(xt)
}

