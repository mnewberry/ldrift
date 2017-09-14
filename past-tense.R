path.to.tsinfer = "~/src/tsinfer/tsinfer"

if(!file.exists(path.to.tsinfer)) {
  cat("Could not find tsinfer.  Please specify a path by editing this file.\n")
  quit() }

#
print("Loading libraries and functions")
# Import libraries
library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
source("helper.R")
# Load in the data
print("Loading and formatting data")
# Read in the data
# NOTE: Can we speed this up by specifying colclass?
data.COHA.full = tbl_df(read.delim('local/data/vvd-COHA', header=TRUE, col.names = c("genre","year","id","vvd","lemma"), fill=T, 
                 skipNul=T, colClasses = c("character", "integer", "numeric", "character", "character")))
# Filter out:
#   non-identified documents: # NOTE: How does this happen? In grep?
#       data.COHA.full %>% filter(is.na(id)) 
#   and empty lemmas: 
#       data.COHA.full %>% filter(lemma=="") # These looks like mispellings or mistokenizing
data.COHA = data.COHA.full %>% filter(!is.na(id)) %>% filter(lemma!="")
data.COHA = data.COHA %>% mutate(year = as.integer(as.character(year))) # make sure year is numeric
# Look at the distribution of tokens over time
# year.count = data.COHA %>% group_by(year) %>% summarize(count=n())
# ggplot(year.count, aes(year, log(count))) + geom_point() + geom_line() # Tokens are on the same order of magnitude after 1850 or so

# Find sufficiently frequent lemmas (n > 50) that have multiple past tense forms
variants.COHA = data.COHA %>% group_by(lemma,vvd) %>% summarize(variant.count=n()) %>% 
  filter(variant.count>50) %>% group_by(lemma) %>% mutate(form.count=n()) %>% filter(form.count>1)
# We manually filter this list to exclude:
#   -Changes in spelling conventions: cancelled vs. canceled
#   -Past participles: (has) seen vs. saw
#   -Differences in meaning: He bored a hole in the wood vs. He bored them to death
#   -Verbs with multiple irregular forms
#         multiple.irregular.variants = c('begin', 'bid', 'drink', 'ring', ('run'),
#                                       ('see'), 'shrink', 'sing', 'sink', 'speak',
#                                       'spit', 'spring', 'write')
lemmas.to.use = c('awake', 'build', 'burn', 'catch', 'dive', 'draw', 'dream',
                    'dwell', 'grow', 'hang', 'hear', 'heave', 'kneel', 'knit', 
                    'know', 'lay', 'lean', 'leap', 'learn', 'light', 'plead', 
                    'quit', 'shine', 'smell', 'sneak', 'speed', 'spell', 'spill', 'spoil',
                    'strew', 'tell', 'throw', 'wake', 'weave', 'wed', 'wet')       

print("Selected data to use")
data.to.use = data.COHA %>% filter(id  != 762347) %>% filter(lemma %in% lemmas.to.use) %>% # BeetonsBookNeedlework: knitting book with instructions: data.COHA %>% filter(id  == 762347, lemma == "knit")
  group_by(lemma,vvd) %>% mutate(variant.count=n()) %>% filter(variant.count>50)

data.binned = c() # Store data for visualization of all time series
data.results = c() # Store results for analysis

print("Running FIT test and inferring population size")
dir.create('local/out/variants', recursive=TRUE, mode="0777", showWarnings=FALSE)
dir.create('local/out/tsinfer', recursive=TRUE, mode="0777", showWarnings=FALSE)
# Loop over all lemmas to use
for (i in seq_along(lemmas.to.use)) {
  l = lemmas.to.use[i]
  data.results$lemma[i] = l ## add lemma
  # Subset data to lemma with variants
  variant.data = data.to.use %>% filter(lemma == l)
  freq = nrow(variant.data)
  # Determine regular variant
  variants = as.vector(unique(variant.data$vvd))
  regular.variant = variants[sapply(variants, nchar) == max(sapply(variants, nchar))]
  irregular.variant = variants[sapply(variants, nchar) == min(sapply(variants, nchar))]
  data.results$regular[i] = regular.variant # add regular variant
  data.results$irregular[i] = irregular.variant # add irregular variant
  variant.df = variant.data %>% mutate(value = as.integer(vvd == regular.variant))
  # Set number of bins  
  n = nrow(variant.df)
  q = ceiling(log(n))
  data.results$q[i] = q # add number of bins
  # Bin the data by quantiles
  variant.write = adjust.absorption(bin.by.quantile(variant.df, q)) 
  variant.q = variant.write %>% mutate(lemma = rep(l,q))
  data.binned = rbind(data.binned, variant.q)
  #data.binned = rbind(data.binned, cbind(variant.q, rep(l,q)))
  # Output in format for tsinfer
  variant.in = paste('local/out/variants/',l,'.in',sep='')
  variant.out = paste('local/out/tsinfer/',l,'.out',sep='')
  variant.f =  paste('local/out/tsinfer/', 'f-', l,'.out',sep='')
  write.table(t(as.matrix(variant.write)), variant.in, 
               quote=F, col.names=F, row.names=F, sep='\t')
  # Run tsinfer by specifying the path to the compiled program
  cmdline = paste(path.to.tsinfer, variant.in, "-neut", ">", variant.out, sep=' ')
  cat(cmdline, "\n")
  system(cmdline)
  cmdline = paste(path.to.tsinfer, variant.in, "-f", ">", variant.f, sep=' ')
  system(cmdline)
  # Get results from tsinfer
  # TODO: use version of tsinfer for S0 only
  S0ALPHA.NEUT = system(paste('grep \"S0ALPHA\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
  data.results$ALPHA.NEUT[i] = extract.number(S0ALPHA.NEUT)
  S0ALPHA.F =  system(paste('grep \"S0ALPHA\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
  data.results$ALPHA.F[i] = extract.number(S0ALPHA.F)
  MLALPHA = system(paste('grep \"MLALPHA\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
  data.results$MLALPHA[i] = extract.number(MLALPHA)
  MLS = system(paste('grep \"MLS\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
  data.results$MLS[i] = extract.number(MLS)
  # Run the fit test and 
  variant.results = FIT.test(variant.q)
  data.results$FIT.p[i] = variant.results$FIT.p
  data.results$FIT.stat[i] = variant.results$FIT.stat
  data.results$Y.bar[i] = variant.results$Y.bar
  data.results$q[i] = variant.results$q
  data.results$mu[i] = variant.results$mu
  data.results$freq[i] = freq
  data.results$sigma[i] = variant.results$sigma
  data.results$W.stat[i] = variant.results$W.stat
  data.results$W.p[i] = variant.results$W.p
}

#   FIT.p: the p-value returned by the FIT test
#   FIT.stat: the test statistic of the FIT test
#   Y.bar: the average of the scaled fitness increments
#   n: number of bins
#   mu: average number of tokens per bin
#   sigma: standard deviation of number of tokens per bin
#   W.stat: test statistic of Shapiro-Wilk test for normality
#   W.p: p-value for Shapiro-Wilk test
#   ALPHA.NEUT: ML value of population size alpha where s = 0
#   ALPHA.F: ML value of population size alpha where s = 0 treating sample frequencies as exact
#   MLS: ML value of the selection coefficient s in the two-parameter model.

# Output results for analysis
data.results = tbl_df(as.data.frame(data.results))
data.write = data.results %>% arrange(FIT.p) %>%
              select(lemma, regular, irregular, FIT.p, FIT.stat, q,
                      mu, freq, ALPHA.NEUT, ALPHA.F, MLALPHA, MLS)
write.csv(data.write, file='local/out/past-tense-results.csv', row.names=F)
data.binned = tbl_df(as.data.frame(data.binned))
write.csv(data.binned, file = 'local/out/past-tense-binned.csv', row.names=F)
# Compare distribution of nominal p-values to uniform distribution
ks.test(data.results$FIT.p, punif, exact=T)
# Correlation of ALPHA.NEUT and ALPHA.F
cor.test(data.results$ALPHA.NEUT, data.results$ALPHA.F, method="spearman") 
# Correlation of ALPHA.F and freq for drifting variants
drift.freq = data.results %>% filter(FIT.p > .2)
cor.test(drift.freq$ALPHA.F, drift.freq$freq, method="spearman")
cor.test(drift.freq$ALPHA.F, drift.freq$freq)
cor.test(log(drift.freq$ALPHA.F), log(drift.freq$freq))

