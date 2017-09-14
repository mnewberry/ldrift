path.to.tsinfer = "~/src/tsinfer/tsinfer"

if(!file.exists(path.to.tsinfer)) {
  cat("Could not find tsinfer.  Please specify a path by editing this file.\n")
  quit() }


# Import libraries
print("Loading libraries and functions")
library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
#library(tsinfer)
source("helper.R")
# Load in the data
print("Loading and formatting data")
# Read in the data
data.DO.full = tbl_df(read.delim('local/data/do-PPCHE'))

# Filter out sentences with modals, other clause types, and non-sentential negation
print("Selecting data to use")
data.DO = data.DO.full %>% filter(do.supp != "MOD", ! clause %in% c("OTHER", "SUBQ", "SBJQ"), negation != "_") %>%
	mutate(context = paste(negation, clause, sep='.'), year = YoC) %>%
	filter(context != "AFF.DEC", context != "AFF.IMP") %>%
	filter(! Genre %in% c('philosophy', 'sermon', 'wycliffe_tyndale_bible', 'statute', 'king_james_bible', 'elizabeth_boethius'))

data.binned = c() # Store data for visualization of all time series
data.results = c() # Store results for analysis
data.logistic = c()

sub.contexts = c() 
for (ind in 1:100) {
  neg.dec.sub = subset(data.DO, context == "NEG.DEC")
  neg.dec.sub = neg.dec.sub[sample(seq(1,nrow(neg.dec.sub)),606),]
  sub.contexts[ind] = sprintf("%03d.NEG.DEC.SUB", ind)
  neg.dec.sub$context = sub.contexts[ind]
  data.DO = rbind(data.DO, neg.dec.sub) }

print("Running FIT test and inferring population size")
contexts.to.use = c("NEG.DEC", "NEG.IMP", "AFF.QUE",  "NEG.QUE", sub.contexts)
dir.create('local/out/variants', recursive=TRUE, mode="0777", showWarnings=FALSE)
dir.create('local/out/tsinfer', recursive=TRUE, mode="0777", showWarnings=FALSE)
# Loop over all lemmas to use
for (i in seq_along(contexts.to.use)) {
  cat(paste(contexts.to.use[i],"\n"))
  # Subset data to lemma with variants
  c = contexts.to.use[i]
  data.results$context[i] = c ## add lemma
  variant.data = data.DO %>% filter(context == c) 
  #min.year = min(variant.data[variant.data$do.supp == "DO", ]$year)
  #variant.data = variant.data %>% filter(year >= min.year)
  freq = nrow(variant.data)
  
  # Determine regular variant
  variant.df = variant.data %>% mutate(value = as.integer(do.supp == "DO")) 
  # Set number of bins  
  n = nrow(variant.df)
  q = ceiling(log(n))
  data.results$q[i] = q # add number of bins
  # Bin the data by quantiles
  variant.write = adjust.absorption(bin.by.quantile(variant.df, q))
  variant.q = variant.write %>% mutate(context = rep(c,q))
  data.binned = rbind(data.binned, variant.q)
  # Output in format for tsinfer
  variant.in = paste('local/out/variants/',c,'.in',sep='')
  # Try using tsinfer-R
  # print(as.data.frame(tsinfer(variant.write$year - min(variant.write$year), variant.write$value, variant.write$count, iffreq=T)))
  variant.out = paste('local/out/tsinfer/',c,'.out',sep='')
  write.table(t(as.matrix(variant.write)), variant.in, 
               quote=F, col.names=F, row.names=F, sep='\t')
  # Run tsinfer by specifying the path to the compiled program
  #cmdline = paste(path.to.tsinfer, variant.in, "-f", ">", variant.out, sep=' ')
  cmdline = paste(path.to.tsinfer, variant.in, "-f -start:0.0,500.0", ">", variant.out, sep=' ')
  #cmdline = paste(path.to.tsinfer, variant.in, "-f -plot:grid", ">", variant.out, sep=' ')
  system(cmdline)
  
  # Get results from tsinfer
  # TODO: use version of tsinfer for S0 only
  S0ALPHA.F =  system(paste('grep \"S0ALPHA\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
  data.results$ALPHA.F[i] = extract.number(S0ALPHA.F)
  MLALPHA = system(paste('grep \"MLALPHA\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
  data.results$MLALPHA[i] = extract.number(MLALPHA)
  MLS = system(paste('grep \"MLS\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
  data.results$MLS[i] = extract.number(MLS)
  MNLOGL = system(paste('grep \"MNLOGL\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
  data.results$MNLOGL[i] = extract.number(MNLOGL)
  # Run the fit test and 
  variant.results = FIT.test(variant.q, one.sided=T)
  data.results$FIT.p[i] = variant.results$FIT.p
  data.results$FIT.stat[i] = variant.results$FIT.stat
  data.results$Y.bar[i] = variant.results$Y.bar
  data.results$q[i] = variant.results$q
  data.results$mu[i] = variant.results$mu
  data.results$freq[i] = freq
  data.results$sigma[i] = variant.results$sigma
  data.results$W.stat[i] = variant.results$W.stat
  data.results$W.p[i] = variant.results$W.p
  
  # Get the initial state, beginning and end of the change
  x0 = variant.q$value[1]/variant.q$count[1] # initial starting state in context
  t0 = variant.q$year[1]
  tN = tail(variant.q$year, n=1)
  variant.logistic = data.frame(context = rep(c, tN - t0 + 1), year=c(t0:tN),p=logistic(x0,t0,tN+1,data.results$MLS[i]))
  data.logistic = rbind(data.logistic, variant.logistic) 
}

data.results = tbl_df(as.data.frame(data.results))
data.write = data.results %>% arrange(FIT.p) %>%
              select(context, FIT.p, FIT.stat, q,
                      mu, freq, ALPHA.F, MLALPHA, MLS, MNLOGL)
write.csv(data.write, file='local/out/do-support-results.csv', row.names=F)
data.binned = tbl_df(as.data.frame(data.binned))
write.csv(data.binned, file = 'local/out/do-support-binned.csv', row.names=F)

# Plots
#print("Plotting data")
#plot.data = data.DO %>% group_by(year, context) %>% 
#  summarize(n = n(),s = sum(do.supp == "DO")) %>% mutate(p = s / n)
#
#ggplot(aes(x = year, y = p, color = context, group = context), data = plot.data) +
#  geom_smooth(aes(weight = n), se = FALSE, method="glm", method.args = list(family="binomial")) +
#  facet_wrap(~ context) + 
#  coord_cartesian(ylim = c(-0.05,1.05)) +
#  ylab("Proportion do") +
#  geom_point(aes(size = n), alpha = 0.4) +
#  xlab("Year") +
#  scale_size_area("N", max_size = 20) +
#  theme(text = element_text(size=20), legend.position="none")
#ggsave('local/out/do-support-contexts-smooth-plot.pdf')
#
#ggplot(aes(x = year, y = p, color = context, group = context), data = plot.data) +
#  coord_cartesian(ylim = c(-0.05,1.05)) +
#  ylab("Proportion do") +
#  geom_point(aes(size = n), alpha = 0.4) +
#  xlab("Year") +
#  scale_size_area("N", max_size = 20) +
#  theme(text = element_text(size=20), legend.position="none") +
#  geom_line(data=data.logistic, aes(x=year, y=p, size=1.5)) +
#  facet_wrap(~ context) + 
#ggsave('local/out/do-support-contexts-plot.pdf')
#
#
#ggplot(data.binned, aes(x=year, y=value/count, color=as.factor(context))) + geom_line()
#ggsave(file='local/out/do-support-plot.pdf', ) 
#
