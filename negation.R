path.to.tsinfer = "~/src/tsinfer/tsinfer"

if(!file.exists(path.to.tsinfer)) {
  cat("Could not find tsinfer.  Please specify a path by editing this file.\n")
  quit() }

print("Loading libraries and functions")
# Import libraries
library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(reshape2, warn.conflicts = F)
source("helper.R")
# Load in the data
print("Loading and formatting data")
# Read in the data
data.PPCME2.full = tbl_df(read.csv('local/data/neg-data.csv', header=TRUE))
# Filter out documents and anomalous environments 
excluded.texts = c("CMORM","CMBOETH","CMNTEST","CMOTEST")
# CMBOETH : translation of Boethius' "Consolation of Philosophy", which is notably stilted
# CMORM   : Ormulum is very specific poetic format where adding an additional syllable for the meter is required
# CMOTEST, CMNTEST : Old and new testaments, which are known to carry archaisms longer than other texts
data.PPCME2 = data.PPCME2.full %>% 
            filter(finite != "-") %>% # Exclude non-finite clauses
            filter(clausetype != "imperative") %>% # Exclude imperatives
            filter(exclude != "only") %>% # Exclude focus constructions
            filter(exclude != "constituent") %>% # Exclude constituent negation
            filter(exclude != "contraction") %>% # Exclude contraction
            filter(exclude != "coordination") %>% # Exclude coordinated clauses
            filter(exclude != "concord") %>% # Exclude cases of negative concord
            filter(exclude != "X") %>% # Exclude corpus errors
            filter(! document %in% excluded.texts) %>% # Exclude texts
            select(year, document, stage)

# Plot the proportion of the three forms over time
plot.data =  data.PPCME2 %>% group_by(year, document) %>% 
    summarize(total=n(), ne=sum(stage==1, na.rm=TRUE)/total,
              not=sum(stage==3, na.rm=TRUE)/total,
              ne...not=sum(stage==2, na.rm=TRUE)/total)
plot.data = melt(plot.data, id=c("year", 'document', "total"))
ggplot(plot.data, aes(x = year, y = value, color = variable)) +
  geom_point(aes(size = total), alpha = 0.5) +
  geom_smooth(method="loess", se = F, size=4) + # aes(weight = total), span=.6,
  scale_x_continuous(name="Year", limits=c(1100, 1500)) +
  scale_y_continuous(name="Proportion of forms", breaks=seq(0,1,.25)) +   scale_size_area("N", max_size = 20) +
  theme(text = element_text(size=20)) + #  legend.position="none" # to remove legend 
  coord_cartesian(ylim = c(-.1,1.1)) #+ theme_hc()
ggsave('local/out/neg-year-lines.pdf', height=6, width=8)

# Get data from first transition
first.data = data.PPCME2 %>% group_by(year) %>% 
	mutate(value = as.integer(! stage==1), transition="first") %>% 
	select(year, document, value, transition)
# Get data from second transition
second.data = data.PPCME2 %>% group_by(year) %>%
	 mutate(value = as.integer(stage==3), transition="second") %>% 
	select(year, document, value, transition)
# Bind data frames for transitions together 
data.to.use = rbind(first.data, second.data)
print("Selected data to use")

print("Running FIT test and inferring population size")
dir.create('local/out/variants', recursive=TRUE, mode="0777", showWarnings=FALSE)
dir.create('local/out/tsinfer', recursive=TRUE, mode="0777", showWarnings=FALSE)


data.binned = c()
data.results = c()
data.logistic = c()

transitions = c("first", "second")
transitions.data = list(first.data, second.data)
# Loop over both transitions
for (i in seq_along(transitions)) {
	l = transitions[i]
	data.results$transition[i] = l ## add lemma
	# Pick out data set
	variant.data = transitions.data[[i]]
	freq = nrow(variant.data)
	# Set number of bins  
	n = nrow(variant.data)
	q = 6 # floor(log(n))
        bins = q
        #bins = c(1125,1200,1225,1340,1387,1396,1400,1440,1452,1500)
        #bins = c(1125,1200,1225,1275,1350,1388,1396,1400,1440,1450,1470,1500)
        #q = length(bins) - 1
	data.results$q[i] = q # add number of bins
	# Bin the data by quantiles
	variant.write = adjust.absorption(bin.by.quantile(variant.data, bins=bins)) 
	variant.q = variant.write %>% mutate(lemma = rep(l,q))
	data.binned = rbind(data.binned, variant.q)
	# Output in format for tsinfer
	variant.in = paste('local/out/variants/',l,'.in',sep='')
	variant.out = paste('local/out/tsinfer/',l,'.out',sep='')
	variant.f =  paste('local/out/tsinfer/', 'f-', l,'.out',sep='')
	write.table(t(as.matrix(variant.write)), variant.in, 
	               quote=F, col.names=F, row.names=F, sep='\t')
	# Run tsinfer by specifying the path to the compiled program
	cmdline = paste(path.to.tsinfer, variant.in, "-neut", ">", variant.out, sep=' ')
	system(cmdline)
	cmdline = paste(path.to.tsinfer, variant.in, "-f", ">", variant.f, sep=' ')
	system(cmdline)
	# Get results from tsinfer
	S0ALPHA.NEUT = system(paste('grep \"S0ALPHA\"', variant.out, sep=' '),intern=T, ignore.stderr = T)
	data.results$ALPHA.NEUT[i] = extract.number(S0ALPHA.NEUT)
	S0ALPHA.F =  system(paste('grep \"S0ALPHA\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
	data.results$ALPHA.F[i] = extract.number(S0ALPHA.F)
	MLALPHA = system(paste('grep \"MLALPHA\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
	data.results$MLALPHA[i] = extract.number(MLALPHA)
	MLS = system(paste('grep \"MLS\"', variant.f, sep=' '),intern=T, ignore.stderr = T)
	data.results$MLS[i] = extract.number(MLS)
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
  	variant.logistic = data.frame(transition = rep(l, (tN - t0 + 1)), year=c(t0:tN),p=logistic(x0,t0,tN+1,data.results$MLS[i]))
  	data.logistic = rbind(data.logistic, variant.logistic) 
}

plot.data = data.to.use  %>% group_by(year, document, transition) %>% 
    summarize(p = sum(value)/n(), total=n())
ggplot(aes(x = year, y = p), data = plot.data) +
  facet_wrap(~ transition) +
  geom_point(aes(size = total), alpha = 0.5, position = "identity") +
  geom_line(data=data.logistic, aes(x=year, y=p, size=2), color="blue") + 
  scale_x_continuous(name="Year", limits=c(1100, 1500)) +   
  scale_y_continuous(name="Proportion of forms", breaks=seq(0,1,.25)) +
  theme(text = element_text(size=15)) +  theme(legend.position="none", strip.text = element_blank()) +
  scale_size_area("N", max_size = 20) +  coord_cartesian(xlim = c(1090,1540)) + coord_cartesian(ylim = c(-.1,1.1))
ggsave('local/out/neg-transitions.pdf', width=8, height=4)


# Plot the data from the first transition
#first.plot.data = first.data %>% group_by(year, document) %>% 
#    summarize(p = sum(value)/n(), total=n())
#ggplot(aes(x = year, y = p), data = first.plot.data) +
#  geom_point(aes(size = total), alpha = 0.5, position = "identity") +
#  geom_smooth(method="glm", method.args=list(family="binomial"), se = F, size=4) + 
#  scale_x_continuous(name="Year", limits=c(1100, 1500)) +   
#  scale_y_continuous(name="Proportion of forms", breaks=seq(0,1,.25)) +
#  theme(text = element_text(size=20)) +  theme(legend.position="none") +
#  scale_size_area("N", max_size = 20) +  coord_cartesian(xlim = c(1090,1540)) + coord_cartesian(ylim = c(-.1,1.1))
#ggsave('local/out/first-transition.pdf', height=6, width=8) 
## Plot the data from the second transition
#second.plot.data = second.data %>% group_by(year, document) %>% 
#    summarize(p = sum(value)/n(), total=n())
#ggplot(aes(x = year, y = p), data = second.plot.data) +
#  geom_point(aes(size = total), alpha = 0.5, position = "identity") +
#  geom_smooth(method="glm", method.args=list(family="binomial"), se = F, size=4) + 
#  scale_x_continuous(name="Year", limits=c(1100, 1500)) +   
#  scale_y_continuous(name="Proportion of forms", breaks=seq(0,1,.25)) +
#  theme(text = element_text(size=20)) +  theme(legend.position="none") +
#  scale_size_area("N", max_size = 20) +  coord_cartesian(xlim = c(1090,1540)) + coord_cartesian(ylim = c(-.1,1.1))
#ggsave('local/out/second-transition.pdf', height=6, width=8) 
#
write.csv(data.PPCME2, file='local/out/neg-data.csv', row.names=F)
data.results = tbl_df(as.data.frame(data.results))
data.write = data.results %>% arrange(FIT.p) %>%
              select(transition, FIT.p, FIT.stat, q,
                      mu, freq, ALPHA.NEUT, ALPHA.F, MLALPHA, MLS)
write.csv(data.write, file='local/out/neg-results.csv', row.names=F)
data.binned = tbl_df(as.data.frame(data.binned))
write.csv(data.binned, file = 'local/out/neg-binned.csv', row.names=F)
data.logistic = tbl_df(data.logistic)
write.csv(data.logistic, file='local/out/neg-logistic.csv', row.names=F)

