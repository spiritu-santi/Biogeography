library(BAMMtools) # Assuming you have installed BAMMtools!
library(coda)
library(here)
t <- read.nexus("MCC_Master_Pruned.tre")
node <- getMRCA(t,c("Cyathea_austropallescens","Sphaeropteris_horrida"))
extract.clade(t,node=node)
write.tree(phytools::force.ultrametric(extract.clade(t,node=node)),"Cyatheaceae_ultra_mcc_master_pruned.tre")
write.tree(phytools::force.ultrametric(t),"Ultra_mcc_master_pruned.tre")
is.binary.tree(t)
setBAMMpriors(read.tree("all_globalSample/Ultra_mcc_master_pruned.tre"))


burn = 0.5
cual = "all_globalSample"
mcmcout <- read.csv(here(cual,"mcmc_out.txt"), header=T)
burnstart <- floor(burn * nrow(mcmcout))
postburn <- mcmcout[burnstart:nrow(mcmcout), ]
post_probs <- table(postburn$N_shifts) / nrow(postburn)
effectiveSize(postburn$N_shifts)
effectiveSize(postburn$logLik)
#plot(postburn$logLik,ylim=c(-1570,-1520))
#plot(mcmcout$N_shifts ~ mcmcout$generation,type="l")

tree <- read.tree(here(cual,"Ultra_mcc_master_pruned.tre"))
edata <- getEventData(tree, eventdata = here(cual,"event_data.txt"), burnin=burn)
summary(edata)
(bfmat <- computeBayesFactors(mcmcout, expectedNumberOfShifts=1, burnin=burn))
(css <- credibleShiftSet(edata, expectedNumberOfShifts = 1, threshold = 50, set.limit = 0.95,spex="netdiv"))

plot.credibleshiftset(css,spex="netdiv",plotmax=4,legend=T,breaksmethod = "jenks",logcolor = F,pal = MetBrewer::met.brewer("Hiroshige",direction=-1))

#q <- plot.bammdata(edata, breaksmethod='linear',spex="netdiv")
#ratesHistogram(q, plotBrks = TRUE, xlab = 'rates')
msc.set <- maximumShiftCredibility(edata, maximize='product')
msc.config <- subsetEventData(edata, index = msc.set$sampleindex)
plot.bammdata(msc.config, lwd=2,spex="netdiv",legend=T,breaksmethod = "jenks",logcolor = F,pal = MetBrewer::met.brewer("Hiroshige",direction=-1))
addBAMMshifts(msc.config)
#allrates <- getCladeRates(edata)
#plot(allrates$mu ~ allrates$lambda)
#getRateThroughTimeMatrix(edata) -> rtt
#plotRateThroughTime(rtt, ratetype="netdiv") #auto extinction netdiv
#cmat <- getCohortMatrix(edata)
#cohorts(cmat, edata)




library(rgdal)
readOGR("~/Desktop/Phanerozoic_EarthByte_Coastlines/reconstructed_0.00Ma.shp") %>% plot(.)


read.table("~/Desktop/offsets_reducido.tsv",sep="\t",header=TRUE) %>% as_tibble() %>% mutate(ID=1:nrow(.)) %>% ggplot(aes(x=bio1,y=off,color=sp)) + geom_point() + facet_wrap(~sp) + geom_text(aes(label=ID),color="black")
