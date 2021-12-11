# HERE BE DRAGONS
# This is research code I wrote a few years ago, in sore need of tidying up
#
# Results are part of the paper:
# "Modelling the Simultaneous Encoding/ Serial Experience theory of the Perceptual Moment: a Blink of Meta-Experience"
#

library(entropy)
library(ggplot2)
library(gplots)
library(lme4)
library(R2jags)
library(reticulate)
library(SDMTools)
library(MuMIn)
library(car)


# Finding relative paths in R is a pain, change this to your own
datafull = read.csv(file="G:/D/HanBehavioral/BehavioralExpCSV.csv",head=TRUE,sep=",")
datacsv = datafull[datafull$ResponseCode!=54&datafull$ResponseCode!=56&datafull$ResponseCode!=57&datafull$Code!=9,]
acc.mu= mean(as.integer((datacsv$ResponseCode==51|datacsv$ResponseCode==52)))
acc.sigma = sd(as.integer((datacsv$ResponseCode==51|datacsv$ResponseCode==52)))
acc.zscore = vector(length=6, mode='numeric')
acc.acc = vector(length=6, mode='numeric')
acc.lag = c(1,2,3,4,6,8)
for(i in 1:6)
{
  lagdata = datacsv[datacsv$Code==acc.lag[i],]
  lagmean = mean(as.integer((lagdata$ResponseCode==51|lagdata$ResponseCode==52)))
  acc.acc[i] = lagmean
  #acc.zscore[i] = (lagmean - acc.mu)/acc.sigma
}



#Data matrix. Lag, visibility, Stimulus, Response
lag_count = 6
vis_count = 2
sub_count = 18

entropyNSB <- function(bincounts)
{
  nsb <- import("nsbentropy")
  H = nsb$entropy_nsb(bincounts)
  return(as.numeric(as.character(H)))
}

entropyJIAO <- function(bincounts)
{
  jiao <- import("jiaoentropy")
  if(sum(bincounts) == 0)
  {
    return(0)
  }
  else
  {
    H = jiao$entropyJIAO(bincounts)
    return(as.numeric(as.character(H)))    
  }
}

lag_vis_sub = lag_count*vis_count*sub_count
lag_vis = lag_count*vis_count
StimRespVisMatrix = array(data = 0,dim=c(sub_count,lag_count,vis_count,21,21))
LagVect = c("Lag1","Lag2","Lag3","Lag4","Lag6","Lag8")
Names = c(
  "alex", "alext", "alice", "ana", "annav", "anthony",
  "bex", "florence", "jack", "kinga", "laurai", "louise",
  "lowri", "rebeccaw", "sarah", "sophie", "theodora", "urte")

vis_counter = vector(length=6, mode='numeric')
vis_score = vector(length=6, mode='numeric')
for(k in 1:18)
{
  for(j in 1:4)
  {
    #Pull the data from one of hannahs files
    data = read.delim2(
      paste(
        "G:/D/HanBehavioral/Behavioural Expt Data/Behavioural Expt Data/bkall/",
        Names[k],
        j,
        "-AB_EEG3.log",
        sep = ""
      ),
      header=FALSE,
      sep="\t"
      )
    #Reduce the data to the bit we actually need
    reducedData = data[grepl("Response",data$V3,fixed=TRUE)|grepl("Code:",data$V4,fixed=TRUE)|grepl("T2",data$V4,fixed=TRUE)|grepl("(?<!.)5.",data$V4,perl=TRUE),][4]
    reducedData[nrow(reducedData)+1,] = "Code:9"
    if(j ==1){reducedData = data.frame(reducedData[-c((1:(10*6))),])}#Exclude practice trials
    else{reducedData = data.frame(reducedData[-c((1:(2*6))),])}
    Code = 0
    Vis = 1
    Stim = 1
    Resp = 1
    T1 = 0
    for(i in 1:dim(reducedData)[1])
    {
      #print(reducedData[i,])
      if(grepl("Code:9",reducedData[i,],fixed=TRUE))
      {
        if(Code == 1) {
          StimRespVisMatrix[k,1,Vis,Stim, Resp] = StimRespVisMatrix[k,1,Vis,Stim, Resp] + 1
          vis_counter[1] = vis_counter[1] + 1
          vis_score[1] = vis_score[1] + ((TrueVis-1)/5)
        }
        if(Code == 2) {
          StimRespVisMatrix[k,2,Vis,Stim, Resp] = StimRespVisMatrix[k,2,Vis,Stim, Resp] + 1
          vis_counter[2] = vis_counter[2] + 1
          vis_score[2] = vis_score[2] + ((TrueVis-1)/5)
        }
        if(Code == 3) {
          StimRespVisMatrix[k,3,Vis,Stim, Resp] = StimRespVisMatrix[k,3,Vis,Stim, Resp] + 1
          vis_counter[3] = vis_counter[3] + 1
          vis_score[3] = vis_score[3] + ((TrueVis-1)/5)
        }
        if(Code == 4) {
          StimRespVisMatrix[k,4,Vis,Stim, Resp] = StimRespVisMatrix[k,4,Vis,Stim, Resp] + 1
          vis_counter[4] = vis_counter[4] + 1
          vis_score[4] = vis_score[4] + ((TrueVis-1)/5)
        }
        if(Code == 6) {
          StimRespVisMatrix[k,5,Vis,Stim, Resp] = StimRespVisMatrix[k,5,Vis,Stim, Resp] + 1
          vis_counter[5] = vis_counter[5] + 1
          vis_score[5] = vis_score[5] + ((TrueVis-1)/5)
        }
        if(Code == 8) {
          StimRespVisMatrix[k,6,Vis,Stim, Resp] = StimRespVisMatrix[k,6,Vis,Stim, Resp] + 1
          vis_counter[6] = vis_counter[6] + 1
          vis_score[6] = vis_score[6] + ((TrueVis-1)/5)
        }
        Code = 0
        T1 = 0
      }
      else if(grepl("Code:",reducedData[i,],fixed=TRUE))
      {
        if(Code == 1) {
          StimRespVisMatrix[k,1,Vis,Stim, Resp] = StimRespVisMatrix[k,1,Vis,Stim, Resp] + 1
          vis_counter[1] = vis_counter[1] + 1
          vis_score[1] = vis_score[1] + ((TrueVis-1)/5)
        }
        if(Code == 2) {
          StimRespVisMatrix[k,2,Vis,Stim, Resp] = StimRespVisMatrix[k,2,Vis,Stim, Resp] + 1
          vis_counter[2] = vis_counter[2] + 1
          vis_score[2] = vis_score[2] + ((TrueVis-1)/5)
        }
        if(Code == 3) {
          StimRespVisMatrix[k,3,Vis,Stim, Resp] = StimRespVisMatrix[k,3,Vis,Stim, Resp] + 1
          vis_counter[3] = vis_counter[3] + 1
          vis_score[3] = vis_score[3] + ((TrueVis-1)/5)
        }
        if(Code == 4) {
          StimRespVisMatrix[k,4,Vis,Stim, Resp] = StimRespVisMatrix[k,4,Vis,Stim, Resp] + 1
          vis_counter[4] = vis_counter[4] + 1
          vis_score[4] = vis_score[4] + ((TrueVis-1)/5)
        }
        if(Code == 6) {
          StimRespVisMatrix[k,5,Vis,Stim, Resp] = StimRespVisMatrix[k,5,Vis,Stim, Resp] + 1
          vis_counter[5] = vis_counter[5] + 1
          vis_score[5] = vis_score[5] + ((TrueVis-1)/5)
        }
        if(Code == 8) {
          StimRespVisMatrix[k,6,Vis,Stim, Resp] = StimRespVisMatrix[k,6,Vis,Stim, Resp] + 1
          vis_counter[6] = vis_counter[6] + 1
          vis_score[6] = vis_score[6] + ((TrueVis-1)/5)
        }
        Code = as.integer(unlist(strsplit(as.character(reducedData[i,]), split=":", fixed=TRUE))[2])
        T1 = 0
      }
      else if(grepl("T2",reducedData[i,],fixed=TRUE))
      {
        Stim = as.integer(unlist(strsplit(as.character(reducedData[i,]), split=":", fixed=TRUE))[2])
      }
      else if(as.integer(as.character(reducedData[i,])) > 150)
      {
        Vis = as.integer(as.character(reducedData[i,])) - 150
        TrueVis = Vis
        if(Vis == 1) {Vis = 1}
        if(Vis == 2) {Vis = 1}
        if(Vis == 3) {Vis = 1}
        if(Vis == 4) {Vis = 2}
        if(Vis == 5) {Vis = 2}
        if(Vis == 6) {Vis = 2}
      }
      else if((T1 > 0)&(as.integer(as.character(reducedData[i,])) > 99))
      {
        Resp = as.integer(as.character(reducedData[i,])) - 100
        if(Resp > 21) {Code = 0}
      }
      else if((as.integer(as.character(reducedData[i,])) > 50)&(as.integer(as.character(reducedData[i,])) < 58))
      {
        #NOTHING YET, JUST USEFUL INFO TO HAVE
        #print(as.integer(as.character(reducedData[i,])))
        if(!(as.integer(as.character(reducedData[i,])) %in% c(51, 52, 53, 55))){Code = 0}
        if((as.integer(as.character(reducedData[i,])) %in% c(52,55))){
          Resp = T1
          if(Resp > 21) {Code = 0}
          }
        #print(Resp)
      }
      else
      {
        T1 = (as.integer(as.character(reducedData[i,])) - 100)
      }
    }
  }
}





acc.acc = apply(apply(StimRespVisMatrix, c(2, 4, 5), sum), 1, function(x) sum(diag(x))/sum(x))
acc.vis = vis_score/vis_counter
acc.zscore = (acc.acc - mean(acc.acc))/sd(acc.acc)
acc.acc.highvis = apply(apply(StimRespVisMatrix, c(2, 3, 4, 5), sum)[,2,,], 1, function(x) sum(diag(x))/sum(x))
acc.acc.lowvis = apply(apply(StimRespVisMatrix, c(2, 3, 4, 5), sum)[,1,,], 1, function(x) sum(diag(x))/sum(x))
acc.vis.zscore = (acc.vis - mean(acc.vis))/sd(acc.vis)




MIMatrix = vector(length=lag_vis_sub, mode='numeric')
MILag = vector(length=lag_vis_sub, mode='character')
MIVis = vector(length=lag_vis_sub, mode='numeric')
MISub = vector(length=lag_vis_sub, mode='character')
MICount = vector(length=lag_vis_sub, mode='numeric')
MITrial = vector(length=lag_vis_sub, mode='character')
NoSubMIMatrix = vector(length=lag_vis, mode='numeric')
NoSubMILag = vector(length=lag_vis, mode='character')
NoSubMIVis = vector(length=lag_vis, mode='numeric')
NoSubMICount = vector(length=lag_vis, mode='numeric')
for(i in 1:lag_count)
{
  for(j in 1:vis_count)
  {
    NoSubStimResp = matrix(0, nrow = 21, ncol = 21)
    for(k in 1:sub_count)
    {
      StimResp = StimRespVisMatrix[k,i,j,,]#apply(StimRespVisMatrix[-k,i,j,,], c(2,3), sum)#
      NoSubStimResp = NoSubStimResp + StimResp
      H1 = entropyNSB(colSums(StimResp))
      H2 = entropyNSB(rowSums(StimResp))
      H12 = entropyNSB(as.vector(StimResp))
      MIMatrix[((k-1)*lag_vis)+((i-1)*vis_count)+j] = (H1 + H2 - H12)
      MILag[((k-1)*lag_vis)+((i-1)*vis_count)+j] = i
      MIVis[((k-1)*lag_vis)+((i-1)*vis_count)+j] = j
      MISub[((k-1)*lag_vis)+((i-1)*vis_count)+j] = k
      if(sum(StimResp) <= 0)
      {
        MICount[((k-1)*lag_vis)+((i-1)*vis_count)+j] = 0
        MIMatrix[((k-1)*lag_vis)+((i-1)*vis_count)+j] = NA
      }
      else
      {
        MICount[((k-1)*lag_vis)+((i-1)*vis_count)+j] = 1/sum(StimResp)
      }
    }
    H1 = entropyNSB(colSums(NoSubStimResp))
    H2 = entropyNSB(rowSums(NoSubStimResp))
    H12 = entropyNSB(as.vector(NoSubStimResp))
    NoSubMIMatrix[((i-1)*vis_count)+j] = (H1 + H2 - H12)
    NoSubMILag[((i-1)*vis_count)+j] = i
    NoSubMIVis[((i-1)*vis_count)+j] = j
    if(sum(StimResp) == 0)
    {
      NoSubMICount[((i-1)*vis_count)+j] = 0
    }
    else
    {
      NoSubMICount[((i-1)*vis_count)+j] = sum(NoSubStimResp)
    }
  }
}

#Mash the 1,6,8 and 2,3,4 bins together into the mixet effect
#and try with t-tests
#MIMatrix[is.na(MIMatrix)] <- 0
res <- data.frame(MIMatrix, as.factor(MILag), as.factor(MIVis), as.factor(MISub), MICount)
colnames(res) <- c("MI", "Lag", "Vis", "Sub", "Count")
# res[res$Lag==6,]$Lag = 5
# res[res$Lag==1,]$Lag = 1
# res[res$Lag==3,]$Lag = 2
# res[res$Lag==4,]$Lag = 2
# res <- droplevels(res[res$Lag==2|res$Lag==5,])

contrasts(res$Lag) = rbind(diag(5)[1:5,],c(-0,-0,-0,-0,-0))#rbind(diag(5), c(-1,-1,-1,-1,-1))
#contrasts(res$Vis) = contr.sum(6)
#contrasts(res$Vis) = rbind(diag(5)[1:2,],c(-1,-1,-1,-1,-1), diag(5)[3:5,])#rbind(diag(5), c(-1,-1,-1,-1,-1))
#contrasts(res$Vis) = rbind(c(-0,-0,-0,-0,-0),diag(5)[1:5,])
#contrasts(res$Vis) = rbind(c(1,0),c(0,0), c(0,1))#rbind(diag(5), c(-1,-1,-1,-1,-1))
# contrasts(res$Lag) = rbind(1,0)
contrasts(res$Vis) = rbind(1,0)
V = res$Vis
L = res$Lag
#V = dummy(res$Vis, c("1", "2", "3", "4", "5", "6"))
#L = dummy(res$Lag, c("1", "2", "3", "4", "5"))

res.lag.model = lmer(MI~ L + Count + (1|Sub), res, REML=FALSE)
res.lag.null = lmer(MI~ Count + (1|Sub), res, REML=FALSE)
res.lag.anova = anova(res.lag.null, res.lag.model)

res.vis.model = lmer(MI~ V + Count + (1|Sub), res, REML=FALSE)
res.vis.null = lmer(MI~ Count + (1|Sub), res, REML=FALSE)
res.vis.anova = anova(res.vis.null, res.vis.model)

res.int.model = lmer(MI~ V+L+V:L + Count + (1|Sub), res, REML=FALSE)
res.int.null = lmer(MI~ V+L + Count + (1|Sub), res, REML=FALSE)
res.int.anova = anova(res.int.null, res.int.model)

##No count

# res.lag.model = lmer(MI~ L + (1|Sub), res, REML=FALSE)
# res.lag.null = lmer(MI~  (1|Sub), res, REML=FALSE)
# res.lag.anova = anova(res.lag.null, res.lag.model)
# 
# res.vis.model = lmer(MI~ V + (1|Sub), res, REML=FALSE)
# res.vis.null = lmer(MI~  (1|Sub), res, REML=FALSE)
# res.vis.anova = anova(res.vis.null, res.vis.model)
# 
# res.int.model = lmer(MI~ V+L+V:L + (1|Sub), res, REML=FALSE)
# res.int.null = lmer(MI~ V+L + (1|Sub), res, REML=FALSE)
# res.int.anova = anova(res.int.null, res.int.model)

toDataFrame <- function(matrix)
{
  size = dim(matrix)
  dep = as.vector(matrix)
  row = rep(colnames(matrix), length.out=size[1]*size[2], each=size[1])
  col = rep(rownames(matrix), length.out=size[2]*size[1])
  return(data.frame(dep, row, col))
}

Model <- model.matrix(~V+L+V:L, res)

if(vis_count == 6)
{
  res.lag.beta = c(res.lag.model@beta[2:6], 0)
  res.lag.vis = rep(1, length.out=6)
  res.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.lag <- data.frame(res.lag.lag, res.lag.vis, res.lag.beta)
  names(postres.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/LagPlot6Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.vis.beta = c(res.vis.model@beta[2:3],0, res.vis.model@beta[4:6])
  res.vis.vis = rep(c(1,2,3,4,5,6), length.out=6)
  res.vis.lag = rep(1, length.out=6)
  postres.vis <- data.frame(res.vis.lag, res.vis.vis, res.vis.beta)
  names(postres.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(0, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/VisPlot6Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.beta = res.int.model@beta[13:37]
  res.int.vis = rep(c(1,2,4,5,6), length.out=25)
  res.int.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6"), length.out=25, each=5)
  postres.int <- data.frame(res.int.lag, res.int.vis, res.int.beta)
  names(postres.int) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int, aes(x = vis, y = beta, colour = lag, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 1), breaks=seq(-1, 1, 0.1)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(0, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/InteractionPlot6Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.lag.beta = c(res.int.model@beta[7:11], 0)
  res.int.lag.vis = rep(1, length.out=6)
  res.int.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.int.lag <- data.frame(res.int.lag.lag, res.int.lag.vis, res.int.lag.beta)
  names(postres.int.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Lag(Int)Plot6Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.vis.beta = c(res.int.model@beta[2:3],0, res.int.model@beta[4:6])
  res.int.vis.vis = rep(c(1,2,3,4,5,6), length.out=6)
  res.int.vis.lag = rep(1, length.out=6)
  postres.int.vis <- data.frame(res.int.vis.lag, res.int.vis.vis, res.int.vis.beta)
  names(postres.int.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(0, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Vis(Int)Plot6Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  nosubres <- data.frame(NoSubMIMatrix, as.factor(NoSubMILag), NoSubMIVis, NoSubMICount)
  colnames(nosubres) <- c("MI", "Lag", "Vis", "Count")
  
  nosubgrandavg.MI = NoSubMIMatrix
  nosubgrandavg.vis = rep(c(1,2,3,4,5,6), length.out=36)
  nosubgrandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=36, each=6)
  
  nosubfullgrandavg.MI = vector(length=6, mode='numeric')
  for(i in 1:6)
  {
    #m = nosubres$MI[((i-1)*6)+1:i*6]
    #w = nosubres$Count[((i-1)*6)+1:i*6]
    StimResp = apply(StimRespVisMatrix[,i,,,], c(3,4), sum)
    H1 = entropyNSB(colSums(StimResp))
    H2 = entropyNSB(rowSums(StimResp))
    H12 = entropyNSB(as.vector(StimResp))
    nosubfullgrandavg.MI[i] = H1 + H2 - H12#weighted.mean(m,w, na.rm=TRUE)
  }
  nosubfullgrandavg.lag = c(1,2,3,4,5,6)
  
  
  
  grandavg.MI = sapply(split(res$MI, list(res$Lag, res$Vis, drop=TRUE)),function(x) mean(x, na.rm=TRUE))
  grandavg.vis = rep(c(1,2,3,4,5,6), length.out=36)
  grandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=36, each=6)
  
  fullgrandavg.MI = sapply(split(res$MI, res$Lag),function(x) mean(x, na.rm=TRUE))
  fullgrandavg.vis = c(1,2,3,4,5,6)
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMI6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMIRegress6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  Acc <- acc.acc
  Vis <- acc.vis
  # MI <- nosubfullgrandavg.MI/10
  MI <- nosubfullgrandavg.MI/10
  Out <- c(Acc, Vis, MI)
  Lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=18)
  measure <- rep(c("Accuracy", "Visibility", "Mutual information"), length.out=18, each=6)
  
  miaccvis.res <- data.frame(Out, Lag, measure)
  res_plot = ggplot(data=miaccvis.res, aes(x=Lag, y=Out, colour=measure, group=measure)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Measure", labels=c("Accuracy", "Mutual information/10","Visibility"), values = c("#e91e63", "#ff5722", "#ff9800")) +
    scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.05)) +
    #scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/MIAccVis6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 1.1), breaks=seq(0, 1.1, 0.1)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMI6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 1.1), breaks=seq(0, 1.1, 0.1)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMIRegress6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.vis)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.vis, y=fullgrandavg.MI)) +
  #   geom_line(size = 1.5) +
  #   #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
  #   geom_point(size=2.5) + 
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  # 
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.vis)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.vis, y=fullgrandavg.MI)) +
  #   #geom_line(size = 1.5) +
  #   geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
  #   #geom_point(size=2.5) + 
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress6Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
}
if(vis_count == 3)
{
  res.lag.beta = c(res.lag.model@beta[2:6], 0)
  res.lag.vis = rep(1, length.out=6)
  res.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.lag <- data.frame(res.lag.lag, res.lag.vis, res.lag.beta)
  names(postres.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/LagPlot3Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.vis.beta = c(res.vis.model@beta[2],0, res.vis.model@beta[3])
  res.vis.vis = rep(c(1,2,3), length.out=3)
  res.vis.lag = rep(1, length.out=3)
  postres.vis <- data.frame(res.vis.lag, res.vis.vis, res.vis.beta)
  names(postres.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 0.5), breaks=seq(-1, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(0, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/VisPlot3Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.beta = res.int.model@beta[10:19]
  res.int.vis = rep(c(1,3), length.out=10)
  res.int.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6"), length.out=10, each=2)
  postres.int <- data.frame(res.int.lag, res.int.vis, res.int.beta)
  names(postres.int) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int, aes(x = vis, y = beta, colour = lag, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 1), breaks=seq(-1, 1, 0.1)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(0, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/InteractionPlot3Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.lag.beta = c(res.int.model@beta[4:8], 0)
  res.int.lag.vis = rep(1, length.out=6)
  res.int.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.int.lag <- data.frame(res.int.lag.lag, res.int.lag.vis, res.int.lag.beta)
  names(postres.int.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Lag(Int)Plot3Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.vis.beta = c(res.int.model@beta[2],0, res.int.model@beta[3])
  res.int.vis.vis = rep(c(1,2,3), length.out=3)
  res.int.vis.lag = rep(1, length.out=3)
  postres.int.vis <- data.frame(res.int.vis.lag, res.int.vis.vis, res.int.vis.beta)
  names(postres.int.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 0.5), breaks=seq(-1, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(0, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Vis(Int)Plot3Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  nosubres <- data.frame(NoSubMIMatrix, as.factor(NoSubMILag), NoSubMIVis, NoSubMICount)
  colnames(nosubres) <- c("MI", "Lag", "Vis", "Count")
  
  nosubgrandavg.MI = NoSubMIMatrix
  nosubgrandavg.vis = rep(c(1,2,3), length.out=18)
  nosubgrandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=18, each=3)
  
  nosubfullgrandavg.MI = vector(length=6, mode='numeric')
  for(i in 1:6)
  {
    m = nosubres$MI[((i-1)*vis_count)+1:i*vis_count]
    w = nosubres$Count[((i-1)*vis_count)+1:i*vis_count]
    nosubfullgrandavg.MI[i] = weighted.mean(m,w, na.rm=TRUE)
  }
  nosubfullgrandavg.lag = c(1,2,3,4,5,6)
  
  grandavg.MI = sapply(split(res$MI, list(res$Lag, res$Vis, drop=TRUE)),function(x) mean(x, na.rm=TRUE))
  grandavg.vis = rep(c(1,2,3), length.out=18)
  grandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=18, each=3)
  
  fullgrandavg.MI = sapply(split(res$MI, res$Lag),function(x) mean(x, na.rm=TRUE))
  fullgrandavg.lag = c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8")
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(1, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMI3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(1, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMIRegress3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  Acc <- c(0.7010050,0.6233184,0.6129032,0.6186253,0.7660944,0.7413793)
  Vis <- c(0.2949749,0.2946188,0.3105376,0.3476718,0.5060086,0.5064655)
  # MI <- nosubfullgrandavg.MI/10
  MI <- nosubfullgrandavg.MI/10
  Out <- c(Acc, Vis, MI)
  Lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=18)
  measure <- rep(c("Accuracy", "Visibility", "Mutual information"), length.out=18, each=6)
  
  miaccvis.res <- data.frame(Out, Lag, measure)
  res_plot = ggplot(data=miaccvis.res, aes(x=Lag, y=Out, colour=measure, group=measure)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Measure", labels=c("Accuracy", "Mutual information/10","Visibility"), values = c("#e91e63", "#ff5722", "#ff9800")) +
    scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.05)) +
    #scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/MIAccVis3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 1.5), breaks=seq(0, 1.5, 0.1)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(1, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMI3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 1.5), breaks=seq(0, 1.5, 0.1)) +
    scale_x_continuous(limits = c(1, 3), breaks=seq(1, 3, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMIRegress3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.lag)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.lag, y=fullgrandavg.MI)) +
  #   geom_line(size = 1.5) +
  #   #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
  #   geom_point(size=2.5) + 
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   #scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  # 
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.vis)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.vis, y=fullgrandavg.MI)) +
  #   #geom_line(size = 1.5) +
  #   geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
  #   #geom_point(size=2.5) + 
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress3Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  # 
}
if(vis_count == 2)
{
  res.lag.beta = c(res.lag.model@beta[2:6], 0)
  res.lag.vis = rep(1, length.out=6)
  res.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.lag <- data.frame(res.lag.lag, res.lag.vis, res.lag.beta)
  names(postres.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-0.5, 0.5), breaks=seq(-0.5, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/LagPlot2Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.vis.beta = c(res.vis.model@beta[2],0)
  res.vis.vis = rep(c(1,2), length.out=2)
  res.vis.lag = rep(1, length.out=2)
  postres.vis <- data.frame(res.vis.lag, res.vis.vis, res.vis.beta)
  names(postres.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1.5, 0.5), breaks=seq(-1.5, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(0, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/VisPlot2Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.beta = res.int.model@beta[9:13]
  res.int.vis = rep(c(1), length.out=5)
  res.int.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6"), length.out=5, each=1)
  postres.int <- data.frame(res.int.lag, res.int.vis, res.int.beta)
  names(postres.int) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int, aes(x = vis, y = beta, colour = lag, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 1.5), breaks=seq(-1, 1.5, 0.1)) +
    scale_x_continuous(limits = c(1, 1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/InteractionPlot2Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.lag.beta = c(res.int.model@beta[3:7], 0)
  res.int.lag.vis = rep(1, length.out=6)
  res.int.lag.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=6)
  postres.int.lag <- data.frame(res.int.lag.lag, res.int.lag.vis, res.int.lag.beta)
  names(postres.int.lag) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.lag, aes(x = lag, y = beta, group = vis)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1, 0.5), breaks=seq(-1, 0.5, 0.1)) +
    #scale_x_continuous(limits = c(-0.1, 1.1), breaks=seq(0, 1, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Lag(Int)Plot2Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  res.int.vis.beta = c(res.int.model@beta[2],0)
  res.int.vis.vis = rep(c(1,2), length.out=2)
  res.int.vis.lag = rep(1, length.out=2)
  postres.int.vis <- data.frame(res.int.vis.lag, res.int.vis.vis, res.int.vis.beta)
  names(postres.int.vis) <- c("lag", "vis", "beta")
  postres_plot = ggplot(data=postres.int.vis, aes(x = vis, y = beta, group = lag)) +
    geom_path(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size = 1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-1.5, 0.5), breaks=seq(-1.5, 0.5, 0.1)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(0, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Beta") +
    theme_bw()
  postres_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/Vis(Int)Plot2Bin.png", plot=postres_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  nosubres <- data.frame(NoSubMIMatrix, as.factor(NoSubMILag), NoSubMIVis, NoSubMICount)
  colnames(nosubres) <- c("MI", "Lag", "Vis", "Count")
  
  nosubgrandavg.MI = NoSubMIMatrix
  nosubgrandavg.vis = rep(c(1,2), length.out=12)
  nosubgrandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=12, each=2)
  
  nosubfullgrandavg.MI = vector(length=6, mode='numeric')
  nosubfullgrandavg.zscore = vector(length=6, mode='numeric')
  w = rep(1,12)
  nosubgrandmean = wt.mean(nosubres$MI, w)
  nosubgrandsd = wt.sd(nosubres$MI, w)
  for(i in 1:6)
  {
    m = nosubres$MI[(((i-1)*vis_count)+1):(i*vis_count)]
    w = nosubres$Count[(((i-1)*vis_count)+1):(i*vis_count)]
    w = c(1,1)
    StimResp = apply(StimRespVisMatrix[,i,,,], c(3,4), sum)
    H1 = entropyNSB(colSums(StimResp))
    H2 = entropyNSB(rowSums(StimResp))
    H12 = entropyNSB(as.vector(StimResp))
    nosubfullgrandavg.MI[i] = H1 + H2 - H12#weighted.mean(m,w, na.rm=TRUE)
  }
  
  nosubfullgrandavg.lag = c(1,2,3,4,6,8)
  nosubfullgrandavg.zscore = (nosubfullgrandavg.MI - mean(nosubfullgrandavg.MI))/sd(nosubfullgrandavg.MI)
  
  nosubfullgrandavgvis.MI = vector(length=2, mode='numeric')
  for(i in 1:2)
  {
    m = nosubres$MI[seq(i,10+i,2)]
    w = nosubres$Count[seq(i,10+i,2)]
    nosubfullgrandavgvis.MI[i] = weighted.mean(m,w, na.rm=TRUE)
  }
  nosubfullgrandavgvis.vis = c(1,2)

  grandavg.MI = sapply(split(res$MI, list(res$Lag, res$Vis, drop=TRUE)),function(x) mean(x, na.rm=TRUE))
  grandavg.vis = rep(c(1,2), length.out=12, each = 2)
  grandavg.lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=12)
  
  fullgrandavg.MI = sapply(split(res$MI, res$Lag),function(x) mean(x, na.rm=TRUE))
  fullgrandavg.lag = c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8")
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(1, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMI2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubgrandavg.res <- data.frame(nosubgrandavg.MI, nosubgrandavg.lag, nosubgrandavg.vis)
  res_plot = ggplot(data=nosubgrandavg.res, aes(x=nosubgrandavg.vis, y=nosubgrandavg.MI, colour=nosubgrandavg.lag, group=nosubgrandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(1, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubGrandAvgMIRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  
  nosubfullgrandavg.lag
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape = 15) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    ggtitle("Main Effect of Lag") + 
    theme_bw() +
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      plot.title = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
 
   
  nosubfullgrandavg.res <- data.frame(nosubfullgrandavg.MI, nosubfullgrandavg.lag)
  res_plot = ggplot(data=nosubfullgrandavg.res, aes(x=nosubfullgrandavg.lag, y=nosubfullgrandavg.MI)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0.5, 3), breaks=seq(-0.5, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  nosubfullgrandavgvis.res <- data.frame(nosubfullgrandavgvis.MI, nosubfullgrandavgvis.vis)
  res_plot = ggplot(data=nosubfullgrandavgvis.res, aes(x=nosubfullgrandavgvis.vis, y=nosubfullgrandavgvis.MI)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape=15) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0, 3), breaks=seq(-0, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(1, 2, 1), labels=c("Low", "High")) +
    xlab("Visibility") +
    ylab("Mutual information") +
    ggtitle("Main effect of Visibility") + 
    theme_bw() + 
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      plot.title = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot  
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgVis2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  Acc <- acc.acc
  Vis <- acc.vis
  # MI <- nosubfullgrandavg.MI/10
  MI <- nosubfullgrandavg.MI/10
  Out <- c(Acc, Vis, MI)
  Lag = rep(c("Lag1", "Lag2", "Lag3", "Lag4", "Lag6", "Lag8"), length.out=18)
  measure <- rep(c("Accuracy", "Visibility", "Mutual information"), length.out=18, each=6)
  
  miaccvis.res <- data.frame(Out, Lag, measure)
  res_plot = ggplot(data=miaccvis.res, aes(x=Lag, y=Out, colour=measure, group=measure)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Measure", labels=c("Accuracy", "Mutual information/10","Visibility"), values = c("#e91e63", "#ff5722", "#ff9800")) +
    scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.05)) +
    #scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/MIAccVis2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 2), breaks=seq(0, 2, 0.1)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(1, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMI2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  grandavg.res <- data.frame(grandavg.MI, grandavg.lag, grandavg.vis)
  res_plot = ggplot(data=grandavg.res, aes(x=grandavg.vis, y=grandavg.MI, colour=grandavg.lag, group=grandavg.lag)) +
    #geom_line(size = 1.5) +
    geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    #geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0, 2), breaks=seq(0, 2, 0.1)) +
    scale_x_continuous(limits = c(1, 2), breaks=seq(1, 2, 1)) +
    xlab("Visibility Bin") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/GrandAvgMIRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  
  nosubregress.MI = nosubgrandavg.res$nosubgrandavg.MI[nosubgrandavg.res$nosubgrandavg.vis==2]-nosubgrandavg.res$nosubgrandavg.MI[nosubgrandavg.res$nosubgrandavg.vis==1]
  nosubregress.lag = c(1,2,3,4,6,8)
  nosubregress.res <- data.frame(nosubregress.MI, nosubregress.lag)
  res_plot = ggplot(data=nosubregress.res, aes(x=nosubregress.lag, y=nosubregress.MI)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape=15) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-1, 3), breaks=seq(-1, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Information (Bits)") +
    ggtitle("Meta-Experience") +
    theme_bw() + 
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      plot.title = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubMetaCogRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
  regress.MI = grandavg.res$grandavg.MI[grandavg.res$grandavg.vis==2]-grandavg.res$grandavg.MI[grandavg.res$grandavg.vis==1]
  regress.res <- data.frame(regress.MI, nosubregress.lag)
  res_plot = ggplot(data=regress.res, aes(x=nosubregress.lag, y=regress.MI)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-1, 3), breaks=seq(-1, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/MetaCogRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  
  #compare.MI = nosubfullgrandavg.MI-nosubregress.MI
  compare.res <- data.frame(c(nosubfullgrandavg.MI, nosubregress.MI), rep(nosubregress.lag, 2), rep(c("Lag", "Metacognition"), length.out=12, each=6))
  names(compare.res) = c("MI", "Lag", "Measure")
  res_plot = ggplot(data=compare.res, aes(x=Lag, y=MI, group=Measure, colour=Measure)) +
    geom_line(size = 1.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=2.5) + 
    scale_colour_manual(name="Lag", labels=c("Lag", "Metacognition", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(0.5, 2.5), breaks=seq(0.5, 2.5, 0.1)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Mutual information") +
    theme_bw()
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/MetaCogVLag2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  nosubregress.lag = c(1,2,3,4,6,8)
  nosubregress.zscore = (nosubregress.MI - mean(nosubregress.MI))/sd(nosubregress.MI)
  nosubaccdiff = acc.acc.highvis - acc.acc.lowvis
  nosubaccdiff.zscore = (nosubaccdiff - mean(nosubaccdiff))/sd(nosubaccdiff)
  compare.res <- data.frame(c(acc.zscore, nosubfullgrandavg.zscore, nosubregress.zscore, acc.vis.zscore, nosubaccdiff.zscore), rep(nosubregress.lag, 5), rep(c("Acc", "MI", "MetaExperience", "Vis", "MetaAccuracy"), length.out=30, each=6))
  names(compare.res) = c("Acc", "Lag", "Measure")
  res_plot = ggplot(data=compare.res, aes(x=Lag, y=Acc, group=Measure, colour=Measure)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape = 15) + 
    scale_colour_manual(name="Lag", breaks = c("Acc", "Vis", "MI", "MetaExperience", "MetaAccuracy"), values = c("Acc" = "#e91e63", "Vis"="#ff5722", "MI" = "#ff9800", "MetaExperience"="#ffc107", "MetaAccuracy" = "#ffeb3b", "#cddc39")) +
    scale_y_continuous(limits = c(-2, 2), breaks=seq(-2, 2, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("ZScore") +
    theme_bw() + 
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/AccMIMeta2BinZSCORE.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

  
  nosubregress.MI = c(nosubgrandavg.res$nosubgrandavg.MI[nosubgrandavg.res$nosubgrandavg.vis==2], nosubgrandavg.res$nosubgrandavg.MI[nosubgrandavg.res$nosubgrandavg.vis==1])
  nosubregress.lag = c(1,2,3,4,6,8,1,2,3,4,6,8)
  nosubregress.Measure = c(rep("High", 6), rep("Low", 6))
  nosubregress.res <- data.frame(nosubregress.MI, nosubregress.lag, nosubregress.Measure)
  res_plot = ggplot(data=nosubregress.res, aes(x=nosubregress.lag, y=nosubregress.MI, group = nosubregress.Measure, colour = nosubregress.Measure)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape = 15) + 
    scale_colour_manual(name="Lag", labels=c("High", "Low", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-1, 3), breaks=seq(-1, 3, 0.5)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Mutual Information") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubMetaCogCompare2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  
   
  nosubregress.MI = c(acc.acc.highvis, acc.acc.lowvis)
  nosubregress.lag = c(1,2,3,4,6,8,1,2,3,4,6,8)
  nosubregress.Measure = c(rep("High", 6), rep("Low", 6))
  nosubregress.res <- data.frame(nosubregress.MI, nosubregress.lag, nosubregress.Measure)
  res_plot = ggplot(data=nosubregress.res, aes(x=nosubregress.lag, y=nosubregress.MI, group = nosubregress.Measure, colour = nosubregress.Measure)) +
    geom_line(size = 2.5) +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape = 15) + 
    scale_colour_manual(name="Lag", labels=c("High", "Low", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0, 1), breaks=seq(-0, 1, 0.2)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Accuracy") +
    theme_bw() + 
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubAccCompare2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  nosubregress.MI = c(acc.acc.highvis - acc.acc.lowvis)
  nosubregress.lag = c(1,2,3,4,6,8)
  nosubregress.Measure = c(rep("MetaExperienceA", 6))
  nosubregress.res <- data.frame(nosubregress.MI, nosubregress.lag, nosubregress.Measure)
  res_plot = ggplot(data=nosubregress.res, aes(x=nosubregress.lag, y=nosubregress.MI, group = nosubregress.Measure, colour = nosubregress.Measure)) +
    geom_line(size = 2.5, colour="black") +
    #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) + 
    geom_point(size=5, shape = 15, colour="black") + 
    #scale_colour_manual(name="Lag", labels=c("High", "Low", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
    # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
    scale_y_continuous(limits = c(-0, 1), breaks=seq(-0, 1, 0.2)) +
    scale_x_continuous(limits = c(1, 8), breaks=seq(1, 8, 1)) +
    xlab("Lag") +
    ylab("Accuracy") +
    theme_bw() + 
    theme(
      axis.text = element_text(size = 32, face="bold"),
      axis.ticks = element_line(colour = "black", size = 2.5),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line.x = element_line(colour = "black", size = 2.5),
      axis.line.y = element_line(colour = "black", size = 2.5),
      axis.title.x = element_text(colour = "black", size = 40, face="bold"),
      axis.title.y = element_text(colour = "black", size = 40, face="bold"),
      legend.position = c(0.8,0.2),
      legend.text = element_text(size = 20, face="bold"),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
  res_plot
  ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/NoSubMetaExperienceACompare2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.lag)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.lag, y=fullgrandavg.MI)) +
  #   geom_line(size = 1.5) +
  #   #geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) +
  #   geom_point(size=2.5) +
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   #scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMI2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)
  # 
  # fullgrandavg.res <- data.frame(fullgrandavg.MI, fullgrandavg.vis)
  # res_plot = ggplot(data=fullgrandavg.res, aes(x=fullgrandavg.vis, y=fullgrandavg.MI)) +
  #   #geom_line(size = 1.5) +
  #   geom_smooth(method="lm",formula=y~x, se=FALSE, size=1.5) +
  #   #geom_point(size=2.5) +
  #   scale_colour_manual(name="Lag", labels=c("1", "2", "3", "4", "6", "8"), values = c("#e91e63", "#ff5722", "#ff9800", "#ffc107", "#ffeb3b", "#cddc39")) +
  #   # scale_y_continuous(limits = c(0, 4), breaks=seq(0, 4, 0.1)) +
  #   scale_y_continuous(limits = c(0, 1), breaks=seq(0, 1, 0.1)) +
  #   scale_x_continuous(limits = c(1, 6), breaks=seq(1, 6, 1)) +
  #   xlab("Visibility Bin") +
  #   ylab("Mutual information") +
  #   theme_bw()
  # res_plot
  # ggsave(filename="C:/R/MI/plots/MetaMI/Normalised/FullGrandAvgMIRegress2Bin.png", plot=res_plot, width = 297, height = 210, units="mm",dpi = 300)

}



if(FALSE)
{
  for(i in 1:6)
  {
    for(j in 1:6)
    {
      StimResp = as.vector((StimRespVisMatrix[i,j,,]/sum(StimRespVisMatrix[i,j,,])))
      Stim = rep(1:21, 21)
      Resp = rep(1:21, each=21)
      res = as.data.frame(StimResp, Stim, Resp)
      res_plot = ggplot(data=res, aes(x = Stim, y = Resp)) +
        geom_tile(aes(fill=StimResp), colour = "white") +
        scale_fill_gradient(low="white", high="darkred", limits = c(0,0.3)) +
        ggtitle(paste(LagVect[i],"Vis",j, " MI:", MIMatrix[((i-1)*6)+j], sep="")) + 
        theme_bw()
      ggsave(
        filename=paste("C:/R/MI/plots/Heatmaps/Heatmap",LagVect[i],"Vis",j,".png", sep=""),
        plot=res_plot,
        width = 297,
        height = 210,
        units="mm",
        dpi = 300
      )
    }
  }
}
