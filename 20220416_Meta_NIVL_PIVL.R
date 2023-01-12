#### April 16, 2022, may thurner/ illiac vein stenting project in meta analysis ####
# NIVL vs. PIVL vs. NIVL+PIVL
# outcomes that we are interested in: primary and secondary Stent patency 
# rate (12month), denominator column H,
# complications: column AA (<30 day), AB (>30 day),denominator is
# col G (total Px)

rm(list=ls())
pacman::p_load(metafor,meta,tidyverse,summarytools,compareGroups)

setwd("G:/My Drive/meta_project")



#### meta analysis NIVL v.s. PIVL

## short term complication
NIVL.sc<-data.frame(
author=c("Sang H","DeWolf"),
  NumPositive=c("5","1"),
  NumSample  =c("36","75")   )
NIVL.sc$group <-"NIVL"



PIVL.sc<-data.frame(
  author=c("Zhu Q","Jiang C","Zhu Y","Fanatsu A","Yonghua B","Stuck A",
           "Hue G","Zhu Q","Matsuda A","Blanch Alerlany"),
  NumPositive=c("6","99","12", "1","4","22",
                "4","4","2","1"),
  NumSample  =c("111","300","19","60","46","62",
                "59","85","24","107")   )
PIVL.sc$group <-"PIVL"
data <- rbind(NIVL.sc,PIVL.sc)
data$NumPositive <- as.numeric(data$NumPositive)
data$NumSample <- as.numeric(data$NumSample)
pes.summary=metaprop(NumPositive, NumSample,author, 
    data=data, sm="PRAW",fixed = FALSE,subgroup=group,
    tau.common=F,method = "Inverse",
     method.tau="DL", method.ci="NAsm")
#forest(pes.summary, layout = "RevMan5",squaresize=0.5, col.square="grey")  

png(file = "forestplot_short.png", width = 2800, height = 2400, res = 300)

forest(pes.summary, xlim=c(0,1),
       rightcols=FALSE,   leftcols=c("studlab","event","n", "effect", "ci"),
       #leftlabs=c("Study", "Proportion", "95% C.I."),
       weight.study="random", squaresize=0.5, 
       pooled.totals=TRUE,
       comb.fixed=FALSE,
       fs.hetstat=10,  print.tau2=TRUE,   print.Q=FALSE,
       print.pval.Q=TRUE,print.I2=FALSE, digits=2)
dev.off()

#  Assessment for small study effects
funnel.meta(pes.summary)
##  Linear regression test of funnel plot asymmetry
metabias(pes.summary)


## long term complication
NIVL.lc<-data.frame(
  author=c("Sang H"),
  NumPositive=c("17"),
  NumSample  =c("67")   )
NIVL.lc$group <-"NIVL"
PIVL.lc<-data.frame(
  author=c("Zhu Y","Fanatsu A","Hue G","Zhu Q","Matsuda A","Blanch Alerlany"),
  NumPositive=c("2","4",  "7", "3","2","3"),
  NumSample  =c("19","59","61","26","13","41")   )
PIVL.lc$group <-"PIVL"
data <- rbind(NIVL.lc,PIVL.lc)
data$NumPositive <- as.numeric(data$NumPositive)
data$NumSample <- as.numeric(data$NumSample)
pes.summary=metaprop(NumPositive, NumSample,author, 
                     data=data, sm="PRAW",fixed = FALSE,subgroup=group,
                     tau.common=F,method = "Inverse",
                     method.tau="DL", method.ci="NAsm")
#forest(pes.summary, layout = "RevMan5",squaresize=0.5, col.square="grey")  
forest(pes.summary, xlim=c(0,1),
       rightcols=FALSE,   leftcols=c("studlab","event","n", "effect", "ci"),
       #leftlabs=c("Study", "Proportion", "95% C.I."),
       weight.study="random", squaresize=0.5, 
       pooled.totals=TRUE,
       comb.fixed=FALSE,
       fs.hetstat=10,  print.tau2=TRUE,   print.Q=FALSE,
       print.pval.Q=TRUE,print.I2=FALSE, digits=2)

funnel.meta(pes.summary)


