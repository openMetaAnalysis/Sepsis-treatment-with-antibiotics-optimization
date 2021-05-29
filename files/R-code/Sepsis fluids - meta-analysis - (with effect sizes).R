#This file is best used within R Studio
# rbadgett@kumc.edu
### Start----------------------------------------
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + strheight("A")
ymax <- par("usr")[4] - strheight("A")
##* Libraries------------------------------------
library(openxlsx) # read.xlsx
#library(xlsx)    # read.xlsx2
library(plyr)
library(metafor)
library (meta)    # metamean
library(grid)
library(boot)     #inv.logit
library(dominanceanalysis) # Helps interpret LM by estimating contributions to R2 from each predictor
library(splines)
#library(Rcmdr)   # For NumSummary
#library(stringr) #str_locate, str_to_title
#library(DescTools) #StrPos
library (esc) # David Wilson's from Campbell Collaboration
#library(moments) # skewness
#library(tseries) # jarque.bera.tes
#library(ggplot2)
#library(barplot3d)
#library(rgl)# plot3d and text3d
#library("rqdatatable") # natural_join (replaces NA values in a dataframe merge)
#library(Rcmdr)
##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
(current.date.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
p.value <- sprintf(p.value, fmt='%#.3f')
I2.label <- expression(paste( plain("I") ^ plain("2"), plain("(%)")))
summary.label <- bquote(paste("RE Model (I^2 = ",(formatC(res$I2, digits=1, format="f")), "%)"))
##* Encoding characters---------------
# https://www.unicodepedia.com/groups/
# http://www.unicode.org/charts/
##* Footnotes
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \A7
# Double Vertical Line \u2016
# Para    \B6 or \u0086
##*Greek
# https://unicode.org/charts/nameslist/n_0370.html
##* Troubleshooting grab -----------------------
options(error=NULL)
library(tcltk) # For troubleshooting
# msgbox for troubleshooting: 
# tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
# browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q

## Create dataframe------------------------------
Leisman.2017 <- read.table(textConnection('
  Study,        Year,   PMID,     OR,   CI.l, CI.u,  Size, Outcome,    group, Setting,      Patients, Notes,         style
  "Leisman.30",   2017, 28671898, 0.74, 0.62, 0.87, 5336*, "Survival", 1,   ".Initiation",  "All",    "Estimated",     "normal"
  "Leisman.120",  2017, 28671898, 0.73, 0.61, 0.88, 2388*, "Survival", 1,   ".Initiation",  "All",    "Estimated",     "normal"
'), header=TRUE, sep=",",strip.white=TRUE)

dataframe <- Leisman.2017
# I have to correct Leisman later!

## Create array for Leisman------------------------------
Leisman <- c("Leisman\u00A7",2017,28671898,exp(meta1$TE.random),exp(meta1$lower.random),exp(meta1$upper.random),11182,"Mortality",1,"Initiation","All sepsis","< 120 minutes vs later","normal")

# Fluid initiation
dataframe <- read.table(textConnection('
  Study,        Year, PMID,     OR,   CI.l, CI.u,  Size, Outcome,    group, Setting,      Patients,                     Notes,         style
  "Lane*",       2014, 30646296, 0.67, 0.49, 0.90,   594, "Mortality", 1,   "Prehospital", "Severe sepsis (prehospital setting)",        "EMS\u00A7 vs usual care",   "normal"
  "Leisman",    2016, 27085369, 0.63, 0.46, 0.86,  1866, "Mortality", 1,   "Initiation",  "Severe sepsis / shock",                      "< 30 minutes vs later",   "normal"
  "Seymour\u2021",    2018, 28528569, 0.46, 0.23, 0.882,  1350, "Mortality", 1,   "Prehospital", "Severe sepsis (prehospital setting)",  "EMS\u00A7 vs usual care",    "normal" 
  "Pruinelli\u2021",  2018, 29298189, 0.94,  0.87, 1.00,  5072, "Mortality", 1,   "Initiation",  "Severe sepsis / shock",               "< 100 minutes vs later",  "normal"
'), header=TRUE, sep=",",strip.white=TRUE)

dataframe <- rbind(dataframe,Leisman)

#dataframe[1,12] <- "EMS\u2016 vs usual care"
#dataframe[3,12] <- "EMS\u2016 vs usual care"

dataframe$Study <- paste(dataframe$Study, ", ",dataframe$Year, sep="")
# dataframe$Year <- as.numeric(dataframe$Year) allow footnotes
dataframe$PMID <- as.numeric(dataframe$PMID)

dataframe$OR <- as.numeric(dataframe$OR)
dataframe$CI.l <- as.numeric(dataframe$CI.l )
dataframe$CI.u <- as.numeric(dataframe$CI.u )

# Invert ratios for studies reported as failure to administer
myvalues <- c('Survival')
for(i in 1:nrow(dataframe)){
  if (dataframe$Study[i] %in% myvalues){
    dataframe$OR[i]   <-1/dataframe$OR[i]
    dataframe$CI.l[i] <-1/dataframe$CI.l[i]
    dataframe$CI.u[i] <-1/dataframe$CI.u[i]
  }
}

# Sort
#Don't do this when replicating Sterling
#dataframe <- dataframe[order(dataframe$Recommended,dataframe$Year),]

#---------------------------------
write.csv(dataframe, file = "all-studies-out.csv", quote = TRUE, 
            eol = "\n", na = "NA", row.names = FALSE)
#---------------------------------

#Ln transformations
dataframe$OR.Ln <- log(dataframe$OR)
dataframe$CI.l.Ln <- log(dataframe$CI.l)
dataframe$CI.u.Ln <- log(dataframe$CI.u)

# OR conversion to effect size
# From:
# Chinn, 2000. From http://pubmed.gov/11113947
# Replicating Chinn using her data
effect.size <- log(1.32)/1.81
SE <-(log(5.68) - log(0.31))/2/1.96/1.81
# Now using our data
dataframe$CI.Ln_Width <- abs(dataframe$CI.u.Ln - dataframe$CI.l.Ln)
dataframe$SE.Ln <- dataframe$CI.Ln_Width/2/1.96 
dataframe$effect.size <- dataframe$OR.Ln#/1.81
dataframe$effect.size.SE <- dataframe$SE.Ln#/1.81
#SD from SE: http://handbook.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm

# Alternative: Pruinelli excluded from Chinn approach and using values calculated from Figure 4A. 
# Rationale, Chinn's assumptions not needed since we have direct data for effect size.
# Pruinelli corrections
myvalues <- c(29298189)
for(i in 1:nrow(dataframe))
  {
  if (dataframe$PMID[i] %in% myvalues){
    dataframe$effect.size[i]   <- -0.03145273;
    dataframe$effect.size.SE[i] <- 0.01785714 }
}
# Leisman corrections
myvalues <- c(28671898)
for(i in 1:nrow(dataframe))
{
  if (dataframe$PMID[i] %in% myvalues){
    dataframe$effect.size[i]   <- -0.307;
    dataframe$effect.size.SE[i] <- 0.0635 }
}

# Meta-analysis from library meta
analyticmethod = "Random effects model"
hartung = FALSE
if (hartung){analyticmethod = paste(analyticmethod," (Hartung-Knapp)")}
#meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", backtransf = TRUE, studlab=meta1$Study)

#Timing
meta1 <- dataframe
#hartung = FALSE
#meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", data=meta1, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=meta1$Study)
# Subgrouped
meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", byvar=Patients, data=meta1, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=meta1$Study)
meta1$I2.w
meta1$lower.I2.w
meta1$upper.I2.w

##** Forest plot------------------------
# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above
forest(meta1, bylab =NULL, 
       byseparator=NULL , bysort = FALSE, resid.hetlab = "Residual I2: ",
       print.p=FALSE, xlim=c(0.2,5), xlab="Odds ratio for mortality", 
       leftcols=c("studlab","Size","Notes"), leftlabs=c("Study","Patients","Fluid comparisons"), 
       digits.addcols = 0, just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, text.random=analyticmethod,text.random.w=analyticmethod, print.Q.subgroup=FALSE)
#grid.text("Speed of fluids and mortality reduction", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Speed of fluid initiation and mortality reduction in patients with sepsis", 0.5, 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
#Footer
grid.text('Notes:', 0.02, 0.18, hjust=0, gp=gpar(cex=1, font=2))
Footer <- paste('',"\n","* Data is from Lane at al's severe subgroup. No benefit among non-severe patients. Hypotension defined as SBP < 100 mm Hg.",sep='')
Footer <- paste(Footer,"\n","\u2020 Pruinelli at al's effect size is interpolated from their Figure 4A and converted to an odds ratio with the R package of Lüdecke.",sep='')
Footer <- paste(Footer,"\n","\u2020 Leisman at al's 2017 effect size is estiamted by combining their < 30 and < 120 minutes results.",sep='')
Footer <- paste(Footer,"\n","\u00A7 EMS. Emergency medical services.",sep='')
grid.text(Footer,   0.02, 0.11, hjust=0, gp=gpar(cex=1))

PlotName <- 'forest plot - fluid timing - '
#PlotName <- 'Outcome-Primary'
Directory <- '../Forest-plots/'

##** Export plot------------------------
rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(Directory,PlotName,current.date,'.png',sep=''),
  format = "png", width = 900, height = 800)

## Meta-regression (cannot do until more studies)
data.timing <- dataframe[dataframe$Setting == 'Initiation',]

# Will replace 'WSize' in example below with the product of mean lactate * mean age of each study
res <- rma.uni(data.timing$effect.size, data.timing$effect.size.SE*sqrt(Size), mods = ~ data.timing$Size*data.timing$Lactate, data=data.timing, method = "DL", intercept = TRUE, knha = TRUE)
res
