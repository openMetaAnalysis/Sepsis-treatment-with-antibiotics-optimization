#This file is best used within R Studio
#---------------------------------
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../data")
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
date.current <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
#---------------------------------

library(meta)      # Used for function metagen
library(grid)

#---------------------------------
# Get external data (NOT USED)
#File format is:  4 columns "Subject","Site", "Outcomes", "Observations"
file.filter <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename = choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
data<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
dataframe <- data
#---------------------------------

# dataframe$Year <- as.numeric(dataframe$Year) allow footnotes

#---------------------------------
write.csv(dataframe, file = "all-studies.csv", quote = TRUE, 
          eol = "\n", na = "NA", row.names = FALSE)
#---------------------------------

# Invert ratios for studies reported as failure to administer
myvalues <- c('Survival')
for(i in 1:nrow(dataframe)){
  if (dataframe$Outcome[i] %in% myvalues){
    dataframe$OR[i]   <-1/dataframe$OR[i]
    dataframe$CI.l[i] <-1/dataframe$CI.l[i]
    dataframe$CI.u[i] <-1/dataframe$CI.u[i]
  }
}

# Sort
#Don't do this when replicating Sterling
#dataframe <- dataframe[order(dataframe$Recommended,dataframe$Year),]

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
meta1 <- metagen(meta1$effect.size, meta1$effect.size.SE, sm="OR", byvar=Analysis, data=meta1, comb.fixed=FALSE, hakn=hartung, backtransf = TRUE, studlab=meta1$Study)
# WARNING: after backtransf for display in forest plots, note that point estimates and I.I.s do not exactly match the data inputted in the table 'data' above
forest(meta1, bylab ='Fluid: ', 
       byseparator=NULL , bysort = FALSE, resid.hetlab = "Residual I2: ",
       print.p=FALSE, xlim=c(0.2,5), xlab="Odds ratio for mortality", leftcols=c("studlab","Size"), 
       digits.addcols = 0, just.addcols.left = "left", colgap.left = "5mm", print.tau2=FALSE,col.diamond="blue", col.diamond.lines="blue", print.I2.ci=TRUE,overall=TRUE,test.subgroup=FALSE, test.subgroup.random=TRUE, text.random=analyticmethod,text.random.w=analyticmethod, print.Q.subgroup=FALSE)
grid.text("Speed of fluids and mortality", 0.5, 0.97, gp = gpar(fontsize = 14, fontface = "bold"))
Footer <- "Notes:"
Footer <- paste(Footer,"\n","* Seymour excluded patients with delays > 12 hours",sep='')
grid.text(Footer, 0.10, 0.075, hjust = 0, gp=gpar(cex=1))

plotname <- plotname <- paste('forest plot - fluid timing - ',date.current,'.png',sep='')
plotname <- plotname <- paste('Outcome-Primary','.png',sep='')

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  plotname,
  format = "png", width = 800, height = 700)
