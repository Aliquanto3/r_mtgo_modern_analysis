library(rprojroot)

setwd(rprojroot::find_rstudio_root_file())
getwd()

source(file.path(paste(rprojroot::find_rstudio_root_file(),"1-PARAMETERS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"2-ARCHETYPES-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"3-METAGAME_FUNCTIONS.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"4-CARD_DATA-IMPORT.R",sep="/")))
source(file.path(paste(rprojroot::find_rstudio_root_file(),"5-CARD_STATS.R",sep="/")))

#TO BE USED LATER WITH A LOOP ON EACH ARCHETYPE?
#WHAT IS THE DIFFERENCE BETWEEN BOTH? CAN THEY BE IN A SINGLE FILE?
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/6-DECKLISTS_ANALYSIS.R")
# source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/7_ARCHETYPE_ANALYSIS.R")

# df=generate_df(EventType,MTGFormat,RawFile)
# archetype_acc=getArchetypeAcc(Classification)
# #STILL HAS WARNINGS, WHERE DO THEY COME FROM? - NOT REALLY AN EMERGENCY
# cardDataSub=getCardData(DirectoryFile)
# df=addCMC(df)
# MDStats=CardsStatsGetter(df,"Mainboard")
# SBStats=CardsStatsGetter(df,"Sideboard")
# #CardResults=CardsStatsGetter(df,"Allboards")

#source("D:/MTG/Meta analysis/r_mtgo_modern_analysis/EXPORT_GRAPHS_AND_TXT.R")

#LAST FILE TO EXECUTE FOR ARTICLE DATA

df=generate_df(EventType,MTGFormat,RawFile)
archetype_acc=getArchetypeAcc(Classification)

metric_df=metric_points_archetypes(df,Beginning,End)
metric_df_log_matches=metric_df
metric_df_log_matches$TotalMatches=
  log(metric_df_log_matches$TotalMatches)

dnormCust = function(x){
  return(dnorm(x, mean = mean(metric_df$TotalMatches), sd = sd(metric_df$TotalMatches)))
}
plot(x = metric_df$WinrateAverage, y = metric_df$TotalMatches)
# plot (dnormCust, min(metric_df$WinrateAverage), max(metric_df$WinrateAverage), add=TRUE)
lines(lowess(metric_df$TotalMatches ~ metric_df$WinrateAverage, f=0.1))

x = seq(from=min(metric_df$WinrateAverage),to=max(metric_df$WinrateAverage),by=0.00001) 
y = dnorm(x, mean = mean(metric_df$WinrateAverage), sd = sd(metric_df$WinrateAverage))
plot(x=x,y=y)

fitG =
  function(x,y,mu,sig,scale){
    
    f = function(p){
      d = p[3]*dnorm(x,mean=p[1],sd=p[2])
      sum((d-y)^2)
    }
    
    optim(c(mu,sig,scale),f)
  }



library("dplyr")
library("ggpubr")

my_data=metric_df
dplyr::sample_n(my_data, 10)
dim(my_data)
ggdensity(my_data$WinrateAverage, 
          main = "Density plot of win rate",
          xlab = "Win rate")
ggqqplot(my_data$WinrateAverage)
shapiro.test(my_data$WinrateAverage)
