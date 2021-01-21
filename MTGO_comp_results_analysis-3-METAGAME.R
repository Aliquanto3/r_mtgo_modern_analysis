#3rd file to execute
#provides functions to determine metagame share and winrates, as well as graphs
#to display those results

#also imports all the libraries that can be useful here or in following files

#Execute this file only once, unless you directly edit it, it doesn't treat data
#only provides some functions to analyse it

################################################################################
#LIBRARIES
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggrepel")
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("purrr")
#install.packages("jsonify")
#install.packages("plyr")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("corrplot")
#install.packages("ggpubr")
#install.packages("ClustMAPDP")
#install.packages("expm")
#install.packages("matrixStats")
#devtools::install_github('thomasp85/gganimate')
#install.packages('gganimate')
#install.packages("gifski")
#install.packages("raster")
#install.packages("conflicted")
library(ggplot2)
library(dplyr)
library(ggrepel)
library(jsonlite)
library(tidyverse)
library(data.table)
library(purrr)
library(jsonify)
library(plyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggpubr)
library(ClustMAPDP)
library(expm)
library(matrixStats)
library(gganimate)
library(gifski)
library(raster)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("arrange", "dplyr")

################################################################################

#VARIABLE FOR THE ACCURACY ON THE NAMING OF THE ARCHETYPES
#PROVIDES THE NAME OF THE COLUMN OF THE DATAFRAME TO BE USED
archetype_acc=NA
if(Classification=="Super"){
  archetype_acc="SUPER_ARCH"
}else if(Classification=="Exact"){
  archetype_acc="ARCHETYPE"
}

#LIST ALL THE DIFFERENT ARCHETYPES IN THE DATA
generate_archetype_list = function(df,beginning,end){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  periodDf=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  arch_list=data.frame(unique(df[[archetype_acc]]))
  names(arch_list)[1] = c("ARCHETYPES")
  return(arch_list)
}

#COMPUTES THE SHARE OF EACH ARCHETYPE IN THE DATA
#presence CAN BE EITHER "Copies", "Players", "Matches" or "Ratio M/P"
generate_metagame_data = function(df,graph_share,presence,beginning,end){
  
  arch_list=generate_archetype_list(df,beginning,end)
  
  #ADD THE PRESENCE OF EACH ARCHETYPE IN THE DATA
  arch_list$PRESENCE=rep(0,length(arch_list$ARCHETYPES))
  for (i in 1:length(arch_list$PRESENCE)){
    arch_id=which(df[[archetype_acc]]==arch_list$ARCHETYPES[i])
    if (presence=="Copies"){
      #NUMBER OF COPIES
      arch_list$PRESENCE[i]=length(arch_id)
    }else if (presence=="Players"){
      #NUMBER OF PLAYERS
      arch_list$PRESENCE[i]=length(unique(df[arch_id,]$PLAYER))
    }else if (presence=="Matches"){
      #NUMBER OF ROUNDS PLAYED
      arch_list$PRESENCE[i]=sum(df[arch_id,]$NB_ROUNDS,df[arch_id,]$TOP8_MATCHES)
    }else if (presence=="Ratio M/P"){
      #NUMBER OF ROUNDS PLAYED
      arch_list$PRESENCE[i]=sum(df[arch_id,]$NB_ROUNDS,df[arch_id,]$TOP8_MATCHES)/
        length(unique(df[arch_id,]$PLAYER))
    }
  }
  
  #FOR EASIER READING OF THE GRAPHS, AGGREGATE ALL THE ARCHETYPES ACCOUNTING FOR 
  #LESS THAN graph_share% OF THE DATA
  graph_perc=graph_share/100*sum(arch_list$PRESENCE)
  arch_list_vis=arch_list[arch_list$PRESENCE >= graph_perc, ]
  
  #ADD AN "OTHER" CATEGORY CONTAINING THE SUM OF COPIES OF ALL ARCHETYPES UNDER X%
  sum_others=sum(arch_list[arch_list$PRESENCE < graph_perc, ]$PRESENCE)
  otherName=paste("Other (<",graph_share,"%)",sep="")
  arch_list_vis=rbind(arch_list_vis,c(otherName, sum_others))
  arch_list_vis=arch_list_vis[order(arch_list_vis$ARCHETYPES),]
  
  arch_list_vis$PRESENCE=as.numeric(arch_list_vis$PRESENCE)
  arch_list_vis$SHARE=as.numeric(format(round(arch_list_vis$PRESENCE/
                                                sum(arch_list_vis$PRESENCE)*100,
                                              1), nsmall = 1))
  
  arch_list_vis$ARCHETYPES = reorder(arch_list_vis$ARCHETYPES, 
                                     as.numeric(arch_list_vis$PRESENCE))
  
  return(arch_list_vis)
}

#COMPUTES A NAME FOR THE HISTOGRAM AND THE PIE CHART
#presence CAN BE EITHER "Copies", "Players" or "Matches"
generate_metagame_graph_title = function(presence,beginning,end,EventType){
  MetaGraphTitle=paste("Proportion of", Classification,"archetypes in MTGO", 
                       EventType,"between", beginning, "and", end,
                       "based on number of", presence,sep = " ")
  return(MetaGraphTitle)
}

#GENERATE A PIE CHART BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_pie_chart = function(df,presence,beginning,end,EventType){
  
  #CHANGE THE NUMBER FOR THE PROPORTION OF THE "OTHERS" CATEGORY HERE
  df_gen=generate_metagame_data(df,PieShare,presence,beginning,end)
  
  ggplot(df_gen, aes(x="", SHARE, fill = ARCHETYPES)) + 
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(SHARE, "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = NULL, subtitle = "by Anael Yahi",
         title = generate_metagame_graph_title(presence,beginning,end,EventType)) + 
    guides(color = FALSE, size = FALSE) +
    scale_color_gradient(low="red", high="blue") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0, color = "#111111"))
  
}

#GENERATE A BOX PLOT BASED ON DATA IN DF
#presence CAN BE EITHER "Copies", "Players" or "Matches"
metagame_box_plot = function(df,presence,beginning,end,EventType){
  
  #GET THE DATA FOR ALL ARCHETYPES HAVING A META SHARE ABOVE HistShare
  df_gen=generate_metagame_data(df,HistShare,presence,beginning,end)
  
  #GENERATE A TITLE FOR THE BOXPLOT
  boxplot_title=paste(generate_metagame_graph_title(presence,beginning,end,EventType),"-",
                      df_gen[grep("Other",df_gen$ARCHETYPES),]$ARCHETYPES, sep=" ")
  
  #THIS GRAPH DOESN'T DISPLAY THE "Other" CATEGORY
  df_gen=df_gen[!grepl("Other",df_gen$ARCHETYPES),]
  
  #REORDER ARCHETYPES BY ASCENDING PRESENCE
  df_gen$ARCHETYPES = reorder(df_gen$ARCHETYPES, as.numeric(df_gen$PRESENCE))
  
  #plot is much clearer
  ggplot(df_gen, aes(x=ARCHETYPES, y=as.numeric(SHARE), fill=ARCHETYPES)) + 
    geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
    labs(x = NULL, y = "Presence (%)", fill = NULL, 
         title = boxplot_title, subtitle = "by Anael Yahi") + 
    scale_color_gradient(low="blue", high="red") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme(axis.text.x  = element_text(size=12))
}

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING WINRATES DEPENDING ON
#PRESENCE, WHICH IS HIGHLIGHTED BY THE DIAMETERS OF ANOTHER TYPE OF PRESENCE
#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
#tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
#isLog is a boolean
#only_best is a boolean
metric_graph = function(metric_df,presence,diameters,diam_ratio,beginning,end,
                        tiers,isLog,only_best,EventType) {
  if(only_best){
    metric_df=metric_df[metric_df$TOTAL_NB_MATCHES>mean(metric_df$TOTAL_NB_MATCHES),]
  }
  
  #COMPUTES THE PARAMETERS OF THE LINES TO APPEAR ON THE GRAPH
  if (presence=="Copies"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$NB_COPIES/
                                                   sum(metric_df$NB_COPIES)*100)
  }else if (presence=="Players"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$NB_PLAYERS/
                                                   sum(metric_df$NB_PLAYERS)*100)
  }else if (presence=="Matches"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE)/max(metric_df$TOTAL_NB_MATCHES/
                                                   sum(metric_df$TOTAL_NB_MATCHES)*100)
  }
  average=mean(metric_df$WINRATE_AVERAGE)
  sdeviation=sd(metric_df$WINRATE_AVERAGE)
  
  #GENERATES THE LABELS
  if (presence=="Copies"){
    x_label="Total number of copies of each archetype (%)"
  }else if (presence=="Players"){
    x_label="Total number of different players for each archetype (%)"
  }else if (presence=="Matches"){
    x_label="Total number of matches played by each archetype (%)"
  }
  y_label="Average winrate of each archetype (%)"
  graph_title=paste("Winrates depending on presence:", Classification,"archetypes ", 
                    "between", beginning, "and", end, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Circle diameters depending on",diameters,"\nby Anael Yahi",sep=" ")
  if(tiers=="Win+Pres"){
    graph_subtitle=paste("Separated by mean +/- n standard deviations (n={0,1,2,3}) 
Circle diameters depending on",diameters,"\nby Anael Yahi",sep=" ")
  }
  
  #GENERATES THE GRAPH
  if (presence=="Copies"){
    metric_df$NB_COPIES=metric_df$NB_COPIES/sum(metric_df$NB_COPIES)*100
    metric_plot=ggplot(metric_df, aes(NB_COPIES, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$NB_COPIES)
    std_presence=sd(metric_df$NB_COPIES)
  }else if (presence=="Players"){
    metric_df$NB_PLAYERS=metric_df$NB_PLAYERS/sum(metric_df$NB_PLAYERS)*100
    metric_plot=ggplot(metric_df, aes(NB_PLAYERS, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$NB_PLAYERS)
    std_presence=sd(metric_df$NB_PLAYERS)
  }else if (presence=="Matches"){
    metric_df$TOTAL_NB_MATCHES=metric_df$TOTAL_NB_MATCHES/
      sum(metric_df$TOTAL_NB_MATCHES)*100
    metric_plot=ggplot(metric_df, aes(TOTAL_NB_MATCHES, WINRATE_AVERAGE*100))
    avg_presence=mean(metric_df$TOTAL_NB_MATCHES)
    std_presence=sd(metric_df$TOTAL_NB_MATCHES)
  }
  
  if (diameters=="Copies"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_COPIES*diam_ratio,
                 show.legend = FALSE)
  }else if (diameters=="Players"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_PLAYERS*diam_ratio,
                 show.legend = FALSE)
  }else if (diameters=="Matches"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$TOTAL_NB_MATCHES*
                   diam_ratio,show.legend = FALSE)
  }
  
  metric_plot=metric_plot + coord_cartesian() + theme_bw() + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA) 

    #tiers CAN BE EITHER "Win+Pres","Pres M+SD" or "Pres %"
  if (tiers=="Win+Pres"){
    #TIERS BASED ON COMBINATION OF MEAN AND STANDARD DEVIATION OF PRESENCE AND WINRATE
    metric_plot=metric_plot + geom_abline(intercept = average, slope = coeffdir,
                                          color="red", size=1.5) +
      geom_abline(intercept = average+1*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average+2*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average+3*sdeviation, slope = coeffdir,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average-1*sdeviation, slope = coeffdir,
                  color="red", linetype="dotted", size=1.5) +
    geom_abline(intercept = average-2*sdeviation, slope = coeffdir,
                color="red", linetype="dotted", size=1.5) +
    geom_abline(intercept = average-3*sdeviation, slope = coeffdir,
                color="red", linetype="dashed", size=1.5)
    
  }else if (tiers=="Pres M+SD"){
    #TIERS BASED ON MEAN + N * STANDARD DEVIATION OF PRESENCE, N={0,1,2,3}
    metric_plot=metric_plot + geom_vline(xintercept = avg_presence, linetype="dotted",
                                        color = "purple", size=2) +
      geom_text(aes(x=avg_presence, label="Tiers 2.5\n", y=
                      max(WINRATE_AVERAGE*100)), colour="purple",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence, label="\nPresence mean", y=
                      max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + std_presence, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_text(aes(x=avg_presence + std_presence, label="Tiers 2\n",
                    y=max(WINRATE_AVERAGE*100)), colour="blue",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + std_presence, label="\nMean + 1*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 2*std_presence, linetype="dotted",
                 color = "dark green", size=1.5) +
      geom_text(aes(x=avg_presence + 2*std_presence, label="Tiers 1.5\n",
                    y=max(WINRATE_AVERAGE*100)), colour="dark green",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 2*std_presence, label="\nMean + 2*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 3*std_presence, linetype="dotted",
                 color = "orange", size=1.5) +
      geom_text(aes(x=avg_presence + 3*std_presence, label="Tiers 1\n",
                    y=max(WINRATE_AVERAGE*100)), colour="orange",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 3*std_presence, label="\nMean + 3*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4) +
      geom_vline(xintercept = avg_presence + 4*std_presence, linetype="dotted",
                 color = "red", size=1.5) +
      geom_text(aes(x=avg_presence + 4*std_presence, label="Tiers 0\n",
                    y=max(WINRATE_AVERAGE*100)), colour="red",
                angle=0, size=8) +
      geom_text(aes(x=avg_presence + 4*std_presence, label="\nMean + 4*sd",
                    y=max(WINRATE_AVERAGE*100)), colour="grey",
                angle=0, size=4)
    
  }else if (tiers=="Pres %"){
    #TIERS BASED ON ARBITRARY % OF PRESENCE: 2,4,6,8,10
    metric_plot=metric_plot + geom_vline(xintercept = 10, linetype="dashed",
                                         color = "blue", size=2) +
      geom_vline(xintercept = 8, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 6, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 4, linetype="dotted",
                 color = "blue", size=1.5) +
      geom_vline(xintercept = 2, linetype="dashed",
                 color = "blue", size=2) +
      geom_abline(intercept = average, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average+sdeviation, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average-sdeviation, slope = 0,
                  color="red", linetype="dashed", size=1.5) +
      geom_abline(intercept = average+0.5*sdeviation, slope = 0,
                  color="red", linetype="dotted", size=1.5) +
      geom_abline(intercept = average-0.5*sdeviation, slope = 0,
                  color="red", linetype="dotted", size=1.5)
  }
  
  if (isLog){
    metric_plot=metric_plot + scale_x_continuous(trans = 'log10')
  }
  
  return(metric_plot)
  
}

#FILL IN METRIC POINTS IN AN ARCHETYPES DATA FRAME
metric_points_archetypes = function(df,beginning,end){
  df2=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df2,beginning,end)
  players_df=metric_points_players(df,beginning,end)
  
  metric_df$NB_COPIES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$NB_PLAYERS=rep(0,length(metric_df$ARCHETYPES))
  metric_df$TOTAL_NB_MATCHES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_AVERAGE=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_95_MIN=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_95_MAX=rep(0,length(metric_df$ARCHETYPES))
  metric_df$MATCHES_PER_PLAYER=rep(0,length(metric_df$ARCHETYPES))
  metric_df$WINRATE_ARCH_OUT_PLAYERS=rep(0,length(metric_df$ARCHETYPES))
  
  for (i in 1:length(metric_df$ARCHETYPES)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    arch_identification=which(df2[[archetype_acc]]==metric_df$ARCHETYPES[i])
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df$NB_COPIES[i]=length(arch_identification)
    #NUMBER OF DIFFERENT PLAYERS PLAYING THAT DECK
    metric_df$NB_PLAYERS[i]=length(unique(df2[arch_identification,]$PLAYER))
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df$TOTAL_NB_MATCHES[i]=sum(df2[arch_identification,]$NB_ROUNDS,
                                      df2[arch_identification,]$TOP8_MATCHES)
    #NUMBER OF MATCHES PER PLAYER - THE HIGHER, THE MORE A PLAYER wiTH THAT DECK
    #APPEARED IN THE RESULTS
    metric_df$MATCHES_PER_PLAYER[i]=metric_df$TOTAL_NB_MATCHES[i]/
      metric_df$NB_PLAYERS[i]
    
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df2$POINTS[arch_identification] + 
                           df2$TOP8_PTS[arch_identification])/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df2$NB_ROUNDS[arch_identification] + 
                             df2$TOP8_MATCHES[arch_identification])
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df$WINRATE_AVERAGE[i]=binom.test(total_wins_arch, total_matches_arch, 
                                            p=0.5,alternative="two.sided", 
                                            conf.level=0.95)$estimate
    #LOWER BOUND OF THE "TRUE" WINRATE             
    metric_df$WINRATE_95_MIN[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[1]
    #UPPER BOUND OF THE "TRUE" WINRATE 
    metric_df$WINRATE_95_MAX[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[2]
    
    #WINRATE OF THAT ARCHETYPE DIVIDED BY THE AVERAGE WINRATE OF ITS PILOTS
    metric_df$WINRATE_ARCH_OUT_PLAYERS[i]=metric_df$WINRATE_AVERAGE[i]/
      mean(players_df[grep(metric_df$ARCHETYPES[i],
                           players_df$ARCHETYPE_NAMES),]$WINRATE_AVERAGE)
  }

  return(metric_df)
}

#COMBINES THE RATIOS OF POINTS PER ROUND AND NUMBER OF COPIES FOR EACH
#ARCHETYPE, THEN PROVIDES A RANK BASED ON THAT
#THE NEW METRIC OBTAINED THAT WAY IS NORMALIZED TO BE BETWEEN 0 AND 1
#ALSO IMPLEMENTS THE VS META SCORE 
#https://www.vicioussyndicate.com/vs-meta-score-new-metric-measuring-archetypes-standing-meta/
archetypes_ranking = function(metric_df,beginning,end){
  
  metric_df$METRIC_COMB=metric_df$WINRATE_AVERAGE
  for (i in 1:length(metric_df$METRIC_COMB)){
    metric_df$METRIC_COMB[i] = 
      (Presence_Weight * (metric_df$TOTAL_NB_MATCHES[i]-
                            min(metric_df$TOTAL_NB_MATCHES)) /
         max(metric_df$TOTAL_NB_MATCHES) +
         (PPR_Weight * metric_df$WINRATE_AVERAGE[i]-
            min(metric_df$WINRATE_AVERAGE)) /
         max(metric_df$WINRATE_AVERAGE )) /
      (Presence_Weight+PPR_Weight)
  }
  
  metric_df = metric_df[order(-metric_df$METRIC_COMB),]
  
  metric_df$RANK=metric_df$METRIC_COMB
  for (i in 1:length(metric_df$RANK)){
    metric_df$RANK[i]=i
  }
  
  metric_df$VS_META_SCORE=metric_df$METRIC_COMB
  MetaPeak=c(max(metric_df$TOTAL_NB_MATCHES) - min(metric_df$TOTAL_NB_MATCHES) /
               max(metric_df$TOTAL_NB_MATCHES),
             max(metric_df$WINRATE_AVERAGE) - min(metric_df$WINRATE_AVERAGE) /
               max(metric_df$WINRATE_AVERAGE))
  
  for (i in 1:length(metric_df$VS_META_SCORE)){
    ArchCoord=c((metric_df$TOTAL_NB_MATCHES[i]-min(metric_df$TOTAL_NB_MATCHES)) /
      max(metric_df$TOTAL_NB_MATCHES),
          (metric_df$WINRATE_AVERAGE[i]-min(metric_df$WINRATE_AVERAGE)) /
      max(metric_df$WINRATE_AVERAGE))
    
    metric_df$VS_META_SCORE[i]=pointDistance(MetaPeak, ArchCoord, lonlat=FALSE)
  }
  return(metric_df)
}

#PLOT OF THE AVERAGE WINRATE FOR THE MOST POPULAR ARCHETYPES
#presence CAN BE EITHER "Copies", "Players" or "Matches"
winrates_graph = function(df,arch_ranked,presence,beginning,end,EventType){
  
  #GET ONLY THE DECKS APPEARING THE MOST IN THE DATA
  if (presence=="Copies"){
    #KEEP ONLY THE DECK WITH THE MOST COPIES
    presence_min=HistShare/100*length(df$ARCHETYPE)
    arch_most_played=arch_ranked[arch_ranked$NB_COPIES>=presence_min,]
  }else if (presence=="Players"){
    #KEEP ONLY THE DECK WITH THE MOST PLAYERS
    presence_min=HistShare/100*length(unique(df$PLAYER))
    arch_most_played=arch_ranked[arch_ranked$NB_PLAYERS>=presence_min,]
  }else if (presence=="Matches"){
    #KEEP ONLY THE DECK WITH THE MOST MATCHES
    presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
    arch_most_played=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  }
  
  #REORDER ARCHETYPES BY ASCENDING AVERAGE WINRATE
  arch_most_played$ARCHETYPES = reorder(arch_most_played$ARCHETYPES, 
                                        as.numeric(arch_most_played$WINRATE_AVERAGE))
  #PLOT THE AVERAGE WINRATE AND THE CONFIDENCE INTERVALS
  y_label_winrate="Winrates of the most popular archetypes (%)"
  graph_title_winrate=paste(
    "Confidence intervals on the winrates of the most present archetypes ", 
    "( at least ",HistShare,"% of the ",presence,") between ", beginning, 
    " and ", end, " in MTGO ", EventType,sep="")
  
  ggplot(arch_most_played, aes(x=ARCHETYPES, y=WINRATE_AVERAGE*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_AVERAGE*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y=y_label_winrate, title=graph_title_winrate,
         subtitle="Red lines for the average of the bounds of the CI
Green line for the average of the computed winrate
by Anael Yahi")+
    geom_errorbar(aes(ymax = WINRATE_95_MAX*100, ymin = WINRATE_95_MIN*100)) + 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_AVERAGE*100), 
               color="green", linetype="dashed", size=1)+ 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_95_MIN*100), 
               color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = mean(arch_most_played$WINRATE_95_MAX*100), 
               color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#PLOT THE REPARTITION FOR THE LINEAR COMBINATION OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
linear_comb_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  
  meanData=mean(arch_ranked_sub_2$METRIC_COMB*100)
  sdData=sd(arch_ranked_sub_2$METRIC_COMB*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$METRIC_COMB)
  
  titleLinearComb=paste("Linear combination of the metrics for the most popular archetypes
At least ",HistShare,"% of presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight, "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=METRIC_COMB*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(METRIC_COMB*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Value of the linear combination metric", title=titleLinearComb,
         subtitle="Green line for the average of the metrics linear combination
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PLOT THE REPARTITION FOR THE A LOGARITHMIC COMBINATION OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
log_comb_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=log(presence_min),]
  
  meanData=mean(arch_ranked_sub_2$METRIC_COMB*100)
  sdData=sd(arch_ranked_sub_2$METRIC_COMB*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$METRIC_COMB)
  
  titleLinearComb=paste("Combination of the metrics for the most popular archetypes
At least ",HistShare,"% of presence - Linear winrate, logarithmic presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight,  "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=METRIC_COMB*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(METRIC_COMB*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Value of the linear combination metric", title=titleLinearComb,
         subtitle="Green line for the average of the metrics linear combination
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PLOT THE REPARTITION FOR THE LINEAR COMBINATION OF THE PRESENCE AND WINRATES
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
lower_bound_ci_winrate_graph = function(df,arch_ranked,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  arch_ranked_sub_2=arch_ranked[arch_ranked$TOTAL_NB_MATCHES>=presence_min,]
  
  meanData=mean(arch_ranked_sub_2$WINRATE_95_MIN*100)
  sdData=sd(arch_ranked_sub_2$WINRATE_95_MIN*100)
  meanPlusSd=meanData+sdData
  meanMinusSd=meanData-sdData
  
  arch_ranked_sub_2$ARCHETYPES=reorder(arch_ranked_sub_2$ARCHETYPES,
                                       arch_ranked_sub_2$WINRATE_95_MIN)
  
  titleLinearComb=paste("Lower bound of the confidence intervals for the winrates of the most popular decks
At least ",HistShare,"% of presence
Presence Weight = ",Presence_Weight, " / Winrate weight = ",PPR_Weight,   "
Between ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(arch_ranked_sub_2, aes(x=ARCHETYPES, y=WINRATE_95_MIN*100)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_95_MIN*100,1), nsmall = 1)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Lower estimation of the winrate (%)", title=titleLinearComb,
         subtitle="Green line for the average of the lower estimation of winrates
Red lines for the average +/- a standard deviation
by Anael Yahi")+
    geom_hline(yintercept = meanData, color="green", linetype="dashed", size=1)+ 
    geom_hline(yintercept = meanPlusSd, color="red", linetype="dashed", size=0.5)+ 
    geom_hline(yintercept = meanMinusSd, color="red", linetype="dashed", size=0.5) + 
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#PLOT THE REPARTITION FOR THE WINRATE RATIO OF ARCHETYPE OUT OF PLAYERS
#FOR THE MOST POPULAR ARCHETYPES
#PRESENCE: NUMBER OF MATCHES
winrate_ratio_arch_out_player_graph = function(df,metric_df,beginning,end,EventType){
  
  presence_min=HistShare/100*(sum(df$NB_ROUNDS)+sum(df$TOP8_MATCHES))
  metric_df_2=metric_df[metric_df$TOTAL_NB_MATCHES>=presence_min,]
  
  metric_df_2$ARCHETYPES=reorder(metric_df_2$ARCHETYPES,
                                       metric_df_2$WINRATE_ARCH_OUT_PLAYERS)
  
  titleRatioWin=paste("Ratio of the winrate of the archetypes out of the winrate ", 
  "of their pilotes for the most popular decks
At least ",HistShare,"% of presence",
"\nBetween ",beginning," and ",end," in MTGO ",EventType,sep="")
  
  ggplot(metric_df_2, aes(x=ARCHETYPES, y=WINRATE_ARCH_OUT_PLAYERS)) + 
    theme_classic() + geom_point(size=2,color="blue") +  
    geom_text_repel(aes(label=format(round(WINRATE_ARCH_OUT_PLAYERS,3), nsmall = 3)),
                    hjust=-0.3, vjust=-0.3,point.padding = NA)+ 
    labs(x=NULL, y="Ratio", title=titleRatioWin,
         subtitle="Ratio>1 means that the archetype winrate is over its pilots' winrate
         \nby Anael Yahi")+
    geom_hline(yintercept = 1, color="green", linetype="dotted", size=1)+
    theme(axis.text.x  = element_text(size=12)) +
    scale_x_discrete(guide = guide_axis(n.dodge=2))
}

#SORT THE ARCHETYPES IN CLUSTERS BASED ON PRESENCE AND WINRATE
kmeans_arch = function (metric_df,k,iter,init,algo,beginning,end,
                        count_wr,only_best,EventType){
  df_elim=select(metric_df, TOTAL_NB_MATCHES, WINRATE_AVERAGE, ARCHETYPES,NB_PLAYERS)
  if(only_best){
    df_elim=df_elim[df_elim$TOTAL_NB_MATCHES>mean(df_elim$TOTAL_NB_MATCHES),]
  }
  df_elim$PRESENCE=df_elim$TOTAL_NB_MATCHES/(sum(df_elim$TOTAL_NB_MATCHES))*100
  if (count_wr){
    df_kde=select(df_elim, PRESENCE, WINRATE_AVERAGE)
  }else{
    df_kde=select(df_elim, PRESENCE)
  }
  
  set.seed(123)
  res.km=kmeans(scale(df_kde), k, iter.max = iter, nstart = init, 
                algorithm = algo)
  df_kde$CLUSTER=factor(res.km$cluster)
  df_kde$ARCHETYPES=df_elim$ARCHETYPES
  df_kde$NB_PLAYERS=df_elim$NB_PLAYERS
  df_kde$WINRATE_AVERAGE=df_elim$WINRATE_AVERAGE
  
  x_label="Presence"
  y_label="Winrate"
  graph_title=paste("Winrates depending on presence:", Classification,"archetypes ", 
                    "between", beginning, "and", end, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Clustered in",k,"categories with",algo,"algorithm
by Anael Yahi",sep = " ")
  ggplot(data = df_kde,  mapping = aes(x = PRESENCE, y = WINRATE_AVERAGE, 
                                       colour = CLUSTER)) + 
    coord_cartesian() + theme_bw() + scale_x_continuous(trans = 'log10') + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) +
    geom_text_repel(aes(label=ARCHETYPES),hjust=0.5, vjust=-1.5,point.padding = NA) + 
    geom_point(aes(size=NB_PLAYERS),show.legend = FALSE)
}

#LIST ALL THE DIFFERENT PLAYERS IN THE DATA
generate_player_list = function(df,beginning,end){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  periodDf=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  play_list=data.frame(unique(df$PLAYER))
  names(play_list)[1] = c("PLAYERS")
  return(play_list)
}

#EronRelentless AND Kanonenfutter ARE THE SAME PERSON
#FILL IN METRIC POINTS IN A PLAYER DATA FRAME
metric_points_players = function(df,beginning,end){
  df2=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  #GET THE LIST OF THE DIFFERENT PLAYERS IN THE DATA
  metric_df_players=generate_player_list(df2,beginning,end)
  
  metric_df_players$NO_APPEARANCES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$TOTAL_NB_MATCHES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_AVERAGE=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_95_MIN=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$WINRATE_95_MAX=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$TOTAL_POINTS=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$ARCHETYPE_NAMES=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$ARCHETYPE_COUNT=rep(0,length(metric_df_players$PLAYERS))
  metric_df_players$URL=rep(0,length(metric_df_players$PLAYERS))
  for (i in 1:length(metric_df_players$PLAYERS)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    play_identification=which(df2$PLAYER==metric_df_players$PLAYERS[i])
    df3=df2[play_identification,]
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df_players$NO_APPEARANCES[i]=length(play_identification)
    #LIST OF DIFFERENT ARCHETYPES THAT PLAYER PLAYED
    metric_df_players$ARCHETYPE_NAMES[i]=list(unique(df3$ARCHETYPE))
      #paste(unique(df3$ARCHETYPE),collapse=",")
    arch_count=c()
    for(j in 1:length(metric_df_players$ARCHETYPE_NAMES[[i]])){
      arch_count[j]=length(df3[df3$ARCHETYPE==metric_df_players$
                                 ARCHETYPE_NAMES[[i]][[j]],]$URL)
    }
    metric_df_players$ARCHETYPE_COUNT[i]=list(arch_count)
    
    metric_df_players$URL[i]=list(unique(df3$URL))
      #paste(df3$URL,collapse=",")
    
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df_players$TOTAL_NB_MATCHES[i]=sum(df3$NB_ROUNDS,
                                      df3$TOP8_MATCHES)
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df3$POINTS + df3$TOP8_PTS)/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df3$NB_ROUNDS + df3$TOP8_MATCHES)
    metric_df_players$TOTAL_POINTS[i]=total_wins_arch*3
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df_players$WINRATE_AVERAGE[i]=binom.test(total_wins_arch, total_matches_arch, 
                                            p=0.5,alternative="two.sided", 
                                            conf.level=0.95)$estimate
    #LOWER BOUND OF THE MEASURED WINRATE             
    metric_df_players$WINRATE_95_MIN[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[1]
    #UPPER BOUND OF THE MEASURED WINRATE 
    metric_df_players$WINRATE_95_MAX[i]=binom.test(total_wins_arch, total_matches_arch, 
                                           p=0.5,alternative="two.sided", 
                                           conf.level=0.95)$conf.int[2]
  }
  metric_df_players=arrange(metric_df_players,desc(TOTAL_POINTS))
  
  return(metric_df_players)
}

#GENERATE A DF CONTAINING TIERS LISTS DEPENDING ON VARIOUS METRICS
generate_tiers_lists = function(arch_ranked){
  #TIERS LIST BASED ON METRIC SCORE
  arch_metric_score=arch_ranked[c("ARCHETYPES","METRIC_COMB")]
  arch_metric_score=arrange(arch_metric_score,desc(METRIC_COMB))
  head(arch_metric_score)
  mmc=mean(arch_metric_score$METRIC_COMB)
  sdmc=sd(arch_metric_score$METRIC_COMB)
  arch_metric_score_tiers1=arch_metric_score[arch_metric_score$METRIC_COMB>mmc+sdmc,]
  arch_metric_score_tiers1.5=arch_metric_score[arch_metric_score$METRIC_COMB>mmc &
                                                 arch_metric_score$METRIC_COMB<=mmc+sdmc,]
  arch_metric_score_tiers2=arch_metric_score[arch_metric_score$METRIC_COMB<=mmc &
                                               arch_metric_score$METRIC_COMB>=mmc-sdmc,]
  arch_metric_score_tiers2.5=arch_metric_score[arch_metric_score$METRIC_COMB<mmc-sdmc,]
  arch_metric_score_tiers1
  arch_metric_score_tiers1.5
  arch_metric_score_tiers2
  arch_metric_score_tiers2.5
  
  #TIERS LIST BASED ON VS META SCORE
  arch_meta_score=arch_ranked[c("ARCHETYPES","VS_META_SCORE")]
  arch_meta_score=arrange(arch_meta_score,VS_META_SCORE)
  head(arch_meta_score)
  mms=mean(arch_meta_score$VS_META_SCORE)
  sdms=sd(arch_meta_score$VS_META_SCORE)
  arch_meta_score_tiers2.5=arch_meta_score[arch_meta_score$VS_META_SCORE>mms+sdms,]
  arch_meta_score_tiers2=arch_meta_score[arch_meta_score$VS_META_SCORE>mms &
                                           arch_meta_score$VS_META_SCORE<=mms+sdms,]
  arch_meta_score_tiers1.5=arch_meta_score[arch_meta_score$VS_META_SCORE<=mms &
                                             arch_meta_score$VS_META_SCORE>=mms-sdms,]
  arch_meta_score_tiers1=arch_meta_score[arch_meta_score$VS_META_SCORE<mms-sdms,]
  arch_meta_score_tiers1
  arch_meta_score_tiers1.5
  arch_meta_score_tiers2
  arch_meta_score_tiers2.5
  
  #TIERS LIST BASED ON PRESENCE
  arch_presence_score=arch_ranked[c("ARCHETYPES","TOTAL_NB_MATCHES")]
  arch_presence_score=arrange(arch_presence_score,desc(TOTAL_NB_MATCHES))
  head(arch_presence_score)
  mps=mean(arch_presence_score$TOTAL_NB_MATCHES)
  sdps=sd(arch_presence_score$TOTAL_NB_MATCHES)
  arch_presence_score_tiers1=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES>mps+sdps,]
  arch_presence_score_tiers1.5=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES>mps &
                                                     arch_presence_score$TOTAL_NB_MATCHES<=mps+sdps,]
  arch_presence_score_tiers2=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES<=mps &
                                                   arch_presence_score$TOTAL_NB_MATCHES>=mps-sdps,]
  arch_presence_score_tiers2.5=arch_presence_score[arch_presence_score$TOTAL_NB_MATCHES<mps-sdps,]
  arch_presence_score_tiers1
  arch_presence_score_tiers1.5
  arch_presence_score_tiers2
  arch_presence_score_tiers2.5
  
  #TIERS LIST BASED ON AVERAGE WINRATE
  arch_winrate_score=arch_ranked[c("ARCHETYPES","WINRATE_AVERAGE")]
  arch_winrate_score=arrange(arch_winrate_score,desc(WINRATE_AVERAGE))
  head(arch_winrate_score)
  mws=mean(arch_winrate_score$WINRATE_AVERAGE)
  sdws=sd(arch_winrate_score$WINRATE_AVERAGE)
  arch_winrate_score_tiers1=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE>mws+sdws,]
  arch_winrate_score_tiers1.5=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE>mws &
                                                   arch_winrate_score$WINRATE_AVERAGE<=mws+sdws,]
  arch_winrate_score_tiers2=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE<=mws &
                                                 arch_winrate_score$WINRATE_AVERAGE>=mws-sdws,]
  arch_winrate_score_tiers2.5=arch_winrate_score[arch_winrate_score$WINRATE_AVERAGE<mws-sdws,]
  arch_winrate_score_tiers1
  arch_winrate_score_tiers1.5
  arch_winrate_score_tiers2
  arch_winrate_score_tiers2.5
  
  #TIERS LIST BASED ON LOWER ESTIMATION OF WINRATE
  arch_low_winrate_score=arch_ranked[c("ARCHETYPES","WINRATE_95_MIN")]
  arch_low_winrate_score=arrange(arch_low_winrate_score,desc(WINRATE_95_MIN))
  head(arch_low_winrate_score)
  mlws=mean(arch_low_winrate_score$WINRATE_95_MIN)
  sdlws=sd(arch_low_winrate_score$WINRATE_95_MIN)
  arch_low_winrate_score_tiers1=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN>mlws+sdlws,]
  arch_low_winrate_score_tiers1.5=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN>mlws &
                                                           arch_low_winrate_score$WINRATE_95_MIN<=mlws+sdlws,]
  arch_low_winrate_score_tiers2=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN<=mlws &
                                                         arch_low_winrate_score$WINRATE_95_MIN>=mlws-sdlws,]
  arch_low_winrate_score_tiers2.5=arch_low_winrate_score[arch_low_winrate_score$WINRATE_95_MIN<mlws-sdlws,]
  arch_low_winrate_score_tiers1
  arch_low_winrate_score_tiers1.5
  arch_low_winrate_score_tiers2
  arch_low_winrate_score_tiers2.5
  
  #EXPORT ALL THE TIERS LISTS TO CSV
  low_winrate_score_tiers_list=list(paste(arch_low_winrate_score_tiers1$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers2$ARCHETYPES,collapse=" - "),
                                    paste(arch_low_winrate_score_tiers2.5$ARCHETYPES,collapse=" - "))
  winrate_score_tiers_list=list(paste(arch_winrate_score_tiers1$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers2$ARCHETYPES,collapse=" - "),
                                paste(arch_winrate_score_tiers2.5$ARCHETYPES,collapse=" - "))
  presence_score_tiers_list=list(paste(arch_presence_score_tiers1$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers1.5$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers2$ARCHETYPES,collapse=" - "),
                                 paste(arch_presence_score_tiers2.5$ARCHETYPES,collapse=" - "))
  meta_score_tiers_list=list(paste(arch_meta_score_tiers1$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers1.5$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers2$ARCHETYPES,collapse=" - "),
                             paste(arch_meta_score_tiers2.5$ARCHETYPES,collapse=" - "))
  metric_score_tiers_list=list(paste(arch_metric_score_tiers1$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers1.5$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers2$ARCHETYPES,collapse=" - "),
                               paste(arch_metric_score_tiers2.5$ARCHETYPES,collapse=" - "))
  
  df_tiers_list=data.frame(presence=I(presence_score_tiers_list),
                           winrate=I(winrate_score_tiers_list),
                           low_winrate=I(low_winrate_score_tiers_list),
                           comb_presence_winrate=I(metric_score_tiers_list),
                           meta_score=I(metric_score_tiers_list)
  )
  
  rownames(df_tiers_list)=c("Tiers 1","Tiers 1.5","Tiers 2","Tiers 2.5")
  
  return(df_tiers_list)
}

#COUNT THE NUMBER OF TOP8 FOR EACH PLAYER
players_top8 = function(df,beginning,end) {
  
  #REMOVES ALL THE PLAYERS WHOSE RESULT 
  top8results = c("1st Place","2nd Place", "3rd Place", "4th Place", "5th Place", 
                  "6th Place", "7th Place", "8th Place")
  dftop8=df[df$RESULT %in% top8results,]
  top8players=generate_player_list(dftop8,beginning,end)
  top8players$NO_APPEARANCES=rep(1,length(top8players$PLAYERS))
  for (i in 1:length(top8players$NO_APPEARANCES)){
    play_identification=which(dftop8$PLAYER==top8players$PLAYERS[i])
    top8players_sub=dftop8[play_identification,]
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    top8players$NO_APPEARANCES[i]=length(play_identification)
    #LIST OF DIFFERENT ARCHETYPES THAT PLAYER PLAYED
    top8players$ARCHETYPE_NAMES[i]=list(unique(top8players_sub$ARCHETYPE))
    arch_count=c()
    for(j in 1:length(top8players$ARCHETYPE_NAMES[[i]])){
      arch_count[j]=length(top8players_sub[top8players_sub$ARCHETYPE==top8players$
                                 ARCHETYPE_NAMES[[i]][[j]],]$URL)
    }
    top8players$ARCHETYPE_COUNT[i]=list(arch_count)
    
    top8players$URL[i]=list(unique(top8players_sub$URL))
  }
  top8players=arrange(top8players,desc(NO_APPEARANCES))
  return(top8players)
}
