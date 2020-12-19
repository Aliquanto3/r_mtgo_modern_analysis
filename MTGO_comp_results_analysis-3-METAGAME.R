#3rd file to execute
#provides functions to determine metagame share and winrates, as well as graphs
#to display those results

#also imports all the libraries that can be useful here or in following files

#Execute this file only once, unless you directly edit it, it doesn't treat data
#only provides some functions to analyse it

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
library(ggplot2)
library(dplyr)
library(ggrepel)
library(jsonlite)
library(tidyverse)
library(data.table)
library(purrr)
library(jsonify)
library(plyr)

#VARIABLE FOR THE ACCURACY ON THE NAMING OF THE ARCHETYPES
#PROVIDES THE NAME OF THE COLUMN OF THE DATAFRAME TO BE USED
archetype_acc=NA
if(Classification=="Super"){
  archetype_acc="SUPER_ARCH"
}else if(Classification=="Exact"){
  archetype_acc="ARCHETYPE"
}

#LIST ALL THE DIFFERENT ARCHETYPES IN THE DATA
generate_archetype_list = function(df){
  #CREATE A DATAFRAME CONTAINING THE LIST OF ARCHETYPES
  arch_list=data.frame(unique(df[[archetype_acc]]))
  names(arch_list)[1] <- "ARCHETYPES"
  return(arch_list)
}

#COMPUTES THE SHARE OF EACH ARCHETYPE IN THE DATA
generate_metagame_data = function(df,graph_share){
  
  arch_list=generate_archetype_list(df)
  
  #ADD THE NUMBER OF COPIES FOR EACH ARCHETYPE IN THE DATA
  arch_list$NB_COPIES=rep(0,length(arch_list$ARCHETYPES))
  for (i in 1:length(arch_list$NB_COPIES)){
    arch_list$NB_COPIES[i]=length(which(df[[archetype_acc]]==arch_list$ARCHETYPES[i]))
  }
  
  #FOR EASIER READING OF THE GRAPHS, AGGREGATE ALL THE ARCHETYPES ACCOUNTING FOR 
  #LESS THAN graph_share% OF THE DATA
  graph_perc=graph_share/100*sum(arch_list$NB_COPIES)
  arch_list_vis=arch_list[arch_list$NB_COPIES >= graph_perc, ]
  
  #ADD AN "OTHER" CATEGORY CONTAINING THE SUM OF COPIES OF ALL ARCHETYPES UNDER X%
  sum_others=sum(arch_list[arch_list$NB_COPIES < graph_perc, ]$NB_COPIES)
  arch_list_vis=rbind(arch_list_vis,c("Other", sum_others))
  arch_list_vis=arch_list_vis[order(arch_list_vis$ARCHETYPES),]
  
  arch_list_vis$NB_COPIES=as.numeric(arch_list_vis$NB_COPIES)
  arch_list_vis$SHARE=as.numeric(format(round(arch_list_vis$NB_COPIES/
                                                sum(arch_list_vis$NB_COPIES)*100,
                                              1), nsmall = 1))
  
  arch_list_vis$ARCHETYPES = reorder(arch_list_vis$ARCHETYPES, as.numeric(arch_list_vis$NB_COPIES))
  
  return(arch_list_vis)
}

#COMPUTES A NAME FOR THE HISTOGRAM AND THE PIE CHART
generate_metagame_graph_title = function(){
  MetaGraphTitle=paste("Proportion of", Classification,"archetypes in MTGO", 
                       EventType,"between", Beginning, "and", End, sep = " ")
  return(MetaGraphTitle)
}

#GENERATE A PIE CHART BASED ON DATA IN DF
metagame_pie_chart = function(df){
  
  #CHANGE THE NUMBER FOR THE PROPORTION OF THE "OTHERS" CATEGORY HERE
  df_gen=generate_metagame_data(df,PieShare)
  
  ggplot(df_gen, aes(x="", SHARE, fill = ARCHETYPES)) + 
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(SHARE, "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = generate_metagame_graph_title()) + 
    guides(color = FALSE, size = FALSE) +
    scale_color_gradient(low="red", high="blue") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0, color = "#111111")) 
  
}

#GENERATE A BOX PLOT BASED ON DATA IN DF
metagame_box_plot = function(df){
  
  #GET THE DATA FOR ALL ARCHETYPES HAVING A META SHARE ABOVE HistShare
  df_gen=generate_metagame_data(df,HistShare)
  #THIS GRAPH DOESN'T DISPLAY THE "Other" CATEGORY
  df_gen=df_gen[df_gen$ARCHETYPES!="Other",]
  
  #reorder ARCHETYPES by ascending count
  df_gen$ARCHETYPES = reorder(df_gen$ARCHETYPES, as.numeric(df_gen$NB_COPIES))
  
  #plot is much clearer
  ggplot(df_gen, aes(x=ARCHETYPES, y=as.numeric(SHARE), fill=ARCHETYPES)) + 
    geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL, title = generate_metagame_graph_title()) + 
    scale_color_gradient(low="blue", high="red")+
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING WINRATES DEPENDING ON
#PRESENCE, WHICH IS HIGHLIGHTED BY THE DIAMETERS OF ANOTHER TYPE OF PRESENCE
#presence AND diameters CAN BE EITHER "Copies", "Players" or "Matches"
metric_graph = function(metric_df,presence,diameters) {
  
  #COMPUTES THE PARAMETERS OF THE LINES TO APPEAR ON THE GRAPH
  if (presence=="Copies"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE )/max(metric_df$NB_COPIES)
  }else if (presence=="Players"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE )/max(metric_df$NB_PLAYERS)
  }else if (presence=="Matches"){
    coeffdir=-max(metric_df$WINRATE_AVERAGE )/max(metric_df$TOTAL_NB_MATCHES)
  }
  average=mean(metric_df$WINRATE_AVERAGE )
  sdeviation=sd(metric_df$WINRATE_AVERAGE )
  
  #GENERATES THE LABELS
  if (presence=="Copies"){
    x_label="Total number of copies of each archetype"
  }else if (presence=="Players"){
    x_label="Total number of different players for each archetype"
  }else if (presence=="Matches"){
    x_label="Total number of matches played for each archetype"
  }
  y_label="Average winrate of each archetype"
  graph_title=paste("Winrates:", Classification,"archetypes ", "between", 
                    Beginning, "and", End, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Separated by mean + 4*n standard deviations (n={0,1,2,3,4,5}) 
  Circle diameters depending on",diameters,sep=" ")
  
  #GENERATES THE GRAPH
  if (presence=="Copies"){
    metric_plot=ggplot(metric_df, aes(NB_COPIES, WINRATE_AVERAGE ))
  }else if (presence=="Players"){
    metric_plot=ggplot(metric_df, aes(NB_PLAYERS, WINRATE_AVERAGE )) 
  }else if (presence=="Matches"){
    metric_plot=ggplot(metric_df, aes(TOTAL_NB_MATCHES, WINRATE_AVERAGE ))
  }
  
  if (diameters=="Copies"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_COPIES)
  }else if (diameters=="Players"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$NB_PLAYERS)
  }else if (diameters=="Matches"){
    metric_plot=metric_plot + 
      geom_point(aes(color = ARCHETYPES), size=metric_df$TOTAL_NB_MATCHES)
  }
  
  metric_plot=metric_plot + coord_cartesian() + theme_bw() + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_abline(intercept = average, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+4*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+8*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+12*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+16*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+20*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA)
  
  return(metric_plot)
  
}
#FILL IN METRIC POINTS IN AN ARCHETYPES DATA FRAME
metric_points_archetypes = function(df){
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df)
  
  for (i in 1:length(metric_df$ARCHETYPES)){
    #POSITION OF THE CORRESPONDING EXACT OR SUPER ARCHETYPE IN THE DATA
    arch_identification=which(df[[archetype_acc]]==metric_df$ARCHETYPES[i])
    #NUMBER OF APPEARANCES IN THE DATA OF THE CORRESPONDING ARCHETYPE
    metric_df$NB_COPIES[i]=length(arch_identification)
    #NUMBER OF DIFFERENT PLAYERS PLAYING THAT DECK
    metric_df$NB_PLAYERS[i]=length(unique(df[arch_identification,]$PLAYER))
    #NUMBER OF MATCHES PLAYED BY THAT ARCHETYPE IN THE DATA
    metric_df$TOTAL_NB_MATCHES[i]=sum(df[arch_identification,]$NB_ROUNDS,
                                      df[arch_identification,]$TOP8_MATCHES)
    #NUMBER OF WINS OF THAT ARCHETYPE
    total_wins_arch=sum((df$POINTS[arch_identification] + 
                       df$TOP8_PTS[arch_identification])/3)
    #NUMBER OF MATCHES OF THAT ARCHETYPE
    total_matches_arch=sum(df$NB_ROUNDS[arch_identification] + 
      df$TOP8_MATCHES[arch_identification])
    
    #95% CONFIDENCE INTERVALS OF THE WINRATE
    #EFFECTIVE WINRATE IN THE DATA
    metric_df$WINRATE_AVERAGE[i]=binom.test(total_wins_arch, total_matches_arch, p=0.5,
                                        alternative="two.sided", conf.level=0.95)$estimate
    #LOWER BORN OF THE "TRUE" WINRATE             
    metric_df$WINRATE_95_MIN[i]=binom.test(total_wins_arch, total_matches_arch, p=0.5,
                                       alternative="two.sided", conf.level=0.95)$conf.int[1]
    #UPPER BORN OF THE "TRUE" WINRATE 
    metric_df$WINRATE_95_MAX[i]=binom.test(total_wins_arch, total_matches_arch, p=0.5,
                                       alternative="two.sided", conf.level=0.95)$conf.int[2]
  }
  
  return(metric_df)
}

metric_df=metric_points_archetypes(df)

#COMBINES THE RATIOS OF POINTS PER ROUND AND NUMBER OF COPIES FOR EACH
#ARCHETYPE, THEN PROVIDES A RANK BASED ON THAT
#THE NEW METRIC OBTAINED THAT WAY IS NORMALIZED TO BE BETWEEN 0 AND 1
archetypes_ranking = function(metric_df){
  
  metric_df$COMB_PPR=metric_df$WINRATE_AVERAGE
  for (i in 1:length(metric_df$COMB_PPR)){
    metric_df$COMB_PPR[i] = 
      (Presence_Weight * metric_df$NB_COPIES[i] /
      max(metric_df$NB_COPIES) +
      PPR_Weight * metric_df$WINRATE_AVERAGE [i] /
      max(metric_df$WINRATE_AVERAGE ))/(Presence_Weight+PPR_Weight)
  }
  
  metric_df = metric_df[order(-metric_df$COMB_PPR),]
  
  metric_df$RANK=metric_df$COMB_PPR
  for (i in 1:length(metric_df$RANK)){
    metric_df$RANK[i]=i
  }
  
  return(metric_df)
}

arch_ranked=archetypes_ranking(metric_df)
