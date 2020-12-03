#Execute this file only once, unless you directly edit it

#LIBRARIES
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(ggrepel)

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
  
  #FOR EASIER READ OF THE GRAPHS, AGGREGATE ALL THE ARCHETYPES ACCOUNTING FOR 
  #LESS THAN graph_share% OF THE DATA
  graph_perc=graph_share/100*sum(arch_list$NB_COPIES)
  arch_list_vis=arch_list[arch_list$NB_COPIES >= graph_perc, ]
  
  #ADD AN "OTHER" CATEGORY CONTAINING THE SUM OF COPIES OF ALL ARCHETYPES UNDER X%
  sum_others=sum(arch_list[arch_list$NB_COPIES < graph_perc, ]$NB_COPIES)
  arch_list_vis=rbind(arch_list_vis,c("Other", sum_others))
  arch_list_vis=arch_list_vis[order(arch_list_vis$ARCHETYPES),]
  
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
  
  df_gen$ARCHETYPES <- reorder(df_gen$ARCHETYPES, as.numeric(df_gen$NB_COPIES))
  df_gen <- df_gen %>%
    group_by(ARCHETYPES) %>%
    summarise(copies = sum(as.numeric(NB_COPIES)), .groups="drop") %>%
    mutate(share=copies/sum(copies)*100.0) %>%
    arrange(desc(copies))
  
  ggplot(df_gen, aes("", share, fill = ARCHETYPES)) +
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(share), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = generate_metagame_graph_title()) + 
    guides(color = FALSE, size = FALSE) +
    scale_color_gradient(low="red", high="green") +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#666666"))
  
}

#GENERATE A BOX PLOT BASED ON DATA IN DF
metagame_box_plot = function(df){
  
  #CHANGE THE NUMBER FOR THE PROPORTION OF THE "OTHERS" CATEGORY HERE
  df_gen=generate_metagame_data(df,HistShare)
  df_gen=df_gen[df_gen$ARCHETYPES!="Other",]
  
  #reorder ARCHETYPES by ascending count
  df_gen$ARCHETYPES <- reorder(df_gen$ARCHETYPES, as.numeric(df_gen$NB_COPIES))
  
  #plot is much more clear
  ggplot(df_gen, aes(x=ARCHETYPES, y=as.numeric(NB_COPIES), fill=ARCHETYPES)) + 
    geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
    labs(x = NULL, y = NULL, fill = NULL, title = generate_metagame_graph_title()) + 
    scale_color_gradient(low="blue", high="red")+
    scale_x_discrete(guide = guide_axis(n.dodge=2))
  
}

#NOW THAT THE DATA IS READY AND WE KNOW WHAT IT CONTAINS, WE CAN START WORKING 
# ON METRICS TO DETERMINE WHICH ARCHETYPES APPEAR TO BE THE BEST PERFORMERS OVERALL
#WE WANT METRICS THAT TAKE INTO ACCOUNT THE NUMBER OF APPEARANCES OF EACH DECK,
#AND THEIR WIN/DEFEAT RATIO ON EVENTS OF VARIOUS SIZES (AND CONSIDER THE SIZES TOO)

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING AVERAGE POINTS DEPENDING ON
#TOTAL POINTS, THE NUMBER OF COPIES OF EACH ARCHETYPE IS SHOWN BY THE DIAMETERS
metric_graph = function(metric_df,metric_name) {
  
  #COMPUTES THE PARAMETERS OF THE LINES TO APPEAR ON THE GRAPH
  coeffdir=-max(metric_df$PPR_AVERAGE)/max(metric_df$PPR)
  average=mean(metric_df$PPR_AVERAGE)
  sdeviation=sd(metric_df$PPR_AVERAGE)
  
  #GENERATES THE LABELS
  x_label="Total number of copies of each archetype"
  y_label="Average number of points per round of each archetype"
  graph_title=paste(metric_name,":", Classification,"archetypes ", "between", 
                    Beginning, "and", End, "in MTGO", EventType,sep = " ")
  graph_subtitle="Separated by mean + 2*n standard deviations (n={0,1,2,3,4,5})"
  
  #DISPLAY THE GRAPH
  metric_plot=ggplot(metric_df, aes(NB_COPIES, PPR_AVERAGE)) + 
    geom_point(aes(color = ARCHETYPES), size=metric_df$NB_COPIES) +
    coord_cartesian() + theme_bw() + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_abline(intercept = average, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+2*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+4*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+6*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+8*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+10*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA)
  
  return(metric_plot)
  
}

#FILL IN METRIC POINTS IN AN ARCHETYPES DATA FRAME
metric_points_archetypes = function(df){
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df)
  
  #ADD THE NUMBER OF POINTS FOR EACH ARCHETYPE IN THE DATA
  metric_df$PPR=rep(0,length(metric_df$ARCHETYPES))
  for (i in 1:length(metric_df$PPR)){
    metric_df$PPR[i]=
      sum(df$PPR[which(df[[archetype_acc]]==metric_df$ARCHETYPES[i])])
  }
  
  #ADD THE NUMBER OF COPIES FOR EACH ARCHETYPE IN THE DATA FOR THE AVERAGE POINTS
  metric_df$NB_COPIES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$PPR_AVERAGE=rep(0,length(metric_df$ARCHETYPES))
  for (i in 1:length(metric_df$NB_COPIES)){
    metric_df$NB_COPIES[i]=
      length(which(df[[archetype_acc]]==metric_df$ARCHETYPES[i]))
    metric_df$PPR_AVERAGE[i]=metric_df$PPR[i]/metric_df$NB_COPIES[i]
  }
  
  return(metric_df)
}

#COMPUTE THE NUMBER OF POINTS PER ROUND FOR EACH ARCHETYPE
points_per_round = function(df) {
  #CREATE A COLUMN FOR THE METRIC
  df$PPR=rep(0,length(df$NB_ROUNDS))
  
  #FILL THE COLUMN WITH THE SUITED POINTS
  for (i in 1:length(df$PPR)){
    df$PPR[i]=(df$POINTS[i]+df$TOP8_PTS[i])/
      (df$NB_ROUNDS[i]+df$TOP8_MATCHES[i])
  }
  
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  ppr_df=metric_points_archetypes(df)
  
  return(ppr_df)
}

#COMBINES THE RATIOS OF POINTS PER ROUND AND TOTAL POINTS PER ROUND FOR EACH
#ARCHETYPE, THEN PROVIDES A RANK BASED ON THAT
archetypes_ranking = function(ppr_df){
  
  ppr_df$COMB_PPR=ppr_df$PPR
  for (i in 1:length(ppr_df$COMB_PPR)){
    ppr_df$COMB_PPR[i] = 
      Presence_Weight * ppr_df$NB_COPIES[i] /
      max(ppr_df$NB_COPIES) +
      PPR_Weight * ppr_df$PPR_AVERAGE[i] /
      max(ppr_df$PPR_AVERAGE) 
  }
  
  ppr_df = ppr_df[order(-ppr_df$COMB_PPR),]
  
  ppr_df$RANK=ppr_df$COMB_PPR
  for (i in 1:length(ppr_df$RANK)){
    ppr_df$RANK[i]=i
  }
  
  return(ppr_df)
}
