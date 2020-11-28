#DATA SHARED BY PHELPS-SAN @ TRON DISCORD, HIS WORK CAN BE FOUND HERE:
#https://github.com/Badaro/MTGODecklistCache
#https://github.com/Badaro/MTGOArchetypeParser

#LIBRARIES
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)

#IMPORT DATA
setwd(DirectoryFile)
rawData=read.csv(RawFile,sep=",",header=T)
rawData=subset(rawData,select = -12)

#NAMES AND DATE DON'T ALLOW THE IDENTIFICATION OF AN EVENT, BUT THE COMBINATION
#OF BOTH CAN, HENCE THE ADDITION OF ANOTHER COLUMN FOR THIS IDENTIFICATION
event_names=rep(NA,length(rawData$EVENT))
for (i in 1:length(rawData$EVENT)){
  event_names[i]=paste(rawData$EVENT[i],as.character(rawData$DATE[i]),sep=" ")
}
rawData$EVENT_NAME=event_names
#View(rawData) 

if(is.na(Beginning)){
  Beginning=min(rawData$DATE)
}
if(is.na(End)){
  End=max(rawData$DATE)
}

#SELECT DATA FOR A SPECIFIC PERIOD
rawData$DATE <- as.Date(rawData$DATE)
rawData$POINTS <- as.numeric(rawData$POINTS)
periodData=rawData
periodData=subset(rawData, DATE > as.Date(Beginning) & DATE < as.Date(End))

#View(periodData)
#length(periodData$PLAYER)

generate_Prelim_Data = function() {
  
  #COLLECT PRELIMINARIES ONLY FOR SPECIFIC TREATMENT
  PrelimData=periodData[grep("Preliminary", periodData$EVENT), ]
  #View(PrelimData) 
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE PRELIMINARIES - ALWAYS 5
  nbRoundsVecPrelim=rep(5,length(PrelimData$EVENT))
  PrelimData$NB_ROUNDS=nbRoundsVecPrelim
  #View(PrelimData)
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN PRELIMINARIES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  PrelimData$NB_DEFEATS=PrelimData$NB_ROUNDS - PrelimData$POINTS/3
  
  if(EventType=="Competitions"){
    #ADD TOP8 COLUMNS FOR MERGE WITH CHALLENGES
    PrelimData$TOP8_PTS=PrelimData$POINTS
    PrelimData$TOP8_DEF=PrelimData$NB_DEFEATS
  }
  
  return(PrelimData)
  
}

generate_Challenge_Data = function() {
  
  #COLLECT CHALLENGES ONLY FOR SPECIFIC TREATMENT
  ChallData=periodData[grep("Challenge", periodData$EVENT), ]
  #View(ChallData)                                   
  
  #CALCULATE THE NUMBER OF ROUNDS IN EACH EVENT FOR THE CHALLENGES
  #DIVIDE THE MAXIMUM NUMBER OF POINTS IN SWISS TO GET THE RESULT
  #IF MORE THAN 1 PLAYER HAS THE MAXIMUM OF POINTS, THEN IT IS LIKELY THAT 
  #THERE IS NOT ANY PLAYER AT X-0, SO YOU ADD 1 TO THE RESULT
  listEventsChall=unique(ChallData$EVENT_NAME)
  nbRoundsVec=c()
  for (i in 1:length(listEventsChall)){
    periodChallEventData=subset(ChallData, EVENT_NAME == listEventsChall[i])
    maxPoints=max(periodChallEventData$POINTS)
    nbPlayMaxPts=length(which(periodChallEventData$POINTS == maxPoints))
    if(nbPlayMaxPts==1){
      nbRounds=maxPoints/3
    }else{
      nbRounds=1+maxPoints/3
    }
    nbRoundsEvent=rep(nbRounds,length(periodChallEventData$EVENT))
    nbRoundsVec=c(nbRoundsVec,nbRoundsEvent)
  }
  ChallData$NB_ROUNDS=nbRoundsVec
  #View(ChallData)
  
  #CALCULATE THE NUMBER OF DEFEAT OF EACH DECK IN CHALLENGES - 
  #NUMBER OF ROUNDS MINUS THE NUMBER OF POINTS/3 (3 PTS EARNED BY WIN) 
  ChallData$NB_DEFEATS=ChallData$NB_ROUNDS - ChallData$POINTS/3
  
  #ADD TOP8 POINTS: 3*3 to 1st, 3*2 to 2nd, 3*1 to 3rd and 4th, none to others
  ChallData$TOP8_PTS=ChallData$POINTS
  for (i in 1:length(ChallData$RESULT)){
    if (ChallData$RESULT[i] == "1st Place"){
      ChallData$TOP8_PTS[i] = 9 + ChallData$TOP8_PTS[i]
    }else if (ChallData$RESULT[i] == "2nd Place"){
      ChallData$TOP8_PTS[i] = 6 + ChallData$TOP8_PTS[i]
    }else if (ChallData$RESULT[i] == "3rd Place" | ChallData$RESULT[i] == "4th Place"){
      ChallData$TOP8_PTS[i] = 3 + ChallData$TOP8_PTS[i]
    }
  }
  #View(ChallData)
  
  #ADD TOP8 DEFEATS: 0 FOR THE WINNER, 1 FOR THE OTHERS
  ChallData$TOP8_DEF=ChallData$NB_DEFEATS
  for (i in 1:length(ChallData$TOP8_DEF)){
    if (ChallData$RESULT[i] == "2nd Place" | ChallData$RESULT[i] == "3rd Place" | 
        ChallData$RESULT[i] == "4th Place" | ChallData$RESULT[i] == "5th Place"| 
        ChallData$RESULT[i] == "6th Place" | ChallData$RESULT[i] == "7th Place"| 
        ChallData$RESULT[i] == "8th Place"){
      ChallData$TOP8_DEF[i] = 1 + ChallData$NB_DEFEATS[i]
    }
  }
  
  #REMOVE ALL THE RESULTS WITH 3 DEFEATS (BECAUSE WE ONLY HAVE PART OF THEM, 
  #WHEREAS WE HAVE ALL THE X-0, X-1 AND X-2 RESULTS)
  ChallDataWO3Def=ChallData[ChallData$NB_DEFEATS != 3, ]
  #View(ChallDataWO3Def)
  
  return(ChallDataWO3Def)
  
}

#Enter here the name of the dataframe you want to use
df=rawData

if (EventType=="Competitions"){
  #FUSE THE DATA BACK TO GET ALL THE COMPETITIVE RESULTS IN THE SAME FILE
  df=rbind(generate_Challenge_Data(),generate_Prelim_Data())
}else if (EventType=="Challenges"){
  df=generate_Challenge_Data()
}else if (EventType=="Preliminaries"){
  df=generate_Prelim_Data()
}
#View(df)

add_super_archetypes = function(df){
  
  for (i in 1:length(df$SUPER_ARCH)){
    if(df$ARCHETYPE[i]=="WURG Control" | 
       df$ARCHETYPE[i]=="Bant Midrange"| 
       df$ARCHETYPE[i]=="Scapeshift"| 
       df$ARCHETYPE[i]=="UBRG Control"| 
       df$ARCHETYPE[i]=="Niv To Light"| 
       df$ARCHETYPE[i]=="Omnath Saheeli"| 
       df$ARCHETYPE[i]=="Bant Blink"| 
       df$ARCHETYPE[i]=="Temur Control"| 
       df$ARCHETYPE[i]=="Sultai Control"| 
       df$ARCHETYPE[i]=="Bant Control"){
      
      df$SUPER_ARCH[i]="UGx Control"
      
    }
    
    if(df$ARCHETYPE[i]=="Jund Prowess" | 
       df$ARCHETYPE[i]=="Izzet Prowess"| 
       df$ARCHETYPE[i]=="Obosh Aggro"| 
       df$ARCHETYPE[i]=="Mono Red Prowess"| 
       df$ARCHETYPE[i]=="Rakdos Prowess"|  
       df$ARCHETYPE[i]=="Naya Prowess"){
      
      df$SUPER_ARCH[i]="Rx Prowess"
      
    }
    
    if(df$ARCHETYPE[i]=="Jeskai Control" | 
       df$ARCHETYPE[i]=="Izzet Control"| 
       df$ARCHETYPE[i]=="Dimir Control"| 
       df$ARCHETYPE[i]=="Azorius Control"|
       df$ARCHETYPE[i]=="Esper Control"| 
       df$ARCHETYPE[i]=="Grixis Control"|
       df$ARCHETYPE[i]=="UW Miracles"|
       df$ARCHETYPE[i]=="UR Kiki Boilproof"){
      
      df$SUPER_ARCH[i]="Non UGx Control"
      
    }
    
    if(df$ARCHETYPE[i]=="Amulet Titan" | 
       df$ARCHETYPE[i]=="KGC Amulet Titan"| 
       df$ARCHETYPE[i]=="Primeval Titan"| 
       df$ARCHETYPE[i]=="Reclaimer Titan"){
      
      df$SUPER_ARCH[i]="P.Titan"
      
    }
    
    if(df$ARCHETYPE[i]=="UBRG Shadow" | 
       df$ARCHETYPE[i]=="Grixis Shadow"|
       df$ARCHETYPE[i]=="Shadow Prowess"| 
       df$ARCHETYPE[i]=="Jund Shadow"|
       df$ARCHETYPE[i]=="RB Shadow Lurrus"|
       df$ARCHETYPE[i]=="Mardu Shadow Lurrus"){
      
      df$SUPER_ARCH[i]="DS"
      
    }
    
    if(df$ARCHETYPE[i]=="Azorius Taxes" | 
       df$ARCHETYPE[i]=="Mono White Taxes"| 
       df$ARCHETYPE[i]=="Selenya Taxes"|
       df$ARCHETYPE[i]=="BW Eldrazi & Taxes"){
      
      df$SUPER_ARCH[i]="D&T"
      
    }
    
    if(df$ARCHETYPE[i]=="Abzan Company" | 
       df$ARCHETYPE[i]=="Selenya Midrange"){
      
      df$SUPER_ARCH[i]="GWx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Mardu Midrange" | 
       df$ARCHETYPE[i]=="Rakdos Midrange"){
      
      df$SUPER_ARCH[i]="RBx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Bant Spirits" | 
       df$ARCHETYPE[i]=="Spirits"){
      
      df$SUPER_ARCH[i]="Spirits"
      
    }
    
    if(df$ARCHETYPE[i]=="Belcher"|
       df$ARCHETYPE[i]=="UW Belcher"|
       df$ARCHETYPE[i]=="RG Belcher" | 
       df$ARCHETYPE[i]=="Oops All Spells"){
      
      df$SUPER_ARCH[i]="All Spells"
      
    }
    
    if(df$ARCHETYPE[i]=="Gifts Storm"){
      
      df$SUPER_ARCH[i]="Storm"
      
    }
    
    if(df$ARCHETYPE[i]=="Izzet Restore Balance" |
       df$ARCHETYPE[i]=="Temur Foretold Balance" |
       df$ARCHETYPE[i]=="Izzet Living End"){
      
      df$SUPER_ARCH[i]="URx Balance"
      
    }
    
    if(df$ARCHETYPE[i]=="Slivers"){
      
      df$SUPER_ARCH[i]="Slivers"
      
    }
    
    if(df$ARCHETYPE[i]=="E Tron"){
      
      df$SUPER_ARCH[i]="Eldrazi"
      
    }
    
    if(df$ARCHETYPE[i]=="U Tron" |
       df$ARCHETYPE[i]=="Dice Factory Tron"){
      
      df$SUPER_ARCH[i]="Other Tron"
      
    }
    
    if(df$ARCHETYPE[i]=="Elementals"){
      
      df$SUPER_ARCH[i]="Elementals"
      
    }
    
    if(df$ARCHETYPE[i]=="Humans"){
      
      df$SUPER_ARCH[i]="Humans"
      
    }
    
    if(df$ARCHETYPE[i]=="Merfolks"){
      
      df$SUPER_ARCH[i]="Merfolks"
      
    }
    
    if(df$ARCHETYPE[i]=="Heliod Combo" |
       df$ARCHETYPE[i]=="GW Heliod"|
       df$ARCHETYPE[i]=="Mono White Heliod"){
      
      df$SUPER_ARCH[i]="Heliod"
      
    }
    
    if(df$ARCHETYPE[i]=="Jund Midrange" |
       df$ARCHETYPE[i]=="Jund Lurrus Midrange"|
       df$ARCHETYPE[i]=="Sultai Midrange"|
       df$ARCHETYPE[i]=="Abzan Midrange"){
      
      df$SUPER_ARCH[i]="BGx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Gruul Midrange" |
       df$ARCHETYPE[i]=="Naya Midrange"){
      
      df$SUPER_ARCH[i]="RGx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Dredge"){
      
      df$SUPER_ARCH[i]="Dredge"
      
    }
    
    if(df$ARCHETYPE[i]=="Burn" |
       df$ARCHETYPE[i]=="RW Burn"|
       df$ARCHETYPE[i]=="RG Burn"|
       df$ARCHETYPE[i]=="RB Burn"){
      
      df$SUPER_ARCH[i]="Rx Burn"
      
    }
    
    if(df$ARCHETYPE[i]=="Crabvine"){
      
      df$SUPER_ARCH[i]="Crabvine"
      
    }
    
    if(df$ARCHETYPE[i]=="Orzhov Midrange"){
      
      df$SUPER_ARCH[i]="BWx Midrange"
      
    }
    
    if(df$ARCHETYPE[i]=="Thopter Urza" |
       df$ARCHETYPE[i]=="Grixis Whirza"){
      
      df$SUPER_ARCH[i]="Urza"
      
    }
    
    if(df$ARCHETYPE[i]=="Bant Stoneblade" |
       df$ARCHETYPE[i]=="UW Stoneblade"){
      
      df$SUPER_ARCH[i]="UWx Stoneblade"
      
    }
    
    if(df$ARCHETYPE[i]=="Infect"){
      
      df$SUPER_ARCH[i]="Infect"
      
    }
    
    if(df$ARCHETYPE[i]=="UB Inverter"){
      
      df$SUPER_ARCH[i]="Inverter"
      
    }
    
    if(df$ARCHETYPE[i]=="MonoG Tron" |
       df$ARCHETYPE[i]=="KGC Tron"){
      
      df$SUPER_ARCH[i]="Gx Tron"
      
    }
    
    if(df$ARCHETYPE[i]=="Bogles"){
      
      df$SUPER_ARCH[i]="Bogles"
      
    }
    
    if(df$ARCHETYPE[i]=="Devoted"|
       df$ARCHETYPE[i]=="GW Devoted Lurrus"){
      
      df$SUPER_ARCH[i]="Devoted"
      
    }
    
    if(df$ARCHETYPE[i]=="Ad Nauseam"){
      
      df$SUPER_ARCH[i]="Ad Nauseam"
      
    }
    
    if(df$ARCHETYPE[i]=="Rogues"){
      
      df$SUPER_ARCH[i]="Rogues"
      
    }
    
    if(df$ARCHETYPE[i]=="Mill" |
       df$ARCHETYPE[i]=="UB Mill"){
      
      df$SUPER_ARCH[i]="Mill"
      
    }
    
    if(df$ARCHETYPE[i]=="Hammer Time"){
      
      df$SUPER_ARCH[i]="Hammer Time"
      
    }
    
    if(df$ARCHETYPE[i]=="Enduring Ideal"){
      
      df$SUPER_ARCH[i]="Enduring Ideal"
      
    }
    
    if(df$ARCHETYPE[i]=="Polymorph"){
      
      df$SUPER_ARCH[i]="Polymorph"
      
    }
    
    if(df$ARCHETYPE[i]=="Red Prison" | 
       df$ARCHETYPE[i]=="Boros Land Destruction"){
      
      df$SUPER_ARCH[i]="Red Prison"
      
    }
    
    if(df$ARCHETYPE[i]=="Kiki Chord"){
      
      df$SUPER_ARCH[i]="Kiki Chord"
      
    }
  }
  
  return(df)
  
}

#/!\ to be updated when you change data, at least check if there isn't any new archetype
#ADD SUPER ARCHETYPES DEPENDING ON EXACT ARCHETYPE

df$SUPER_ARCH=df$ARCHETYPE
length(unique(df$SUPER_ARCH))
unique(df$SUPER_ARCH)

#TO SEE WHICH DECKLISTS CORRESPONDS TO A LABEL, FOR INSTANCE "Bant Midrange"
#df[grep("Bant Midrange", df$ARCHETYPE), ]$URL

df=add_super_archetypes(df)
#THE CODE DOES NOT CHECK WHETHER WE WANT TO USE SUPER ARCHETYPES HERE, BECAUSE 
#IT IS STILL VERY FAST TO ADD THAT COLUMN, AND IT IS EASIER FOR USE WHEN YOU 
#CHANGE THE TYPE OF ARCHETYPES YOU WANT TO USE IN THE CONSOLE ONCE YOU EXECUTED
#THE ENTIRE CODE ONCE TO GENERATE VARIOUS GRAPHS

#TO SEE WHICH EXACT ARCHETYPES ARE CONTAINED IN A SUPER ARCHETYPE, for instance 
#"UGx Control"
#unique(df[grep("UGx Control", df$SUPER_ARCH), ]$ARCHETYPE)

#WE CAN START DISPLAYING THE REPARTITION OF THE ARCHETYPES IN THE DATA

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
    scale_color_gradient(low="blue", high="red")
  
}

#NOW THAT THE DATA IS READY AND WE KNOW WHAT IT CONTAINS, WE CAN START WORKING 
# ON METRICS TO DETERMINE WHICH ARCHETYPES APPEAR TO BE THE BEST PERFORMERS OVERALL
#WE WANT METRICS THAT TAKE INTO ACCOUNT THE NUMBER OF APPEARANCES OF EACH DECK,
#AND THEIR WIN/DEFEAT RATIO ON EVENTS OF VARIOUS SIZES (AND CONSIDER THE SIZES TOO)

#PROVIDE A GRAPH FOR A METRIC DATAFRAME DISPLAYING AVERAGE POINTS DEPENDING ON
#TOTAL POINTS, THE NUMBER OF COPIES OF EACH ARCHETYPE IS SHOWN BY THE DIAMETERS
metric_graph = function(metric_df,metric_name) {
  
  #COMPUTES THE PARAMETERS OF THE LINES TO APPEAR ON THE GRAPH
  coeffdir=-max(metric_df$METRIC_AVERAGE)/max(metric_df$METRIC_POINTS)
  average=mean(metric_df$METRIC_AVERAGE)
  sdeviation=sd(metric_df$METRIC_AVERAGE)
  
  #GENERATES THE LABELS
  MetaGraphTitle=paste("Proportion of", Classification,"archetypes in MTGO", 
                       EventType,"between", Beginning, "and", End, sep = " ")
  x_label=paste("Number of points acquired in", EventType, "by each archetype", 
                sep = " ")
  y_label=paste("Number of points divided by the number of copies of each 
                archetype", sep = " ")
  graph_title=paste(metric_name,":", Classification,"archetypes ", "between", 
                    Beginning, "and", End, sep = " ")
  graph_subtitle="Separated by mean + 2*n standard deviation (n=0,1,2,3,4)"
  
  
  #DISPLAY THE GRAPH
  ggplot(metric_df, aes(METRIC_POINTS, METRIC_AVERAGE)) + 
    geom_point(aes(color = ARCHETYPES), size=metric_df$NB_COPIES) +
    coord_cartesian() + theme_bw() + 
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) +
    geom_text(aes(label=ARCHETYPES),hjust=0, vjust=0) + 
    geom_abline(intercept = average, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+2*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+4*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+6*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5) + 
    geom_abline(intercept = average+8*sdeviation, slope = coeffdir, 
                color="red", linetype="dashed", size=1.5)
}


#WE START OFF WITH A METRIC THAT GIVES A SPECIFIC AMOUNT OF POINT DEPENDING ON
#THE NUMBER OF DEFEATS IN SWISS, THEN MULTIPLY THAT SCORE BY THE NUMBER OF ROUNDS
m_defeat_weight = function(df){
  
  #CREATE A COLUMN FOR THE METRIC
  df$DEFEAT_WEIGHT=rep(0,length(df$NB_DEFEATS))
  
  #FILL THE COLUMN WITH THE SUITED POINTS
  for (i in 1:length(df$NB_DEFEATS)){
    if (df$NB_DEFEATS[i]==0){
      df$METRIC_POINTS[i]=X_0_PTS
      df$METRIC_POINTS[i]=df$METRIC_POINTS[i]*df$NB_ROUNDS[i]
    }else if (df$NB_DEFEATS[i]==1){
      df$METRIC_POINTS[i]=X_1_PTS
      df$METRIC_POINTS[i]=df$METRIC_POINTS[i]*df$NB_ROUNDS[i]
    }else if (df$NB_DEFEATS[i]==2){
      df$METRIC_POINTS[i]=X_2_PTS
      df$METRIC_POINTS[i]=df$METRIC_POINTS[i]*df$NB_ROUNDS[i]
    }
  }
  
  #GET THE LIST OF THE DIFFERENT ARCHETYPES IN THE DATA
  metric_df=generate_archetype_list(df)
  
  #ADD THE NUMBER OF POINTS FOR EACH ARCHETYPE IN THE DATA
  metric_df$METRIC_POINTS=rep(0,length(metric_df$ARCHETYPES))
  for (i in 1:length(metric_df$METRIC_POINTS)){
    metric_df$METRIC_POINTS[i]=
      sum(df$METRIC_POINTS[which(df[[archetype_acc]]==metric_df$ARCHETYPES[i])])
  }
  
  #ADD THE NUMBER OF COPIES FOR EACH ARCHETYPE IN THE DATA FOR THE AVERAGE POINTS
  metric_df$NB_COPIES=rep(0,length(metric_df$ARCHETYPES))
  metric_df$METRIC_AVERAGE=rep(0,length(metric_df$ARCHETYPES))
  for (i in 1:length(metric_df$NB_COPIES)){
    metric_df$NB_COPIES[i]=
      length(which(df[[archetype_acc]]==metric_df$ARCHETYPES[i]))
    metric_df$METRIC_AVERAGE[i]=metric_df$METRIC_POINTS[i]/metric_df$NB_COPIES[i]
  }
  
  metric_graph(metric_df, "Defeat Weight")
  
}

#m_defeat_weight(df)

#
m_swiss_wins = function(df){
  
}

#
m_top8_swiss_wins = function(df){
  
}


