#6th file to execute
#Selects a specific archetype in the data for which we try to optimize the 
#decklist through an analysis of the number of different cards

#TO BE MANUALLY CHOSEN AFTER ANALYSIS OF ALL THE METRICS
#KEEP ONLY THE DATA OF A SINGLE ARCHETYPE
#bestArchetype="Urza Oko"

#ADD THE NUMBER OF CARDS HAVING AT LEAST A SPECIFIC TYPE OF EACH DECK
#USE THE FOLLOWING COMMAND TO FIND ALL THE TYPES YOU CAN USE:
#unique(cardDataSub$types)
addRatioType = function(df,type){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowNameType=paste("Ratio",type,sep="")
  #CREATE THE COLUMN
  df[[rowNameType]]=rep(0,nrow(df))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:nrow(df)){
    landcount=0
    for (j in 1:length(df$Mainboard[[i]]$CardName)){
      if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$name){
        if((length(grep(type,cardDataSub[cardDataSub$name==df$Mainboard[[i]]$CardName[j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$name==df$Mainboard[[i]]$CardName[j],]$types)==1)){
          landcount=landcount+df$Mainboard[[i]]$Count[j]
        }
      }else if (df$Mainboard[[i]]$CardName[j] %in% cardDataSub$faceName){
        if((length(grep(type,cardDataSub[cardDataSub$faceName==df$Mainboard[[i]]$CardName[j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$faceName==df$Mainboard[[i]]$CardName[j],]$types)==1)){
          landcount=landcount+df$Mainboard[[i]]$Count[j]
        }
      }
    }
    df[[rowNameType]][i]=landcount/df$TotalMD[i]
  }
  return(df)
}

infos_best_arch=function(df,bestArchetype){
  df_best_arch=df[df$ARCHETYPE==bestArchetype,]
  
  #ADD THE NUMBER OF MAINDECK CARDS FOR EACH DECK - DEFAULT IS 60
  df_best_arch$TOTAL_MD=rep(60,length(df_best_arch$URL))
  for (i in 1:length(df_best_arch$TOTAL_MD)){
    df_best_arch$TOTAL_MD[i]=sum(df_best_arch$MDCounts[[i]])
  }
  
  #ADD THE WINRATE OF EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    #NUMBER OF WINS OF EACH DECK
    total_wins_deck=(df_best_arch$POINTS[i] + df_best_arch$TOP8_PTS[i])/3
    #NUMBER OF MATCHES OF EACH DECK
    total_matches_deck=df_best_arch$NB_ROUNDS[i] + df_best_arch$TOP8_MATCHES[i]
    #WINRATE OF EACH DECK
    df_best_arch$WINRATE=total_wins_deck/total_matches_deck
  }
  #IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
  df_best_arch=addRatioType(df_best_arch,"Land")
  df_best_arch=addRatioType(df_best_arch,"Creature")
  df_best_arch=addRatioType(df_best_arch,"Planeswalker")
  df_best_arch=addRatioType(df_best_arch,"Instant")
  df_best_arch=addRatioType(df_best_arch,"Sorcery")
  df_best_arch=addRatioType(df_best_arch,"Artifact")
  df_best_arch=addRatioType(df_best_arch,"Enchantment")
  
  sbCards=unique(unlist(df_best_arch$SBCards))
  for (i in 1:length(sbCards)){
    df_best_arch=isPresentSBCard(df_best_arch,sbCards[i])
  }
  
  mdCards=unique(unlist(df_best_arch$MDCards))
  for (i in 1:length(mdCards)){
    df_best_arch=isPresentMDCard(df_best_arch,mdCards[i])
  }
  
  return(df_best_arch)
}

#names(df_best_arch)

#unique(unlist(df_best_arch$SBCards))
#table(unlist(df_best_arch$SBCards))
#length(df_best_arch$URL)

#ADD A VARIABLE TELLING WHETHER cardName IS IN THE SB (1) OR NOT (0)
isPresentSBCard = function(df_best_arch,cardName){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowName=gsub('[[:punct:] ]+',' ',cardName)
  rowName=chartr(" ", "_", rowName)
  rowNameCard=toupper(paste("SB_PRESENCE",rowName,sep="_"))
  
  #CREATE THE COLUMN
  df_best_arch[[rowNameCard]]=rep(0,length(df_best_arch$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    if(cardName %in% df_best_arch$SBCards[[i]]){
      df_best_arch[[rowNameCard]][i]=1
    }
  }
  return(df_best_arch)
}

#ADD A VARIABLE TELLING WHETHER cardName IS IN THE MD (1) OR NOT (0)
isPresentMDCard = function(df_best_arch,cardName){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowName=gsub('[[:punct:] ]+',' ',cardName)
  rowName=chartr(" ", "_", rowName)
  rowNameCard=toupper(paste("MD_PRESENCE",rowName,sep="_"))
  
  #CREATE THE COLUMN
  df_best_arch[[rowNameCard]]=rep(0,length(df_best_arch$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df_best_arch$URL)){
    if(cardName %in% df_best_arch$MDCards[[i]]){
      df_best_arch[[rowNameCard]][i]=1
    }
  }
  return(df_best_arch)
}

#COMPUTES THE AVERAGE CHARACTERISTICS OF THE ARCHETYPE
#AVERAGE RATIO OF EACH TYPE OF CARD, AVERAGE NUMBER OF EACH CARD IN MD/SB...
archAverageData = function(df,bestArchetype){
  df_best_arch=df[df$ARCHETYPE==bestArchetype,]
  cardsList=unique(c(unlist(df_best_arch$SBCards),unlist(df_best_arch$MDCards)))
  averageCards=setNames(data.frame(matrix(ncol = 6, nrow = 0)), 
                        c("CardName","Types","AvgMdCount", "AvgSbCount",
                          "AlwaysMDPresent","AlwaysSBPresent"))
  
  for (i in 1:length(cardsList)){
    cardNamei=cardsList[i]
    
    if (cardNamei %in% cardDataSub$name){
      cardTypei=cardDataSub[cardDataSub$name==cardNamei,]$types
    }else if (cardNamei %in% cardDataSub$faceName){
      cardTypei=cardDataSub[cardDataSub$faceName==cardNamei,]$types
    }
    
    AvgMdCounti=0
    AvgSbCounti=0
    AlwaysMDPresenti=TRUE
    AlwaysSBPresenti=TRUE
    
    for (j in 1:length(df_best_arch$URL)){
      #1 == TRUE
      #0 == FALSE
      if (!is.na(match(cardNamei,df_best_arch$MDCards[[j]]))){
        AvgMdCounti=AvgMdCounti+df_best_arch$MDCounts[[j]][match(
          cardNamei, df_best_arch$MDCards[[j]])]
      }else{
        AlwaysMDPresenti=FALSE
      }
      if (!is.na(match(cardNamei,df_best_arch$SBCards[[j]]))){
        AvgSbCounti=AvgSbCounti+df_best_arch$SBCounts[[j]][match(
          cardNamei, df_best_arch$SBCards[[j]])]
      }else{
        AlwaysSBPresenti=FALSE
      }
    }
    
    AvgMdCounti=as.numeric(format(round(AvgMdCounti/length(df_best_arch$URL),2),
                                  nsmall = 2))
      
    AvgSbCounti=as.numeric(format(round(AvgSbCounti/length(df_best_arch$URL),2),
                                  nsmall = 2))
    
    averageCardsTmpi=data.frame(cardNamei,cardTypei,AvgMdCounti,AvgSbCounti,
                               AlwaysMDPresenti,AlwaysSBPresenti)
    
    
    names(averageCardsTmpi)=c("CardName","Types","AvgMdCount", "AvgSbCount",
                             "AlwaysMDPresent","AlwaysSBPresent")
    averageCards=rbind(averageCards, averageCardsTmpi)
  }
  return(averageCards)
}

#GENERATE AN AVERAGE DECKLIST ROUNDING DOWN THE NUMBER OF CARDS
Average_decklist_round_down= function(df_avg_cards,avgCardRatio){
  MDCardsByType=list(arrange(df_avg_cards[df_avg_cards$Types=="Land",],
                             desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Creature",],
                        desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Planeswalker",],
                        desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Instant",],
                        desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Sorcery",],
                        desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Artifact",],
                        desc(AvgMdCount)),
                arrange(df_avg_cards[df_avg_cards$Types=="Enchantment",],
                        desc(AvgMdCount)))
  averageMD=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("CardName","CardCount"))
  for (i in 1:length(MDCardsByType)){
    k=0
    j=0
    notEnoughCards=TRUE
    cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                      c("CardName","CardCount"))
    while (notEnoughCards){
      j=j+1
      cardNameij=MDCardsByType[[i]][j,]$CardName
      cardCountij=round(MDCardsByType[[i]][j,]$AvgMdCount)
      
      k=sum(cardList$CardCount)+cardCountij
      
      if(k<avgCardRatioRound[[i]] && avgCardRatioRound[[i]]!=0 && cardCountij !=0){
        cardij=data.frame(cardNameij,cardCountij)
        names(cardij)=c("CardName","CardCount")
        cardList=rbind(cardList,cardij)
      }else{
        notEnoughCards=FALSE
      }
    }
    averageMD=rbind(averageMD,cardList)
  }
  return(averageMD)
}

#GENERATE AN AVERAGE DECKLIST ROUNDING UP THE NUMBER OF CARDS
Average_decklist_round_up= function(df_avg_cards,avgCardRatio){
  MDCardsByType=list(arrange(df_avg_cards[df_avg_cards$Types=="Land",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Creature",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Planeswalker",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Instant",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Sorcery",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Artifact",],
                             desc(AvgMdCount)),
                     arrange(df_avg_cards[df_avg_cards$Types=="Enchantment",],
                             desc(AvgMdCount)))
  averageMD=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("CardName","CardCount"))
  for (i in 1:length(MDCardsByType)){
    k=0
    j=0
    notEnoughCards=TRUE
    cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                      c("CardName","CardCount"))
    while (notEnoughCards){
      j=j+1
      cardNameij=MDCardsByType[[i]][j,]$CardName
      cardCountij=round(MDCardsByType[[i]][j,]$AvgMdCount)
      
      if(cardCountij!=0){
        k=sum(cardList$CardCount)+cardCountij
        cardij=data.frame(cardNameij,cardCountij)
        names(cardij)=c("CardName","CardCount")
        cardList=rbind(cardList,cardij)
      }
      if(k>=avgCardRatioRound[[i]] | avgCardRatioRound[[i]]==0 | cardCountij==0){
        notEnoughCards=FALSE
      }
    }
    averageMD=rbind(averageMD,cardList)
  }
  return(averageMD)
}

#Generate the basic structure of an average SB
Average_SB= function(df_avg_cards){
  averageSB=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("CardName","CardCount"))
  SBCardsSorted=arrange(df_avg_cards, desc(AvgSbCount ))
  k=0
  j=0
  notEnoughCards=TRUE
  cardList=setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                    c("CardName","CardCount"))
  while (notEnoughCards){
    j=j+1
    cardNameij=SBCardsSorted[j,]$CardName
    cardCountij=round(SBCardsSorted[j,]$AvgSbCount)
    
    if(k<15 && cardCountij !=0){
      cardij=data.frame(cardNameij,cardCountij)
      names(cardij)=c("CardName","CardCount")
      cardList=rbind(cardList,cardij)
    }else{
      notEnoughCards=FALSE
    }
  }
  averageSB=rbind(averageSB,cardList)
  return(averageSB)
}


################################################################################
#TESTING THE PRESENCE OF ALL THE SB CARDS IN REGARD TO THE WINRATE
################################################################################

# #ACP sur la présence ou non de certaines cartes en sideboard et le winrate

# listColSB=c(grep("WINRATE", colnames(df_best_arch)),
#             grep("SB_PRESENCE", colnames(df_best_arch)))
# activeBestArchSB=df_best_arch[,listColSB]
# # names(activeBestArchSB)
# # length(names(activeBestArchSB))
# # fviz_pca_var(res.pca2, col.var = "contrib", gradient.cols = c("blue", "green", "red"))  
# #   
# # res.pca2 = PCA(activeBestArchSB, graph = FALSE)
# # fviz_eig (res.pca2)
# # fviz_pca_var(res.pca2, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# # var2 = get_pca_var(res.pca2)
# # var2$coord
# 
# activeBestArch2A=activeBestArchSB[,c(1,2:11)]
# res.pca2A = PCA(activeBestArch2A, graph = FALSE)
# fviz_pca_var(res.pca2A, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2A)$coord
# 
# activeBestArch2B=activeBestArchSB[,c(1,12:22)]
# res.pca2B = PCA(activeBestArch2B, graph = FALSE)
# fviz_pca_var(res.pca2B, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2B)$coord
# 
# activeBestArch2C=activeBestArchSB[,c(1,23:33)]
# res.pca2C = PCA(activeBestArch2C, graph = FALSE)
# fviz_pca_var(res.pca2C, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2C)$coord
# 
# activeBestArch2D=activeBestArchSB[,c(1,34:44)]
# res.pca2D = PCA(activeBestArch2D, graph = FALSE)
# fviz_pca_var(res.pca2D, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2D)$coord
# 
# activeBestArch2E=activeBestArchSB[,c(1,45:54)]
# res.pca2E = PCA(activeBestArch2E, graph = FALSE)
# fviz_pca_var(res.pca2E, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2E)$coord
# 
# names(activeBestArchSB)
# sum(activeBestArchSB$SB_PRESENCE_AETHER_GUST)
# length(activeBestArchSB$SB_PRESENCE_AETHER_GUST)
# ################################################################################
# #TESTING THE PRESENCE OF ALL THE MD CARDS IN REGARD TO THE WINRATE
# ################################################################################
# #ACP sur la présence ou non de certaines cartes en sideboard
# listColMD=c(grep("WINRATE", colnames(df_best_arch)),
#             grep("MD_PRESENCE", colnames(df_best_arch)))
# activeBestArchMD=df_best_arch[,listColMD]
# # names(activeBestArchMD)
# # length(names(activeBestArchMD))
# # fviz_pca_var(res.pca2, col.var = "contrib", gradient.cols = c("blue", "green", "red"))  
# #   
# # res.pca2 = PCA(activeBestArchMD, graph = FALSE)
# # fviz_eig (res.pca2)
# # fviz_pca_var(res.pca2, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# # var2 = get_pca_var(res.pca2)
# # var2$coord
# 
# activeBestArch2A=activeBestArchMD[,c(1,2:11)]
# res.pca2A = PCA(activeBestArch2A, graph = FALSE)
# fviz_pca_var(res.pca2A, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2A)$coord
# 
# activeBestArch2B=activeBestArchMD[,c(1,12:22)]
# res.pca2B = PCA(activeBestArch2B, graph = FALSE)
# fviz_pca_var(res.pca2B, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2B)$coord
# 
# activeBestArch2C=activeBestArchMD[,c(1,23:33)]
# res.pca2C = PCA(activeBestArch2C, graph = FALSE)
# fviz_pca_var(res.pca2C, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2C)$coord
# 
# activeBestArch2D=activeBestArchMD[,c(1,34:44)]
# res.pca2D = PCA(activeBestArch2D, graph = FALSE)
# fviz_pca_var(res.pca2D, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2D)$coord
# 
# activeBestArch2E=activeBestArchMD[,c(1,45:55)]
# res.pca2E = PCA(activeBestArch2E, graph = FALSE)
# fviz_pca_var(res.pca2E, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2E)$coord
# 
# activeBestArch2F=activeBestArchMD[,c(1,56:66)]
# res.pca2F = PCA(activeBestArch2F, graph = FALSE)
# fviz_pca_var(res.pca2F, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2F)$coord
# 
# activeBestArch2G=activeBestArchMD[,c(1,67:77)]
# res.pca2G = PCA(activeBestArch2G, graph = FALSE)
# fviz_pca_var(res.pca2G, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# get_pca_var(res.pca2G)$coord
# 
# 
# names(activeBestArchMD)
# sum(activeBestArchMD$MD_PRESENCE_PATH_TO_EXILE)
# length(activeBestArchMD$MD_PRESENCE_PATH_TO_EXILE)
################################################################################
