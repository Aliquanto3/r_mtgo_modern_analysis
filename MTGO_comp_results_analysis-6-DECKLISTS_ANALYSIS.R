#6th file to execute
#Selects a specific archetype in the data for which we try to optimize the 
#decklist through an analysis of the number of different cards

#TO BE MANUALLY CHOSEN AFTER ANALYSIS OF ALL THE METRICS
#KEEP ONLY THE DATA OF A SINGLE ARCHETYPE
bestArchetype="WURG Control"
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

#ADD THE NUMBER OF CARDS HAVING AT LEAST A SPECIFIC TYPE OF EACH DECK
#USE THE FOLLOWING COMMAND TO FIND ALL THE TYPES YOU CAN USE:
#unique(cardDataSub$types)
addRatioType = function(df,type){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowNameType=toupper(paste("RATIO",type,sep="_"))
  #CREATE THE COLUMN
  df[[rowNameType]]=rep(0,length(df$URL))
  #FILL THE COLUMN WITH THE NUMBER OF CARDS OF THE CHOSEN TYPE IN EACH DECK
  for (i in 1:length(df$URL)){
    landcount=0
    for (j in 1:length(df$MDCards[[i]])){
      if (df$MDCards[[i]][j] %in% cardDataSub$name){
        if((length(grep(type,cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$name==df$MDCards[[i]][j],]$types)==1)){
          landcount=landcount+df$MDCounts[[i]][j]
        }
      }else if (df$MDCards[[i]][j] %in% cardDataSub$faceName){
        if((length(grep(type,cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types))==1)&&
           (grep(type,cardDataSub[cardDataSub$faceName==df$MDCards[[i]][j],]$types)==1)){
          landcount=landcount+df$MDCounts[[i]][j]
        }
      }
    }
    df[[rowNameType]][i]=landcount/df_best_arch$TOTAL_MD[i]
  }
  return(df)
}

#IT CAN BE A BIT LONG TO EXECUTE, LEAVE IT A MINUTE OR TWO
df_best_arch=addRatioType(df_best_arch,"Land")
df_best_arch=addRatioType(df_best_arch,"Creature")
df_best_arch=addRatioType(df_best_arch,"Planeswalker")
df_best_arch=addRatioType(df_best_arch,"Instant")
df_best_arch=addRatioType(df_best_arch,"Sorcery")
df_best_arch=addRatioType(df_best_arch,"Artifact")
df_best_arch=addRatioType(df_best_arch,"Enchantment")

#names(df_best_arch)

#unique(unlist(df_best_arch$SBCards))
#table(unlist(df_best_arch$SBCards))
#length(df_best_arch$URL)

#ADD A VARIABLE TELLING WHETHER cardName IS IN THE SB (1) OR NOT (0)
isPresentSBCard = function(df_best_arch,cardName){
  #GENERATE THE NAME OF THE COLUMN BASED ON THE TYPE
  rowName=gsub('[[:punct:] ]+',' ',cardName)
  rowName=chartr(" ", "_", rowName)
  rowNameCard=toupper(paste("PRESENCE",rowName,sep="_"))
  
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

df_best_arch=isPresentSBCard(df_best_arch,"Kozilek, Butcher of Truth")
df_best_arch=isPresentSBCard(df_best_arch,"Dispel")
df_best_arch=isPresentSBCard(df_best_arch,"Nexus of Fate")
df_best_arch=isPresentSBCard(df_best_arch,"Condemn")
df_best_arch=isPresentSBCard(df_best_arch,"Stony Silence")
df_best_arch=isPresentSBCard(df_best_arch,"Deicide")
df_best_arch=isPresentSBCard(df_best_arch,"Tale's End")
df_best_arch=isPresentSBCard(df_best_arch,"Madcap Experiment")
df_best_arch=isPresentSBCard(df_best_arch,"Nature's Claim")
df_best_arch=isPresentSBCard(df_best_arch,"Containment Priest")
df_best_arch=isPresentSBCard(df_best_arch,"Gaea's Blessing")
df_best_arch=isPresentSBCard(df_best_arch,"Timely Reinforcements")

#names(df_best_arch)