#6th file to execute
#Selects a specific archetype in the data for which we try to optimize the 
#decklist through an analysis of the number of different cards

#TO BE MANUALLY CHOSEN AFTER ANALYSIS OF ALL THE METRICS
#KEEP ONLY THE DATA OF A SINGLE ARCHETYPE
bestArchetype="WURG Control"
df_best_arch=df[df$ARCHETYPE==bestArchetype,]

df_best_arch$TOTAL_MD=rep(60,length(df_best_arch$URL))
for (i in 1:length(df_best_arch$TOTAL_MD)){
  df_best_arch$TOTAL_MD[i]=sum(df_best_arch$MDCounts[[i]])
}

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

#
names(df_best_arch)

regBestArch=lm(WINRATE~RATIO_SORCERY+RATIO_INSTANT+RATIO_PLANESWALKER+RATIO_CREATURE+
                 RATIO_LAND+RATIO_ARTIFACT+RATIO_ENCHANTMENT+TOTAL_MD+CMC,data=df_best_arch)
summary(regBestArch)

activeBestArch=select(df_best_arch, RATIO_SORCERY,RATIO_INSTANT,RATIO_PLANESWALKER,
                      RATIO_CREATURE,RATIO_LAND,RATIO_ARTIFACT,RATIO_ENCHANTMENT,
                      TOTAL_MD,CMC,WINRATE)
#View(activeBestArch)


res.pca <- PCA(activeBestArch, graph = FALSE)
print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)
# Coordonnées des variables
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
head(var$cos2, 4)

corrplot(var$cos2, is.corr=FALSE)
# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

head(var$contrib, 4)

corrplot(var$contrib, is.corr=FALSE)  

# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Changez la transparence en fonction de contrib
fviz_pca_var(res.pca, alpha.var = "contrib")

# Créer une variable aléatoire continue de longueur 10
set.seed (123)
my.cont.var <- rnorm (4)
# Colorer les variables en fonction de la variable continue
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")
