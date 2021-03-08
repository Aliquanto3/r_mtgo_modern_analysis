install.packages("gapminder")
library(gapminder)
head(gapminder)
theme_set(theme_bw())

setwd("MTGO_Data/Animation_tests")

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p = p + transition_time(year) +
  labs(title = "Year: {frame_time}")
animate(p, renderer = ffmpeg_renderer())

animate(p, duration = 5, fps = 20, width = 600, height = 600, 
        renderer = gifski_renderer())
anim_save("output.gif")



plot2020byWeek=ggplot(df_2020, aes(x=ARCHETYPES, y=as.numeric(SHARE), fill=ARCHETYPES)) + 
  geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
  labs(x = NULL, y = "Presence (%)", fill = NULL, 
       title = "boxplot_title", subtitle = "by Anael Yahi") + 
  scale_color_gradient(low="blue", high="red") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +  
  #geom_text(x=-Inf, y=Inf, label=paste("Week",week,sep=" ")) +
  theme(axis.text.x  = element_text(size=12)) + transition_time(WEEK) +
  labs(title = "Week: {frame_time}")

animate(plot2020byWeek, duration = 10, fps = 10, width = 600, height = 600, 
        renderer = gifski_renderer())

##############################
byWeekDf=function(startDate,stopDate){
  mondays2020=seq(as.Date(startDate),as.Date(stopDate),by=7)
  beginning=mondays2020[1]
  end=mondays2020[2]
  dfWeek=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
  metricDfWeek=metric_points_archetypes(dfWeek,beginning,end)
  metricDfWeek$WEEK=rep(1,length(metricDfWeek$ARCHETYPES))
  for (i in 2:length(mondays2020)-1){
    print(i)
    beginning=mondays2020[i]
    end=mondays2020[i+1]
    dfWeek=subset(df, DATE >= as.Date(beginning) & DATE < as.Date(end))
    metric_df_gen=metric_points_archetypes(dfWeek,beginning,end)
    metric_df_gen$WEEK=rep(i,length(metric_df_gen$ARCHETYPES))
    metricDfWeek=rbind(metricDfWeek,metric_df_gen)
  }
  return(metricDfWeek)
}

generateMetricPlot = function(metric_df,beginning,end){
  average=mean(metric_df$WINRATE_AVERAGE)
  sdeviation=sd(metric_df$WINRATE_AVERAGE)
  x_label="Total number of matches played by each archetype (%)"
  metric_df$TOTAL_NB_MATCHES=metric_df$TOTAL_NB_MATCHES/sum(metric_df$TOTAL_NB_MATCHES)*100
  metric_plot=ggplot(metric_df, aes(TOTAL_NB_MATCHES, WINRATE_AVERAGE*100))
  avg_presence=mean(metric_df$TOTAL_NB_MATCHES)
  std_presence=sd(metric_df$TOTAL_NB_MATCHES)
  y_label="Average winrate of each archetype (%)"
  graph_title=paste("Winrates depending on presence:", Classification,"archetypes ", 
                    "between", beginning, "and", end, "in MTGO", EventType,sep = " ")
  graph_subtitle=paste("Circle diameters depending on Players\nby Anael Yahi",sep=" ")
  diam_ratio=0.3
  metric_plot=metric_plot + 
    geom_point(aes(color = ARCHETYPES), size=metric_df$NB_PLAYERS*diam_ratio,
               show.legend = FALSE)
  metric_plot=metric_plot + coord_cartesian() + theme_bw() +  
    scale_x_continuous(trans = 'log10') +
    labs(x=x_label, y=y_label, title=graph_title, subtitle=graph_subtitle) + 
    geom_text_repel(aes(label=ARCHETYPES),hjust=0, vjust=0,point.padding = NA)
  metric_plot = metric_plot + transition_time(WEEK) +
    labs(title = "Week: {frame_time}")
  metric_plot=metric_plot + geom_vline(xintercept = avg_presence, linetype="dotted",
                                       color = "purple", size=2) +
    geom_text(aes(x=avg_presence, label="Tiers 2.5\n", y=
                    max(WINRATE_AVERAGE*100)-1), colour="purple",
              angle=0, size=8) +
    geom_text(aes(x=avg_presence, label="\nPresence mean", y=
                    max(WINRATE_AVERAGE*100)-1), colour="black",
              angle=0, size=4) +
    geom_vline(xintercept = avg_presence + std_presence, linetype="dotted",
               color = "blue", size=1.5) +
    geom_text(aes(x=avg_presence + std_presence, label="Tiers 2\n",
                  y=max(WINRATE_AVERAGE*100)-2), colour="blue",
              angle=0, size=8) +
    geom_text(aes(x=avg_presence + std_presence, label="\nPresence mean + 1*sd",
                  y=max(WINRATE_AVERAGE*100)-2), colour="black",
              angle=0, size=4) +
    geom_vline(xintercept = avg_presence + 2*std_presence, linetype="dotted",
               color = "dark green", size=1.5) +
    geom_text(aes(x=avg_presence + 2*std_presence, label="Tiers 1.5\n",
                  y=max(WINRATE_AVERAGE*100)-4), colour="dark green",
              angle=0, size=8) +
    geom_text(aes(x=avg_presence + 2*std_presence, label="\nPresence mean + 2*sd",
                  y=max(WINRATE_AVERAGE*100)-4), colour="black",
              angle=0, size=4) +
    geom_vline(xintercept = avg_presence + 3*std_presence, linetype="dotted",
               color = "orange", size=1.5) +
    geom_text(aes(x=avg_presence + 3*std_presence, label="Tiers 1\n",
                  y=max(WINRATE_AVERAGE*100)-7), colour="orange",
              angle=0, size=8) +
    geom_text(aes(x=avg_presence + 3*std_presence, label="\nPresence mean + 3*sd",
                  y=max(WINRATE_AVERAGE*100)-7), colour="black",
              angle=0, size=4) +
    geom_vline(xintercept = avg_presence + 4*std_presence, linetype="dotted",
               color = "red", size=1.5) +
    geom_text(aes(x=avg_presence + 4*std_presence, label="Tiers 0\n",
                  y=max(WINRATE_AVERAGE*100)-11), colour="red",
              angle=0, size=8) +
    geom_text(aes(x=avg_presence + 4*std_presence, label="\nPresence mean + 4*sd",
                  y=max(WINRATE_AVERAGE*100)-11), colour="black",
              angle=0, size=4)
  return(metric_plot)
}


startDate="2019-12-30"
stopDate="2020-08-01"
#stopDate="2021-01-04"
metric_2020_df=byWeekDf(startDate,stopDate)

metric_plot=generateMetricPlot(metric_2020_df,startDate,stopDate)
animate(metric_plot, fps = 2, width = 1000, height = 1000, 
        renderer = gifski_renderer())
metric_plot
##############################

testplot=testplot + labs(x = "Number of matches", y = "Winrate") + 
  transition_time(WEEK) + labs(title = "Year: {frame_time}")

animate(testplot, fps = 10, width = 600, height = 600, 
        renderer = gifski_renderer())


metagame_box_plot_return = function(df,presence,beginning,end,EventType){
  
  #GET THE DATA FOR ALL ARCHETYPES HAVING A META SHARE ABOVE HistShare
  df_gen=generate_metagame_data(df,HistShare,presence,beginning,end,week)
  
  #GENERATE A TITLE FOR THE BOXPLOT
  boxplot_title=paste(generate_metagame_graph_title(presence,beginning,end,EventType),"-",
                      df_gen[grep("Other",df_gen$ARCHETYPES),]$ARCHETYPES, sep=" ")
  
  #THIS GRAPH DOESN'T DISPLAY THE "Other" CATEGORY
  df_gen=df_gen[!grepl("Other",df_gen$ARCHETYPES),]
  
  #REORDER ARCHETYPES BY ASCENDING PRESENCE
  df_gen$ARCHETYPES = reorder(df_gen$ARCHETYPES, as.numeric(df_gen$PRESENCE))
  
  #plot is much clearer
  animatedPlot=ggplot(df_gen, aes(x=ARCHETYPES, y=as.numeric(SHARE), fill=ARCHETYPES)) + 
    geom_bar(stat="identity") + theme_minimal() + guides( fill = FALSE) +
    labs(x = NULL, y = "Presence (%)", fill = NULL, 
         title = boxplot_title, subtitle = "by Anael Yahi") + 
    scale_color_gradient(low="blue", high="red") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +  
    geom_text(x=-Inf, y=Inf, label=paste("Week",week,sep=" ")) +
    theme(axis.text.x  = element_text(size=12)) + transition_time(year) +
    labs(title = "Year: {frame_time}")
  
  return(animatedPlot)
}