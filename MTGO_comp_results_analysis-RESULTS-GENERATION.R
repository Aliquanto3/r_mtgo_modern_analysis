################################################################################
#USE THE METHODS BELOW TO GENERATE THE GRAPHS AND RESULTS YOU LOOK FOR

#GENERATE THE METAGAME PIE CHART FOR THE SELECTED DATA
metagame_pie_chart(df)

#GENERATE THE METAGAME HISTOGRAM FOR THE SELECTED DATA
metagame_box_plot(df)

#DISPLAYS THE DATA AND THE GRAPH FOR THE DEFEAT WEIGHTS METRIC
metric_1_df_defeat_weight=m_1_defeat_weight(df)
metric_1_df_defeat_weight
metric_1_plot_defeat_weight=metric_graph(metric_1_df_defeat_weight, "M1 - Defeat Weight")
metric_1_plot_defeat_weight

#DISPLAYS THE DATA AND THE GRAPH FOR THE SWISS WINS METRIC
metric_2_df_swiss_wins=m_2_swiss_wins(df)
metric_2_df_swiss_wins
metric_2_plot_swiss_wins=metric_graph(metric_2_df_swiss_wins, "M2 - Swiss wins")
metric_2_plot_swiss_wins

#DISPLAYS THE DATA AND THE GRAPH FOR THE SWISS WINS METRIC
metric_2.5_df_top8_swiss_wins=m_2.5_top8_swiss_wins(df)
metric_2.5_df_top8_swiss_wins
metric_2.5_plot_top8_swiss_wins=metric_graph(metric_2.5_df_top8_swiss_wins, "M2.5 - Top8 + Swiss wins")
metric_2.5_plot_top8_swiss_wins

#FIRST COMPILATION
compilation_df=metrics_compilation(df)
compilation_df
compilation_plot=metric_graph(compilation_df, "Metrics Compilation")
compilation_plot

#FINAL COMPILATION
final_df=final_compilation(df)
final_df
m=mean(final_df$COMPILATION_POINTS)
s=sd(final_df$COMPILATION_POINTS)
final_df[final_df$COMPILATION_POINTS>m+s,]
final_df[final_df$COMPILATION_POINTS>m+2*s,]
