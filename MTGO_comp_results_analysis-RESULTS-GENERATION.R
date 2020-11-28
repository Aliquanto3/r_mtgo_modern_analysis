################################################################################
#USE THE METHODS BELOW TO GENERATE THE GRAPHS AND RESULTS YOU LOOK FOR

#GENERATE THE METAGAME PIE CHART FOR THE SELECTED DATA
metagame_pie_chart(df)

#GENERATE THE METAGAME HISTOGRAM FOR THE SELECTED DATA
metagame_box_plot(df)

#DISPLAYS THE DATA AND THE GRAPH FOR THE DEFEAT WEIGHTS METRIC
metric_df_defeat_weight=m_defeat_weight(df)
metric_df_defeat_weight
metric_plot_defeat_weight=metric_graph(metric_df, "Defeat Weight")
metric_plot_defeat_weight

#DISPLAYS THE DATA AND THE GRAPH FOR THE SWISS WINS METRIC
metric_df_swiss_wins=m_swiss_wins(df)
metric_df_swiss_wins
metric_plot_swiss_wins=metric_graph(metric_df, "Swiss wins")
metric_plot_swiss_wins

#DISPLAYS THE DATA AND THE GRAPH FOR THE SWISS WINS METRIC
metric_df_top8_swiss_wins=m_top8_swiss_wins(df)
metric_df_top8_swiss_wins
metric_plot_top8_swiss_wins=metric_graph(metric_df, "Top8 + Swiss wins")
metric_plot_top8_swiss_wins
