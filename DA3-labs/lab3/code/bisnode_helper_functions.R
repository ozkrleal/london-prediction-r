# helper functions for classification code: ch17-predicting-firm-exit
# v.1.0. 2019-11-xx
# v.1.3 2019-12-21 slight graph changes
# v.1.4 2019-12-24 adds coloring, size


twoClassSummaryExtended <- function (data, lev = NULL, model = NULL)
{
  lvls <- levels(data$obs)
  rmse <- sqrt(mean((data[, lvls[1]] - ifelse(data$obs == lev[2], 0, 1))^2))
  c(defaultSummary(data, lev, model), "RMSE" = rmse)
}



#createLossPlot <- function(r, best_coords, file_name,  mywidth_large=12, myheight_large = 9) {
createLossPlot <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]
  n <- rowSums(best_coords[c("tn", "tp", "fn", "fp")])[1]

  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  all_coords <- all_coords %>%
    mutate(loss = (fp*FP + fn*FN)/n)
  l <- all_coords[all_coords$threshold == t, "loss"]

  loss_plot <- ggplot(data = all_coords, aes(x = threshold, y = loss)) +
    geom_line(color=color[1]) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_vline(xintercept = t , color = color[3] ) +
    annotate(geom = "text", x = t, y= min(all_coords$loss),
             label=paste0("best threshold: ", round(t,2)),
             colour=color[3], angle=90, vjust = -1, hjust = -0.5, size = 7) +
    annotate(geom = "text", x = t, y= l,
             label= round(l, 2), hjust = -0.3, size = 7) +
    theme_bg() +
    theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))

  ggsave(plot = loss_plot, paste0(file_name,".png"),
         width=mywidth_small, height=myheight_small, dpi=1200)
  cairo_ps(filename = paste0(file_name,".eps"),
           width = mywidth_small, height = myheight_small, pointsize = 12,
           fallback_resolution = 1200)
  print(loss_plot)
  dev.off()

  loss_plot
}



#createRocPlotWithOptimal <- function(r, best_coords, file_name,  mywidth_large=12, myheight_large = 9) {
createRocPlotWithOptimal <- function(r, best_coords, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {

  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)
  t <- best_coords$threshold[1]
  sp <- best_coords$specificity[1]
  se <- best_coords$sensitivity[1]

  roc_plot <- ggplot(data = all_coords, aes(x = specificity, y = sensitivity)) +
    geom_line(color=color[1]) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_x_reverse(breaks = seq(0, 1, by = 0.1)) +
    geom_point(aes(x = sp, y = se)) +
    annotate(geom = "text", x = sp, y = se,
             label = paste(round(sp, 2),round(se, 2),sep = ", "),
             hjust = 1, vjust = -1, size = 7) +
    theme_bg() +
    theme(axis.text.x = element_text(size=20), axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))

  ggsave(plot = roc_plot, paste0(file_name, ".png"),
         width=mywidth_small, height=myheight_small, dpi=1200)
  cairo_ps(filename = paste0(file_name, ".eps"),
           width = mywidth_small, height = myheight_small, pointsize = 12,
           fallback_resolution = 1200)
  print(roc_plot)
  dev.off()

  roc_plot
}
# createRocPlot <- function(r, file_name,  mywidth_large=12, myheight_large = 9) {
createRocPlot <- function(r, file_name,  myheight_small = 5.625, mywidth_small = 7.5) {
  all_coords <- coords(r, x="all", ret="all", transpose = FALSE)

  roc_plot <- ggplot(data = all_coords, aes(x = fpr, y = tpr)) +
    geom_line(size = 1) +
    geom_area(aes(fill = color[5], alpha=0.4), alpha = 0.3, position = 'identity', color = color[1]) +
    scale_fill_viridis(discrete = TRUE, begin=0.6, alpha=0.5, guide = FALSE) +
    xlab("False Positive Rate (1-Specifity)") +
    ylab("True Positive Rate (Sensitivity)") +
    geom_abline(intercept = 0, slope = 1,  linetype = "dotted", col = "black") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0.01)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0.01, 0)) +
    theme_bg() +
    theme(axis.text.x = element_text(size=13), axis.text.y = element_text(size=13),
          axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))

  ggsave(plot = roc_plot, paste0(file_name, ".png"),
         width=mywidth_small, height=myheight_small, dpi=1200)
  cairo_ps(filename = paste0(file_name, ".eps"),
           width = mywidth_small, height = myheight_small, pointsize = 12,
           fallback_resolution = 1200)
  print(roc_plot)
  dev.off()

  roc_plot
}
