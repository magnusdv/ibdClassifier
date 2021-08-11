library(ggplot2)

distPlot = function(top3, segData, obs, cols = 2:4) {

  plotdatList = lapply(1:3, function(i) {
    r = top3[i]
    segs = segData[[r]]
    data.frame(Nr = i, Rel = r,
               Count = lengths(segs),
               Average = vapply(segs, safeMean, FUN.VALUE = 1))
  })

  plotdat = do.call(rbind, plotdatList)
  plotdat$Nr = as.factor(plotdat$Nr)


  ggplot(plotdat, aes_(~Count, ~Average, color = ~Nr)) +
    geom_jitter(width = 0.35, alpha = 0.5, shape = 1, size = 0.9, show.legend = FALSE) +
    suppressWarnings(stat_ellipse(size = 1.1)) +
    annotate("point", x = length(obs), y = mean(obs), size = 3.5, stroke = 2, shape = 4, colour = 1) +
    labs(x = "Number of segments", y = "Average segment (cM)", col = NULL) +
    scale_color_manual(values = cols) +
    theme_classic(base_size = 14) +
    theme(legend.position = c(.99, .99),
          legend.justification = c("right", "top"),
          legend.key.width = unit(1.3, "cm"),
          legend.text = element_text(size = 16))
}


plotped = function(ped, ids, title, post, col = 1) {
  col = setNames(list(ids), as.character(col))
  plot(ped, labs = NULL, hatched = ids, col = col, margin = c(.5,1,4.2,1))
  #box("outer")
  mtext(title, line = 2.3, font = 2, cex = 1.5, xpd = NA)
  mtext(sprintf("P = %.1f%%", post * 100), line = 1, font = 1, cex = 1.2)
}
