.article.rate.plotter.grid <- function(mat, xl, fn.end, w=6, h=3) {
  library(gridExtra)
  plots <- lapply(split(mat, mat$article), function(spl) {
    ggplot(spl, aes(x=var, y=rate, ymin=min.rate, ymax=max.rate, fill=var)) +
    geom_point(aes(shape=var)) +
    geom_bar(stat="identity", alpha=0.7) +
    geom_errorbar(width=0.2) +
    xlab(xl) +
    ylab("Selection Rate") +
    ggtitle(spl$article[1]) +
    guides(fill=guide_legend(nrow=2), shape=guide_legend(nrow=2)) +
    theme(
      # axis.text.x = element_text(size=10, angle=45, hjust=1),
      axis.text.x = element_blank(),
      axis.title.x = element_text(size=9),
      axis.title.y = element_text(size=9),
      strip.text.x = element_text(size=9),
      legend.position="none"
    )
  })
  do.call(grid.arrange, c(plots, ncol=5, nrow=4))
  fn <- paste("out", fn.end, sep="/")
  png(fn, width=5, height=4)
  dev.off()
  fn
  # fn <- paste("out", fn.end, sep="/")
  # ggsave(fn, plot, width=w, height=h)
}