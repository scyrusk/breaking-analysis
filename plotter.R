plot.hist <- function(mat, col, label=NA, fn=NA) {
  plot <- ggplot(mat, aes_string(col)) +
    geom_histogram(binwidth=1)

  if (!is.na(label)) {
    plot <- plot + xlab(label)
  }

  if (!is.na(fn)) {
    full.path <- paste("out", fn, sep="/")
    ggsave(full.path, plot, width=3.5, height=3.5)
  }

  (plot)
}

error.model.95cis <- function(x) {
  multiplier <- 1.96
  fe <- fixef(x)
  se <- sqrt(diag(vcov(x)))
  lower <- fe - multiplier*se
  upper <- fe + multiplier*se
  coeffs <- data.frame(b=fe, bmin=lower, bmax=upper)
  coeffs
}

plot.error.model.95cis <- function(dat=error.model.95cis(heard.about.model)) {
  dat$predictor <- row.names(dat)
  limits <- aes(ymax=bmax, ymin=bmin)
  ggplot(dat, aes(x=predictor, y=b, color=predictor)) +
    geom_crossbar(limits, width=0.2) +
    ylab("Coeffs") +
    # ggtitle("2-Week Recall Rate Across Conditions And Allowable Errors") +
    theme(legend.position="none",
          axis.text.x = element_text(angle=45, hjust=1, size=9, face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=10))
}