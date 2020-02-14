rq1.plot.collapsed.rates <- function() {
  print("rq1_how.png")
  .article.rate.plotter(rq1.how.heard.all(), "Source", "rq1_how.png")
  print("rq1_who.png")
  .article.rate.plotter(rq1.social.source.all(), "Source", "rq1_who.png")
  print("rq1_channel.png")
  .article.rate.plotter(rq1.comm.channel.all(), "Channel", "rq1_channel.png")
  # .article.rate.plotter(rq1.perceived.rationale.all(), "Reason", "rq1_rationale.png")
  # .article.rate.plotter(rq1.perceived.content.all(), "Content", "rq1_content.png")
}

rq1.plot.by.eventtype.rates <- function() {
  print("rq1_how_by_eventtype.png")
  .stacked.plotter(rq1.how.heard.by.eventtype(), "Source", "rq1_how_by_eventtype.png")
  print("rq1_who_by_eventtype.png")
  .stacked.plotter(rq1.social.source.by.eventtype(), "Source", "rq1_who_by_eventtype.png")
  print("rq1_channel_by_eventtype.png")
  .stacked.plotter(rq1.comm.channel.by.eventtype(), "Channel", "rq1_channel_by_eventtype.png")
}

rq1.plot.faceted.eventtype.rates <- function() {
  print("rq1_how_by_eventtype_facet.png")
  .facet.plotter(rq1.how.heard.by.eventtype(), "Source", "rq1_how_by_eventtype_facet.png")
  print("rq1_who_by_eventtype_facet.png")
  .facet.plotter(rq1.social.source.by.eventtype(), "Source", "rq1_who_by_eventtype_facet.png")
  print("rq1_channel_by_eventtype_facet.png")
  .facet.plotter(rq1.comm.channel.by.eventtype(), "Channel", "rq1_channel_by_eventtype_facet.png")
}

.stacked.plotter <- function(mat, xl, fn.end, w=4, h=6) {
  print(mat)
  plot <-
    ggplot(mat, aes(x=eventtype, y=rate, fill=var)) +
    geom_bar(position="stack", stat='identity', alpha=0.7) +
    xlab(xl) +
    ylab("Selection Rate") +
    guides(fill=guide_legend(nrow=3), shape=guide_legend(nrow=3)) +
    theme(
      axis.text.x = element_text(size=8, angle=45, hjust=1),
      axis.text.y = element_text(size=8),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      legend.direction="horizontal",
      legend.position="bottom",
      legend.title=element_blank(),
      strip.text.x = element_text(size=9)
    )
  (plot)
  fn <- paste("out", "final", fn.end, sep="/")
  ggsave(fn, plot, width=w, height=h)
}

.facet.plotter <- function(mat, xl, fn.end, w=6, h=4) {
  plottable.names <- c(
    "Corporate Personal Data Breaches"="Corporate Personal\nData Breaches",
    "Financial Data Breaches"="Financial\nData Breaches",
    "Politicized / Activist Security"="Politicized / Activist\nSecurity",
    "Sensitive Systems and Data Breaches"="Sensitive Systems\nand Data Breaches"
  )
  mat$plot.event.type <- plottable.names[as.character(mat$eventtype)]
  plot <-
    ggplot(mat, aes(x=var, y=rate, ymin=min.rate, ymax=max.rate, fill=var)) +
    facet_grid(. ~ plot.event.type) +
    geom_bar(stat='identity', alpha=0.7) +
    geom_errorbar(width=0.2) +
    xlab(xl) +
    ylab("Selection Rate") +
    guides(fill=guide_legend(nrow=2), shape=guide_legend(nrow=2)) +
    theme(
      # axis.text.x = element_blank(),
      axis.text.x = element_text(size=8, angle=45, hjust=1),
      axis.text.y = element_text(size=8),
      # axis.title.x = element_text(size=12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=12),
      # legend.direction="horizontal",
      # legend.position="bottom",
      legend.position="none",
      legend.title=element_blank(),
      strip.text.x = element_text(size=9)
    )
  (plot)
  fn <- paste("out", "final", fn.end, sep="/")
  ggsave(fn, plot, width=w, height=h)
}

.article.rate.plotter <- function(mat, xl, fn.end, w=4, h=3) {
  print(mat)
  plot <-
    ggplot(mat, aes(x=var, y=rate, ymin=min.rate, ymax=max.rate, fill=var, shape=var)) +
    geom_point(aes(shape=var)) +
    geom_bar(stat="identity", alpha=0.7) +
    geom_errorbar(width=0.2) +
    xlab(xl) +
    ylab("Selection Rate") +
    guides(fill=guide_legend(nrow=2), shape=guide_legend(nrow=2)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      legend.direction="horizontal",
      legend.position="bottom",
      legend.title=element_blank(),
      strip.text.x = element_text(size=9)
    )
  fn <- paste("out", "final", fn.end, sep="/")
  ggsave(fn, plot, width=w, height=h)
}

# all.heard.about.or.recalled.other is a data.frame without event types and qualitative codes
# could be used as an alternative to regression.clustered.events
# In general, how do people hear about security breaches and information? What percentage of it is social?
rq1.how.heard.all <- function(mat=regression.clustered.events) {
  .rq1.collapsed.rates(mat, how.heard.cols(), how.heard.nice.rename())
}

rq1.how.heard.by.eventtype <- function(mat=regression.clustered.events) {
  .rq1.rates.helper(mat, how.heard.cols(), how.heard.nice.rename())
}

# Among socially propagated information, who do participants typically hear from?
rq1.social.source.all <- function(mat=.filter.only.social(regression.clustered.events)) {
  .rq1.collapsed.rates(mat, heard.from.who.cols(), heard.who.nice.rename())
}

rq1.social.source.by.eventtype <- function(mat=.filter.only.social(regression.clustered.events)) {
  .rq1.rates.helper(mat, heard.from.who.cols(), heard.who.nice.rename())
}

# Among socially propagated information, how does security information propagate through social channels?
rq1.comm.channel.all <- function(mat=subset(regression.clustered.events, HowHeard.SomeoneElse==1)) {
  .rq1.collapsed.rates(mat, heard.from.how.cols(), heard.how.nice.rename())
}

rq1.comm.channel.by.eventtype <- function(mat=subset(regression.clustered.events, HowHeard.SomeoneElse==1)) {
  .rq1.rates.helper(mat, heard.from.how.cols(), heard.how.nice.rename())
}

# Among socially propagated information, why do participants believe the information is being shared with them?
rq1.perceived.rationale <- function(mat=.filter.only.social(regression.clustered.events)) {
  .rq1.rates.helper(mat, heard.from.why.cols(), heard.why.nice.rename())
}

# Among socially propagated information, what do participants believe is being shared with them?
rq1.perceived.content <- function(mat=.filter.only.social(regression.clustered.events)) {
  .rq1.rates.helper(mat, heard.from.what.cols(), heard.what.nice.rename())
}

# Helpers
.filter.only.social <- function(mat) {
  ix <- which(rowSums(mat[,social.how.heard.cols()]) > 0)
  mat[ix,]
}

.calculate.rates <- function(dat, cols) {
  print(nrow(dat))
  rates <- colSums(dat[,cols]) / nrow(dat)
  sp <- 1.96*sqrt(rates*(1-rates) / nrow(dat))
  rv <- data.frame(
    rate=round(rates*100, 2),
    min.rate=sapply(round((rates-sp)*100, 2), function(x) { max(x, 0) }),
    max.rate=sapply(round((rates+sp)*100, 2), function(x) { min(x, 100) })
  )
}

.rq1.rates.helper <- function(mat, cols, nice.rename) {
  spl <- split(mat, factor(mat$community))
  rv <- lapply(seq_along(spl), function(i) {
    x <- spl[[i]]
    nm <- names(spl)[i]
    vals <- .calculate.rates(x, cols)
    data.frame(eventtype=nm, var=nice.rename[extract.last.in.path(rownames(vals))], vals)
  })

  # agg <- .calculate.rates.by.source(mat, cols)
  # rv$aggregated <- data.frame(article="aggregated", var=nice.rename[extract.last.in.path(rownames(agg))], agg)
  mrv <- do.call("rbind", rv)
  rownames(mrv) <- as.character(1:nrow(mrv))
  mrv$var <- factor(mrv$var, nice.rename)
  mrv
}

.rq1.collapsed.rates <- function(mat, cols, nice.rename) {
  vals <- .calculate.rates(mat, cols)
  rv <- data.frame(var=nice.rename[extract.last.in.path(rownames(vals))], vals)
  rownames(rv) <- as.character(1:nrow(rv))
  rv$var <- factor(rv$var, nice.rename)
  rv
}