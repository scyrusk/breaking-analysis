rq2.plot.rates <- function() {
  .article.rate.plotter(rq2.audience.all(), "Audience", "rq2_who.png")
  .article.rate.plotter(rq2.comm.channel.all(), "Channel", "rq2_channel.png")
  .article.rate.plotter(rq2.rationale.all(), "Reason", "rq2_rationale.png")
  .article.rate.plotter(rq2.content.all(), "Content", "rq2_content.png")
}

rq2.plot.by.eventtype.rates <- function() {
  .stacked.plotter(rq2.audience.by.eventtype(), "Audience", "rq2_who_by_eventtype.png")
  .stacked.plotter(rq2.comm.channel.by.eventtype(), "Channel", "rq2_channel_by_eventtype.png")
  .stacked.plotter(rq2.rationale.by.eventtype(), "Reason", "rq2_rationale_by_eventtype.png")
  .stacked.plotter(rq2.content.by.eventtype(), "Content", "rq2_content_by_eventtype.png")
}

rq2.plot.faceted.eventtype.rates <- function() {
  .facet.plotter(rq2.audience.by.eventtype(), "Audience", "rq2_who_by_eventtype.png")
  .facet.plotter(rq2.comm.channel.by.eventtype(), "Channel", "rq2_channel_by_eventtype.png")
  .facet.plotter(rq2.rationale.by.eventtype(), "Reason", "rq2_rationale_by_eventtype.png")
  .facet.plotter(rq2.content.by.eventtype(), "Content", "rq2_content_by_eventtype.png")
}

.article.reshare.frequency.plotter <- function(mat=rq2.reshare.rate.by.eventtype()) {
  plot <-
    ggplot(mat, aes(x=community, y=rate)) +
    geom_bar(stat="identity", alpha=0.7) +
    ylab("Reshare Rate") +
    theme(
      axis.text.x = element_text(size=9, angle=45, hjust=1),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size=12)
    )
  fn <- paste("out", "final", "reshare_rates.png", sep="/")
  ggsave(fn, plot, width=4, height=3)
  (plot)
}

rq2.reshare.rate.all <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  nrow(mat) / nrow(regression.clustered.events)
}

rq2.reshare.rate.by.eventtype <- function(mat=regression.clustered.events) {
  ddply(regression.clustered.events, .(community), function(n) { c(rate=round(length(which(n$ToldOthers)) / nrow(n), 2)) })
}

# In general, how/to who/what/why do people share or reshare security information?
# Who do participants typically share with?
rq2.audience.all <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.collapsed.rates(mat, told.who.cols(), told.who.nice.rename())
}

rq2.audience.by.eventtype <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.rates.helper(mat, told.who.cols(), told.who.nice.rename())
}

rq2.who.others <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  table(mat$ToldWho.WhichOther)
}

# Among socially propagated information, how does security information propagate through social channels?
rq2.comm.channel.all <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.collapsed.rates(mat, told.how.cols(), told.how.nice.rename())
}

rq2.comm.channel.by.eventtype <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.rates.helper(mat, told.how.cols(), told.how.nice.rename())
}

rq2.other.channels <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  table(mat$ToldHow.WhichOther)
}

# Among socially propagated information, why do participants believe the information is being shared with them?
rq2.rationale.all <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.collapsed.rates(mat, told.why.cols(), told.why.nice.rename())
}

rq2.rationale.by.eventtype <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.rates.helper(mat, told.why.cols(), told.why.nice.rename())
}

rq2.other.rationales <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  table(mat$ToldWhy.WhichOther)
}

# Among socially propagated information, what do participants believe is being shared with them?
rq2.content.all <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.collapsed.rates(mat, told.what.cols(), told.what.nice.rename())
}

rq2.content.by.eventtype <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  .rq1.rates.helper(mat, told.what.cols(), told.what.nice.rename())
}

rq2.other.content <- function(mat=.filter.only.told.others(regression.clustered.events)) {
  table(mat$ToldWhat.WhichOther)
}

# Helpers
.filter.only.told.others <- function(mat=regression.clustered.events) {
  mat[which(mat$ToldOthers),]
}