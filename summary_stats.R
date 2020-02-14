orig.sbi.fac.anal <- function(mat=all.orig) {
  library(lavaan)
  mat <- append.literacy.correct.cols(mat)
  model <-
    " literacy =~ Literacy.PrivateBrowsing.Correct + Literacy.Cookies.Correct +
        Literacy.Encryption.Correct + Literacy.Tor.Correct +
        Literacy.VPNProxyEqual.Correct + Literacy.IPID.Correct +
        Literacy.HTTPS.Correct + Literacy.ProxyTrace.Correct
      familiarity =~ Familiarity.IPAddr + Familiarity.Cookies + Familiarity.SSL +
        Familiarity.VPN + Familiarity.Encryption + Familiarity.ProxyServer +
        Familiarity.Tor + Familiarity.BrowserPrivacy + Familiarity.PrivateBrowsing
      behavior =~ Behavior.TempNameEmail + Behavior.FakeNameUN + Behavior.FalseInfo +
        Behavior.BrowserCookieOff + Behavior.CookieClear + Behavior.TorVPN +
        Behavior.EncryptedMail + Behavior.StopUseRealName + Behavior.DeletedPost +
        Behavior.AskedToRemovePosts + Behavior.PublicCPU
      sbi =~ literacy + familiarity + behavior"
  sem.mod <- sem(model, data=mat, std.lv=T)
  print(summary(sem.mod, fit.measures=T))
  append <- predict(sem.mod, newdata=mat)
  cbind(mat, append)
}

sebis.fac.anal <- function(mat=all.new) {
  library(lavaan)
  # regmat[,value.cols()] <- scale(regmat[,value.cols()])
  # constraining variance of annoyed to 0
  model <-
    "passwords =~ SEBIS.ChangePasswords + SEBIS.DiffPasswords + SEBIS.NewPasswords + SEBIS.SpecialCharPasswords
     web =~ SEBIS.LinkVerify + SEBIS.AestheticVerify + SEBIS.HTTPSVerify + SEBIS.LinkMouseover
     updates =~ SEBIS.ReportSecurity + SEBIS.ImmediateUpdate + SEBIS.UpdatedSoftware + SEBIS.AVAutoUpdates
     auth =~ SEBIS.AutoLock + SEBIS.LaptopAuth + SEBIS.ManualLock + SEBIS.MobileAuth
     sbi =~ passwords + auth + web + updates"

  sem.mod <- sem(model, data=mat, std.lv=T)
  print(summary(sem.mod, fit.measures=T))
  append <- predict(sem.mod, newdata=mat)
  cbind(mat, append)
}

iuipc.fac.anal <- function(mat=all.new) {
  library(lavaan)
  model <-
    "control =~ IUIPC.Control1 + IUIPC.Control2 + IUIPC.Control3
     awareness =~ IUIPC.Awareness1 + IUIPC.Awareness2 + IUIPC.Awareness3
     collection =~ IUIPC.Collection1 + IUIPC.Collection2 + IUIPC.Collection3 + IUIPC.Collection4
     privacy_concern =~ control + awareness + collection"

  sem.mod <- sem(model, data=mat, std.lv=T)
  print(summary(sem.mod, fit.measures=T))
  append <- predict(sem.mod, newdata=mat)
  cbind(mat, append)
}

plot.sbi.score.orig <- function(mat=orig.sbi.fac.anal()) {
  plot <- ggplot(mat, aes(x=sbi)) +
    geom_histogram() +
    xlab("Security Expertise") +
    theme(legend.position="bottom", legend.direction="horizontal") +
    guides(fill=guide_legend(title="Told Others"))

  ggsave("out/orig_sbi_score.png", plot, width=3, height=3)
}

plot.sbi.score.new <- function(mat=sebis.fac.anal()) {
  plot <- ggplot(mat, aes(x=sbi)) +
    geom_histogram() +
    xlab("Security Behavioral Intention") +
    theme(legend.position="bottom", legend.direction="horizontal")

  ggsave("out/sbi_score.png", plot, width=3, height=3)
}

plot.iuipc.score <- function(mat=iuipc.fac.anal()) {
  plot <- ggplot(mat, aes(x=privacy_concern)) +
    geom_histogram() +
    xlab("Privacy Concern") +
    theme(legend.position="bottom", legend.direction="horizontal")

  ggsave("out/iuipc_score.png", plot, width=3, height=3)
}

demographics <- function(dat=all.common.data, dem.cols=common.cat.demographic.cols()) {
  return(lapply(dat[, dem.cols], function(col) {
    table(col)
  }))
}

plot.demo <- function(dat=all.common.data) {
  dat[which(dat$Age == "Prefer not to answer"), "Age"] <- "Unknown"
  dat[which(dat$Gender == "Prefer not to answer"), "Gender"] <- "Unknown"
  plot <- ggplot(dat, aes(x=Age)) +
    geom_bar(position="stack", aes(fill=Gender)) +
    xlab("Age") +
    ylab("Count") +
    theme(
      axis.text.x = element_text(size=9, angle=45, hjust=1),
      axis.title.y = element_text(size=12),
      legend.direction="horizontal",
      legend.position="bottom",
      legend.title=element_blank()
    )
  ggsave(paste("out", "demo.png", sep="/"), plot, width=3, height=3)
}

plot.sebis.violin <- function(dat=all.new) {
  n <- nrow(all.new)
  base <- melt(all.new[, sebis.cols()])
  base <- cbind(base, sebis.var=SEBIS.cats.idx()[base$variable], sebis.type=SEBIS.cats()[base$variable])

  plot <- ggplot(base, aes(x=sebis.var, y=value)) +
    geom_violin() +
    scale_y_discrete(labels=SEBIS.scale.reverse.conversion()) +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=8, face='bold'),
      legend.position = "none"
    )
  ggsave("out/sebis_violin.png", plot, width=10, height=3)
}

plot.security.literacy <- function(mat=all.orig) {
  plot.hist(mat, "Literacy.Correct", "# Correct", "orig_literacy_hist.png")
}

plot.familiarity.violin <- function(mat=all.orig) {
  n <- nrow(mat)
  base <- melt(mat[, familiarity.cols()])
  base <- cbind(base, fam.var=familiarity.nice.rename()[base$variable])

  plot <- ggplot(base, aes(x=fam.var, y=value)) +
    geom_violin() +
    scale_y_discrete(labels=familiarity.reverse.map()) +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=8, face='bold'),
      legend.position = "none"
    )

  ggsave("out/familiairty_violin.png", plot, width=8, height=3)
}

plot.behavior.rates <- function(mat=all.orig) {
  n <- nrow(mat)
  sums <- colSums(mat[,behavior.selection.cols()])
  rates <- round(sums / n, 2)
  base <- data.frame(behavior=behavior.selection.nice.rename()[names(rates)], rate=rates)

  plot <- ggplot(base, aes(x=behavior, y=rate)) +
    geom_bar(stat="identity") +
    ylab("% Who Use") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=8, face='bold')
    )

  ggsave("out/behavior_rates.png", plot, width=8, height=3)
}

plot.sebis.scale <- function() {
  n <- nrow(all.new)
  base <- melt(all.new[, sebis.cols()])

  faceted <- cbind(
    base,
    row=c(rep(0, n*4), rep(1,n*4), rep(2,n*4), rep(3,n*4)),
    col=rep(c(rep(0, n), rep(1,n), rep(2,n), rep(3,n)), 4)
  )

  plot <- ggplot(faceted, aes(x=value)) + geom_histogram() + facet_grid(row ~ col, labeller = function(var, val) { "" })
  ggsave(paste("out", "sebis_grid.png", sep="/"), plot, width=3.5, height=3.5)
}

who.heard.about <- function(dat=all.common.data) {
  .sum.tf.col.spl(dat, "HeardAbout", c("Article"))
}

who.reshared <- function(dat=all.data) {
  .sum.tf.col.spl(dat, "ToldOthers", c("Article"))
}

who.reshared.that.heard.about <- function(dat=all.data) {
  .sum.tf.col.spl(dat, "ToldOthers", c("Article", "HeardAbout"))
}

.sum.tf.col.spl <- function(dat, col, cond) {
  ddply(dat, cond, function(x) {
    .tf.table.summary(table(x[[col]]))
  })
}

.tf.table.summary <- function(tab) {
  c(
    True=tab["TRUE"],
    False=tab["FALSE"],
    Rate=round(tab["TRUE"] / sum(tab), 2)
  )
}