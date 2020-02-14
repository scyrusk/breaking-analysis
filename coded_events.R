output.clusters <- function(mat=cd.df.tidy) {
  library(apcluster)
  clus <- apcluster(negDistMat(r=2), mat[,.coded.event.cols])
  mat <- cbind(mat, cluster=labels(clus))
  write.csv(mat, file="out/clustered_coded_events.csv")
}

make.event.graph <- function(mat=cd.df.tidy) {
  library(igraph)
  library(apcluster)

  rownames(mat) <- mat$EventName

  dist.mat <- corSimMat(mat[,.filtered.event.cols])
  dist.mat[lower.tri(dist.mat, diag=T)] <- NA
  g.edges <- melt(dist.mat, na.rm=T)
  colnames(g.edges) <- c("v1", "v2", "weight")
  # g.edges <- g.edges[which(g.edges$weight != 0),]
  abs.min <- abs(min(g.edges$weight))
  g.edges$weight <- (g.edges$weight + abs.min) / max(g.edges$weight + abs.min)
  print(nrow(g.edges))

  g <- graph.data.frame(g.edges, directed=F)
}

find.communities.in.event.graph <- function(g=make.event.graph(), df=cd.df.tidy) {
  c <- cluster_louvain(g)

  to.merge <- data.frame(EventName=c$names, community=.community.names[c$membership])
  event.communities <<- merge(df[,c("EventName", .filtered.event.cols)], to.merge)
  regression.clustered.events <<- merge(regression.coded.events, event.communities[,c("EventName", "community")])
}

.community.names <-
  c(
    "Financial Data Breaches",
    "Sensitive Systems and Data Breaches",
    "Corporate Personal Data Breaches",
    "Politicized / Activist Security"
  )

calc.col.rate.vs.event.type <- function(col, mat=regression.clustered.events) {
  totals <- table(mat[[col]])
  ret <- ddply(mat, .(community), function(x) {
    tab <- table(x[[col]])
    incl <- which(!names(totals) %in% names(tab))
    if (length(incl) > 0) {
      tab <- c(tab, totals[incl])
      tab[names(totals)[incl]] <- 0
      return(tab[sort(names(tab))])
    }
    tab
  })
  list(
    counts=ret,
    percents=data.frame(community=as.character(ret$community), t(apply(ret[,-1], 1, function(row) { round(row / sum(row), 2) }))),
    chisq=chisq.test(t(ret[,-1]))
  )
}

test.chisq.how.heard.vs.event.type <- function(mat=regression.clustered.events) {
  hh.chisqs <<- lapply(how.heard.cols(), function(hhcol) {
    dat <- t(ddply(regression.clustered.events, .(community), function(x) { table(x[[hhcol]]) })[,c(2,3)])
    list(
      data=dat,
      test=chisq.test(dat)
    )
  })
  names(hh.chisqs) <<- how.heard.cols()
}

filter.col.rates <- function(rates, cols) {
  rates[which(names(rates) %in% cols)]
}

heard.col.rates.by.event.type <- function() {
  hh.rates.by.event <<- lapply(c(
    "HowHeard.OnlineNews"="HowHeard.OnlineNews",
    "HowHeard.SomeoneElse"="HowHeard.SomeoneElse",
    "HowHeard.SocialMedia"="HowHeard.SocialMedia",
    "HowHeard.TVVideo"="HowHeard.TVVideo",
    "HowHeard.CompanyServiceProvider"="HowHeard.CompanyServiceProvider",
    "HowHeard.Other"="HowHeard.Other",
    "HeardFromWho.Friend"="HeardFromWho.Friend",
    "HeardFromWho.FamilyMember"="HeardFromWho.FamilyMember",
    "HeardFromWho.SignificantOther"="HeardFromWho.SignificantOther",
    "HeardFromWho.Colleague"="HeardFromWho.Colleague",
    "HeardFromWho.Other"="HeardFromWho.Other",
    "HeardFromHow.FaceToFace"="HeardFromHow.FaceToFace",
    "HeardFromHow.PhoneCall"="HeardFromHow.PhoneCall",
    "HeardFromHow.SMSEmail"="HeardFromHow.SMSEmail",
    "HeardFromHow.SocialMedia"="HeardFromHow.SocialMedia",
    "HeardFromHow.Other"="HeardFromHow.Other",
    # "HeardFromWhy.NoticedInsecureBehavior"="HeardFromWhy.NoticedInsecureBehavior",
    # "HeardFromWhy.ProvideInfoForProtection"="HeardFromWhy.ProvideInfoForProtection",
    # "HeardFromWhy.PersonallyExperienced"="HeardFromWhy.PersonallyExperienced",
    # "HeardFromWhy.ReadArticle"="HeardFromWhy.ReadArticle",
    # "HeardFromWhy.Unsure"="HeardFromWhy.Unsure",
    # "HeardFromWhy.Other"="HeardFromWhy.Other",
    "HeardFromWhat.GeneralInfo"="HeardFromWhat.GeneralInfo",
    "HeardFromWhat.SolutionsAdvice"="HeardFromWhat.SolutionsAdvice",
    "HeardFromWhat.Story"="HeardFromWhat.Story",
    "HeardFromWhat.Venting"="HeardFromWhat.Venting",
    "HeardFromWhat.Other"="HeardFromWhat.Other"
  ), function(col) {
    calc.col.rate.vs.event.type(col)
  })
}

told.col.rates.by.event.type <- function() {
  to.rates.by.event <<- lapply(c(
    "ToldWho.Friend"="ToldWho.Friend",
    "ToldWho.Family"="ToldWho.Family",
    "ToldWho.SignificantOther"="ToldWho.SignificantOther",
    "ToldWho.Colleague"="ToldWho.Colleague",
    "ToldWho.Other"="ToldWho.Other",
    "ToldHow.FaceToFace"="ToldHow.FaceToFace",
    "ToldHow.PhoneCall"="ToldHow.PhoneCall",
    "ToldHow.SMSEmail"="ToldHow.SMSEmail",
    "ToldHow.SocialMedia"="ToldHow.SocialMedia",
    "ToldHow.Other"="ToldHow.Other",
    "ToldWhy.NoticedInsecureBehavior"="ToldWhy.NoticedInsecureBehavior",
    "ToldWhy.ProvideInfoForProtection"="ToldWhy.ProvideInfoForProtection",
    "ToldWhy.FeltResponsibility"="ToldWhy.FeltResponsibility",
    "ToldWhy.PersonallyExperienced"="ToldWhy.PersonallyExperienced",
    "ToldWhy.ReadArticle"="ToldWhy.ReadArticle",
    "ToldWhy.Other"="ToldWhy.Other",
    "ToldWhat.GeneralInfo"="ToldWhat.GeneralInfo",
    "ToldWhat.SolutionsAdvice"="ToldWhat.SolutionsAdvice",
    "ToldWhat.Story"="ToldWhat.Story",
    "ToldWhat.Venting"="ToldWhat.Venting",
    "ToldWhat.Other"="ToldWhat.Other"
  ), function(col) {
    calc.col.rate.vs.event.type(col, mat=regression.clustered.events[regression.clustered.events$ToldOthers==T,])
  })
}

get.significant.cols.by.event.type <- function() {
  lapply(list(
    hh=heard.col.rates.by.event.type(),
    to=told.col.rates.by.event.type()
  ), function(lis) {
    Filter(function(x) { x$chisq$p.value <= 0.05 }, lis)
  })
}

logistic.to.regression.clustered.events <- function(
  mat=regression.clustered.events
) {
  library(lme4)
  toce.glm <<- glmer(ToldOthers ~ as.integer(Age) + Gender + sbi + community + (1|EventName), data=mat, family="binomial")

  contrast.matrix <- rbind(
    `community:Corporate Personal Data Breaches vs. community:Financial Data Breaches` = c(0, 0, 0, 0, 1, 0, 0),
    `community:Corporate Personal Data Breaches vs. community:Politicized / Activist Security` = c(0, 0, 0, 0, 0, 1, 0),
    `community:Corporate Personal Data Breaches vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, 0, 0, 1),
    `community:Financial Data Breaches vs. community:Politicized / Activist Security` = c(0, 0, 0, 0, -1, 1, 0),
    `community:Financial Data Breaches vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, -1, 0, 1),
    `community:Politicized / Activist Security vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, 0, -1, 1)
  )

  library(multcomp)
  toce.glm.comps <<- glht(toce.glm, contrast.matrix)
}

logistic.how.heard.regression.clustered.events <- function(
  mat=regression.clustered.events
) {
  library(multcomp)
  ivs <- "as.integer(Age) + Gender + sbi + community + (1|EventName)"
  hhce.glms <<- lapply(how.heard.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    mod <- glmer(form, data=mat, family="binomial")

    contrast.matrix <- rbind(
      `community:Corporate Personal Data Breaches vs. community:Financial Data Breaches` = c(0, 0, 0, 0, 1, 0, 0),
      `community:Corporate Personal Data Breaches vs. community:Politicized / Activist Security` = c(0, 0, 0, 0, 0, 1, 0),
      `community:Corporate Personal Data Breaches vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, 0, 0, 1),
      `community:Financial Data Breaches vs. community:Politicized / Activist Security` = c(0, 0, 0, 0, -1, 1, 0),
      `community:Financial Data Breaches vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, -1, 0, 1),
      `community:Politicized / Activist Security vs. community:Sensitive Systems and Data Breaches` = c(0, 0, 0, 0, 0, -1, 1)
    )

    list(
      mod=mod,
      pairwise=glht(mod, contrast.matrix)
    )
  })
  names(hhce.glms) <<- how.heard.cols()
}

logistic.told.who.regression.clustered.events <- function(
  mat=subset(regression.clustered.events, ToldOthers == T)
) {
  library(multcomp)
  ivs <- "as.integer(Age) + Gender + sbi + (1|EventName)"
  twce.glms <<- lapply(told.who.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    glmer(form, data=mat, family="binomial")
  })
  names(twce.glms) <<- told.who.cols()
}


logistic.rationale.regression.clustered.events <- function(
  mat=subset(regression.clustered.events, ToldOthers == T)
) {
  ivs <- "as.integer(Age) + Gender + sbi + (1|EventName)"
  lrce.glms <<- lapply(told.why.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    mod <- glmer(form, data=mat, family="binomial")
  })
  names(lrce.glms) <<- told.why.cols()
}