# How do age, gender, and security expertise / behavioral intention affect
# how people hear about security and privacy news events? (later: add event type)
.filter.prefer.not.to.ans <- function(mat) {
  subset(mat, Age != "Prefer not to answer" & Gender != "Prefer not to answer")
}

.make.regression.ready.datasets <- function(orig=all.orig, new=all.new) {
  list(
    orig = .filter.prefer.not.to.ans(
      .make.all.heard.about.or.recalled.other(
        orig.sbi.fac.anal(orig)
      )
    ),
    new =
      .filter.prefer.not.to.ans(
        .make.all.heard.about.or.recalled.other(
          iuipc.fac.anal(
            sebis.fac.anal(new)
          )
        )
      )[-802,]
  )
}

regression.ready.datasets <- function() {
  vals <- .make.regression.ready.datasets()
  regression.orig <<- vals[["orig"]]
  regression.new <<- vals[["new"]]
  common.cols <- colnames(regression.new)[which(colnames(regression.new) %in% colnames(regression.orig))]
  regression.common <<- rbind(regression.orig[, common.cols], regression.new[, common.cols])
  regression.coded.events <<- .create.coded.event.regmat()
  "done"
}

.create.coded.event.regmat <- function() {
  coded.events()
  add.event.name.to.regression.dataset()
  merge(regression.events, cd.df.tidy)
}

split.by.sbi <- function(mat=regression.clustered.events, tiles=c(0, 0.33, 0.67, 1.0)) {
  spls <- quantile(mat$sbi, tiles)
  sbi.quant <- ""
  sbi.quant[which(mat$sbi < spls[2])] <- "low"
  sbi.quant[intersect(
    which(mat$sbi >= spls[2]),
    which(mat$sbi < spls[3]))] <- "med"
  sbi.quant[which(mat$sbi >= spls[3])] <- "high"
  mat$sbi.quant <- factor(sbi.quant)

  ddply(mat, .(sbi.quant), function(x) {
    tmp <- ddply(x, .(community), function(y) { table(y$ToldOthers )})
    rates <- tmp[["TRUE"]] / (tmp[["FALSE"]] + tmp[["TRUE"]])
    names(rates) <- tmp$community
    rates
  })
}

logistic.how.heard.regression <- function(
  mat=regression.common
) {
  ivs <- "as.integer(Age) + Gender + sbi"
  hh.glms <<- lapply(how.heard.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    glm(form, data=mat, family="binomial")
  })
  names(hh.glms) <<- how.heard.cols()
}

# logistic.how.heard.regression.clustered.events <- function(
#   mat=regression.clustered.events
# ) {
#   ivs <- "as.integer(Age) + Gender + sbi + community"
#   hhce.glms <<- lapply(how.heard.cols(), function(dv) {
#     form <- as.formula(paste(dv, ivs, sep=" ~ "))
#     glm(form, data=mat, family="binomial")
#   })
#   names(hhce.glms) <<- how.heard.cols()
# }

logistic.to.regression <- function(
  mat=regression.common
) {
  to.glm <<- glm(ToldOthers ~ as.integer(Age) + Gender + sbi, data=mat, family="binomial")
}


logistic.told.why.regression <- function(
  mat=regression.common
) {
  ivs <- "as.integer(Age) + Gender + sbi"
  tw.glms <<- lapply(told.why.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    glm(form, data=mat, family="binomial")
  })
  names(tw.glms) <<- told.why.cols()
}

logistic.told.who.regression <- function(
  mat=regression.common
) {
  ivs <- "as.integer(Age) + Gender + sbi"
  twho.glms <<- lapply(told.who.cols(), function(dv) {
    form <- as.formula(paste(dv, ivs, sep=" ~ "))
    glm(form, data=mat, family="binomial")
  })
  names(twho.glms) <<- told.who.cols()
}

summarize.coeffs <- function(mod.list) {
  for (ix in seq_along(mod.list)) {
    print(names(mod.list)[ix])
    print("====")
    print(summary(mod.list[[ix]]))
  }
}

# What factors affect how people hear about security and privacy news events?
mcmc.how.heard.regression <- function(
  mat=regression.common
) {
  hh.mcmc <<- run.mcmc.regression(how.heard.cols(), mat, orig.ivs())
}

# What factors affect whether or not people reshare news events about security and privacy with others?
mcmc.to.regression <- function(
  # orig=regression.orig,
  # new=regression.new,
  mat=regression.common
) {
  # to.new <<- glm(ToldOthers ~ as.integer(Age) + Gender + sbi, data=regression.new, family="binomial")
  # to.old <<- glm(ToldOthers ~ as.integer(Age) + Gender + sbi, data=regression.orig, family="binomial")
  to.mcmc <<- glm(ToldOthers ~ as.integer(Age) + Gender + sbi, data=mat, family="binomial")
}

# mcmc.

# get model from running value.regression()
summarize.fixef <- function(model, dvs, include.cis=F) {
  sum <- summary(model)
  mat <- sum$solutions
  selection <- c("post.mean", "pMCMC")
  if (include.cis) {
    selection <- c(selection, "l-95% CI", "u-95% CI")
  }

  rv <- lapply(dvs, function(colname) {
    matches <- which(grepl(colname, rownames(mat)))
    sub <- mat[matches,selection]
    rownames(sub) <- gsub(paste("trait", colname, sep=""), colname, rownames(sub))
    round(sub, 3)
  })
  names(rv) <- dvs
  rv
}
# among the former 5 events
# among the latter 15 events

orig.ivs <- function() {
  c(
    "as.integer(Age)",
    "Gender",
    "sbi"
    # eventually add in event type cols
  )
}

new.ivs <- function() {
  c(
    "as.integer(age)",
    "gender",
    "sbi",
    "privacy_concern"
    # eventually add in event type cols
  )
}

ce.ivs <- function() {
  c(
    "as.integer(age)",
    "gender",
    "sbi",
    "community"
  )
}

# =================================
# Old

# How do demographics differ in how/why/from who they hear about
# security information?

# Are certain demographics more likely to hear about info?
# HeardAbout ~ Age + Gender + Literacy.Correct + Agg.Familiarity + Agg.Behaviors
heard.about.regression <- function(dat=all.data) {
  heard.about.model <<- glmer(
    formula=heard.about.formula(),
    data=.append.extra.cols(dat),
    family="binomial",
    verbose=T
  )
}

told.others.regression <- function(dat=all.data) {
  told.others.model <<- glmer(
    formula=told.others.formula(),
    data=.append.extra.cols(dat),
    family="binomial",
    verbose=T
  )
}

told.why.regression <- function(dat=all.da)

# TODO NEXT: run these models, import script to pretty format results, write
# models for sharing information as well.
heard.from.how.differences.regression <- function(data=filter.heard.and.append(all.data)) {
  hfh.diff.m <<- run.mcmc.regression(.remove.other(heard.from.how.cols()), data)
}

heard.what.differences.regression <- function(data=filter.heard.and.append(all.data)) {
  hfw.diff.m <<- run.mcmc.regression(.remove.other(heard.from.what.cols()), data)
}

how.heard.differences.regression <- function(data=filter.heard.and.append(all.data)) {
  hh.diff.m <<- run.mcmc.regression(.remove.other(how.heard.cols()), data)
}

heard.about.formula <- function() {
  .construct.glmer.formula("HeardAbout", heard.about.select.ivs())
}

told.others.formula <- function() {
  .construct.glmer.formula("ToldOthers", all.ivs())
}

# Did different demographics hear about these articles in
# different ways?
heard.differences.formula <- function(
  dv=c("Age", "Gender"),
  iv=c(
    how.heard.cols(),
    heard.from.how.cols(),
    heard.from.what.cols())
) {
  .construct.glmer.formula(dv[1], iv[1])
}

run.mcmc.hh.regression <- function(data=regression.clustered.events) {
  prior <- .construct.uninform.prior(how.heard.cols())
  data$iAge <- as.integer(data$Age)
  formula <-
    as.formula(paste(
      paste("cbind(", paste(how.heard.cols(), collapse=", "), ")", sep=""),
      paste(
        "-1",
        "trait",
        paste(paste("trait", c("iAge", "Gender", "sbi", "community"), sep=":"), collapse=" + "),
        sep=" + "
      ),
      sep=" ~ "
    ))

  print(formula)
  family <- rep("threshold", length(how.heard.cols()))

  MCMCglmm(
    formula,
    random=~EventName,
    rcov=~us(trait):units,
    prior=prior,
    family=family,
    nitt=5000,
    # burnin=10000,
    thin=25,
    data=data
  )
}

run.mcmc.regression <- function(dvs, data, ivs=diff.ivs()) {
  prior <- .construct.uninform.prior(dvs)
  formula <- .construct.mcmc.formulas(dvs, ivs)
  family <- rep("threshold", length(dvs))

  MCMCglmm(
    formula,
    random=~Article,
    rcov=~us(trait):units,
    prior=prior,
    family=family,
    # nitt=60000,
    # burnin=10000,
    thin=25,
    data=data
  )
}

.construct.mcmc.formulas <- function(dvs, traits) {
  as.formula(paste(
    paste("cbind(", paste(dvs, collapse=", "), ")", sep=""),
    paste(
      "-1",
      "trait",
      paste(paste("trait", traits, sep=":"), collapse=" + "),
      sep=" + "
    ),
    sep=" ~ "
  ))
}

.construct.uninform.prior <- function(dvs) {
  prior <- list(
    R=list(V=diag(length(dvs)), n=0.02), # residual covariance prior
    G=list(
      G1=list(V=1, n=0.02) # uninformative prior on random effect
    )
  )
}

.construct.glmer.formula <- function(dv, ivs) {
  ivs <- paste(ivs, collapse=" + ")
  ri <- "(1 | Article)"
  as.formula(paste(dv, paste(ivs, ri, sep="+"), sep=" ~ "))
}

all.ivs <- function() {
  c(
    numeric.demographic.cols(),
    "factor(Gender)",
    "Literacy.Correct",
    "AggBehaviors",
    "AggFamiliarity"
  )
}

heard.about.select.ivs <- function() {
  c(
    numeric.demographic.cols(),
    "factor(Gender)",
    "Literacy.Correct",
    "AggFamiliarity",
    "AggBehaviors",
    "NumOnlineComm"
  )
}

diff.ivs <- function() {
  c(
    numeric.demographic.cols(),
    "Gender",
    "Literacy.Correct",
    "AggFamiliarity",
    "AggBehaviors"
  )
}

# Helpers
.collapse.familiarity <- function(mat=all.data) {
  mat[,familiarity.cols()] <- sapply(mat[,familiarity.cols()], function(x) {
    ifelse(x > 3, 1, 0)
  })
  mat
}

.append.familiarity.agg <- function(mat=.collapse.familiarity(all.data)) {
  mat$AggFamiliarity <- rowSums(mat[,familiarity.cols()])
  mat
}

.append.behavior.agg <- function(mat=all.data) {
  mat$AggBehaviors <- rowSums(mat[,behavior.selection.cols()])
  mat
}

.append.extra.cols <- function(mat=all.data) {
  .append.behavior.agg(.append.familiarity.agg(.collapse.familiarity(mat)))
}

.filter.heard.about.only <- function(mat) {
  mat[which(mat$HeardAbout),]
}

.remove.other <- function(vals) {
  vals[-which(grepl("Other", vals))]
}

.filter.heard.and.append <- function(mat=all.data) {
  .filter.heard.about.only(.append.extra.cols(mat))
}


# Coded Event cols
.responsible.party.cols <-
  c(
    "corporation",
    "domestic government",
    "foreign government",
    "hacktivists / whistleblowers / vigilantes",
    "journalist",
    "named hacker / researcher",
    "unnamed hacker / researcher"
  )

.intention.cols <-
  c(
    "constructive",
    "destructive",
    "unclear"
  )

.topic.cols <-
  c(
    "account credentials leak",
    "celebrity data breach",
    "corporate data breach",
    "cyberwarfare",
    "denial of service",
    "financial data / resource breach",
    "going dark problem / government surveillance",
    "government data / resource breach",
    "legislation",
    "personal account / data breach",
    "ransomware",
    "security implementation bug"
  )

.context.cols <-
  c(
    "big-brother government",
    "children",
    "corporate security",
    "election",
    "financial institution",
    "foreign affairs",
    "gaming",
    "government / national security",
    "medical institution",
    "opinion journalism",
    "point-of-sale hack",
    "race violence",
    "sexual life",
    "social media",
    "stale software",
    "terrorism"
  )

.coded.event.cols <-
  c(
    .responsible.party.cols,
    .intention.cols,
    .topic.cols,
    .context.cols
  )

.unused.event.cols <-
  c(
    "named hacker / researcher",
    "unnamed hacker / researcher",
    .intention.cols,
    "corporate data breach",
    "stale software",
    "social media",
    "opinion journalism"
  )

.filtered.event.cols <- .coded.event.cols[which(!.coded.event.cols %in% .unused.event.cols)]