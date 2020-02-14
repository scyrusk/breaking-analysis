base.dir <- function(orig=T) {
  paste("raw_data", ifelse(orig, "original_dissem_surveys", "new_dissem_surveys"), sep="/")
}

behavior.dir <- function() {
  "raw_data/behavioral"
}

behavioral.rebase <- function() {
  behavioral.data <<-  read.csv(paste(behavior.dir(), "tech_usage_subset_2.csv", sep="/"), header=T)
}

event.names <- function() {
  ev.names.df <<- read.csv("raw_data/events/other_links_event_names.csv")
}

coded.events <- function() {
  library(dplyr)
  library(tidyr)

  cd.df <<- read.csv("raw_data/events/coded_events.csv")
  tidy.coded.events()
}

tidy.coded.events <- function(mat=cd.df) {
  rp <- mat %>%
    separate(ResponsibleParties, into = c("rp1", "rp2"), sep = ";;", fill="right") %>%
    gather(rp,name,starts_with("rp")) %>%
    mutate(present = 1) %>%
    select(-rp) %>%
    spread(name,present,fill = 0)

  tp <- rp %>%
    separate(Topic, into = c("tp1", "tp2"), sep = ";;", fill="right") %>%
    gather(tp,topic,starts_with("tp")) %>%
    mutate(present = 1) %>%
    select(-tp) %>%
    spread(topic,present,fill = 0)

  intention <- tp %>%
    separate(Intention, into = c("int1"), sep = ";;", fill="right") %>%
    gather(int,intention,starts_with("int")) %>%
    mutate(present = 1) %>%
    select(-int) %>%
    spread(intention,present,fill = 0)

  context <- intention %>%
    separate(Context, c("ctx1", "ctx2", "ctx3"), sep = ";;", fill="right") %>%
    gather(ctx,context,starts_with("ctx")) %>%
    mutate(id=1:n(), present = 1) %>%
    select(-ctx) %>%
    spread(context,present,fill = 0)

  cull.indices <- union(
    union(
      grep('N\\/?A',colnames(context)),
      grep('id',colnames(context))
    ),
    which(colnames(context) == "")
  )

  cd.df.tidy <<- context[1:nrow(mat), -cull.indices]
}

add.event.name.to.regression.dataset <- function(en=ev.names.df, reg=regression.common, testAlignment=F) {
  # assumes alignment between en and reg s.t. rows in en match rows in subset(reg, RecallOtherIncident == T & LinkToOtherIncident != "")
  # to test this alignment, run with testAlignment = T
  base.mapping <- c(
    "AFF Hack"="Adult Friend Finder Hack",
    "Apple letter"="Apple's Letter on Encryption to Customers",
    "Brazzers hack"="Brazzers Hack",
    "CENTCOM"="CENTCOM Social Media Hack",
    "DEA Spy"="DEA Driver Spying",
    "Dropbox hack"="Dropbox Hack",
    "FBI Leak"="FBI Employee Hack",
    "French Wifi"="France Blocking Free WiFi",
    "GoGo SSL"="GoGo Fake SSL Certs",
    "Gov't Anounce"="Obama Security Announcement",
    "LA Ransom"="Ransomware in LA Hospital",
    "Mexico Breach"="93.4 mil Mexican's Info Exposed on Amazon",
    "Mirai Krebs"="Krebs Mirai IoT DDoS Attack",
    "Oliver Encryption"="John Oliver Encryption Skit",
    "Other"="Other",
    "Philippines Breach"="55 mil Filipino Voter Info Leak",
    "Sony hack"="Sony Pictures Entertainment Hacked",
    "Voter Records"="191 mil Voter Records Exposed",
    "Yahoo 500mil"="500 million Yahoo Accounts",
    "Yahoo Google emails"="Yahoo Google Email Security Breach"
  )

  reg$EventName <- as.character(base.mapping[as.character(reg$Article)])

  if (testAlignment) {
    sub <- subset(reg, RecallOtherIncident == T & LinkToOtherIncident != "")
    return(all(as.character(en$Article.Link) == as.character(sub$LinkToOtherIncident)))
  } else {
    sub.idx <- intersect(which(reg$RecallOtherIncident == T), which(reg$LinkToOtherIncident != ""))
    reg$EventName[sub.idx] <- as.character(en$Event.Name)
    regression.events <<- reg
    return(T)
  }
}

rebase <- function() {
  .rebase <- function(orig=T) {
    cns <- new.clean.col.names()
    if (orig) cns <- clean.col.names()
    data.files <- Filter(function(x) { !file.info(x)$isdir }, paste(base.dir(orig), dir(base.dir(orig)), sep="/"))
    print(data.files)
    data.list <- lapply(data.files, function(x) {
      mat <- read.csv(x, header=T, col.names=cns)
      preprocess(mat, orig)
    })
    names(data.list) <- sapply(strsplit(data.files, "/"), function(x) { x[3]} )
    combined <- do.call("rbind", lapply(seq_along(data.list), function(ix) {
      article <- names(data.list)[ix]
      mat <- data.list[[ix]]
      mat$Article <- article.nice.rename()[ifelse(mat$HeardAbout, article, "Other")]
      mat
    }))
    list(
      data.list=data.list,
      combined=combined
    )
  }
  l(orig.list, orig.combined) %=% .rebase(T)
  l(new.list, new.combined) %=% .rebase(F)

  data.list <<- c(orig.list, new.list)
  all.orig <<- orig.combined
  all.new <<- new.combined

  common.cols <- colnames(all.new)[which(colnames(all.new) %in% colnames(all.orig))]
  all.common.data <<- rbind(all.orig[, common.cols], all.new[, common.cols])

  # preprocess to not double count online news from social media
  ix <- which(all.common.data$HowHeard.OnlineNews == 1)
  all.common.data[ix, "HowHeard.OnlineNews"] <<-
    all.common.data[ix, "HowHeard.OnlineNews"] - all.common.data[ix, "HowHeard.SocialMedia"]
  .make.all.heard.about.or.recalled.other(all.common.data)
}

.make.all.heard.about.or.recalled.other <- function(mat=all.common.data) {
  all.heard.about.or.recalled.other <<-
    subset(mat, (HeardAbout | RecallOtherIncident) & Article != 'Anon v ISIS')
}

behavioral.preprocess <- function(mat=behavioral.data) {
  yes.no.map <- function() { c("Yes"=T, "No"=F, " "=NA) }
  selection.convert <- function(col) {
    col <- as.integer(ifelse(col=="", 0, 1))
    col[which(is.na(col))] <- 0
    col
  }
  scale.convert <- function(col, scale) {
    scale[as.character(col)]
  }

  # Convert yes/no cols to T/F
  yn.cols <- get.matching.cols(colnames(mat), behavioral.yes.no.cols())
  mat[,yn.cols] <- sapply(mat[,yn.cols], function(x) {
    yes.no.map()[as.character(x)]
  })

  # Convert checkbox selection cols to 0/1
  as.cols <- get.matching.cols(colnames(mat), behavioral.selection.cols())
  mat[,as.cols] <- sapply(mat[,as.cols], function(x) {
    selection.convert(x)
  })

  mat[,familiarity.cols()] <- sapply(mat[,familiarity.cols()], function(x) {
    # ordered(familiarity.map()[as.character(x)]) # ordered is technically more accurate
    as.integer(familiarity.map()[as.character(x)]) # but integer is simpler
  })
  mat$NumOnlineComm <- change.factor.baseline("None", mat$NumOnlineComm)
  mat$RelationshipStatus <- change.factor.baseline("Currently single", mat$RelationshipStatus)
  mat <- cbind(mat, literacy.score.computation(mat))

  mat[,sebis.cols()] <- sapply(mat[,sebis.cols()], function(x) scale.convert(x, SEBIS.scale.conversion()))
  mat
}

preprocess <- function(mat, orig=T) {
  get.matching.cols <- function(cns, comparison) {
    which(cns %in% comparison)
  }

  yes.no.map <- function() { c("Yes"=T, "No"=F, " "=NA) }

  selection.convert <- function(col) {
    col <- as.integer(ifelse(col=="", 0, 1))
    col[which(is.na(col))] <- 0
    col
  }

  scale.convert <- function(col, scale) {
    scale[as.character(col)]
  }

  discretize.age <- function(age.col) {
    mapper <- function(val) {
      if (val <= 24) {
        return("18-24")
      } else if (val <= 29) {
        return("25-29")
      } else if (val <= 34) {
        return("30-34")
      } else if (val <= 44) {
        return("35-44")
      } else if (val <= 54) {
        return("45-54")
      } else {
        return("55+")
      }
    }
    if (is.numeric(age.col)) {
      age.col[age.col > 1000] <- 2015 - age.col[age.col > 1000] # transform people who entered birthyear
      return(factor(sapply(age.col, mapper), levels=c("18-24", "25-29", "30-34", "35-44", "45-54", "55+")))
    }
  }

  # Convert yes/no cols to T/F
  yn.cols <- get.matching.cols(colnames(mat), yes.no.cols())
  mat[,yn.cols] <- sapply(mat[,yn.cols], function(x) {
    yes.no.map()[as.character(x)]
  })

  # Convert checkbox selection cols to 0/1
  as.cols <- get.matching.cols(colnames(mat), all.selection.cols())
  mat[,as.cols] <- sapply(mat[,as.cols], function(x) {
    selection.convert(x)
  })

  if (orig) {
    mat[,familiarity.cols()] <- sapply(mat[,familiarity.cols()], function(x) {
      # ordered(familiarity.map()[as.character(x)]) # ordered is technically more accurate
      as.integer(familiarity.map()[as.character(x)]) # but integer is simpler
    })
    # reorder factor levels
    # num online communities
    mat$NumOnlineComm <- change.factor.baseline("None", mat$NumOnlineComm)
    mat$RelationshipStatus <- change.factor.baseline("Currently single", mat$RelationshipStatus)
    mat$Age <- discretize.age(mat$Age)
    mat <- cbind(mat, literacy.score.computation(mat))
  } else {
    # new surveys with SEBIS / IUIPC
    # compute SEBIS / IUIPC scores point-value scores (perhaps through factor analysis)
    mat[,sebis.cols()] <- sapply(mat[,sebis.cols()], function(x) scale.convert(x, SEBIS.scale.conversion()))
    mat[,iuipc.cols()] <- sapply(mat[,iuipc.cols()], function(x) scale.convert(x, IUIPC.scale.conversion()))
  }
  mat
}

literacy.score.computation <- function(mat) {
  lcs <- literacy.cols()
  sum.cols <- c()
  for (ix in seq_along(lcs)) {
    cn <- lcs[ix]
    ncn <- paste(cn, "Correct", sep=".")
    sum.cols <- c(sum.cols, ncn)
    mat[[ncn]] <- toupper(mat[[cn]]) == toupper(literacy.answer.key()[cn])
  }

  num.corr <- apply(mat[, sum.cols], 1, function(x) {
    length(which(x))
  })

  num.not.sure <- apply(mat[, lcs], 1, function(x) {
    length(which(toupper(x) == toupper("I'm not sure")))
  })

  data.frame(
    Literacy.Correct=num.corr,
    Literacy.NotSure=num.not.sure
  )
}

append.literacy.correct.cols <- function(mat=all.orig) {
  lcs <- literacy.cols()
  for (ix in seq_along(lcs)) {
    cn <- lcs[ix]
    ncn <- paste(cn, "Correct", sep=".")
    mat[[ncn]] <- ifelse(toupper(mat[[cn]]) == toupper(literacy.answer.key()[cn]), 1, 0)
  }
  mat
}