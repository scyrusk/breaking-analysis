coalesce <- function(...) {
  apply(cbind(...), 1, function(x) x[which(!is.na(x))[1]])
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

'%=%' = function(l, r, ...) UseMethod('%=%')

l = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  List
}

'%=%.lbunch' <- function(l, r, ...) {
  Envir = as.environment(-1)

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

split.vec.into.chunks <- function(vec, nchunks) {
  mx <- length(vec) / nchunks
  split(vec, ceiling(seq_along(vec)/mx))
}

split.matrix.by.columns <- function(mat, nchunks) {
  tmp.vec <- 1:ncol(mat)
  col.idxs <- split.vec.into.chunks(tmp.vec, nchunks)
  lapply(col.idxs, function(x) {
    mat[,x]
  })
}

split.matrix.by.rows <- function(mat, nchunks) {
  tmp.vec <- 1:nrow(mat)
  col.idxs <- split.vec.into.chunks(tmp.vec, nchunks)
  lapply(col.idxs, function(x) {
    mat[x,]
  })
}

remove.na.columns <- function(df) {
  df[,colSums(is.na(df))<nrow(df)]
}

get.common.cols <- function(mat.list) {
  Reduce(function(x, y) intersect(x,y), lapply(mat.list, colnames))
}

# desired.order should be the levels of the column in the desired order
# col should already be a factor
# usage: x$y <- ordered.factor.levels(c("a", "b", "c"), x$y))
ordered.factor.levels <- function(desired.order, col) {
  levs <- sapply(desired.order, function(x) {
    which(levels(col) == x)
  })
  ordered(col, levels=levs)
}

change.factor.baseline <- function(baseline, col) {
  olevs <- levels(col)
  ix <- which(olevs == baseline)
  if (length(ix) == 0) return(col)
  factor(col, levels=c(olevs[ix], olevs[-ix]))
}

clean.col.names <- function() {
  c(
    "ResponseID",
    "DateSubmitted",
    "HeardAbout",
    "RecallOtherIncident",
    "LinkToOtherIncident",
    "HowHeard.OnlineNews",
    "HowHeard.SomeoneElse",
    "HowHeard.SocialMedia",
    "HowHeard.TVVideo",
    "HowHeard.CompanyServiceProvider",
    "HowHeard.Other",
    "HowHeard.WhichCompanyServiceProvider",
    "HowHeard.WhichOther",
    "HeardFromWho.Friend",
    "HeardFromWho.FamilyMember",
    "HeardFromWho.SignificantOther",
    "HeardFromWho.Colleague",
    "HeardFromWho.Other",
    "HeardFromWho.WhichOther",
    "HeardFromHow.FaceToFace",
    "HeardFromHow.PhoneCall",
    "HeardFromHow.SMSEmail",
    "HeardFromHow.SocialMedia",
    "HeardFromHow.Other",
    "HeardFromHow.WhichOther",
    "HeardFromWhy.NoticedInsecureBehavior",
    "HeardFromWhy.ProvideInfoForProtection",
    "HeardFromWhy.PersonallyExperienced",
    "HeardFromWhy.ReadArticle",
    "HeardFromWhy.Unsure",
    "HeardFromWhy.Other",
    "HeardFromWhy.WhichOther",
    "HeardFromWhat.GeneralInfo",
    "HeardFromWhat.SolutionsAdvice",
    "HeardFromWhat.Story",
    "HeardFromWhat.Venting",
    "HeardFromWhat.Other",
    "HeardFromWhat.WhichOther",
    "ToldOthers",
    "ToldWho.Friend",
    "ToldWho.Family",
    "ToldWho.SignificantOther",
    "ToldWho.Colleague",
    "ToldWho.Other",
    "ToldWho.WhichOther",
    "ToldHow.FaceToFace",
    "ToldHow.PhoneCall",
    "ToldHow.SMSEmail",
    "ToldHow.SocialMedia",
    "ToldHow.Other",
    "ToldHow.WhichOther",
    "ToldWhy.NoticedInsecureBehavior",
    "ToldWhy.ProvideInfoForProtection",
    "ToldWhy.FeltResponsibility",
    "ToldWhy.PersonallyExperienced",
    "ToldWhy.ReadArticle",
    "ToldWhy.Other",
    "ToldWhy.WhichOther",
    "ToldWhat.GeneralInfo",
    "ToldWhat.SolutionsAdvice",
    "ToldWhat.Story",
    "ToldWhat.Venting",
    "ToldWhat.Other",
    "ToldWhat.WhichOther",
    "OtherInfoOpenEnded",
    "CompLiteracy",
    "NetLiteracy",
    "NumOnlineComm",
    "Familiarity.IPAddr",
    "Familiarity.Cookies",
    "Familiarity.SSL",
    "Familiarity.VPN",
    "Familiarity.Encryption",
    "Familiarity.ProxyServer",
    "Familiarity.Tor",
    "Familiarity.BrowserPrivacy",
    "Familiarity.PrivateBrowsing",
    "Literacy.PrivateBrowsing",
    "Literacy.Cookies",
    "Literacy.Encryption",
    "Literacy.Tor",
    "Literacy.VPNProxyEqual",
    "Literacy.IPID",
    "Literacy.HTTPS",
    "Literacy.ProxyTrace",
    "Behavior.TempNameEmail",
    "Behavior.FakeNameUN",
    "Behavior.FalseInfo",
    "Behavior.BrowserCookieOff",
    "Behavior.CookieClear",
    "Behavior.TorVPN",
    "Behavior.EncryptedMail",
    "Behavior.StopUseRealName",
    "Behavior.DeletedPost",
    "Behavior.AskedToRemovePosts",
    "Behavior.PublicCPU",
    "Gender",
    "Age",
    "RelationshipStatus",
    "ParentUnder18",
    "HighestDegree",
    "Occupation",
    "Occuptation.Other",
    "Nationality",
    "NativeEnglish",
    "Ethnicity"
  )
}

new.clean.col.names <- function() {
  c(
    "ResponseID",
    "DateSubmitted",
    "State",
    "HeardAbout",
    "RecallOtherIncident",
    "LinkToOtherIncident",
    "HowHeard.OnlineNews",
    "HowHeard.SomeoneElse",
    "HowHeard.SocialMedia",
    "HowHeard.TVVideo",
    "HowHeard.CompanyServiceProvider",
    "HowHeard.Other",
    "HowHeard.WhichCompanyServiceProvider",
    "HowHeard.WhichOther",
    "HeardFromWho.Friend",
    "HeardFromWho.FamilyMember",
    "HeardFromWho.SignificantOther",
    "HeardFromWho.Colleague",
    "HeardFromWho.Other",
    "HeardFromWho.WhichOther",
    "HeardFromHow.FaceToFace",
    "HeardFromHow.PhoneCall",
    "HeardFromHow.SMSEmail",
    "HeardFromHow.SocialMedia",
    "HeardFromHow.Other",
    "HeardFromHow.WhichOther",
    "HeardFromWhy.NoticedInsecureBehavior",
    "HeardFromWhy.ProvideInfoForProtection",
    "HeardFromWhy.PersonallyExperienced",
    "HeardFromWhy.ReadArticle",
    "HeardFromWhy.Unsure",
    "HeardFromWhy.Other",
    "HeardFromWhy.WhichOther",
    "HeardFromWhat.GeneralInfo",
    "HeardFromWhat.SolutionsAdvice",
    "HeardFromWhat.Story",
    "HeardFromWhat.Venting",
    "HeardFromWhat.Other",
    "HeardFromWhat.WhichOther",
    "ToldOthers",
    "ToldWho.Friend",
    "ToldWho.Family",
    "ToldWho.SignificantOther",
    "ToldWho.Colleague",
    "ToldWho.Other",
    "ToldWho.WhichOther",
    "ToldHow.FaceToFace",
    "ToldHow.PhoneCall",
    "ToldHow.SMSEmail",
    "ToldHow.SocialMedia",
    "ToldHow.Other",
    "ToldHow.WhichOther",
    "ToldWhy.NoticedInsecureBehavior",
    "ToldWhy.ProvideInfoForProtection",
    "ToldWhy.FeltResponsibility",
    "ToldWhy.PersonallyExperienced",
    "ToldWhy.ReadArticle",
    "ToldWhy.Other",
    "ToldWhy.WhichOther",
    "ToldWhat.GeneralInfo",
    "ToldWhat.SolutionsAdvice",
    "ToldWhat.Story",
    "ToldWhat.Venting",
    "ToldWhat.Other",
    "ToldWhat.WhichOther",
    "OtherInfoOpenEnded",
    "SEBIS.ChangePasswords",
    "SEBIS.DiffPasswords",
    "SEBIS.NewPasswords",
    "SEBIS.SpecialCharPasswords",
    "SEBIS.LinkVerify",
    "SEBIS.AestheticVerify",
    "SEBIS.HTTPSVerify",
    "SEBIS.LinkMouseover",
    "SEBIS.ReportSecurity",
    "SEBIS.ImmediateUpdate",
    "SEBIS.UpdatedSoftware",
    "SEBIS.AVAutoUpdates",
    "SEBIS.AutoLock",
    "SEBIS.LaptopAuth",
    "SEBIS.ManualLock",
    "SEBIS.MobileAuth",
    "IUIPC.Control1",
    "IUIPC.Control2",
    "IUIPC.Control3",
    "IUIPC.Awareness1",
    "IUIPC.Awareness2",
    "IUIPC.Awareness3",
    "IUIPC.Collection1",
    "IUIPC.Collection2",
    "IUIPC.Collection3",
    "IUIPC.Collection4",
    "Gender",
    "Age",
    "Occupation",
    "Occuptation.Other",
    "Nationality",
    "NativeEnglish",
    "Ethnicity.Asian",
    "Ethnicity.Black",
    "Ethnicity.White",
    "Ethnicity.Hisp",
    "Ethnicity.Native",
    "Ethnicity.Other",
    "Ethnicity.Other2"
  )
}

clean.col.classes <- function() {
  c(
    "integer",
    "character",
    "factor",
    "factor",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "character",
    "integer",
    "integer",
    "integer",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "factor",
    "integer",
    "factor",
    "logical",
    "factor",
    "factor",
    "character",
    "factor",
    "logical",
    "factor"
  )
}