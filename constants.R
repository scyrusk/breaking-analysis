social.selection.cols <- function() {
  c(
    "HowHeard.OnlineNews",
    "HowHeard.SomeoneElse",
    "HowHeard.SocialMedia",
    "HowHeard.TVVideo",
    "HowHeard.CompanyServiceProvider",
    "HowHeard.Other",
    "HeardFromWho.Friend",
    "HeardFromWho.FamilyMember",
    "HeardFromWho.SignificantOther",
    "HeardFromWho.Colleague",
    "HeardFromWho.Other",
    "HeardFromHow.FaceToFace",
    "HeardFromHow.PhoneCall",
    "HeardFromHow.SMSEmail",
    "HeardFromHow.SocialMedia",
    "HeardFromHow.Other",
    "HeardFromWhy.NoticedInsecureBehavior",
    "HeardFromWhy.ProvideInfoForProtection",
    "HeardFromWhy.PersonallyExperienced",
    "HeardFromWhy.ReadArticle",
    "HeardFromWhy.Unsure",
    "HeardFromWhy.Other",
    "HeardFromWhat.GeneralInfo",
    "HeardFromWhat.SolutionsAdvice",
    "HeardFromWhat.Story",
    "HeardFromWhat.Venting",
    "HeardFromWhat.Other",
    "ToldWho.Friend",
    "ToldWho.Family",
    "ToldWho.SignificantOther",
    "ToldWho.Colleague",
    "ToldWho.Other",
    "ToldHow.FaceToFace",
    "ToldHow.PhoneCall",
    "ToldHow.SMSEmail",
    "ToldHow.SocialMedia",
    "ToldHow.Other",
    "ToldWhy.NoticedInsecureBehavior",
    "ToldWhy.ProvideInfoForProtection",
    "ToldWhy.FeltResponsibility",
    "ToldWhy.PersonallyExperienced",
    "ToldWhy.ReadArticle",
    "ToldWhy.Other",
    "ToldWhat.GeneralInfo",
    "ToldWhat.SolutionsAdvice",
    "ToldWhat.Story",
    "ToldWhat.Venting",
    "ToldWhat.Other"
  )
}

behavior.selection.cols <- function() {
  c(
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
    "Behavior.PublicCPU"
  )
}

familiarity.cols <- function() {
  c(
    "Familiarity.IPAddr",
    "Familiarity.Cookies",
    "Familiarity.SSL",
    "Familiarity.VPN",
    "Familiarity.Encryption",
    "Familiarity.ProxyServer",
    "Familiarity.Tor",
    "Familiarity.BrowserPrivacy",
    "Familiarity.PrivateBrowsing"
  )
}

familiarity.map <- function() {
  c(
    "I have never heard of this"=1,
    "I have heard of this but I don't know what it is"=2,
    "I know what this is but I don't know how it works"=3,
    "I know generally how it works"=4,
    "I know very well how this works"=5
  )
}

familiarity.reverse.map <- function() {
  c(
    "Never heard",
    "Heard don't know",
    "Know concept",
    "Know workings",
    "Know well"
  )
}

literacy.cols <- function() {
  c(
    "Literacy.PrivateBrowsing",
    "Literacy.Cookies",
    "Literacy.Encryption",
    "Literacy.Tor",
    "Literacy.VPNProxyEqual",
    "Literacy.IPID",
    "Literacy.HTTPS",
    "Literacy.ProxyTrace"
  )
}

literacy.answer.key <- function() {
  c(
    "Literacy.PrivateBrowsing"="FALSE",
    "Literacy.Cookies"="TRUE",
    "Literacy.Encryption"="TRUE",
    "Literacy.Tor"="TRUE",
    "Literacy.VPNProxyEqual"="FALSE",
    "Literacy.IPID"="FALSE",
    "Literacy.HTTPS"="TRUE",
    "Literacy.ProxyTrace"="FALSE"
  )
}

iuipc.cols <- function() {
  c(
    "IUIPC.Control1",
    "IUIPC.Control2",
    "IUIPC.Control3",
    "IUIPC.Awareness1",
    "IUIPC.Awareness2",
    "IUIPC.Awareness3",
    "IUIPC.Collection1",
    "IUIPC.Collection2",
    "IUIPC.Collection3",
    "IUIPC.Collection4"
  )
}

sebis.cols <- function() {
  c(
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
    "SEBIS.MobileAuth"
  )
}

numeric.demographic.cols <- function() {
  c("Age")
}

categorical.demographic.cols <- function() {
  c(
    "Gender",
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

common.cat.demographic.cols <- function() {
  c(
    "Age",
    "Gender",
    "Occupation",
    "NativeEnglish"
  )
}

yes.no.cols <- function() {
  c(
    "HeardAbout",
    "RecallOtherIncident",
    "ToldOthers",
    "ParentUnder18",
    "NativeEnglish"
  )
}

all.selection.cols <- function() {
  c(social.selection.cols(),
    behavior.selection.cols(), binary.ethnicity.cols())
}

how.heard.cols <- function() {
  c(
    "HowHeard.OnlineNews",
    "HowHeard.SomeoneElse",
    "HowHeard.SocialMedia",
    "HowHeard.TVVideo",
    "HowHeard.CompanyServiceProvider",
    "HowHeard.Other"
  )
}

how.heard.nice.rename <- function() {
  c(
    "SomeoneElse"="Person",
    "SocialMedia"="Social media",
    "OnlineNews"="Online news",
    "TVVideo"="TV news",
    "CompanyServiceProvider"="Provider",
    "Other"="Other"
  )
}

social.how.heard.cols <- function() {
  c(
    "HowHeard.SomeoneElse",
    "HowHeard.SocialMedia"
  )
}

heard.from.who.cols <- function() {
  c(
    "HeardFromWho.Friend",
    "HeardFromWho.FamilyMember",
    "HeardFromWho.SignificantOther",
    "HeardFromWho.Colleague",
    "HeardFromWho.Other"
  )
}

heard.who.nice.rename <- function() {
  c(
    "Friend"="Friend",
    "FamilyMember"="Family",
    "SignificantOther"="Sig. Other",
    "Colleague"="Colleague",
    "Other"="Other"
  )
}

heard.from.how.cols <- function() {
  c(
    "HeardFromHow.FaceToFace",
    "HeardFromHow.PhoneCall",
    "HeardFromHow.SMSEmail",
    "HeardFromHow.SocialMedia",
    "HeardFromHow.Other"
  )
}

heard.how.nice.rename <- function() {
  c(
    "FaceToFace"="Face to face",
    "PhoneCall"="Phone call",
    "SMSEmail"="SMS/email",
    "SocialMedia"="Social media",
    "Other"="Other"
  )
}

heard.from.why.cols <- function() {
  c(
    "HeardFromWhy.NoticedInsecureBehavior",
    "HeardFromWhy.ProvideInfoForProtection",
    "HeardFromWhy.PersonallyExperienced",
    "HeardFromWhy.ReadArticle",
    "HeardFromWhy.Unsure",
    "HeardFromWhy.Other"
  )
}

heard.why.nice.rename <- function() {
  c(
    "NoticedInsecureBehavior"="Noticed insecure",
    "ProvideInfoForProtection"="Info for protection",
    "PersonallyExperienced"="Share experience",
    "ReadArticle"="Read article",
    "Unsure"="Unclear",
    "Other"="Other"
  )
}

heard.from.what.cols <- function() {
  c(
    "HeardFromWhat.GeneralInfo",
    "HeardFromWhat.SolutionsAdvice",
    "HeardFromWhat.Story",
    "HeardFromWhat.Venting",
    "HeardFromWhat.Other"
  )
}

heard.what.nice.rename <- function() {
  c(
    "GeneralInfo"="General info",
    "SolutionsAdvice"="Provide advice",
    "Story"="Story",
    "Venting"="Venting",
    "Other"="Other"
  )
}

extract.last.in.path <- function(vec, path.sep="\\.") {
  sapply(strsplit(vec, path.sep), function(x) { x[length(x)]} )
}

fac.nice.rename <- function(orig, rn.func) {
  factor(rn.func()[as.character(orig)], rn.func())
}

article.nice.rename <- function() {
  c(
    "CENTCOM.csv"="CENTCOM",
    "Obama.csv"="Gov't Anounce",
    "GoGo.csv"="GoGo SSL",
    "DEA.csv"="DEA Spy",
    "Sony.csv"="Sony hack",
    "anon_isis.csv"="Anon v ISIS",
    "apple_letter.csv"="Apple letter",
    "fbi_breach.csv"="FBI Leak",
    "france_wifi.csv"="French Wifi",
    "hospital_ransomware.csv"="LA Ransom",
    "mexican_voters.csv"="Mexico Breach",
    "oliver_skit.csv"="Oliver Encryption",
    "phillippines.csv"="Philippines Breach",
    "voter_records.csv"="Voter Records",
    "adult_friend_finder.csv"="AFF Hack",
    "krebs_ddos.csv"="Mirai Krebs",
    "yahoo_500mil.csv"="Yahoo 500mil",
    "yahoo_google_email.csv"="Yahoo Google emails",
    "brazzers.csv"="Brazzers hack",
    "dropbox_hack.csv"="Dropbox hack",
    "aggregated"="Aggregated",
    "Other"="Other"
  )
}

told.who.cols <- function() {
  c(
    "ToldWho.Friend",
    "ToldWho.Family",
    "ToldWho.SignificantOther",
    "ToldWho.Colleague",
    "ToldWho.Other"
  )
}

told.who.nice.rename <- function() {
  c(
    "Friend"="Friend",
    "Family"="Family",
    "SignificantOther"="Sig. other",
    "Colleague"="Colleague",
    "Other"="Other"
  )
}

told.how.cols <- function() {
  c(
    "ToldHow.FaceToFace",
    "ToldHow.PhoneCall",
    "ToldHow.SMSEmail",
    "ToldHow.SocialMedia",
    "ToldHow.Other"
  )
}

told.how.nice.rename <- function() {
  c(
    "FaceToFace"="Face to face",
    "PhoneCall"="Phone call",
    "SMSEmail"="SMS/email",
    "SocialMedia"="Social media",
    "Other"="Other"
  )
}

told.why.cols <- function() {
  c(
    "ToldWhy.NoticedInsecureBehavior",
    "ToldWhy.ProvideInfoForProtection",
    "ToldWhy.FeltResponsibility",
    "ToldWhy.PersonallyExperienced",
    "ToldWhy.ReadArticle",
    "ToldWhy.Other"
  )
}

told.why.nice.rename <- function() {
  c(
    "NoticedInsecureBehavior"="Noticed insecure",
    "ProvideInfoForProtection"="Info for protection",
    "FeltResponsibility"="Felt responsible",
    "PersonallyExperienced"="Share experience",
    "ReadArticle"="Read article",
    "Other"="Other"
  )
}

told.what.cols <- function() {
  c(
    "ToldWhat.GeneralInfo",
    "ToldWhat.SolutionsAdvice",
    "ToldWhat.Story",
    "ToldWhat.Venting",
    "ToldWhat.Other"
  )
}

told.what.nice.rename <- function() {
  c(
    "GeneralInfo"="General info",
    "SolutionsAdvice"="Provide advice",
    "Story"="Story",
    "Venting"="Venting",
    "Other"="Other"
  )
}

binary.ethnicity.cols <- function() {
  c(
    "Ethnicity.Asian",
    "Ethnicity.Black",
    "Ethnicity.White",
    "Ethnicity.Hisp",
    "Ethnicity.Native",
    "Ethnicity.Other",
    "Ethnicity.Other2"
  )
}

SEBIS.scale.conversion <- function() {
  c(
    "Never"=1,
    "Rarely"=2,
    "Sometimes"=3,
    "Often"=4,
    "Always"=5
  )
}

SEBIS.scale.reverse.conversion <- function() {
  c(
    "1"="Never",
    "2"="Rarely",
    "3"="Sometimes",
    "4"="Often",
    "5"="Always"
  )
}

IUIPC.scale.conversion <- function() {
  c(
    "Strongly disagree"=0,
    "Disagree"=1,
    "Neither agree nor disagree"=2,
    "Agree"=3,
    "Strongly agree"=4
  )
}

SEBIS.cats <- function() {
  c(
    "SEBIS.ChangePasswords"="Passwords",
    "SEBIS.DiffPasswords"="Passwords",
    "SEBIS.NewPasswords"="Passwords",
    "SEBIS.SpecialCharPasswords"="Passwords",
    "SEBIS.LinkVerify"="Web",
    "SEBIS.AestheticVerify"="Web",
    "SEBIS.HTTPSVerify"="Web",
    "SEBIS.LinkMouseover"="Web",
    "SEBIS.ReportSecurity"="Update",
    "SEBIS.ImmediateUpdate"="Update",
    "SEBIS.UpdatedSoftware"="Update",
    "SEBIS.AVAutoUpdates"="Update",
    "SEBIS.AutoLock"="Auth",
    "SEBIS.LaptopAuth"="Auth",
    "SEBIS.ManualLock"="Auth",
    "SEBIS.MobileAuth"="Auth"
  )
}

SEBIS.cats.idx <- function() {
  c(
    "SEBIS.ChangePasswords"="Don't\nchange\npwds",
    "SEBIS.DiffPasswords"="Use\ndifferent\npwds",
    "SEBIS.NewPasswords"="Make\nstrong\npwds",
    "SEBIS.SpecialCharPasswords"="Special\nchar\npwds",
    "SEBIS.LinkVerify"="Don't\nverify\nlinks",
    "SEBIS.AestheticVerify"="Verify\nby\nlook",
    "SEBIS.HTTPSVerify"="Don't\nverify\nhttps",
    "SEBIS.LinkMouseover"="Mouseover\nlinks",
    "SEBIS.ReportSecurity"="Report\nproblems",
    "SEBIS.ImmediateUpdate"="Immediate\nupdate",
    "SEBIS.UpdatedSoftware"="Keep\nsoftware\nupdated",
    "SEBIS.AVAutoUpdates"="Verify\nAV\nupdate",
    "SEBIS.AutoLock"="Set auto\nscreen\nlock",
    "SEBIS.LaptopAuth"="Use\nlaptop\nauth",
    "SEBIS.ManualLock"="Manual\nlock\nscreen",
    "SEBIS.MobileAuth"="Use phone\nauth"
  )
}

SEBIS.valence <- function() {
  c(
    "SEBIS.ChangePasswords"="Negative",
    "SEBIS.DiffPasswords"="Positive",
    "SEBIS.NewPasswords"="Positive",
    "SEBIS.SpecialCharPasswords"="Positive",
    "SEBIS.LinkVerify"="Negative",
    "SEBIS.AestheticVerify"="Negative",
    "SEBIS.HTTPSVerify"="Negative",
    "SEBIS.LinkMouseover"="Positive",
    "SEBIS.ReportSecurity"="Positive",
    "SEBIS.ImmediateUpdate"="Positive",
    "SEBIS.UpdatedSoftware"="Positive",
    "SEBIS.AVAutoUpdates"="Positive",
    "SEBIS.AutoLock"="Positive",
    "SEBIS.LaptopAuth"="Positive",
    "SEBIS.ManualLock"="Positive",
    "SEBIS.MobileAuth"="Positive"
  )
}

familiarity.nice.rename <- function() {
  c(
    "Familiarity.IPAddr"="IP\naddress",
    "Familiarity.Cookies"="Browser\ncookies",
    "Familiarity.SSL"="SSL/\nTLS",
    "Familiarity.VPN"="VPN",
    "Familiarity.Encryption"="Encryption",
    "Familiarity.ProxyServer"="Proxy\nserver",
    "Familiarity.Tor"="Tor\nBrowser",
    "Familiarity.BrowserPrivacy"="Browser\nprivacy\nsettings",
    "Familiarity.PrivateBrowsing"="Private\nbrowsing\nmode"
  )
}

behavior.selection.nice.rename <- function() {
  c(
    "Behavior.TempNameEmail"="Used\ntemp\naccount",
    "Behavior.FakeNameUN"="Used\nfake\nname",
    "Behavior.FalseInfo"="Given\nfalse\nPII",
    "Behavior.BrowserCookieOff"="Disabled\ncookies",
    "Behavior.CookieClear"="Clear\nbrowser\nhistory",
    "Behavior.TorVPN"="Used\nTor/Proxy/\nVPN",
    "Behavior.EncryptedMail"="Encrypt\ne-mails",
    "Behavior.StopUseRealName"="Stop\nuse b/c\nof PII",
    "Behavior.DeletedPost"="Deleted\nSNS post",
    "Behavior.AskedToRemovePosts"="Asked\nothers to\nremove post",
    "Behavior.PublicCPU"="Used\npublic\ncomputer"
  )
}