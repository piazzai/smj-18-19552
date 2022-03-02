# load packages needed to run the code below

library(combinat)
library(data.table)
library(doParallel)
library(dplyr)
library(foreach)
library(ggplot2)
library(Hmisc)
library(MASS)
library(perturb)
library(reshape2)
library(rjson)
library(rvest)
library(stringr)
library(qlcMatrix)

# set the number of cores for parallel processing

clust <- detectCores() - 1

# load raw data from the tarball

single <- read.csv("single.csv", stringsAsFactors = F)[, -1] %>%
  data.table()
billboard <-
  read.csv("billboard.csv", stringsAsFactors = F)[, -1] %>%
  data.table()
fuzzy <- read.csv("fuzzy.csv", stringsAsFactors = F)[, -1] %>%
  data.table()
label <- read.csv("label.csv", stringsAsFactors = F) %>%
  data.table()
parent <- read.csv("parent.csv", stringsAsFactors = F) %>%
  data.table()
hierarchy <-
  read.csv("hierarchy.csv", stringsAsFactors = F)[, -1] %>%
  data.table()
mahala <- read.csv("mahala.csv", stringsAsFactors = F) %>%
  data.table()

# keep only data about releases from 2004 onward

label <- label[rYear > 2003]
parent <- parent[(kEnd > 2003 | is.na(kEnd)) & kId == 200]
parent[is.na(parent$kStart)]$kStart <- 0
parent[is.na(parent$kEnd)]$kEnd <- 9999

# delimit style names by angle brackets

single$mStyle2 <-
  gsub(",", ">,<", paste('<', (single$mStyle), '>', sep = ""))

# recursively search the parent table to link each release to one of the majors
# or flag it as independent

FindTopParent <- function(x, y) {
  if (is.na(x)) {
    return("self")
  }
  child <- x
  repeat {
    if (nrow(parent[cId == x & kStart <= y & kEnd >= y]) == 0) {
      return("indie")
    }
    owner <- min(parent[cId == child & kStart <= y & kEnd >= y]$pId)
    if (nrow(parent[cId == owner & kStart <= y & kEnd >= y]) == 0) {
      break
    }
    child <- owner
  }
  top <- unique(parent[pId == owner]$pName)
  ifelse(grepl("Sony", top),
         "sony",
         ifelse(
           grepl("Universal", top),
           "universal",
           ifelse(
             grepl("Warner", top),
             "warner",
             ifelse(grepl("EMI", top), "emi", "indie")
           )
         ))
}

label$lParent <- mapply(FindTopParent, label$lDiscogs, label$rYear)

# put data in yearly format and drop emi observations after its disbandment

majors <- c("sony", "universal", "warner", "emi")
years <- 2004:2014
genres <- unique(unlist(strsplit(data.matrix(single$mGenre), ",")))
styles <- unique(unlist(strsplit(data.matrix(single$mStyle), ",")))
styles <- styles[-which(styles == "shoegazer")]

yearly <-
  expand.grid("major" = majors,
              "year" = years,
              "style" = styles) %>% data.table()

yearly <- yearly[!(major == "emi" & year > 2011) & year > 2004]

# set lag for the computation of variables

lag <- 1

# find the parent genre of each style

FindRootGenre <- function(x) {
  as.character(hierarchy[child == x]$root)
}

yearly$genre <-
  mcmapply(FindRootGenre, yearly$style, mc.cores = clust)

# count the total number of singles released by the major

CountOwnSingles <- function(x, y, z) {
  z <- paste("<", z, ">", sep = "")
  inYear <- label[lParent == x & rYear == y]$mId
  inStyle <- single[mId %in% inYear & grepl(z, mStyle2)]
  nrow(inStyle)
}

yearly$ownSingles <-
  mcmapply(CountOwnSingles,
           yearly$major,
           yearly$year,
           yearly$style,
           mc.cores = clust)

# count the total number of singles released by rival majors

ComputeRivalSingles <- function(x, y, z) {
  z <- paste("<", z, ">", sep = "")
  inYear <-
    label[lParent != x &
            lParent != "indie" & lParent != "self" & rYear == y]$mId
  inStyle <- single[mId %in% inYear & grepl(z, mStyle2)]$mId
  rivals <- majors[-which(majors == x)]
  if (y > 2011) {
    rivals <- rivals[-which(rivals == "emi")]
  }
  total <- 0
  for (i in rivals) {
    total <- c(total, nrow(label[mId %in% inStyle & lParent == i]))
  }
  sum(total)
}

yearly$rivalSingles <-
  mcmapply(ComputeRivalSingles,
           yearly$major,
           yearly$year - lag,
           yearly$style,
           mc.cores = clust)

# count the total number of singles released by independents

CountIndieSingles <- function(x, y, z) {
  z <- paste("<", z, ">", sep = "")
  inYear <- label[lParent == "indie" & rYear == y]$mId
  inStyle <- single[mId %in% inYear & grepl(z, mStyle2)]
  nrow(inStyle)
}

yearly$indieSingles <-
  mcmapply(CountIndieSingles,
           yearly$major,
           yearly$year - lag,
           yearly$style,
           mc.cores = clust)

# lag the total number of singles released by the major

yearly$ownSinglesLag <-
  mcmapply(CountOwnSingles,
           yearly$major,
           yearly$year - lag,
           yearly$style,
           mc.cores = clust)

# compute the total size of the major's product portfolio

CountOwnTotal <- function(x, y) {
  inYear <- label[lParent == x & rYear == y]
  nrow(inYear)
}

yearly$ownTotal <-
  mcmapply(CountOwnTotal, yearly$major, yearly$year - lag, mc.cores = clust)

# calculate the number of billboard entries for each major-style-year as a
# measure of competitive performance

MeasureBillboardRanks <- function(x, y, z) {
  z <- paste("<", z, ">", sep = "")
  inYear <- label[lParent == x & rYear == y]$mId
  inStyle <- single[mId %in% inYear & grepl(z, mStyle2)]$mId
  ranks <- billboard[fuzzy10 %in% inStyle & bYear == y]$bRank
  sum((101 - ranks) / 100)
}

yearly$billboard <-
  mcmapply(MeasureBillboardRanks,
           yearly$major,
           yearly$year - lag,
           yearly$style,
           mc.cores = clust)

# compute a herfindahl-hirschman index to measure diversification across styles

ComputeHerfindahl <- function(x, y) {
  inYear <- label[lParent == x & rYear == y]$mId
  byStyle <-
    table(unlist(strsplit(data.matrix(single[mId %in% inYear]$mStyle), ",")))
  if (sum(byStyle) == 0) {
    return(0)
  }
  index <- 0
  for (i in 1:length(byStyle)) {
    index <- index + (byStyle[i] / sum(byStyle)) ^ 2
  }
  index
}

yearly$herfindahl <-
  mcmapply(ComputeHerfindahl, yearly$major, yearly$year - lag, mc.cores = clust)

# compute the total number of Billboard entries for each style-year, regardless
# of record company, as a measure of demand for the style

ComputeDemand <- function(y, z) {
  z <- paste("<", z, ">", sep = "")
  inStyle <- single[grepl(z, mStyle2)]$mId
  ranks <- billboard[fuzzy10 %in% inStyle & bYear == y]$bRank
  sum((101 - ranks) / 100)
}

yearly$demand <-
  mcmapply(ComputeDemand, yearly$year - lag, yearly$style, mc.cores = clust)

# compute the level of product space complexity using acoustic features and
# mahalanobis distances

features <- colnames(single)[5:33]

prototype <-
  expand.grid("style" = styles, "year" = years) %>% data.table()

prototype[, features] <- NA

ComputeMeanFeature <- function(x, y, z) {
  x <- paste("<", x, ">", sep = "")
  ifelse(nrow(single[grepl(x, mStyle2) & mYear == y]) == 0,
         NA, ifelse(sum(!is.na(single[grepl(x, mStyle2) &
                                        mYear == y][[z]])) == 0,
                    NA, mean(single[grepl(x, mStyle2) &
                                      mYear == y][[z]], na.rm = T)))
}

for (i in 3:31) {
  prototype[[i]] <-
    mcmapply(ComputeMeanFeature,
             prototype$style,
             prototype$year,
             i + 2,
             mc.cores = clust)
}

cl <- makeCluster(clust)
registerDoParallel(cl)

distance <-
  data.table("single" = character(),
             "prototype" = character(),
             "year" = integer())

distance <-
  foreach(i = 1:nrow(single), .combine = "rbind") %dopar% {
    a <- single[i, "mId"]
    y <- single[i, "mYear"]
    b <- unlist(strsplit(single[i, "mStyle"], ","))
    expand.grid("single" = a,
                "prototype" = b,
                "year" = y)
  } %>% data.table() %>% droplevels()

stopCluster(cl)
rm(cl)

covariance <-
  cov(single[complete.cases(single), 5:33])

ComputeMahalanobis <- function(x, y, z) {
  a <- single[as.matrix(single)[, 1] == x, 5:33]
  b <-
    prototype[as.matrix(prototype)[, 1] == y &
                as.matrix(prototype)[, 2] == z, 3:31]
  if (sum(is.na(a)) > 0 | sum(is.na(b)) > 0) {
    return(NA)
  }
  mahalanobis(as.matrix(a), as.matrix(b), covariance)
}

distance$mahala <-
  mcmapply(ComputeMahalanobis,
           distance$single,
           distance$prototype,
           distance$year,
           mc.cores = clust)

distance$prototype <- as.character(distance$prototype)

ComputeComplexity <- function(x, y) {
  z <- paste("<", x, ">", sep = "")
  inStyle <- single[grepl(z, mStyle2) & mYear == y]$mId
  sqrtdist <-
    sqrt(distance[single %in% inStyle & prototype == x]$mahala)
  if (length(sqrtdist) == 0 | sum(!is.na(sqrtdist)) == 0) {
    return(NA)
  }
  mean(sqrtdist, na.rm = T)
}

yearly$complex <-
  mcmapply(ComputeComplexity,
           yearly$style,
           yearly$year - lag,
           mc.cores = clust)

# if complexity cannot be computed for a style-year (e.g., because no records
# were released) then the last known value for that style carries forward

CarryComplexity <- function(x, y) {
  current <- unique(yearly[style == x & year == y]$complex)
  if (is.na(current)) {
    i <- 0
    repeat {
      i <- i + 1
      if (nrow(yearly[style == x & year == y - i]) == 0)
        break
      current <- unique(yearly[style == x & year == y - i]$complex)
      if (!is.na(current))
        break
    }
  }
  current
}

yearly$complex2 <-
  mcmapply(CarryComplexity,
           yearly$style,
           yearly$year,
           mc.cores = clust)

# compute the level of rival product proliferation

ComputeProliferation <- function(x, y, z) {
  z <- paste("<", z, ">", sep = "")
  inYear <-
    label[lParent != x &
            lParent != "indie" & lParent != "self" & rYear == y]$mId
  inStyle <- single[mId %in% inYear & grepl(z, mStyle2)]$mId
  rivals <- majors[-which(majors == x)]
  if (y > 2011) {
    rivals <- rivals[-which(rivals == "emi")]
  }
  total <- 0
  for (i in rivals) {
    total <- c(total, nrow(label[mId %in% inStyle & lParent == i]))
  }
  max(total)
}

yearly$prolif <-
  mcmapply(ComputeProliferation,
           yearly$major,
           yearly$year - lag,
           yearly$style,
           mc.cores = clust)

# subtract proliferation from the count of rival singles to avoid collinearity

yearly$rivalSingles2 <- yearly$rivalSingles - yearly$prolif

# put together the sample used for regression analysis

sample <-
  data.table(
    "Major" = yearly$major,
    "Style" = yearly$style,
    "Year" = yearly$year,
    "Genre" = yearly$genre,
    "OwnSingles" = yearly$ownSingles,
    "OwnSinglesLagged" = yearly$ownSinglesLag,
    "PortfolioSize" = yearly$ownTotal,
    "Performance" = yearly$billboard,
    "Diversification" = yearly$herfindahl,
    "Demand" = yearly$demand,
    "IndieSingles" = yearly$indieSingles,
    "MajorSingles" = yearly$rivalSingles2,
    "Proliferation" = yearly$prolif,
    "Complexity" = yearly$complex2
  )

# save as data.csv

save(sample, file = "data.csv")

# standardize regression variables

iv <-
  c(
    "OwnSinglesLagged",
    "PortfolioSize",
    "Performance",
    "Diversification",
    "Demand",
    "IndieSingles",
    "MajorSingles",
    "Proliferation",
    "Complexity"
  )

dv <- c("OwnSingles")

ctrl <- c("Major", "Year", "Style", "Genre")

noctrl <- c(dv, iv)
vars <- c(dv, ctrl, iv)

sample.unstd <- sample[complete.cases(sample[, ..vars]), ..vars]
sample.mean <-
  cbind(sample.unstd[, ..dv], sample.unstd[, ..ctrl], scale(sample.unstd[, ..iv], scale = F))
sample.std <-
  cbind(sample.unstd[, ..dv], sample.unstd[, ..ctrl], scale(sample.unstd[, ..iv]))

# compute descriptive statistics and correlations

ComputeCorrelations <- function(x) {
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  mystars <-
    ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  R <- format(round(cbind(rep(-1.11, ncol(
    x
  )), R), 2))[, -1]
  Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  P <- format(round(cbind(rep(-1.11, ncol(
    x
  )), p), 3))[, -1]
  Pnew <- matrix(P, ncol = ncol(x))
  diag(Pnew) <- paste(diag(P), " ", sep = "")
  rownames(Pnew) <- colnames(x)
  colnames(Pnew) <- paste(colnames(x), "", sep = "")
  Pnew <- as.matrix(Pnew)
  Pnew[upper.tri(Pnew, diag = TRUE)] <- ""
  Pnew <- as.data.frame(Pnew)
  Pnew <- cbind(Pnew[1:length(Pnew) - 1])
  return(list(Rnew, Pnew))
}

descr <-
  data.table(
    "mean" = round(as.numeric(lapply(
      sample.unstd[, ..noctrl], mean, na.rm = T
    )), 2),
    "sd" = round(as.numeric(lapply(
      sample.unstd[, ..noctrl], sd, na.rm = T
    )), 2),
    "min" = round(as.numeric(lapply(
      sample.unstd[, ..noctrl], min, na.rm = T
    )), 2),
    "max" = round(as.numeric(lapply(
      sample.unstd[, ..noctrl], max, na.rm = T
    )), 2)
  )

corr.r <- ComputeCorrelations(sample.unstd[, ..noctrl])[[1]]
corr.p <- ComputeCorrelations(sample.unstd[, ..noctrl])[[2]]

# compute condition number of the data matrix before and after mean-centering

cond.unstd <- colldiag(sample.unstd[, ..iv])
cond.mean <- colldiag(sample.mean[, ..iv])

# print descriptive statistics, correlations, and condition number

descr
corr.r
corr.p
cond.unstd
cond.mean

# estimate null model

fit0 <- glm(OwnSingles ~ 1, family = "quasipoisson", data = sample.std)

# estimate control-only model

model1 <-
  OwnSingles ~ factor(Major) + factor(Year) + factor(Genre) +
  OwnSinglesLagged + PortfolioSize + Performance + Diversification +
  Demand + IndieSingles + MajorSingles
fit1 <- glm(model1, family = "quasipoisson", data = sample.std)

# print estimates and diagnostics

summary(fit1)
anova(fit0, fit1, test = "Chisq")
max(influence.measures(fit1)$infmat[, "cook.d"])

# estimate a model with the proliferation variable

model2 <-
  OwnSingles ~ factor(Major) + factor(Year) + factor(Genre) +
  OwnSinglesLagged + PortfolioSize + Performance + Diversification +
  Demand + IndieSingles + MajorSingles + Proliferation
fit2 <- glm(model2, family = "quasipoisson", data = sample.std)

# print estimates and diagnostics

summary(fit2)
anova(fit1, fit2, test = "Chisq")
max(influence.measures(fit2)$infmat[, "cook.d"])

# estimate a model with the complexity variable

model3 <-
  OwnSingles ~ factor(Major) + factor(Year) + factor(Genre) +
  OwnSinglesLagged + PortfolioSize + Performance + Diversification +
  Demand + IndieSingles + MajorSingles + Proliferation + Complexity
fit3 <- glm(model3, family = "quasipoisson", data = sample.std)

# print estimates and diagnostics

summary(fit3)
anova(fit2, fit3, test = "Chisq")
max(influence.measures(fit3)$infmat[, "cook.d"])

# estimate a model with the interaction between proliferation and complexity

model4 <-
  OwnSingles ~ factor(Major) + factor(Year) + factor(Genre) +
  OwnSinglesLagged + PortfolioSize + Performance + Diversification +
  Demand + IndieSingles + MajorSingles + Proliferation * Complexity
fit4 <- glm(model4, family = "quasipoisson", data = sample.std)

# print estimates and diagnostics

summary(fit4)
anova(fit3, fit4, test = "Chisq")
max(influence.measures(fit4)$infmat[, "cook.d"])

# print incidence risk ratios and confidence intervals from the final model

irr.ci <- function(x) {
  coef <- round(fit4$coefficients[x], 2)
  se <- sqrt(diag(vcov(fit4)))[x]
  sd <-
    round(sd(sample.unstd[[which(colnames(sample.unstd) == x)]]), 2)
  irr <- round(exp(coef / sd), 2)
  ci.low <- round(exp((coef - 2 * se) / sd), 2)
  ci.high <- round(exp((coef + 2 * se) / sd), 2)
  cbind(irr, ci.low, ci.high)
}

irr.int.ci <- function(x, y) {
  x.coef <- round(fit4$coefficients["Proliferation"], 2)
  x.se <- sqrt(diag(vcov(fit4)))["Proliferation"]
  x.sd <-
    round(sd(sample.unstd[[which(colnames(sample.unstd) == "Proliferation")]]), 2)
  y.coef <- round(fit4$coefficients["Proliferation:Complexity"], 2)
  y.sd <-
    round(sd(sample.unstd[[which(colnames(sample.unstd) == "Complexity")]]), 2)
  irr <- round(exp(x * (x.coef + y * y.coef / y.sd) / x.sd), 2)
  ci.low <-
    round(exp(x * ((x.coef - 2 * x.se) + y * y.coef / y.sd) / x.sd), 2)
  ci.high <-
    round(exp(x * ((x.coef + 2 * x.se) + y * y.coef / y.sd) / x.sd), 2)
  cbind(irr, ci.low, ci.high)
}

rbind(
  irr.ci("OwnSinglesLagged"),
  irr.ci("PortfolioSize"),
  irr.ci("Performance"),
  irr.ci("Diversification"),
  irr.ci("Demand"),
  irr.ci("IndieSingles"),
  irr.ci("MajorSingles"),
  irr.ci("Proliferation"),
  irr.ci("Complexity")
)

rbind(irr.int.ci(1, 0),
      irr.int.ci(1, 1),
      irr.int.ci(1, 11.15),
      irr.int.ci(4, 11.15))

# re-estimate the final model after excluding outliers and using a negative
# binomial specification instead of quasi-poisson

fit4.out <-
  glm(model4, family = "quasipoisson", data = sample.std[Complexity > -2 &
                                                           Complexity < 2])
fit4.nb <- glm.nb(model4, data = sample.std)

# print estimates

summary(fit4.out)
summary(fit4.nb)

# plot the interaction between proliferation and complexity

fit4c <- glm(model4, family = "quasipoisson", data = sample.unstd)

x <- sample.unstd$Proliferation
z <- sample.unstd$Complexity
a <- seq(min(x), max(x), .1)
b <- seq(mean(z), max(z), .001)

margins <-
  data.frame(
    Major = rep("universal", length(a) * length(b)),
    Year = rep(2009, length(a) * length(b)),
    Genre = rep("electronic", length(a) * length(b)),
    OwnSinglesLagged = rep(mean(sample.unstd$OwnSinglesLagged), length(a) * length(b)),
    PortfolioSize = rep(mean(sample.unstd$PortfolioSize), length(a) * length(b)),
    Performance = rep(mean(sample.unstd$Performance), length(a) * length(b)),
    Diversification = rep(mean(sample.unstd$Diversification), length(a) * length(b)),
    Demand = rep(mean(sample.unstd$Demand), length(a) * length(b)),
    IndieSingles = rep(mean(sample.unstd$IndieSingles), length(a) * length(b)),
    MajorSingles = rep(mean(sample.unstd$MajorSingles), length(a) * length(b)),
    Proliferation = rep(a, each = length(b)),
    Complexity = rep(b, length(a))
  )
margins$OwnSingles <- predict(fit4c, margins)

rm(a, b)

marginsplot <- ggplot(data = margins) +
  geom_line(
    aes(
      y = exp(OwnSingles),
      x = Proliferation,
      group = Complexity,
      col = Complexity
    ),
    size = .75,
    lineend = "round"
  ) +
  scale_color_continuous(
    low = "grey80",
    high = "grey20",
    breaks = c(mean(z) + .3, max(z) - .2),
    labels = c("Mean", "Max.")
  ) +
  labs(y = "Multiplicative effect on the probability of imitation",
       x = "Level of proliferation by a rival in a product subspace during the previous year",
       col = "Level of complexity of the subspace\n") +
  scale_x_continuous(breaks = seq(0, 6, 1),
                     labels = 0:6,
                     limits = c(0, 6)) +
  scale_y_continuous(
    limits = c(0, 2.5),
    breaks = seq(0, 2.5, .5),
    labels = c("0%", "50%", "100%", "150%", "200%", "250%")
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")

# print interaction plot

marginsplot
