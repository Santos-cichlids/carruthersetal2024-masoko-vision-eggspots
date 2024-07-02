dat <- read.table('colour_calibration_results_quadrant.txt',head=TRUE,sep="\t")

dat$control.hue <- atan(dat$control.b/dat$control.a)
dat$control.chroma <- sqrt(dat$control.a^2 + dat$control.b^2)

### code for calculating hue while accounting for quadrant
# see paper M.R. McLellan, L.R. Lind, R.W. Kime (1994). "Hue angle determinations and statistical analysis for multiquadrant Hunter L,a,b data"
x <- NULL
for (i in 1:nrow(dat)) {
	a <- dat$red.a[i]
	b <- dat$red.b[i]

	if (is.na(a) || is.na(b)) {
		x <- c(x,NA)	
	} else if (a >= 0 && b >= 0) {
		x <- c(x,atan(b/a))
	} else if (a < 0) {
		x <- c(x,180 + atan(b/a))
	} else if (a > 0 && b < 0) {
		x <- c(x,360+atan(b/a))	
	}
}

### plotting

dat$Depth <- as.character(dat$Depth)
dat$depth.code <- replace(dat$Depth, which(dat$Depth == "Shallow"),1)
dat$depth.code <- replace(dat$depth.code, which(dat$depth.code == "Intermediate"),2)
dat$depth.code <- replace(dat$depth.code, which(dat$depth.code == "Deep"),3)
dat$depth.code <- factor(dat$depth.code, levels=c("1","2","3"))

# control plot
boxplot(control.hue ~ depth.code, names=c("shallow","intermediate","deep"), outline=FALSE, xlab=NULL, ylab="Hue", main="", ylim=c(min(dat$control.hue, na.rm=TRUE)-0.1, max(dat$control.hue,na.rm=TRUE)+0.1),at=c(1,2,3), range=1.5, data=dat) 

levs <- levels(dat$depth.code)
for (i in levs) {
	idx = which(as.character(dat$depth.code) == i)
	xjitter = jitter(rep(which(levs == i),length(idx)), amount=0.2)
	points(control.hue ~ xjitter, col="grey60", pch=1, cex=1.3, data=dat[idx,])
	points(control.hue ~ xjitter, col=alpha("grey20", 0.6), pch=16, cex=1.3, data=dat[idx,])
}

# spot plot
x11()
boxplot(spot.hue ~ depth.code, names=c("shallow","intermediate","deep"), outline=FALSE, xlab=NULL, ylab="Hue", main="", ylim=c(min(dat$spot.hue, na.rm=TRUE)-0.1, max(dat$spot.hue,na.rm=TRUE)+0.1),at=c(1,2,3), range=1.5, data=dat) 

levs <- levels(dat$depth.code)
for (i in levs) {
	idx = which(as.character(dat$depth.code) == i)
	xjitter = jitter(rep(which(levs == i),length(idx)), amount=0.2)
	points(spot.hue ~ xjitter, col="grey60", pch=1, cex=1.3, data=dat[idx,])
	points(spot.hue ~ xjitter, col=alpha("grey20", 0.6), pch=16, cex=1.3, data=dat[idx,])
}

### test for significant differences in distributions of controls

# test control intermediate vs shallow - not different

wilcox.test(x=dat$control.hue[which(dat$Depth == "Intermediate")],y=dat$control.hue[which(dat$Depth == "Shallow")],alternative="two.sided")

#	Wilcoxon rank sum test with continuity correction
#
#data:  dat$control.hue[which(dat$Depth == "Intermediate")] and dat$control.hue[which(dat$Depth == #"Shallow")]
#W = 915, p-value = 0.1437
#alternative hypothesis: true location shift is not equal to 0

# test control Deep vs Shallow - different

wilcox.test(x=dat$control.hue[which(dat$Depth == "Deep")],y=dat$control.hue[which(dat$Depth == #"Shallow")],alternative="two.sided")
#
#	Wilcoxon rank sum test with continuity correction
#
#data:  dat$control.hue[which(dat$Depth == "Deep")] and dat$control.hue[which(dat$Depth == "Shallow")]
#W = 648, p-value = 6.499e-05
#alternative hypothesis: true location shift is not equal to 0

# test control Deep vs intermediate - different

wilcox.test(x=dat$control.hue[which(dat$Depth == "Deep")],y=dat$control.hue[which(dat$Depth == "Intermediate")],alternative="two.sided")

#	Wilcoxon rank sum test with continuity correction
#
#data:  dat$control.hue[which(dat$Depth == "Deep")] and dat$control.hue[which(dat$Depth == #"Intermediate")]
#W = 1236, p-value = 0.003307
#alternative hypothesis: true location shift is not equal to 0

# test control Deep vs shallow + intermediate - different

wilcox.test(x=dat$control.hue[which(dat$Depth == "Deep")],y=dat$control.hue[which(dat$Depth == "Shallow" | dat$Depth == "Intermediate")],alternative="two.sided")

#	Wilcoxon rank sum test with continuity correction
#
#data:  dat$control.hue[which(dat$Depth == "Deep")] and dat$control.hue[which(dat$Depth == "Shallow" #| dat$Depth == "Intermediate")]
#W = 1884, p-value = 6.002e-05
#alternative hypothesis: true location shift is not equal to 0

### Test variance differences between control groups. These should not be significant 
### if the calibration did an alright job, which it seems to have. The calibration still cannot
### correct for the major depth effect between deeps and non-deeps.

control.deep <- dat$control.hue[which(dat$Depth == "Deep" & !is.na(dat$control.hue))]
control.shallow <- dat$control.hue[which(dat$Depth == "Shallow" & !is.na(dat$control.hue))]
control.intermediate <- dat$control.hue[which(dat$Depth == "Intermediate" & !is.na(dat$control.hue))]

# deep vs shallow - no difference

leveneTest(y=c(control.deep, control.shallow), group=factor(c(rep("deep",length(control.deep)), rep#("shallow",length(control.shallow)))), center = median)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   1  1.8643 0.1752
#      100

# deep vs intermediate - no difference

leveneTest(y=c(control.deep, control.intermediate), group=factor(c(rep("deep",length(control.deep)), rep("intermediate",length(control.intermediate)))), center = median)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   1   8e-04 0.9776
#      118

# shallow vs intermediate - no difference

leveneTest(y=c(control.shallow, control.intermediate), group=factor(c(rep("shallow",length(control.shallow)), rep("intermediate",length(control.intermediate)))), center = median)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  1   1.764 0.1873
#      94

### Bootstrap hypothesis testing for whether there is a biological difference between
### deeps and non-deeps (littoral)

# difference in control means

control.diff <- abs(mean(dat$control.hue[which(dat$Depth == "Deep")], na.rm=TRUE) - mean(dat$control.hue[which(dat$Depth == "Shallow" | dat$Depth == "Intermediate")], na.rm=TRUE))

#> control.diff
#[1] 0.03943337

# difference in spot means

spot.diff <- abs(mean(dat$spot.hue[which(dat$Depth == "Deep")], na.rm=TRUE) - mean(dat$spot.hue[which(dat$Depth == "Shallow" | dat$Depth == "Intermediate")], na.rm=TRUE))

#> spot.diff
#[1] 0.1915365

### GLM to assess whether distinguishing between spots versus controls matters in assessing
### variation in hue

df <- data.frame(hue = c(dat$control.hue, dat$spot.hue), depth = factor(c(as.character(dat$Depth), as.character(dat$Depth)),levels=c("Deep", "Intermediate", "Shallow")), measure = factor(c(rep("control",nrow(dat)),rep("spot",nrow(dat)))))

# saturated model with two-way interaction between depth and measure
sat.model <- glm(hue ~ depth * measure, data=df, family=Gamma("inverse"))

# main effects model only
reduced.model <- glm(hue ~ depth + measure, data=df, family=Gamma("inverse"))

anova(reduced.model,sat.model,test="F")

#Analysis of Deviance Table
#
#Model 1: hue ~ depth + measure
#Model 2: hue ~ depth * measure
#  Resid. Df Resid. Dev Df Deviance      F    Pr(>F)    
#1       315     2.9133                                 
#2       313     2.6699  2  0.24335 14.121 1.344e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# interactions between depth and case versus control are significant in explaining variation in hue. 
# Since case/control matters it seems that the large spot difference between deeps and benthics is 
# biologically significant.


### ANALYSES FOR DEEP AND SHALLOW ONLY (INTERMEDIATE EXCLUDED) ###

# Under the null we assume that the deep and non-deep samples are from the same population
# such that we expect no spot differences between them other than what is attributable to 
# phenotyping artifacts.

dat.dl <- dat[-which(dat$Depth == "Intermediate"),]

# bootstrap function

bootColDiff <- function(df = NULL, grp1idx, grp2idx, nrep = 5000, case.name = spot.hue, control.name = control.hue, control.condition = 0, alpha=1) {

	# df: data.frame of color data
	# grp1idx: data frame indices of the individuals belonging to group 1 (exlude those with NA color values)
	# grp2idx: data frame indices of the indivdiuals belonging to group 2 (exclude those with NA color values)
	# nrep: number bootstrap replicates
	# case.name: name of trait in fish
	# control.name: name of control (color card) trait
	
	# control.condition: keep bootstrap replicates only if (control.condition = 1) group1 mean control values are greater than
	# group2 OR (control.condition = -1) group1 mean control values are less than group2. If 0, no filtering is applied based on control value.

	# alpha: retain bootstrap replicates only if the difference in control means is significant at alpha 
	# signficance level (setting this to 1 disables this filter).

	traitidx = which(colnames(df) == case.name)
	controlidx = which(colnames(df) == control.name)
	case1 <- df[grp1idx,traitidx]
	control1 <- df[grp1idx,controlidx]
	case2 <- df[grp2idx,traitidx]
	control2 <- df[grp2idx,controlidx]

	# make null populations
	combined.pop.trait <- c(case1, case2)
	combined.pop.control <- c(control1, control2)


	# perform bootstraps - we assume phenotypes are from the same population
	diff.boot <- NULL
	n <- 0
	while (n < nrep) {
		d.idx = NULL
		l.idx = NULL
		while (length(d.idx) < 1 || length(l.idx) < 1) {
			d.idx <- sample(1:length(combined.pop.trait),length(grp1idx), replace=TRUE)
			l.idx <- sample(1:length(combined.pop.trait),length(grp2idx), replace=TRUE)
		}

		trait.diff.sample = mean(combined.pop.trait[d.idx]) - mean(combined.pop.trait[l.idx])
		control.diff.sample = mean(combined.pop.control[d.idx]) - mean(combined.pop.control[l.idx])
		# without any phenotyping artifacts control should be zero

		if (control.condition == 1 && control.diff.sample <= 0) {
			next
		} else if (control.condition == -1 && control.diff.sample >= 0) {
			next
		}

		pval = ifelse(alpha < 1, wilcox.test(x=combined.pop.control[d.idx],y=combined.pop.control[l.idx],alternative="two.sided")$p.value, 1)
		if (pval > alpha) next

		#diff.boot <- c(diff.boot, abs(trait.diff.sample - control.diff.sample))
		diffstat = abs(trait.diff.sample) - abs(control.diff.sample) # difference in the magnitude of average case differences and average control differences
		diff.boot <- c(diff.boot, diffstat)
		n = n+1
	}
	return(diff.boot)
}
	
# calculate observed statistic for spots

deep.idx.dl <- which(dat.dl$Depth == "Deep" & !is.na(dat.dl$spot.hue) & !is.na(dat.dl$control.hue))
spot.dl.deep <- dat.dl$spot.hue[deep.idx.dl]
control.dl.deep <- dat.dl$control.hue[deep.idx.dl]

littoral.idx.dl <- which(dat.dl$Depth == "Shallow" & !is.na(dat.dl$spot.hue) & !is.na(dat.dl$control.hu))
spot.dl.littoral <- dat.dl$spot.hue[littoral.idx.dl] 
control.dl.littoral <- dat.dl$control.hue[littoral.idx.dl]

#obs.adjusted.diff.dl <- abs((mean(spot.dl.deep) - mean(spot.dl.littoral)) - (mean(control.dl.deep) - mean(control.dl.littoral))) # observed statistic
obs.adjusted.diff.dl <- abs(mean(spot.dl.deep) - mean(spot.dl.littoral)) - abs(mean(control.dl.deep) - mean(control.dl.littoral)) # observed statisti
#obs.adjusted.diff.dl
#[1] 0.1530401 <-- deep vs shallow spot difference minus batch effect

obs.unadjusted.diff.dl <- abs(mean(dat.dl$spot.hue[which(dat.dl$Depth == "Deep")], na.rm=TRUE) - mean(dat.dl$spot.hue[which(dat.dl$Depth == "Shallow")], na.rm=TRUE))
#obs.unadjusted.diff.dl
#[1] 0.2066991 <-- deep vs shallow spot difference (including artifacts)

control.hue.diff <- abs(mean(dat.dl$control.hue[which(dat.dl$Depth == "Deep")], na.rm=TRUE) - mean(dat.dl$control.hue[which(dat.dl$Depth == "Shallow")], na.rm=TRUE))
#control.hue.diff
# [1] 0.05456431 <-- deep vs shallow difference in control color

# bootstrap test for hue difference between deeps and shallows
set.seed(947)
spot_dl_boot <- bootColDiff(df = dat.dl, grp1idx=deep.idx.dl, grp2idx=littoral.idx.dl, nrep = 10000, case.name = "spot.hue", control.name = "control.hue", control.condition=0, alpha=1)

max(spot_dl_boot)
#[1] 0.1305842

# calculate p-value as the proportion of bootstrap statistics that are greater than or equal to
# our observed difference of 0.1530401

spot_dl.pval = length(which(spot_dl_boot >= obs.adjusted.diff.dl))/length(spot_dl_boot)
#spot_dl.pval
#[1] 0 <-- bootstrap p-value for shallow versus deep
