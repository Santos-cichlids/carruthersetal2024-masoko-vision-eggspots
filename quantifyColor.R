# quantifyColor.R

# version 2
# 29 October 2021

# quantifies the average color of an image in various color spaces

avgColorList <- function(imageDir, imageList, suffix='png', rgb='sRGB', colorspace='Lab', white='D65') {
	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of image files
	# rgb: input image RGB color space ('sRGB' or 'Apple RGB')
	# colorspace: color space of returned color. See convertColor() 'to' option.
	# white: reference white point

	# prepare image directory
	imageDir <- gsub("/$", x=imageDir, replacement='')

	# read in images
	imagevec <- as.vector(read.table(imageList)$V1)
	nsamples <- length(imagevec)

	# process images
	colors <- matrix(nrow = nsamples, ncol=3)

	for (i in 1:nsamples) {
		image <- paste(imageDir, "/", imagevec[i], ".", suffix, sep='')
		print(imagevec[i])
		if (i < nsamples) {
			colors[i,] <- avgColor(image=image, colspace=colorspace, rgbtype=rgb, refwhite=white)
		}
		else {
			# format color matrix
			avgcol <- avgColor(image=image, colspace=colorspace, rgbtype=rgb, refwhite=white)
			colors[i,] <- avgcol
			dimnames(colors) <- list(imagevec, names(avgcol))
		}
	}

	return(colors)
}

modelP <- function(modelobject) {
	if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
	f <- summary(modelobject)$fstatistic
	p <- pf(f[1],f[2],f[3],lower.tail=F)
	attributes(p) <- NULL
	return(p)
}

minMax <- function(arr1, arr2) {
	c(min(c(arr1,arr2),na.rm=TRUE), max(c(arr1,arr2),na.rm=TRUE))
}

calibrateLab <- function(roidir, sampleList, photodir = NULL, truefile, rgbtype="sRGB", colornames = NULL, plotFit = NULL, ...) {
	# roidir: directory containing XY coordinates of colorchecker regions
	# photodir: directory containing photos (preppended to the names in sampleList). Same is roidir if NULL.
	# sampleList: List of JPEG photos to extract ROI info from (one photo per row)
	# truefile: TSV file of color checker values with L*, a*, b* columns, and rows for white, black, red in descending order
	# colornames: vector of color checker color names to callibrate against, e.g. c("white", "black", "red"). Must match descending order of truefile colors.
	# rgbtype: type of RGB input, see convertColor() details
	# plotFit: file name prefix to output plots of true versus observed points and the fitted regression line. If NULL no plotting.
	# ...: aditional arguments to convertColor(), e.g. to.ref.white="D50"

	# Outputs a list of:
	# [[1]] L*, a*, b* mapping function values (parameters of linear model)
	# [[2]] the distance of the L*, a*, b* channels from the truth for each color AFTER adjustment.
	# [[3]] the distance of the L*, a*, b* channels from teh truth for each color BEFORE adjustment.
	# [[4]] p-values of the regression model fits to each L*a*b* axis used to obtain calibration factors.

	# load jpeg library
	if (! "jpeg" %in% (.packages())) library(jpeg)

	# check paramters
	if (rgbtype != "sRGB" && rgbtype != "Apple RGB") stop(paste(rgbtype, "is not a valid rgb type ('sRGB' or 'Apple RGB')"))
	
	labtest <- read.table(truefile,head=FALSE,nrows=1)
	lab.head = (is.numeric(labtest[1,1]) == FALSE)
	truelab <- read.table(truefile,head=lab.head)
	roidir <- gsub("/$", x=roidir, replacement='')
	if (is.null(photodir)) photodir = roidir else photodir = gsub("/$", x=photodir, replacement='')
	samplist <- read.table(sampleList,head=FALSE)
	mat.adjust <- NULL
	model.p <- NULL
	map.params <- list() # parameters of linear model mapping functionb between observed and true colors
	mat.error = NULL # difference between observed and expected AFTER adjustment
	mat.error.raw = NULL # difference between observed and expected BEFORE adjustment
	ax.adjust = 5
	ncolors = length(colornames)
	if (!is.null(plotFit)) pdf(file=paste0(plotFit,".pdf"))

	
	for (n in 1:nrow(samplist)) {
		cat(paste0(samplist$V1[n]),"\n") # to track progress
		photo.prefix = gsub("\\.[^.]+$",samplist$V1[n],perl=TRUE,replacement='')
		img <- readJPEG(source=paste0(photodir,"/",samplist$V1[n]))
		labmean <- matrix(rep(NA,nrow(truelab)*ncol(truelab)), nrow=nrow(truelab), ncol=ncol(truelab))
		k=1

		cat("Parsing input files and calculating observed l*a*b* values\n")
		for (col in colornames) { # see /home/tyler/Dropbox/TEST_COLOUR/NEW_FILES/colourcode.jpg
			coordfile <- paste0(roidir,"/",photo.prefix,"_",col,".txt")
			if (file.exists(coordfile)) {
				headtest <- read.table(coordfile,head=FALSE,nrows=1)
				is.head = (is.numeric(headtest[1,1]) == FALSE)
				xy <- read.table(coordfile,head=is.head)
				xcord=min(xy[,1]):max(xy[,1])
				ycord=min(xy[,2]):max(xy[,2])
				rgbmat.sub <- matrix(c(as.vector(img[,,1][ycord,xcord]), as.vector(img[,,2][ycord,xcord]), as.vector(img[,,3][ycord,xcord])), ncol=3)
				labmat <- convertColor(color=rgbmat.sub, from=rgbtype, to='Lab', from.ref.white="D65", ...)
				#labmat <- convertColor(color=rgbmat.sub, from=rgbtype, to='Lab', from.ref.white="D65", to.ref.white="D50") # for testing
				labmean[k,] = c(mean(labmat[,1]), mean(labmat[,2]), mean(labmat[,3]))
			} else labmean[k,] = rep(NA,3)
			k = k+1
		}
		if (length(which(!is.na(labmean[,1]))) < 2 || length(which(!is.na(labmean[,2]))) < 2 || length(which(!is.na(labmean[,3]))) < 2) {
			stop("Insufficient observed data data points in call to calibrateLab()")			
		}

		# Assuming linear relationship between observed and true L*a*b* values for now, which seems appropriate. Could fit
		# a polynomial if necessary.
		cat("Fitting mapping parameters\n")
		models <- list()
		modfit <- list()
		for (i in 1:ncol(labmean)) {
			models[[i]] <- lm(truelab[,i] ~ labmean[,i], na.action = "na.omit")
			modfit[[i]] <- summary(models[[i]])
			#mod <- poly(x,c) # polynomial fitting template
		}
		
		mod.intercept <- sapply(1:3,function(x,fitlist){unname((modfit[[x]]$coefficients[,1])[1])},fitlist=modfit)
		mod.coef <- sapply(1:3,function(x,fitlist){unname((modfit[[x]]$coefficients[,1])[2])},fitlist=modfit)
		map.params[[n]] <- list(photo=photo.prefix, params=data.frame(intercept=mod.intercept,coef=mod.coef)) # rows: are 1=l*, 2=a*, 3=b*
		
		model.p = rbind(model.p, data.frame(photo=photo.prefix, L=modelP(models[[1]]), a=modelP(models[[2]]), b=modelP(models[[3]])))

		labmean.calib = t(t(labmean)*map.params[[n]]$params$coef + map.params[[n]]$params$intercept)
		mat.error <- rbind(mat.error,cbind(data.frame(sample=rep(photo.prefix,ncolors), color=colornames),abs(labmean.calib - truelab)))
		mat.error.raw <- rbind(mat.error.raw,cbind(data.frame(sample=rep(photo.prefix,ncolors), color=colornames),abs(labmean - truelab)))

		# plot (optional)
		if (!is.null(plotFit)) {
			cat("Plotting\n")
			pointlab=which(!is.na(labmean[,1]))
			par(mfrow=c(2,2))
			
			ax.lim=minMax(labmean[,1], truelab[,1]) + c(-ax.adjust, ax.adjust)
			#plot(y=truelab[,1], x=labmean[,1], main=paste0(photo.prefix, " l*"), xlab = "Observed", ylab="Expected",xlim=ax.lim, ylim=ax.lim)
			plot(y=truelab[,1], x=labmean[,1], main=paste0(photo.prefix, " l*"), xlab = "Observed", ylab="Expected", type="n", xlim=ax.lim, ylim=ax.lim)
			text(y=truelab[,1], x=labmean[,1],labels=pointlab)
			abline(a=0,b=1,col="blue",lty=2)
			abline(a=map.params[[n]]$params$intercept[1], b=map.params[[n]]$params$coef[1], col="red")
			text(x=quantile(labmean[,1],0.25,na.rm=TRUE),y=quantile(truelab[,1],0.98),paste0("R2 = ",round(modfit[[1]]$r.squared,3)))

			ax.lim=minMax(labmean[,2], truelab[,2]) + c(-ax.adjust, ax.adjust)			
			#plot(y=truelab[,2], x=labmean[,2], main=paste0(photo.prefix, " a*"), xlab = "Observed", ylab="Expected", xlim=ax.lim, ylim=ax.lim)
			plot(y=truelab[,2], x=labmean[,2], main=paste0(photo.prefix, " a*"), xlab = "Observed", ylab="Expected", type="n", xlim=ax.lim, ylim=ax.lim)
			text(y=truelab[,2], x=labmean[,2],labels=pointlab)
                        abline(a=0,b=1,col="blue",lty=2)
			abline(a=map.params[[n]]$params$intercept[2], b=map.params[[n]]$params$coef[2], col="red")
                        text(x=quantile(labmean[,2],0.25,na.rm=TRUE),y=quantile(truelab[,2],0.98),paste0("R2 = ",round(modfit[[2]]$r.squared,3)))

			ax.lim=minMax(labmean[,3], truelab[,3]) + c(-ax.adjust, ax.adjust)
			#plot(y=truelab[,3], x=labmean[,3], main=paste0(photo.prefix, " b*"), xlab = "Observed", ylab="Expected", xlim=ax.lim, ylim=ax.lim)
			plot(y=truelab[,3], x=labmean[,3], main=paste0(photo.prefix, " b*"), xlab = "Observed", ylab="Expected", type="n", xlim=ax.lim, ylim=ax.lim)
			text(y=truelab[,3], x=labmean[,3],labels=pointlab)
                        abline(a=0,b=1,col="blue",lty=2)
			abline(a=map.params[[n]]$params$intercept[3], b=map.params[[n]]$params$coef[3], col="red")
                        text(x=quantile(labmean[,3],0.25,na.rm=TRUE),y=quantile(truelab[,3],0.98),paste0("R2 = ",round(modfit[[3]]$r.squared,3)))

			plot.new()
		}

	}
	colnames(mat.error) = c("photo", "color", "L_error", "a_error", "b_error")
	colnames(mat.error.raw) = c("photo", "color", "L_error", "a_error", "b_error")
	if (!is.null(plotFit)) dev.off()
	return(list(map.params, mat.error, mat.error.raw, model.p))
}

roiLab <- function(pngimage, rgbtype="sRGB", calibrate = NULL, sampleid = NULL, ...) {
	# pngimage: a single png file (with suffix ".png" or ".PNG") or TSV file with (1) sample/photo ID and (2) masked png file (one sample per row) 
	# rgbtype: The RGB color spaces of input PNG files, see details of convertColor()
	# calibrate: A list() of photo calibration parameters, [[1]] of calibrateLab().
	# Note that pngimage IDs must match the calibrate IDs.
	# sampleid: If passing a single png file and calibrating, this is the sample's name in the 'calibrate' data.frame
	# ...: Additional arguments to convertColor(), e.g. to.ref.white = "D50"

	# outputs a data.frame of L*, a*, b* values

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# check parameters
	if (rgbtype != "sRGB" && rgbtype != "Apple RGB") stop(paste(rgbtype, "is not a valid rgb type ('sRGB' or 'Apple RGB')"))

	imagelist = NULL
	if (length(grep("\\.png$", pngimage, ignore.case=TRUE)) < 1) {
		imagelist <- read.table(pngimage, head=FALSE) 
	} else {
		imagelist = data.frame(V1=pngimage)
		if (!is.null(calibrate) & is.null(sampleid)) stop("'sampleid' must be supplied when callibrating a single image")
		imagelist = data.frame(V1=sampleid, V2=pngimage)
	}

	photos <- NULL
	if (!is.null(calibrate)) {
		photos <- sapply(1:length(calibrate), function(x,photolist){photolist[[x]]$photo},photolist=calibrate)
	} else {
		stop("No calibration paramters found, check 'calibrate'")
	}

	labmean = NULL

	for (i in 1:nrow(imagelist)) {
		id = as.character(imagelist[i,1])
		cat(paste0(id),"\n") # track progress
		img = readPNG(source = as.character(imagelist[i,2]))

		# find non-masked (transparent) region of image
		imgidx <- which(img[,,4] == 1)
		rgbmat <- matrix(data=c(img[,,1][imgidx], img[,,2][imgidx], img[,,3][imgidx]), nrow=length(imgidx), ncol=3)
		labmat <- convertColor(color=rgbmat, from=rgbtype, to="Lab", from.ref.white="D65", ...)
		write("calibrating")
		if (length((idx = which(photos == id))) > 0) {
			for (k in 1:3) labmat[,k] = labmat[,k] * calibrate[[idx]]$params$coef[k] + calibrate[[idx]]$params$intercept[k]
		} else stop(paste(id, "not found in callibration data.frame"))
		labmean = rbind(labmean, data.frame(id = id, L = mean(labmat[,1]), a = mean(labmat[,2]), b = mean(labmat[,3])))
	}

	return(labmean)
}

avgColor <- function(image, colspace='sRGB', rgbtype='sRGB', refwhite='D65', reflab=NULL) {
	# image: png image to analyze
	# colspace: color space of returned color. See convertColor() 'to' option.
	# rgbtype: input image color space ('sRGB' or 'Apple RGB')
	# refwhite: reference white point

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# check parameters
	if (rgbtype != "sRGB" && rgbtype != "Apple RGB") stop(paste(rgbtype, "is not a valid rgb type ('sRGB' or 'Apple RGB')"))

	# read in image
	img <- readPNG(source=image)
	
	# find nontransparent portion of image
	imgidx <- which(img[,,4] == 1)
	rgbmat <- matrix(data=c(img[,,1][imgidx], img[,,2][imgidx], img[,,3][imgidx]), nrow=length(imgidx), ncol=3, dimnames=list(NULL,c('r','g','b')))
	
	# vector for storing average colors
	avgcolor <- rep(NA,3)

	# calculate average RGB value
	rgb <- avgRGB(rgbmat)
	
	# convert RGB to other color space
	if (length(grep('rgb', colspace, ignore.case=TRUE)) < 1)  {
		avgcolor <- convertColor(color=rgb, from=rgbtype, to=colspace, to.ref.white=refwhite)[1,]
		if (colspace == "Lab") names(avgcolor) <- c('L','a','b')
	}
	else {
		avgcolor <- rgb[1,]
	}

	return(avgcolor)
}

avgRGB <- function(rgbvals) {
	# rgbvals: matrix of RGB values

	# calculate average rgb
	nvals <- nrow(rgbvals)
	avgrgb <- matrix(data=c(sqrt(sum(rgbvals[,1]^2/nvals)), sqrt(sum(rgbvals[,2]^2/nvals)), sqrt(sum(rgbvals[,3]^2/nvals))), nrow=1,ncol=3,dimnames=list(NULL,c('r','g','b')))

	return(avgrgb)
}
