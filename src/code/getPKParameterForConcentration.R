
getPKParameterForConcentration <- function(time, concentration, infusionTime = 0, timeRange = NA, extrapolationRange = 0.1, methode = "linLog", LLOQ = 0)
{
	## for debugging purposes only
	#options(error = recover)
	##
	if (!is.na(timeRange) & (length(timeRange) != 2))
	{
		error("Improper TimeRange")
	}
	if (is.na(timeRange))
	{
		timeRange = c(min(time), max(time))
	}
	 if (!is.matrix(concentration))
	 {
		 concentration <- as.matrix(concentration)
	 }
	
	# These variables are calculated
	C_max <- {}
	t_max <- {}
	AUC_tend <- {}
	AUC_inf <- {}
	t_Half <- {}
	mrt <- {}
	t_LLOQ <- {}
	
	for (profilenr in 1:dim(concentration)[2])
	{
		if(all(concentration[,profilenr] <0))
		{
			error(paste("Concentrationprofile ", profilenr,"contains only negative values", sep = ""))
		}
		# only these subindices will be used
		use_idx <- intersect(which(concentration[,profilenr] >= 0), which((time >= timeRange[1])&(time <= timeRange[2])))
		
		# Cmax, tmax
		C_max <- c(C_max, max(concentration[use_idx,profilenr]))
		t_max <- c(t_max, time[which.max(concentration[use_idx,profilenr])])
		
		# AUC_tend
		tmp_AUC <- calculate_AUC(concentration[use_idx, profilenr],time[use_idx], methode, timeRange)
		AUC_tend <- c(AUC_tend, tmp_AUC$AUC)
		
		if (extrapolationRange == 0)
		{
			return(list(C_max = C_max, t_max = t_max, AUC_tend = AUC_tend, AUC_inf = AUC_inf, t_Half = t_Half, mrt = mrt, t_LLOQ= t_LLOQ))
		}
		
		# extrapolation Stuff
		if (is.na(extrapolationRange))
		{
			id_after_tmax <- which(time[use_idx] > t_max)
			Rsquare <- {}
			p <- {}
			for (j in 3:length(id_after_tmax))
			{
				id_rem <- id_after_tmax[(length(id_after_tmax) - j + 1):length(id_after_tmax)]
				p <- cbind(p, as.matrix(
								lm(log(concentration[use_idx[id_rem]], profile_nr)~time[use_idx[id_rem]])$coefficients
						))
				tmpCor<-cor(time[use_idx[id_rem]],log(concentration[use_idx[id_rem]]))
				Rsquare <- c(Rsquare, (1- (1-tmpCor^2))*(length(id_rem)-1)/(length(id_rem)-2))
			}
			maxR <- max(Rsquare)
			idmaxR <- which.max(Rsquare)
			tmpid <- which(Rsquare[idmaxR:length(Rsquare)] >= (maxR - 0.0001))
			lambda <- -1*p(2, tmpid[length(tmpid)]+(idmaxR - 1))
			intercept <- exp(p(1, tmpid[length(tmpid)]+(idmaxR - 1)))
		} else {
			if (length(extrapolationRange) == 1)
			{
				if (extrapolationRange > 1)
				{
					error ("Extrapolation Range must be smaller than 1")
				}
				n <- length(use_idx)
				win <- (n - floor(n*extrapolationRange)):n
			} else {
				if (any(extrapolationRange[1] < time[use_idx[1]],extrapolationRange[length(extrapolationRange)] > time[use_idx[length(use_idx)]]))
				{
					error ("Extrapolation Range is lager than time range")
				}
				win <- which(all(time[use_idx] >= extrapolationRange[1], 
								time[use_idx <= extrapolationRange[length(extrapolationRange)]]))
			}
			n <- length(win)
			if (any(concentration[use_idx[win], profilenr]<= 0))
			{
				warning("There ar values below or equal 0 in the extrapolation Range!")
				lambda <- NA
				intercept <- NA
			} else {
				p <- lm(log(concentration[use_idx[win], profilenr])~time[use_idx[win]])$coefficients
				lambda <- as.double(-p[2])
				intercept <- exp(as.double(p[1]))
			}
			
		}
		if (!is.na(lambda) && lambda < 0)
		{	
			stop("Improper Lambda")
		}
		
		
		#t 1/2
		t_Half <- c(t_Half, log(2)/lambda)
		
		#AUC_tend -> inf
		C_last <- intercept * exp(-lambda*time[use_idx[length(use_idx)]])
		AUC_3 <- C_last / lambda
		
		# AUC -> inf
		AUC_inf <- c(AUC_inf, AUC_tend[profilenr]+ AUC_3)
		
		#t_LLOQ
		if (LLOQ > 0)
		{
			if (concentration[use_idx[length(use_idx)], profilenr] < LLOQ)
			{
				t_LLOQ <- time[use_idx[which.min(abs(concentration[use_idx[length(use_idx)], profilenr] - LLOQ))]]
				
			} else {
				t_LLOQ = log(intercept/LLOQ)/lambda
			}
		}
		AUMC_3 <- (time[use_idx[length(use_idx)]]*C_last/lambda) + (C_last / lambda^2)
		mrt <- c(mrt, (tmp_AUC$AUMC + AUMC_3)/AUC_inf[profilenr] - (infusionTime / 2))

	}
	return(list(C_max = C_max, t_max = t_max, AUC_tend = AUC_tend, AUC_inf = AUC_inf, t_Half = t_Half, mrt = mrt, t_LLOQ= t_LLOQ))
}

calculate_AUC <- function(concentration, time, methode= "linLog", timeRange)
{
	t_max <-  time[which.max(concentration)]
	id1 <- which(time <= t_max)
	TimeSteps <- diff(time[id1], lag = 1)
	# t1 -> t_max einfache Summenbildung als Integralapproximation
	if (tolower(methode) %in% c("lin", "linlog"))
	{
		meanVals <- 0.5*(concentration[id1][-1] + concentration[id1][-length(id1)])
		AUC_1 <- sum(TimeSteps * meanVals)
	} else {
		if (tolower(methode) == "log")
		{
			meanValues <- (concentration[id1][-1] - concentration[id1][-length(id1)])
			logValues <- log(concentration[id1][-1] / concentration[id1][-length(id1)])
			tmpid <- which(abs(logValues) > 0)
			tmprem <- which(logValues == 0)
			AUC_1 <- sum(TimeSteps[tmpid] * meanValues[tmpid] / logValues[tmpid])
			if (length(tmprem) > 0)
			{
				AUC_1 <- AUC_1 + sum(TimeSteps[tmprem]* concentration[id1[tmprem]])
			}
			
		} else {
			error ("Unknown method for calculating AUC.")
		}
	}
	
	# AUC tmax -> tend
	id2 <- which(time >= t_max)
	TimeSteps <- diff(time[id2], lag = 1)
	if (tolower(methode) %in% c("log", "linlog"))
	{
		meanValues <- (concentration[id2][-1] - concentration[id2][-length(id2)])
		logValues <- log(concentration[id2][-1] / concentration[id2][-length(id2)])
		tmpid <- which(abs(logValues) > 0)
		tmprem <- which(logValues == 0)
		AUC_2 <- sum(TimeSteps[tmpid] * meanValues[tmpid] / logValues[tmpid]) 
		if (length(tmprem) > 0)
		{
				AUC_2 <- AUC_2 + sum(TimeSteps[tmprem]* concentration[id2[tmprem]])
		}
	} else {
		if (tolower(methode) == "lin")
		{
			meanVals <- 0.5*(concentration[id2][-1] + concentration[id2][-length(id2)])
			AUC_2 <- sum(TimeSteps * meanVals)
		} else {
			error ("Unknown method for calculating AUC.")
		}
	}
	
	if (time[1] > timeRange[1])
	{
		TimeSteps <- time[1]
		meanValues <- 0.5 * concentration[1]
		AUC_0 <- TimeSteps*meanValues
	} else {
		AUC_0 <- 0
	}
	
	AUC <- AUC_0 + AUC_1 + AUC_2

	### AUCM
	# First Momentum
	concM <- concentration*time
	tmax_ix <- which.max(concM)
	# AUMC 0 -> tmax
	TimeSteps <- time[2:tmax_ix] - time[1:(tmax_ix-1)]
	if (tolower(methode) %in% c("lin", "linlog"))
	{
		momentumValues <- 0.5*(concM[2:tmax_ix] + concM[1:(tmax_ix - 1)])
		AUMC_1 <- sum(momentumValues * TimeSteps)
	} else {
		if (tolower(methode) == "log")
		{
			momentumValues <- (concM[2:tmax_ix] - concM[1:(tmax_ix - 1)])
			logValues <- log(concM[2:tmax_ix] / concM[1:(tmax_ix - 1)])
			AUMC_1 <- sum(TimeSteps * momentumValues / logValues)
			tmpid <- which(abs(logValues) > 0)
			tmprem <- which(logValues == 0)
			AUMC_1 <- sum(momentumValues[tmpid]*TimeSteps[tmpid]/logValues[tmpid]);
			if (length(tmprem) > 0)
			{
				AUMC_1 <- AUMC_1 + sum(TimeSteps[tmprem]* concM[(2:tmax_ix)[tmprem]])
			}
			
		} else {
			error ("Unknown method for calculating AUC.")
		}
	}
	
	TimeSteps <- time[(tmax_ix+1):length(time)] - time[tmax_ix : (length(time)-1)]
	if (tolower(methode) %in% c("log", "linlog"))
	{
		momentumValues <- concM[(tmax_ix+1):length(time)]-concM[tmax_ix : (length(time)-1)];
		logValues <- log(concM[(tmax_ix+1):length(time)]/concM[tmax_ix : (length(time)-1)])
		tmpid <- which(abs(logValues) > 0)
		tmprem <- which(logValues == 0)
		AUMC_2 <- sum(momentumValues[tmpid]*TimeSteps[tmpid]/logValues[tmpid]);
		if (length(tmprem) > 0)
		{
			AUMC_2 <- AUMC_2 + sum(TimeSteps[tmprem]* concM[((tmax_ix+1):length(time))[tmprem]])
		}
	} else {
		if (tolower(methode) == "lin")
		{
			momentumValues <- concM[(tmax_ix+1):length(time)]-concM[tmax_ix : (length(time)-1)];
			AUMC_2 <- sum(momentumValues*TimeSteps);	
		} else {
			error ("Unknown method for calculating AUC.")
		}
	}
	return(list(AUC = AUC, AUMC = AUMC_1+ AUMC_2))
}
