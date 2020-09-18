###=============================================================================
###	TRAC PHASE 2
###	FUNCTIONS AND SOURCE FOR DESCRIPTIVE STATISTICS OF LIFELOG
###	BUMJOON KANG, UFL
###=============================================================================

# Data read and output ----------------------------------------------------------------

# fix column names
fix.colnames.1 <- function(x) {
  if (!is.data.frame(x)) {
      stop("x is not a data frame")
  }
  command.setname <- paste("colnames(", x, ") <- gsub('_','.',tolower(colnames(", x, ")))", sep="")
  cat (command.setname)
  eval(parse(text=command.setname))
  #colnames(x) <- gsub("_",".",tolower(colnames(x)))
  #colnames(x)<-gsub("..", ".", colnames(x), fixed=T)
  #colnames(x)<-gsub(" ", ".", colnames(x))
  #colnames(x)<-gsub("/", ".", colnames(x))
  #olnames(x)<-gsub("#", "num", colnames(x))
  #return(x)
}

fix.colnames <- function(x) {
  if (!is.data.frame(x)) {
      stop("x is not a data frame")
  }
  
  v.names <- colnames(x)
  v.names <- gsub("_",".",tolower(v.names))
  v.names <-gsub("..", ".", v.names, fixed=T)
  v.names <-gsub(" ", ".", v.names)
  v.names <-gsub("/", ".", v.names)
  v.names <-gsub("-", ".", v.names)
  v.names <-gsub("#", "num", v.names)
  return(v.names)
}

unfix.colnames <- function(x) {
  if (!is.data.frame(x)) {
      stop("x is not a data frame")
  }
  v.names <- colnames(x)
  v.names <- gsub(".","_",tolower(v.names), fixed=T)
  v.names <- gsub("..", "_", v.names, fixed=T)
  v.names <- gsub(" ", "_", v.names)
  v.names <- gsub("/", "_", v.names)
  v.names <- gsub("#", "num", v.names)
  return(v.names)
}


# Graphics ----------------------------------------------------------------

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

# Easy tips--------------------------------------------------------------
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
"%not in%" <- function (x, y) is.na(match(x, y, nomatch=NA_integer_))

#(1:10) %w/o% c(3,7,12)

###=============================================================================
### Make variable names
###=============================================================================

make.var.prefix.suffix <- function(prefix,suffix) {
	x <- NULL
	for(j in 1:length(suffix)) {
		for(k in 1:length(prefix)) {
			x <- c(x, paste(prefix[k],suffix[j],sep="_"))
		}
	}
	x
}
make.var.suffix.prefix <- function(prefix,suffix) {
	x <- NULL
	for(k in 1:length(prefix)) {
		for(j in 1:length(suffix)) {
			x <- c(x, paste(prefix[k],suffix[j],sep="_"))
		}
	}
	x
}

###-----------------------------------------------------------------------------
### variables for generating other variables
###-----------------------------------------------------------------------------
var.dist <- c("min","25qtl","median","mean","75qtl","max","sd")
var.tm <- c("trip","walk","bike","car","transit")
var.rec <- c("acc","gps","place","trip")

###-----------------------------------------------------------------------------
### variables for data collection
###-----------------------------------------------------------------------------
var.valid <- make.var.prefix.suffix(c("vd"),"n")
var.raw <- make.var.prefix.suffix(var.rec, "raw_n")
var.num <- make.var.prefix.suffix(c(var.rec,"bout_500x5x7"), "n")
var.data.collection <- c(var.valid, var.raw, var.num)

###-----------------------------------------------------------------------------
### variables for person-day level variables
###-----------------------------------------------------------------------------
### accelerometer data
var.acc <- c("acc_d_hour",
	     "nonwear_0x20_d_hour",
	     make.var.prefix.suffix(c("mvpa_500","mpa_2020_5999","vpa_5999p"),
				    "d_minute"),
	     "count_d_sum")
### count distribution
var.count.dist <- make.var.prefix.suffix("count", var.dist)
### gps data
var.gps <- c("gps_d_minute")
### speed distribution
var.speed.dist <- make.var.prefix.suffix("speed", var.dist)
### place data
var.place <- c("place_d_n",
	       make.var.prefix.suffix(c("place","home","work","fitness"),
				      "d_minute"))
### trip data
var.trip <- make.var.prefix.suffix(var.tm, c("d_n","d_minute"))
### accelerometer + GPS data
var.acc.gps <- "acc_to_gps_d_ratio"
### accelerometer + place data
var.acc.place <- make.var.prefix.suffix(c("place","home","work","fitness"),
					"count_d_sum")
### accelerometer + trip data
var.acc.trip <-  make.var.prefix.suffix(var.tm, "count_d_sum")
### GPS + place data
var.gps.place <- "gps_to_place_d_ratio"
### GPS + trip data
var.gps.trip <- "gps_to_trip_d_ratio"
### bout
var.bout <- make.var.suffix.prefix(c("bout","bout_500x5x7","bout_500x4x5"),
				   c("d_n","d_minute","count_d_sum"))
### all of above
var.person.day <- c(var.acc, var.count.dist, var.gps, var.speed.dist, var.place, var.trip,
		    var.acc.gps, var.acc.place, var.acc.trip, var.gps.place, var.gps.trip, var.bout)

###-----------------------------------------------------------------------------
### variables for person-average-day level
###-----------------------------------------------------------------------------
var.person.avg.day <- var.person.day
var.cs.dist <- c(var.count.dist, var.speed.dist)


################################################################################
###                                                                          ###
### Data collection level                                                    ###
###                                                                          ###
################################################################################

###=============================================================================
### Get stats at subject level
###=============================================================================

get.stat.data.collection <- function() {
	##----------------------------------------------------------------------
	## result to be stored here
	##----------------------------------------------------------------------
	stat.data.collection.i <- data.frame(matrix(data=NA, nrow=1, ncol=length(var.data.collection)+1))
	colnames(stat.data.collection.i) <- c("id", var.data.collection)

	## reset all values as NA
	for(j in 1:length(var.data.collection)) {
		eval(parse(text=sprintf("%s <- NA", var.data.collection[j])))
	}

	##----------------------------------------------------------------------
	## calc new stats
	##----------------------------------------------------------------------
	###vdl_n <- length(unique(subset(dat.raw, vdl==1)$jday_trac) %w/o% NA)
	vd_n <- length(unique(dat$jdaya_trac) %w/o% NA) # generic valid days

	## raw data
	acc_raw_n <- nrow(subset(dat.raw, !is.na(recnum_acc)))
	gps_raw_n <- nrow(subset(dat.raw, !is.na(recnum_gps)))
	place_raw_n <- length(unique(dat.raw$recnum_place) %w/o% NA)
	trip_raw_n <- length(unique(dat.raw$recnum_trip) %w/o% NA)

	## number of records in total
	acc_n <- nrow(subset(dat, !is.na(recnum_acc)))
	gps_n <- nrow(subset(dat, !is.na(recnum_gps))) # number of GPS records
	place_n <- length(unique(dat$recnum_place) %w/o% NA)
	trip_n <- length(unique(dat$recnum_trip) %w/o% NA)
	bout_500x5x7_n <- length(unique(dat$bout_num_500x5x7) %w/o% NA)

	##----------------------------------------------------------------------
	## store results
	##----------------------------------------------------------------------
	for (j in 1:length(var.data.collection)) {
		stat.data.collection.i[1, var.data.collection[j]] <- eval(parse(text=sprintf("%s", var.data.collection[j])))
	}
	stat.data.collection.i[1,"id"] <- id.
	## output result
	stat.data.collection.i
}

################################################################################
###                                                                          ###
### Person-day level                                                         ###
###                                                                          ###
################################################################################

###=============================================================================
### Get stats at splitted day level
###=============================================================================

get.stat.person.day <- function() {

	##----------------------------------------------------------------------
	## reset all values as NA
	##----------------------------------------------------------------------
	for(k in 1:length(var.person.day)) {
		eval(parse(text=sprintf("%s <- NA", var.person.day[k])))
	}

	##----------------------------------------------------------------------
	## calc new stats for j-th day
	##----------------------------------------------------------------------

	## accelerometer data
	acc_d_hour <- nrow(subset(dasj, !is.na(recnum_acc))) / (2*60)
	nonwear_0x20_d_hour <- (sum(dasj$nonwear_0x20, na.rm=TRUE) + sum(is.na(dasj$nonwear_0x20)))/(2*60)
	nonwear_0x60_2_d_hour <- (sum(dasj$nonwear_0x60_2, na.rm=TRUE) + sum(is.na(dasj$nonwear_0x60_2)))/(2*60)

	mvpa_500_d_minute <- sum(dasj$mvpa_500==1, na.rm=TRUE) / 2
	mpa_2020_5999_d_minute <- sum(dasj$counts >= 2020/2 & dasj$counts < 5999/2, na.rm=TRUE) / 2
	vpa_5999p_d_minute <- sum(dasj$counts >= 5999/2, na.rm=TRUE) / 2

	count_d_sum <- sum(subset(dasj, !is.na(recnum_acc) & vd==1)$counts, na.rm=T)

	## count distribution
	x <- subset(dasj, !is.na(recnum_acc))$counts

	if(length(x)>0) {		# with only available ACC data
		count_min <- min(x)
		count_25qtl <- quantile(x, probs=0.25)
		count_median <- median(x)
		count_mean <- mean(x)
		count_75qtl <- quantile(x, probs=0.75)
		count_max <- max(x)
		count_sd <- sd(x)
	}

	## gps data
	gps_d_minute <- nrow(subset(dasj, !is.na(recnum_gps))) / 2

	## speed distribution
	x <- subset(dasj, !is.na(recnum_gps))$speed_kmh

	if(length(x)>0) {		# with only available GPS data
		speed_min <- min(x)
		speed_25qtl <- quantile(x, probs=0.25)
		speed_median <- median(x)
		speed_mean <- mean(x)
		speed_75qtl <- quantile(x, probs=0.75)
		speed_max <- max(x)
		speed_sd <- sd(x)
	}

	## place data
	place_d_n <- length(unique(dasj$seq_id) %w/o% NA)
	place_d_minute <- sum(!is.na(dasj$seq_id)) / 2
	home_d_minute <- length(grep("HOME|HOUSE", toupper(dasj$placename))) / 2
	work_d_minute <- sum(dasj$activity==10,na.rm=T) / 2
	fitness_d_minute <- sum(dasj$activity==5,na.rm=T) / 2

	## trip data
	## trip frequency
	trip_d_n <- length(unique(dasj$tripnum) %w/o% NA)
	walk_d_n <- length(unique(subset(dasj, travel_mode==13)$tripnum) %w/o% NA)
	bike_d_n <- length(unique(subset(dasj, travel_mode==12)$tripnum) %w/o% NA)
	car_d_n <- length(unique(subset(dasj, travel_mode==1 | travel_mode==2)$tripnum) %w/o% NA)
	transit_d_n <- length(unique(subset(dasj, travel_mode==3 |
					    travel_mode==4 |
					    travel_mode==5 |
					    travel_mode==6)$tripnum) %w/o% NA)
	## trip duration
	trip_d_minute <- sum(!is.na(dasj$tripnum)) / 2
	walk_d_minute <- sum(dasj$travel_mode==13, na.rm=T) / 2
	bike_d_minute <- sum(dasj$travel_mode==12, na.rm=T) / 2
	car_d_minute <- sum(dasj$travel_mode==1 | dasj$travel_mode==2 , na.rm=T) / 2
	transit_d_minute <- sum(dasj$travel_mode==3 |
				dasj$travel_mode==4 |
				dasj$travel_mode==5 |
				dasj$travel_mode==6, na.rm=T) / 2

	## accelerometer + GPS data
	acc_to_gps_d_ratio <- mean(!is.na(subset(dasj, !is.na(recnum_acc))$recnum_gps))

	## accelerometer + place data
	place_count_d_sum <- sum(subset(dasj, !is.na(seq_id))$counts, na.rm=T)
	home_count_d_sum <- sum(dasj[grep("HOME|HOUSE", toupper(dasj$placename)),]$counts, na.rm=T)
	work_count_d_sum <- sum(subset(dasj, activity==10)$counts,na.rm=T)
	fitness_count_d_sum <- sum(subset(dasj, activity==5)$counts,na.rm=T)

	## accelerometer + trip data
	trip_count_d_sum <- sum(subset(dasj, !is.na(tripnum))$counts,na.rm=T)
	walk_count_d_sum <- sum(subset(dasj, travel_mode==13)$counts,na.rm=T)
	bike_count_d_sum <- sum(subset(dasj, travel_mode==12)$counts,na.rm=T)
	car_count_d_sum <- sum(subset(dasj, travel_mode==1 | travel_mode==2)$counts,na.rm=T)
	transit_count_d_sum <- sum(subset(dasj, travel_mode==3 |
					  travel_mode==4 |
					  travel_mode==5 |
					  travel_mode==6)$counts,na.rm=T)

	## GPS + place data
	gps_to_place_d_ratio <- ifelse(gps_d_minute > 0, mean(!is.na(subset(dasj, !is.na(recnum_gps))$seq_id)), NA)

	## GPS + trip data
	gps_to_trip_d_ratio <- ifelse(gps_d_minute > 0, mean(!is.na(subset(dasj, !is.na(recnum_gps))$tripnum)), NA)

	## bout
	bout_500x5x7_d_n <- length(unique(dasj$bout_num_500x5x7) %w/o% NA)
	bout_500x5x7_d_minute <- nrow(subset(dasj, bout_500x5x7==1)) / 2
	bout_500x5x7_count_d_sum <- sum(subset(dasj, bout_500x5x7==1)$counts, na.rm=T)

	bout_500x4x5_d_n <- length(unique(dasj$bout_num_500x4x5) %w/o% NA)
	bout_500x4x5_d_minute <- nrow(subset(dasj, bout_500x4x5==1)) / 2
	bout_500x4x5_count_d_sum <- sum(subset(dasj, bout_500x4x5==1)$counts, na.rm=T)

	bout_d_n <- 		bout_500x5x7_d_n + 	bout_500x4x5_d_n
	bout_d_minute <- 	bout_500x5x7_d_minute + 	bout_500x4x5_d_minute
	bout_count_d_sum <- 	bout_500x5x7_count_d_sum + 	bout_500x4x5_count_d_sum

	stat.person.day.i[j,"id"] <- id.
	stat.person.day.i[j,"id_jday_trac"] <- jday.names[j]
	stat.person.day.i[j,"id_day_trac"] <- dasj$id_day_trac[1]

	##----------------------------------------------------------------------
	## store stats for j-th day
	##----------------------------------------------------------------------

	for (k in 1:length(var.person.day)) {
		stat.person.day.i[j, var.person.day[k]] <- eval(parse(text=sprintf("%s", var.person.day[k])))
	}
	stat.person.day.i
}

################################################################################
###                                                                          ###
### Person-average-day level                                                 ###
###                                                                          ###
################################################################################

###=============================================================================
### Get stats at subject level from dat
###=============================================================================

get.stat.cs.dist <- function() {

	##----------------------------------------------------------------------
	## result to be stored here
	##----------------------------------------------------------------------
	stat.cs.dist.i <- data.frame(matrix(data=NA, nrow=1, ncol=length(var.cs.dist)+1))
	colnames(stat.cs.dist.i) <- c("id", var.cs.dist)

	## reset all values as NA
	for(j in 1:length(var.cs.dist)) {
		eval(parse(text=sprintf("%s <- NA", var.cs.dist[j])))
	}

	##----------------------------------------------------------------------
	## calc new stats
	##----------------------------------------------------------------------
	## count distribution
	x <- subset(dat, !is.na(recnum_acc))$counts

	if(length(x)>0) {		# with only available ACC data
		count_min <- min(x)
		count_25qtl <- quantile(x, probs=0.25)
		count_median <- median(x)
		count_mean <- mean(x)
		count_75qtl <- quantile(x, probs=0.75)
		count_max <- max(x)
		count_sd <- sd(x)
	}

	## speed distribution
	x <- subset(dat, !is.na(recnum_gps))$speed_kmh

	if(length(x)>0) {		# with only available GPS data
		speed_min <- min(x)
		speed_25qtl <- quantile(x, probs=0.25)
		speed_median <- median(x)
		speed_mean <- mean(x)
		speed_75qtl <- quantile(x, probs=0.75)
		speed_max <- max(x)
		speed_sd <- sd(x)
	}

	##----------------------------------------------------------------------
	## store results
	##----------------------------------------------------------------------
	for (j in 1:length(var.cs.dist)) {
		stat.cs.dist.i[1, var.cs.dist[j]] <- eval(parse(text=sprintf("%s", var.cs.dist[j])))
	}
	stat.cs.dist.i[1,"id"] <- id.
	## output result
	stat.cs.dist.i
}

################################################################################
###                                                                          ###
### Bout Level summary of summaries                                          ###
###                                                                          ###
################################################################################

###=============================================================================
### Get stats of records of bout subset
###=============================================================================

get.stat.bout <- function(dat) {
	subject_bout_n <- length(unique(dat$id))
	bout_n <- length(unique(dat$id_bout_num))
	rec_bout_n <- nrow(dat)
	rec_bout_gps_n <- nrow(subset(dat, !is.na(recnum_gps)))

	x <- dat$counts
	if(length(x)>0) {		# with only available ACC data
		count_bout_min <- min(x)
		count_bout_25qtl <- quantile(x, probs=0.25)
		count_bout_median <- median(x)
		count_bout_mean <- mean(x)
		count_bout_75qtl <- quantile(x, probs=0.75)
		count_bout_max <- max(x)
		count_bout_sd <- sd(x)
	}

	x <- subset(dat, !is.na(recnum_gps))$speed_kmh
	if(length(x)>0) {		# with only available ACC data
		speed_bout_min <- min(x)
		speed_bout_25qtl <- quantile(x, probs=0.25)
		speed_bout_median <- median(x)
		speed_bout_mean <- mean(x)
		speed_bout_75qtl <- quantile(x, probs=0.75)
		speed_bout_max <- max(x)
		speed_bout_sd <- sd(x)
	}
	var.bout.rec <- c("subject_bout_n","bout_n","rec_bout_n","rec_bout_gps_n",
			  make.var.suffix.prefix(c("count_bout","speed_bout"), var.dist))

	bout.rec <- data.frame(matrix(data=NA, nrow=1, ncol=length(var.bout.rec)))
	colnames(bout.rec) <- var.bout.rec

	for(k in 1:length(var.bout.rec)) {
		eval(parse(text=sprintf("bout.rec[1,k] <- %s", var.bout.rec[k])))
	}
bout.rec
}

###=============================================================================
### Get stats of bouts at bout level
###=============================================================================

### general information
###-----------------------------------------------------------------------------
get.stat.per.bout <- function(dat) {
	## duration
	bout.duration <- aggregate(dat$id_bout_num, by=list(dat$id_bout_num), function(x) length(x)/2)
	colnames(bout.duration) <- c("id_bout_num","bout_duration_minute")

	## minutes of mvpa500
	mvpa500 <- aggregate(dat$mvpa500_minute, by=list(dat$id_bout_num), function(x) length(x)/2)
	colnames(bout.duration) <- c("id_bout_num","mvpa500_minute")

	## gps coverage
	bout.gps.cov <- aggregate(!is.na(dat$recnum_gps), by=list(dat$id_bout_num), mean)
	bout.speed.mean <- aggregate(dat$speed, by=list(dat$id_bout_num), function(x) mean(x, na.rm=T))
	colnames(bout.speed.mean) <- c("id_bout_num","bout_speed_kmh_mean")

	stat.bout <- data.frame(id_bout_num=unique(dat$id_bout_num))
	stat.bout <- merge(stat.bout, bout.duration, by="id_bout_num", all=T)
	stat.bout <- merge(stat.bout, bout.gps.cov, by="id_bout_num", all=T)
	stat.bout <- merge(stat.bout, bout.count.mean, by="id_bout_num", all=T)
	stat.bout <- merge(stat.bout, bout.speed.mean, by="id_bout_num", all=T)
	stat.bout <- within(stat.bout, bout_speed_kmh_mean <- ifelse(bout_gps_cov_ratio==0, NA, bout_speed_kmh_mean))

	stat.name.bout <- colnames(stat.bout) %w/o% "id_bout_num"
	per.bout <- NULL
	for(i in 1:length(stat.name.bout)) {
		stat <- stat.name.bout[i]
		eval(parse(text=sprintf('
			   x <- stat.bout$%s
			   xx <- data.frame(
				      %s_min = min(x, na.rm=T),
				      %s_25qtl = quantile(x, probs=0.25, na.rm=T),
				      %s_median = median(x, na.rm=T),
				      %s_mean = mean(x, na.rm=T),
				      %s_75qtl = quantile(x, probs=0.75, na.rm=T),
				      %s_max = max(x, na.rm=T),
				      %s_sd = sd(x, na.rm=T))',
			   stat,stat,stat,stat,stat,stat,stat,stat)))
		per.bout <- data.frame(c(per.bout, xx))
	}
	per.bout
}

### bout X trip/place data
###-----------------------------------------------------------------------------
## all trips
string.tm <- 'data.frame(rbind(
			   bout_overlap_%s_n=c(length(id_bout_num.trip), NA),
			   bout_overlap_%s_proportion = c(mean(with(bout.ovlp.trip, aggregate(!is.na(tripnum), 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
					                    sd(with(bout.ovlp.trip, aggregate(!is.na(tripnum), 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
			   bout_overlap_%s_count = c(mean(with(bout.ovlp.trip, aggregate(counts, 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
					               sd(with(bout.ovlp.trip, aggregate(counts, 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
			   bout_overlap_%s_gpv_cov = c(mean(with(bout.ovlp.trip, aggregate(!is.na(recnum_gps), 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
					                 sd(with(bout.ovlp.trip, aggregate(!is.na(recnum_gps), 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
			   bout_overlap_%s_speed = c(mean(with(bout.ovlp.trip, aggregate(speed_kmh, 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
					               sd(with(bout.ovlp.trip, aggregate(speed_kmh, 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T))
			   ))'
## by travel modes
get.stat.bout.travel.mode <- function(bout.all, tm, var.tm) {
	## bout.all: given data
	## tm: given travel mode vector
	## var.tm: given travel mode character for variable names
	id_bout_num.x <- unique(subset(bout.all, travel_mode %in% tm)$id_bout_num) # only ids for those bouts overlapping with the given travel modes
	bout.ovlp.x <- subset(bout.all, id_bout_num %in% id_bout_num.x) # only those bouts overlapping with the given travel modes

	string <- 'data.frame(rbind(
				   bout_overlap_%s_n = c(length(id_bout_num.x), NA),
				   bout_overlap_%s_proportion = c(mean(with(bout.ovlp.x, aggregate(travel_mode %%in%% tm, 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
						                    sd(with(bout.ovlp.x, aggregate(travel_mode %%in%% tm, 	by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
				   bout_overlap_%s_count = c(mean(with(bout.ovlp.x, aggregate(counts,    			by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
						               sd(with(bout.ovlp.x, aggregate(counts,    			by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
				   bout_overlap_%s_gpv_cov = c(mean(with(bout.ovlp.x, aggregate(!is.na(recnum_gps), 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
						                 sd(with(bout.ovlp.x, aggregate(!is.na(recnum_gps), 		by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T)),
				   bout_overlap_%s_speed = c(mean(with(bout.ovlp.x, aggregate(speed_kmh,			by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T),
						               sd(with(bout.ovlp.x, aggregate(speed_kmh, 			by=list(id_bout_num), function(x) mean(x, na.rm=T)))$x, na.rm=T))
				   ))'
	eval(parse(text=sprintf(string, var.tm,var.tm,var.tm,var.tm,var.tm)))
}

################################################################################
###                                                                          ###
### Read table from gist server                                              ###
###                                                                          ###
################################################################################

lifelog.columns <- "
id
, recnum_acc
, recnum_gps
, recnum_place
, recnum_trip
, daygroup
, axis1
, axis2
, axis3
, counts
, time_acc_utc
, time_acc_local
, jdaya
, jdaya_trac
, mvpa_500x5x7
, bout_500x5x7
, bout_num_500x5x7
, acc_source
, wear_troiano
, nonwear_0x20
, vda_0x20
, latitude
, longitude
, speed_kmh
, altitude_m
, time_gps_utc
, time_gps_utc_std
, x_waspn
, y_waspn
, jdayg
, jdayg_trac
, gps_source
, placenum_from
, t_time_start_utc
, t_time_end_utc
, t_time_start_local
, t_time_end_local
, placename_from
, placenum_to
, placename_to
, travel_mode
, duration_minutes
, tour
, daynum
, placenum
, placename
, address
, city
, zip
, activity
, comment
, calendar_date
, p_time_arrived_utc
, p_time_left_utc
, p_time_arrived_local
, p_time_left_local
, vd
"
###=============================================================================
### for pushing into gist server
###=============================================================================
lifelog.schema <- "
(
id
, text
, recnum_acc integer
, recnum_gps bigint
, recnum_place bigint
, recnum_trip bigint
, daygroup bigint
, axis1 integer
, axis2 integer
, axis3 integer
, counts integer
, time_acc_utc timestamp without time zone
, time_acc_local timestamp without time zone
, jdaya double precision
, jdaya_trac double precision
, mvpa_500x5x7 integer
, bout_500x5x7 integer
, bout_num_500x5x7 integer
, acc_source character varying
, wear_troiano integer
, nonwear_0x20 integer
, vda_0x20 integer
, latitude double precision
, longitude double precision
, speed_kmh double precision
, altitude_m double precision
, time_gps_utc timestamp without time zone
, time_gps_utc_std timestamp without time zone
, x_waspn double precision
, y_waspn double precision
, jdayg integer
, jdayg_trac integer
, gps_source character varying
--, the_geom_4326 geometry(PointZM, 4326) -------------------- geom
--, the_geom_2926 geometry(PointZM, 2926) -------------------- geom
, placenum_from bigint
, t_time_start_utc timestamp without time zone
, t_time_end_utc timestamp without time zone
, t_time_start_local timestamp without time zone
, t_time_end_local timestamp without time zone
, placename_from text
, placenum_to bigint
, placename_to text
, travel_mode text
, duration_minutes bigint
, tour integer
, daynum bigint
, placenum bigint
, placename text
, address text
, city text
, zip text
, activity text
, comment character varying
, calendar_date date
, p_time_arrived_utc timestamp without time zone
, p_time_left_utc timestamp without time zone
, p_time_arrived_local timestamp without time zone
, p_time_left_local timestamp without time zone
, vd integer
)"

################################################################################
###                                                                          ###
### Lifelog                                                                  ###
###                                                                          ###
################################################################################

### functions
### considering spring forward and fall back, calc jday_trac
spring.forward <- c("2004-04-04","2005-04-03","2006-04-02","2007-03-11","2008-03-09","2009-03-08","2010-03-14","2011-03-13","2012-03-11","2013-03-10","2014-03-09")
fall.back <-      c("2004-10-31","2005-10-30","2006-10-29","2007-11-04","2008-11-02","2009-11-01","2010-11-07","2011-11-06","2012-11-04","2013-11-03","2014-11-02")


get.jday.trac <- function(x) {
	date.jday <- substr(x, 1, 10)
	hour.subtract <- rep(3,length(x))
	hour.subtract <- ifelse(date.jday %in% spring.forward, 2, hour.subtract)
	hour.subtract <- ifelse(date.jday %in% fall.back,      4, hour.subtract)
	as.POSIXlt(x-hour.subtract*60*60)$yday
}

get.day.trac <- function(x) {
  date.jday <- substr(x, 1, 10)
  hour.subtract <- rep(3,length(x))
  hour.subtract <- ifelse(date.jday %in% spring.forward, 2, hour.subtract)
  hour.subtract <- ifelse(date.jday %in% fall.back,      4, hour.subtract)
  x1 <- as.POSIXlt(x-hour.subtract*60*60)
  x1$hour <- 0
  x1$min <- 0
  x1$sec <- 0
  as.character(x1)
}

################################################################################
###                                                                          ###
### Misc.                                                                    ###
###                                                                          ###
################################################################################

tm.code <- data.frame(code=c(1:14,97),tm=c("car","carpool","bus","light_rail","monorail","heavy_rail","dial-a-ride","school_bus","ferry","taxi","motocycle","bicycle","walk","airplane","other"))

###=============================================================================
### END OF CODE
###=============================================================================


