#Make sure we're in the right place.
# Set to working directory where data is
wd <- ''
setwd(wd)

library(maps)

#Check if data file is there, if it is read it into cali_penn_census data frame
# This data can be found at http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/01/calif_penn_2011.csv
if(file.exists('calif_penn_2011.csv')) {
	cali_penn_census <- read.csv('calif_penn_2011.csv', sep=',')
} else {
	print("calif_penn_2011.csv not found in this directory\n")
	quit(1)
}

#Removing tracts and printing out a few stats about them Question 1
print("Removing rows with NA's...")
cali_penn_census_clean <- na.omit(cali_penn_census)

question_1.a <- function () {
	num_na <- length(cali_penn_census[,1]) - length(cali_penn_census_clean[,1])
	print( paste("Removed ", num_na, " na's", sep="") )
}

question_1.b <- function () {
	removed_tracts <- setdiff(cali_penn_census , cali_penn_census_clean)
	pop_na <- sum( removed_tracts$POPULATION )
	print( paste("Removed NA's had population of", pop_na, sep="") )
}

question_1.c <- function() {
	print( "Summary of Median House values for overall dataset vs cleaned data set" ) 
	summary(cali_penn_census$Median_house_value)
	summary(cali_penn_census_clean$Median_house_value)

	print( "Summary of Median Household income for overall dataset vs cleaned data set" ) 
	summary(cali_penn_census$Median_household_income)
	summary(cali_penn_census_clean$Median_household_income)
}

question_2.a <- function () {
	med_house_val <- cali_penn_census_clean$Median_house_value
	med_house_inc <- cali_penn_census_clean$Median_household_income
	val_inc.fit <- lm( med_house_val ~ med_house_inc ) 
	plot( med_house_inc, med_house_val, 
		xlab="Median Household Income", ylab="Median Household Value",
		cex=0.1, pch=3, col="red"
	)
	abline(val_inc.fit)
	summary(val_inc.fit)
}

question_2.b <- function () {
	med_house_val <- cali_penn_census_clean$Median_house_value
	# mean_househould_income Mispelled in Data frame.  Figured I'd just leave it as is
	mean_house_inc <- cali_penn_census_clean$Mean_househould_income
	val_inc.fit <- lm( med_house_val ~ mean_house_inc ) 
	plot( mean_house_inc, med_house_val, 
		xlab="Mean Household Income", ylab="Median Household Value",
		cex=0.1, pch=3, col="red"
	)
	abline(val_inc.fit)
	summary(val_inc.fit)
}

question_2.c <- function () {
	med_house_val <- cali_penn_census_clean$Median_house_value
	# mean_househould_income Mispelled in Data frame.  Figured I'd just leave it as is
	med_house_inc <- cali_penn_census_clean$Median_household_income
	mean_house_inc <- cali_penn_census_clean$Mean_househould_income

	val_inc.fit <- lm( med_house_val ~ mean_house_inc + med_house_inc ) 
	summary(val_inc.fit)
}

filter_census_by_state <- function ( filter = '' ) {
	ifelse( filter=='', 
	   records <- cali_penn_census_clean ,
		{ grp_vec <- grepl( filter , cali_penn_census_clean$GEO.display.label , 
			ignore.case=TRUE
		)  
	   records <- cali_penn_census_clean[ grp_vec , ] }
	)
	return(records)
}

question_3_4_5_6_setup <- function ( filter = '' ) {
	records <- filter_census_by_state(filter)
	
	val_inc.fit <- lm( Median_house_value ~ Mean_househould_income + 
		Median_household_income + 
		POPULATION + 
		Total_units +
		Vacant_units + 
		Owners + 
		Median_rooms + 
		Mean_household_size_owners + 
		Mean_household_size_renters , data=records ) 
	return(val_inc.fit)
}

question_3 <- function () {
	val_inc.fit <- question_3_4_5_6_setup()
	summary(val_inc.fit)
}

question_5.a <- function () {
	val_inc.fit <- question_3_4_5_6_setup()
	plot(val_inc.fit, which=2)
}

question_5.b.plotter <- function (predictor_var, resids, xlab_in) {
	#Rule of thumb for bandwidth is sd*n^(-1/5)
	band_wif <- sd(predictor_var) * length(predictor_var)^(-1/5)
	plot(predictor_var, resids, xlab=xlab_in, ylab="Residuals")
	abline(0,0, col="black")
	lines(ksmooth(predictor_var, resids, "normal", bandwidth=band_wif), col="green")
}

question_5.b <- function () {
	med_house_val <- cali_penn_census_clean$Median_house_value
	# mean_househould_income Mispelled in Data frame.  Figured I'd just leave it as is
	med_house_inc <- cali_penn_census_clean$Median_household_income
	mean_house_inc <- cali_penn_census_clean$Mean_househould_income
	pop <- cali_penn_census_clean$POPULATION 
	tot_units <- cali_penn_census_clean$Total_units
	vac_units <- cali_penn_census_clean$Vacant_units
	owners <- cali_penn_census_clean$Owners
	med_rooms <- cali_penn_census_clean$Median_rooms
	mean_household_size_owners <- cali_penn_census_clean$Mean_household_size_owners
	mean_household_size_renters <- cali_penn_census_clean$Mean_household_size_renters

	val_inc.fit <- question_3_4_5_6_setup()
	par(mfrow=c(2,5), cex=0.3, pch=3)
	question_5.b.plotter(med_house_val, resid(val_inc.fit), "Median House Value" )
	question_5.b.plotter(med_house_inc, resid(val_inc.fit), "Median House Income") 
	question_5.b.plotter(mean_house_inc, resid(val_inc.fit), "Mean House Income") 
	question_5.b.plotter(pop, resid(val_inc.fit), "Population") 
	question_5.b.plotter(tot_units, resid(val_inc.fit), "Total # Units in Tract") 
	question_5.b.plotter(vac_units, resid(val_inc.fit), "Vacant # Units in Tract") 
	question_5.b.plotter(owners, resid(val_inc.fit), "# of Owners") 
	question_5.b.plotter(med_rooms, resid(val_inc.fit), "Median Number of Rooms") 
	question_5.b.plotter(mean_household_size_owners, resid(val_inc.fit), "Mean Household Size (Owners)") 
	question_5.b.plotter(mean_household_size_renters, resid(val_inc.fit), "Mean Household Size (Renters)") 
}

question_5.c <- function () {
	med_house_val <- cali_penn_census_clean$Median_house_value
	# mean_househould_income Mispelled in Data frame.  Figured I'd just leave it as is
	med_house_inc <- cali_penn_census_clean$Median_household_income
	mean_house_inc <- cali_penn_census_clean$Mean_househould_income
	pop <- cali_penn_census_clean$POPULATION 
	tot_units <- cali_penn_census_clean$Total_units
	vac_units <- cali_penn_census_clean$Vacant_units
	owners <- cali_penn_census_clean$Owners
	med_rooms <- cali_penn_census_clean$Median_rooms
	mean_household_size_owners <- cali_penn_census_clean$Mean_household_size_owners
	mean_household_size_renters <- cali_penn_census_clean$Mean_household_size_renters

	val_inc.fit <- question_3_4_5_6_setup()
	par(mfrow=c(2,5), cex=0.3, pch=3)
	question_5.b.plotter(med_house_val, resid(val_inc.fit)^2, "Median House Value" )
	question_5.b.plotter(med_house_inc, resid(val_inc.fit)^2, "Median House Income") 
	question_5.b.plotter(mean_house_inc, resid(val_inc.fit)^2, "Mean House Income") 
	question_5.b.plotter(pop, resid(val_inc.fit)^2, "Population") 
	question_5.b.plotter(tot_units, resid(val_inc.fit)^2, "Total # Units in Tract") 
	question_5.b.plotter(vac_units, resid(val_inc.fit)^2, "Vacant # Units in Tract") 
	question_5.b.plotter(owners, resid(val_inc.fit)^2, "# of Owners") 
	question_5.b.plotter(med_rooms, resid(val_inc.fit)^2, "Median Number of Rooms") 
	question_5.b.plotter(mean_household_size_owners, resid(val_inc.fit)^2, "Mean Household Size (Owners)") 
	question_5.b.plotter(mean_household_size_renters, resid(val_inc.fit)^2, "Mean Household Size (Renters)") 
}

question_6 <- function( part ) {
	cal.fit <- question_3_4_5_6_setup('California')
	penn.fit <- question_3_4_5_6_setup('Pennsylvania')

	if(part == 'a') {
		summary(cal.fit)
		summary(penn.fit)
	}
	if(part == 'b') {
		print("California's RMSE:")
		summary(cal.fit)$sigma
		print("Pennsylvania's RMSE:")
		summary(penn.fit)$sigma
	}
	if(part == 'c') {
		pred <- predict( cal.fit, data=filter_census_by_state('Pennsylvania'), 
			se.fit=TRUE 
		)
		print("The predictions RMSE is:")
		pred$residual.scale
		print("The correlation between the coefficients of the two models is:")
		cor(cal.fit$coefficients, penn.fit$coefficients)
	}
	 
}

question_6.a <- function() { question_6('a') }
question_6.b <- function() { question_6('b') }
question_6.c <- function() { question_6('c') }

# Make a map of the median house values
# Color of house value determined by value 
# (higher: closer to red, lower:closer to blue)

question_7 <- function () {
	par( mar = c(1,1,1,1) )
	par( mfrow=c(1,2) )

	#library(rworldmap)
	#newmap <- getMap( resolution = "low" )
	#plot Pennsylvania
	#plot(newmap, xlim = c(-80, -75), ylim = c(38, 43), asp = 2)
	#points( filter_census_by_state("Pennsylvania")$LONGITUDE, 
	#	  filter_census_by_state("Pennsylvania")$LATITUDE, 
	#	cex=0.6, col="red" )

	penn_records <- filter_census_by_state('Pennsylvania')
	map('county', 'pennsylvania', fill=FALSE, col = palette() )
	#Make color gradient
	colfunc <- colorRampPalette(c("blue", "red"))
	penn_records_ordered <- penn_records[ order(penn_records$Median_house_value) , ]
	points( penn_records_ordered$LONGITUDE, penn_records_ordered$LATITUDE, 
		cex=1, pch=20, col=colfunc( length(penn_records$LONGITUDE) ), asp=1 )

	cali_records <- filter_census_by_state('California')
	map('county', 'california', fill=FALSE, col="black" )
	colfunc <- colorRampPalette(c("blue", "red"))
	cali_records_ordered <- cali_records[ order(cali_records$Median_house_value) , ]
	points( cali_records_ordered$LONGITUDE, cali_records_ordered$LATITUDE, 
		cex=1, pch=20, col=colfunc( length(cali_records$LONGITUDE) ), asp=1 )
}

# This function should plot the two states side by side, and then graph their residuals
# across both states within the two respective graphs.
question_8 <- function () {
	par( mar = c(1,1,1,1) )

	val_inc.fit <- question_3_4_5_6_setup()
	census_clean <- cali_penn_census_clean 

	# 1 split residuals and join to penn_records and cali_records data frames
	# Hmm, it seems like there should be a shortcut for this but its the best I've got
	quest_8_df <- data.frame( state = census_clean$GEO.display.label, 
		lat = census_clean$LATITUDE,
		lon = census_clean$LONGITUDE,
		res = residuals(val_inc.fit),
		abs_res = abs( residuals(val_inc.fit) )
	)
	quest_8_df_res_ordered <- quest_8_df[ order(quest_8_df$res) , ]
	quest_8_df_absres_ordered <- quest_8_df[ order(quest_8_df$abs_res) , ]

	colfunc <- colorRampPalette(c("blue", "red"))

	quest_8_df_res_ordered$color_vec <- colfunc( nrow(quest_8_df_res_ordered) )
	quest_8_df_absres_ordered$color_vec <- colfunc( nrow(quest_8_df_absres_ordered) )

	# So now we should have two dataframes with colors attached
	# Now we plot
	
	# Make 4 graphics boxes on device.
	par( mfrow = c(2,2) ) 
	par( oma = c(0.1,0.1,0.1,0.1) )
	
	# Print penn residual map
	map('county', 'pennsylvania', fill=FALSE, col = "black" )
	title("Pennsylvania Residuals", cex.main=0.5)
	penn_points <- quest_8_df_res_ordered[ grepl( 'Pennsylvania', quest_8_df_res_ordered$state ) , ]
	points( penn_points$lon, penn_points$lat, cex=1, pch=20, col=penn_points$color_vec, asp=1 )
	
	map('county', 'california', fill=FALSE, col = "black" )
	title("California Residuals", cex.main=0.5)
	cali_points <- quest_8_df_res_ordered[ grepl( 'California', quest_8_df_res_ordered$state ) , ]
	points( cali_points$lon, cali_points$lat, cex=1, pch=20, col=cali_points$color_vec, asp=1 )

	map('county', 'pennsylvania', fill=FALSE, col = "black" )
	title("Pennsylvania Absolute Value of Residuals", cex.main=0.5)
	penn_points <- quest_8_df_absres_ordered[ grepl( 'Pennsylvania', quest_8_df_absres_ordered$state ) , ]
	points( penn_points$lon, penn_points$lat, cex=1, pch=20, col=penn_points$color_vec, asp=1 )
	
	map('county', 'california', fill=FALSE, col = "black" )
	title("California Absolute Value of Residuals", cex.main=0.5)
	cali_points <- quest_8_df_absres_ordered[ grepl( 'California', quest_8_df_absres_ordered$state ) , ]
	points( cali_points$lon, cali_points$lat, cex=1, pch=20, col=cali_points$color_vec, asp=1 )

}

question_9 <- function () {
	val_inc.fit <- lm( Median_house_value ~ Mean_househould_income + 
		Median_household_income + 
		POPULATION + 
		Total_units +
		Vacant_units + 
		Owners + 
		Median_rooms + 
		Mean_household_size_owners + 
		Mean_household_size_renters +
		LONGITUDE +
		LATITUDE , data=cali_penn_census_clean ) 
	summary(val_inc.fit)
	return(val_inc.fit)
}

question_10 <- function () {
	par( mar = c(1,1,1,1) )

	val_inc.fit <- question_9()
	census_clean <- cali_penn_census_clean 

	# 1 split residuals and join to penn_records and cali_records data frames
	# Hmm, it seems like there should be a shortcut for this but its the best I've got
	quest_8_df <- data.frame( state = census_clean$GEO.display.label, 
		lat = census_clean$LATITUDE,
		lon = census_clean$LONGITUDE,
		res = residuals(val_inc.fit),
		abs_res = abs( residuals(val_inc.fit) )
	)
	quest_8_df_res_ordered <- quest_8_df[ order(quest_8_df$res) , ]
	quest_8_df_absres_ordered <- quest_8_df[ order(quest_8_df$abs_res) , ]

	colfunc <- colorRampPalette(c("blue", "red"))

	quest_8_df_res_ordered$color_vec <- colfunc( nrow(quest_8_df_res_ordered) )
	quest_8_df_absres_ordered$color_vec <- colfunc( nrow(quest_8_df_absres_ordered) )

	# So now we should have two dataframes with colors attached
	# Now we plot
	
	# Make 4 graphics boxes on device.
	par( mfrow = c(2,2) ) 
	par( oma = c(0.1,0.1,0.1,0.1) )
	
	# Print penn residual map
	map('county', 'pennsylvania', fill=FALSE, col = "black" )
	title("Pennsylvania Residuals", cex.main=0.5)
	penn_points <- quest_8_df_res_ordered[ grepl( 'Pennsylvania', quest_8_df_res_ordered$state ) , ]
	points( penn_points$lon, penn_points$lat, cex=1, pch=20, col=penn_points$color_vec, asp=1 )
	
	map('county', 'california', fill=FALSE, col = "black" )
	title("California Residuals", cex.main=0.5)
	cali_points <- quest_8_df_res_ordered[ grepl( 'California', quest_8_df_res_ordered$state ) , ]
	points( cali_points$lon, cali_points$lat, cex=1, pch=20, col=cali_points$color_vec, asp=1 )

	map('county', 'pennsylvania', fill=FALSE, col = "black" )
	title("Pennsylvania Absolute Value of Residuals", cex.main=0.5)
	penn_points <- quest_8_df_absres_ordered[ grepl( 'Pennsylvania', quest_8_df_absres_ordered$state ) , ]
	points( penn_points$lon, penn_points$lat, cex=1, pch=20, col=penn_points$color_vec, asp=1 )
	
	map('county', 'california', fill=FALSE, col = "black" )
	title("California Absolute Value of Residuals", cex.main=0.5)
	cali_points <- quest_8_df_absres_ordered[ grepl( 'California', quest_8_df_absres_ordered$state ) , ]
	points( cali_points$lon, cali_points$lat, cex=1, pch=20, col=cali_points$color_vec, asp=1 )

}

question_11 <- function () {
	val_inc.fit <- lm( log(Median_house_value) ~ Mean_househould_income + 
		Median_household_income + 
		POPULATION + 
		Total_units +
		Vacant_units + 
		Owners + 
		Median_rooms + 
		Mean_household_size_owners + 
		Mean_household_size_renters +
		LONGITUDE +
		LATITUDE , data=cali_penn_census_clean ) 
	summary(val_inc.fit)
	return(val_inc.fit)
}
