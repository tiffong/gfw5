#' Get fishing hours from AIS data 
#'
#' @param bbox bounding box, vector c(minlat,maxlat,minlon,maxlon)
#' @param year year to query for effort
#' @param month1 MM of time interval start
#' @param month2 MM of time interval end
#' @param day1 DD of time interval start
#' @param day2 DD of time interval end
#' @param reslat resolution of latitude
#' @param reslon resolution of longitude
#' @param Area mainly for naming the files
#' @param cashed if TRUE means I want to work on local file that was previously downloaded, so non need to query the Google Cloud 
#' @param vesselType select the desired vessel type: "all" means all vessel types together (fishing vessels). So the output data is a three column data.frame with lat lon and fishing hours of all together; "allGroups" returns a 4-column data.frame with the additional column being the vessel type; one of the following - passenger, purse_seines,	unknown_fishing	, other_not_fishing, drifting_longlines, ERROR, reefer, remove, trawlers, squid_jigger, pole_and_line, drifting_gillnets, no_idea_what_it_is, cargo, research other_fishing, set_longlines, troller, set_gillnets, tender_packer tender_packer, tug	 sailing, pots_and_traps, seismic_vessel - returns a 3-column data.frame with hours being the hours of just the specified vessel. vesselType can also be a vector of types, i.e. c("purse_seines","drifting_longlines").
#' @param byVesselType MUST BE COMPLETED - not yet in use, designed to produce extra columns by gear type for each dataframe
#' @param paper if "Global" it gets global fishing effort from the fishing data classified by the convolutional neural network. Else it uses the manually classified fishing vessels for the NEP pacific paper. In both cases bbox needs to be specified.
#' @param country flag country. From now it works only with NEP data
#' @export
getAISFishHours = function(bbox = c(0,70,-180,-95), year = "2014", month1="01", day1 = "01", year2 = year, month2 = "12", day2 = "31", reslat = 5,
reslon = 5, cashed = TRUE, vesselType = "all", byVesselType = FALSE, paper = NULL, country = "all"){

	require(bigrquery)
	require(raster)
	require(maps)
	require(mapdata)
	
		
	time1 = paste(year,"-",month1,"-",day1," 00:00", sep = "")
	time2 = paste(year2,"-",month2,"-",day2," 23:59", sep = "")

	minlat = bbox[1]
	maxlat = bbox[2]
	minlon = bbox[3]
	maxlon = bbox[4]

	

if (cashed==FALSE){
		project <- "world-fishing-827" # put your project name here


		if (vesselType[1] == "all") {
			sql = paste("
			SELECT inferred_label_allyears 
			FROM [gfw_research.vessel_info_20170606] 
			WHERE on_fishing_list IS TRUE 
				AND offsetting IS NULL 
				AND inferred_label_allyears IS NOT NULL 
			GROUP BY inferred_label_allyears
			", sep = "")
			sql <- gsub("\n","",sql) 
			sql <- gsub("\t","",sql)
			gears <- query_exec(sql, project)
			gears = gears[!gears$inferred_label_allyears %in% c("seismic_vessel","tug","gear","cargo_or_tanker","reefer","passenger"),]
			vesselType = gears
		}
		 
		# specify the query
		# bin resolution is:
		
		if (paper=="Global") {  # it takes the neural network classified table
			sql = paste("
				SELECT SUM(hours) fishing_hours, floor(lat/",reslat,")*",reslat," lat_bin, floor(lon/",reslon,")*",reslon," lon_bin, 
				FROM [gfw_research.nn] 
				WHERE _partitiontime between TIMESTAMP('",time1,"') 
					AND TIMESTAMP('",time2,"') 
					AND lat >", minlat," 
					AND lon <",maxlon," 
					AND lat <",maxlat," 
					AND lon >",minlon," 
					AND mmsi in (
						SELECT mmsi 
						FROM [gfw_research.vessel_info_20170606] 
						WHERE on_fishing_list is TRUE 
							AND inferred_label_allyears IN ('",paste(vesselType, collapse = "','"),"') 
							AND offsetting is null group by mmsi) 
					AND nnet_score > .5 
					AND seg_id in (select seg_id from [gfw_research.good_segments]) 
				GROUP BY lat_bin, lon_bin", sep = "")

			sql = cleanSql(sql)
#-------------------------------------------------------------------------------------
# Overlap paper
#------------------------------------------------------------------------------------
		} else { 
			if (vesselType[1]=="all") { # else it takes the table for the pacific
				sql = paste("
				SELECT FLOOR(lat/",reslat,")*",reslat," lat_bin, FLOOR(lon/",reslon,")*",reslon," lon_bin, SUM(hours) fishing_hours 
				FROM [world-fishing-827:gfw_research_archived.FAO] 
				WHERE _PARTITIONTIME BETWEEN TIMESTAMP('",time1,"') 
					AND TIMESTAMP('",time2,"') 
					AND lat >", minlat,"
					AND lon <",maxlon," 
					AND lat <",maxlat," 
					AND lon >",minlon," 
					AND measure_new_score >.5 
				GROUP BY lat_bin, lon_bin;", sep = "")
				sql = cleanSql(sql)
	
			} else if (vesselType[1]=="allGroups") {
					sql <- paste("
					SELECT FLOOR(lat/",reslat,")*",reslat," lat_bin, FLOOR(lon/",reslon,")*",reslon," lon_bin, SUM(hours) fishing_hours, first_label 
					FROM (
						SELECT lat, lon, mmsi, hours 
						FROM [world-fishing-827:gfw_research_archived.FAO] 
						WHERE _PARTITIONTIME BETWEEN TIMESTAMP('",time1,"') 
							AND TIMESTAMP('",time2,"') 
							AND lat >", minlat,"
							AND lon <",maxlon," 
							AND lat <",maxlat," 
							AND lon >",minlon," 
							AND measure_new_score >.5) a 
					LEFT JOIN (
						SELECT mmsi, first_label 
						FROM [scratch_stanford.ne_pacific_labels_20171024]) b 
					ON a.mmsi = b.mmsi 
					GROUP BY lat_bin, lon_bin, first_label;", sep = "")
					sql = cleanSql(sql)
		
			} else { # only for a specific fishing gear given by vesselType
					# this now works for multiple gear types - the collapse argument allows multiple gear types to be entered
				 sql = paste("
				SELECT lat_bin,lon_bin,fishing_hours 
				FROM (
					SELECT FLOOR(lat/",reslat,")*",reslat," lat_bin,FLOOR(lon/",reslon,")*",reslon," lon_bin,  first_label,SUM(hours) fishing_hours 	
					FROM (
						SELECT lat, lon, mmsi,hours 
						FROM [world-fishing-827:gfw_research_archived.FAO] 
						WHERE _PARTITIONTIME BETWEEN TIMESTAMP('",time1,"') 
							AND TIMESTAMP('",time2,"') 
							AND lat > ", minlat," 
							AND lon <",maxlon," 
							AND lat <",maxlat," 
							AND lon >",minlon," 
							AND measure_new_score >.5) a 
						LEFT JOIN (
							SELECT mmsi,first_label 
							FROM [scratch_stanford.ne_pacific_labels_20171026_onlyfishing]) b 
						ON  a.mmsi = b.mmsi 
						GROUP BY lat_bin,lon_bin, first_label)
						WHERE first_label in ('",paste(vesselType, collapse = "','"),"')", sep = "")
				
				sql = cleanSql(sql)	
				
				if (country != "all") {
				
				sql = paste("
				SELECT lat_bin,lon_bin,fishing_hours, flag_country_name 
				FROM (
					SELECT FLOOR(lat/",reslat,")*",reslat," lat_bin,FLOOR(lon/",reslon,")*",reslon," lon_bin,  first_label,SUM(hours) fishing_hours, flag_country_name 	
					FROM (
						SELECT lat, lon, mmsi,hours 
						FROM [world-fishing-827:gfw_research_archived.FAO] 
						WHERE _PARTITIONTIME BETWEEN TIMESTAMP('",time1,"') 
							AND TIMESTAMP('",time2,"') 
							AND lat > ", minlat," 
							AND lon <",maxlon," 
							AND lat <",maxlat," 
							AND lon >",minlon," 
							AND measure_new_score >.5) a 
						LEFT JOIN (
							SELECT mmsi,first_label, country_name as flag_country_name 
							FROM [scratch_stanford.ne_pacific_labels_20171026_onlyfishing]) b 
						ON  a.mmsi = b.mmsi 
						GROUP BY lat_bin,lon_bin, first_label, flag_country_name) 
						WHERE first_label in ('",paste(vesselType, collapse = "','"),"') 
							AND flag_country_name in ('",paste(country, collapse = "','"),"')", sep = "")
							
				
				sql = cleanSql(sql)	
				
					
				
				}
				
							  
			}
		}
		
		
# other variation we could use is this one that can returns a table of 4 column with the 4 column is gear type

	dat <- query_exec(sql, project)
	write.csv(dat, paste("../data/fishingHours",substr(time1,1,10),"_",substr(time2,1,10),"_",reslat,"x",reslon,paste(vesselType,collapse = "_",sep=""),".csv",sep = ""), row.names = F)
		
} else {
		
	dat = read.csv(paste("../data/fishingHours",substr(time1,1,10),"_",substr(time2,1,10),"_",reslat,"x",reslon,paste(vesselType,collapse = "_",sep=""),".csv",sep = ""))

}
	

dat
}