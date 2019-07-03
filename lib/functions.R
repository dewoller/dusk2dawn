#!/usr/bin/env Rscript

source( "lib/functions_preload.R")
detach_all_packages()
library(tsibble)
#library(sp)

safe_load( 'workflowr')
#safe_load("seas")
safe_load("magrittr")
safe_load("stringr")
safe_load("knitr")
#safe_load("kableExtra")
#safe_load("pander")
safe_load("lubridate")

safe_load("readstata13" )
#safe_load("foreign" )
safe_load("wrapr" )   # for the qc function

#safe_load("ordinal" )
safe_load("DataCache" )
safe_load( 'fuzzyjoin')
library('IRanges')
safe_load("multidplyr")
safe_load("tidyverse")



keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))


destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}

save_data_single <- function( item ) {
	fwrite( eval(parse(text = item)), paste0(item, ".csv"))
}
save_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, save_data_single )
	} else {
		save_data_single( item ) 
	}
}
#
read_data_single <- function( item ) {
	 do.call("<<-",list(item, fread( paste0(item, ".csv")) %>% as.tibble()))
}
read_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, read_data_single )
	} else {
		read_data_single( item ) 
	}
}

# display a map for each lga in lga_categories, displaying only a colors corresponding to the display_factors
# show the category_name

#display_factors=c("Moderate", "High", "Very High")
#display_colors=c("green", "orange", "red")
#category_column = "lga_colorisation"
#df_lga_category=lga_colors

get_australia_state_map <- function( df_lga_category , 
						state_id = 1 , 
						category_column = "lga_colorisation" , 
						display_factors = levels( df_lga_category[, category_column ]), # default to labelling everything
						category_name  = category_column , 
						display_colors= brewer.pal( length( display_factors ), "RdBu")[1:length(display_factors)],
						label_factors = display_factors, # which factors to default to labelling everything that we display 
						label_lga = df_lga_category$lga  # which LGAS to display,  default to labelling everything that we display
						)
						{
	
	df_lga_category %<>% 
		mutate( lga_colorisation = get(category_column ) ) 
	df_lga_category %>% 
		filter(!lga_colorisation %in% display_factors) %>% 
		select(category_column) %>% 
		unique() %>% 
		count()  %>%
		{.} -> nwhite
	actual_colors=c( rep( "white", nwhite ), display_colors )
	df_lga_category %>% 
		filter(lga_colorisation %in% label_factors) %>%
		filter( lga %in% label_lga )  %>%
		select(lga) %>%
        {.} -> lga_to_label 

	area_extents <- get_state_geo_df(state_id)
    area <- readShapePoly("~/mydoc/research/mofi/shapefiles/LGA11aAust.shp") 
    tidy( area, region="LGA_CODE11") %>% 
        as.tibble() %>%
        rename( lga = id ) %>%
        mutate( lga=factor(lga)) %>%
        left_join( area@data,  by=c("lga"="LGA_CODE11")) %>%
        inner_join( df_lga_category, by="lga") %>%
        mutate( lga=factor(lga)) %>%
        {.} -> area.t

	area.t %>% 
		rename( lga_name = LGA_NAME11) %>%
		select(lga, lga_name, lga_colorisation ) %>%
		unique() %>%
		mutate( lga_name = 
					lga_name %>% 
					as.character() %>%
					substr(1, str_length(lga_name)-4) %>%
					tools::toTitleCase( ) %>%
					gsub("  *","\n", .)
				)  %>%
	  mutate(lga=as.character(lga)) %>%
		{.} -> lga_names

	area.t %>% 
		group_by(lga ) %>%
		summarize ( lat=mean(range(lat)), lon=mean(range(long))) %>%
		mutate(lga=as.character(lga)) %>%
		select(lga, lat, lon) %>%
		{.} -> lga_center
		
	lga_names %>%
		inner_join( lga_center, by="lga") %>%
		inner_join( lga_to_label, by="lga" ) %>%
		filter( is_geographic_LGA( lga )) %>%
		{.} ->  lga_names_to_display 
#	


	ggplot() +
		geom_polygon(aes_string(x = "long",
						y = "lat",
						group = "group",
						fill=category_column,
						color=category_column
		),
		color="black",
		data = area.t
		) +
		## Configure the colors, transparency and panel
		#scale_alpha(range = c(.25, .55)) +
		coord_quickmap( 
			xlim=c( area_extents$state_tl_lon + .1, area_extents$state_br_lon -.1 ) 
			, ylim=c( area_extents$state_tl_lat + .1, area_extents$state_br_lat - .1 ) 
		) + 
		theme_map() + 
		scale_fill_manual(breaks=display_factors, values= actual_colors , name = category_name )  %>%
		{.} -> map
	list( map=map
		 , repel_names = geom_label_repel(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
		 , regular_names = geom_label(data=lga_names_to_display , aes(lon, lat, label = lga_name, fill=lga_colorisation), color="black", size=3, fontface='bold')
		 )
}


# ------------------------------------------------------------------
get_australia_base_map = function(states) {
read_shape("~/mydoc/research/mofi/shapefiles/LGA11aAust.shp")  %>%
  subset( STATE_CODE %in% states )  %>% 
  simplify_shape(0.05)

}


# ------------------------------------------------------------------
#unused?
getBoundary <- function( state_id ) {
	
	area.t %>% 
	  filter(substr(lga,1,1)== state_id) %>%
		summarize( 
			center_long = min(long) + (max(long) - min(long))/2
			,center_lat = min(lat) + (max(lat) - min(lat))/2
			,max_lat = max(lat) 
			,min_lat = min(lat) 
			,max_long = max(long) 
			,min_long = min(long) 
			) %>%
	{.} -> area_extents
return(area_extents)
}

# ------------------------------------------------------------------
box_around_capital <- function(state_id, lat_offset = .1, lon_offset) {
#
	get_state_geo_df( state_id ) %>%
		select( starts_with("capital_"))  %$%
		data.frame( 
				    lat=c(capital_tl_lat + lat_offset,capital_tl_lat + lat_offset,capital_br_lat - lat_offset,capital_br_lat - lat_offset,capital_tl_lat + lat_offset)
					, lon=c(capital_tl_lon - lon_offset,capital_br_lon + lon_offset,capital_br_lon + lon_offset,capital_tl_lon - lon_offset, capital_tl_lon - lon_offset)) %>%
		geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}
# ------------------------------------------------------------------
box_around_annotation = function( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset, lon_offset )  {
#
		data.frame( 
				    lat=c(ymax_t_lat + lat_offset,ymax_t_lat + lat_offset,ymin_b_lat - lat_offset,ymin_b_lat - lat_offset,ymax_t_lat + lat_offset)
					, lon=c(xmin_l_lon - lon_offset,xmax_r_lon + lon_offset,xmax_r_lon + lon_offset,xmin_l_lon - lon_offset, xmin_l_lon - lon_offset)) %>%
		geom_path(data=.,  aes(x = lon, y = lat), size = 0.7, color="red") 
}
							
# ------------------------------------------------------------------
get_inset_capital_map <- function( map_list, state_id, inset_location="br" ) {

	ratio = 3 # inset map should be 1:ratio the size of big map

	geo =get_state_geo_df (state_id) 
    box_size_lon = abs(geo$state_br_lon - geo$state_tl_lon)/ ratio
    box_size_lat = abs(geo$state_br_lat - geo$state_tl_lat)/ ratio

	# multiply latitude by this ratio to get even longitude
	lat2lon_ratio = (geo$capital_tl_lat - geo$capital_br_lat) / (geo$capital_br_lon-geo$capital_tl_lon)
	lat_offset= .0001
	lon_offset = lat_offset / lat2lon_ratio

	if (inset_location == "br") {
		xmin_l_lon = geo$state_br_lon-box_size_lon + 1
		xmax_r_lon = geo$state_br_lon + 1
		ymin_b_lat = geo$state_br_lat
		ymax_t_lat = geo$state_br_lat+box_size_lat
		arrow=data.frame(
			arrow_from_lon = xmin_l_lon
			, arrow_from_lat = ymax_t_lat 
			, arrow_to_lon = geo$capital_br_lon
			, arrow_to_lat = geo$capital_br_lat
		)
	} else if (inset_location == "tc") {  # broken
		cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
		xmin_l_lon = cl_lon 
		xmax_r_lon = cl_lon + box_size_lon
		ymin_b_lat = geo$state_tl_lat 
		ymax_t_lat = geo$state_tl_lat + box_size_lat
		arrow=data.frame(
			arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
			, arrow_from_lat = ymin_b_lat 
			, arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
			, arrow_to_lat = geo$capital_tl_lat
		)
	} else if (inset_location == "bc") {  # broken
		cl_lon = mean(geo$state_tl_lon, geo$state_br_lon) + box_size_lon/2
		xmin_l_lon = cl_lon 
		xmax_r_lon = cl_lon + box_size_lon
		ymin_b_lat = geo$state_br_lat - box_size_lat
		ymax_t_lat = geo$state_br_lat 
		arrow=data.frame(
			arrow_from_lon = mean(xmin_l_lon, xmax_r_lon) 
			, arrow_from_lat = ymax_t_lat 
			, arrow_to_lon = mean( geo$capital_br_lon, geo$capital_tl_lon)
			, arrow_to_lat = geo$capital_br_lat
		)
	} else if (inset_location == "tr") {  
		xmin_l_lon = geo$state_br_lon - box_size_lon
		xmax_r_lon = geo$state_br_lon
		ymin_b_lat = geo$state_tl_lat - box_size_lat
		ymax_t_lat = geo$state_tl_lat
		arrow=data.frame(
			arrow_from_lon = xmin_l_lon 
			, arrow_from_lat = ymin_b_lat
			, arrow_to_lon = geo$capital_br_lon
			, arrow_to_lat = geo$capital_tl_lat
		)
	}
							

	map_list [[ 'map' ]] +
		map_list [[ 'repel_names' ]] +
		annotation_custom(grob = get_australia_capital_map( map_list [[ 'map' ]] + map_list [[ 'regular_names' ]] 
														   , state_id) %>% ggplotGrob()
							, xmax = xmax_r_lon
							, xmin = xmin_l_lon
							, ymax = ymax_t_lat  
							, ymin = ymin_b_lat
							)  +
		expand_limits( x=xmax_r_lon ) +
		expand_limits( x=xmin_l_lon ) +
		expand_limits( y=ymin_b_lat ) +
		expand_limits( y=ymax_t_lat ) +
		box_around_annotation( xmax_r_lon ,xmin_l_lon ,ymax_t_lat  ,ymin_b_lat, lat_offset/ratio, lon_offset/ratio ) +
		box_around_capital( state_id , lat_offset, lon_offset ) +
			geom_curve(data=arrow, 
					   	aes(x=arrow_from_lon,y=arrow_from_lat,xend=arrow_to_lon,yend=arrow_to_lat),
								curvature = 0.1, 
								arrow = arrow(type="closed",length = unit(0.25,"cm")), color="red") 
}

# get Australia Capital Map ------------------------------------------------------------------

get_australia_capital_map <- function( map, state_id ) {
	capital <- get_state_geo_df( state_id ) %>%
    select( starts_with("capital_"))
	 getZoomedMap( map, 
	              edge_lat <- capital %>% 
            	    select( ends_with("_lat")) %>%
	                unlist(),
            	  edge_lon <- capital %>% 
            	    select( ends_with("_lon")) %>%
	                unlist()
	) 
}
# get Zoomed ------------------------------------------------------------------

 
getZoomedMap <- function( map, edge_lat, edge_lon ) {
	map + 
		coord_quickmap( xlim=edge_lon, ylim=edge_lat) +
		#theme(panel.background = element_rect(fill = NULL)) +
		theme(legend.position = "none") +
		ggtitle("")
}

# theme_map------------------------------------------------------------------


theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}

# ------------------------------------------------------------------
testGetSeifaColors <- function (state_id=1) {
	# get me the LGA Seifa's that are in the sampledata
	dbGetQuery(con, paste(
		"
			SELECT DISTINCT lga, lga_name, value AS score
			FROM lga_full_2011 l
			JOIN seifa_2011 a USING (lga)
			WHERE a.measure_code='SCORE'
			AND a.index_code='IRSAD' 
			AND " , get_lga_restriction( state_id ) 
			)
		) %>% 
		mutate( lga_colorisation = ntile( score, 4)  %>%
		ordered(  levels=1:4, labels=c("Least", "Moderate", "High", "Very High")) ) %>%
		select(lga, lga_colorisation) 
}

# ------------------------------------------------------------------
# testGetMap ------------------------------------------------------------------


# ------------------------------------------------------------------
testGetMap  <- function( state_id=1 ) {
	testGetSeifaColors( state_id) %>%
		get_australia_state_map(  )
}

# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
	sapply( year, function(year) { year.length(as.character(year )) })
}


# seqNext ------------------------------------------------------------------
seqNext <- function(x1, y1) {
  dat <- data.frame( x= x1
  			 , y=y1) 
  unname(
    predict(lm(y ~ x, data=dat), newdata=list(x=c(2016)))
  )
}

# bothDiff  ------------------------------------------------------------------
bothDiff <- function ( set1, set2 ) {
	print(setdiff( set1, set2 ))
	print(setdiff(set2, set1))
}

# ------------------------------------------------------------------
slurpTable <- function( name ) {

	state_geo_query = paste( "
			SELECT * FROM ", name
			, sep=""
			)
	dbGetQuery(con, state_geo_query ) %>% 
		as.tibble() %>%
	return( . )
}

# -------------------------------------------------
get_state_code_restriction <- function( state_id) { 
#
state = c( "NSW" , "VIC" , "QLD" , "WA" , "SA" , "TAS" , "NT" , "ACT" , "UNK")
sapply( state_id, function( x )  {
                    ifelse( x == 1
                        , "'NSW', 'ACT'"
                        , paste(
                                "'"
                                , state[ as.numeric( x ) ]
                                , "'"
                                , sep=""
                                )) 
                  }
    )
}

# -------------------------------------------------
get_state_code_from_lga <- function( lga ) { 
#
state = c( "NSW" , "VIC" , "QLD" , "WA" , "SA" , "TAS" , "NT" , "ACT" , "UNK")
sapply( substr( lga, 1, 1) , function( x )  {
                    ifelse( x >= '1' & x <='999999999'
                            , state[ as.numeric( x ) ]
                            , "UNK"
                            )
                  }
    )
}


# -------------------------------------------------
get_significant_trend <- function( df, ddd_direction_labels)	{

	df  %>% 
		mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
		filter( supply_year != 2012 & supply_year != 2016 ) %>%
		group_by(lga, supply_year) %>% 
		summarise( no_doses=sum(no_doses)) %>%
		inner_join( 
			df_population %>%
				mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
				filter( supply_year != 2012 | supply_year != 2016 ) %>%
				group_by( lga, supply_year) %>%
				summarize( person_days = sum(population * my_year_length(supply_year)))
			, by=c("lga", "supply_year") 
		) %>%
		mutate( ddd = (no_doses * 1000) / (person_days)) %>%
		as_tibble() %>%
		select( lga, supply_year, ddd) %>%
		split(.$lga)   %>%
		map(~  # p value for each lga
			lm(ddd ~ as.integer(supply_year), data=.)   %>% 
			tidy() %>% 
			filter(term != "(Intercept)") %>% 
			select(estimate, p.value) 
		) %>%
		do.call("rbind", . ) %>%
		mutate( lga = as.factor(rownames(.))) %>%
		mutate( direction=ifelse( round(p.value, 2) >.05 , "None" , ifelse( estimate>0, "Rising", "Falling"))
			, direction=factor(direction, levels=ddd_direction_labels)) 
}
# -------------------------------------------------


singleFacetPlot_boxplot = function( df_raw_input, var1, var2 ) {

		gb = c("lga", var1, var2 )
		df_raw_input %>%
			group_by_at( vars(gb)) %>%
			summarise( no_users = n()) %>%
			ggplot() +
			geom_boxplot( mapping=aes_string(x = var1, y= "no_users" , color=var1, fill=var1)) + 
			ggtitle( paste("The range of LGA number of users for each", var1, "facetted by", var2)) +
			facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}

# test_multiplots  --------------------------------------------------------------
test_multiplots <- function () {

  variables = qw("gender age state year ")
  variables = qw("year state")
  user_status_df %>% 
    filter( continuing_user ) %>% 
    generate_multiplots( variables )
}

# multiplots  --------------------------------------------------------------

generate_multiplots <- function ( df_raw_input, variables = qw("gender age") , output="screen" ) {
		
	to_plot = combn( variables, 2 , simplify=FALSE)
	to_plot=c(to_plot, lapply(to_plot, rev))
	plot_list = list()

	for (i in 1:length(to_plot)) {
		cat( to_plot[[ i ]][1], to_plot[[ i ]][2], i, "\n")
		plot_list[[i]] <- singleFacetPlot_boxplot( df_raw_input, to_plot[[ i ]][1], to_plot[[ i ]][2] )
        if (output=="screen") {
          print(plot_list[[i]])
        } else if (endsWith(output, "tiff")) {
          file_name = paste("graphics/boxplot_", to_plot[[ i ]][1], "_", to_plot[[ i ]][2], i, ".tiff", sep="")
          tiff(file_name)
          print(plot_list[[i]])
          dev.off()
        }
	}

    if (endsWith(output, ".pdf")) {
      pdf(output)
      for (i in 1:length(plot_list)) {
          print(plot_list[[i]])
      }
      dev.off()
    }
    return(plot_list)

}

#-------------------------------------------------------------------------------------------

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#-------------------------------------------------------------------------------------------

urbanisation_rollup_single <- function( code, desc ) {
	rv = as.character(desc)
	if (substr(as.character( code ), 1, 2) %in% qw("RA RT UD UF UR")) {
			rv =  sub("^([^ ]* [^ ]*) .*","\\1", rv) 
	}
	rv = sub( "Agriculture", "Agricultural", rv )
	rv
}
#-------------------------------------------------------------------------------------------
urbanisation_rollup <- function(code, desc) {
	mapply( urbanisation_rollup_single, code, desc )
}

#-------------------------------------------------------------------------------------------
is_geographic_LGA  <- function( lga ) {
  !endsWith( as.character(lga), '99') && 
    !startsWith( as.character( lga), 'UNK') && 
    (lga != '.')
}

#-------------------------------------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

#-------------------------------------------------------------------------------------------
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

#-------------------------------------------------------------------------------------------
sourcep <- function(file){
  coms <- parse(file)
  for (i in seq_along(coms)){
    print(coms[[i]])
    eval(coms[[i]],envir=.GlobalEnv)
    mess <- paste("Expression",i,"of",length(coms),"parsed. Press <return> to continue.")
    cat(mess)
    readLines(n=1)
  }
}

#-----------------------------------------------------------------------------
age_grouping_mofi <- function( age) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
  cut( age, 
      breaks=c(-1,19,44,64, Inf),
      labels=c("0-19", "20-44", "45-64", "65+"),
      ordered_result=TRUE)
}


#-----------------------------------------------------------------------------
age_grouping <- function( age, n=25 ) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
      paste0(sprintf("%02d", 
                     floor(as.numeric(age)/n)*n), 
             "-", 
             sprintf( "%02d", 
                     (floor(as.numeric(age) / n) + 1 ) * n - 1)
             )
  }



#-----------------------------------------------------------------------------

rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))




#-----------------------------------------------------------------------------

find_pill_group = function( supply_date, difference, ndays, threshold  = .25) {
  id=1
  sum_difference = 0
  sum_days = 0
  df=data.frame( supply_date, difference, ndays)
  rv=c()
  for (A in rows(df)) {
    sum_difference = sum_difference + A$difference
    sum_days = sum_days + A$ndays
    rv <- c( rv, id )
    if (is.na(A$difference ) || is.na(A$ndays)  || ( (sum_days / sum_difference ) < threshold)) {
      id<- id+1
    }
  }  
  rv
}


df$lat=b$latitude
df$lon=b$longitude

kalman = function ( lat, lon ) {

  df = data.frame( lat=lat, lon= lon ) 
  #initializing variables
  count <- nrow(df) # amount of data points in the day
  z <- cbind(df$lon,df$lat) #measurements

  #Allocate space:
  xhat <- matrix(rep(0,2*count),ncol =2) #a posteri estimate at each step
  P <- array(0,dim=c(2,2,count))  #a posteri error estimate
  xhatminus <- matrix(rep(0,2*count),ncol =2) #a priori estimate
  Pminus <- array(0,dim=c(2,2,count)) #a priori error estimate
  K <- array(0,dim=c(2,2,count)) #gain

  #Initializing matrices
  A <-diag(2)
  H<-diag(2)
  R<-function(k) diag(2)* df$accuracy[k]^2#estimate of measurement variance
  Q<-function(k) diag(2)* as.numeric(df$timeNextMeasure[k])^1.5# the process variance

  #initialise guesses:
  xhat[1,] <- z[1,]
  P[,,1] <- diag(2)


  for (k in 2:count){
    #time update
    #project state ahead
    xhatminus[k,] <- A %*% xhat[k-1,] #+ B %*% u[k-1]

    #project error covariance ahead
    Pminus[,,k] <- A %*% P[,,k-1] %*%  t(A) + (Q(k))

    #measurement update
    # kalman gain
    K[,,k] <- Pminus[,,k] %*% t(H)/ (H %*% Pminus[,,k] %*% t(H) + R(k))

    #what if NaaN?
    K[,,k][which(is.nan(K[,,k]))]<-0

    # update estimate with measurement
    xhat[k,] <-  xhatminus[k,] + K[,,k] %*% (z[k,] - H %*% xhatminus[k,])
    #update error covariance
    P[,,k] = (diag(2) - K[,,k]%*% H) %*% Pminus[,,k]
  }

  P
}
