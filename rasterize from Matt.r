	###############################################
	## convert SA1 counts to grid cells
	
	## load packages
	
library(haven)
library(rgdal)
library(raster)
library(broom)
library(viridis)
library(INLA)
library(spdep)
library(maptools)
library(ggplot2)
library(SpatialEpi)
library(egg)
library(doParallel)

setwd("U:/Tuson Project Folder/2019/MHED_raster")
dir.create("inla_graphFiles")	

	## load shapefile

shp_orig <- readOGR(
	"P:/Tuson Project Folder/2018/constructPolys/Data/SHP",
	"SA1_2011_AUST")
	
my_drive <- "R"
arearefx <- "metro_NO_mandurah"
yr <- 2016
shp_sub <- readOGR(paste0(my_drive,":/",arearefx,"/SHP"),paste0("shp_",yr))

shp_orig_sa1 <- subset(shp_orig,SA1_7DIG11 %in% shp_sub$SA1_7DIG11)
shp_orig_sa1_df <- tidy(shp_orig_sa1,region = "SA1_7DIG11")

shp_orig_outline <- unionSpatialPolygons(shp_orig_sa1,IDs = rep(1,length(shp_orig_sa1)))
shp_orig_outline_df <- tidy(shp_orig_outline)

	## load outcome data
	
datdir <- "W:/Volume A (copy)/Folder 10 - 201506.004 Modelling Predictors of Demand 30082018/SAS files"
emer <- data.frame(read_sas(paste0(datdir,"/emer.sas7bdat"),cols_only = c("diag","mdc","pres_date","sa111","rootnum")))
colnames(emer) <- tolower(colnames(emer))
nrow(emer); nrow(emer <- subset(emer,substr(diag,1,1) %in% c("F") | mdc %in% 19:20))
emer$sa1_7dig11 <- as.numeric(substr(emer$sa111,7,100)) + 5100000
nrow(emer); nrow(emer <- subset(emer,sa1_7dig11 %in% shp_orig_sa1$SA1_7DIG11))
emer <- subset(emer,select = -c(sa111,diag))
emer$mhed <- 1
emer$year <- as.numeric(substr(emer$pres_date,1,4))
nrow(emer); nrow(emer <- subset(emer,year == 2016))
nrow(emer); nrow(emer <- aggregate(mhed ~ year + sa1_7dig11,emer,sum))

	## load population data
	
datdir_pop <- "K:/Mei Ruu Archive/2012-27 Total Population Projections/Population file - Sep 2018/2011 geographical boundaries/by calendar year"
pops_full <- data.frame(read_sas(paste0(datdir_pop,"/sa1totalpop.sas7bdat")))
colnames(pops_full) <- tolower(colnames(pops_full))
nrow(pops_full); nrow(pops_full <- subset(pops_full,sa1_7dig11 %in% shp_orig_sa1$SA1_7DIG11))
nrow(pops_full); nrow(pops_full_agg <- aggregate(pop ~ year + sa1_7dig11,pops_full,sum))
nrow(pops_full_agg); nrow(pops_full_agg <- subset(pops_full_agg,year == 2016))

	## merge population and outcome data
	## final data consists of population & MH ED count for each SA1
	
dat <- merge(pops_full_agg,emer,by = c("year","sa1_7dig11"),all.x = TRUE)
dat$mhed[is.na(dat$mhed)] <- 0

	## convert SA1 data to xy coordinates (using SA1 centroids)
	
xy1 <- coordinates(shp_orig_sa1)
rownames(xy1) <- NULL
colnames(xy1) <- c("x","y")
xy1 <- lapply(1:nrow(xy1),function(rowid) {
	(temp.num <- dat[rowid,"mhed"])
	if (temp.num > 0) { 
		temp <- do.call(rbind,lapply(1:temp.num,function(inner) {
			xy1[rowid,]
		}))
	} else {
		temp <- NULL
	}
	return(temp)
})
xy1 <- xy1[!sapply(xy1,is.null)]
xy1 <- do.call(rbind,xy1)
nrow(xy1) == sum(dat$mhed)

xy2 <- coordinates(shp_orig_sa1)
rownames(xy2) <- NULL
colnames(xy2) <- c("x","y")
xy2 <- lapply(1:nrow(xy2),function(rowid) {
	temp.num <- dat[rowid,"pop"]
	if (temp.num > 0) { 	
		temp <- do.call(rbind,lapply(1:temp.num,function(inner) {
			xy2[rowid,]
		}))
	} else {
		temp <- NULL
	}
	return(temp)
})
xy2 <- xy2[!sapply(xy2,is.null)]
xy2 <- do.call(rbind,xy2)
nrow(xy2) == sum(dat$pop)

	## create raster grid

ext <- extent(shp_orig_sa1)
my_nrows <- 20
my_ncols <- 14

# r <- raster(shp_orig_sa1,ext = ext,ncols = my_ncols,nrows = my_nrows)
r <- raster(xmn = ext@xmin,xmx = ext@xmax,ymn = ext@ymin,ymx = ext@ymax,ncols = my_ncols,nrows = my_nrows)

rp <- as(r,"SpatialPolygons")

par(mfrow = c(1,1), mar = rep(0,4))
plot(shp_orig_outline,col = "beige")
# plot(shp_orig_sa1,col = "beige")
lines(rp, col="blue")

	## rasterize

d1 <- rasterize(x = xy1,y = r,fun = "count",background = 0,getCover = TRUE)
d2 <- rasterize(x = xy2,y = r,fun = "count",background = 0,getCover = TRUE)

par(mfrow=c(1,2),mar = rep(2,4))
plot(d1,axes = FALSE,legend = FALSE,box = FALSE)
plot(shp_orig_outline, add=TRUE)
plot(d2,axes = FALSE,legend = FALSE,box = FALSE)
plot(shp_orig_outline, add = TRUE)

	## create minimal grid cells

r_min <- raster::disaggregate(d1,fact = c(10,10))
plot(r_min)
# rp_min <- as(r_min,"SpatialPolygonsDataFrame")

	## restrict minimal grid cells according to the polygon extent
	
ext_min <- extract(r_min,shp_orig_outline,weights = TRUE,cellnumbers = TRUE)
(ext_cell <- ext_min[[1]][,"cell"])
(ext_cell_null <- (1:length(r_min))[-ext_cell])
	
r_min[ext_cell_null] <- NA
s_min <- crop(r_min,shp_orig_outline,snap = "out")
par(mfrow=c(1,1),mar = rep(1,4))
plot(s_min,axes = FALSE,legend = FALSE,box = FALSE)
sp_min <- as(s_min,"SpatialPolygons")
sp_min_outline <- unionSpatialPolygons(sp_min,IDs = rep(1,length(sp_min)))
lines(sp_min_outline,col = "blue")

	## restrict large grid cells according to the restricted subset of minimal cells
	## to do this create a "data point" for each minimal unit centroid
	## ...for minimal units within the polygon extent...
	
min_coords <- coordinates(s_min)
xy_minext <- min_coords[ext_min[[1]][,"cell"],]
rownames(xy_minext) <- NULL
colnames(xy_minext) <- c("x","y")

d_minext <- rasterize(x = xy_minext,y = r,fun = "count",background = 0,getCover = TRUE)

	## now restrict large grid to cells comprising some part of the polygon extent
	## we do this by referencing the extent of the cropped minimal grid
	
ext_d1 <- extract(d1,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
ext_d2 <- extract(d2,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
table(ext_d1[[1]][,"cell"] == ext_d2[[1]][,"cell"])

(ext_cell <- ext_d1[[1]][,"cell"])
(ext_cell_null <- (1:length(d1))[-ext_cell])
	
d1[ext_cell_null] <- NA
d2[ext_cell_null] <- NA
s_d1 <- crop(d1,sp_min_outline,snap = "out")
s_d2 <- crop(d2,sp_min_outline,snap = "out")

	## continue to plotting
	
par(mfrow=c(1,2),mar = rep(2,4))
plot(s_d1,axes = FALSE,legend = FALSE,box = FALSE)
lines(sp_min_outline,col = "blue")
# plot(shp_orig_outline, add=TRUE)
# lines(as(s_d1,"SpatialPolygons"),col = "blue")
plot(s_d2,axes = FALSE,legend = FALSE,box = FALSE)
lines(sp_min_outline,col = "blue")
# plot(shp_orig_outline, add = TRUE)
# lines(as(s_d2,"SpatialPolygons"),col = "blue")

s <- stack(s_d1, s_d2)
names(s) = c("mhed", "pop")
s <- crop(s,sp_min_outline,snap = "out")
par(mar = rep(2,4))
plot(s,addfun = function() lines(sp_min_outline),axes = FALSE,legend = FALSE,box = FALSE)

# p <- rasterToPoints(s)
# cell <- cellFromXY(s, p[,1:2])
# res <- data.frame(grid_id=cell, p[,3:4])

s_poly <- rasterToPolygons(s)
s_poly_df <- tidy(s_poly)
vals_df <- data.frame(
	id = sapply(s_poly@polygons,function(x) x@ID),
	mhed = s_poly@data[,"mhed"][!is.na(s_poly@data[,"mhed"])],
	pop = s_poly@data[,"pop"][!is.na(s_poly@data[,"pop"])])
vals_df$mhed[vals_df$pop == 0 & vals_df$mhed > 0] <- 0
vals_df$r <- vals_df$mhed / vals_df$pop
s_poly_df2 <- merge(s_poly_df,vals_df,by = "id")

kev_mhed <- ggplot() + 
	geom_polygon(
		data = s_poly_df2,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = mhed)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "MH ED\ncount",
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal")
		
kev_pop <- ggplot() + 
	geom_polygon(
		data = s_poly_df2,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = pop)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "Pop.",
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal",
		legend.key.size = unit(0.75,"cm"))
		
kev_rate <- ggplot() + 
	geom_polygon(
		data = s_poly_df2,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = r * 100000)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "Rate p/\n100K pop",
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal",
		legend.key.size = unit(0.75,"cm"))
		
grid.arrange(kev_mhed,kev_pop,kev_rate,ncol = 2)
		
	## calculate expected counts

population <- vals_df$pop
cases <- vals_df$mhed
(n.strata <- 1)
vals_df$E <- expected(population,cases,n.strata)
	
	## add small number if there are any zero-population polygons
		
vals_df$E[vals_df$E == 0] <- 1e-5
		
	## fit BYM model
	
s_poly_nb <- poly2nb(s_poly,queen = TRUE)
nb2INLA("inla_graphFiles/r",s_poly_nb)

g <- inla.read.graph(filename = "inla_graphFiles/r")

vals_df$id_num <- as.numeric(as.character(vals_df$id))
formula_bym <- mhed ~ 1 + f(id_num,model = "bym",graph = g,hyper = list(
	prec.unstruct = list(prior = "loggamma",param = list(0.001,0.001)),
	prec.spatial = list(prior = "loggamma",param = list(0.1,0.1))))

# mod_sa2_bym_eb1 <- inla(
	# formula_sa2_bym,family = "poisson",data = d_sa2,E = E,
	# control.compute = list(dic = TRUE,cpo = TRUE,mlik = TRUE),
	# control.inla = list(diagonal = diag.seq[counter],strategy = "gaussian",int.strategy = "eb"),
	# control.predictor = list(link = 1),
	# verbose = FALSE)
mod_bym <- inla(
	formula_bym,family = "poisson",data = vals_df,E = E,
	control.compute = list(dic = TRUE,cpo = TRUE,mlik = TRUE),
	# control.mode = list(result = mod_sa2_bym_eb1,restart = TRUE),
	control.predictor = list(link = 1),
	verbose = TRUE)

	## extract spatial residuals (zeta_i) and associated probability of exceedance 
	
vals_df$zeta_bym = unname(sapply(mod_bym$marginals.random[[1]][1:nrow(vals_df)],function(x) inla.emarginal(exp,x)))

a <- 0
vals_df$p_exc_zeta_bym <- unname(sapply(mod_bym$marginals.random[[1]][1:nrow(vals_df)],function(x) {
	1 - inla.pmarginal(a,x)
}))

	## identify hot spots according to a threshold
	
thr_exceed <- 0.95	
vals_df$sig <- factor(ifelse(vals_df$p_exc_zeta_bym >= thr_exceed,1,0),levels = 0:1)
levels(vals_df$sig) <- c("No","Yes")
table(vals_df$sig)
	
	## plot these values
	
s_poly_df3 <- merge(s_poly_df,vals_df,by = "id")

kev_zeta <- ggplot() + 
	geom_polygon(
		data = s_poly_df3,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = zeta_bym)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "zeta_i",
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal")
		
kev_p_exc <- ggplot() + 
	geom_polygon(
		data = s_poly_df3,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = p_exc_zeta_bym)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "P(exc.) for\nzeta_i",
		limits = c(0,1),
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal")

	## plot hot spots
	
kev_hs <- ggplot() + 
	geom_polygon(
		data = s_poly_df3,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = sig)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = paste0("Hot spot (using\np(exc.) >= ",thr_exceed,")"),
		discrete = TRUE,
		option = "inferno") +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal")
		
grid.arrange(kev_zeta,kev_p_exc,kev_hs,ncol = 2)

	## disaggregate large grid into minimal grid
	## NOTE: by default raster::disaggregate assigns each minimal unit the same value as the large cell in which it is comprised

vals_df$sig_num <- vals_df$sig
levels(vals_df$sig_num) <- 0:1
vals_df$sig_num <- as.numeric(as.character(vals_df$sig_num))	
	
s_fin <- s_d1
s_fin@data@values[!is.na(s_fin@data@values)] <- vals_df$sig_num
s_fin_min <- raster::disaggregate(s_fin,fact = c(10,10))

	## restrict to grid cells comprising some part of the polygon extent
	
ext_fin_min <- extract(s_fin_min,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
(ext_cell <- ext_fin_min[[1]][,"cell"])
(ext_cell_null <- (1:length(s_fin_min))[-ext_cell])
	
s_fin_min[ext_cell_null] <- NA
s_fin_min <- crop(s_fin_min,sp_min_outline,snap = "out")
sp_fin_min <- as(s_fin_min,"SpatialPolygons")
sp_fin_min_outline <- unionSpatialPolygons(sp_fin_min,IDs = rep(1,length(sp_fin_min)))

par(mfrow=c(1,1),mar = rep(2,4))
plot(s_fin_min,axes = FALSE,legend = FALSE,box = FALSE)
plot(shp_orig_outline, add=TRUE)
lines(sp_fin_min_outline,col = "blue")

sp_fin_min_df <- tidy(sp_fin_min)
vals_df_fin <- data.frame(
	id = sapply(sp_fin_min@polygons,function(x) x@ID),
	sig = s_fin_min@data@values[!is.na(s_fin_min@data@values)])
sp_fin_min_df2 <- merge(sp_fin_min_df,vals_df_fin,by = "id")
sp_fin_min_df2$sig <- factor(sp_fin_min_df2$sig)
levels(sp_fin_min_df2$sig) <- c("No","Yes")
table(sp_fin_min_df2$sig)

kev_fin <- ggplot() + 
	geom_polygon(
		data = sp_fin_min_df2,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = sig)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "Hot spot",
		discrete = TRUE) +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal") + 
		
	geom_path(
		data = shp_orig_outline_df,
		aes(
			x = long,y = lat,group = group))



	##################################
	## now shift grid and repeat process for each new shift

	## first calculate basic cell widths (in lat/long for now)
	
(x_cellw <- (ext@xmax - ext@xmin) / my_ncols)
(y_cellw <- (ext@ymax - ext@ymin) / my_nrows)

	## define target number of shifts

my_numsteps <- 10
(x_step <- x_cellw / my_numsteps)
(y_step <- y_cellw / my_numsteps)

	## loop through possible shifts (x-direction: i.e. longitude)
	
registerDoParallel(clus <- makeCluster(my_numsteps - 1))
clusterExport(clus,c(
	"my_numsteps","my_ncols","my_nrows","y_step","x_step",
	"ext",
	"thr_exceed",
	"shp_orig_outline",
	"sp_min_outline",
	"xy1","xy2"))
clusterEvalQ(clus,library(raster))
clusterEvalQ(clus,library(broom))
clusterEvalQ(clus,library(SpatialEpi))
clusterEvalQ(clus,library(spdep))
clusterEvalQ(clus,library(INLA))
clusterEvalQ(clus,library(maptools))
	
ii_list <- parLapply(clus,0:(my_numsteps - 1),function(ii) {

		## loop through possible shifts (y-direction: i.e. latitude)

	jj_list <- list()
	
	for (jj in 0:(my_numsteps -1)) {
	
		print(jj); flush.console()

		if (ii == 0 & jj == 0) {
		
			next
			
		}
		
		my_ncols_new <- ifelse(ii > 0,my_ncols + 1,my_ncols)
		my_nrows_new <- ifelse(jj > 0,my_nrows + 1,my_nrows)
		
		r_shift <- raster(
			xmn = ifelse(ii > 0,ext@xmin + (-(my_numsteps - ii) * x_step),ext@xmin),
			xmx = ifelse(ii > 0,ext@xmax + (ii * x_step),ext@xmax),
			ymn = ifelse(jj > 0,ext@ymin + (-(my_numsteps - jj) * y_step),ext@ymin),
			ymx = ifelse(jj > 0,ext@ymax + (jj * y_step),ext@ymax),
			ncols = my_ncols_new,
			nrows = my_nrows_new)

		# par(mfrow=c(1,1),mar = rep(3,4))
		# plot(shp_orig_outline,col = "beige")
		# lines(rp,col = "blue")
		# rp_shift <- as(r_shift,"SpatialPolygons")
		# lines(rp_shift,col = "red",xpd = NA)
		
			## rasterize

		d1_shift <- rasterize(x = xy1,y = r_shift,fun = "count",background = 0,getCover = TRUE)
		d2_shift <- rasterize(x = xy2,y = r_shift,fun = "count",background = 0,getCover = TRUE)

		# par(mfrow=c(1,2),mar = rep(1,4))
		# plot(d1_shift,legend = FALSE,axes = FALSE,box = FALSE)
		# plot(shp_orig_outline, add=TRUE)
		# plot(d2_shift,legend = FALSE,axes = FALSE,box = FALSE)
		# plot(shp_orig_outline, add = TRUE)

			## restrict to grid cells comprising some part of the polygon extent
			## can do this via the large cells directly or via the minimal grid cells...
			
		ext_d1_shift <- extract(d1_shift,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
		ext_d2_shift <- extract(d2_shift,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
		table(ext_d1_shift[[1]][,"cell"] == ext_d2_shift[[1]][,"cell"])

		(ext_cell_shift <- ext_d1_shift[[1]][,"cell"])
		(ext_cell_shift_null <- (1:length(d1_shift))[-ext_cell_shift])
			
		d1_shift[ext_cell_shift_null] <- NA
		d2_shift[ext_cell_shift_null] <- NA
		s_d1_shift <- crop(d1_shift,sp_min_outline,snap = "out")
		s_d2_shift <- crop(d2_shift,sp_min_outline,snap = "out")

			## continue to plotting
			
		# par(mfrow=c(1,2),mar = rep(1,4))
		# plot(s_d1_shift,axes = FALSE,legend = FALSE,box = FALSE)
		# plot(shp_orig_outline, add=TRUE)
		# lines(as(s_d1_shift,"SpatialPolygons"),col = "blue")
		# plot(s_d2_shift,axes = FALSE,legend = FALSE,box = FALSE)
		# plot(shp_orig_outline, add = TRUE)
		# lines(as(s_d2_shift,"SpatialPolygons"),col = "blue")

		s_shift <- stack(s_d1_shift, s_d2_shift)
		names(s_shift) = c("mhed", "pop")
		s_shift <- crop(s_shift,sp_min_outline,snap = "out")
		# plot(s_shift, addfun=function()lines(shp_orig_outline))

		# p <- rasterToPoints(s)
		# cell <- cellFromXY(s, p[,1:2])
		# res <- data.frame(grid_id=cell, p[,3:4])

		s_shift_poly <- rasterToPolygons(s_shift)
		s_shift_poly_df <- tidy(s_shift_poly)
		vals_df_shift <- data.frame(
			id = sapply(s_shift_poly@polygons,function(x) x@ID),
			mhed = s_shift_poly@data[,"mhed"][!is.na(s_shift_poly@data[,"mhed"])],
			pop = s_shift_poly@data[,"pop"][!is.na(s_shift_poly@data[,"pop"])])
		vals_df_shift$mhed[vals_df_shift$pop == 0 & vals_df_shift$mhed > 0] <- 0
		vals_df_shift$r <- vals_df_shift$mhed / vals_df_shift$pop
		
			## calculate expected counts

		population <- vals_df_shift$pop
		cases <- vals_df_shift$mhed
		(n.strata <- 1)
		vals_df_shift$E <- expected(population,cases,n.strata)
			
			## add small number if there are any zero-population polygons
				
		vals_df_shift$E[vals_df_shift$E == 0] <- 1e-5
				
			## fit BYM model
			
		s_shift_poly_nb <- poly2nb(s_shift_poly,queen = TRUE)
		nb2INLA(paste0("inla_graphFiles/r_",ii,"_",jj),s_shift_poly_nb)

		g_shift <- inla.read.graph(filename = paste0("inla_graphFiles/r_",ii,"_",jj))

		vals_df_shift$id_num <- as.numeric(as.character(vals_df_shift$id))
		formula_bym_shift <- mhed ~ 1 + f(id_num,model = "bym",graph = g_shift,hyper = list(
			prec.unstruct = list(prior = "loggamma",param = list(0.001,0.001)),
			prec.spatial = list(prior = "loggamma",param = list(0.1,0.1))))

		mod_bym_shift <- inla(
			formula_bym_shift,family = "poisson",data = vals_df_shift,E = E,
			control.compute = list(dic = TRUE,cpo = TRUE,mlik = TRUE),
			control.predictor = list(link = 1),
			verbose = FALSE)

			## extract spatial residuals (zeta_i) and associated probability of exceedance 
			
		vals_df_shift$zeta_bym = unname(sapply(mod_bym_shift$marginals.random[[1]][1:nrow(vals_df_shift)],function(x) inla.emarginal(exp,x)))

		a <- 0
		vals_df_shift$p_exc_zeta_bym <- unname(sapply(mod_bym_shift$marginals.random[[1]][1:nrow(vals_df_shift)],function(x) {
			1 - inla.pmarginal(a,x)
		}))

			## identify hot spots according to a threshold
			
		vals_df_shift$sig <- factor(ifelse(vals_df_shift$p_exc_zeta_bym >= thr_exceed,1,0),levels = 0:1)
		levels(vals_df_shift$sig) <- c("No","Yes")
		table(vals_df_shift$sig)
			
			## plot these values
	
		s_shift_poly_df3 <- merge(s_shift_poly_df,vals_df_shift,by = "id")

		# kev_shift_zeta <- ggplot() + 
			# geom_polygon(
				# data = s_shift_poly_df3,
				# aes(
					# x = long,
					# y = lat,
					# group = group,
					# fill = zeta_bym)) + 
					
			# xlab("Longitude") + 
			# ylab("Latitude") +
					
			# scale_fill_viridis(
				# name = "zeta_i",
				# option = "inferno") +
				
			# theme(
				# legend.position = c(0.65,0.9),
				# legend.direction = "horizontal")
				
		# kev_shift_p_exc <- ggplot() + 
			# geom_polygon(
				# data = s_shift_poly_df3,
				# aes(
					# x = long,
					# y = lat,
					# group = group,
					# fill = p_exc_zeta_bym)) + 
					
			# xlab("Longitude") + 
			# ylab("Latitude") +
					
			# scale_fill_viridis(
				# name = "P(exc.) for\nzeta_i",
				# limits = c(0,1),
				# option = "inferno") +
				
			# theme(
				# legend.position = c(0.65,0.9),
				# legend.direction = "horizontal")

			# ## plot hot spots
			
		# kev_shift_hs <- ggplot() + 
			# geom_polygon(
				# data = s_shift_poly_df3,
				# aes(
					# x = long,
					# y = lat,
					# group = group,
					# fill = sig)) + 
					
			# xlab("Longitude") + 
			# ylab("Latitude") +
					
			# scale_fill_viridis(
				# name = paste0("Hot spot (using\np(exc.) >= ",thr_exceed,")"),
				# discrete = TRUE,
				# option = "inferno") +
				
			# theme(
				# legend.position = c(0.65,0.9),
				# legend.direction = "horizontal")
				
		# grid.arrange(kev_shift_zeta,kev_shift_p_exc,kev_shift_hs,ncol = 2)
		
		# grid.arrange(kev_hs,kev_shift_hs,ncol = 2)
		
			## now update hot spot count for the minimal grid cells...

			## disaggregate large grid into minimal grid
			## NOTE: by default raster::disaggregate assigns each minimal unit the same value as the large cell in which it is comprised

		vals_df_shift$sig_num <- vals_df_shift$sig
		levels(vals_df_shift$sig_num) <- 0:1
		vals_df_shift$sig_num <- as.numeric(as.character(vals_df_shift$sig_num))	
			
		s_fin_shift <- s_d1_shift
		s_fin_shift@data@values[!is.na(s_fin_shift@data@values)] <- vals_df_shift$sig_num
		s_fin_shift_min <- raster::disaggregate(s_fin_shift,fact = c(10,10))

			## restrict to grid cells comprising some part of the polygon extent
			
		ext_fin_min_shift <- extract(s_fin_shift_min,sp_min_outline,weights = TRUE,cellnumbers = TRUE)
		(ext_cell_shift <- ext_fin_min_shift[[1]][,"cell"])
		(ext_cell_shift_null <- (1:length(s_fin_shift_min))[-ext_cell_shift])
			
		s_fin_shift_min[ext_cell_shift_null] <- NA
		s_fin_shift_min <- crop(s_fin_shift_min,sp_min_outline,snap = "out")
		sp_shift_fin_min <- as(s_fin_shift_min,"SpatialPolygons")
		sp_shift_fin_min_outline <- unionSpatialPolygons(sp_shift_fin_min,IDs = rep(1,length(sp_shift_fin_min)))

		# par(mfrow=c(1,1),mar = rep(2,4))
		# plot(s_fin_shift_min,axes = FALSE,legend = FALSE,box = FALSE)
		# plot(shp_orig_outline, add=TRUE)
		# lines(sp_shift_fin_min_outline,col = "blue")

		# sp_shift_fin_min_df <- tidy(sp_shift_fin_min)
		vals_df_shift_fin <- data.frame(
			id = sapply(sp_shift_fin_min@polygons,function(x) x@ID),
			sig = s_fin_shift_min@data@values[!is.na(s_fin_shift_min@data@values)])
		# sp_shift_fin_min_df2 <- merge(sp_shift_fin_min_df,vals_df_shift_fin,by = "id")
		# sp_shift_fin_min_df2$sig <- factor(sp_shift_fin_min_df2$sig)
		# levels(sp_shift_fin_min_df2$sig) <- c("No","Yes")
		# table(sp_shift_fin_min_df2$sig)

		# kev_fin_shift <- ggplot() + 
			# geom_polygon(
				# data = sp_shift_fin_min_df2,
				# aes(
					# x = long,
					# y = lat,
					# group = group,
					# fill = sig)) + 
					
			# xlab("Longitude") + 
			# ylab("Latitude") +
					
			# scale_fill_viridis(
				# name = "Hot spot",
				# discrete = TRUE) +
				
			# theme(
				# legend.position = c(0.65,0.9),
				# legend.direction = "horizontal") + 
				
			# geom_path(
				# data = shp_orig_outline_df,
				# aes(
					# x = long,y = lat,group = group))
				
		# grid.arrange(
			# kev_fin + geom_path(data = shp_orig_outline_df,aes(x = long,y = lat,group = group),colour = "black"),
			# kev_fin_shift + geom_path(data = shp_orig_outline_df,aes(x = long,y = lat,group = group),colour = "black"),
			# ncol = 2)
			
			## save
			
		jj_list[[match(jj,0:(my_numsteps - 1))]] <- vals_df_shift_fin
				
	}
	
		## combine
	
	jj_list <- jj_list[!sapply(jj_list,is.null)]
	jj_list2 <- lapply(1:length(jj_list),function(index) {
		temp <- jj_list[[index]]
		colnames(temp)[colnames(temp) == "sig"] <- paste0("sig_",index)
		return(temp)
	})
	jj_list2 <- Reduce(merge,jj_list2)
	jj_list2$sig_tot <- rowSums(jj_list2[,grepl("sig",colnames(jj_list2))])
	
		## save
		
	return(jj_list2)
	
})

stopCluster(clus)

ii_list2 <- lapply(1:length(ii_list),function(index) {
	temp <- ii_list[[index]]
	colnames(temp)[colnames(temp) == "sig_tot"] <- paste0("sig_tot_",index)
	return(temp[,c("id",paste0("sig_tot_",index))])
})
ii_list2 <- Reduce(merge,ii_list2)
# ii_list2$sig_tot <- rowSums(ii_list2[,paste0("sig_tot_",1:my_numsteps)])

	## finally combine to get a hot spot count...
	
vals_df_fin <- vals_df_fin[order(as.numeric(as.character(vals_df_fin$id))),]
ii_list2 <- ii_list2[order(as.numeric(as.character(ii_list2$id))),]

dat_fin <- merge(vals_df_fin,ii_list2,by = "id")
dat_fin$sig_tot <- rowSums(dat_fin[,grepl("sig",colnames(dat_fin))])

sp_fin_min_df <- tidy(sp_fin_min)	
sp_fin_min_df2 <- merge(sp_fin_min_df,dat_fin,by = "id")
	
kev_overlay <- ggplot() + 
	geom_polygon(
		data = sp_fin_min_df2,
		aes(
			x = long,
			y = lat,
			group = group,
			fill = sig_tot)) + 
			
	xlab("Longitude") + 
	ylab("Latitude") +
			
	scale_fill_viridis(
		name = "Hot spot\ncount",
		limits = c(0,100)) +
		
	theme(
		legend.position = c(0.65,0.9),
		legend.direction = "horizontal") + 
		
	geom_path(
		data = shp_orig_outline_df,
		aes(
			x = long,y = lat,group = group))