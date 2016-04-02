\name{sdmData}

\alias{sdmData}
\alias{sdmData,ANY,data.frame,missing-method}
\alias{sdmData,ANY, formula,missing-method}
\alias{sdmData,ANY,data.frame,missing,missing-method}
\alias{sdmData,ANY,SpatialPoints,Raster-method}
\alias{sdmData,ANY,SpatialPointsDataFrame,ANY-method}
\alias{sdmData,ANY,SpatialPointsDataFrame,Raster-method}

\title{creating sdm Data object}

\description{
Creates sdmData objects including species (single or multiple) and explanatory variates
}

\usage{
sdmData(formula,train, test,predictors,bg,filename, crs,...)
}

\arguments{
  \item{formula}{optional, a user can specify which species and explanatory variables should be taken from the data, the type of variables (e.g., factor) as well as other information including coordinates, time, grouping, info, etc. can be specified}
  \item{train}{training data including species observations as a (\code{data.frame} or \code{SpatialPoints} or \code{SpatialPointsDataFrames})}
  \item{predictors}{ explanatory variables (predictors), defined as a raster object (\code{RasterStack} or \code{RasterBrick}).}
  
  \item{test}{independent test data}
  \item{bg}{background data, can be a list containing the setting to generate background data (if a Raster object is provided in predictors), or a data.frame incuding background records}
  \item{filename}{filename of the sdm data object to store in the disk (not implemented) }
  \item{crs}{optional, coordinate reference system}
  }
\details{
sdmData can be used to create a data object, for single or multiple species.
sdmData can automatically detect the variables contain species data (if train is data.frame), but in formula, the species names can be specifies in the left hand side. In the right hand side of the formula the coordinate columns (if train is data.frame), explanatory variables that should be used as well as the factors can be specified (see examples).

}

\value{
 an object of class \code{sdmdata}
}

\references{
Naimi, B., Araújo, M.B. (2016) sdm: a reproducible and extensible R platform for species distribution modelling, Ecography, DOI: 10.1111/ecog.01881
}

\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://biogeoinformatics.org}
}

\seealso{ #}

\examples{
\dontrun{
# Example 1: a data.frame containing records for a species (sp) and two predictors (b15 & NDVI):
file <- system.file("external/pa_df.csv", package="sdm")
df <- read.csv(file)
head(df) 

d <- sdmData(sp~b15+NDVI,train=df)

d

# or simply:
d <- sdmData(sp~b15+NDVI,train=df)
d
#--------
# if formula is not specified, function tries to detect species and covariates
# it works well if dataset contains no additional columns but species and covariates

d <- sdmData(train=df)

d

# # only right hand side of the formula is specified (one covariate), so function detects species:
d <- sdmData(~NDVI,train=df) 

d 

#----------
###########
# Example 2: a data.frame containing presence-absence records for 1 species, 4 covariates, and x, y coordinates:
file <- system.file("external/pa_df_with_xy.csv", package="sdm")
df <- read.csv(file)
head(df) 

d <- sdmData(sp~b15+NDVI+categoric1+categoric2+coords(x+y),train=df) 

d
#----
# categoric1 and categoric2 are categorical variables (factors),
# if not sure the data.frame has them as factor, it can be specified in the formula:
d <- sdmData(sp~b15+NDVI+f(categoric1)+f(categoric2)+coords(x+y),train=df) 

d
# more simple forms of the formula:
d <- sdmData(sp~.+coords(x+y),train=df) 

d

d <- sdmData(~.+coords(x+y),train=df)  # function detects the species

d
##############
# Example 3: a data.frame containing presence-absence records for 10 species:

file <- system.file("external/multi_pa_df.csv", package="sdm")
df <- read.csv(file)
head(df) 

# in the following formula, the coordinates columns are specified and the rest is detected by the function:
d <- sdmData(~.+coords(x+y),train=df)  

d

#--- or it can be customized wich species and which covariates are needed:
d <- sdmData(sp1+sp2+sp3~b15+NDVI+f(categoric1) + coords(x+y),train=df) 

d # 3 species, 3 covariates, and coordinates
# just be careful that if  you put . in the right hand side while not all species column (or coordinates) are
# specified in the formula, then it takes those columns as covariates which is NOT right!

#########
# Example 4: Spatial data:

file <- system.file("external/pa_spatial_points.shp", package="sdm") # path to a shapefile
# use a package like rgdal, or maptools, or shapefile function in package raster to read shapefile:
p <- shapefile(file)
class(p) # a "SpatialPointsDataFrame"

plot(p)

head(p) # it contains data for 3 species

# presence-absence plot for the first species (i.e., sp1)
plot(p[p@data$sp1 == 1,],col='blue',pch=16, main='Presence-Absence for sp1')
points(p[p@data$sp1 == 0,],col='red',pch=16)

# Let's read raster dataset containing predictor variables for this study area:

file <- system.file("external/predictors.grd", package="sdm") # path to a raster object

r <- brick(file)

r # a RasterBrick object including 2 rasters (covariates)

plot(r)

# now, we can use the species points and predictor rasters in sdmData function:
d <- sdmData(sp1+sp2+sp3~b15+NDVI,train=p,predictors = r)

d

##################
# Example 5: presence-only records:


file <- system.file("external/po_spatial_points.shp", package="sdm") # path to a shapefile
# use a package like rgdal, or maptools, or shapefile function in package raster to read shapefile:
po <- shapefile(file)
class(po) # a "SpatialPointsDataFrame"


head(po) # it contains data for one species (sp4) and the column has only presence records!


d <- sdmData(sp4~b15+NDVI,train=po,predictors = r)

d # as you see in the type, the data is Presence-Only

### we can add another argument (i.e., bg) to ask the function to generate background (pseudo-absence) records:
# in bg, we are going to provide a list containing the setting to generate background
#------ the setting includes n (number of background records), method (the method used for background generation; gRandom refers to random in geographic space), 
#-------and remove (whether points located in presence sites should be removed)
d <- sdmData(sp4~b15+NDVI,train=po,predictors = r,bg=list(n=1000,method='gRandom',remove=TRUE))

d # as you see in the type, the data is Presence-Background

# you can alternatively, put a data.frame including background records in bg!
}
}

\keyword{spatial,species,data}
