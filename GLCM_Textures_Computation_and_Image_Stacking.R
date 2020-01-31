
#---------------------------------------
# Free Memory in R/RStudio: Clearing the panels
# 1.Clear plots
if(!is.null(dev.list())) dev.off() # equivalent to clear all plots in the plots panel


# 2.Cean workspace
rm(list=ls()) # equivalent to button 'clear objects from workspace' in the evironment panel

# 3.Clear console :
cat("\014")  # equivalent to Ctrl+L or clear console from the Edit menu

#memory.limit() # to know the storage capacity of r version
#memory.limit(size=56000) # increase r storage capacity to 7GB, doesn't work
#---------------------------------------


# Load required packages or libraries

library(sp)
library(raster)
library(caret)
library(rgdal)
library(glcm)

#------------------------------------------------
# Batch process GLCM texture measures for 16uint images
# Compute the glcm using 5 by 5 window, aggregate direction and 1pixel displacement


# Set working directory (where images images to be processed)

setwd ('C:/....../Bak_8uintImages_16pixeldepth')

# Use sample image to extract extent and extension for processing images
a = raster('Bak_20150606_VH_4bit.tif') 
a
#extent = c(1402550, 1416150, 505910.5, 516350.5) # specify the extent of image
e = extent(a)

#Create output directory
outpath1 = "C:/..../GLCM_Texture_Images_4uint/"
dir.create(outpath1)


# read and create list of files containing Bak_...._6bit.tif
files1 = list.files(pattern = "Bak_.*._4bit.tif$")

# add output directory
outfiles = paste0(outpath1, files1)

# Change extensions of output files
extension(outfiles) ="tif"

# Create filename to rename image bands to glcm textures
#tempo = c("contrast", "entropy", "variance", "correlation")
#filename = paste0("new", tempo, ".tif")

# Loop through image (file) list, compute glcm for each and write image to output

for(i in 1:length(files1)){
  img = raster(files1[i])
  img_c = crop(img, e)
  img_c_T = glcm(img_c, n_grey = 16, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
                        statistics = 
                          c( "contrast", "entropy", "variance", "correlation"))
  
  img_c_T = writeRaster(img_c_T, outfiles[i], format='GTIff')
  #writeRaster(img_c_T, outfiles[i], filename=names(img_c_T), bylayer=TRUE, format="GTiff", overwrite=TRUE)
}

#------------------------------------------------
# Batch process GLCM texture measures for 64uint images

# Set working directory (where images images to be processed)

setwd ('C:/...../Bak_8uintImages_64pixeldepth')

# Use sample image to extract extent and extension for processing images
b = raster('Bak_20150606_VH_6bit.tif') 
b
e = extent(b)

#Create output directory
outpath2 = "C:/......./GLCM_Texture_Images_6uint/"
dir.create(outpath2)


# read and create list of files containing Bak_...._6bit.tif
files2 = list.files(pattern = "Bak_.*._6bit.tif$")

# add output directory
outfiles2 = paste0(outpath2, files2)

# Change extensions of output files
extension(outfiles2) = "tif"


# Loop through image (file) list, compute glcm for each and write image to output

for(i in 1:length(files2)){
  img = raster(files2[i]) #reading multiband image with "brick" function
  img_c = crop(img, e)
  img_c_T = glcm(img_c, n_grey = 64, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
                 statistics = 
                   c( "contrast", "entropy", "variance", "correlation"))
  
  img_c_T = writeRaster(img_c_T, outfiles2[i])
  #writeRaster(img_c_T, outfiles2[i], filename=names(img_c_T), bylayer=TRUE, format="GTiff")
}



#------------------------------------------------
#------------------------------------------------
# CREATING STACKS WITH 4 COMPUTED GLCM TEXTURES FOR EACH IMAGE

# A) Stacking 64GreyLevel GLCM Images

setwd('C:/......./GLCM_Texture_Images_6uintStack')


#create folder in current directory
outpath3 = "./GLCMstacks_Textures64/" 
dir.create(outpath3)

# create list of images in the wd with names Bak_...._6bit.tif
glcmstacks64 = list.files(pattern = "Bak_.*._6bit.tif$")

# add output directory
glcm64images = paste0(outpath3, glcmstacks64)

# Change extensions of output files
extension(glcm64images) = "GTiff"

# Creating a spatial object fro croping the raster to same extent

e = as(extent(1402550, 1416150, 505910.5, 516350.5), 'SpatialPolygons')
crs(e) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Loop through image (file) list, and rename the bands for each image 

for(i in 1:length(glcmstacks64)){
  img = brick(glcmstacks64[i])
  crs(img) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # change zone to Zone32
  img_c = crop(img, e) # crop image to extent of e file
  names(img_c) = c('contrast', 'entropy', 'variance', 'correlation')
  #img_stack = stack(img)
  img_c= writeRaster(img_c, glcm64images[i], format ='GTiff')
}



stacked64pd_1 = brick('Bak_20150606_VH_6bit.tif')

stacked64pd_1

# view number of bands
names(stacked64pd_1)
nlayers(stacked64pd_1)

crs(stacked64pd_1)

plot(stacked64pd_1[[1]])


# -----------------------------------------------------------

# Processing 4uint glcm images

setwd('C:/...../GLCM_Texture_Images_4uintStack')
rootdir16glcm1 = 'C:/....../GLCM_Texture_Images_4uintStack/GLCMstacks_IndTextures16'
#img_S = raster(file.path(rootdir64glcm1, 'Bak_20150606_VH_6bit.tif'))

#create folder in current directory
outpath4 = "./GLCMstacks_Textures16/" 
dir.create(outpath4)
outpath5 = "./GLCMstacks_IndTextures16/" 
dir.create(outpath5)


# create list of images in the wd with names Bak_...._6bit.tif
glcmstacks16 = list.files(pattern = "Bak_.*._4bit._glcm.tif$")
glcmstacksInd16 = list.files(pattern = "Bak_.*._4bit._glcm.tif$")

# add output directory
glcm16images = paste0(outpath4, glcmstacks16)

glcm16Indimages = paste0(outpath5, glcmstacksInd16)

# Change extensions of output files
extension(glcm16images) = "GTiff"
extension(glcm16Indimages) = "GTiff"

# Creating a spatial object fro croping the raster to same extent

e = as(extent(1402550, 1416150, 505910.5, 516350.5), 'SpatialPolygons')
crs(e) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Loop through image (file) list, and rename the bands for each image 

for(i in 1:length(glcmstacks16)){
  img = brick(glcmstacks16[i])
  crs(img) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # change zone to Z32
  img_c = crop(img, e) # crop image to extent of e file
  names(img_c) = c('contrast', 'entropy', 'variance', 'correlation')
  #img_stack = stack(img)
  img_c= writeRaster(img_c, glcm16images[i], format ='GTiff')
}


#----------------------------------------------------------
#----------------------------------------------------------

# C) EXTRACTING THE COMPUTED GLCM IMAGES, FOR EACH IMAGE, AS A SEPARATE IMAGE

# Ci) Processing and saving 4uint glcm images separately

setwd('C:/....../GLCM_Texture_Images_4uintStack')
imagedir = 'C:/....../GLCM_Texture_Images_4uintStack'
rootdir16glcm1 = 'C:/..../GLCM_Texture_Images_4uintStack/GLCMstacks_IndTextures16'

# Creating a spatial object fro croping the raster to same extent

e = as(extent(1402550, 1416150, 505910.5, 516350.5), 'SpatialPolygons')
crs(e) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


imagefiles = list.files(imagedir, pattern = "Bak_.*._4bit._glcm.tif$")

# unstacking and saving individual GLCM texture images 

bandnames = c('contrast', 'entropy', 'variance', 'correlation')


#img = stack('Bak_20150606_VH_4bit._glcm.tif')

for (i in 1:length(imagefiles)){
  img = stack(imagefiles[i])
  #bands = list(strsplit(names(img), '\\.')[[1]])
  basename = strsplit(names(img), '[.]')[[1]][1] #use strstrip function to split file name and get 1 item as basename
  #basename
  crs(img) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # change zone to Z32
  img_c = crop(img, e) # crop image to extent of e file
  s = unstack(img_c)
  
  for(i in seq_along(s)){
    writeRaster(s[[i]],  file.path(rootdir16glcm1, filename= paste0(basename,bandnames[i], ".tif")), format="GTiff" )
  }
}



'/
 
for(i in seq_along(s)){
  writeRaster(s[[i]],  file.path(rootdir16glcm1, file= bandnames[i]), format="GTiff" )
  }
      
for(i in seq_len(nlayers(img_c))){
  writeRaster(img_c[[i]],  file.path(rootdir16glcm1), file = bandnames[i], format="GTiff", overwrite=TRUE)}

/'

# Cii) Processing and separately saving 6uint glcm images 

setwd('C:/...../GLCM_Texture_Images_6uintStack')
imagedir = 'C:/...../GLCM_Texture_Images_6uintStack'
rootdir64glcm1 = 'C:/..../GLCM_Texture_Images_6uintStack/GLCMstacks_IndTextures64'

# Creating a spatial object fro croping the raster to same extent

e = as(extent(1402550, 1416150, 505910.5, 516350.5), 'SpatialPolygons')
crs(e) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


imagefiles = list.files(imagedir, pattern = "Bak_.*._6bit.tif$")

# unstacking and separately saving individual images 

bandnames = c('contrast', 'entropy', 'variance', 'correlation')


#img = stack('Bak_20150606_VH_4bit._glcm.tif')

for (i in 1:length(imagefiles)){
  img = stack(imagefiles[i])
  #bands = list(strsplit(names(img), '\\.')[[1]])
  basename = strsplit(names(img), '[.]')[[1]][1] #use strstrip function to split file name and get 1 item as basename
  #basename
  crs(img) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # change zone to Z32
  img_c = crop(img, e) # crop image to extent of e file
  s = unstack(img_c)
  
  for(i in seq_along(s)){
    writeRaster(s[[i]],  file.path(rootdir64glcm1, filename= paste0(basename,bandnames[i], ".tif")), format="GTiff" )
  }
}




# Cii) Processing and separately saving 8uint glcm images 
# C:/Users/Fred/Documents/SubsetSentinel1SAR/SAR_Results/SAR_Convert1/Bakoa_GLCM_Textures/SAR_Subset_8uint/Bakoa_S1A_10images/Bak_S1A_10img_stacks/Bak_S1A_10img_GLCM_8bits

setwd('C:/...../GLCM_Texture_Images_6uintStack')
imagedir = 'C:/...../GLCM_Texture_Images_6uintStack'
rootdir64glcm1 = 'C:/......./GLCM_Texture_Images_6uintStack/GLCMstacks_IndTextures64'

# Creating a spatial object fro croping the raster to same extent

e = as(extent(1402550, 1416150, 505910.5, 516350.5), 'SpatialPolygons')
crs(e) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


imagefiles = list.files(imagedir, pattern = "Bak_.*._6bit.tif$")

# unstacking and saving individual images separately

bandnames = c('contrast', 'entropy', 'variance', 'correlation')


#img = stack('Bak_20150606_VH_4bit._glcm.tif')

for (i in 1:length(imagefiles)){
  img = stack(imagefiles[i])
  #bands = list(strsplit(names(img), '\\.')[[1]])
  basename = strsplit(names(img), '[.]')[[1]][1] #use strstrip function to split file name and get 1 item as basename
  #basename
  crs(img) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # change zone to Z32
  img_c = crop(img, e) # crop image to extent of e file
  s = unstack(img_c)
  
  for(i in seq_along(s)){
    writeRaster(s[[i]],  file.path(rootdir64glcm1, filename= paste0(basename,bandnames[i], ".tif")), format="GTiff" )
  }
}




#-----------------------------------------------
#-----------------------------------------------

# D) CREATE STACK OF 80GLCM IMAGES, WITH SAME EXTENT, FOR GLCM TEXTURE IMAGES AT EACH PIXEL DEPTH (Grey Level Quantization)

#---------------------------------------
# Free Memory in R/RStudio: Clearing the panels
# 1.Clear plots
if(!is.null(dev.list())) dev.off() # equivalent to clear all plots in the plots panel


# 2.Cean workspace
rm(list=ls()) # equivalent to button 'clear objects from workspace' in the evironment panel

# 3.Clear console :
cat("\014")  # equivalent to Ctrl+L or clear console from the Edit menu

#memory.limit() # to know the storage capacity of r version
#memory.limit(size=56000) # increase r storage capacity to 7GB, doesn't work
#---------------------------------------


# Load required packages or libraries

library(sp)
library(raster)
library(caret)
library(rgdal)
library(glcm)

# Di) Stacking 4unit GLCM images

setwd('C:/......./GLCMstacks_IndTextures16')

rootdirglcmStacks = 'C:/....../GLCM_Textures_Image_Stacks'

#stacked64pd_1 = brick('Bak_20150606_VH_6bit.tif')
#img_S = extent(stacked64pd_1[[1]])
#crs(stacked64pd_1)
#stack_64glcm_C = crop(stack_64glcm, img_S) # crop stacked images to extent of img_S (sample image)

stack_16glcm = stack(list.files(path=getwd(), pattern = "Bak_.*.tif$"), quick=TRUE) # stack all images in folder

writeRaster(stack_16glcm, file.path(rootdirglcmStacks, filename= paste0('Bak20152017_4bitglcmStack.tif', names(stack_16glcm))),
            bylayer=TRUE, format="GTiff", overwrite=TRUE, options=c('TFW=YES'))

# consistency check on data
#install.packages("testthat")
library(raster)
library(testthat) #package to test data

b = raster(xmn=1402550, xmx=1416150, ymn=505910.5, ymx=516350.5, res=10)
crs(a) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

env_data = list.files(path=getwd(), pattern = "Bak_.*.tif$", full.names = TRUE)

r.env = list()
for(i in env_data){
  r = try(raster(i))
  r.env[i] = data.frame(name = i, nrow=nrow(r), ncol=ncol(r), res=res(r)[1],
                        proj=proj4string(r), xmin=extent(r)[1],
                        xmax=extent(r)[2], ymin=extent(r)[3],
                        ymax=extent(r)[4])
}

test = list()
for(i in 1:length(env_data)){
  test[[i]] = capture_warnings(compareRaster(b, env_data[[i]], res=T, orig=T, stopiffalse=F,showwarning = T))
}

test

# Dii) Stacking 6uint GLCM images

setwd('C:/......./GLCMstacks_IndTextures64')

rootdirglcmStacks = 'C:/...../GLCM_Textures_Image_Stacks'

stack_64glcm = stack(list.files(path=getwd(), pattern = "Bak_.*.tif$"), quick=TRUE) # stack all images in folder
stack_64glcm
plot(stack_64glcm[1])
writeRaster(stack_64glcm, file.path(rootdirglcmStacks, filename= paste0('Bak20152017_6bitglcmStack.tif')),
            format="GTiff", overwrite=TRUE)






#------------------------------------------------
#------------------------------------------------
#------------------------------------------------

# E) TRIALS CODES: WHILE CREATE IMAGE STACKS OF COMPUTED GLCM Texture IMAGES

setwd('C:/......../GLCM_Texture_Images_6uint')
rootdir64glcm1 = 'C:/......../Bak_8uintImages_64pixeldepth'
img_S = raster(file.path(rootdir64glcm1, 'Bak_20150606_VH_6bit.tif'))

outpath3 = "./GLCM_Textures64_extent/" #create folder in current directory
dir.create(outpath3)


# create list of images in the wd with names Bak_...._6bit.tif
glcms64 = list.files(pattern = "Bak_.*._6bit.tif$")

e64 = extent(img_S) # create extent for cropping other images

# add output directory
glcm64images = paste0(outpath3, glcms64)

# Change extensions of output files
extension(glcm64images) = "GTiff"


# Loop through image (file) list, and crop each image to have same extent

for(i in 1:length(glcms64)){
  img = raster(glcms64[i])
  img_c = crop(img, e64)
  img_c = writeRaster(img_c, glcm64images[i], overwrite=TRUE)
}


#------------------------------------------------
#------------------------------------------------

img1 = 'Bak_20150606_VH_4bit.tif'
img1_T = raster(img1) # import img using raster package

img1_T = crop(img1_T, extent) # crop image to desired extent
plot(img1_T)
img1_T
  
# Resetting the coordinate reference system (CRS)
# projection(img1_T) = "proj=utm +zone=32 +datum=WGS84"

''' 
texture_img1_T = glcm(img1_T, n_grey = 16, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
               statistics = 
       c( "contrast", "entropy", "variance", "correlation"), 
       min_x=NULL, max_x=NULL, na_opt="any", 
     na_val=NA, scale_factor=1, asinteger=FALSE)
'''

texture_img1_T = glcm(img1_T, n_grey = 16, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
               statistics = 
       c( "contrast", "entropy", "variance", "correlation"))

#texture_img1_Tb = glcm(img1_T, n_grey = 4, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
#                      statistics = 
#                        c( "contrast", "entropy", "variance", "correlation"))

names(texture_img1_T) # see names of calculated textures for img1_T
plot(texture_img1_T$glcm_contrast)
plot(texture_img1_T$glcm_entropy)
plot(texture_img1_T$glcm_variance)
plot(texture_img1_T$glcm_correlation)

hist(texture_img1_T$glcm_variance)
hist(texture_img1_T$glcm_correlation)


writeRaster(texture_img1_T, 'Bak_20150606_VH_4bit_glcm.tif',
            format="GTiff", overwrite=TRUE, options=c('TFW=YES', 'OVR=YES'))


#
#-----------------------------------------------------------
#GLCM for 64bits images (6uint.tif)

rootdir2 = 'C:/......./Bak_8uintImages_64pixeldepth'

img64_1 = raster(file.path(rootdir2, 'Bak_20150606_VH_6bit.tif'))
img64_1 
plot(img64_1)


text_img64_1 = glcm(img64_1, n_grey = 64, window = c(5, 5), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
                      statistics = 
                        c( "contrast", "entropy", "variance", "correlation"))

plot(text_img64_1$glcm_contrast)
plot(text_img64_1$glcm_entropy)
plot(text_img64_1$glcm_variance)
plot(text_img64_1$glcm_correlation)

hist(text_img64_1$glcm_contrast)
hist(text_img64_1$glcm_entropy)
hist(text_img64_1$glcm_variance)
hist(text_img64_1$glcm_correlation)


writeRaster(text_img64_1, file.path(rootdir2, 'Bak_20150606_VH_6bit_glcm.tif'),
            format="GTiff", overwrite=TRUE, options=c('TFW=YES'))




#---------------------------------------------------


# Creating a spatial object from a raster to same extent

# Load required packages or libraries

library(sp)
library(raster)
library(caret)
library(rgdal)
library(glcm)
library(dplyr)

setwd('C:/....../RESULTS/')

img = raster('C:/....../RESULTS/BakSAR_VVVH_6bitglcm_RFclassification_Prediction.tif')

RF_clf_extent = as(extent(img), 'SpatialPolygons')
crs(RF_clf_extent) = crs(img)
data = data.frame(f=99.9)

RF_clf_extentddf = SpatialPolygonsDataFrame(RF_clf_extent, data)
RF_clf_extentddf
writeOGR(RF_clf_extentddf, '.', "RF_clf_extent", driver = "ESRI Shapefile")









