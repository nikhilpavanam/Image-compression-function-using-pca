# Group (Murugan - E20017, Nikhil - E20018)

#Question 5:

#Function to compress image and it in same folder


imagecompress = function(image_path,image_saveas,clarity)
{
  library(jpeg)
  image=readJPEG(image_path)
  
  r <- image[,,1]
  g <- image[,,2]
  b <- image[,,3]
  
  image.r.pca <- prcomp(r, center = FALSE)
  image.g.pca <- prcomp(g, center = FALSE)
  image.b.pca <- prcomp(b, center = FALSE)
  
  #Storing PCA objects into a list.
  rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)
  length(rgb.pca)
  
  
  # Selecting the ncomp value
  propvar_r=cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100
  propvar_b=cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100
  propvar_g=cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100
  
  count_r=0
  count_b=0
  count_g=0
  
  for (i in propvar_r) {
    if (i<=clarity){ 
      count_r <- count_r +1
    } else {
      break
    }
  }
  
  for (i in propvar_b) {
    if (i<=clarity){ 
      count_b <- count_b +1
    } else {
      break
    }
  }
  
  for (i in propvar_g) {
    if (i<=clarity){ 
      count_g <- count_g +1
    } else {
      break
    }
  }
  
  ncomp=max(count_b,count_g,count_r)
  #print(i," - ",ncomp)
  print(paste(image_path,ncomp,sep= "-"))
  
  #Reconstruct
  
  R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
  B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
  G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
  
  R = ifelse(R>1,1,R)
  G = ifelse(G>1,1,G)
  B = ifelse(B>1,1,B)
  
  R = ifelse(R<0,0,R)
  G = ifelse(G<0,0,G)
  B = ifelse(B<0,0,B)
  
  img=array(c(R,G,B),dim=c(dim(image)))
  
  fil=paste(getwd(),"/",image_saveas,sep= "")     # renaming the file
  writeJPEG(img,fil)                    # saving the compressed file
  
}

setwd("C:\\Users\\lenovo\\Documents\\Imagecompression")

imagecompress(image_path="C:\\Users\\lenovo\\Documents\\Imagecompression\\katia.jpg",
              image_saveas="katia_compr9925.jpg",clarity=99.25)

#=======================================================================================
# Function to compress images into different folder


#Step 1:

imagecompression = function(loc,clarity,del_org=F)
{
  default=getwd()
  
#Step 2: Extracting only the image files
  files=list.files(path = loc,all.files = T)
  img.files = files[grep(".jpg", files, fixed=T)]
  
  
  # getting the path of all the image files
  p=c()
  for(i in img.files)
  {
    path=paste(loc,"\\",i,sep= "")
    p=c(p,path)
  }

  # Step 3:
  count=0
  for (i in p)
  {
    library(jpeg)
    image=readJPEG(i)
    #print=i
    r <- image[,,1]
    g <- image[,,2]
    b <- image[,,3]
    
    image.r.pca <- prcomp(r, center = FALSE)
    image.g.pca <- prcomp(g, center = FALSE)
    image.b.pca <- prcomp(b, center = FALSE)
    
    #Storing PCA objects into a list.
    rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)
    length(rgb.pca)
    
#Step 4:    
    # Selecting the ncomp value
    propvar_r=cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100
    propvar_b=cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100
    propvar_g=cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100
    
    count_r=0
    count_b=0
    count_g=0
    
    for (i in propvar_r) {
      if (i<=clarity){ 
        count_r <- count_r +1
      } else {
        break
      }
    }
    
    for (i in propvar_b) {
      if (i<=clarity){ 
        count_b <- count_b +1
      } else {
        break
      }
    }
    
    for (i in propvar_g) {
      if (i<=clarity){ 
        count_g <- count_g +1
      } else {
        break
      }
    }
    
    ncomp=max(count_b,count_g,count_r)
    #print(i," - ",ncomp)
    #print(paste(print,ncomp,sep= "-"))
    
#Step 5:    
    #Reconstruct
    
    R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
    B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
    G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
    
    R = ifelse(R>1,1,R)
    G = ifelse(G>1,1,G)
    B = ifelse(B>1,1,B)
    
    R = ifelse(R<0,0,R)
    G = ifelse(G<0,0,G)
    B = ifelse(B<0,0,B)
    
    img=array(c(R,G,B),dim=c(dim(image)))
    library(stringr)
    
    # splitting the file name for writing compressed image
    
    count=count+1
    c=str_split(img.files[count], "\\.")[[1]][1]
    
    fil=paste(c,"_compress.jpg",sep= "")  # renaming the file
    writeJPEG(img,fil)                    # saving the compressed file
  }
  files=list.files(path = loc,all.files = T)
  #img.files1 = files[grep(".jpg", files, fixed=T)]
  img.files2 = files[grep("compress.jpg", files, fixed=T)]
  
  # getting the path of all the actual image files
  p1=c()
  for(i in img.files)
  {
    path=paste(loc,"\\",i,sep= "")
    p1=c(p1,path)
  }
  
  # getting the path of all the compressed image files
  p2=c()
  for(i in img.files2)
  {
    path=paste(loc,"\\",i,sep= "")
    p2=c(p2,path)
  }
  
  # creating a folder for the compressed images
  
  dir.create(file.path(loc,"compressed_image"), showWarnings = FALSE)
  setwd(file.path(loc,"compressed_image"))
  
  # copying the compressed images to the created folder
  
  for (i in p2)
  {
    file.copy(from=i, to=getwd(), 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    file.remove(i)
  }
  
  #check for the condition to delete the orginal file - if "yes" copy the orginal files
  #by creating a folder and deleteing from the source destination
  
  if (del_org == T){ 
    dir.create(file.path(loc,"Orginal_image"), showWarnings = FALSE)
    setwd(file.path(loc,"Orginal_image"))
    for (i in p1)
    {
      file.copy(from=i, to=getwd(), 
                overwrite = TRUE, recursive = FALSE, 
                copy.mode = TRUE)
      file.remove(i)
    }
  }
  setwd(default)
}

imagecompression(loc="C:\\Users\\lenovo\\Documents\\Imagecompression",
                 clarity = 99.5,del_org=F)