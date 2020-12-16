Centroid <- function(){
  
  CDLM<-NULL
  
  tempCentroidDist = 0
  tempCentroidI = 0
  tempCentroidJ = 0
  
  inACluster = NULL
  
  while (length(inACluster)!=a[1]) {
    
    centroidDist = 1000000000
    
    varc = 1
    
    if(length(inACluster) != a[1]){
      while(is.na(match(varc,inACluster))==FALSE){
        varc = varc+1
      }
    }
    for(i in varc:a[1]){
      if(i == a[1]+1){
        break
      }
      
      
      for(j in 1:a[1]){
        
        if(j == a[1]+1){
          break
        }

        if(i == j){
          next
        }
        
        if((is.na(match(i,inACluster))==FALSE) && (is.na(match(j,inACluster))==FALSE)
           && (length(inACluster)!=a[1])){
          next
        }
        
        if(is.na(match(i,inACluster))==FALSE){
          
          clusterI = 0
          trackerI = length(CDLM)
          
          while(clusterI == 0){
            if(is.na(match(i,CDLM[[trackerI]]))==FALSE){
              
              cI = my_data[CDLM[[trackerI]][1:length(CDLM[[trackerI]])],]
              cI = colSums(cI,na.rm=TRUE)
              cI = cI/length(CDLM[[trackerI]])
              clusterI=length(CDLM[[trackerI]])
            }else{
              trackerI = trackerI-1
            }
          }
          tempCentroidDist =  (sqrt(sum(abs(my_data[j,] - cI)^2)))
          
        }else if(is.na(match(j,inACluster))==FALSE){
          
          clusterJ = 0
          trackerJ = length(CDLM)
          
          while(clusterJ == 0){
            if(is.na(match(j,CDLM[[trackerJ]]))==FALSE){
              
              cJ = my_data[CDLM[[trackerJ]][1:length(CDLM[[trackerJ]])],]
              cJ = colSums(cJ,na.rm=TRUE)
              cJ = cJ/length(CDLM[[trackerJ]])
              clusterJ=length(CDLM[[trackerJ]])
            }else{
              trackerJ = trackerJ-1
            }
          }
          tempCentroidDist =  (sqrt(sum(abs(my_data[i,] - cJ)^2))) 
          
        }else{
          
          tempCentroidDist = sqrt(sum(abs(my_data[i,] - my_data[j,])^2))
          
        }
        
        if(tempCentroidDist<centroidDist){
          centroidDist = tempCentroidDist
          tempCentroidI = i
          tempCentroidJ = j
        }
        
      }
      
    }
    
    if(is.na(match(tempCentroidI,inACluster)) && is.na(match(tempCentroidJ,inACluster))){
      inACluster = c(inACluster,tempCentroidI,tempCentroidJ)
      
      CDLM[[length(CDLM)+1]] = sort(c(tempCentroidI,tempCentroidJ),decreasing = FALSE) 
      
    }else if(is.na(match(tempCentroidI,inACluster))){
      
      jCluster = 0
      for(r in length(CDLM):1){
        if(is.na(match(tempCentroidJ,CDLM[[r]]))==FALSE){
          jCluster = r
          break
        }
      }
      CDLM[[length(CDLM)+1]] = sort(c(CDLM[[jCluster]],tempCentroidI),decreasing = FALSE)
      inACluster = c(inACluster,tempCentroidI) 
      
      
    }else if(is.na(match(tempCentroidJ,inACluster))){
      
      iCluster = 0
      for(r in length(CDLM):1){
        if(is.na(match(tempCentroidI,CDLM[[r]]))==FALSE){
          iCluster = r
          break
        }
      }
      CDLM[[length(CDLM)+1]] = sort(c(CDLM[[iCluster]],tempCentroidJ),decreasing = FALSE)
      inACluster = c(inACluster,tempCentroidJ)
    }
  }
  
  clusterLocation = NULL
  clusters = (1:a[1])
  counter = 1
  p = length(CDLM)
  
  while(length(clusterLocation) != a[1]){
    if(is.na(match(clusters[counter],CDLM[[p]]))==FALSE){
      clusterLocation = c(clusterLocation,p)
      p =length(CDLM)
      counter = counter+1
    }else{
      p = p-1
    }
  }
  
  
  while(all(clusterLocation[1:a[1]] == clusterLocation[1])!= TRUE){
    
    tempCentroidDist = 0
    tempCentroidT = 0
    tempCentroidZ = 0
    tempclusterT = 0
    tempclusterZ = 0
    
    centroidDist = 1000000000
    
    for(t in 1:a[1]){
      
      if(t == a[1]+1){
        break
      }
      
      for(z in 1:a[1]){
        
        if(z == a[1]+1){
          break
        }
        
        if(t == z){
          next
        }
        
        clusterT = 0
        trackerT = length(CDLM)
        
        while(clusterT == 0){
          if(is.na(match(clusters[t],CDLM[[trackerT]]))==FALSE){
            clusterT=trackerT
            trackerT=length(CDLM)
          }else{
            trackerT = trackerT-1
          }
        }
        
        
        clusterZ = 0
        trackerZ = length(CDLM)
       
        while(clusterZ == 0){
          if(is.na(match(clusters[z],CDLM[[trackerZ]]))==FALSE){
            clusterZ=trackerZ
            trackerZ=length(CDLM)
          }else{
            trackerZ = trackerZ-1
          }
        }
        
        if(clusterT == clusterZ){
          next
        }
        
        
        
        clusterT = 0
        trackerT = length(CDLM)
        
        while(clusterT == 0){
          if(is.na(match(z,CDLM[[trackerT]]))==FALSE){
            
            cT = my_data[CDLM[[trackerT]][1:length(CDLM[[trackerT]])],]
            cT = colSums(cT,na.rm=TRUE)
            cT = cT/length(CDLM[[trackerT]])
            clusterT = trackerT
          }else{
            trackerT = trackerT-1
          }
        }
        
        
        clusterZ = 0
        trackerZ = length(CDLM)
        
        while(clusterZ == 0){
          if(is.na(match(t,CDLM[[trackerZ]]))==FALSE){
            
            cZ = my_data[CDLM[[trackerZ]][1:length(CDLM[[trackerZ]])],]
            cZ = colSums(cZ,na.rm=TRUE)
            cZ = cZ/length(CDLM[[trackerZ]])
            clusterZ= trackerZ
          }else{
            trackerZ = trackerZ-1
          }
        }
        
        tempCentroidDist = (sqrt(sum(abs(cT - cZ)^2)))   
        
        if(tempCentroidDist<centroidDist){
          centroidDist = tempCentroidDist
          tempClusterT = clusterT
          tempClusterZ = clusterZ
        }
        
      }
    }
    CDLM[[length(CDLM)+1]] = sort(c(CDLM[[tempClusterT]],CDLM[[tempClusterZ]]),
                                  decreasing = FALSE)
    
    clusterLocation = NULL
    clusters = (1:a[1])
    counter = 1
    p = length(CDLM)
    
    while(length(clusterLocation) != a[1]){
      if(is.na(match(clusters[counter],CDLM[[p]]))==FALSE){
        clusterLocation = c(clusterLocation,p)
        p =length(CDLM)
        counter = counter+1
      }else{
        p = p-1
      }
    }
    
  }
  return(CDLM)
}