
Complete <- function(){

  CLM<-NULL
  
  tempMaxDist = 0
  tempMaxI = 0
  tempMaxJ = 0
  
  inACluster = NULL
  
  while (length(inACluster)!=a[1]) {
    
    maxDist = 100000000000
    
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
          trackerI = length(CLM)
          tpi = 0
          while(clusterI == 0){
            if(is.na(match(i,CLM[[trackerI]]))==FALSE){
              tempclusterI = 0
              
              for(o in 1:length(CLM[[trackerI]])){
                tempclusterI= 
                  (sqrt(sum(abs(my_data[j,] - my_data[CLM[[trackerI]][o],])^2)))
                if(tempclusterI>clusterI){
                  clusterI = tempclusterI
                  tpi = CLM[[trackerI]][o]
                }
              }
              trackerI=length(CLM[[trackerI]])
              
            }else{
              trackerI = trackerI-1
            }
          }
          tempMaxDist = clusterI
          tpj = j
          
        }else if(is.na(match(j,inACluster))==FALSE){
          
          clusterJ = 0
          trackerJ = length(CLM)
          tpj = 0
          while(clusterJ == 0){
            tempclusterJ = 0
            if(is.na(match(j,inACluster))==FALSE){
              for(o in 1:length(CLM[[trackerJ]])){
                
                tempclusterJ= 
                  (sqrt(sum(abs(my_data[i,] - my_data[CLM[[trackerJ]][o],])^2)))
                
                if(tempclusterJ>clusterJ){
                  clusterJ = tempclusterJ
                  tpj = CLM[[trackerJ]][o]
                }
              }  
              trackerJ=length(CLM[[trackerJ]])
            }else{
              trackerJ = trackerJ-1
            }
          }
          tempMaxDist = clusterJ  
          tpi = i
        }else{
          
          tempMaxDist = sqrt(sum(abs(my_data[i,] - my_data[j,])^2))
          tpi = i
          tpj = j
        }
        if(tempMaxDist<maxDist){
          maxDist = tempMaxDist
          tempMaxI = tpi
          tempMaxJ = tpj
        }
        
      }
      
    }
    
    if(is.na(match(tempMaxI,inACluster)) && is.na(match(tempMaxJ,inACluster))){
      inACluster = c(inACluster,tempMaxI,tempMaxJ)
      
      CLM[[length(CLM)+1]] = sort(c(tempMaxI,tempMaxJ),decreasing = FALSE) 
      
    }else if(is.na(match(tempMaxI,inACluster))){
      
      jCluster = 0
      for(r in length(CLM):1){
        if(is.na(match(tempMaxJ,CLM[[r]]))==FALSE){
          jCluster = r
          break
        }
      }
      CLM[[length(CLM)+1]] = sort(c(CLM[[jCluster]],tempMaxI),decreasing = FALSE)
      inACluster = c(inACluster,tempMaxI) 
      
      
    }else if(is.na(match(tempMaxJ,inACluster))){
      
      iCluster = 0
      for(r in length(CLM):1){
        if(is.na(match(tempMaxI,CLM[[r]]))==FALSE){
          iCluster = r
          break
        }
      }
      CLM[[length(CLM)+1]] = sort(c(CLM[[iCluster]],tempMaxJ),decreasing = FALSE)
      inACluster = c(inACluster,tempMaxJ)
    }
    
  }
  clusterLocation = NULL
  clusters = (1:a[1])
  counter = 1
  p = length(CLM)
  
  while(length(clusterLocation) != a[1]){
    if(is.na(match(clusters[counter],CLM[[p]]))==FALSE){
      clusterLocation = c(clusterLocation,p)
      p =length(CLM)
      counter = counter+1
    }else{
      p = p-1
    }
  }
  
  
  
  while(all(clusterLocation[1:a[1]] == clusterLocation[1])!= TRUE){
    
    tempMaxDist = 0
    tempMaxT = 0
    tempMaxZ = 0
    tempclusterT = 0
    tempclusterZ = 0
    
    maxDist = 10000000000
    
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
        trackerT = length(CLM)
        
        
        while(clusterT == 0){
          if(is.na(match(clusters[t],CLM[[trackerT]]))==FALSE){
            clusterT=trackerT
          }else{
            trackerT = trackerT-1
          }
        }
        
        
        clusterZ = 0
        trackerZ = length(CLM)

        while(clusterZ == 0){
          if(is.na(match(clusters[z],CLM[[trackerZ]]))==FALSE){
            clusterZ=trackerZ
          }else{
            trackerZ = trackerZ-1
          }
        }
        
        if(clusterT == clusterZ){
          next
        }
        
        clusterT = 0
        tempclusterT = 0
        for(o in 1:length(CLM[[trackerT]])){
          tempclusterT= 
            (sqrt(sum(abs(my_data[z,] - my_data[CLM[[trackerT]][o],])^2)))
          
          if(tempclusterT>clusterT){
            clusterT = tempclusterT
          }
        }
        
        tempMaxDist = clusterT
        if(tempMaxDist<maxDist){
          maxDist = tempMaxDist
          tempClusterT = trackerT
          tempClusterZ = clusterZ
        }
        
      }
    }
    
    CLM[[length(CLM)+1]] = sort(c(CLM[[tempClusterT]],CLM[[tempClusterZ]]),
                                decreasing = FALSE)
    
    clusterLocation = NULL
    clusters = (1:a[1])
    counter = 1
    p = length(CLM)
    

    while(length(clusterLocation) != a[1]){
      if(is.na(match(clusters[counter],CLM[[p]]))==FALSE){
        clusterLocation = c(clusterLocation,p)
        p =length(CLM)
        counter = counter+1
      }else{
        p = p-1
      }
    }
    
  }
  return(CLM)
}



