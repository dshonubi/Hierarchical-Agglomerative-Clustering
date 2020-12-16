Average <- function(){
 
  ALM<-NULL

  tempAvgDist = 0
  tempAvgI = 0
  tempAvgJ = 0
  
  inACluster = NULL
  
  while (length(inACluster)!=a[1]) {
    
    avgDist = 1000000000
    
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
          trackerI = length(ALM)
          
          while(clusterI == 0){
            if(is.na(match(i,ALM[[trackerI]]))==FALSE){
              
              for(o in 1:length(ALM[[trackerI]])){
                clusterI= clusterI+
                  (sqrt(sum(abs(my_data[j,] - my_data[ALM[[trackerI]][o],])^2)))
              }  
              trackerI=length(ALM[[trackerI]])
            }else{
              trackerI = trackerI-1
            }
          }
          tempAvgDist = clusterI / trackerI
          
        }else if(is.na(match(j,inACluster))==FALSE){
          
          clusterJ = 0
          trackerJ = length(ALM)
          
          while(clusterJ == 0){
            if(is.na(match(j,ALM[[trackerJ]]))==FALSE){
              
              for(o in 1:length(ALM[[trackerJ]])){
                clusterJ = clusterJ +
                  (sqrt(sum(abs(my_data[i,] - my_data[ALM[[trackerJ]][o],])^2)))
              }  
              trackerJ=length(ALM[[trackerJ]])
            }else{
              trackerJ = trackerJ-1
            }
          }
          tempAvgDist = clusterJ / trackerJ    
          
        }else{
          
          tempAvgDist = sqrt(sum(abs(my_data[i,] - my_data[j,])^2))
          
        }
        
        if(tempAvgDist<avgDist){
          avgDist = tempAvgDist
          tempAvgI = i
          tempAvgJ = j
        }
        
      }
      
    }
    
    if(is.na(match(tempAvgI,inACluster)) && is.na(match(tempAvgJ,inACluster))){
      inACluster = c(inACluster,tempAvgI,tempAvgJ)
      
      ALM[[length(ALM)+1]] = sort(c(tempAvgI,tempAvgJ),decreasing = FALSE) 
      
    }else if(is.na(match(tempAvgI,inACluster))){
      
      jCluster = 0
      for(r in length(ALM):1){
        if(is.na(match(tempAvgJ,ALM[[r]]))==FALSE){
          jCluster = r
          break
        }
      }
      ALM[[length(ALM)+1]] = sort(c(ALM[[jCluster]],tempAvgI),decreasing = FALSE)
      inACluster = c(inACluster,tempAvgI) 
      
      
    }else if(is.na(match(tempAvgJ,inACluster))){
      
      iCluster = 0
      for(r in length(ALM):1){
        if(is.na(match(tempAvgI,ALM[[r]]))==FALSE){
          iCluster = r
          break
        }
      }
      ALM[[length(ALM)+1]] = sort(c(ALM[[iCluster]],tempAvgJ),decreasing = FALSE)
      inACluster = c(inACluster,tempAvgJ)
    }
  }
  
  clusterLocation = NULL
  clusters = (1:a[1])
  counter = 1
  p = length(ALM)
  
  while(length(clusterLocation) != a[1]){
    if(is.na(match(clusters[counter],ALM[[p]]))==FALSE){
      clusterLocation = c(clusterLocation,p)
      p =length(ALM)
      counter = counter+1
    }else{
      p = p-1
    }
  }
  
  
  while(all(clusterLocation[1:a[1]] == clusterLocation[1])!= TRUE){
    
    tempAvgDist = 0
    tempAvgT = 0
    tempAvgZ = 0
    tempclusterT = 0
    tempclusterZ = 0
    
    avgDist = 1000000000
    
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
        trackerT = length(ALM)
        
        while(clusterT == 0){
          if(is.na(match(clusters[t],ALM[[trackerT]]))==FALSE){
            clusterT=trackerT
            trackerT=length(ALM)
          }else{
            trackerT = trackerT-1
          }
        }
        
        
        clusterZ = 0
        trackerZ = length(ALM)
        
        while(clusterZ == 0){
          if(is.na(match(clusters[z],ALM[[trackerZ]]))==FALSE){
            clusterZ=trackerZ
            trackerZ=length(ALM)
          }else{
            trackerZ = trackerZ-1
          }
        }
        
        if(clusterT == clusterZ){
          next
        }
        
        
        clusterTsum = 0
        pairsT = 0    
        for(o in 1:length(ALM[[clusterT]])){
          clusterTsum = clusterTsum +
            (sqrt(sum(abs(my_data[z,] - my_data[ALM[[clusterT]][o],])^2)))
          pairsT = pairsT+1
        }
        
        
        clusterZsum = 0
        pairsZ = 0 
        for(o in 1:length(ALM[[clusterZ]])){
          clusterZsum = clusterZsum +
            (sqrt(sum(abs(my_data[t,] - my_data[ALM[[clusterZ]][o],])^2)))
          pairsZ = pairsZ+1
        }  
        
        tempAvgDist = (clusterZsum + clusterTsum)/ (pairsT + pairsZ)   
        
        if(tempAvgDist<avgDist){
          tempClusterT = clusterT
          tempClusterZ = clusterZ
        }
        
      }
    }
    ALM[[length(ALM)+1]] = sort(c(ALM[[tempClusterT]],ALM[[tempClusterZ]]),
                                decreasing = FALSE)
    
    clusterLocation = NULL
    clusters = (1:a[1])
    counter = 1
    p = length(ALM)
    
    while(length(clusterLocation) != a[1]){
      if(is.na(match(clusters[counter],ALM[[p]]))==FALSE){
        clusterLocation = c(clusterLocation,p)
        p =length(ALM)
        counter = counter+1
      }else{
        p = p-1
      }
    }
    
  }
  return(ALM)
}