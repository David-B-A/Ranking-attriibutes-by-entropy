entropia <- function(s){
  e=0
  if(s==0){
    e=0
  }else if(s==1){
    e=0
  }else{
    e = (s*log(s))+((1-s)*(log(s)))
  }
}

entropiaTotal <- function(data,tipos){
  et=0
  nregistros = nrow(data)
  nvariables = ncol(data)
  for (i in 1:(nregistros-1)){
    for (j in (i+1):nregistros){
      sumaSim=0
      for (k in 1:nvariables){
        if(tipos[k]==0){
          if(data[i,k]==data[j,k]){
            sumaSim = sumaSim+1
          }
        } else{
          sumaSim = sumaSim + (1-abs(data[i,k]-data[j,k])/(max(data[,k])-min(data[,k])))
        }
      }
      
      simPromedio = sumaSim/nvariables
      et = et + entropia(simPromedio)
    }
  }
  return(et)
}

#The data should be in a matrix
#It is necessary to define the attribute type, and put it in a vector (ordered by the variable number) where 1 = continuous, 0 = discrete.
#Returns a vector

variableQueMenosAporta <- function(matrixData, tipos){
  entropiaTotalDatos <- entropiaTotal(matrixData, tipos)
  nregistros = nrow(matrixData)
  nvariables = ncol(matrixData)
  entropiaVector = vector(length = nvariables)
  entropiaVector[] = 0
  for(variable in 1:nvariables){
    dataTemp <- matrixData[,-variable]
    tiposTemp <- tipos[-variable]
    nvariablesTemp <- nvariables-1
    matrizSim <- matrix(nrow=nregistros,ncol=nregistros)
    entropiaQuitandoX1 = 0;
    for (i in 1:(nregistros-1)){
      for (j in (i+1):nregistros){
        sumaSim=0
        if(nvariablesTemp>=2){
          for (k in 1:nvariablesTemp){
            if(tiposTemp[k]==0){
              if(dataTemp[i,k]==dataTemp[j,k]){
                sumaSim = sumaSim+1
              }
            } else{
              sumaSim = sumaSim + (1-abs(dataTemp[i,k]-dataTemp[j,k])/(max(dataTemp[,k])-min(dataTemp[,k])))
            }
          }
        }
        else{
          for (k in 1:nvariablesTemp){
            if(tiposTemp[k]==0){
              if(dataTemp[i]==dataTemp[j]){
                sumaSim = sumaSim+1
              }
            } else{
              sumaSim = sumaSim + (1-abs(dataTemp[i]-dataTemp[j])/(max(dataTemp)-min(dataTemp)))
            }
          }
        }
        
        
        simPromedio = sumaSim/nvariablesTemp
        entropiaVector[variable] = entropiaVector[variable] + entropia(simPromedio)
      }
    }
  }
  difentropia = abs(entropiaVector-entropiaTotalDatos)
  resultado = which(difentropia==min(difentropia))
  return(resultado)
}

rankingEntropy <- function(data,tipos){
  rankingVariables <- variableQueMenosAporta(data,tipos)
  nvariables = ncol(data)
  ndata = data
  ntipos = tipos
  for(variable in 2:(nvariables-1)){
    ndata = ndata[,-rankingVariables[variable-1]]
    ntipos = ntipos[-rankingVariables[variable-1]]
    temp <- variableQueMenosAporta(ndata,ntipos)
    for(a in 1:length(rankingVariables)){
      if (rankingVariables[a]<=temp){
        temp = temp+1
      }
    }
    rankingVariables[variable] <- temp
  }
  return(rankingVariables)
}