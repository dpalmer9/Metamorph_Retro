## Library ##


## Read CSV ##
file.path = file.choose()
raw.data = read.csv(file.path)

## Latency Averaging (Longitudinal)
corr.lat.start = which(colnames(raw.data) == 'PD.Correct.Latency..1.' )
corr.lat.end = which(colnames(raw.data) == 'PD.Incorrect.Latency..1.' ) - 1

incorr.lat.start = which(colnames(raw.data) == 'PD.Incorrect.Latency..1.' )
incorr.lat.end = which(colnames(raw.data) == 'Metacognitive.Latency..1.' ) - 1

meta.lat.start = which(colnames(raw.data) == 'Metacognitive.Latency..1.')
meta.lat.end = which(colnames(raw.data) == 'Trial.Analysis...Condition..1.') - 1

mean.lat.data = as.data.frame(matrix(nrow = nrow(raw.data),ncol = 2))
colnames(mean.lat.data) = c('Correct Latency', 'Incorrect Latency')

for(a in 1:nrow(mean.lat.data)){
  mean.lat.data[a,1] = mean(as.vector(as.matrix(raw.data[a,c(corr.lat.start:corr.lat.end)])), na.rm=TRUE)
  mean.lat.data[a,2] = mean(as.vector(as.matrix(raw.data[a,c(incorr.lat.start:incorr.lat.end)])), na.rm=TRUE)
}


## Difficulty Dependent Data ##
measure.vector1 = c('Overall Accuracy','Accuracy High Risk', 'Accuracy Low Risk', 'High Risk Rate', 'Low Risk Rate')
measure.vector2 = c('Correct Latency','Incorrect Latency','Metacognitive High Risk Latency', 'Metacognitive Low Risk Latency', 'Correct Latency High Risk', 'Correct Latency Low Risk', 'Incorrect Latency High Risk', 'Incorrect Latency Low Risk')
measure.vector3 = c('Diff1','Diff2','Diff3','Diff4','Diff5')
final.part1 = c(measure.vector1,measure.vector2)

final.measure.vector = c()
curr.position = 1

for(a in 1:length(final.part1)){
  for(b in 1:5){
    final.measure.vector[curr.position] = paste(final.part1[a],b,sep='.')
    curr.position = curr.position + 1
  }
}

difficulty.data = as.data.frame(matrix(nrow = nrow(raw.data), ncol = length(final.measure.vector)))
colnames(difficulty.data) = final.measure.vector

trial.count.start = which(colnames(raw.data) == 'Trial.Analysis...Condition..1.')
trial.correct.start = which(colnames(raw.data) == 'Trial.Analysis...Correct.Counter...Generic.Counter..1.')
trial.highrisk.start = which(colnames(raw.data) == 'Trial.Analysis...High.Risk.Counter...Generic.Counter..1.')
trial.difficulty.start = which(colnames(raw.data) == 'Trial.Analysis...Generic.Evaluation..1.')

for(a in 1:nrow(raw.data)){
  trial.max = length(as.vector(as.matrix(raw.data[a,c(trial.count.start:(trial.correct.start - 1))])))
  
  temp.vec.corlat = list(c(),c(),c(),c(),c())
  temp.vec.corlat.high = list(c(),c(),c(),c(),c())
  temp.vec.corlat.low = list(c(),c(),c(),c(),c())
  temp.vec.incorlat = list(c(),c(),c(),c(),c())
  temp.vec.incorlat.high = list(c(),c(),c(),c(),c())
  temp.vec.incorlat.low = list(c(),c(),c(),c(),c())
  temp.vec.high = list(c(),c(),c(),c(),c())
  temp.vec.low = list(c(),c(),c(),c(),c())
  
  temp.low.correct = c(0,0,0,0,0)
  temp.high.correct = c(0,0,0,0,0)
  temp.low = c(0,0,0,0,0)
  temp.high = c(0,0,0,0,0)
  temp.total = c(0,0,0,0,0)
  
  for(b in 0:(trial.max - 1)){
    curr.difficulty = raw.data[a,(trial.difficulty.start + b)]
    curr.correct = raw.data[a,(trial.correct.start + b)]
    curr.high = raw.data[a,(trial.highrisk.start + b)]
    if(isTRUE((curr.difficulty == 1) | (curr.difficulty == 2))){
      temp.total[1] = temp.total[1] + 1
      if(curr.high >= 1){
        temp.high[1] = temp.high[1] + 1
        temp.vec.high[[1]][[(length(temp.vec.high[[1]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.high.correct[1] = temp.high.correct[1] + 1
          temp.vec.corlat[[1]][[(length(temp.vec.corlat[[1]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.high[[1]][[(length(temp.vec.corlat.high[[1]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[1]][[(length(temp.vec.incorlat[[1]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.high[[1]][[(length(temp.vec.incorlat.high[[1]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        temp.vec.low[[1]][[(length(temp.vec.low[[1]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.low.correct[1] = temp.low.correct[1] + 1
          temp.vec.corlat[[1]][[(length(temp.vec.corlat[[1]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.low[[1]][[(length(temp.vec.corlat.low[[1]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[1]][[(length(temp.vec.incorlat[[1]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.low[[1]][[(length(temp.vec.incorlat.low[[1]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 3) | (curr.difficulty == 4))){
      temp.total[2] = temp.total[2] + 1
      if(curr.high >= 1){
        temp.high[2] = temp.high[2] + 1
        temp.vec.high[[2]][[(length(temp.vec.high[[2]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.high.correct[2] = temp.high.correct[2] + 1
          temp.vec.corlat[[2]][[(length(temp.vec.corlat[[2]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.high[[2]][[(length(temp.vec.corlat.high[[2]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[2]][[(length(temp.vec.incorlat[[2]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.high[[2]][[(length(temp.vec.incorlat.high[[2]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        temp.vec.low[[2]][[(length(temp.vec.low[[2]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.low.correct[2] = temp.low.correct[2] + 1
          temp.vec.corlat[[2]][[(length(temp.vec.corlat[[2]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.low[[2]][[(length(temp.vec.corlat.low[[2]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[2]][[(length(temp.vec.incorlat[[2]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.low[[2]][[(length(temp.vec.incorlat.low[[2]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 5) | (curr.difficulty == 6))){
      temp.total[3] = temp.total[3] + 1
      if(curr.high >= 1){
        temp.high[3] = temp.high[3] + 1
        temp.vec.high[[3]][[(length(temp.vec.high[[3]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.high.correct[3] = temp.high.correct[3] + 1
          temp.vec.corlat[[3]][[(length(temp.vec.corlat[[3]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.high[[3]][[(length(temp.vec.corlat.high[[3]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[3]][[(length(temp.vec.incorlat[[3]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.high[[3]][[(length(temp.vec.incorlat.high[[3]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        temp.vec.low[[3]][[(length(temp.vec.low[[3]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.low.correct[3] = temp.low.correct[3] + 1
          temp.vec.corlat[[3]][[(length(temp.vec.corlat[[3]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.low[[3]][[(length(temp.vec.corlat.low[[3]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[3]][[(length(temp.vec.incorlat[[3]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.low[[3]][[(length(temp.vec.incorlat.low[[3]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 7) | (curr.difficulty == 8))){
      temp.total[4] = temp.total[4] + 1
      if(curr.high >= 1){
        temp.high[4] = temp.high[4] + 1
        temp.vec.high[[4]][[(length(temp.vec.high[[4]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.high.correct[4] = temp.high.correct[4] + 1
          temp.vec.corlat[[4]][[(length(temp.vec.corlat[[4]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.high[[4]][[(length(temp.vec.corlat.high[[4]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[4]][[(length(temp.vec.incorlat[[4]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.high[[4]][[(length(temp.vec.incorlat.high[[4]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        temp.vec.low[[4]][[(length(temp.vec.low[[4]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.low.correct[4] = temp.low.correct[4] + 1
          temp.vec.corlat[[4]][[(length(temp.vec.corlat[[4]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.low[[4]][[(length(temp.vec.corlat.low[[4]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[4]][[(length(temp.vec.incorlat[[4]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.low[[4]][[(length(temp.vec.incorlat.low[[4]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 9) | (curr.difficulty == 10))){
      temp.total[5] = temp.total[5] + 1
      if(curr.high >= 1){
        temp.high[5] = temp.high[5] + 1
        temp.vec.high[[5]][[(length(temp.vec.high[[5]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.high.correct[5] = temp.high.correct[5] + 1
          temp.vec.corlat[[5]][[(length(temp.vec.corlat[[5]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.high[[5]][[(length(temp.vec.corlat.high[[5]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[5]][[(length(temp.vec.incorlat[[5]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.high[[5]][[(length(temp.vec.incorlat.high[[5]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        temp.vec.low[[5]][[(length(temp.vec.low[[5]]) + 1)]] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct >= 1){
          temp.low.correct[5] = temp.low.correct[5] + 1
          temp.vec.corlat[[5]][[(length(temp.vec.corlat[[5]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
          temp.vec.corlat.low[[5]][[(length(temp.vec.corlat.low[[5]]) + 1)]] = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.incorlat[[5]][[(length(temp.vec.incorlat[[5]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
          temp.vec.incorlat.low[[5]][[(length(temp.vec.incorlat.low[[5]]) + 1)]] = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }
  }
  for(b in 0:4){
    difficulty.data[a,(1 + b)] = ((temp.low.correct[(b + 1)] + temp.high.correct[(b + 1)]) / (temp.total[(b + 1)])) * 100
    difficulty.data[a,(6 + b)] = ((temp.high.correct[(b + 1)]) / (temp.high[(b + 1)])) * 100
    difficulty.data[a,(11 + b)] = ((temp.low.correct[(b + 1)]) / ((temp.total[(b + 1)]) - (temp.high[(b + 1)]))) * 100
    difficulty.data[a,(16 + b)] = (temp.high[(b + 1)]) / (temp.total[(b + 1)])
    difficulty.data[a,(21 + b)] = ((temp.total[(b + 1)]) - (temp.high[(b + 1)])) / (temp.total[(b + 1)])
    difficulty.data[a,(26 + b)] = mean(temp.vec.corlat[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(31 + b)] = mean(temp.vec.incorlat[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(36 + b)] = mean(temp.vec.high[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(41 + b)] = mean(temp.vec.low[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(46 + b)] = mean(temp.vec.corlat.high[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(51 + b)] = mean(temp.vec.corlat.low[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(56 + b)] = mean(temp.vec.incorlat.high[[(b + 1)]], na.rm=TRUE)
    difficulty.data[a,(61 + b)] = mean(temp.vec.incorlat.low[[(b + 1)]], na.rm=TRUE)
  }
}

final.data = cbind(raw.data[ ,1:10], difficulty.data)
