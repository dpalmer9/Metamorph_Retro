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
curr.position = 0

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
  
  temp.diff1.low.correct = 0
  temp.diff1.high.correct = 0
  temp.diff1.low = 0
  temp.diff1.high = 0
  temp.diff1.total = 0
  
  temp.diff2.low.correct = 0
  temp.diff2.high.correct = 0
  temp.diff2.low = 0
  temp.diff2.high = 0
  temp.diff2.total = 0
  
  temp.diff3.low.correct = 0
  temp.diff3.high.correct = 0
  temp.diff3.low = 0
  temp.diff3.high = 0
  temp.diff3.total = 0
  
  temp.diff4.low.correct = 0
  temp.diff4.high.correct = 0
  temp.diff4.low = 0
  temp.diff4.high = 0
  temp.diff4.total = 0
  
  temp.diff5.low.correct = 0
  temp.diff5.high.correct = 0
  temp.diff5.low = 0
  temp.diff5.high = 0
  temp.diff5.total = 0
  
  initial.correct = 0
  initial.high = 0
  
  temp.vec.diff1.corlat = vector()
  temp.vec.diff1.corlat.high = vector()
  temp.vec.diff1.corlat.low = vector()
  temp.vec.diff1.incorlat = vector()
  temp.vec.diff1.incorlat.high = vector()
  temp.vec.diff1.incorlat.low = vector()
  temp.vec.diff1.high = vector()
  temp.vec.diff1.low = vector()
  
  temp.vec.diff2.corlat = vector()
  temp.vec.diff2.corlat.high = vector()
  temp.vec.diff2.corlat.low = vector()
  temp.vec.diff2.incorlat = vector()
  temp.vec.diff2.incorlat.high = vector()
  temp.vec.diff2.incorlat.low = vector()
  temp.vec.diff2.high = vector()
  temp.vec.diff2.low = vector()
  
  temp.vec.diff3.corlat = vector()
  temp.vec.diff3.corlat.high = vector()
  temp.vec.diff3.corlat.low = vector()
  temp.vec.diff3.incorlat = vector()
  temp.vec.diff3.incorlat.high = vector()
  temp.vec.diff3.incorlat.low = vector()
  temp.vec.diff3.high = vector()
  temp.vec.diff3.low = vector()
  
  temp.vec.diff4.corlat = vector()
  temp.vec.diff4.corlat.high = vector()
  temp.vec.diff4.corlat.low = vector()
  temp.vec.diff4.incorlat = vector()
  temp.vec.diff4.incorlat.high = vector()
  temp.vec.diff4.incorlat.low = vector()
  temp.vec.diff4.high = vector()
  temp.vec.diff4.low = vector()
  
  temp.vec.diff5.corlat = vector()
  temp.vec.diff5.corlat.high = vector()
  temp.vec.diff5.corlat.low = vector()
  temp.vec.diff5.incorlat = vector()
  temp.vec.diff5.incorlat.high = vector()
  temp.vec.diff5.incorlat.low = vector()
  temp.vec.diff5.high = vector()
  temp.vec.diff5.low = vector()
  
  ##
  
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
  
  ##
  for(b in 0:(trial.max - 1)){
    curr.difficulty = raw.data[a,(trial.difficulty.start + b)]
    curr.correct = raw.data[a,(trial.correct.start + b)]
    curr.high = raw.data[a,(trial.highrisk.start + b)]
    if(isTRUE((curr.difficulty == 1) | (curr.difficulty == 2))){
      temp.diff1.total = temp.diff1.total + 1
      if(curr.high <= 1){
        temp.diff1.high = temp.diff1.high + 1
        temp.vec.diff1.high[b] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct <= 1){
          temp.diff1.high.correct = temp.diff1.high.correct + 1
          temp.vec.diff1.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff1.corlat.high = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff1.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff1.incorlat.high = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        if(curr.correct <= 1){
          temp.diff1.low.correct = temp.diff1.low.correct + 1
          temp.vec.diff1.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff1.corlat.low = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff1.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff1.incorlat.low = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 3) | (curr.difficulty == 4))){
      temp.diff2.total = temp.diff2.total + 1
      if(curr.high <= 1){
        temp.diff2.high = temp.diff2.high + 1
        temp.vec.diff2.high[b] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct <= 1){
          temp.diff2.high.correct = temp.diff2.high.correct + 1
          temp.vec.diff2.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff2.corlat.high = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff2.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff2.incorlat.high = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        if(curr.correct <= 1){
          temp.diff2.low.correct = temp.diff2.low.correct + 1
          temp.vec.diff2.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff2.corlat.low = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff2.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff2.incorlat.low = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 5) | (curr.difficulty == 6))){
      temp.diff3.total = temp.diff3.total + 1
      if(curr.high <= 1){
        temp.diff3.high = temp.diff3.high + 1
        temp.vec.diff3.high[b] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct <= 1){
          temp.diff3.high.correct = temp.diff3.high.correct + 1
          temp.vec.diff3.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff3.corlat.high = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff3.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff3.incorlat.high = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        if(curr.correct <= 1){
          temp.diff3.low.correct = temp.diff3.low.correct + 1
          temp.vec.diff3.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff3.corlat.low = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff3.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff3.incorlat.low = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 7) | (curr.difficulty == 8))){
      temp.diff4.total = temp.diff4.total + 1
      if(curr.high <= 1){
        temp.diff4.high = temp.diff4.high + 1
        temp.vec.diff4.high[b] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct <= 1){
          temp.diff4.high.correct = temp.diff4.high.correct + 1
          temp.vec.diff4.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff4.corlat.high = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff4.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff4.incorlat.high = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        if(curr.correct <= 1){
          temp.diff4.low.correct = temp.diff4.low.correct + 1
          temp.vec.diff4.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff4.corlat.low = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff4.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff4.incorlat.low = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }else if(isTRUE((curr.difficulty == 9) | (curr.difficulty == 10))){
      temp.diff5.total = temp.diff5.total + 1
      if(curr.high <= 1){
        temp.diff5.high = temp.diff5.high + 1
        temp.vec.diff5.high[b] = raw.data[a,(meta.lat.start + b)]
        if(curr.correct <= 1){
          temp.diff5.high.correct = temp.diff5.high.correct + 1
          temp.vec.diff5.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff5.corlat.high = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff5.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff5.incorlat.high = raw.data[a,(incorr.lat.start + b)]
        }
      }else if(curr.high == 0){
        if(curr.correct <= 1){
          temp.diff5.low.correct = temp.diff5.low.correct + 1
          temp.vec.diff5.corlat = raw.data[a,(corr.lat.start + b)]
          temp.vec.diff5.corlat.low = raw.data[a,(corr.lat.start + b)]
        }else if(curr.correct == 0){
          temp.vec.diff5.incorlat = raw.data[a,(incorr.lat.start + b)]
          temp.vec.diff5.incorlat.low = raw.data[a,(incorr.lat.start + b)]
        }
      }
    }
  }
  for(b in 0:4){
    difficulty.data[a,(1 + b)] = (temp.)
  }
}