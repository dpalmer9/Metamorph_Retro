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
difficulty.data = as.data.frame(matrix(nrow = nrow(raw.data), ncol = ))

trial.count.start = which(colnames(raw.data) == 'Trial.Analysis...Condition..1.')
trial.correct.start = which(colnames(raw.data) == 'Trial.Analysis...Correct.Counter...Generic.Counter..1.')
trial.highrisk.start = which(colnames(raw.data) == 'Trial.Analysis...High.Risk.Counter...Generic.Counter..1.')
trial.difficulty.start = which(colnames(raw.data) == 'Trial.Analysis...Generic.Evaluation..1.')

for(a in 1:nrow(raw.data)){
  trial.max = length(as.vector(as.matrix(raw.data[a,c(trial.count.start:(trial.correct.start - 1))])))
  
}