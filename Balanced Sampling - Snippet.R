#Balanced Sampling and Train-Test Split 5000 observations per tree type
n = 5000
class1_ind = which(df$class == 1)
class2_ind = which(df$class == 2)
class3_ind = which(df$class == 3)
class4_ind = which(df$class == 4)
class5_ind = which(df$class == 5)
class6_ind = which(df$class == 6)
class7_ind = which(df$class == 7)

class1_samp = sample(class1_ind, min(n, length(class1_ind)), replace = F)
class2_samp = sample(class2_ind, min(n, length(class2_ind)), replace = F)
class3_samp = sample(class3_ind, min(n, length(class3_ind)), replace = F)
class4_samp = sample(class4_ind, 0.8*min(n, length(class4_ind)), replace = F)
class5_samp = sample(class5_ind, min(n, length(class5_ind)), replace = F)
class6_samp = sample(class6_ind, min(n, length(class6_ind)), replace = F)
class7_samp = sample(class7_ind, min(n, length(class7_ind)), replace = F)

indices = c(class1_samp, class2_samp, class3_samp, class4_samp, class5_samp, class6_samp, class7_samp)
train = df[indices, ]
test = df[-indices, ]