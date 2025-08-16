Name = c("Anna", "Caroline", "Laura")
Psychology = c(80, 63, 100)
History = c(70, 20, 50)

df = data.frame(Name, Psychology, History)
df

cov_matrix = cov(df[,2:3])
cov_matrix
