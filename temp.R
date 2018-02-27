N=100000
mt=matrix(nr=N,nc=7)
for(i in 1:N)
  mt[i,]=sample(1:52,7)

system.time(for(i in 1:N) # 1:10.35, 2:9.66s, 3:9.55s
  seq5in7(mt[i,1:2],mt[i,3:7]))




m=t(combn(13,7))
n=matrix(nr=nrow(m),nc=5)
for(i in 1:nrow(m))
  n[i,]=seq5in7(m[i,1:2],m[i,3:7])



j=1:7
system.time({
  for(i in 1:1000000) j=3:9
})

system.time({
  for(i in 1:1000000) j[]=2:8
})
