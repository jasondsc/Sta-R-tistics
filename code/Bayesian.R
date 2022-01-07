


# simulate Monty Hall probelm

stay=c()
switch=c()

for (i in 1:10000){
  doors=1:3
  door_win=sample(1:3,1)
  door_pick=sample(1:3,1)
  door_host_open=doors[!(1:3 %in% c(door_pick,door_win))]
  if (length(door_host_open)==2){
    door_host_open= door_host_open[sample(1:2,1)]
  }
  
  if (door_pick==door_win) {
    stay[i]=1
    switch[i]=0
  } else{
    stay[i]=0
    switch[i]=1
  }
  
  
}

sum(stay)/10000
sum(switch)/10000