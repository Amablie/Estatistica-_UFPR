lista0=[]
def Vizinho(lis):
  i=0
  v=0
  while i<len(lis):
    if i==0:
      next
    else:
      if lis[i]< lis[i-1] and lis[i]<lis[i+1]:
        v=v+1
      else:
        if i==len(lis)-2:
          break
    i=i+1
  print(v)
