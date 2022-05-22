def tempo_tarefa(hhi,mmi,hhf,mmf):
  hora=hhf-hhi
  minuto=mmf-mmi
  if hora<0:
    hora=hora+24
  if minuto<0:
    hora=hora-1
    minuto=minuto +60
  if hora>0:
    hora=hora*60
  tempo=hora+minuto
  print("Sua tarefa durou",tempo,"minuto(s)")
  
