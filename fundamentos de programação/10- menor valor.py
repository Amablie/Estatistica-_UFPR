from string import*
from math import*
def menorvalor():
    LT= input("digite uma lista de numeros:").split()
    y=min(LT)
    print("o menor valor dessa lista é:", y)
    return(menorvalor())
menorvalor()
    
    
                     
