from math import*
from string import*
def dif():
    print("########lista-diferença#########")
    LT=input("digite uma lista de números:").split()
    dif=max(LT)- min(LT)
    print("A difernça entre o maior e menor número da lista é:", dif)   
dif()
