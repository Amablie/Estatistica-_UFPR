from string import*
from math import*
def dife():
    print("~~~~~~~~~~~~~~~~~~~~~~~~MAIOR NUMERO~~~~~~~~~~~~~~~~~~~~~~~~~~")
    lista= input("Digite uma lista numeros:").split()
    max(lista)
    print("o maior numero dessa lista é", max(lista))
    return(dife())
dife()
    
                     
