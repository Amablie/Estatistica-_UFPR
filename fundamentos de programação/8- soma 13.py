from string import*
def soma_13():
    x= int(input("digite um numero"))
    soma=0
    for i in range(x):
        if (i%13 ==0):
            soma=soma+i
    print('A soma dos multiplos de 13 menores que', x, "é {}". format(soma))
           
    
soma_13()       

