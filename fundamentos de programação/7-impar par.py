def impar():
    print("#############FUNÇÃO IMPAR/PAR ###################")
    x=int(input("digite um numero:"))
    y= x%2
    if y == 0 :
        print("PAR")
    else:
        if y == 1:
            print("IMPAR")
    return(impar())
impar()
