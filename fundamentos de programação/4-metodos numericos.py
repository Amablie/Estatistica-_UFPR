def metonumeric():
    print("~~~~~~~~~~~Metodos Númericos~~~~~~~~~~~~~")
    Freq= float(input("Qual a frequência do aluno(a)?"))
    Nota= float(input("Qual a nota final do aluno(a)?"))
    if (Freq>=75.0)and(Nota>=70.0):
        print("APROVADO")
    else:
        if(Freq< 75.0) or (Nota<70.0):
            print("REPROVADO")
    return(metonumeric())
metonumeric()
