def massa_corp():
    print('~~~~~~~~~~~~~~~~~~~~~~Calculo de IMC~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    pes= float(input("Qual é o seu peso? (kg)"));
    alt= float(input("Qual é a sua altura? (m)"))
    imc= pes/(alt**2)
    print("Seu Indice de Massa Corporal é", imc, "kg/m^2")
    return(massa_corp())
massa_corp()
