def ultima_aparicion(lista, e):
    indice = 0
    match = 0
    for elem in lista:
        if elem == e:
            match = indice
            indice += 1
        else:
            indice += 1
    return match

def pertenece(x, y):
    if x in y:
        return True
    else:
        return False

def elementos_exclusivos(x, y):
    list = []
    for elem in x:
        if elem in y:
            pass
        else:
            list.append(elem)
    for elem in y:
        if elem in x:
            pass
        else:
            list.append(elem)   
    return list

def testFunc(x, y):
    contador = 0
    for key in x:
        if key in y and x[key] == y[key]:
            contador += 1
        else: 
            pass
    return contador

def cuantasVeces(x, y):
    contador = 0
    for item in y:
        if x == item:
            contador += 1
        else:
            pass
    return contador

def convertir_a_diccionario(x):
    diccionario = {}
    for item in x:
        diccionario.update({item: cuantasVeces(item, x)})
    return diccionario

# 50 minutos    