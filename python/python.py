import math
#Guía 1???

def imprimir_hola_mundo():
    return "Hola mundo!"

def cancion():
    return "Todo camino puede andar \nTodo puede andar\nSon cipreses que vi\nSolo en sueños"

def raizDe2(x):
    return round(math.sqrt(x), 2)

def factorial(x):
    contador = 1
    for i in range(1, x+1):
        contador = contador * i
    return contador
    # Full rústico. Con recursión es más simple. No se me dió la gana.

def perimetro():
    return math.pi * 2

def imprimir_saludo():
    nombre = input()
    return print("Hola " + nombre)

def raiz_cuadrada(x):
    return math.sqrt(x)

def fTOc(x):
    return ((x-32)/1.8)

# imprimir_estribillo ni ganas

def es_multiplo_de(x, y):
    if (x % y) == 0:
        return True
    else:
        return False

def es_par(x):
    if es_multiplo_de(x, 2):
        return True
    else:
        return False

def cantidad_de_pizzas(comensales, minPorciones):
    contador = 0
    for comensal in range(1, comensales+1):
        contador += minPorciones
    return math.ceil(contador/8)

def alguno_cero(x, y):
    return x == 0 or y == 0

def ambos_cero(x, y):
    return x == 0 and y == 0

def nombre_largo(x):
    return len(x) >= 3 and len(x) <= 8

def es_bisiesto(x):
    return (x % 400 == 0) or ((x % 4 == 0) and not(x % 100 == 0))

def peso_pino(x):
    if (x*100 >= 300):
        return 900 + (((x*100) - 300) * 2)
    else:
        return (x*100) * 3

def es_peso_util(x):
    if x >= 400 and x <= 1000:
        return True
    else:
        return False

def sirve_pino(x):
    return es_peso_util(peso_pino(x))

def devolver_doble_si_par(x):
    if x % 2 == 0:
        return x*2
    else:
        return x

def lindo_nombre(x):
    if len(x) >= 5:
        return "Tu nombre tiene muchas letras. Cambiatelo."
    else:
        return "Tu nombre tiene menos de 5 caracteres."

def eco():
    contador = 0
    while contador < 10:
        contador += 1
        print("eco")
    return

def unoAlDiez():
    contador = 0
    while contador < 10:
        contador += 1
        print(contador)
    return

def countdown(x):
    while x > 0:
        print(x)
        x -= 1
    print("despegue")
    return