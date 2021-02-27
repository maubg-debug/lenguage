# mau-lang
 Un lenguaje de programacion total mente nuevo hecho por mi

extension : `.mau`

## Syntaxis

Asi seria un comentario en este lenguage de programacion

```
//- Mi comentario
```

Para poder hacer variables seria asi como ejemplo

```js
var ejemplo = "foo"; //- Puede terminar por ; o sin nada
//- Tambien se puede con numeros
var ejemplo2 = 2
var ejemplo3 = 2.20

//- Booleanos
var ejemplo4 = true
var ejemplo5 = false
```

Como podras hacer input/output en este lenguage de programacion

```js
//- Output
printLn("...")

//- Input
var mi_input = input()
var mi_input_en_int = int_input() //- Tambien se puede int(input())
```

Este seria un ejemplo para in if statement en el que se puede implementar tambien `else if` y tambien `else` terminando con `end`. Dentro de las palabras claves deberias de introducir tu condicion

```js
if condicion then
    //- ....
else if condicion then
    //- ....
else
    //- ....
end
```

Aqui te enseño unos build-in para que puedas ver como combertir variables de tipo y como ver el tipo.

```js
var string = "32"
var numero = int(string)
var booleano = bool(string)
var string = str(numero)
var num = float(numero)
var lista = list("1234")
```

Tambien puedes hacer for loops y while loops porque todo buien lenguage de programmacion deberia de tener

```js
//- for loops
for i = 0 to 5 then
    printLn(i)
end

//- while loops
while true then
    printLn("loooop")
end

//- break / continue
for i = 0 to 10 then
    printLn(i)
    if i <= 5 then
        continue
    end
    
    break
end
```

En este lenguage de programmacion se puede hacer dos tipos de funciones (la normal, con flecha)

```js

//- Notar como las funciones no tienes `then` pero si tiene `end`

func bio(nombre, edad) -> "Hola, " + nombre + " | Edad: " + edad

func cojer_edad() //- Puedes pasar argumentos separados por una `,`
    var edad = int_input()
    return str(edad)
end

func main()
    var nombre = input()
    var edad = cojer_edad()

    printLn(bio(nombre, edad))
end

main()
```

En este lenguage de programacion tambien se puede hacer mas cosas con los build-in functions como por ejemplo hacer que el programa se pare por un momento o tambien checkear de que tipo una variable es.

```js
func main()
    printLn("main")
end

//- Y mas
printLn(type(main))
printLn(type(0))
printLn(type("hola"))
printLn(type(true))

if is_func(main) then //- True
    //- ...
end

//- is_int
//- is_str
//- is_func
//- is_list
```

Aprende a como se prodria manipular los arrays en este lenguage de programmacion

```js
var arr = [10, 20 , 30, 50, 40]

printLn(arr)

join(arr, 60)
has_value(arr, 60) //- true
pop(arr, 60)
extend(arr, [1, 3, 4, 6])
```

Para mas funcionalidad y diversion hemos añadido algunos build-in para hacerlos mas interesante

```js
sleep(4)
CLS()
isinstance("hola", str)
//- ...
```
