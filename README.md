Primero se define una relación como una serie de lineas con el siguiente formato:

```text
NombreRelacion
A(S)         B(I)  C(I)   D(F)      E(I)
"sdfsd"      7     20     20.0      4
N            3     4      4.0       5
```

Guardado en un archivo "relacion.r". Por simplicidad todas las relación en la carpeta "relations"
del proyecto se cargan al inicio, los nombres de las relaciones se cargan como identificadores de
las mismas.

Las operaciones serian:
* project(A; B)(NombreRelacion): permite generar una nueva relación solo con parte de las
  atributos originales.
* select(A = "sdfsd")(NombreRelacion): permite generar una nueva relacion con solo las
  filas que cumplen con una condicion booleana dada. Esta puede contener los operadores
  equals (=), different (<>), less than (<), more than (>) y los compuestos >= y <=.
* rename(Relacion)(NombreRelacion): renombra una relacion existente
* R1 + R2: la suma une dos relaciones, agregando atributos y filas. Al
  agregar atributos, las celdas sin valor pasan a tener el valor "N" que representa la falta
  de valor.
* R1 - R2: devuelve una copia de R1 sin las filas duplicadas en R2.
* R1 / R2: devuelve una copia de R1 sin los atributos que tiene R2, y elimina de R1 las filas
  con valores duplicados en los atributos duplicados.
* R1 * R2: Devuelve el producto cartesiano entre las relaciones.
* R1 |R1.A=R2.B| R2: Devuelve las filas de  producto cartesiano A*B donde R1.A=R2.B.
* R1 |*| R2: Caso especial del operador anterior, todos los atributos con el mismo nombre
  son comparados, solo se devuelven las filas donde son todos idénticos.
* print(R1): perimite visualizar la relacion.
* También hay una operación de asignación en donde una relación se le asigna un nombre. La 
  diferencia entre esta y renombre es que renombre no renombra globalmente, solo localmente 
  al contexto del termino. En cambio asignación asigna un nombre nuevo a una relación, 
  ya sea una que ya tiene nombre o una que no tiene.

Errores y tipado:
* Los atributos se pueden escribir tanto como "R1.A" como "A" si no son ambiguos,
  si de hecho son ambiguos, un error es devuelto.
* Las relaciones y los atributos usados en un termino deben existir, si no existen,
  un error es devuelto.
* La primera linea define los tipos de los argumentos de base de datos: integer "I",
  float "F" o string "S", todos los tipos tienen un elemento especial "N" que implica la no
  existencia de un valor, si los tipos difieren de los indicados, se emite un error
  de tipo en la carga.
* El lenguaje posee otro sistema de tipos: relación, atributo y
  booleano, las comparaciones se hacen entre atributos, y nunca dan true si los
  tipos de los atributos en la relación difieren, incluso si ambos son "N".

Para implementar este TP final se requiere el uso de:
* Parsec (https://hackage.haskell.org/package/parsec) para parsear la entrada
* Data.Set (https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Set.html)
* y otros: Data.Map.Strict, Data.Maybe, Prelude
* Tambien se usa stack para obtener dependencias