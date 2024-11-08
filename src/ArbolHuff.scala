
import scala.annotation.tailrec

abstract class ArbolHuff {
  //Calcula el peso del arbol
  def pesoArbol: Int = this match
    case HojaHuff(_, p) => p
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.pesoArbol + nodoDcha.pesoArbol

  //Función que devuelve la lista de caracteres de un ArbolHuff
  def caracteres:List[Char]= this match
    case HojaHuff(c, _) => List(c)
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.caracteres ++ nodoDcha.caracteres


  type Bit = 0 | 1 //Creación del tipo de dato bit

  // Función que a partir de una lista de bits devuelve el texto
  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(subArbol: ArbolHuff, restobits: List[Bit], resultado: List[Char]): String = (subArbol, restobits) match
      //Caso hoja, añadir caracter y empezar de nuevo
      case (HojaHuff(caracter, _), _) => decodificarAux(this, restobits, caracter :: resultado)
      //Caso rama, depende del bit 0 izq 1dcha
      case (RamaHuff(nodoIzq, _), 0 :: tail) => decodificarAux(nodoIzq, tail, resultado)
      case (RamaHuff(_, nodoDcha), 1 :: tail) => decodificarAux(nodoDcha, tail, resultado)
      //No quedan bits
      case _ => listaCharsACadena(resultado.reverse) //Invertimos y convertimos a cadena de texto

    decodificarAux(this, bits, Nil)

  //Determina si el árbol contiene o no un caracter
  def arbolcontiene(char:Char): Boolean = this match
    case HojaHuff(caracter, _) => caracter == char
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.arbolcontiene(char) || nodoDcha.arbolcontiene(char)
  //Convierte el texto a una cadena de bits
  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(caracter: Char, arbol: ArbolHuff, listabits: List[Bit]): List[Bit] = arbol match
      case HojaHuff(c, _) if c == caracter => listabits
      case RamaHuff(nodoIzq, _) if nodoIzq.arbolcontiene(caracter) => codificarAux(caracter, nodoIzq, listabits :+ 0)
      case RamaHuff(_, nodoDcha) if nodoDcha.arbolcontiene(caracter) => codificarAux(caracter, nodoDcha, listabits :+ 1)
      case _ => throw new IllegalArgumentException("Carácter no está")
    //Convertir cada caracter de la cadena a binario y concatena la lista
    cadena.foldLeft(List[Bit]())((acumulador, char) => acumulador ++ codificarAux(char, this, Nil))

  
}

//Función que pasa de cadena de texto a lista de caracteres
def cadenaAListaChars(cadena: String): List[Char] =
  @tailrec
  def cAlCharsAux(posicion: Int, listaAux: List[Char]): List[Char] = posicion match
    case p if p >= cadena.length => listaAux.reverse //caso base, hemos llegado al final de la cadena
    case _ => cAlCharsAux(posicion + 1, cadena.charAt(posicion) :: listaAux) // Agregamos el carácter a la listaAux y continuamos con la recursividad

  cAlCharsAux(0, Nil)

def listaCharsACadena(listaCar: List[Char]): String =
  @tailrec
  def lCharsACAux(listaCar: List[Char], cadena: String): String = listaCar match
    case Nil => cadena // Cuando la lista está vacía, devuelve el resultado acumulado como String
    case head :: tail => lCharsACAux(tail, cadena + head) // Añadimos el carácter al StringBuilder y continuamos

  lCharsACAux(listaCar, "")
// Convierte la lista de caracteres en distribución de frecuencias.
def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
  // Función auxiliar recursiva de cola
  @tailrec
  def ListaCharAux(listaChars: List[Char], listaDevolver: List[(Char, Int)]): List[(Char, Int)] = listaChars match
    case Nil => listaDevolver.reverse // Caso base: si la lista está vacía, devolvemos el acumulador
    case head :: tail =>
      listaDevolver.find(_._1 == head) match
        case Some((c, count)) =>
          // Si el carácter ya está en el acumulador, actualizamos la frecuencia
          ListaCharAux(tail, listaDevolver.filterNot(_._1 == head) :+ (c, count + 1))
        case None =>
          // Si el carácter no está, lo agregamos con frecuencia 1
          ListaCharAux(tail, (head, 1)::listaDevolver)

  // Llamada inicial con la lista de caracteres y una lista vacía como acumulador
  ListaCharAux(listaChar, List.empty)


// Convierte la distribución en una lista de hojas ordenada
def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[HojaHuff] =
  @tailrec
  def insertar(hoja: HojaHuff, lista: List[HojaHuff], acumulado: List[HojaHuff]): List[HojaHuff] = lista match
    case Nil => acumulado.reverse :+ hoja // insertamos hoja al final del acumulado
    case head :: tail if hoja.peso <= head.peso => (acumulado.reverse :+ hoja) ::: lista // Insertamos la hoja en la posición sin invertir
    case head :: tail => insertar(hoja, tail, head :: acumulado) // Recorremos hasta encontrar la posición

  // Convertimos cada tupla a HojaHuff e insertamos en la lista ordenada
  frec.foldLeft(List.empty[HojaHuff]) { case (acc, (caracter, peso)) => insertar(HojaHuff(caracter, peso), acc, Nil)}

// Crea un objeto RamaHuff integrando los dos ArbolHuff (izquierdo y derecho) que se le pasan como parámetros


//Comprobar que la lista solo tenga un elemento
def esListaSingleton(lista: List[ArbolHuff]): Boolean =
  lista.length == 1 //la lista tiene un elemento


//Función que coge una lista de nodos de clase ArbolHuff ordenandolos por sus pesos de forma creciente y combinandolos
@tailrec
def combinar(nodos: List[ArbolHuff]): List[ArbolHuff] =
  def creaRamaHuff(izq: ArbolHuff, dcha: ArbolHuff): RamaHuff =
    RamaHuff(izq, dcha)

  if nodos.length <= 1 then nodos
  else
    val (izq, dch) = (nodos.head, nodos.tail.head) //dos primeros nodos
    val nuevaRama = creaRamaHuff(izq, dch)
    val nuevalista = insertarConOrden(nuevaRama, nodos.tail.tail) //añade la rama a la lista
    combinar(nuevalista) //combina la lista


def insertarConOrden(nuevaRama: ArbolHuff, lista: List[ArbolHuff]): List[ArbolHuff] = lista match
  case Nil => List(nuevaRama)
  case head :: tail =>
    if (nuevaRama.pesoArbol <= head.pesoArbol) nuevaRama :: lista
    else  head :: insertarConOrden(nuevaRama,tail )

//Crear el Arbol de Huffman
def crearArbolHuffman(cadena:String):ArbolHuff=
  @tailrec
  def repetirHasta(combinar: List[ArbolHuff] => List[ArbolHuff], esListaSingleton: List[ArbolHuff] => Boolean)(lista: List[ArbolHuff]): List[ArbolHuff] =
    if lista == Nil then Nil
    else if esListaSingleton(lista) then lista // Caso base
    else
      val nuevalista = combinar(lista)
      repetirHasta(combinar, esListaSingleton)(nuevalista)

  //Lista de caracteres
  val listaChars= cadenaAListaChars(cadena)
  //frecuencias
  val frecuencias=ListaCharsADistFrec(listaChars)
  //lista de hojas segun el peso
  val listaHojas = DistribFrecAListaHojas(frecuencias)
  //Arbol combinando nodos
  repetirHasta(combinar, esListaSingleton)(listaHojas) match
    case List(arbol)=> arbol //devuelve el arbol completo
    case _ => throw new IllegalStateException("Error al crear el arbol")




object ArbolHuff {
  def apply(cadena: String): ArbolHuff =
    crearArbolHuffman(cadena)
    

  private type Bit = 0 | 1
  private type TablaCodigos = List[(Char, List[Bit])]

  // Transforma un árbol de Huffman en una tabla de codificaciónjengi
  def deArbolATabla(arbol: ArbolHuff): TablaCodigos =
    def recorrer(arbol: ArbolHuff, codigoActual: List[Bit]): TablaCodigos = arbol match
      case HojaHuff(caracter, _) => List((caracter, codigoActual))
      case RamaHuff(nodoIzq, nodoDcha) => recorrer(nodoIzq, codigoActual :+ 0) ::: recorrer(nodoDcha, codigoActual :+ 1)

    recorrer(arbol, Nil)


  //Codifica mensajes con la tabla
  def codificarTabla(arbol: TablaCodigos)(cadena: String): List[Bit]=
    //encuentra el codigo correspondiente
    @tailrec
    def buscarCodigo(char: Char, tabla: TablaCodigos): List[Bit] = tabla match
      case Nil => throw new IllegalArgumentException("no encontrado")
      case (caracter, codigo) :: resto =>
        if caracter == char then codigo
        else buscarCodigo(char, resto)
    @tailrec
    def codificarTablaAux(cadena: List[Char], resultado: List[Bit]): List[Bit] = cadena match
      case Nil => resultado
      case char :: resto =>
        val bits: List[Bit] = buscarCodigo(char, arbol) //busca la codificacion del caracter
        codificarTablaAux(resto, resultado ::: bits) //añade los bits encontrados
    codificarTablaAux(cadena.toList, Nil)

  //Decodifica mensajes empleando la tabla
  def decodificarTabla(tabla: TablaCodigos)(lista: List[Bit]): String =
    // Encuentra el carácter
    def buscarChar(bits: List[Bit], tabla: TablaCodigos): Char =
      @tailrec
      def buscarAux(tabla: TablaCodigos): Char = tabla match
        case Nil => throw new IllegalArgumentException("no encontrado")
        case (caracter, codigo) :: resto =>
          if codigo == bits then caracter
          else buscarAux(resto) //Sigue buscando
      buscarAux(tabla)

    def decodificarTablaAux(bits: List[Bit], resultado: String, acumulado: List[Bit]): String = bits match
      case Nil =>
        if acumulado.isEmpty then resultado  //caso acumulado vacio
        else resultado + buscarChar(acumulado, tabla)
      case bit :: resto =>
        val nuevoacumulado: List[Bit] = acumulado :+ bit  //Añade el bit al acumulado
        try
          val char = buscarChar(nuevoacumulado, tabla)
          decodificarTablaAux(resto, resultado + char, Nil) //si encuentra el caracter reinicia el acumulado
        catch
          case _: IllegalArgumentException => decodificarTablaAux(resto, resultado, nuevoacumulado) //Si no, sigue acumulando

    decodificarTablaAux(lista, "", Nil)
}

case class RamaHuff(nodoIzq:ArbolHuff, nodoDcha: ArbolHuff) extends ArbolHuff
case class HojaHuff(caracter:Char, peso: Int) extends ArbolHuff

object miPrograma extends App {
  //TESTEAMOS UN CON UNA FRASE
  val textoOriginal = "ejemplo de Arbol Huffman"

  // Crear el árbol de Huffman
  val arbolHuffman = ArbolHuff(textoOriginal)
  println(s"Texto original: $textoOriginal")

  // Convertir el árbol a una tabla de códigos
  val tablaCodigos = ArbolHuff.deArbolATabla(arbolHuffman)
  println(s"Tabla de códigos: $tablaCodigos")

  // Codificar el texto original
  val codificacion = ArbolHuff.codificarTabla(tablaCodigos)(textoOriginal)
  println(s"Codificación: $codificacion")

  // Decodificar la codificación de vuelta al texto original
  val textoDecodificado = ArbolHuff.decodificarTabla(tablaCodigos)(codificacion)
  println(s"Texto decodificado: $textoDecodificado")

  // Convertir la cadena a lista de caracteres
  val listaChars = cadenaAListaChars(textoOriginal)
  println(s"Lista de caracteres de '$textoOriginal': $listaChars")

  // Convertir la lista de caracteres de nuevo a cadena
  val cadenaDesdeLista = listaCharsACadena(listaChars)
  println(s"Cadena convertida desde la lista de caracteres: $cadenaDesdeLista")

  // Probar la creación de la lista de distribución de frecuencias
  val frecuencias = ListaCharsADistFrec(listaChars)
  println(s"Frecuencias de caracteres: $frecuencias")

  // Probar la conversión de frecuencias a hojas
  val listaHojas = DistribFrecAListaHojas(frecuencias)
  println(s"Lista de hojas según frecuencias: $listaHojas")

  // Comprobar la combinación de nodos
  val nodosCombinados = combinar(listaHojas)
  println(s"Nodos combinados: $nodosCombinados")

  // Comprobar si la lista de nodos tiene un solo elemento
  println(s"La lista de nodos combinados tiene un único elemento: ${esListaSingleton(nodosCombinados)}")

  // Mostrar el peso del árbol
  println(s"Peso del árbol de Huffman: ${arbolHuffman.pesoArbol}")



  //TESTEAMOS CON UNA CADENA CON UN ÚNICO ESPACIO
  val textoOriginal3 = " "

  // Crear el árbol de Huffman
  val arbolHuffman3 = ArbolHuff(textoOriginal3)
  println(s"Texto original: $textoOriginal3")

  // Convertir el árbol a una tabla de códigos
  val tablaCodigos3 = ArbolHuff.deArbolATabla(arbolHuffman3)
  println(s"Tabla de códigos: $tablaCodigos3")

  // Codificar el texto original
  val codificacion3 = ArbolHuff.codificarTabla(tablaCodigos3)(textoOriginal3)
  println(s"Codificación: $codificacion3")

  // Decodificar la codificación de vuelta al texto original
  val textoDecodificado3 = ArbolHuff.decodificarTabla(tablaCodigos3)(codificacion3)
  println(s"Texto decodificado: $textoDecodificado3")

  // Convertir la cadena a lista de caracteres
  val listaChars3 = cadenaAListaChars(textoOriginal3)
  println(s"Lista de caracteres de '$textoOriginal3': $listaChars3")

  // Convertir la lista de caracteres de nuevo a cadena
  val cadenaDesdeLista3 = listaCharsACadena(listaChars3)
  println(s"Cadena convertida desde la lista de caracteres: $cadenaDesdeLista3")

  // Probar la creación de la lista de distribución de frecuencias
  val frecuencias3 = ListaCharsADistFrec(listaChars3)
  println(s"Frecuencias de caracteres: $frecuencias3")

  // Probar la conversión de frecuencias a hojas
  val listaHojas3 = DistribFrecAListaHojas(frecuencias3)
  println(s"Lista de hojas según frecuencias: $listaHojas3")

  // Comprobar la combinación de nodos
  val nodosCombinados3 = combinar(listaHojas3)
  println(s"Nodos combinados: $nodosCombinados3")

  // Comprobar si la lista de nodos tiene un solo elemento
  println(s"La lista de nodos combinados tiene un único elemento: ${esListaSingleton(nodosCombinados3)}")

  // Mostrar el peso del árbol
  println(s"Peso del árbol de Huffman: ${arbolHuffman3.pesoArbol}")


  //TESTEAMOS CON UNA CADEA VACÍA
  val textoOriginal2 = ""

  try {
    // Intentamos crear el árbol de Huffman
    val arbolHuffman2: ArbolHuff = ArbolHuff(textoOriginal2)

    // Si se crea correctamente, mostramos el texto original
    println(s"Texto original: $textoOriginal2")

  } catch {
    // Captura de cualquier excepción que ocurra
    case e: Exception =>
      println("Ocurrió un error al crear el árbol de Huffman.")
  }
  //Al crear el arbol salta el error ---> Caused by: java.lang.IllegalStateException: Error al crear el arbol
  //que hemos tratado en la función crearArbolHuffman.


  //TESTEAMOS EL CASO EN EL QUE HAY TILDES
  val textoOriginal4 = "aaáá"
  // Crear el árbol de Huffman
  val arbolHuffman4 = ArbolHuff(textoOriginal4)
  println(s"Texto original: $textoOriginal4")

  // Convertir el árbol a una tabla de códigos
  val tablaCodigos4 = ArbolHuff.deArbolATabla(arbolHuffman4)
  println(s"Tabla de códigos: $tablaCodigos4")

  // Codificar el texto original
  val codificacion4 = ArbolHuff.codificarTabla(tablaCodigos4)(textoOriginal4)
  println(s"Codificación: $codificacion4")

  // Decodificar la codificación de vuelta al texto original
  val textoDecodificado4 = ArbolHuff.decodificarTabla(tablaCodigos4)(codificacion4)
  println(s"Texto decodificado: $textoDecodificado4")

  // Convertir la cadena a lista de caracteres
  val listaChars4 = cadenaAListaChars(textoOriginal4)
  println(s"Lista de caracteres de '$textoOriginal4': $listaChars4")

  // Convertir la lista de caracteres de nuevo a cadena
  val cadenaDesdeLista4 = listaCharsACadena(listaChars4)
  println(s"Cadena convertida desde la lista de caracteres: $cadenaDesdeLista4")

  // Probar la creación de la lista de distribución de frecuencias
  val frecuencias4 = ListaCharsADistFrec(listaChars4)
  println(s"Frecuencias de caracteres: $frecuencias4")

  // Probar la conversión de frecuencias a hojas
  val listaHojas4 = DistribFrecAListaHojas(frecuencias4)
  println(s"Lista de hojas según frecuencias: $listaHojas4")

  // Comprobar la combinación de nodos
  val nodosCombinados4 = combinar(listaHojas4)
  println(s"Nodos combinados: $nodosCombinados4")

  // Comprobar si la lista de nodos tiene un solo elemento
  println(s"La lista de nodos combinados tiene un único elemento: ${esListaSingleton(nodosCombinados4)}")

  // Mostrar el peso del árbol
  println(s"Peso del árbol de Huffman: ${arbolHuffman4.pesoArbol}")
}