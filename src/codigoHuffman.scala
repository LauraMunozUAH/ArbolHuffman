import scala.annotation.tailrec

trait codigoHuffman {
  //Clase abstracta de ArbolHuffman
  abstract class ArbolHuff

  //clases case que extienden de ArbolHuff
  case class RamaHuff(nodoIzq: ArbolHuff, nodoDcha: ArbolHuff, caracteresList: List[Char], peso: Int) extends ArbolHuff
  case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuff

  //Devuelve el peso de un ArbolHuf
  def peso(arbol: ArbolHuff): Int = arbol match
    case RamaHuff(_, _, _, p) => p //En el caso de que el arbol sea una rama
    case HojaHuff(_, p) => p       //En el caso de que el arbol sea una hoja

  //Función que devuelve la lista de caracteres de un árbol Huffman
  def caracteres(arbol: ArbolHuff): List[Char] = arbol match
    case RamaHuff(_, _, chars, _) => chars // Si es una rama, devolvemos los caracteres de la rama
    case HojaHuff(c, _) => List(c) // Si es una hoja, devolvemos el carácter como una lista

  //Función que pasa de cadena de texto a lista de caracteres
  def cadenaAListaChars(cadena: String): List[Char] =
    @tailrec
    def cAlCharsAux(posicion:Int, listaAux: List[Char]): List[Char] = posicion match
      case p if p >= cadena.length => listaAux.reverse //caso base, hemos llegado al final de la cadena
      case _ => cAlCharsAux(posicion+1, cadena.charAt(posicion)::listaAux) // Agregamos el carácter a la listaAux y continuamos con la recursividad
    cAlCharsAux(0, Nil)

  def listaCharsACadena(listaCar: List[Char]): String =
    @tailrec
    def lCharsACAux(listaCar: List[Char], cadena: String): String = listaCar match
      case Nil => cadena // Cuando la lista está vacía, devuelve el resultado acumulado como String
      case head :: tail => lCharsACAux(tail, cadena+head) // Añadimos el carácter al StringBuilder y continuamos
    lCharsACAux(listaCar, "")

  type Bit = 0|1 //Creación del tipo de dato bit

  //Función que a partir de una lista de bits devuelve el texto
  def decodificar(arbol: ArbolHuff)(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(subArbol: ArbolHuff, restobits: List[Bit], resultado: List[Char]): String = (subArbol, restobits) match
      //Caso hoja, añadir caracter y empezar de nuevo
      case (HojaHuff(caracter, _), _) => decodificarAux(arbol, restobits, caracter :: resultado)
      //Caso rama, depende del bit 0 izq 1dcha
      case (RamaHuff(nodoIzq, _, _, _), 0 :: tail) => decodificarAux(nodoIzq, tail, resultado)
      case (RamaHuff(_, nodoDcha, _, _), 1 :: tail) => decodificarAux(nodoDcha, tail, resultado)
      //No quedan bits
      case _ => listaCharsACadena(resultado.reverse) //Invertimos y convertimos a cadena de texto

    decodificarAux(arbol,bits,Nil)

  //Determina si el árbol contiene o no un caracter
  def arbolcontiene(arbol:ArbolHuff, char:Char):Boolean = arbol match
    case HojaHuff(caracter,_) => caracter == char
    case RamaHuff(nodoIzq,nodoDcha,_,_) => arbolcontiene(nodoIzq,char) || arbolcontiene(nodoDcha,char)

  // Función que convierte el texto en una cadena de bits
  def procesocodificación(subArbol: ArbolHuff, char: Char): List[Bit] = subArbol match
    //Caso hoja, encontramos el caracter
    case HojaHuff(caracter, _) if caracter == char => Nil
    //Caso rama
    case RamaHuff(nodoIzq, nodoDcha, _, _) =>
      //Rama izq
      if arbolcontiene(nodoIzq, char) then 0 :: procesocodificación(nodoIzq, char) //Añade 0 delante
      else 1 :: procesocodificación(nodoDcha, char) //Añade 1 delante
    case _ => throw new IllegalArgumentException("Carácter no encontrado")

  def codificarLista(arbol: ArbolHuff, caracteres: List[Char]): List[Bit] =  //codifica una lista de caracteres
    @tailrec
    def codificarListaAux(char:List[Char],temp:List[Bit]):List[Bit] = char match
      case Nil => temp //no quedan caracteres
      case head::tail=>
        val bits = procesocodificación(arbol,head) //codificar caracter actual
        codificarListaAux(tail,temp++bits)
    codificarListaAux(caracteres,Nil)
  def codificar(arbol: ArbolHuff)(cadena: String): List[Bit] =
    //cadena a lista de caracteres
    val listacaracteres:List[Char]=cadenaAListaChars(cadena)
    codificarLista(arbol,listacaracteres)

  def main(args: Array[String]): Unit =
    //INSTANCIAR EL ARBOL
    //Hojas
    val hojaEspacio = HojaHuff(' ', 7)
    val hojaA = HojaHuff('a', 4)
    val hojaE = HojaHuff('e', 4)
    val hojaF = HojaHuff('f', 3)
    val hojaH = HojaHuff('h', 2)
    val hojaI = HojaHuff('i', 2)
    val hojaM = HojaHuff('m', 2)
    val hojaN = HojaHuff('n', 2)
    val hojaS = HojaHuff('s', 2)
    val hojaT = HojaHuff('t', 2)
    val hojaL = HojaHuff('l', 1)
    val hojaO = HojaHuff('o', 1)
    val hojaP = HojaHuff('p', 1)
    val hojaR = HojaHuff('r', 1)
    val hojaU = HojaHuff('u', 1)
    val hojaX = HojaHuff('x', 1)
    //Ramas
    val ramaLO = RamaHuff(hojaL, hojaO, List('l', 'o'), 2)
    val ramaUX = RamaHuff(hojaU, hojaX, List('u', 'x'), 2)
    val ramaPR = RamaHuff(hojaP, hojaR, List('p', 'r'), 2)

    val ramaLRUX = RamaHuff(ramaLO, ramaUX, List('l', 'o', 'u', 'x'), 4)
    val ramaPRF = RamaHuff(ramaPR, hojaF, List('p', 'r', 'f'), 5)

    val ramaLRUXPRF = RamaHuff(ramaLRUX, ramaPRF, List('l', 'o', 'u', 'x', 'p', 'r', 'f'), 9)

    val ramaHT = RamaHuff(hojaH, hojaT, List('h', 't'), 4)
    val ramaMN = RamaHuff(hojaM, hojaN, List('m', 'n'), 4)
    val ramaSI = RamaHuff(hojaS, hojaI, List('s', 'i'), 4)

    val ramaHTMN = RamaHuff(ramaHT, ramaMN, List('h', 't', 'm', 'n'), 8)
    val ramaSIEA = RamaHuff(ramaSI, hojaE, List('s', 'i', 'e'), 6)

    val ramaA = RamaHuff(ramaSIEA, hojaA, List('s', 'i', 'e', 'a'), 10)
    val ramaESPACIO = hojaEspacio

    //Arbol
    val arbolHuffman:ArbolHuff = RamaHuff(ramaHTMN, ramaLRUXPRF, List('h', 't', 'm', 'n', 'l', 'o', 'u', 'x', 'p', 'r', 'f'), 17)
    val listabits:List[Bit] = List(0,1,0,0,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,0)
    val mensaje = decodificar(arbolHuffman)(listabits)
    println(mensaje)

}
