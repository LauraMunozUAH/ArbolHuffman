import scala.annotation.tailrec

abstract class ArbolHuff {
  def pesoArbol: Int = this match
    case HojaHuff(_, p) => p
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.pesoArbol + nodoDcha.pesoArbol

  //Función que devuelve la lista de caracteres de un ArbolHuff
  def caracteres:List[Char]= this match
    case HojaHuff(c, _) => List(c)
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.caracteres ++ nodoDcha.caracteres

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

  type Bit = 0 | 1 //Creación del tipo de dato bit

  // Función que a partir de una lista de bits devuelve el texto
  def decodificar (bits: List[Bit]): String =
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
}

case class RamaHuff(nodoIzq:ArbolHuff, nodoDcha: ArbolHuff) extends ArbolHuff
case class HojaHuff(caracter:Char, peso: Int) extends ArbolHuff

@main
def main():Unit=
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
  val ramaLO = RamaHuff(hojaL, hojaO)
  val ramaUX = RamaHuff(hojaU, hojaX)
  val ramaPR = RamaHuff(hojaP, hojaR)

  val ramaLRUX = RamaHuff(ramaLO, ramaUX)
  val ramaPRF = RamaHuff(ramaPR, hojaF)

  val ramaLRUXPRF = RamaHuff(ramaLRUX, ramaPRF)

  val ramaHT = RamaHuff(hojaH, hojaT)
  val ramaMN = RamaHuff(hojaM, hojaN)
  val ramaSI = RamaHuff(hojaS, hojaI)

  val ramaHTMN = RamaHuff(ramaHT, ramaMN)
  val ramaSIEA = RamaHuff(ramaSI, hojaE)

  val ramaA = RamaHuff(ramaSIEA, hojaA)
  val ramaESPACIO = hojaEspacio

  //Arbol
  val arbolHuffman: ArbolHuff = RamaHuff(ramaHTMN, ramaLRUXPRF)
  println(arbolHuffman.pesoArbol)
  val arbolHuff2: ArbolHuff= RamaHuff(HojaHuff('s', 4), RamaHuff(HojaHuff('o', 3), RamaHuff(HojaHuff('e', 2), HojaHuff(' ', 2))))
  println(arbolHuff2.pesoArbol)
  println(arbolHuff2.caracteres.mkString(", "))
  println(arbolHuff2.decodificar(List(0,1,0,1,1,0)))