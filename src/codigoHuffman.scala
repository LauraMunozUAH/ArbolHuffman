import scala.annotation.tailrec

trait codigoHuffman {
  //Clase abstracta de ArbolHuffman
  abstract class ArbolHuff

  //clases case que extienden de ArbolHuff
  case class RamaHuff(nodoIzq: ArbolHuff, nododDerch: ArbolHuff, caracteresList: List[Char], peso: Int) extends ArbolHuff
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

}
