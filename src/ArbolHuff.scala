abstract class ArbolHuff {
  def pesoArbol: Int = this match
    case HojaHuff(_, p) => p
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.pesoArbol + nodoDcha.pesoArbol

  //Función que devuelve la lista de caracteres de un ArbolHuff
  def caracteres:List[Char]= this match
    case HojaHuff(c, _) => List(c)
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.caracteres ++ nodoDcha.caracteres
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