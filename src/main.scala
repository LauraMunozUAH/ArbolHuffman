import ArbolHuff.given 
import RamaHuff.given 
import HojaHuff.given 
@main
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
}  