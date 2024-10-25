abstract class ArbolHuff {
  def pesoArbol: Int = this match
    case HojaHuff(_, p) => p
    case RamaHuff(nodoIzq, nodoDcha) => nodoIzq.pesoArbol + nodoDcha.pesoArbol
}    
    
case class RamaHuff(nodoIzq:ArbolHuff, nodoDcha: ArbolHuff) extends ArbolHuff
case class HojaHuff(caracter:Char, peso: Int) extends ArbolHuff
