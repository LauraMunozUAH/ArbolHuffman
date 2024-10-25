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
