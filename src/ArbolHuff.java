abstract class ArbolHuff
        // Devuelve el peso de un ArbolHuf.
    def peso: Int = this match
            case HojaHuff(_,p) => p
            case RamaHuff(nodoIzq,nodoDcha) => nodoIzq.peso + nodoDcha.peso
case class RamaHuff(nodoIzq: ArbolHuff, nodoDcha: ArbolHuff) extends ArbolHuffman
case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman
//hola