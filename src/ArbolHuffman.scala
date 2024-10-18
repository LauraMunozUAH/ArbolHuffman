abstract class ArbolHuffman {
    case class RamaHuff(nodoIzq:ArbolHuffman{}, nododDerch: ArbolHuffman{}, caracteresList: List[Char], peso: Int) extends ArbolHuffman   
    case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman
}
