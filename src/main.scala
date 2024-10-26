import scala.annotation.tailrec

/*
  // Convierte la lista de caracteres en distribución de frecuencias.
def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
  @tailrec
  def frecuenciasAux(frecuencias: List[(Char, Int)], char: Char): List[(Char, Int)] = frecuencias match
    case Nil => List((char, 1)) //añade 1 a la cuenta
    case (c, i) :: tail if c == char => (c, i + 1) :: tail // incrementamos 1 la frecuencia
    case head :: tail => head :: frecuenciasAux(tail, char) // continua contando

  // recorremos la lista y acumulamos las frecuencias
  listaChar.foldLeft(List[(Char, Int)]()) { (frecuencias, char) => frecuenciasAux(frecuencias, char)

    // Convierte la lista de caracteres en distribución de frecuencias.
    def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
      @tailrec
      def ListaCharsAux(frecuencias: List[(Char,Int)], resto:List[Char]):List[(Char,Int)] = resto match
        case Nil => frecuencias
        case h::tail =>
          //recorre la lista de frecuencias y devuelve la lista de frecuencias act y si el caracter esta en la lista
          val updated = frecuencias.foldLeft((List[(Char, Int)](), false)) {
            case ((acc, encontrado), (c, f)) =>
              if (c == char) ((c, f + 1) :: acc, true) else ((c, f) :: acc, encontrado) //se actualiza solo si c==char
          }
          val (nuevafrec, encontrado) = updated  //resultados de foldleft
          if (!encontrado) ListaCharsAux((char,1) :: nuevafrec, resto)) //agrega char a frecuencias
    else ListaCharsAux(nuevafrec, resto)
    ListaCharsAux(Nil,listaChar)
  */