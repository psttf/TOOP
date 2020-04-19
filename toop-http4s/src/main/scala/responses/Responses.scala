package responses

case class Reduced(reduced: String, history: List[String])

case class ReduceError(error: String, history: List[String])
