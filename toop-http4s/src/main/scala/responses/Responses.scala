package responses

case class Reduced(reduced: String, history: Seq[String])

case class ReduceError(error: String, history: Seq[String])
