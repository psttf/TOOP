package models

final case class EvalSuccess(termString: String)
final case class EvalFailure(errorMessage: String)
