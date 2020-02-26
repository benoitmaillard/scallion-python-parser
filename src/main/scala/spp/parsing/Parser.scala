package spp.parsing

import scallion.syntactic._

import spp.utils._
import spp.structure._
import spp.structure.AbstractSyntaxTree._
import spp.parsing._

object Parser extends Pipeline[Iterator[Tokens.Token], Program] with Syntaxes {
  type Token = Tokens.Token
  type Kind = Tokens.Token
  
  def run(ctx: Context)(v: Iterator[Token]): Program = ???
  
  def getKind(token: Token): Kind = ???
}