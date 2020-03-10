package spp.lexer

import spp.structure.Tokens.Token
import spp.utils.Context
import spp.structure.Tokens._

class TokensCleaner(val tokens: List[Token], implicit val ctx: Context) {
  /**
    * Returns a list of token ready for parsing
    *
    * @return resulting tokens
    */
  def process(): List[Token] = {
    val (init, eof) = tokens.splitAt(tokens.size - 1)
    
    val res = TokensCleaner(init)
      .clean()
      .checkFirstIndent()
      .fixImplicitLineJoin()
      .fixIndent()
      .checkErrorTokens()
      .tokens
    
    res ++ eof
  }

  // Verify that the first statement is not indented
  def checkFirstIndent(): TokensCleaner = {
    val res = tokens match {
      case t@Space() :: tail => ctx.reporter.fatal("Unexpected indent")
      case t@PhysicalIndent(l) :: tail =>
        if (l > 0) ctx.reporter.fatal("Unexpected indent")
        else tail
      case _ => tokens
    }
    TokensCleaner(res)
  }
  
  // Remove useless tokens and make sure that each line has a newline token
  def clean() = {
    val filtered = tokens.filter {
      case Space() | Comment() => false
      case _ => true
    }

    // Make sure the last token is a physical new line with indent 0
    val fixedLastToken = filtered.last match {
      case PhysicalIndent(_) => filtered.updated(filtered.size - 1, PhysicalIndent(0))
      case _ => filtered :+ PhysicalIndent(0)
    }
    
    // If the file contains no statement, no newline token is needed
    TokensCleaner(
      if (fixedLastToken.size <= 1) Nil
      else fixedLastToken
    )
  }

  // Removing line breaks that are placed inside parenthesis, curly braces or square brackets
  def fixImplicitLineJoin() = {
    val (stack, res) = tokens.foldLeft(0, List[Token]()) {
      case ((depth, acc), t@Delimiter(del)) =>
        if ("({[".contains(del)) (depth + 1, t :: acc)
        else if (")}]".contains(del)) (depth - 1, t :: acc)
        else (depth, t :: acc)
      case ((depth, acc), t@PhysicalIndent(_)) =>
        if (depth > 0) (depth, acc)
        else (depth, t :: acc)
      case ((depth, acc), t) => (depth, t :: acc)
    }
    TokensCleaner(res.reverse)
  }

  // Transforms counts of indentation spaces into INDENT, DEDENT and NEWLINE
  def fixIndent() = {
    val (stack, resTokens) = tokens.foldLeft(List(0), List[Token]()) {
      case ((stack, acc), token@PhysicalIndent(lvl)) =>
        if (lvl > stack.head)
          (lvl :: stack, Indent() :: Newline().setPos(token.position) :: acc)
          else {
            val updatedStack = stack.dropWhile(lvl < _)
            val nDedent = stack.length - updatedStack.length
            if (updatedStack.head == lvl) {
              (updatedStack, (List.fill(nDedent)(Dedent()) ::: List(Newline().setPos(token.position))) ::: acc)
            }
          else ctx.reporter.fatal("Incorrect indentation at" + token.position)
        }
        case ((stack, acc), token) => (stack, token :: acc)
      }
    TokensCleaner(resTokens.reverse)
  }
  
  // Throws an error if there is any error token
  def checkErrorTokens() = {
    TokensCleaner(tokens.map {
      case t@ErrorToken(msg) => ctx.reporter.fatal("Invalid token at " + t.position )
      case t => t
    })
  }
}

object TokensCleaner {
  def apply(tokens: List[Token])(implicit ctx: Context) = {
    new TokensCleaner(tokens, ctx)
  }
}