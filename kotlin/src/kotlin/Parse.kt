import org.jparsec.Parser
import org.jparsec.Parsers
import org.jparsec.Parsers.*
import org.jparsec.Scanners.*
import org.jparsec.Terminals
import org.jparsec.Token
import org.jparsec.pattern.Patterns

object Parse {
    val whitespace: Parser<Void> = or(
            isChar(Char::isWhitespace),
            isChar(','),
            isChar(';').followedBy(notChar('\n').many())
    ).many().cast()

    val symDisallowedChars =
            "\\s,;\"'`~@\\^\\[\\]{}()"
    val symChar: Parser<Void> =
            Patterns.regex("[^$symDisallowedChars]").toScanner("symChar")

    val nil: Parser<Expr.Nil> =
            string("nil").notFollowedBy(symChar).map { Expr.Nil }
    val boolTrue: Parser<Expr.Bool> =
            string("true").notFollowedBy(symChar).map { Expr.Bool(true) }
    val boolFalse: Parser<Expr.Bool> =
            string("false").notFollowedBy(symChar).map { Expr.Bool(false) }

    val symbol: Parser<Expr.Sym> = Parsers.or(
            // Need several cases to parse things like "-" and "->" but not accidentally parse negative numbers
            Patterns.regex("[^\\-\\d$symDisallowedChars][^$symDisallowedChars]*").toScanner("symbol"),
            Patterns.regex("-[^\\d$symDisallowedChars][^$symDisallowedChars]*").toScanner("symbol"),
            string("-").notFollowedBy(Terminals.IntegerLiteral.TOKENIZER)
    ).source().map { Expr.Sym(it) }

    val number: Parser<Expr.Num> =
            Patterns.regex("-?\\d+")
                    .toScanner("number").source()
                    .map { Expr.Num(it.toLong()) }

    val terminals: Terminals = Terminals
            .operators("'", "`", "~@", "~", "@", "^", "(", ")", "[", "]", "{", "}")

    val tokenizer: Parser<Any> = Parsers.or(
            terminals.tokenizer(),
            nil,
            boolTrue,
            boolFalse,
            symbol,
            number,
            Terminals.StringLiteral.DOUBLE_QUOTE_TOKENIZER)

    fun token(s: String): Parser<Token> =
            terminals.token(s)

    val exprRef: Parser.Reference<Expr> =
            Parser.newReference()
    val exprLazy: Parser<Expr> =
            exprRef.lazy()

    val expr: Parser<Expr> = Parsers.or(
            Terminals.StringLiteral.PARSER.map { Expr.Str(it) },
            tokenType(Expr.Nil::class.java, "nil"),
            tokenType(Expr.Bool::class.java, "boolean"),
            tokenType(Expr.Num::class.java, "integer"),
            tokenType(Expr.Sym::class.java, "symbol"),
            sequence(token("'"), exprLazy).map { Expr.List(listOf(Expr.Sym("quote")) + it) },
            sequence(token("`"), exprLazy).map { Expr.List(listOf(Expr.Sym("quasiquote")) + it) },
            sequence(token("~@"), exprLazy).map { Expr.List(listOf(Expr.Sym("splice-unquote")) + it) },
            sequence(token("~"), exprLazy).map { Expr.List(listOf(Expr.Sym("unquote")) + it) },
            sequence(token("@"), exprLazy).map { Expr.List(listOf(Expr.Sym("deref")) + it) },
            sequence(token("^"), exprLazy, exprLazy, { _, meta, expr -> Expr.WithMeta(expr, meta) }),
            between(token("("), exprLazy.many(), token(")")).map { Expr.List(it) },
            between(token("["), exprLazy.many(), token("]")).map { Expr.Vec(it) },
            between(token("{"), exprLazy.many(), token("}")).map { Expr.List(listOf(Expr.Sym("hash-map")) + it) })

    val parser: Parser<Expr> by lazy {
        exprRef.set(expr)
        expr.from(tokenizer, whitespace)
    }

    fun parse(str: String): Expr =
            parser.parse(str)

}
