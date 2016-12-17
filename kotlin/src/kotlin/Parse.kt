import org.apache.commons.lang3.StringEscapeUtils
import org.jparsec.Parser
import org.jparsec.Parsers
import org.jparsec.Scanners
import org.jparsec.pattern.Patterns

object Parse {
    private val whitespace = Parsers.or(
            Scanners.isChar(Char::isWhitespace),
            Scanners.isChar(','),
            Scanners.isChar(';').followedBy(Scanners.notChar('\n').many())
    ).label("whitespace")

    private val exprRef: Parser.Reference<Expr> = Parser.newReference()
    private val exprPairRef: Parser.Reference<Pair<Expr, Expr>> = Parser.newReference()
    private val exprSeqRef: Parser.Reference<MutableList<Expr>> = Parser.newReference()

    private val expr = Parsers.or(
            Patterns.regex("-?\\d+").toScanner("number").source().map { Expr.Num(it.toLong()) },
            Scanners.DOUBLE_QUOTE_STRING.map {
                Expr.Str(StringEscapeUtils.unescapeJava(it.substring(1, it.length - 1)))
            },
            Parsers.sequence(Scanners.string("~@"), exprRef.lazy())
                    .map { Expr.List(listOf(Expr.Sym("splice-unquote"), it)) },
            Parsers.sequence(Scanners.isChar('\''), exprRef.lazy())
                    .map { Expr.List(listOf(Expr.Sym("quote"), it)) },
            Parsers.sequence(Scanners.isChar('~'), exprRef.lazy())
                    .map { Expr.List(listOf(Expr.Sym("unquote"), it)) },
            Parsers.sequence(Scanners.isChar('`'), exprRef.lazy())
                    .map { Expr.List(listOf(Expr.Sym("quasiquote"), it)) },
            Parsers.sequence(Scanners.isChar('@'), exprRef.lazy())
                    .map { Expr.List(listOf(Expr.Sym("deref"), it)) },
            Parsers.sequence(Scanners.isChar('^'), exprPairRef.lazy())
                    .map { (meta, expr) -> Expr.WithMeta(expr, meta) },
            Parsers.between(
                    Scanners.isChar('(').next(whitespace.many()),
                    exprSeqRef.lazy().map { Expr.List(it) },
                    whitespace.many().next(Scanners.isChar(')'))),
            Parsers.between(
                    Scanners.isChar('[').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar(']'))).map { Expr.Vec(it) },
            Parsers.between(
                    Scanners.isChar('{').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar('}'))).map { Expr.List(listOf(Expr.Sym("hash-map")) + it) },
            Patterns.regex("[^\\s\\[\\]{}('\"`,;)@]+").toScanner("nil, bool or symbol").source()
                    .map {
                        when (it) {
                            "nil" -> Expr.Nil
                            "true" -> Expr.Bool(true)
                            "false" -> Expr.Bool(false)
                            else -> Expr.Sym(it)
                        }
                    }
    ).label("expression")

    private val parser: Parser<Expr>

    init {
        exprRef.set(expr)
        exprPairRef.set(Parsers.list(listOf(expr, whitespace.many1(), expr))
                .cast<List<Expr>>()
                .map { Pair(it[0], it[2]) })
        exprSeqRef.set(expr.sepBy(whitespace.many1()))

        parser = Parsers.between(whitespace.many(), expr, whitespace.many())
    }

    fun parse(str: String): Expr =
            parser.parse(str)
}
