import org.codehaus.jparsec.Parser
import org.codehaus.jparsec.Parsers
import org.codehaus.jparsec.Scanners
import org.codehaus.jparsec.pattern.Patterns

sealed class Expr {
    object Nil : Expr()
    data class Bool(val value: Boolean) : Expr()
    data class Num(val value: Long) : Expr()
    data class Str(val value: String) : Expr()
    data class Sym(val value: String) : Expr()
    data class Quote(val expr: Expr) : Expr()
    data class Unquote(val expr: Expr) : Expr()
    data class QuasiQuote(val expr: Expr) : Expr()
    data class SpliceUnquote(val expr: Expr) : Expr()
    data class Deref(val expr: Expr) : Expr()
    data class Metadata(val expr1: Expr, val expr2: Expr) : Expr()
    data class SExpr(val exprs: List<Expr>) : Expr()
    data class Vec(val exprs: List<Expr>) : Expr()
    data class HashMap(val exprs: List<Expr>) : Expr()
}

val parser: Parser<Expr> = {
    val whitespace = Patterns.regex("(\\s|,|;.*$)+").toScanner("whitespace")

    val exprRef: Parser.Reference<Expr> = Parser.newReference()
    val exprPairRef: Parser.Reference<Pair<Expr, Expr>> = Parser.newReference()
    val exprSeqRef: Parser.Reference<MutableList<Expr>> = Parser.newReference()

    val expr = Parsers.or(
            Patterns.regex("-?\\d+").toScanner("number").source().map { Expr.Num(it.toLong()) },
            Scanners.DOUBLE_QUOTE_STRING.map { Expr.Str(it) },
            Scanners.string("nil").map { Expr.Nil },
            Patterns.regex("true|false").toScanner("boolean").source().map { Expr.Bool(it.toBoolean()) },
            Parsers.sequence(Scanners.string("~@"), exprRef.lazy()).map { Expr.SpliceUnquote(it) },
            Parsers.sequence(Scanners.isChar('\''), exprRef.lazy()).map { Expr.Quote(it) },
            Parsers.sequence(Scanners.isChar('~'), exprRef.lazy()).map { Expr.Unquote(it) },
            Parsers.sequence(Scanners.isChar('`'), exprRef.lazy()).map { Expr.QuasiQuote(it) },
            Parsers.sequence(Scanners.isChar('@'), exprRef.lazy()).map { Expr.Deref(it) },
            Parsers.sequence(Scanners.isChar('^'), exprPairRef.lazy()).map { Expr.Metadata(it.component2(), it.component1()) },
            Parsers.between(
                    Scanners.isChar('(').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar(')'))).map { Expr.SExpr(it) },
            Parsers.between(
                    Scanners.isChar('[').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar(']'))).map { Expr.Vec(it) },
            Parsers.between(
                    Scanners.isChar('{').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar('}'))).map { Expr.HashMap(it) },
            Patterns.regex("[^\\s\\[\\]{}('\"`,;)@]+").toScanner("symbol").source().map { Expr.Sym(it) }
    )

    exprRef.set(expr)
    exprPairRef.set(Parsers.list(listOf(expr, whitespace, expr))
            .cast<MutableList<Expr>>()
            .map { Pair(it[0], it[2]) })
    exprSeqRef.set(expr.sepBy(whitespace))

    Parsers.between(whitespace.many(), expr, whitespace.many())
}()

fun parse(line: String): Expr =
        parser.parse(line)

fun show(expr: Expr): String =
        when (expr) {
            is Expr.Nil -> "nil"
            is Expr.Bool -> expr.value.toString()
            is Expr.Num -> expr.value.toString()
            is Expr.Str -> expr.value.toString()
            is Expr.Sym -> expr.value.toString()
            is Expr.Quote -> "(quote ${show(expr.expr)})"
            is Expr.Unquote -> "(unquote ${show(expr.expr)})"
            is Expr.QuasiQuote -> "(quasiquote ${show(expr.expr)})"
            is Expr.SpliceUnquote -> "(splice-unquote ${show(expr.expr)})"
            is Expr.Deref -> "(deref ${show(expr.expr)})"
            is Expr.Metadata -> "(with-meta ${show(expr.expr1)} ${show(expr.expr2)})"
            is Expr.SExpr -> expr.exprs
                    .map { show(it) }
                    .joinToString(prefix = "(", separator = " ", postfix = ")")
            is Expr.Vec -> expr.exprs
                    .map { show(it) }
                    .joinToString(prefix = "[", separator = " ", postfix = "]")
            is Expr.HashMap -> expr.exprs
                    .map { show(it) }
                    .joinToString(prefix = "{", separator = " ", postfix = "}")
        }

class EvalException(msg: String) : RuntimeException(msg)

fun eval(expr: Expr): Expr =
        when (expr) {
            is Expr.Nil -> expr
            is Expr.Bool -> expr
            is Expr.Num -> expr
            is Expr.Str -> expr

            is Expr.Sym -> evalSym(expr)

            is Expr.Quote -> throw EvalException("Cannot eval Quote")

            is Expr.Unquote -> throw EvalException("Cannot eval Unquote")

            is Expr.QuasiQuote -> throw EvalException("Cannot eval QuasiQuote")

            is Expr.SpliceUnquote -> throw EvalException("Cannot eval SpliceUnquote")

            is Expr.Deref -> throw EvalException("Cannot eval Deref")

            is Expr.Metadata -> throw EvalException("Cannot eval Metadata")

            is Expr.SExpr -> evalSExpr(expr)

            is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(it) })

            is Expr.HashMap -> Expr.HashMap(expr.exprs.map { eval(it) })
        }

fun evalSym(sym: Expr.Sym): Expr =
        if (sym.value.startsWith(':'))
            sym
        else
            throw EvalException("Undefined symbol ${sym.value}")

fun evalSExpr(expr: Expr.SExpr): Expr {
    if (expr.exprs.isEmpty()) {
        return expr
    }

    val func = expr.exprs[0] as? Expr.Sym
            ?: throw EvalException("First element of s-expr should be a Sym")

    val args = expr.exprs.drop(1)

    return when (func.value) {
        "+" -> Expr.Num(args.fold(0L, { acc, expr -> acc + (eval(expr) as Expr.Num).value }))
        "-" -> {
            if (args.isEmpty())
                throw EvalException("${args.size} args passed to -")
            val first = eval(args[0]) as Expr.Num
            val rest = args.drop(1)
            Expr.Num(rest.fold(
                    first.value,
                    { acc, expr -> acc - (eval(expr) as Expr.Num).value }))
        }
        "*" -> Expr.Num(args.fold(1L, { acc, expr -> acc * (eval(expr) as Expr.Num).value }))
        "/" -> {
            if (args.size < 2)
                throw EvalException("${args.size} args passed to /")
            val first = eval(args[0]) as Expr.Num
            val rest = args.drop(1)
            Expr.Num(rest.fold(
                    first.value,
                    { acc, expr -> acc / (eval(expr) as Expr.Num).value }))
        }
        else -> throw EvalException("Undefined symbol ${func.value}")
    }
}