import org.codehaus.jparsec.Parser
import org.codehaus.jparsec.Parsers
import org.codehaus.jparsec.Scanners
import org.codehaus.jparsec.error.ParserException
import org.codehaus.jparsec.pattern.Patterns
import java.util.*

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
    data class List(val exprs: kotlin.collections.List<Expr>) : Expr()
    data class Vec(val exprs: kotlin.collections.List<Expr>) : Expr()
    data class HashMap(val exprs: kotlin.collections.List<Expr>) : Expr()
}

class Env {
    private val scopes: Stack<MutableMap<String, Expr>> = {
        val stack = Stack<MutableMap<String, Expr>>()
        stack.push(mutableMapOf())
        stack
    }()

    fun get(sym: String): Expr? =
            scopes.reversed().fold(null as Expr?, { expr, scope -> expr ?: scope[sym] })

    fun set(sym: String, value: Expr) {
        scopes.peek()[sym] = value
    }

    fun pushScope() {
        scopes.push(mutableMapOf())
    }

    fun popScope() {
        scopes.pop()
    }
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
                    whitespace.many().next(Scanners.isChar(')'))).map { Expr.List(it) },
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
            is Expr.List -> expr.exprs
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

class Evaluator {
    val env: Env = Env()

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

                is Expr.List -> evalSExpr(expr)

                is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(it) })

                is Expr.HashMap -> Expr.HashMap(expr.exprs.map { eval(it) })
            }

    fun evalSym(sym: Expr.Sym): Expr =
            if (sym.value.startsWith(':'))
                sym
            else
                env.get(sym.value) ?: throw EvalException("Undefined symbol ${sym.value}")

    fun evalSExpr(expr: Expr.List): Expr {
        if (expr.exprs.isEmpty()) {
            return expr
        }

        val func = expr.exprs[0] as? Expr.Sym
                ?: throw EvalException("First element of list should be a Sym")

        val args = expr.exprs.drop(1)

        return when (func.value) {
            "+" ->
                Expr.Num(args.fold(0L, { acc, expr -> acc + (eval(expr) as Expr.Num).value }))

            "-" -> {
                if (args.isEmpty())
                    throw EvalException("${args.size} args passed to -, expected one or more")
                val first = eval(args[0]) as Expr.Num
                val rest = args.drop(1)
                Expr.Num(rest.fold(
                        first.value,
                        { acc, expr -> acc - (eval(expr) as Expr.Num).value }))
            }

            "*" ->
                Expr.Num(args.fold(1L, { acc, expr -> acc * (eval(expr) as Expr.Num).value }))

            "/" -> {
                if (args.size < 2)
                    throw EvalException("${args.size} args passed to /, expected 2 or more")
                val first = eval(args[0]) as Expr.Num
                val rest = args.drop(1)
                Expr.Num(rest.fold(
                        first.value,
                        { acc, expr -> acc / (eval(expr) as Expr.Num).value }))
            }

            "def!" -> {
                if (args.size != 2)
                    throw EvalException("${args.size} args passed to def!, expected 2")

                val name = args[0]
                val value = args[1]

                if (name !is Expr.Sym)
                    throw EvalException("First arg to def! must be Sym")

                val result = eval(value)
                env.set(name.value, result)

                result
            }

            "let*" -> {
                if (args.size != 2)
                    throw EvalException("${args.size} args passes to let*, expected 2")
                val firstArg = args[0]
                val bindingsList = when (firstArg) {
                    is Expr.List -> firstArg.exprs
                    is Expr.Vec -> firstArg.exprs
                    else -> throw EvalException("First argument to let* must be list or vec")
                }
                if ((bindingsList.size % 2) != 0)
                    throw EvalException("let* bindings list must have even # of elements")

                val names = bindingsList.filterIndexed({ n, expr -> n % 2 == 0 })
                val values = bindingsList.filterIndexed({ n, expr -> n % 2 == 1 })
                val bindings = names.zip(values)

                env.pushScope()
                bindings.forEach {
                    val name = it.component1() as? Expr.Sym
                            ?: throw EvalException("let must bind to a Sym")
                    val value = it.component2()
                    env.set(name.value, eval(value))
                }
                val result = eval(args[1])
                env.popScope()
                result
            }

            else ->
                throw EvalException("Undefined symbol ${func.value}")
        }
    }
}

fun rep() {
    val evaluator = Evaluator()

    while (true) {
        print("user> ")

        val line = readLine() ?: break
        if (line.isBlank())
            continue

        try {
            println(show(evaluator.eval(parse(line))))
        } catch (e: ParserException) {
            println("Parse error: ${e.message}")
        } catch (e: EvalException) {
            println("Eval error: ${e.message}")
        }
    }
}
