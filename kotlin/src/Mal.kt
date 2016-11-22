import org.apache.commons.lang3.StringEscapeUtils
import org.codehaus.jparsec.Parser
import org.codehaus.jparsec.Parsers
import org.codehaus.jparsec.Scanners
import org.codehaus.jparsec.error.ParserException
import org.codehaus.jparsec.pattern.Patterns
import java.util.*

interface Seq {
    val exprs: List<Expr>
}

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
    data class HashMap(val exprs: kotlin.collections.List<Expr>) : Expr()
    data class Fn(val argNames: kotlin.collections.List<String>,
                  val varArgName: String?,
                  val body: Expr,
                  val env: Env) : Expr()

    class List(override val exprs: kotlin.collections.List<Expr>) : Expr(), Seq {
        override fun equals(other: Any?): Boolean =
                (other is Seq) && (this.exprs == other.exprs)

        override fun hashCode(): Int =
                Objects.hash(exprs)
    }

    class Vec(override val exprs: kotlin.collections.List<Expr>) : Expr(), Seq {
        override fun equals(other: Any?): Boolean =
                (other is Seq) && (this.exprs == other.exprs)

        override fun hashCode(): Int =
                Objects.hash(exprs)
    }
}

class Env {
    private val scopes: Stack<MutableMap<String, Expr>>

    constructor() {
        scopes = Stack<MutableMap<String, Expr>>()
        scopes.push(mutableMapOf())
    }

    constructor(other: Env) {
        scopes = Stack()
        other.scopes.forEach { otherScope ->
            val scope = mutableMapOf<String, Expr>()
            scope.putAll(otherScope)
            scopes.push(scope)
        }
    }

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
            Scanners.DOUBLE_QUOTE_STRING.map {
                Expr.Str(StringEscapeUtils.unescapeJava(it.substring(1, it.length - 1)))
            },
            Scanners.string("nil").map { Expr.Nil },
            Scanners.string("true").map { Expr.Bool(true) },
            Scanners.string("false").map { Expr.Bool(false) },
            Parsers.sequence(Scanners.string("~@"), exprRef.lazy()).map { Expr.SpliceUnquote(it) },
            Parsers.sequence(Scanners.isChar('\''), exprRef.lazy()).map { Expr.Quote(it) },
            Parsers.sequence(Scanners.isChar('~'), exprRef.lazy()).map { Expr.Unquote(it) },
            Parsers.sequence(Scanners.isChar('`'), exprRef.lazy()).map { Expr.QuasiQuote(it) },
            Parsers.sequence(Scanners.isChar('@'), exprRef.lazy()).map { Expr.Deref(it) },
            Parsers.sequence(Scanners.isChar('^'), exprPairRef.lazy()).map { (meta, expr) -> Expr.Metadata(expr, meta) },
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
            .cast<List<Expr>>()
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
            is Expr.Str -> '"' + StringEscapeUtils.escapeJava(expr.value) + '"'
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
            is Expr.Fn ->
                "#<function>"
        }

fun prStr(expr: Expr, printReadably: Boolean = false): String =
        if (printReadably)
            show(expr)
        else
            when (expr) {
                is Expr.Str ->
                    expr.value
                is Expr.List -> expr.exprs
                        .map { prStr(it) }
                        .joinToString(prefix = "(", separator = " ", postfix = ")")
                is Expr.Vec -> expr.exprs
                        .map { prStr(it) }
                        .joinToString(prefix = "[", separator = " ", postfix = "]")
                else ->
                    show(expr)
            }

class EvalException(msg: String) : RuntimeException(msg)

class Evaluator {
    var env: Env = Env()

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
                is Expr.List -> evalList(expr)
                is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(it) })
                is Expr.HashMap -> Expr.HashMap(expr.exprs.map { eval(it) })
                is Expr.Fn -> expr
            }

    fun evalSym(sym: Expr.Sym): Expr =
            if (sym.value.startsWith(':'))
                sym
            else
                env.get(sym.value) ?: throw EvalException("Undefined symbol ${sym.value}")

    fun evalList(expr: Expr.List): Expr {
        if (expr.exprs.isEmpty()) {
            return expr
        }

        val firstExpr = expr.exprs.first()
        val first = when (firstExpr) {
            is Expr.Sym -> firstExpr
            else -> eval(firstExpr)
        }
        val args = expr.exprs.drop(1)

        return when (first) {
            is Expr.Sym ->
                when (first.value) {
                    "+" ->
                        evalNumOp(first.value, { acc, n -> acc + n }, 1, args)
                    "-" ->
                        evalNumOp(first.value, { acc, n -> acc - n }, 2, args)
                    "*" ->
                        evalNumOp(first.value, { acc, n -> acc * n }, 1, args)
                    "/" ->
                        evalNumOp(first.value, { acc, n -> acc / n }, 2, args)
                    "=" ->
                        evalEq(args)
                    "not" ->
                        evalNot(args)
                    ">" ->
                        evalComp({ n1: Long, n2: Long -> n1 > n2 }, args)
                    ">=" ->
                        evalComp({ n1: Long, n2: Long -> n1 >= n2 }, args)
                    "<" ->
                        evalComp({ n1: Long, n2: Long -> n1 < n2 }, args)
                    "<=" ->
                        evalComp({ n1: Long, n2: Long -> n1 <= n2 }, args)
                    "list" ->
                        Expr.List(args.map { eval(it) })
                    "list?" ->
                        Expr.Bool(args.all { eval(it) is Expr.List })
                    "empty?" ->
                        evalEmpty(args)
                    "count" ->
                        evalCount(args)
                    "pr-str" ->
                        Expr.Str(args.map { prStr(eval(it), true) }.joinToString(" "))
                    "str" ->
                        Expr.Str(args.map { prStr(eval(it)) }.joinToString(""))
                    "prn" -> {
                        println(args.map { prStr(eval(it), true) }.joinToString(" "))
                        Expr.Nil
                    }
                    "println" ->
                        evalPrintln(args)
                    "def!" ->
                        evalDef(args)
                    "let*" ->
                        evalLet(args)
                    "if" ->
                        evalIf(args)
                    "fn*" ->
                        evalFn(args)
                    "do" ->
                        args.fold(Expr.Nil as Expr, { acc, expr -> eval(expr) })
                    else -> {
                        val fnExpr = env.get(first.value) ?: throw EvalException("${first.value} is not a function")
                        applyFn(eval(fnExpr), args)
                    }
                }
            else ->
                applyFn(first, args)
        }
    }

    private fun evalNumOp(op: String, f: (Long, Long) -> Long, minArgs: Int, args: List<Expr>): Expr {
        if (args.size < minArgs)
            throw EvalException("${args.size} args passed to $op but expected $minArgs or more")
        val evaledArgs = args.map {
            val evaled = eval(it) as? Expr.Num
                    ?: throw EvalException("$op expects Num arguments, got $it")
            evaled.value
        }
        val first = evaledArgs[0]
        val rest = evaledArgs.drop(1)
        return Expr.Num(rest.fold(first, f))
    }

    private fun evalEq(args: List<Expr>): Expr = when {
        args.isEmpty() ->
            Expr.Bool(true)
        else -> {
            val first = eval(args.first())
            Expr.Bool(args.drop(1).all { eval(it) == first })
        }
    }

    private fun evalComp(compare: (Long, Long) -> Boolean, args: List<Expr>): Expr {
        if (args.size < 2)
            throw EvalException("Comparison expects > 2 arguments")
        val nums = args.map {
            val num = eval(it) as? Expr.Num
                    ?: throw EvalException("comparison expects Num arguments")
            num.value
        }
        return Expr.Bool(
                nums.drop(1)
                        .fold(Pair(true, nums.first()), { (acc, prev), n -> Pair(acc && compare(prev, n), n) })
                        .component1()
        )
    }

    private fun evalNot(args: List<Expr>): Expr {
        if (args.size != 1)
            throw EvalException("not takes 1 argument")
        val evaled = eval(args.first())
        return when (evaled) {
            is Expr.Nil -> Expr.Bool(true)
            is Expr.Bool -> Expr.Bool(!evaled.value)
            else -> Expr.Bool(false)
        }
    }

    private fun evalEmpty(args: List<Expr>): Expr =
            Expr.Bool(args.all {
                val value = eval(it)
                when (value) {
                    is Seq -> value.exprs.isEmpty()
                    else -> throw EvalException("empty? expects List or Vec arguments")
                }
            })

    private fun evalCount(args: List<Expr>): Expr =
            if (args.size != 1)
                throw EvalException("count expects one list/nil argument")
            else {
                val value = eval(args.first())
                when (value) {
                    is Expr.Nil -> Expr.Num(0)
                    is Seq -> Expr.Num(value.exprs.size.toLong())
                    else -> throw EvalException("count expects one List/Nil argument")
                }
            }

    private fun evalPrintln(args: List<Expr>): Expr {
        println(args.map { prStr(eval(it)) }.joinToString(" "))
        return Expr.Nil
    }

    private fun evalDef(args: List<Expr>): Expr {
        if (args.size != 2)
            throw EvalException("${args.size} args passed to def!, expected 2")

        val name = args[0]
        val value = args[1]

        if (name !is Expr.Sym)
            throw EvalException("First arg to def! must be Sym")

        val result = eval(value)

        if (result is Expr.Fn)
            result.env.set(name.value, result)

        env.set(name.value, result)

        return result
    }

    private fun evalLet(args: List<Expr>): Expr {
        if (args.size != 2)
            throw EvalException("${args.size} args passed to let*, expected 2")
        val firstArg = args[0]
        val bindingsList = when (firstArg) {
            is Seq -> firstArg.exprs
            else -> throw EvalException("First argument to let* must be List or Vec")
        }
        if ((bindingsList.size % 2) != 0)
            throw EvalException("let* bindings list must have even # of elements")

        val names = bindingsList.filterIndexed({ n, expr -> n % 2 == 0 })
        val values = bindingsList.filterIndexed({ n, expr -> n % 2 == 1 })
        val bindings = names.zip(values)

        env.pushScope()
        bindings.forEach { (nameExpr, valueExpr) ->
            val name = nameExpr as? Expr.Sym
                    ?: throw EvalException("let must bind to a Sym")
            env.set(name.value, eval(valueExpr))
        }
        val result = eval(args[1])
        env.popScope()
        return result
    }

    private fun evalIf(args: List<Expr>): Expr {
        if (args.size < 2 || args.size > 3)
            throw EvalException("${args.size} args passed to if, expected 2 or 3")
        val condResult = eval(args[0])
        val cond = when (condResult) {
            is Expr.Nil -> false
            is Expr.Bool -> condResult.value
            else -> true
        }
        return when {
            cond == true -> eval(args[1])
            args.size == 3 -> eval(args[2])
            else -> Expr.Nil
        }
    }

    private fun evalFn(args: List<Expr>): Expr {
        if (args.size != 2)
            throw EvalException("fn* takes two arguments")
        val argList = args[0] as? Seq
                ?: throw EvalException("First argument to fn* must be a List or Vec")
        val argNames = argList.exprs.map {
            when (it) {
                is Expr.Sym -> it.value
                else -> throw EvalException("fn* argument list must contain only symbols")
            }
        }
        val isVariadic = argNames.any { it == "&" }
        val normalArgNames = argNames.takeWhile { it != "&" }
        val varArgName =
                if (isVariadic) {
                    if (argNames.count { it == "&" } > 1)
                        throw EvalException("Only one & allowed in args list.")
                    argNames.takeLastWhile { it != "&" }.singleOrNull()
                            ?: throw EvalException("One arg required after & in args list")
                } else {
                    null
                }
        val body = args[1]
        val funcEnv = Env(env)
        return Expr.Fn(normalArgNames, varArgName, body, funcEnv)
    }

    private fun applyFn(expr: Expr, args: List<Expr>): Expr {
        val fn = expr as? Expr.Fn
                ?: throw EvalException(" is not a function")
        val argList = buildArgsList(fn.argNames, fn.varArgName, args.map { eval(it) })
        val oldEnv = env
        this.env = Env(fn.env)
        argList.forEach { (argName, argVal) -> env.set(argName, argVal) }
        val funcResult = eval(fn.body)
        this.env = oldEnv
        return funcResult
    }
}

fun buildArgsList(argNames: List<String>,
                  varArgName: String?,
                  args: List<Expr>): List<Pair<String, Expr>> =
        if (varArgName != null) {
            if (args.size < argNames.size)
                throw EvalException("Expected ${argNames.size} or more args, got ${args.size}")
            val argsList = argNames.zip(args).toMutableList()
            val varArgList = args.subList(argNames.size, args.size)
            argsList.add(Pair(varArgName, Expr.List(varArgList)))
            argsList
        } else {
            if (args.size != argNames.size)
                throw EvalException("Expected ${argNames.size} args, got ${args.size}")
            argNames.zip(args)
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
