import org.apache.commons.lang3.StringEscapeUtils
import org.codehaus.jparsec.Parser
import org.codehaus.jparsec.Parsers
import org.codehaus.jparsec.Scanners
import org.codehaus.jparsec.error.ParserException
import org.codehaus.jparsec.pattern.Patterns
import java.io.BufferedReader
import java.io.File
import java.io.InputStream
import java.io.InputStreamReader
import java.util.*
import java.util.stream.Collectors


val SPECIAL_FORMS = listOf(
        "quote", "quasiquote", "unquote", "splice-unquote", "deref",
        "def!", "defmacro!", "let*", "if", "fn*", "do",
        "macroexpand", "try*", "catch*", "throw"
)

interface Seq {
    val exprs: List<Expr>
}

sealed class Expr {
    data class Bool(val value: Boolean) : Expr()
    data class Num(val value: Long) : Expr()
    data class Str(val value: String) : Expr()
    data class Sym(val value: String) : Expr()
    data class Atom(var expr: Expr) : Expr()
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
                  val env: Env,
                  val isMacro: Boolean = false) : Expr()

    data class BuiltInFn(val f: (Env, kotlin.collections.List<Expr>) -> Expr) : Expr()

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

    object Nil : Expr(), Seq {
        override val exprs: kotlin.collections.List<Expr> = emptyList()
    }
}

val parser: Parser<Expr> = {
    val whitespace = Parsers.or(
            Scanners.isChar(Char::isWhitespace),
            Scanners.isChar(','),
            Scanners.isChar(';').followedBy(Scanners.notChar('\n').many())
    ).label("whitespace")

    val exprRef: Parser.Reference<Expr> = Parser.newReference()
    val exprPairRef: Parser.Reference<Pair<Expr, Expr>> = Parser.newReference()
    val exprSeqRef: Parser.Reference<MutableList<Expr>> = Parser.newReference()

    val symFollowedByExpr = { sym: String ->
        Parsers.sequence(Scanners.string(sym), whitespace.many1(), exprRef.lazy())
    }

    val listContents = Parsers.or(
            symFollowedByExpr("splice-unquote").map { Expr.SpliceUnquote(it) },
            symFollowedByExpr("quote").map { Expr.Quote(it) },
            symFollowedByExpr("unquote").map { Expr.Unquote(it) },
            symFollowedByExpr("quasiquote").map { Expr.QuasiQuote(it) },
            symFollowedByExpr("deref").map { Expr.Deref(it) },
            exprSeqRef.lazy().map { Expr.List(it) }
    )

    val expr = Parsers.or(
            Patterns.regex("-?\\d+").toScanner("number").source().map { Expr.Num(it.toLong()) },
            Scanners.DOUBLE_QUOTE_STRING.map {
                Expr.Str(StringEscapeUtils.unescapeJava(it.substring(1, it.length - 1)))
            },
            Parsers.sequence(Scanners.string("~@"), exprRef.lazy()).map { Expr.SpliceUnquote(it) },
            Parsers.sequence(Scanners.isChar('\''), exprRef.lazy()).map { Expr.Quote(it) },
            Parsers.sequence(Scanners.isChar('~'), exprRef.lazy()).map { Expr.Unquote(it) },
            Parsers.sequence(Scanners.isChar('`'), exprRef.lazy()).map { Expr.QuasiQuote(it) },
            Parsers.sequence(Scanners.isChar('@'), exprRef.lazy()).map { Expr.Deref(it) },
            Parsers.sequence(Scanners.isChar('^'), exprPairRef.lazy())
                    .map { (meta, expr) -> Expr.Metadata(expr, meta) },
            Parsers.between(
                    Scanners.isChar('(').next(whitespace.many()),
                    listContents,
                    whitespace.many().next(Scanners.isChar(')'))),
            Parsers.between(
                    Scanners.isChar('[').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar(']'))).map { Expr.Vec(it) },
            Parsers.between(
                    Scanners.isChar('{').next(whitespace.many()),
                    exprSeqRef.lazy(),
                    whitespace.many().next(Scanners.isChar('}'))).map { Expr.HashMap(it) },
            Patterns.regex("[^\\s\\[\\]{}('\"`,;)@]+").toScanner("symbol").source()
                    .map {
                        when (it) {
                            "nil" -> Expr.Nil
                            "true" -> Expr.Bool(true)
                            "false" -> Expr.Bool(false)
                            else -> Expr.Sym(it)
                        }
                    }
    ).label("expression")

    exprRef.set(expr)
    exprPairRef.set(Parsers.list(listOf(expr, whitespace.many1(), expr))
            .cast<List<Expr>>()
            .map { Pair(it[0], it[2]) })
    exprSeqRef.set(expr.sepBy(whitespace.many1()))

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
            is Expr.Sym -> expr.value
            is Expr.Atom -> "(atom ${show(expr.expr)})"
            is Expr.Quote -> "(quote ${show(expr.expr)})"
            is Expr.Unquote -> "(unquote ${show(expr.expr)})"
            is Expr.QuasiQuote -> "(quasiquote ${show(expr.expr)})"
            is Expr.SpliceUnquote -> "(splice-unquote ${show(expr.expr)})"
            is Expr.Deref -> "(deref ${show(expr.expr)})"
            is Expr.Metadata -> "(with-meta ${show(expr.expr1)} ${show(expr.expr2)})"
            is Expr.Fn, is Expr.BuiltInFn ->
                "#<function>"
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

fun prStr(expr: Expr, printReadably: Boolean = false): String =
        if (printReadably)
            show(expr)
        else when (expr) {
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

class Env {
    private val bindings: MutableMap<String, Expr>
    private val outerEnv: Env?

    constructor(bindings: Map<String, Expr>) {
        this.bindings = bindings as MutableMap<String, Expr>
        this.outerEnv = null
    }

    constructor(outerEnv: Env) {
        this.bindings = mutableMapOf()
        this.outerEnv = outerEnv
    }

    fun get(sym: String): Expr? =
            if (bindings.containsKey(sym))
                bindings[sym]
            else
                outerEnv?.get(sym)

    fun set(sym: String, value: Expr) =
            if (sym in SPECIAL_FORMS)
                throw MalException("Cannot define $sym since it is a special form")
            else if (bindings.containsKey(sym) && value == Expr.Nil)  // Bizarre, but makes tests pass
                Unit
            else
                bindings[sym] = value
}

class MalException(val value: Expr) : RuntimeException() {
    constructor(msg: String) : this(Expr.Str(msg))
}

@Suppress("NON_TAIL_RECURSIVE_CALL")  // Not all calls to eval() can be in tail pos
tailrec fun eval(env: Env, exprUnexpanded: Expr): Expr {
    val expr = macroExpand(env, exprUnexpanded)
    return when (expr) {
        is Expr.Nil, is Expr.Bool, is Expr.Num, is Expr.Str, is Expr.Atom, is Expr.Fn, is Expr.BuiltInFn ->
            expr
        is Expr.Sym -> evalSym(env, expr)
        is Expr.Quote -> expr.expr
        is Expr.QuasiQuote -> quasiquote(env, expr.expr)
        is Expr.Unquote -> eval(env, expr.expr)
        is Expr.SpliceUnquote -> throw MalException("Cannot eval SpliceUnquote in non-quasiquote context")
        is Expr.Deref -> deref(env, listOf(expr.expr))
        is Expr.Metadata -> throw MalException("Cannot eval Metadata")
        is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(env, it) })
        is Expr.HashMap -> Expr.HashMap(expr.exprs.map { eval(env, it) })
        is Expr.List -> {
            if (expr.exprs.isEmpty())
                return expr
            val first = expr.exprs.first()
            val args = expr.exprs.drop(1)
            when (first) {
                Expr.Sym("def!") ->
                    evalDef(env, args, isMacro = false)
                Expr.Sym("defmacro!") ->
                    evalDef(env, args, isMacro = true)
                Expr.Sym("let*") -> {
                    if (args.size != 2)
                        throw MalException("${args.size} args passed to let*, expected 2")
                    val firstArg = args[0]
                    val bindingsList = when (firstArg) {
                        is Seq -> firstArg.exprs
                        else -> throw MalException("First argument to let* must be List or Vec")
                    }
                    if ((bindingsList.size % 2) != 0)
                        throw MalException("let* bindings list must have even # of elements")
                    val names = bindingsList.filterIndexed({ n, _ -> n % 2 == 0 })
                    val values = bindingsList.filterIndexed({ n, _ -> n % 2 == 1 })
                    val bindings = names.zip(values)
                    val innerEnv = Env(env)
                    bindings.forEach { (nameExpr, valueExpr) ->
                        val name = nameExpr as? Expr.Sym
                                ?: throw MalException("let must bind to a Sym")
                        innerEnv.set(name.value, eval(innerEnv, valueExpr))
                    }
                    eval(innerEnv, args[1])
                }
                Expr.Sym("if") -> {
                    if (args.size < 2 || args.size > 3)
                        throw MalException("${args.size} args passed to if, expected 2 or 3")
                    val condResult = eval(env, args[0])
                    val cond = when (condResult) {
                        is Expr.Nil -> false
                        is Expr.Bool -> condResult.value
                        else -> true
                    }
                    when {
                        cond == true -> eval(env, args[1])
                        args.size == 3 -> eval(env, args[2])
                        else -> Expr.Nil
                    }
                }
                Expr.Sym("fn*") ->
                    evalFn(env, args)
                Expr.Sym("do") -> when {
                    args.isEmpty() ->
                        Expr.Nil
                    else -> {
                        args.dropLast(1).forEach { eval(env, it) }
                        eval(env, args.last())
                    }
                }
                Expr.Sym("macroexpand") -> {
                    val firstArg = args.singleOrNull() ?: throw MalException("macroexpand takes 1 argument")
                    macroExpand(env, firstArg)
                }
                Expr.Sym("try*") ->
                    tryMal(env, args)
                Expr.Sym("throw") ->
                    throwMal(env, args)
                else -> {
                    val fn = eval(env, first)
                    when (fn) {
                        is Expr.BuiltInFn ->
                            fn.f(env, args)
                        is Expr.Fn -> {
                            val (funcEnv, body) = envAndBodyForApply(env, fn, args)
                            eval(funcEnv, body)
                        }
                        else ->
                            throw MalException("${show(fn)} is not a function")
                    }
                }
            }
        }
    }
}

fun evalSym(env: Env, sym: Expr.Sym): Expr =
        if (sym.value.startsWith(':'))
            sym
        else
            env.get(sym.value) ?: throw MalException("'${sym.value}' not found")

fun plus(env: Env, args: List<Expr>) =
        numOp(env, "+", { acc, n -> acc + n }, 1, args)

fun sub(env: Env, args: List<Expr>) =
        numOp(env, "-", { acc, n -> acc - n }, 2, args)

fun mult(env: Env, args: List<Expr>) =
        numOp(env, "*", { acc, n -> acc * n }, 1, args)

fun div(env: Env, args: List<Expr>) =
        numOp(env, "/", { acc, n -> acc / n }, 2, args)

fun numOp(env: Env, op: String, f: (Long, Long) -> Long, minArgs: Int, args: List<Expr>): Expr.Num {
    if (args.size < minArgs)
        throw MalException("${args.size} args passed to $op but expected $minArgs or more")
    val evaledArgs = args.map {
        val evaled = eval(env, it) as? Expr.Num
                ?: throw MalException("$op expects Num arguments, got $it")
        evaled.value
    }
    val first = evaledArgs[0]
    val rest = evaledArgs.drop(1)
    return Expr.Num(rest.fold(first, f))
}

fun gt(env: Env, args: List<Expr>) =
        comparison(env, { n1: Long, n2: Long -> n1 > n2 }, args)

fun gtEq(env: Env, args: List<Expr>) =
        comparison(env, { n1: Long, n2: Long -> n1 >= n2 }, args)

fun lt(env: Env, args: List<Expr>) =
        comparison(env, { n1: Long, n2: Long -> n1 < n2 }, args)

fun ltEq(env: Env, args: List<Expr>) =
        comparison(env, { n1: Long, n2: Long -> n1 <= n2 }, args)

fun comparison(env: Env, compare: (Long, Long) -> Boolean, args: List<Expr>): Expr.Bool {
    val msg = "Comparison expects 2 or more Num args"
    if (args.size < 2)
        throw MalException(msg)
    val nums = args.map {
        val num = eval(env, it) as? Expr.Num
                ?: throw MalException(msg)
        num.value
    }
    return nums.drop(1)
            .fold(Pair(true, nums.first()), { (acc, prev), n -> Pair(acc && compare(prev, n), n) })
            .component1()
            .let { Expr.Bool(it) }
}

fun eq(env: Env, args: List<Expr>) = when {
    args.isEmpty() ->
        Expr.Bool(true)
    else -> {
        val first = eval(env, args.first())
        Expr.Bool(args.drop(1).all { eval(env, it) == first })
    }
}

fun isSym(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("sym? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) is Expr.Sym)
}

fun isNil(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("nil? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) is Expr.Nil)
}

fun isTrue(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("true? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) == Expr.Bool(true))
}

fun isFalse(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("false? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) == Expr.Bool(false))
}

fun list(env: Env, args: List<Expr>) = args
        .map { eval(env, it) }
        .let { Expr.List(it) }

fun isList(env: Env, args: List<Expr>) = args
        .all {
            val value = eval(env, it)
            value is Expr.Nil || value is Expr.List
        }
        .let { Expr.Bool(it) }

fun empty(env: Env, args: List<Expr>) = args
        .all {
            val value = eval(env, it)
            when (value) {
                is Seq -> value.exprs.isEmpty()
                else -> throw MalException("empty? expects 1 List/Vec/Nil")
            }
        }.let { Expr.Bool(it) }

fun count(env: Env, args: List<Expr>): Expr.Num {
    val msg = "count expects 1 List/Vec/Nil"
    if (args.size != 1)
        throw MalException(msg)
    val value = eval(env, args.single())
    return when (value) {
        is Seq -> Expr.Num(value.exprs.size.toLong())
        else -> throw MalException(msg)
    }
}

fun cons(env: Env, args: List<Expr>): Expr.List {
    val msg = "cons expects an expression and a List/Vec/Nil"
    if (args.size != 2)
        throw MalException(msg)
    val x = eval(env, args[0])
    val xs = eval(env, args[1])
    return when (xs) {
        is Seq -> Expr.List(listOf(x) + xs.exprs)
        else -> throw MalException(msg)
    }
}

fun concat(env: Env, args: List<Expr>) = args
        .flatMap {
            val value = eval(env, it)
            when (value) {
                is Seq -> value.exprs
                else -> throw MalException("concat expects List/Vec/Nil arguments")
            }
        }.let { Expr.List(it) }

fun nth(env: Env, args: List<Expr>): Expr {
    val err = "nth expects a List/Vec/Nil and a Num"
    if (args.size != 2)
        throw MalException(err)
    val seq = eval(env, args[0])
    val n = eval(env, args[1]) as? Expr.Num ?: throw MalException(err)
    return when (seq) {
        is Seq -> seq.exprs.getOrNull(n.value.toInt()) ?: Expr.Nil
        else -> throw MalException(err)
    }
}

fun first(env: Env, args: List<Expr>): Expr {
    val err = "first expects List/Vec/Nil"
    if (args.size != 1)
        throw MalException(err)
    val arg = eval(env, args[0])
    return when (arg) {
        is Seq -> arg.exprs.firstOrNull() ?: Expr.Nil
        else -> throw MalException(err)
    }
}

fun rest(env: Env, args: List<Expr>): Expr {
    val err = "rest expects a List/Vec/Nil"
    if (args.size != 1)
        throw MalException(err)
    val arg: Seq = (eval(env, args[0]) as? Seq)
            ?: throw MalException(err)
    return Expr.List(arg.exprs.drop(1))
}

fun map(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw MalException("map expects a Fn and a List")
    val fn = eval(env, args[0])
    val list = eval(env, args[1]) as? Seq
            ?: throw MalException("2nd argument to map is not List/Vec/Nil")
    return list.exprs.map {
        val arg = eval(env, it)
        when (fn) {
            is Expr.BuiltInFn ->
                fn.f(env, listOf(arg))
            is Expr.Fn -> {
                val (funcEnv, body) = envAndBodyForApply(env, fn, listOf(arg))
                eval(funcEnv, body)
            }
            else ->
                throw MalException("1st argument to map is not a function")
        }

    }.let { Expr.List(it) }
}

fun atom(env: Env, args: List<Expr>): Expr.Atom {
    if (args.size != 1)
        throw MalException("atom expects one argument")
    return Expr.Atom(eval(env, args.single()))
}

fun isAtom(env: Env, args: List<Expr>) = args
        .all { eval(env, it) is Expr.Atom }
        .let { Expr.Bool(it) }

fun deref(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("deref expects one Atom argument")
    val atom = eval(env, args.single()) as? Expr.Atom
            ?: throw MalException("First argument to deref is not an Atom")
    return atom.expr
}

fun reset(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw MalException("reset! expects an Atom and a value")
    val atom = eval(env, args.first()) as? Expr.Atom
            ?: throw MalException("First argument to reset! is not an Atom")
    val value = eval(env, args[1])
    atom.expr = value
    return value
}

fun swap(env: Env, args: List<Expr>): Expr {
    if (args.size < 2)
        throw MalException("swap! expects at least an Atom and a Fn")
    val atom = eval(env, args.first()) as? Expr.Atom
            ?: throw MalException("First argument to swap! is not an Atom")
    val fn = eval(env, args[1])
    val fnArgs = listOf(atom.expr) + args.drop(2).map { eval(env, it) }
    val newVal = when (fn) {
        is Expr.BuiltInFn ->
            fn.f(env, fnArgs)
        is Expr.Fn -> {
            val (funcEnv, body) = envAndBodyForApply(env, fn, fnArgs)
            eval(funcEnv, body)
        }
        else ->
            throw MalException("Second argument to swap! is not a function")
    }
    atom.expr = newVal
    return newVal
}

fun prn(env: Env, args: List<Expr>): Expr.Nil {
    println(args.map { prStr(eval(env, it), true) }.joinToString(" "))
    return Expr.Nil
}

fun str(env: Env, args: List<Expr>) =
        Expr.Str(args.map { prStr(eval(env, it)) }.joinToString(""))

fun prStrMal(env: Env, args: List<Expr>) =
        Expr.Str(args.map { prStr(eval(env, it), true) }.joinToString(" "))

fun printlnMal(env: Env, args: List<Expr>): Expr.Nil {
    println(args.map { prStr(eval(env, it)) }.joinToString(" "))
    return Expr.Nil
}

fun readStr(env: Env, args: List<Expr>): Expr {
    val msg = "read-string takes one Str argument"
    if (args.size != 1)
        throw MalException(msg)
    val str = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    return parse(str.value)
}

fun slurp(env: Env, args: List<Expr>): Expr.Str {
    val msg = "slurp takes one Str argument"
    if (args.size != 1)
        throw MalException(msg)
    val path = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    return Expr.Str(File(path.value).readText(Charsets.UTF_8))
}

fun loadFile(env: Env, args: List<Expr>): Expr {
    val path = args.singleOrNull() as? Expr.Str
            ?: throw MalException("load-file takes one Str argument")
    return loadFileStream(env, File(path.value).inputStream())
}

fun loadFileStream(env: Env, stream: InputStream): Expr {
    val malCode = BufferedReader(InputStreamReader(stream)).use {
        it.lines().collect(Collectors.joining("\n"))
    }
    val ast = readStr(env, listOf(Expr.Str("(do $malCode)")))
    return eval(env, ast)
}

fun evalMal(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("eval takes one argument")
    return eval(env, eval(env, args.single()))
}

fun evalDef(env: Env, args: List<Expr>, isMacro: Boolean): Expr {
    if (args.size != 2)
        throw MalException("${args.size} args passed to def!, expected 2")

    val name = args[0]
    val value = args[1]

    if (name !is Expr.Sym)
        throw MalException("First arg to def! must be Sym")

    var result = eval(env, value)

    if (result is Expr.Fn) {
        result = result.copy(isMacro = isMacro)
        result.env.set(name.value, result)
    }

    env.set(name.value, result)

    return result
}

fun evalFn(env: Env, args: List<Expr>): Expr.Fn {
    val msg = "fn* takes a List/Vec of Sym arguments and a body"
    if (args.size != 2)
        throw MalException(msg)
    val argList = args[0] as? Seq
            ?: throw MalException(msg)
    val argNames = argList.exprs.map {
        when (it) {
            is Expr.Sym -> it.value
            else -> throw MalException(msg)
        }
    }
    val isVariadic = argNames.any { it == "&" }
    val normalArgNames = argNames.takeWhile { it != "&" }
    val varArgName =
            if (isVariadic) {
                if (argNames.count { it == "&" } > 1)
                    throw MalException("fn*: Only one & allowed in args list.")
                argNames.takeLastWhile { it != "&" }.singleOrNull()
                        ?: throw MalException("fn*: One arg, no more, must appear after &")
            } else null
    val body = args[1]
    return Expr.Fn(normalArgNames, varArgName, body, env)  // TODO: Copy env?
}

fun envAndBodyForApply(env: Env, fn: Expr.Fn, unevaledArgs: List<Expr>): Pair<Env, Expr> {
    val args =
            if (fn.isMacro)
                unevaledArgs
            else
                unevaledArgs.map { eval(env, it) }
    val argList = buildArgList(fn.argNames, fn.varArgName, args)
    val funcEnv = Env(fn.env)
    argList.forEach { (argName, argVal) -> funcEnv.set(argName, argVal) }
    return Pair(funcEnv, fn.body)
}

fun buildArgList(argNames: List<String>,
                 varArgName: String?,
                 args: List<Expr>): List<Pair<String, Expr>> =
        if (varArgName != null) {
            if (args.size < argNames.size)
                throw MalException("Expected ${argNames.size} or more args, got ${args.size}")
            val argsList = argNames.zip(args).toMutableList()
            val varArgList = args.subList(argNames.size, args.size)
            argsList.add(Pair(varArgName, Expr.List(varArgList)))
            argsList
        } else {
            if (args.size != argNames.size)
                throw MalException("Expected ${argNames.size} args, got ${args.size}")
            argNames.zip(args)
        }

fun quasiquote(env: Env, expr: Expr): Expr = when (expr) {
    is Expr.Unquote ->
        eval(env, expr.expr)
    is Seq ->
        expr.exprs.flatMap {
            when (it) {
                is Expr.Unquote ->
                    listOf(eval(env, it.expr))
                is Expr.SpliceUnquote -> {
                    val list = eval(env, it.expr) as? Expr.List
                            ?: throw MalException("quasiquote argument should evaluate to a List")
                    list.exprs
                }
                is Expr.List ->
                    listOf(quasiquote(env, it) as Expr.List)
                else ->
                    listOf(it)
            }
        }.let { Expr.List(it) }
    else ->
        expr
}

fun macroExpand(env: Env, initialAst: Expr): Expr {
    var ast = initialAst
    while (true) {
        val (fn, args) = isMacroCall(env, ast) ?: break
        val (funcEnv, body) = envAndBodyForApply(env, fn, args)
        ast = eval(funcEnv, body)
    }
    return ast
}

fun isMacroCall(env: Env, expr: Expr): Pair<Expr.Fn, List<Expr>>? {
    if (expr !is Expr.List || expr.exprs.isEmpty())
        return null
    val sym = expr.exprs.first() as? Expr.Sym
            ?: return null
    val value = env.get(sym.value)
    return when {
        (value is Expr.Fn) && value.isMacro ->
            Pair(value, expr.exprs.drop(1))
        else ->
            null
    }
}

fun tryMal(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw MalException("try expects an expr and a catch* block")
    val catch = args[1]
    if (catch !is Expr.List || catch.exprs.size != 3 || catch.exprs[0] != Expr.Sym("catch*"))
        throw MalException("malformed catch* block")
    val exceptionName = catch.exprs[1] as? Expr.Sym
            ?: throw MalException("malformed catch* block")
    val catchBody = catch.exprs[2]
    return try {
        eval(env, args[0])
    } catch (e: MalException) {
        val catchEnv = Env(env)
        catchEnv.set(exceptionName.value, e.value)
        eval(catchEnv, catchBody)
    }
}

fun throwMal(env: Env, args: List<Expr>): Expr {
    val exception = args.singleOrNull()
            ?: throw MalException("throw* expects 1 arg")
    throw MalException(eval(env, exception))
}

val initialEnv: Map<String, Expr> = mutableMapOf(
        "+" to ::plus,
        "-" to ::sub,
        "*" to ::mult,
        "/" to ::div,
        ">" to ::gt,
        ">=" to ::gtEq,
        "<" to ::lt,
        "<=" to ::ltEq,
        "=" to ::eq,
        "symbol?" to ::isSym,
        "nil?" to ::isNil,
        "true?" to ::isTrue,
        "false?" to ::isFalse,
        "list" to ::list,
        "list?" to ::isList,
        "empty?" to ::empty,
        "count" to ::count,
        "cons" to ::cons,
        "concat" to ::concat,
        "nth" to ::nth,
        "first" to ::first,
        "rest" to ::rest,
        "map" to ::map,
        "atom" to ::atom,
        "atom?" to ::isAtom,
        "deref" to ::deref,
        "reset!" to ::reset,
        "swap!" to ::swap,
        "pr-str" to ::prStrMal,
        "str" to ::str,
        "prn" to ::prn,
        "println" to ::printlnMal,
        "read-string" to ::readStr,
        "slurp" to ::slurp,
        "load-file" to ::loadFile,
        "eval" to ::evalMal,
        "throw" to ::throwMal
).mapValues { (_, f) -> Expr.BuiltInFn(f) }

fun runScript(env: Env, argv: Array<String>) {
    val filePath = argv.first()
    val args = argv.drop(1)
    env.set("*ARGV*", Expr.List(args.map { Expr.Str(it) }))
    loadFile(env, listOf(Expr.Str(filePath)))
}

fun rep(env: Env) {
    env.set("*ARGV*", Expr.List(listOf()))

    while (true) {
        print("user> ")

        val line = readLine() ?: break
        if (line.isBlank())
            continue

        try {
            val ast = parse(line)
            println(show(eval(env, ast)))
        } catch (e: ParserException) {
            println("Parse error: ${e.message}")
        } catch (e: MalException) {
            println("Exception: ${show(e.value)}")
        }
    }
}

fun malMain(argv: Array<String>) {
    val env = Env(initialEnv)
    // Read in core.mal
    object {}.javaClass.getResourceAsStream("/core.mal").use {
        loadFileStream(env, it)
    }
    // Run script file or enter REPL
    if (argv.isNotEmpty())
        runScript(env, argv)
    else
        rep(env)
}
