import org.apache.commons.lang3.StringEscapeUtils
import org.codehaus.jparsec.Parser
import org.codehaus.jparsec.Parsers
import org.codehaus.jparsec.Scanners
import org.codehaus.jparsec.error.ParserException
import org.codehaus.jparsec.pattern.Patterns
import java.io.File
import java.util.*

val SPECIAL_FORMS = listOf("def!", "let*", "if", "fn*", "do", "quote",
        "quasiquote", "unquote", "splice-unquote")

interface Seq {
    val exprs: List<Expr>
}

sealed class Expr {
    object Nil : Expr()
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
                  val env: Env) : Expr()

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
            Scanners.string("nil").map { Expr.Nil },
            Scanners.string("true").map { Expr.Bool(true) },
            Scanners.string("false").map { Expr.Bool(false) },
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
            Patterns.regex("[^\\s\\[\\]{}('\"`,;)@]+").toScanner("symbol").source().map { Expr.Sym(it) }
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

    fun set(sym: String, value: Expr) {
        if (sym in SPECIAL_FORMS)
            throw EvalException("Cannot define $sym since it is a special form")
        bindings[sym] = value
    }
}

class EvalException(msg: String) : RuntimeException(msg)

@Suppress("NON_TAIL_RECURSIVE_CALL")  // Not all calls to eval() can be in tail pos
tailrec fun eval(env: Env, expr: Expr): Expr {
    return when (expr) {
        is Expr.Nil, is Expr.Bool, is Expr.Num, is Expr.Str, is Expr.Atom, is Expr.Fn, is Expr.BuiltInFn ->
            expr
        is Expr.Sym -> evalSym(env, expr)
        is Expr.Quote -> expr.expr
        is Expr.QuasiQuote -> quasiquote(env, expr.expr)
        is Expr.Unquote -> throw EvalException("Cannot eval Unquote in non-quasiquote context")
        is Expr.SpliceUnquote -> throw EvalException("Cannot eval SpliceUnquote in non-quasiquote context")
        is Expr.Deref -> deref(env, listOf(expr.expr))
        is Expr.Metadata -> throw EvalException("Cannot eval Metadata")
        is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(env, it) })
        is Expr.HashMap -> Expr.HashMap(expr.exprs.map { eval(env, it) })
        is Expr.List -> {
            if (expr.exprs.isEmpty())
                return expr
            val first = expr.exprs.first()
            val args = expr.exprs.drop(1)
            when (first) {
                Expr.Sym("def!") ->
                    evalDef(env, args)
                Expr.Sym("let*") -> {
                    if (args.size != 2)
                        throw EvalException("${args.size} args passed to let*, expected 2")
                    val firstArg = args[0]
                    val bindingsList = when (firstArg) {
                        is Seq -> firstArg.exprs
                        else -> throw EvalException("First argument to let* must be List or Vec")
                    }
                    if ((bindingsList.size % 2) != 0)
                        throw EvalException("let* bindings list must have even # of elements")
                    val names = bindingsList.filterIndexed({ n, _ -> n % 2 == 0 })
                    val values = bindingsList.filterIndexed({ n, _ -> n % 2 == 1 })
                    val bindings = names.zip(values)
                    val innerEnv = Env(env)
                    bindings.forEach { (nameExpr, valueExpr) ->
                        val name = nameExpr as? Expr.Sym
                                ?: throw EvalException("let must bind to a Sym")
                        innerEnv.set(name.value, eval(innerEnv, valueExpr))
                    }
                    eval(innerEnv, args[1])
                }
                Expr.Sym("if") -> {
                    if (args.size < 2 || args.size > 3)
                        throw EvalException("${args.size} args passed to if, expected 2 or 3")
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
                            throw EvalException("$fn is not a function")
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
            env.get(sym.value) ?: throw EvalException("Undefined symbol ${sym.value}")

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
        throw EvalException("${args.size} args passed to $op but expected $minArgs or more")
    val evaledArgs = args.map {
        val evaled = eval(env, it) as? Expr.Num
                ?: throw EvalException("$op expects Num arguments, got $it")
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
    if (args.size < 2)
        throw EvalException("Comparison expects > 2 arguments")
    val nums = args.map {
        val num = eval(env, it) as? Expr.Num
                ?: throw EvalException("comparison expects Num arguments")
        num.value
    }
    return Expr.Bool(
            nums.drop(1)
                    .fold(Pair(true, nums.first()), { (acc, prev), n -> Pair(acc && compare(prev, n), n) })
                    .component1()
    )
}

fun eq(env: Env, args: List<Expr>) = when {
    args.isEmpty() ->
        Expr.Bool(true)
    else -> {
        val first = eval(env, args.first())
        Expr.Bool(args.drop(1).all { eval(env, it) == first })
    }
}

fun not(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1)
        throw EvalException("not takes 1 argument")
    val evaled = eval(env, args.first())
    return when (evaled) {
        is Expr.Nil -> Expr.Bool(true)
        is Expr.Bool -> Expr.Bool(!evaled.value)
        else -> Expr.Bool(false)
    }
}

fun list(env: Env, args: List<Expr>) = args
        .map { eval(env, it) }
        .let { Expr.List(it) }

fun isList(env: Env, args: List<Expr>) = args
        .all { eval(env, it) is Expr.List }
        .let { Expr.Bool(it) }

fun empty(env: Env, args: List<Expr>) = args
        .all {
            val value = eval(env, it)
            when (value) {
                is Seq -> value.exprs.isEmpty()
                else -> throw EvalException("empty? expects List or Vec arguments")
            }
        }
        .let { Expr.Bool(it) }

fun count(env: Env, args: List<Expr>): Expr.Num =
        if (args.size != 1)
            throw EvalException("count expects one List/Vec/Nil argument")
        else {
            val value = eval(env, args.first())
            when (value) {
                is Expr.Nil -> Expr.Num(0)
                is Seq -> Expr.Num(value.exprs.size.toLong())
                else -> throw EvalException("count expects one List/Vec/Nil argument")
            }
        }

fun cons(env: Env, args: List<Expr>): Expr.List {
    if (args.size != 2)
        throw EvalException("cons expects an expression and a List or Vec")
    val x = eval(env, args.first())
    val xs = eval(env, args[1]) as? Seq
            ?: throw EvalException("Second argument to cons must be List or Vec")
    return Expr.List(listOf(x) + xs.exprs)
}

fun concat(env: Env, args: List<Expr>) = args
        .map {
            eval(env, it) as? Seq
                    ?: throw EvalException("concat expects List or Vec arguments")
        }
        .flatMap(Seq::exprs)
        .let { Expr.List(it) }

fun atom(env: Env, args: List<Expr>): Expr.Atom {
    if (args.size != 1)
        throw EvalException("atom expects one argument")
    return Expr.Atom(eval(env, args.first()))
}

fun isAtom(env: Env, args: List<Expr>) = args
        .all { eval(env, it) is Expr.Atom }
        .let { Expr.Bool(it) }

fun deref(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw EvalException("deref expects one Atom argument")
    val atom = eval(env, args.first()) as? Expr.Atom
            ?: throw EvalException("First argument to deref is not an Atom")
    return atom.expr
}

fun reset(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw EvalException("reset! expects an Atom and a value")
    val atom = eval(env, args.first()) as? Expr.Atom
            ?: throw EvalException("First argument to reset! is not an Atom")
    val value = eval(env, args[1])
    atom.expr = value
    return value
}

fun swap(env: Env, args: List<Expr>): Expr {
    if (args.size < 2)
        throw EvalException("swap! expects at least an Atom and a Fn")
    val atom = eval(env, args.first()) as? Expr.Atom
            ?: throw EvalException("First argument to swap! is not an Atom")
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
            throw EvalException("First argument to swap! is not a function")
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
    if (args.size != 1)
        throw EvalException("read-string takes one Str argument")
    val str = eval(env, args.first()) as? Expr.Str
            ?: throw EvalException("read-string takes one Str argument")
    return parse(str.value)
}

fun slurp(env: Env, args: List<Expr>): Expr.Str {
    if (args.size != 1)
        throw EvalException("slurp takes one Str argument")
    val path = eval(env, args.first()) as? Expr.Str
            ?: throw EvalException("slurp takes one Str argument")
    return Expr.Str(File(path.value).readText(Charsets.UTF_8))
}

fun loadFile(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw EvalException("loadFile takes one Str argument")
    val str = "(do ${slurp(env, args).value} )"
    val ast = readStr(env, listOf(Expr.Str(str)))
    return eval(env, ast)
}

fun evalMal(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw EvalException("eval takes one argument")
    return eval(env, eval(env, args.first()))
}

fun evalDef(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw EvalException("${args.size} args passed to def!, expected 2")

    val name = args[0]
    val value = args[1]

    if (name !is Expr.Sym)
        throw EvalException("First arg to def! must be Sym")

    val result = eval(env, value)

    if (result is Expr.Fn)
        result.env.set(name.value, result)

    env.set(name.value, result)

    return result
}

fun evalFn(env: Env, args: List<Expr>): Expr.Fn {
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
                    throw EvalException("fn*: Only one & allowed in args list.")
                argNames.takeLastWhile { it != "&" }.singleOrNull()
                        ?: throw EvalException("fn*: One arg, no more, must appear after &")
            } else
                null
    val body = args[1]
    return Expr.Fn(normalArgNames, varArgName, body, env)  // TODO: Copy env?
}

fun envAndBodyForApply(env: Env, fn: Expr.Fn, args: List<Expr>): Pair<Env, Expr> {
    val argList = buildArgList(fn.argNames, fn.varArgName, args.map { eval(env, it) })
    val funcEnv = Env(fn.env)
    argList.forEach { (argName, argVal) -> funcEnv.set(argName, argVal) }
    return Pair(funcEnv, fn.body)
}

fun buildArgList(argNames: List<String>,
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
                            ?: throw EvalException("quasiquote argument should evaluate to a List")
                    list.exprs
                }
                else ->
                    listOf(it)
            }
        }.let { Expr.List(it) }
    else ->
        expr
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
        "not" to ::not,
        "list" to ::list,
        "list?" to ::isList,
        "empty?" to ::empty,
        "count" to ::count,
        "cons" to ::cons,
        "concat" to ::concat,
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
        "eval" to ::evalMal
).mapValues { (_, f) -> Expr.BuiltInFn(f) }

fun runFile(argv: Array<String>) {
    val env = Env(initialEnv)
    val filePath = argv.first()
    val args = argv.drop(1)
    env.set("*ARGV*", Expr.List(args.map { Expr.Str(it) }))
    loadFile(env, listOf(Expr.Str(filePath)))
}

fun rep() {
    val env = Env(initialEnv)
    env.set("*ARGV*", Expr.List(emptyList()))

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
        } catch (e: EvalException) {
            println("Eval error: ${e.message}")
        }
    }
}

fun malMain(argv: Array<String>) =
        if (argv.isNotEmpty())
            runFile(argv)
        else
            rep()
