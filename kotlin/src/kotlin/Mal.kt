import org.jparsec.error.ParserException
import java.io.BufferedReader
import java.io.File
import java.io.InputStream
import java.io.InputStreamReader
import java.util.*
import java.util.stream.Collectors

val BUILT_IN_FUNCTIONS: Map<String, Expr> = mapOf(
        "+" to ::plus,
        "-" to ::sub,
        "*" to ::mult,
        "/" to ::div,
        ">" to ::gt,
        ">=" to ::gtEq,
        "<" to ::lt,
        "<=" to ::ltEq,
        "=" to ::eq,
        "symbol" to ::symbol,
        "keyword" to ::keyword,
        "symbol?" to ::isSymbol,
        "keyword?" to ::isKeyword,
        "nil?" to ::isNil,
        "true?" to ::isTrue,
        "false?" to ::isFalse,
        "string?" to ::isString,
        "list?" to ::isList,
        "vector?" to ::isVector,
        "list" to ::list,
        "vector" to ::vector,
        "hash-map" to ::hashMap,
        "map?" to ::isMap,
        "empty?" to ::empty,
        "sequential?" to ::isSequential,
        "count" to ::count,
        "cons" to ::cons,
        "concat" to ::concat,
        "nth" to ::nth,
        "first" to ::first,
        "rest" to ::rest,
        "map" to ::map,
        "conj" to ::conj,
        "seq" to ::seq,
        "assoc" to ::assoc,
        "dissoc" to ::dissoc,
        "get" to ::get,
        "contains?" to ::contains,
        "keys" to ::keys,
        "vals" to ::vals,
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
        "def!" to ::evalDef,
        "defmacro!" to ::evalDefmacro,
        "fn*" to ::evalFn,
        "quote" to ::quote,
        "quasiquote" to ::quasiquote,
        "unquote" to ::unquote,
        "splice-unquote" to ::spliceUnquote,
        "macroexpand" to ::macroExpandMal,
        "try*" to ::tryMal,
        "throw" to ::throwMal,
        "apply" to ::apply,
        "readline" to ::readline,
        "with-meta" to ::withMeta,
        "meta" to ::meta,
        "time-ms" to ::timeMs,
        "gensym" to ::gensym
).mapValues { (_, f) -> Expr.BuiltInFn(f) }

val BUILT_IN_VARS = mapOf("*host-language*" to Expr.Str("kotlin"))

val INITIAL_ENV: Map<String, Expr> =
        BUILT_IN_FUNCTIONS.plus(BUILT_IN_VARS)

val BANNED_SYMBOLS =
        INITIAL_ENV.keys + listOf("let*", "if", "do")

class Env {
    private val bindings: MutableMap<String, Expr>
    private val outerEnv: Env?

    constructor(bindings: Map<String, Expr>) {
        this.bindings = bindings as MutableMap<String, Expr>
        this.outerEnv = null
    }

    constructor(outerEnv: Env?) {
        this.bindings = mutableMapOf()
        this.outerEnv = outerEnv
    }

    fun get(sym: Expr.Sym): Expr? =
            if (bindings.containsKey(sym.value))
                bindings[sym.value]
            else
                outerEnv?.get(sym)

    fun set(sym: String, value: Expr) =
            if (sym in BANNED_SYMBOLS)
                throw MalException("Cannot define $sym - there is a built-in with the same name")
            else if (bindings.containsKey(sym) && value == Expr.Nil)  // Bizarre, but makes tests pass
                Unit
            else
                bindings[sym] = value
}


class MalException(val value: Expr) : RuntimeException() {
    constructor(msg: String) : this(Expr.Str(msg))
}


@Suppress("NON_TAIL_RECURSIVE_CALL")  // Not all calls to eval() can be in tail pos
tailrec fun eval(env: Env, exprRaw: Expr): Expr {
    val expr = macroExpand(env, exprRaw)
    return when (expr) {
        is Expr.Nil,
        is Expr.Bool,
        is Expr.Num,
        is Expr.Str,
        is Expr.Atom,
        is Expr.Fn,
        is Expr.BuiltInFn,
        is Expr.Keyword,
        is Expr.HashMap ->
            expr
        is Expr.Sym -> evalSym(env, expr)
        is Expr.Vec -> Expr.Vec(expr.exprs.map { eval(env, it) })
        is Expr.List -> {
            if (expr.exprs.isEmpty())
                return expr
            val first = expr.exprs.first()
            val args = expr.exprs.drop(1)
            when (first) {
                Expr.Sym("let*") -> {
                    if (args.size != 2)
                        throw MalException("${args.size} args passed to let*, expected 2")
                    val firstArg = args[0]
                    val bindingsList = when (firstArg) {
                        is Expr.Seq -> firstArg.exprs
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
                        cond -> eval(env, args[1])
                        args.size == 3 -> eval(env, args[2])
                        else -> Expr.Nil
                    }
                }

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
                            throw MalException("${fn.prStr()} is not a function")
                    }
                }
            }
        }
    }
}

fun evalSym(env: Env, sym: Expr.Sym): Expr =
        if (sym.value.startsWith(':'))
            Expr.Keyword(sym.value.drop(1))
        else
            env.get(sym) ?: throw MalException("'${sym.value}' not found")

fun symbol(env: Env, args: List<Expr>): Expr.Sym {
    val msg = "symbol expects one Str argument"
    if (args.size != 1)
        throw MalException(msg)
    val str = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    return Expr.Sym(str.value)
}

fun keyword(env: Env, args: List<Expr>): Expr.Keyword {
    val msg = "keyword expects one Str argument"
    if (args.size != 1)
        throw MalException(msg)
    val str = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    return Expr.Keyword(str.value)
}

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

fun isSymbol(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.Sym>(env, args)

fun isKeyword(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.Keyword>(env, args)

fun isNil(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.Nil>(env, args)

fun isString(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.Str>(env, args)

fun isVector(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.Vec>(env, args)

fun isMap(env: Env, args: List<Expr>): Expr.Bool =
        isType<Expr.HashMap>(env, args)

inline fun <reified T> isType(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("false? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) is T)
}

fun isTrue(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("true? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) == Expr.Bool(true))
}

fun isFalse(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1) throw MalException("false? takes 1 argument")
    return Expr.Bool(eval(env, args.single()) == Expr.Bool(false))
}

fun isSequential(env: Env, args: List<Expr>): Expr.Bool {
    if (args.size != 1)
        throw MalException("sequential? expects 1 arg")
    val value = eval(env, args.single())
    return when (value) {
        is Expr.Nil -> Expr.Bool(false)
        is Expr.List, is Expr.Vec -> Expr.Bool(true)
        else -> Expr.Bool(false)
    }
}

fun list(env: Env, args: List<Expr>) = args
        .map { eval(env, it) }
        .let { Expr.List(it) }

fun vector(env: Env, args: List<Expr>) = args
        .map { eval(env, it) }
        .let { Expr.Vec(it) }

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
                is Expr.Seq -> value.exprs.isEmpty()
                else -> throw MalException("empty? expects 1 List/Vec/Nil")
            }
        }.let { Expr.Bool(it) }

fun count(env: Env, args: List<Expr>): Expr.Num {
    val msg = "count expects 1 List/Vec/Nil"
    if (args.size != 1)
        throw MalException(msg)
    val value = eval(env, args.single())
    return when (value) {
        is Expr.Seq -> Expr.Num(value.exprs.size.toLong())
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
        is Expr.Seq -> Expr.List(listOf(x) + xs.exprs)
        else -> throw MalException(msg)
    }
}

fun concat(env: Env, args: List<Expr>) = args
        .flatMap {
            val value = eval(env, it)
            when (value) {
                is Expr.Seq -> value.exprs
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
        is Expr.Seq -> seq.exprs.getOrNull(n.value.toInt()) ?: Expr.Nil
        else -> throw MalException(err)
    }
}

fun first(env: Env, args: List<Expr>): Expr {
    val err = "first expects List/Vec/Nil"
    if (args.size != 1)
        throw MalException(err)
    val arg = eval(env, args[0])
    return when (arg) {
        is Expr.Seq -> arg.exprs.firstOrNull() ?: Expr.Nil
        else -> throw MalException(err)
    }
}

fun rest(env: Env, args: List<Expr>): Expr {
    val err = "rest expects a List/Vec/Nil"
    if (args.size != 1)
        throw MalException(err)
    val arg: Expr.Seq = (eval(env, args[0]) as? Expr.Seq)
            ?: throw MalException(err)
    return Expr.List(arg.exprs.drop(1))
}

fun map(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw MalException("map expects a Fn and a List")
    val fn = eval(env, args[0])
    val list = eval(env, args[1]) as? Expr.Seq
            ?: throw MalException("2nd argument to map is not List/Vec/Nil")
    return list.exprs.map {
        when (fn) {
            is Expr.BuiltInFn ->
                fn.f(env, listOf(it))
            is Expr.Fn -> {
                val (funcEnv, body) = envAndBodyForApply(env, fn, listOf(it), evalArgs = false)
                eval(funcEnv, body)
            }
            else ->
                throw MalException("1st argument to map is not a function")
        }

    }.let { Expr.List(it) }
}

fun conj(env: Env, args: List<Expr>): Expr {
    val msg = "conj expects at least a List/Vec/Nil"
    if (args.isEmpty())
        throw MalException(msg)
    val collection = eval(env, args[0])
    val items = args.drop(1).map { eval(env, it) }
    return when (collection) {
        is Expr.List ->
            Expr.List(items.reversed() + collection.exprs)
        is Expr.Vec ->
            Expr.Vec(collection.exprs + items)
        else ->
            throw MalException(msg)
    }
}

fun seq(env: Env, args: List<Expr>): Expr {
    val msg = "seq expects 1 Str/List/Vec/Nil argument"
    if (args.size != 1)
        throw MalException(msg)
    val arg = eval(env, args.single())
    return when (arg) {
        is Expr.Str ->
            if (arg.value.isEmpty())
                Expr.Nil
            else
                Expr.List(arg.value.toList().map { Expr.Str(it.toString()) })
        is Expr.Seq ->
            if (arg.exprs.isEmpty())
                Expr.Nil
            else
                Expr.List(arg.exprs)
        else ->
            throw MalException(msg)
    }
}

fun hashMap(env: Env, args: List<Expr>): Expr.HashMap {
    if (args.size % 2 != 0)
        throw MalException("HashMap expects an even # of arguments")
    val argValues = args.map { eval(env, it) }
    val keys = argValues.filterIndexed { i, _ -> i % 2 == 0 }
    val values = argValues.filterIndexed { i, _ -> i % 2 == 1 }
    val map = mutableMapOf<Expr, Expr>()
    keys.zip(values).forEach { (k, v) -> map[k] = v }
    return Expr.HashMap(map)
}

fun assoc(env: Env, args: List<Expr>): Expr.HashMap {
    val msg = "assoc expects a HashMap and key-value pairs"
    if ((args.size % 2) != 1)
        throw MalException(msg)
    val map = eval(env, args[0]) as? Expr.HashMap
            ?: throw MalException(msg)
    val kvPairs = args.drop(1).map { eval(env, it) }
    val keys = kvPairs.filterIndexed { i, _ -> (i % 2) == 0 }
    val values = kvPairs.filterIndexed { i, _ -> (i % 2) == 1 }
    // TODO: ugh
    val newMap = mutableMapOf<Expr, Expr>()
    map.map.forEach { k, v -> newMap[k] = v }
    keys.zip(values).forEach { (k, v) -> newMap[k] = v }
    return Expr.HashMap(newMap)
}

fun dissoc(env: Env, args: List<Expr>): Expr.HashMap {
    val msg = "dissoc expects a HashMap and keys"
    if (args.isEmpty())
        throw MalException(msg)
    val map = eval(env, args[0]) as? Expr.HashMap
            ?: throw MalException(msg)
    val keysToRemove = args.drop(1).map { eval(env, it) }
    // TODO: ugh
    val newMap = mutableMapOf<Expr, Expr>()
    map.map.filter { (k, _) -> k !in keysToRemove }
            .forEach { k, v -> newMap[k] = v }
    return Expr.HashMap(newMap)
}

fun get(env: Env, args: List<Expr>): Expr {
    val msg = "get expects one HashMap and a key"
    if (args.size != 2)
        throw MalException(msg)
    val mapOrNil = eval(env, args[0])
    val key = eval(env, args[1])
    return when (mapOrNil) {
        is Expr.HashMap -> mapOrNil.map[key] ?: Expr.Nil
        is Expr.Nil -> Expr.Nil
        else -> throw MalException(msg)
    }
}

fun contains(env: Env, args: List<Expr>): Expr {
    val msg = "contains expects a HashMap and a key"
    if (args.size != 2)
        throw MalException(msg)
    val map = eval(env, args[0]) as? Expr.HashMap
            ?: throw MalException(msg)
    val key = eval(env, args[1])
    return Expr.Bool(map.map.containsKey(key))
}

fun keys(env: Env, args: List<Expr>): Expr.List {
    val msg = "keys expects one HashMap argument"
    if (args.size != 1)
        throw MalException(msg)
    val map = eval(env, args.single()) as? Expr.HashMap
            ?: throw MalException(msg)
    return Expr.List(map.map.keys.toList())
}

fun vals(env: Env, args: List<Expr>): Expr.List {
    val msg = "vals expects one HashMap argument"
    if (args.size != 1)
        throw MalException(msg)
    val map = eval(env, args.single()) as? Expr.HashMap
            ?: throw MalException(msg)
    return Expr.List(map.map.values.toList())
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
    val fnArgs = listOf(atom.expr) + args.drop(2)
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
    println(args.map { eval(env, it).prStr() }.joinToString(" "))
    return Expr.Nil
}

fun str(env: Env, args: List<Expr>) =
        Expr.Str(args.map { eval(env, it).prStr(printReadably = false) }.joinToString(""))

fun prStrMal(env: Env, args: List<Expr>) =
        Expr.Str(args.map { eval(env, it).prStr() }.joinToString(" "))

fun printlnMal(env: Env, args: List<Expr>): Expr.Nil {
    println(args.map { eval(env, it).prStr(printReadably = false) }.joinToString(" "))
    return Expr.Nil
}

fun readStr(env: Env, args: List<Expr>): Expr {
    val msg = "read-string takes one Str argument"
    if (args.size != 1)
        throw MalException(msg)
    val str = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    return Parse.parse(str.value)
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
    val argValue = eval(env, args.single())
    return eval(env, argValue)
}

fun evalDef(env: Env, args: List<Expr>) =
        evalDefOrDefmacro(env, args, isMacro = false)

fun evalDefmacro(env: Env, args: List<Expr>) =
        evalDefOrDefmacro(env, args, isMacro = true)

fun evalDefOrDefmacro(env: Env, args: List<Expr>, isMacro: Boolean): Expr {
    if (args.size != 2)
        throw MalException("${args.size} args passed to def!, expected 2")

    val name = args[0] as? Expr.Sym
            ?: throw MalException("First arg to def! must be Sym")
    val value = eval(env, args[1])

    return when (value) {
        is Expr.Fn -> {
            val meta = value.meta
            val fnNoMeta = value.copy(isMacro = isMacro)
            fnNoMeta.env.set(name.value, fnNoMeta)
            val fn = fnNoMeta.withMeta(meta)
            env.set(name.value, fn)
            fn
        }
        else -> {
            env.set(name.value, value)
            value
        }
    }
}

fun evalFn(env: Env, args: List<Expr>): Expr.Fn {
    val msg = "fn* takes a List/Vec of Sym arguments and a body"
    if (args.size != 2)
        throw MalException(msg)
    val argList = args[0] as? Expr.Seq
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

fun envAndBodyForApply(env: Env,
                       fn: Expr.Fn,
                       rawArgs: List<Expr>,
                       evalArgs: Boolean = true): Pair<Env, Expr> {
    val args =
            if (fn.isMacro || !evalArgs)
                rawArgs
            else
                rawArgs.map { eval(env, it) }
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

@Suppress("UNUSED_PARAMETER")
fun quote(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("quote expects 1 argument")
    return args.single()
}

fun unquote(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("unquote expects 1 argument")
    return eval(env, args.single())
}

@Suppress("UNUSED_PARAMETER")
fun spliceUnquote(env: Env, args: List<Expr>): Expr =
        throw MalException("Cannot eval splice-unquote outside of a quasiquoted list")

fun quasiquote(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("quasiquote expects 1 arg")
    val arg = args.single()
    return when {
        arg is Expr.List && arg.exprs[0] == Expr.Sym("unquote") -> {
            if (arg.exprs.size != 2)
                throw MalException("unquote expects 1 arg")
            eval(env, arg.exprs[1])
        }
        arg is Expr.Seq -> arg.exprs.flatMap { listItem ->
            when {
                listItem is Expr.List && listItem.exprs[0] == Expr.Sym("unquote") -> {
                    if (listItem.exprs.size != 2)
                        throw MalException("unquote expects 1 arg")
                    listOf(eval(env, listItem.exprs[1]))
                }
                listItem is Expr.List && listItem.exprs[0] == Expr.Sym("splice-unquote") -> {
                    if (listItem.exprs.size != 2)
                        throw MalException("splice-unquote expects 1 arg")
                    val list = eval(env, listItem.exprs[1]) as? Expr.List
                            ?: throw MalException("quasiquote argument should evaluate to a List")
                    list.exprs
                }
                listItem is Expr.List ->
                    listOf(quasiquote(env, listOf(listItem)) as Expr.List)
                else ->
                    listOf(listItem)
            }
        }.let { Expr.List(it) }

        else ->
            arg
    }
}

fun macroExpandMal(env: Env, args: List<Expr>): Expr {
    val firstArg = args.singleOrNull()
            ?: throw MalException("macroexpand takes 1 argument")
    return macroExpand(env, firstArg)
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
    val value = env.get(sym)
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

fun apply(env: Env, args: List<Expr>): Expr {
    if (args.size < 2)
        throw MalException("apply expects at least 1 Fn arg and a List/Vec/Nil")
    val fn = eval(env, args[0])
    val listArg = eval(env, args[args.size - 1]) as? Expr.Seq
            ?: throw MalException("last arg to apply must be a List/Vec/Nil")
    val otherArgs = args.drop(1).dropLast(1)
    val allArgs = otherArgs + listArg.exprs
    return when (fn) {
        is Expr.BuiltInFn ->
            fn.f(env, allArgs)
        is Expr.Fn -> {
            val (funcEnv, body) = envAndBodyForApply(env, fn, allArgs)
            eval(funcEnv, body)
        }
        else ->
            throw MalException("${fn.prStr()} is not a function")
    }
}

fun readline(env: Env, args: List<Expr>): Expr {
    val msg = "apply expects 1 String argument"
    if (args.size != 1)
        throw MalException(msg)
    val arg = eval(env, args.single()) as? Expr.Str
            ?: throw MalException(msg)
    print(arg.value)
    val input = readLine()
    return if (input != null) Expr.Str(input) else Expr.Nil
}

fun withMeta(env: Env, args: List<Expr>): Expr {
    if (args.size != 2)
        throw MalException("with-meta expects 2 args, an expr and its metadata")
    val expr = eval(env, args[0])
    val meta = eval(env, args[1])
    return expr.withMeta(meta)
}

fun meta(env: Env, args: List<Expr>): Expr {
    if (args.size != 1)
        throw MalException("meta expects 1 argument")
    val arg = eval(env, args.single())
    return arg.meta
}

@Suppress("UNUSED_PARAMETER")
fun gensym(env: Env, args: List<Expr>): Expr.Sym {
    if (args.isNotEmpty())
        throw MalException("gensym expects no args")
    val uuid = UUID.randomUUID()
    return Expr.Sym("gensym-$uuid")
}

@Suppress("UNUSED_PARAMETER")
fun timeMs(env: Env, args: List<Expr>): Expr.Num {
    if (args.isNotEmpty())
        throw MalException("time-ms takes no args")
    return Expr.Num(System.currentTimeMillis())
}

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
            val ast = Parse.parse(line)
            println(eval(env, ast).prStr())
        } catch (e: ParserException) {
            println("Parse error: ${e.message}")
        } catch (e: MalException) {
            println("Exception: ${e.value.prStr()}")
        }
    }
}

object Mal {
    @JvmStatic fun main(argv: Array<String>) {
        val env = Env(INITIAL_ENV)
        env.set("start-time", Expr.Num(System.currentTimeMillis()))
        // Read in core.mal
        javaClass.getResourceAsStream("/core.mal").use {
            loadFileStream(env, it)
        }
        // Run script file, or enter REPL
        if (argv.isNotEmpty())
            runScript(env, argv)
        else
            rep(env)
    }
}
