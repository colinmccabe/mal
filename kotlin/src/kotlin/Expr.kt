import org.apache.commons.lang3.StringEscapeUtils
import java.util.*

sealed class Expr {
    interface Seq {
        val exprs: kotlin.collections.List<Expr>
    }

    var meta: Expr = Expr.Nil

    data class Bool(val value: Boolean) : Expr()
    data class Num(val value: Long) : Expr()
    data class Str(val value: String) : Expr()
    data class Sym(val value: String) : Expr()
    data class Keyword(val value: String) : Expr()
    data class Atom(var expr: Expr) : Expr()
    data class HashMap(val map: Map<Expr, Expr>) : Expr()
    data class BuiltInFn(val f: (Env, kotlin.collections.List<Expr>) -> Expr) : Expr()
    data class Fn(val argNames: kotlin.collections.List<String>,
                  val varArgName: String?,
                  val body: Expr,
                  val env: Env,
                  val isMacro: Boolean = false) : Expr()

    class List(override val exprs: kotlin.collections.List<Expr>) : Expr(), Seq {
        override fun equals(other: Any?): Boolean =
                (other is Seq) && (this.exprs == other.exprs)

        override fun hashCode(): Int =
                Objects.hash(exprs)

        override fun toString(): String =
                exprs.joinToString(prefix = "Expr.List(", separator = ", ", postfix = ")")
    }

    class Vec(override val exprs: kotlin.collections.List<Expr>) : Expr(), Seq {
        override fun equals(other: Any?): Boolean =
                (other is Seq) && (this.exprs == other.exprs)

        override fun hashCode(): Int =
                Objects.hash(exprs)

        override fun toString(): String =
                exprs.joinToString(prefix = "Expr.Vec(", separator = ", ", postfix = ")")
    }

    object Nil : Expr(), Seq {
        override val exprs: kotlin.collections.List<Expr> =
                emptyList()

        override fun toString() =
                "Expr.Nil"
    }

    fun prStr(printReadably: Boolean = true): String =
            when (this) {
                is Expr.Nil -> "nil"
                is Expr.Bool -> value.toString()
                is Expr.Num -> value.toString()
                is Expr.Str ->
                    if (printReadably)
                        '"' + StringEscapeUtils.escapeJava(value) + '"'
                    else
                        value
                is Expr.Sym -> value
                is Expr.Keyword -> ":$value"
                is Expr.Atom -> "(atom ${expr.prStr()})"
                is Expr.Fn, is Expr.BuiltInFn ->
                    "#<function>"
                is Expr.List -> exprs
                        .map { it.prStr(printReadably) }
                        .joinToString(prefix = "(", separator = " ", postfix = ")")
                is Expr.Vec -> exprs
                        .map { it.prStr(printReadably) }
                        .joinToString(prefix = "[", separator = " ", postfix = "]")
                is Expr.HashMap -> map
                        .map { (k, v) -> "${k.prStr(printReadably)} ${v.prStr(printReadably)}" }
                        .joinToString(prefix = "{", separator = " ", postfix = "}")
            }

    fun withMeta(meta: Expr): Expr {
        val copied = when (this) {
            is Expr.Bool -> this.copy()
            is Expr.Num -> this.copy()
            is Expr.Str -> this.copy()
            is Expr.Sym -> this.copy()
            is Expr.Keyword -> this.copy()
            is Expr.Atom -> this.copy()
            is Expr.HashMap -> this.copy()
            is Expr.BuiltInFn -> this.copy()
            is Expr.Fn -> this.copy()
            is Expr.List -> Expr.List(this.exprs)
            is Expr.Vec -> Expr.Vec(this.exprs)
            is Expr.Nil -> this
        }
        copied.meta = meta
        return copied
    }

}
