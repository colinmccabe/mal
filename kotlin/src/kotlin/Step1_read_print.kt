import org.jparsec.error.ParserException

object Step1_read_print {
    @JvmStatic fun main(args: Array<String>) {
        while (true) {
            print("user> ")

            val line = readLine() ?: break
            if (line.isBlank()) continue

            try {
                val ast = Parse.parse(line)
                println(fixAst(ast).prStr())
            } catch (e: ParserException) {
                println("Error: ${e.message}")
            }
        }
    }

    /**
     * The parser parses hash maps into lists, e.g. (hash-map ...), but
     * for the tests to pass these need to be evaluated into hash maps.
     */
    fun fixAst(expr: Expr): Expr = when (expr) {
        is Expr.List -> {
            if (!expr.exprs.isEmpty() && expr.exprs[0] == Expr.Sym("hash-map"))
                hashMap(Env(null as Env?), expr.exprs.drop(1).map { fixAst(it) })
            else
                Expr.List(expr.exprs.map { fixAst(it) })
        }
        is Expr.WithMeta ->
                Expr.WithMeta(expr.expr, fixAst(expr.meta))
        else ->
            expr
    }

}
