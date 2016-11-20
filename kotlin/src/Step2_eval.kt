import org.codehaus.jparsec.error.ParserException

fun main(args: Array<String>) {
    while (true) {
        print("user> ")

        val line = readLine() ?: break
        if (line.isBlank()) continue

        try {
            println(show(eval(parse(line))))
        } catch (e: ParserException) {
            println("Parse error: ${e.message}")
        } catch (e: EvalException) {
            println("Eval error: ${e.message}")
        }
    }
}
