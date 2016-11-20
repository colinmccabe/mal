import org.codehaus.jparsec.error.ParserException

fun main(args: Array<String>) {
    while (true) {
        print("user> ")

        val line = readLine() ?: break
        if (line.isBlank()) continue

        try {
            println(show(parse(line)))
        } catch (e: ParserException) {
            println("Error: ${e.message}")
        }
    }
}
