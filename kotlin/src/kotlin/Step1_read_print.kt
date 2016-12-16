import org.jparsec.error.ParserException

fun main(args: Array<String>) {
    while (true) {
        print("user> ")

        val line = readLine() ?: break
        if (line.isBlank()) continue

        try {
            println(prStr(parse(line), printReadably = true))
        } catch (e: ParserException) {
            println("Error: ${e.message}")
        }
    }
}
