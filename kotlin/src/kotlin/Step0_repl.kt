object Step0_repl {
    @JvmStatic fun main(args: Array<String>) {
        while (true) {
            print("user> ")
            val line = readLine() ?: break
            println(line)
        }
    }
}
