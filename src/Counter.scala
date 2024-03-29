package compiler

trait Counter {
    def Fresh(x: String): String
}

object Labels extends Counter {
    // for generating new labels
    var counter = -1

    def Fresh(x: String) = {
        counter += 1
        x ++ "_" ++ counter.toString()
    }
}
