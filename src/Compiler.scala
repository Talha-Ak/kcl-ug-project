package compiler
import mainargs.{main, ParserForMethods}
import os._
import ValParser._, NewCPS._, ClosureConv._
import fastparse.{parse, Parsed}

object Compiler {
    def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
    
    @main
    def print() = {
        val externalFile = scala.io.Source
            .fromResource("test2.txt")
            .mkString
        val ast = fastparse.parse(externalFile, All(_))
        ast match {
        //  case Parsed.Success(value, index) => {println(ast.get.value); println(compile_new(ast.get.value))}
         case Parsed.Success(value, index) => {println(ast.get.value); println(compile_comb(ast.get.value))}
         case Parsed.Failure(label, idx, extra) => println(extra.traced.trace)
        }
    }

    // @main
    // def write() = {
    //     val fname = "test.fun"
    //     val path = os.pwd / fname
    //     val file = fname.stripSuffix("." ++ path.ext)
    //     val externalFile = scala.io.Source
    //         .fromResource("test2.txt")
    //         .mkString
    //     val ast = fastparse.parse(externalFile, All(_))
    //     ast match {
    //      case Parsed.Success(value, index) => {
    //         val code = compile(ast.get.value)
    //         os.write.over(os.pwd / (file ++ ".ll"), code)
    //      }
    //      case Parsed.Failure(label, idx, extra) => println(extra.traced.trace)
    //     }
    // }

    // @main
    // def exec() = {
    //     val externalFile = scala.io.Source
    //         .fromResource("test2.txt")
    //         .mkString
    //     val ast = fastparse.parse(externalFile, All(_))
    //     ast match {
    //         case Parsed.Failure(label, idx, extra) => println(extra.traced.trace)
    //      case Parsed.Success(value, index) => {
    //         val fname = "test.fun"
    //         val path = os.pwd / fname
    //         val file = fname.stripSuffix("." ++ path.ext)
    //         write()
    //         os.proc("llc", "-filetype=obj", "--relocation-model=pic", file ++ ".ll").call()
    //         os.proc("gcc", file ++ ".o", "-o", file ++ ".bin").call()
    //         os.proc(os.pwd / (file ++ ".bin")).call(stdout = os.Inherit)
    //      }
    //     }
    // }

    extension (sc: StringContext) {
        def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
        def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
        def m(args: Any*): String = sc.s(args:_*) ++ "\n"
        def c(args: Any*): String = "@" ++ sc.s(args:_*) ++ "\n"
    }

    // mathematical and boolean operations
    def compile_op(op: String) = op match {
        case "+" => "add i32"
        case "*" => "mul i32"
        case "-" => "sub i32"
        case "/" => "sdiv i32"
        case "%" => "srem i32"
        case "==" => "icmp eq i32"
        case "!=" => "icmp ne i32"
        case "<=" => "icmp sle i32"
        case "<"  => "icmp slt i32"
        case ">=" => "icmp sge i32"
        case ">"  => "icmp sgt i32"
    }

    // compile K values
    def compile_val(v: KVal) : String = v match {
        case KNum(i) => s"$i"
        case KVar(s) => s"%$s"
        case KFnPointer(s) => s"@${s}"
    }

    // compile K expressions
    def compile_exp(a: KExp) : String = a match {
        case KExpVal(v) => compile_val(v)
        case KEnv(vals) => s"kenv ${vals.map(compile_val).mkString(", ")}"
        case KEnvRef(env, idx) => s"kenv_ref $env, $idx"
        case KCall(o, vrs) => {
            val vs = vrs.map(compile_val).mkString("i32 ", ", i32 ", "")
            s"call i32 @${o}($vs)"
        }
        case Kop(op, x1, x2) => 
            s"${compile_op(op)} ${compile_val(x1)}, ${compile_val(x2)}"
    }



    val prelude = """
@.str = private constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define i32 @printInt(i32 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
    ret i32 %x
}

"""


    // compile function for declarations and main
//    def compile_defs(d: Exp) : String = d match {
//        case Func(name, args, body) => { println(CPSi(body));
//            m"define i32 @$name (${args.mkString("i32 %", ", i32 %", "")}) {" ++
//            compile_exp(CPSi(body)) ++
//            m"}\n"
//        }
//        case Const(i, n: Num) => {
//            c"$i = global i32 ${n.i}"
//        }
//        case Main(body) => { println(CPSi(body));
//            m"define i32 @main() {" ++
//            compile_exp(CPS(body)(_ => KReturn(KNum(0)))) ++
//            m"}\n"
//        }
//    }

    def compile_anf(a: KAnf) : String = a match {
        case KReturn(v) =>
            i"ret i32 ${compile_val(v)}"
        case KLet(x: String, e: KExp, a: KAnf) => 
            i"%$x = ${compile_exp(e)}" ++ compile_anf(a)
        case KIf(x, e1, e2) => {
            val if_br = Fresh("if_branch")
            val else_br = Fresh("else_branch")
            i"br i1 %$x, label %$if_br, label %$else_br" ++
            l"\n$if_br" ++
            compile_anf(e1) ++
            l"\n$else_br" ++ 
            compile_anf(e2)
        }
    }


    def compile_cfunc(f: CFunc) : String = {
        val CFunc(name, args, body) = f
        val arglist = args.mkString("i32 %", ", i32 %", "")
        val body2 = compile_anf(body)
        m"define i32 @$name ($arglist) {" ++ body2 ++ m"}\n"
    }
    // main compiler functions
    // def compile(prog: List[Exp]) : String = 
    //     prelude ++ (prog.map(compile_defs).mkString)

    def compile_new(prog: List[Exp]) : String = {
        val cps = prog.map(CPSi)
        val closure = cps.map(convert)
        println(closure)
        println("################")
        val hoisted = closure.map(hoist)
        hoisted.map{case ((cf, a)) => cf.map(compile_cfunc).mkString("\n") ++ compile_anf(a)}.mkString
        hoisted.mkString("\nNEXTELEMENT\n")
    }

    def compile_comb(prog: Exp) : String = {
        val cps = CPSi(prog)
        val closure = convert(cps)
        println(closure)
        println("################")
        val (cfunc, anf) = hoist(closure)
        val output = (cfunc :+ CFunc("main", Nil, anf)).map(compile_cfunc).mkString("\n")
        output

        // val hoisted = hoist(closure)
        // hoisted.map{case ((cf, a)) => cf.map(compile_cfunc).mkString("\n") ++ compile_anf(a)}.mkString
        // hoisted.mkString("\nNEXTELEMENT\n")
    }
}

