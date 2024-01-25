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

    @main
    def write() = {
        val externalFile = scala.io.Source
            .fromResource("test2typed.txt")
            .mkString
        val ast = fastparse.parse(externalFile, All(_))
        val code = ast match {
        //  case Parsed.Success(value, index) => {println(ast.get.value); println(compile_new(ast.get.value))}
         case Parsed.Success(value, index) => {println(ast.get.value); compile_comb(ast.get.value)}
         case Parsed.Failure(label, idx, extra) => println(extra.traced.trace); ""
        }
        val fname = "test.ll"
        os.write.over(os.pwd / fname, code)
    }

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

    extension (t: Type)
        def llvm = t match {
            case PrimType("Int") => "i32"
            case PrimType("Bool") => "i1"
            case PrimType("Unit") => "void"
            case EnvType(env) => s"%${env}_t"
            case t => "???"
        }

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
        case KVar(s, _) => s"%$s"
    }

    // compile K expressions
    def compile_exp(a: KExp) : String = a match {
        case KExpVal(v) => compile_val(v)
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

    def compile_env(x: String, env: Env) : String = {
        val Env(name, vals) = env
        (
            i"%$x = alloca %${name}_t" ++
            vals.zipWithIndex.map{
                case (KVar("foo", Missing), i) => 
                    i"%$x$i = getelementptr %${name}_t, %${name}_t* %$x, i32 0, i32 $i" ++
                    i"store i32 (%${name}_t*, i32)* (i32)* @foo, i32 (%${name}_t*, i32)* (i32)** %$x$i"
                case (KVar(fn, Missing), i) => 
                    i"%$x$i = getelementptr %${name}_t, %${name}_t* %$x, i32 0, i32 $i" ++
                    i"store i32 (%${name}_t*, i32)* @$fn, i32 (%${name}_t*, i32)** %$x$i"
                case (v: KVal, i) => 
                    i"%$x$i = getelementptr %${name}_t, %${name}_t* %$x, i32 0, i32 $i" ++
                    i"store i32 ${compile_val(v)}, i32* %$x$i"
            }.mkString
        )
    }
    
    def compile_env_ref(x: String, ref: Ref) : String = {
        val Ref(env, idx) = ref

        i"%$x = env ref %$env idx $idx"
    }

    def compile_anf(a: KAnf) : String = a match {
        case KReturn(v) =>
            i"ret i32 ${compile_val(v)}"
        case KLetEnv(x, env, a) =>
            compile_env(x, env) ++ compile_anf(a)
        // case KLetEnvRef(x, Ref(env, 0), KLet(y, KCall(fn, vrs), a)) =>
        //     compile_exp(KCall(env, KVar(env) +: vrs)) ++ compile_anf(KLet(x, KVar(fn), KVar(x)))
        case KLetEnvRef(x, ref, a) =>
                compile_env_ref(x, ref) ++ compile_anf(a)
        case KLet(x, e, a) => 
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
        case fun: KFun => throw new Exception(s"KFun '${fun.fnName}' should not be in closed ANF")
    }


    def compile_cfunc(f: CFunc) : String = {
        val CFunc(name, args, ret, body) = f
        // Assuming the first arg is the environment
        val arglist = if args.isEmpty
            then ""
            else args.map{case ((s, t)) => s"${t.llvm} %$s"}.mkString(", ")
        val body2 = compile_anf(body)
        m"define ${ret.llvm} @$name ($arglist) {" ++ body2 ++ m"}"
    }
    // main compiler functions
    // def compile(prog: List[Exp]) : String = 
    //     prelude ++ (prog.map(compile_defs).mkString)

    // def compile_new(prog: List[Exp]) : String = {
    //     val cps = prog.map(CPSi)
    //     val closure = cps.map(convert)
    //     println(closure)
    //     println("################")
    //     val hoisted = closure.map(hoist)
    //     hoisted.map{case ((cf, a)) => cf.map(compile_cfunc).mkString("\n") ++ compile_anf(a)}.mkString
    //     hoisted.mkString("\nNEXTELEMENT\n")
    // }

    given Conversion[List[Env], String] = _.map {
        case Env(name, vals) => {
            vals.map{
                // TODO remove this hack
                case KVar("foo", Missing) => s"i32 (i32)* (%${name}_t*, i32)*"
                case KVar(s, Missing) => s"i32 (%${name}_t*, i32)*"
                case _ => "i32"
            }.mkString(s"%${name}_t = type { ", ", ", " }")
        }
    }.mkString("", "\n", "\n\n")

    def compile_comb(prog: Exp) : String = {
        val cps = CPSi(prog)
        println(cps)
        println("################")
        val (closure, _) = convert(cps)
        val fixedClosure = remove_expval(closure)
        println(fixedClosure)
        println("################")
        val (cfunc, anf, envs) = hoist(fixedClosure)
        val output = envs + (cfunc :+ CFunc("main", Nil, PrimType("Int"), anf)).map(compile_cfunc).mkString("\n")
        output
    }
}

