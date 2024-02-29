package compiler
import mainargs.{main, ParserForMethods}
import os._
import ValParser._, NewCPS._, ClosureConv._
import fastparse.{parse, Parsed}
import java.lang.Long.toHexString
import java.lang.Double.doubleToLongBits

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
            .fromResource("typetest.txt")
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

    val prelude = """@.str = private constant [4 x i8] c"%d\0A\00"
@.str.1 = private constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

define void @print_i32(i32 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
    ret void
}

define void @print_i1(i1 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i1 %x) 
    ret void
}

define void @print_float(float %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str.1, i32 0, i32 0
    %t1 = fpext float %x to double      ; convert float to double
    call i32 (i8*, ...) @printf(i8* %t0, double %t1) 
    ret void
}

; <===== Generated code starts here =====>
"""

    // Another crime committed
    var program_envs: List[Env] = Nil
    var program_structs: List[Struct] = Nil
    var program_funcs: List[CFunc] = Nil
    var program_enums: Map[String, Type] = Map()

    extension (t: Type)
        def llvm: String = t match {
            case IntType => "i32"
            case EnumType(_) => "i32"
            case BoolType => "i1"
            case VoidType => "void"
            case FloatType => "float"
            case EnvType(env) => s"%${env}_t*"
            case FnType(args, ret) => ret.llvm ++ " " ++ args.map(_.llvm).mkString("(", ", ", ")*")
            case UserType(name) => {
                if program_enums.contains(name) then program_enums(name).llvm
                else if program_structs.exists(_.name == name) then s"%$name*"
                else throw new Exception(s"User type $t not found in $program_structs")
            }
        }

    extension (sc: StringContext) {
        def i(args: Any*): String = "    " ++ sc.s(args:_*) ++ "\n"
        def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
        def m(args: Any*): String = sc.s(args:_*) ++ "\n"
        def c(args: Any*): String = "@" ++ sc.s(args:_*) ++ "\n"
    }



    // mathematical and boolean operations
    // TODO: replace missing for float
    def compile_op(op: String, t: Type) = {
        val p = if t == FloatType then "f" else ""
        val q = if t == FloatType then "f" else "s"
        val r = if t == FloatType then "f" else "i"
            op match {
                case "+" => s"${p}add ${t.llvm}"
                case "*" => s"${p}mul ${t.llvm}"
                case "-" => s"${p}sub ${t.llvm}"
                case "/" => s"${q}div ${t.llvm}"
                case "%" => s"${q}rem ${t.llvm}"
                case "==" => s"${r}cmp eq ${t.llvm}"
                case "!=" => s"${r}cmp ne ${t.llvm}"
                case "<=" => s"${r}cmp sle ${t.llvm}"
                case "<"  => s"${r}cmp slt ${t.llvm}"
                case ">=" => s"${r}cmp sge ${t.llvm}"
                case ">"  => s"${r}cmp sgt ${t.llvm}"
        }
    }

    // compile K values
    def compile_val(v: KVal) : String = v match {
        case KVar(s, _: FnType) if program_funcs.exists(_.fname == s) => s"@${s}"
        case KVar(s, _) => s"%$s"
        case KNum(i) => i.toString
        case KBool(b) => if b then "true" else "false"
        case KFloat(f) => "0x" + toHexString(doubleToLongBits(f))
        case KEnum(root, item) => s"$root::$item"
        case KStructRef(st, age, t) => s"strct ref $st $age"
    }

    // compile K expressions
    def compile_exp(a: KExp) : String = a match {
        case KCall(KVar(name, FnType(args, ret)), vrs) => {
            val vs = vrs.map(v => s"${v.get_type.llvm} ${compile_val(v)}").mkString(", ")
            // If the first arg is an environment, we can assume this is a call to a pointer
            if program_funcs.exists(_.fname == name)
            then s"call ${ret.llvm} @${name}($vs)"
            else s"call ${ret.llvm} %${name}($vs)"
        }
        case Kop(op, x1, x2) => 
            s"${compile_op(op, x1.get_type)} ${compile_val(x1)}, ${compile_val(x2)}"
    }

    def compile_env_defs(envs: List[Env]) : String = envs.map {
        case Env(name, vals) => vals.map(_.get_type.llvm).mkString(s"%${name}_t = type { ", ", ", " }")
    }.mkString("", "\n", "\n\n")

    def compile_struct_defs(struct: List[Struct]) : String = {
        // creates a llvm struct (%name = type { i32, i32, i32 })
        struct.map {
            case Struct(name, items) => items.map(_._2.llvm).mkString(s"%$name = type { ", ", ", " }")
        }.mkString("", "\n", "\n\n")
    }

    def compile_env_store(x: String, env: Env) : String = {
        val Env(name, vals) = env
        i"%$x = alloca %${name}_t" ++
        vals.zipWithIndex.map{ case (v, i) =>
            val get_elem_ptr = i"%$x$i = getelementptr %${name}_t, %${name}_t* %$x, i32 0, i32 $i"
            val store = v match {
                case KVar(s, t: FnType) => i"store ${t.llvm} @$s, ${t.llvm}* %$x$i"
                case v => i"store ${v.get_type.llvm} ${compile_val(v)}, ${v.get_type.llvm}* %$x$i"
            }
            get_elem_ptr ++ store
        }.mkString
    }
    
    def compile_fn_ref(x: String, ref: Ref) : String = ref.env match {
        case KVar(_, t: EnvType) => {
            val actual_name = t.env
            val env = program_envs.find(_.name == actual_name).get
            val ptr = Fresh("ptr")
            i"%$ptr = getelementptr %${env.name}_t, %${env.name}_t* %${ref.env.s}, i32 0, i32 ${ref.idx}" ++
            i"%$x = load ${env.vals(ref.idx).get_type.llvm}, ${env.vals(ref.idx).get_type.llvm}* %$ptr"
        }
        case KVar(s, t: FnType) => {
            val actual_name = s
            i"%$x = load ${t.llvm}, ${t.llvm} %$s"
        }
        case _ => throw new Exception(s"Expected environment type, got ${ref.env}")
    }

    def compile_struct_dec(x: String, struct: String, vals: Seq[KVal]) : String = {
        i"%$x = alloca %$struct" ++
        vals.zipWithIndex.map{ case (v, i) =>
            val get_elem_ptr = i"%${x}_$i = getelementptr %$struct, %$struct* %$x, i32 0, i32 $i"
            val store = i"store ${v.get_type.llvm} ${compile_val(v)}, ${v.get_type.llvm}* %${x}_$i"
            get_elem_ptr ++ store
        }.mkString
    }

    def compile_return(v: KVal) : String = v match {
        case KVar(_, VoidType) => i"ret void"
        case v => i"ret ${v.get_type.llvm} ${compile_val(v)}"
    }

    def compile_write(v: KVal) = v match {
        case KStructRef(name, item, UserType(t)) => 
            // get el ptr to struct item based on program_structs
            val struct = program_structs.find(_.name == t).get
            val idx = struct.items.map(_._1).indexOf(item)
            val ptr = Fresh("ptr")
            val el = Fresh("el")
            i"%$ptr = getelementptr %$t, %$t* %$name, i32 0, i32 $idx" ++
            // load the value
            i"%$el = load ${struct.items(idx)._2.llvm}, ${struct.items(idx)._2.llvm}* %$ptr" ++
            // print the value
            i"call void @print_${struct.items(idx)._2.llvm}(${struct.items(idx)._2.llvm} %$el)"
        case _ =>
            i"call void @print_${v.get_type.llvm}(${v.get_type.llvm} ${compile_val(v)})"
    }

    def compile_anf(a: KAnf) : String = a match {
        case KReturn(v) => 
            compile_return(v)
        case KLetEnv(x, env, a) =>
            compile_env_store(x, env) ++ compile_anf(a)
        case KLetEnvRef(x, ref, a) =>
            compile_fn_ref(x, ref) ++ compile_anf(a)
        case KStructDef(struct, a) =>
            // already hoisted
            compile_anf(a)
        case KWrite(v, a) =>
            compile_write(v) ++ compile_anf(a)
        case KLet(x, KStructDec(struct, vals), a) =>
            compile_struct_dec(x, struct, vals) ++ compile_anf(a)
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
        case kconst: KConst => throw new Exception(s"KConst '${kconst.x}' should not be present after postprocess")
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

    def compile_comb(prog: Exp) : String = {
        val (enums, no_enums) = PreProcess.preprocess(prog)
        val cps = CPSi(no_enums)
        println(cps)
        println("################")
        val ppcps = PostProcess.postprocess(cps, enums)
        println(ppcps)
        println("################11")
        val (closure, _) = convert(ppcps)
        println(closure)
        println("################")
        val (cfunc, anf, envs, structs) = hoist(closure)
        program_envs = envs
        program_structs = structs
        program_enums = enums
        program_funcs = cfunc
        val full_program = cfunc :+ CFunc("main", Nil, IntType, anf)
        val output = prelude + compile_env_defs(envs) + compile_struct_defs(structs) + full_program.map(compile_cfunc).mkString("\n")
        output
    }
}
